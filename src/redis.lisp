;;;; Redis connection and low-level operations
;;;; Provides thread-safe Redis access, key schema constants,
;;;; and atomic slot management via Lua scripting.

;;; ---------------------------------------------------------------------------
;;; Configuration (from environment, with defaults)
;;; ---------------------------------------------------------------------------

(defvar *redis-host*
  (or (uiop:getenv "REDIS_HOST") "redis"))

(defvar *redis-port*
  (or (let ((p (uiop:getenv "REDIS_PORT")))
        (and p (parse-integer p :junk-allowed t)))
      6379))

(defvar *max-game-slots*
  (or (let ((s (uiop:getenv "MAX_GAME_SLOTS")))
        (and s (parse-integer s :junk-allowed t)))
      4))

(defvar *game-ttl-seconds*
  (or (let ((t-val (uiop:getenv "GAME_TTL_SECONDS")))
        (and t-val (parse-integer t-val :junk-allowed t)))
      300))

(defvar *game-ended-ttl-seconds* 60
  "TTL for game keys after the game has ended. Allows clients to
   fetch the final board state briefly before cleanup.")

;;; ---------------------------------------------------------------------------
;;; Key schema
;;; ---------------------------------------------------------------------------

(defvar +game-key-prefix+ "game:"
  "Redis key prefix for individual game hashes.")

(defvar +active-set-key+ "games:active"
  "Redis set holding tokens of all active (ongoing) games.")

(defun game-key (token)
  "Build the Redis key for a game hash."
  (concatenate 'string +game-key-prefix+ token))

;;; ---------------------------------------------------------------------------
;;; Thread-safe connection
;;; ---------------------------------------------------------------------------

(defvar *redis-lock* (bordeaux-threads:make-lock "redis")
  "Mutex protecting all Redis operations. cl-redis is not thread-safe,
   so every call must be serialised through this lock.")

(defmacro with-redis (&body body)
  "Execute BODY with a Redis connection, holding the global lock.
   Ensures thread safety for Hunchentoot's multi-threaded handler model."
  `(bordeaux-threads:with-lock-held (*redis-lock*)
     (redis:with-connection (:host *redis-host* :port *redis-port*)
       ,@body)))

;;; ---------------------------------------------------------------------------
;;; Health check
;;; ---------------------------------------------------------------------------

(defun redis-ping ()
  "Return T if Redis is reachable, NIL otherwise."
  (handler-case
      (with-redis (string= "PONG" (red:ping)))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Lua script for atomic slot allocation
;;; ---------------------------------------------------------------------------
;;; The script atomically:
;;;   1. Cleans stale tokens (whose game key has expired) from the active set
;;;   2. Checks whether capacity remains
;;;   3. Adds the new token and creates a placeholder game key with TTL
;;; The placeholder key prevents the stale-entry cleanup from immediately
;;; removing the token before the full game state is saved.
;;; Returns 1 on success, 0 if all slots are occupied.

(defparameter *claim-slot-script*
  "local members = redis.call('SMEMBERS', KEYS[1])
for _, t in ipairs(members) do
  if redis.call('EXISTS', ARGV[3] .. t) == 0 then
    redis.call('SREM', KEYS[1], t)
  end
end
if redis.call('SCARD', KEYS[1]) >= tonumber(ARGV[1]) then
  return 0
end
redis.call('SADD', KEYS[1], ARGV[2])
redis.call('HSET', ARGV[3] .. ARGV[2], 'status', 'pending')
redis.call('EXPIRE', ARGV[3] .. ARGV[2], tonumber(ARGV[4]))
return 1")

;;; ---------------------------------------------------------------------------
;;; Slot management
;;; ---------------------------------------------------------------------------

(defun claim-slot (token)
  "Attempt to claim a game slot for TOKEN. Returns T on success, NIL if full.
   Also creates a placeholder game key to prevent race conditions."
  (with-redis
    (let ((result (red:eval *claim-slot-script*
                            1                          ; number of KEYS
                            +active-set-key+           ; KEYS[1]
                            (princ-to-string *max-game-slots*) ; ARGV[1]
                            token                      ; ARGV[2]
                            +game-key-prefix+          ; ARGV[3]
                            (princ-to-string *game-ttl-seconds*)))) ; ARGV[4]
      ;; cl-redis may return integer 1 or T depending on version
      (or (eql result 1) (eql result t)))))

(defun release-slot (token)
  "Remove TOKEN from the active game set, freeing a slot."
  (with-redis
    (red:srem +active-set-key+ token)))

(defun active-game-count ()
  "Return the number of currently active games.
   Also cleans stale entries whose game keys have expired."
  (with-redis
    ;; Clean stale entries first
    (let ((members (red:smembers +active-set-key+)))
      (dolist (tok members)
        (unless (red:exists (game-key tok))
          (red:srem +active-set-key+ tok))))
    (red:scard +active-set-key+)))

;;; ---------------------------------------------------------------------------
;;; Low-level game hash operations
;;; ---------------------------------------------------------------------------

(defun redis-save-game (token fields-plist ttl)
  "Store game FIELDS-PLIST as a Redis hash under TOKEN's key, with TTL seconds."
  (with-redis
    (let ((key (game-key token)))
      ;; HSET with multiple field-value pairs
      (loop for (field value) on fields-plist by #'cddr
            do (red:hset key (string-downcase (symbol-name field)) value))
      (red:expire key ttl))))

(defun redis-load-game (token)
  "Load all fields of TOKEN's game hash. Returns a plist or NIL if not found."
  (with-redis
    (let ((key (game-key token)))
      (when (red:exists key)
        (let ((raw (red:hgetall key)))
          ;; HGETALL returns a flat list: (field1 val1 field2 val2 ...)
          ;; Convert to keyword plist
          (loop for (field value) on raw by #'cddr
                collect (intern (string-upcase field) :keyword)
                collect value))))))

(defun redis-delete-game (token)
  "Delete TOKEN's game hash from Redis entirely."
  (with-redis
    (red:del (game-key token))))

(defun redis-set-ttl (token ttl)
  "Update the TTL on TOKEN's game key."
  (with-redis
    (red:expire (game-key token) ttl)))

(defun redis-refresh-game-ttl (token)
  "Refresh the active-game TTL on TOKEN's key."
  (redis-set-ttl token *game-ttl-seconds*))
