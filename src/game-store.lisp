;;;; Game Store — domain-level game lifecycle management
;;;; Provides create/load/save/end/delete operations and move validation.
;;;; All Redis interaction is delegated to redis.lisp.

;;; ---------------------------------------------------------------------------
;;; Conditions (error hierarchy)
;;; ---------------------------------------------------------------------------

(define-condition game-error (error)
  ((error-code  :initarg :code       :reader error-code)
   (http-status :initarg :http-status :reader http-status)
   (user-message :initarg :message   :reader user-message))
  (:report (lambda (c s) (format s "~a" (user-message c)))))

(define-condition slots-full (game-error) ()
  (:default-initargs
   :code "slots_full"
   :http-status 503
   :message "All game slots are in use. Try again shortly."))

(define-condition game-not-found (game-error) ()
  (:default-initargs
   :code "game_not_found"
   :http-status 404
   :message "Game not found or expired."))

(define-condition game-over-error (game-error) ()
  (:default-initargs
   :code "game_over"
   :http-status 409
   :message "This game has already ended."))

(define-condition invalid-column-error (game-error) ()
  (:default-initargs
   :code "invalid_column"
   :http-status 400
   :message "Column must be 0-6."))

(define-condition column-full-error (game-error) ()
  (:default-initargs
   :code "column_full"
   :http-status 400
   :message "That column is full."))

(define-condition not-your-turn-error (game-error) ()
  (:default-initargs
   :code "not_your_turn"
   :http-status 409
   :message "It is not your turn."))

(define-condition rate-limited-error (game-error) ()
  (:default-initargs
   :code "rate_limited"
   :http-status 429
   :message "Too many requests. Please slow down."))

;;; ---------------------------------------------------------------------------
;;; Game status constants
;;; ---------------------------------------------------------------------------

(defvar +status-ongoing+     "ongoing")
(defvar +status-player-wins+ "player_wins")
(defvar +status-ai-wins+     "ai_wins")
(defvar +status-draw+        "draw")

(defun game-over-p (status)
  "Return T if STATUS represents a terminal game state."
  (member status (list +status-player-wins+ +status-ai-wins+ +status-draw+)
          :test #'string=))

;;; ---------------------------------------------------------------------------
;;; Token generation
;;; ---------------------------------------------------------------------------

(defun generate-token ()
  "Generate a UUID v4 string from random bytes."
  (let ((state (make-random-state t)))
    (format nil "~8,'0x-~4,'0x-4~3,'0x-~a~3,'0x-~12,'0x"
            (random #xFFFFFFFF state)
            (random #xFFFF state)
            (random #xFFF state)
            ;; Variant bits: 10xx (8, 9, a, b)
            (aref "89ab" (random 4 state))
            (random #xFFF state)
            (random #xFFFFFFFFFFFF state))))

;;; ---------------------------------------------------------------------------
;;; Board serialisation (internal 2D array <-> JSON string for Redis)
;;; ---------------------------------------------------------------------------

(defun board-to-redis-string (board)
  "Serialise a 7x6 board array to a JSON string for Redis storage."
  (cl-json:encode-json-to-string
   (loop for col from 0 below 7
         collect (loop for row from 0 below 6
                       collect (let ((cell (aref board col row)))
                                 (cond ((eq cell 'x) "X")
                                       ((eq cell 'o) "O")
                                       (t nil)))))))

(defun redis-string-to-board (str)
  "Deserialise a JSON string from Redis back to a 7x6 board array."
  (let ((data (cl-json:decode-json-from-string str)))
    (when (and (listp data) (= (length data) 7))
      (let ((board (make-array '(7 6) :initial-element nil)))
        (loop for col-data in data
              for col from 0
              do (loop for cell in col-data
                       for row from 0
                       do (setf (aref board col row)
                                (cond ((equal cell "X") 'x)
                                      ((equal cell "O") 'o)
                                      (t nil)))))
        board))))

;;; ---------------------------------------------------------------------------
;;; Game lifecycle
;;; ---------------------------------------------------------------------------

(defun create-game (depth)
  "Claim a slot, initialise a new game in Redis.
   Returns the token string, or signals SLOTS-FULL."
  (let ((token (generate-token)))
    (unless (claim-slot token)
      (error 'slots-full))
    (let ((board (make-empty-board))
          (now (get-universal-time)))
      (redis-save-game token
                       (list :board  (board-to-redis-string board)
                             :turn   "x"
                             :status +status-ongoing+
                             :move-count "0"
                             :depth  (princ-to-string depth)
                             :created-at (princ-to-string now))
                       *game-ttl-seconds*)
      token)))

(defun load-game (token)
  "Load game state from Redis. Returns a plist or signals GAME-NOT-FOUND."
  (let ((plist (redis-load-game token)))
    (unless plist
      (error 'game-not-found))
    plist))

(defun game-board (game-plist)
  "Extract and deserialise the board from a game plist.
   Signals GAME-NOT-FOUND if the board data is missing or corrupt."
  (let ((board (redis-string-to-board (getf game-plist :board))))
    (unless board
      (error 'game-not-found))
    board))

(defun game-turn (game-plist)
  "Return the current turn as a symbol ('x or 'o)."
  (if (string= (getf game-plist :turn) "x") 'x 'o))

(defun game-status (game-plist)
  "Return the status string from a game plist."
  (getf game-plist :status))

(defun game-depth (game-plist)
  "Return the AI search depth for this game."
  (parse-integer (getf game-plist :depth) :junk-allowed t))

(defun game-move-count (game-plist)
  "Return the move count as an integer."
  (parse-integer (getf game-plist :move-count) :junk-allowed t))

(defun save-game (token board turn status move-count)
  "Persist updated game state to Redis and refresh TTL."
  (redis-save-game token
                   (list :board      (board-to-redis-string board)
                         :turn       (if (eq turn 'x) "x" "o")
                         :status     status
                         :move-count (princ-to-string move-count))
                   *game-ttl-seconds*))

(defun end-game (token status board)
  "Mark a game as ended: free the slot, save terminal state with short TTL."
  (release-slot token)
  (redis-save-game token
                   (list :board  (board-to-redis-string board)
                         :status status)
                   *game-ended-ttl-seconds*))

(defun delete-game (token)
  "Fully remove a game — release the slot and delete the Redis key."
  (release-slot token)
  (redis-delete-game token))

;;; ---------------------------------------------------------------------------
;;; Move validation
;;; ---------------------------------------------------------------------------

(defun validate-move (game-plist column)
  "Validate that a move is legal. Signals an appropriate condition on failure.
   Returns the deserialised board on success (avoids redundant deserialisation)."
  ;; Game must be ongoing
  (when (game-over-p (game-status game-plist))
    (error 'game-over-error))
  ;; Must be player's turn
  (unless (eq (game-turn game-plist) 'x)
    (error 'not-your-turn-error))
  ;; Column must be in range
  (unless (and (integerp column) (<= 0 column 6))
    (error 'invalid-column-error))
  ;; Column must have space — return board for caller to reuse
  (let ((board (game-board game-plist)))
    (unless (legal-move? board 'x column)
      (error 'column-full-error))
    board))
