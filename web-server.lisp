;;;; Connect-4 Web Server — Redis-backed game slot management
;;;; Stateless server instances; all game state lives in Redis.

(require :asdf)
(asdf:load-system :hunchentoot)
(asdf:load-system :cl-json)
(asdf:load-system :lparallel)
(asdf:load-system :cl-redis)

;;; Load modules in dependency order
(load "src/connect-4.lisp")
(load "src/heuristic.lisp")
(load "src/minimax.lisp")
(load "src/redis.lisp")
(load "src/game-store.lisp")

;;; Initialise lparallel kernel
(setf lparallel:*kernel*
      (lparallel:make-kernel
       (max 2 (or (parse-integer (or (uiop:getenv "LISP_AI_THREADS") "") :junk-allowed t)
                   4))))

;;; ---------------------------------------------------------------------------
;;; AI configuration
;;; ---------------------------------------------------------------------------

(defconstant +default-depth+
  (or (let ((env (uiop:getenv "LISP_AI_DEPTH")))
        (and env (parse-integer env :junk-allowed t)))
      3))

(defconstant +max-depth+
  (or (let ((env (uiop:getenv "LISP_AI_MAX_DEPTH")))
        (and env (parse-integer env :junk-allowed t)))
      6))

(defconstant +board-rows+ 6)
(defconstant +board-columns+ 7)

;;; Per-request dynamic variables
(defvar *static-evaluations* 0)
(defvar *max-depth* +default-depth+)

(defun resolve-depth (raw-depth)
  "Parse and clamp depth between 1 and +max-depth+."
  (let ((d (or (and raw-depth (parse-integer (princ-to-string raw-depth)
                                              :junk-allowed t))
               +default-depth+)))
    (max 1 (min d +max-depth+))))

;;; ---------------------------------------------------------------------------
;;; Board conversion (internal -> JSON response)
;;; ---------------------------------------------------------------------------

(defun board-to-json (board)
  "Convert internal 2D array board to JSON-friendly nested lists."
  (loop for col from 0 below 7
        collect (loop for row from 0 below 6
                      collect (let ((cell (aref board col row)))
                                (cond ((eq cell 'x) "X")
                                      ((eq cell 'o) "O")
                                      (t nil))))))

(defun valid-board-p (board)
  "Check if board is a valid 2D array."
  (and (arrayp board)
       (equal (array-dimensions board) '(7 6))))

;;; ---------------------------------------------------------------------------
;;; AI move selection
;;; ---------------------------------------------------------------------------

(defun ai-search (board player)
  "Run minimax search, using parallel search for depth >= 4.
   Binds a fresh TT per request to avoid races between HTTP threads."
  (let ((*tt* (make-array +tt-size+ :initial-element nil)))
    (if (>= *max-depth* 4)
        (minimax-a-b-parallel board player)
        (minimax-a-b board 0 player))))

;;; ---------------------------------------------------------------------------
;;; Game logic helpers
;;; ---------------------------------------------------------------------------

(defun get-game-status (board)
  "Determine game status from board state."
  (cond ((won? board 'o) +status-ai-wins+)
        ((won? board 'x) +status-player-wins+)
        ((drawn? board)  +status-draw+)
        (t               +status-ongoing+)))

(defun find-immediate-win (board player)
  "Check if PLAYER can win in one move. Returns (values board column) or nil."
  (dolist (col '(3 2 4 1 5 0 6))
    (let ((new-board (make-move board player col)))
      (when (and new-board (won? new-board player))
        (return-from find-immediate-win (values new-board col)))))
  nil)

(defun find-immediate-block (board player)
  "Check if PLAYER must block opponent's immediate win. Returns (values board column) or nil."
  (let ((opponent (opposite player)))
    (dolist (col '(3 2 4 1 5 0 6))
      (let ((opp-board (make-move board opponent col)))
        (when (and opp-board (won? opp-board opponent))
          (let ((block-board (make-move board player col)))
            (when block-board
              (return-from find-immediate-block (values block-board col))))))))
  nil)

(defun find-winning-cells (board player)
  "Return the list of (col row) cells forming the winning line, or nil."
  (dolist (line *all-c4-lines*)
    (when (check-line board player line)
      (return-from find-winning-cells
        (mapcar (lambda (pos) (list (first pos) (second pos))) line))))
  nil)

(defun evaluate-move (board player col)
  "Evaluate a single move for scoring display."
  (let* ((row (get-next-blank board col))
         (new-board (make-move board player col)))
    (cond
      ((null new-board) nil)
      ((won? new-board player) 50000)
      (t
       (let* ((*tt* (make-array +tt-size+ :initial-element nil))
              (hash (hash-after-move (board-hash board) col row player))
              (result (minimax-a-b-1 new-board 1 (opposite player) 99999 -99999 nil hash)))
         (if (numberp result)
             (let ((score (- result)))
               (cond ((>= score 50000) 49000)
                     ((<= score -50000) -49000)
                     (t score)))
             0))))))

(defun evaluate-all-moves (board player)
  "Return scored list of all columns: ((col score) ...)."
  (loop for col from 0 below 7
        collect (list col (evaluate-move board player col))))

;;; ---------------------------------------------------------------------------
;;; Error handling
;;; ---------------------------------------------------------------------------

(defmacro with-game-error-handling (&body body)
  "Catch game-error conditions and return appropriate JSON error responses."
  `(handler-case (progn ,@body)
     (game-error (e)
       (setf (hunchentoot:return-code*) (http-status e))
       (when (= (http-status e) 429)
         (setf (hunchentoot:header-out :retry-after)
               (princ-to-string *rate-limit-window-seconds*)))
       (cl-json:encode-json-to-string
        `((:error . ,(error-code e))
          (:message . ,(user-message e)))))
     (error (e)
       (format *error-output* "Unexpected error: ~a~%" e)
       (setf (hunchentoot:return-code*) 500)
       (cl-json:encode-json-to-string
        '((:error . "internal_error")
          (:message . "An unexpected error occurred."))))))

;;; ---------------------------------------------------------------------------
;;; Rate limiting
;;; ---------------------------------------------------------------------------

(defun client-ip ()
  "Get the client's IP address, respecting X-Forwarded-For behind a proxy."
  (or (hunchentoot:header-in* :x-forwarded-for)
      (hunchentoot:remote-addr*)))

(defun enforce-rate-limit ()
  "Signal RATE-LIMITED-ERROR if the client has exceeded the request limit."
  (unless (rate-limit-check (client-ip))
    (error 'rate-limited-error)))

;;; ---------------------------------------------------------------------------
;;; Request body parsing
;;; ---------------------------------------------------------------------------

(defun parse-json-body ()
  "Parse the JSON POST body. Returns an alist or NIL."
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (handler-case
        (when (and body (> (length body) 0))
          (cl-json:decode-json-from-string body))
      (error () nil))))

;;; ---------------------------------------------------------------------------
;;; HTTP Handlers
;;; ---------------------------------------------------------------------------

;;; Health check — includes Redis status and slot info
(hunchentoot:define-easy-handler (api-health :uri "/api/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((redis-ok (redis-ping))
         (active (when redis-ok (active-game-count))))
    (cl-json:encode-json-to-string
     `((:status . ,(if redis-ok "ok" "degraded"))
       (:version . "1.0")
       (:redis . ,(if redis-ok "connected" "disconnected"))
       (:active-games . ,(or active 0))
       (:max-games . ,*max-game-slots*)
       (:heartbeat-ttl . ,*heartbeat-ttl-seconds*)
       (:inactivity-ttl . ,*inactivity-ttl-seconds*)))))

;;; Create a new game (POST only)
(hunchentoot:define-easy-handler (api-new-game :uri "/api/new-game") ()
  (setf (hunchentoot:content-type*) "application/json")
  (unless (eq (hunchentoot:request-method*) :post)
    (setf (hunchentoot:return-code*) 405)
    (return-from api-new-game
      (cl-json:encode-json-to-string
       '((:error . "method_not_allowed")
         (:message . "Use POST method.")))))
  (with-game-error-handling
    (enforce-rate-limit)
    (let* ((json-data (parse-json-body))
           (raw-depth (cdr (assoc :depth json-data)))
           (ai-first  (cdr (assoc :ai-first json-data)))
           (depth     (resolve-depth raw-depth))
           (token     (create-game depth)))
      (handler-case
          (if ai-first
              ;; AI moves first
              (let* ((game-plist (load-game token))
                     (board (game-board game-plist))
                     (*max-depth* depth)
                     (*static-evaluations* 0)
                     (ai-scores (evaluate-all-moves board 'o)))
                (multiple-value-bind (new-board ai-col) (ai-search board 'o)
                  (save-game token new-board 'x +status-ongoing+
                             (1+ (game-move-count game-plist)))
                  (cl-json:encode-json-to-string
                   `((:token . ,token)
                     (:board . ,(board-to-json new-board))
                     (:status . ,+status-ongoing+)
                     (:turn . "x")
                     (:ai-move . ,ai-col)
                     (:ai-scores . ,ai-scores)
                     (:evaluations . ,*static-evaluations*)
                     (:message . "AI moved first!")))))
              ;; Player moves first
              (let* ((game-plist (load-game token))
                     (board (game-board game-plist)))
                (cl-json:encode-json-to-string
                 `((:token . ,token)
                   (:board . ,(board-to-json board))
                   (:status . ,+status-ongoing+)
                   (:turn . "x")
                   (:message . "Your turn!")))))
        (error (e)
          ;; Clean up the slot if game creation succeeded but something else failed
          (delete-game token)
          (error e))))))

;;; Make a move
(hunchentoot:define-easy-handler (api-move :uri "/api/move") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-game-error-handling
    (enforce-rate-limit)
    (let* ((json-data (parse-json-body))
           (token     (cdr (assoc :token json-data)))
           (col       (cdr (assoc :column json-data))))
      ;; Validate token presence and format (UUID = 36 chars)
      (unless (and token (stringp token) (<= 1 (length token) 36))
        (error 'game-not-found))
      ;; Ensure column is an integer
      (unless (integerp col)
        (error 'invalid-column-error))
      ;; Load and validate (validate-move returns the deserialised board)
      (let ((game-plist (load-game token)))
        (let* ((board      (validate-move game-plist col))
               (depth      (game-depth game-plist))
               (move-count (game-move-count game-plist))
               (new-board  (make-move board 'x col)))
          ;; Player move applied — check for terminal state
          (cond
            ;; Player wins
            ((won? new-board 'x)
             (end-game token +status-player-wins+ new-board)
             (cl-json:encode-json-to-string
              `((:board . ,(board-to-json new-board))
                (:status . ,+status-player-wins+)
                (:winning-cells . ,(find-winning-cells new-board 'x))
                (:message . "You win!"))))
            ;; Draw after player move
            ((drawn? new-board)
             (end-game token +status-draw+ new-board)
             (cl-json:encode-json-to-string
              `((:board . ,(board-to-json new-board))
                (:status . ,+status-draw+)
                (:message . "Draw!"))))
            ;; Game continues — AI responds
            (t
             (let* ((*max-depth* depth)
                    (*static-evaluations* 0)
                    (ai-scores (evaluate-all-moves new-board 'o))
                    (ai-board nil)
                    (ai-col nil)
                    (immediate-win-col nil)
                    (immediate-block-col nil))
               ;; Try immediate win
               (multiple-value-bind (wb wc) (find-immediate-win new-board 'o)
                 (when wb
                   (setq ai-board wb ai-col wc immediate-win-col wc)))
               ;; Try immediate block
               (unless ai-board
                 (multiple-value-bind (bb bc) (find-immediate-block new-board 'o)
                   (when bb
                     (setq ai-board bb ai-col bc immediate-block-col bc))))
               ;; Full minimax
               (unless ai-board
                 (multiple-value-bind (b c) (ai-search new-board 'o)
                   (setq ai-board b ai-col c)))
               ;; Determine status after AI move
               (let ((status (get-game-status ai-board))
                     (winning (when (won? ai-board 'o)
                                (find-winning-cells ai-board 'o))))
                 ;; Save or end game
                 (if (game-over-p status)
                     (end-game token status ai-board)
                     (save-game token ai-board 'x status (+ move-count 2)))
                 (cl-json:encode-json-to-string
                  `((:board . ,(board-to-json ai-board))
                    (:status . ,status)
                    (:ai-move . ,ai-col)
                    (:evaluations . ,*static-evaluations*)
                    (:ai-scores . ,ai-scores)
                    (:immediate-win . ,immediate-win-col)
                    (:immediate-block . ,immediate-block-col)
                    ,@(when winning `((:winning-cells . ,winning)))
                    (:message . ,(cond ((string= status +status-ai-wins+) "AI wins!")
                                       ((string= status +status-draw+)    "Draw!")
                                       (t "Your turn!"))))))))))))))

;;; Resign / delete a game
;;; Accepts DELETE (normal) and POST (for navigator.sendBeacon on tab close)
(hunchentoot:define-easy-handler (api-delete-game :uri "/api/game") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((method (hunchentoot:request-method*)))
    (unless (or (eq method :delete) (eq method :post))
      (setf (hunchentoot:return-code*) 405)
      (return-from api-delete-game
        (cl-json:encode-json-to-string
         '((:error . "method_not_allowed")
           (:message . "Use DELETE or POST method."))))))
  (with-game-error-handling
    (let* ((json-data (parse-json-body))
           (token     (cdr (assoc :token json-data))))
      (when (and token (stringp token) (> (length token) 0))
        (delete-game token))
      (cl-json:encode-json-to-string
       '((:message . "Game ended."))))))

;;; Heartbeat — keep a game alive while the tab is open
(hunchentoot:define-easy-handler (api-heartbeat :uri "/api/heartbeat") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-game-error-handling
    (let* ((json-data (parse-json-body))
           (token     (cdr (assoc :token json-data))))
      (unless (and token (stringp token) (<= 1 (length token) 36))
        (error 'game-not-found))
      ;; Verify game exists, then refresh TTL
      (load-game token)
      (redis-refresh-game-ttl token)
      (cl-json:encode-json-to-string
       '((:status . "ok"))))))

;;; Debug endpoint — analyse a game's current board
(hunchentoot:define-easy-handler (api-debug :uri "/api/debug") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-game-error-handling
    (let* ((json-data (parse-json-body))
           (token     (cdr (assoc :token json-data))))
      (unless (and token (stringp token) (<= 1 (length token) 36))
        (error 'game-not-found))
      (let* ((game-plist (load-game token))
             (board (game-board game-plist))
             (d     (game-depth game-plist))
             (*max-depth* d)
             (*static-evaluations* 0))
        (let* ((ai-scores (evaluate-all-moves board 'o))
               (best-col (first (first (sort (remove-if-not #'second (copy-list ai-scores))
                                              #'> :key #'second)))))
          (multiple-value-bind (win-board win-col) (find-immediate-win board 'o)
            (declare (ignore win-board))
            (multiple-value-bind (block-board block-col) (find-immediate-block board 'o)
              (declare (ignore block-board))
              (cl-json:encode-json-to-string
               `((:depth . ,d)
                 (:ai-scores . ,ai-scores)
                 (:best-col . ,best-col)
                 (:evaluations . ,*static-evaluations*)
                 (:immediate-win . ,win-col)
                 (:immediate-block . ,block-col))))))))))

;;; ---------------------------------------------------------------------------
;;; Server startup
;;; ---------------------------------------------------------------------------

(defun start-server (&optional (port 8080))
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :port port
                               :document-root #p"/app/static/")))
    (hunchentoot:start server)
    (format t "~%Connect-4 Server v1.0 running on http://localhost:~a~%" port)
    (format t "Redis: ~a:~a | Slots: ~a | Heartbeat: ~as | Inactivity: ~as~%"
            *redis-host* *redis-port* *max-game-slots*
            *heartbeat-ttl-seconds* *inactivity-ttl-seconds*)
    (force-output)
    server))

;; Auto-start when loaded
(defvar *server* (start-server (let ((port-str (uiop:getenv "PORT")))
                                 (if port-str
                                     (parse-integer port-str :junk-allowed t)
                                     8080))))

;; Keep the process alive (Hunchentoot runs in background threads)
(loop (sleep 86400))
