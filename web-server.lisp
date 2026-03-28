;;;; Connect-4 Web Server - Stateless Version
;;;; Horizontally scalable - no server-side game state

(require :asdf)
(asdf:load-system :hunchentoot)
(asdf:load-system :cl-json)
(asdf:load-system :lparallel)

;;; Load game logic
(load "src/connect-4.lisp")
(load "src/heuristic.lisp")
(load "src/minimax.lisp")

;;; Initialize lparallel kernel
(setf lparallel:*kernel*
      (lparallel:make-kernel
       (max 2 (or (parse-integer (or (uiop:getenv "LISP_AI_THREADS") "") :junk-allowed t)
                   4))))

;;; Constants
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

;;; Global for evaluation count (per-request)
(defvar *static-evaluations* 0)
(defvar *max-depth* +default-depth+)

;;; Depth resolution with clamping
(defun resolve-depth (raw-depth)
  "Parse and clamp depth between 1 and +max-depth+"
  (let ((d (or (and raw-depth (parse-integer raw-depth :junk-allowed t))
               +default-depth+)))
    (max 1 (min d +max-depth+))))

;;; Board conversion utilities
(defun json-to-board (json-board)
  "Convert JSON board array to internal 2D array format.
   Returns nil if input is not a valid 7x6 list structure."
  (when (and json-board
             (listp json-board)
             (= (length json-board) +board-columns+)
             (every (lambda (col) (and (listp col) (= (length col) +board-rows+)))
                    json-board))
    (let ((board (make-array '(7 6) :initial-element nil)))
      (loop for col-data in json-board
            for col from 0
            do (loop for cell in col-data
                     for row from 0
                     do (setf (aref board col row)
                              (cond ((equal cell "X") 'x)
                                    ((equal cell "O") 'o)
                                    (t nil)))))
      board)))

(defun board-to-json (board)
  "Convert internal 2D array board to JSON format"
  (loop for col from 0 below 7
        collect (loop for row from 0 below 6
                     collect (let ((cell (aref board col row)))
                               (cond ((eq cell 'x) "X")
                                     ((eq cell 'o) "O")
                                     (t nil))))))

(defun valid-board-p (board)
  "Check if board is valid 2D array"
  (and (arrayp board)
       (equal (array-dimensions board) '(7 6))))

;;; AI move selection - use parallel search for deeper depths
(defun ai-search (board player)
  "Run minimax search, using parallel search for depth >= 4.
   Binds a fresh TT per request to avoid races between HTTP threads."
  (let ((*tt* (make-array +tt-size+ :initial-element nil)))
    (if (>= *max-depth* 4)
        (minimax-a-b-parallel board player)
        (minimax-a-b board 0 player))))

;;; Game status helpers
(defun get-game-status (board)
  (cond ((won? board 'o) "ai_wins")
        ((won? board 'x) "player_wins")
        ((drawn? board) "draw")
        (t "ongoing")))

;;; Win/block detection with center-first ordering
(defun find-immediate-win (board player)
  (dolist (col '(3 2 4 1 5 0 6))
    (let ((new-board (make-move board player col)))
      (when (and new-board (won? new-board player))
        (return-from find-immediate-win (values new-board col)))))
  nil)

(defun find-immediate-block (board player)
  (let ((opponent (opposite player)))
    (dolist (col '(3 2 4 1 5 0 6))
      (let ((opp-board (make-move board opponent col)))
        (when (and opp-board (won? opp-board opponent))
          (let ((block-board (make-move board player col)))
            (when block-board
              (return-from find-immediate-block (values block-board col))))))))
  nil)

(defun find-winning-cells (board player)
  (dolist (line *all-c4-lines*)
    (when (check-line board player line)
      (return-from find-winning-cells
        (mapcar (lambda (pos) (list (first pos) (second pos))) line))))
  nil)

;;; Move evaluation (for debug/scoring)
(defun evaluate-move (board player col)
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
  (loop for col from 0 below 7
        collect (list col (evaluate-move board player col))))

;;; HTTP Handlers

(hunchentoot:define-easy-handler (api-health :uri "/api/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{\"status\":\"ok\",\"version\":\"stateless\"}")

(hunchentoot:define-easy-handler (api-new-game :uri "/api/new-game") ()
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string
   `((:board . ,(board-to-json (make-empty-board)))
     (:status . "ongoing")
     (:message . "Your turn!"))))

(hunchentoot:define-easy-handler (api-new-game-ai-first :uri "/api/new-game-ai-first") (depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((board (make-empty-board))
         (d (resolve-depth depth))
         (*max-depth* d)
         (*static-evaluations* 0))
    (let ((ai-scores (evaluate-all-moves board 'o)))
      (multiple-value-bind (new-board ai-col) (ai-search board 'o)
        (cl-json:encode-json-to-string
         `((:board . ,(board-to-json new-board))
           (:status . "ongoing")
           (:ai-move . ,ai-col)
           (:ai-scores . ,ai-scores)
           (:evaluations . ,*static-evaluations*)
           (:message . "AI moved first!")))))))

(hunchentoot:define-easy-handler (api-move :uri "/api/move") (column depth)
  (setf (hunchentoot:content-type*) "application/json")
  ;; Get board from POST body
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (json-data (handler-case
                        (when (and body (> (length body) 0))
                          (cl-json:decode-json-from-string body))
                      (error () nil)))
         (board-data (cdr (assoc :board json-data)))
         (board (json-to-board board-data))
         (col (and column (parse-integer column :junk-allowed t)))
         (d (resolve-depth depth)))
    (cond
      ((or (null board) (not (valid-board-p board)))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid or missing board state"))))
      ((or (null col) (< col 0) (>= col +board-columns+))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid column"))))
      (t
       (let ((new-board (make-move board 'x col)))
         (cond
           ((null new-board)
            (setf (hunchentoot:return-code*) 400)
            (cl-json:encode-json-to-string '((:error . "Column is full"))))
           ((won? new-board 'x)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "player_wins")
               (:winning-cells . ,(find-winning-cells new-board 'x))
               (:message . "You win!"))))
           ((drawn? new-board)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "draw")
               (:message . "Draw!"))))
           (t
            (let* ((*max-depth* d)
                   (*static-evaluations* 0)
                   (ai-scores (evaluate-all-moves new-board 'o))
                   (ai-board nil)
                   (ai-col nil)
                   (immediate-win-col nil)
                   (immediate-block-col nil))
              ;; Try immediate win first
              (multiple-value-bind (wb wc) (find-immediate-win new-board 'o)
                (when wb
                  (setq ai-board wb ai-col wc immediate-win-col wc)))
              ;; Then immediate block
              (unless ai-board
                (multiple-value-bind (bb bc) (find-immediate-block new-board 'o)
                  (when bb
                    (setq ai-board bb ai-col bc immediate-block-col bc))))
              ;; Then full minimax
              (unless ai-board
                (multiple-value-bind (b c) (ai-search new-board 'o)
                  (setq ai-board b ai-col c)))
              (let ((status (get-game-status ai-board))
                    (winning (when (won? ai-board 'o) (find-winning-cells ai-board 'o))))
                (cl-json:encode-json-to-string
                 `((:board . ,(board-to-json ai-board))
                   (:status . ,status)
                   (:ai-move . ,ai-col)
                   (:evaluations . ,*static-evaluations*)
                   (:ai-scores . ,ai-scores)
                   (:immediate-win . ,immediate-win-col)
                   (:immediate-block . ,immediate-block-col)
                   ,@(when winning `((:winning-cells . ,winning)))
                   (:message . ,(cond ((equal status "ai_wins") "AI wins!")
                                      ((equal status "draw") "Draw!")
                                      (t "Your turn!"))))))))))))))


;;; Debug endpoint - evaluate board without making a move
(hunchentoot:define-easy-handler (api-debug :uri "/api/debug") (depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (json-data (handler-case
                        (when (and body (> (length body) 0))
                          (cl-json:decode-json-from-string body))
                      (error () nil)))
         (board (json-to-board (cdr (assoc :board json-data))))
         (d (resolve-depth depth)))
    (cond
      ((or (null board) (not (valid-board-p board)))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid or missing board state"))))
      (t
       (let ((*max-depth* d)
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
                  (:immediate-block . ,block-col)))))))))))

;;; Server startup
(defun start-server (&optional (port 8080))
  (let ((server (make-instance 'hunchentoot:easy-acceptor
                               :port port
                               :document-root #p"/app/static/")))
    (hunchentoot:start server)
    (format t "~%Connect-4 Stateless Server running on http://localhost:~a~%" port)
    (format t "Horizontally scalable - no server-side state!~%")
    (force-output)
    server))

;; Auto-start when loaded
(defvar *server* (start-server (let ((port-str (uiop:getenv "PORT")))
                                 (if port-str
                                     (parse-integer port-str :junk-allowed t)
                                     8080))))

;; Keep the process alive (Hunchentoot runs in background threads)
(loop (sleep 86400))
