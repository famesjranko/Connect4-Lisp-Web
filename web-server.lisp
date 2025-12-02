;;;; web-server.lisp - HTTP API for Connect-4 game

(ql:quickload '(:hunchentoot :cl-json) :silent t)

;;; Load original game files (suppress forward-reference warnings)
(locally
  (declare (sb-ext:muffle-conditions style-warning warning))
  (handler-bind ((style-warning #'muffle-warning)
                 (warning #'muffle-warning))
    (load "src/connect-4.lisp")
    (load "src/minimax.lisp")
    (load "src/heuristic.lisp")))

;;; Set AI depth
(setf *max-depth* 4)

;;; Game state storage
(defvar *games* (make-hash-table :test 'equal))

(defun make-game-id ()
  (format nil "~A-~A" (get-universal-time) (random 100000)))

(defun get-game (id)
  (gethash id *games*))

(defun set-game (id board)
  (setf (gethash id *games*) board))

;;; Board serialization
(defun board-to-json (board)
  (mapcar (lambda (col)
            (mapcar (lambda (cell)
                      (cond ((null cell) nil)
                            ((eq cell 'x) "X")
                            ((eq cell 'o) "O")
                            (t (string cell))))
                    col))
          board))

(defun get-game-status (board)
  (cond ((won? board 'x) "player_wins")
        ((won? board 'o) "ai_wins")
        ((drawn? board) "draw")
        (t "ongoing")))

(defun find-ai-move-column (old-board new-board)
  (loop for col from 0 to 6
        when (not (equal (nth col old-board) (nth col new-board)))
        return col))

;;; HTTP handlers
(defvar *acceptor* nil)

(hunchentoot:define-easy-handler (api-health :uri "/api/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string
   `((:status . "ok") (:depth . ,*max-depth*))))

(hunchentoot:define-easy-handler (api-new-game :uri "/api/new-game") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((game-id (make-game-id))
         (board (copy-tree *empty-board*)))
    (set-game game-id board)
    (setf *static-evaluations* 0)
    (cl-json:encode-json-to-string
     `((:game-id . ,game-id)
       (:board . ,(board-to-json board))
       (:status . "ongoing")
       (:message . "Game started. You are X!")))))

(hunchentoot:define-easy-handler (api-new-game-ai-first :uri "/api/new-game-ai-first") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((game-id (make-game-id))
         (board (copy-tree *empty-board*)))
    (setf *static-evaluations* 0)
    (let ((new-board (minimax-a-b board 0 'o)))
      (set-game game-id new-board)
      (cl-json:encode-json-to-string
       `((:game-id . ,game-id)
         (:board . ,(board-to-json new-board))
         (:status . "ongoing")
         (:ai-move . ,(find-ai-move-column board new-board))
         (:message . "AI moved first. Your turn!"))))))

(hunchentoot:define-easy-handler (api-move :uri "/api/move") (game_id column)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((board (get-game game_id))
        (col (and column (parse-integer column :junk-allowed t))))
    (cond
      ((null board)
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid game ID"))))
      ((or (null col) (< col 0) (> col 6))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid column"))))
      (t
       (let ((new-board (make-move board 'x col)))
         (cond
           ((null new-board)
            (setf (hunchentoot:return-code*) 400)
            (cl-json:encode-json-to-string '((:error . "Column is full"))))
           ((won? new-board 'x)
            (set-game game_id new-board)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "player_wins")
               (:message . "You win!"))))
           ((drawn? new-board)
            (set-game game_id new-board)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "draw")
               (:message . "Draw!"))))
           (t
            (let ((ai-board (minimax-a-b new-board 0 'o)))
              (set-game game_id ai-board)
              (let ((status (get-game-status ai-board))
                    (ai-col (find-ai-move-column new-board ai-board)))
                (cl-json:encode-json-to-string
                 `((:board . ,(board-to-json ai-board))
                   (:status . ,status)
                   (:ai-move . ,ai-col)
                   (:evaluations . ,*static-evaluations*)
                   (:message . ,(if (string= status "ai_wins") "AI wins!" "Your turn")))))))))))))

(hunchentoot:define-easy-handler (api-set-depth :uri "/api/set-depth") (depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((d (and depth (parse-integer depth :junk-allowed t))))
    (if (and d (>= d 1) (<= d 6))
        (progn
          (setf *max-depth* d)
          (cl-json:encode-json-to-string `((:depth . ,d))))
        (progn
          (setf (hunchentoot:return-code*) 400)
          (cl-json:encode-json-to-string '((:error . "Depth must be 1-6")))))))

;;; Server control
(defun start-server (&optional (port 8080))
  (when *acceptor* (hunchentoot:stop *acceptor*))
  (setf *acceptor* 
        (make-instance 'hunchentoot:easy-acceptor 
                       :port port
                       :document-root #p"/app/static/"))
  (hunchentoot:start *acceptor*)
  (format t "~%=================================~%")
  (format t "Connect-4 Server running on port ~A~%" port)
  (format t "Open http://localhost:~A to play!~%" port)
  (format t "=================================~%"))

(defun stop-server ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))

;;; Start and keep running
(start-server 8080)
(loop (sleep 3600))
