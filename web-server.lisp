;;;; web-server.lisp - HTTP API for Connect-4 game

(ql:quickload '(:hunchentoot :cl-json :bordeaux-threads) :silent t)

;;; Load game files
(locally
  (declare (sb-ext:muffle-conditions style-warning warning))
  (handler-bind ((style-warning #'muffle-warning)
                 (warning #'muffle-warning))
    (load "src/connect-4.lisp")
    (load "src/minimax.lisp")
    (load "src/heuristic.lisp")))

;;; Constants
(defconstant +default-depth+ 4)
(defconstant +min-depth+ 1)
(defconstant +max-depth-limit+ 10)
(defconstant +board-columns+ 7)
(defconstant +game-max-age+ 3600)
(defconstant +cleanup-interval+ 300)

;;; Game state
(defstruct game-state board depth timestamp)
(defvar *games* (make-hash-table :test 'equal))
(defvar *games-lock* (bordeaux-threads:make-lock "games-lock"))
(defvar *server-start-time* (get-universal-time))
(defvar *acceptor* nil)
(defvar *cleanup-thread* nil)

(defun make-game-id ()
  (format nil "~A-~A-~A" (get-universal-time) (random 1000000000) (random 1000000000)))

(defun get-game (id)
  (bordeaux-threads:with-lock-held (*games-lock*)
    (let ((game (gethash id *games*)))
      (when game (game-state-board game)))))

(defun get-game-depth (id)
  (bordeaux-threads:with-lock-held (*games-lock*)
    (let ((game (gethash id *games*)))
      (if game (game-state-depth game) +default-depth+))))

(defun set-game (id board &optional (depth +default-depth+))
  (bordeaux-threads:with-lock-held (*games-lock*)
    (let ((existing (gethash id *games*)))
      (if existing
          (setf (game-state-board existing) board
                (game-state-timestamp existing) (get-universal-time))
          (setf (gethash id *games*)
                (make-game-state :board board :depth depth :timestamp (get-universal-time)))))))

(defun set-game-depth (id depth)
  (bordeaux-threads:with-lock-held (*games-lock*)
    (let ((game (gethash id *games*)))
      (when game (setf (game-state-depth game) depth)))))

(defun cleanup-old-games ()
  (let ((now (get-universal-time)) (removed 0))
    (bordeaux-threads:with-lock-held (*games-lock*)
      (maphash (lambda (id game)
                 (when (> (- now (game-state-timestamp game)) +game-max-age+)
                   (remhash id *games*)
                   (incf removed)))
               *games*))
    (when (> removed 0) (format t "~&Cleaned up ~A games~%" removed))))

(defun active-game-count ()
  (bordeaux-threads:with-lock-held (*games-lock*)
    (hash-table-count *games*)))

(defun start-cleanup-thread ()
  (setf *cleanup-thread*
        (bordeaux-threads:make-thread
         (lambda () (loop (sleep +cleanup-interval+) (cleanup-old-games)))
         :name "game-cleanup")))

;;; Board utilities
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

(defun find-immediate-win (board player)
  (dolist (col '(3 2 4 1 5 0 6))
    (let ((new-board (make-move board player col)))
      (when (and new-board (won? new-board player))
        (return-from find-immediate-win new-board))))
  nil)

(defun find-immediate-block (board player)
  (let ((opponent (opposite player)))
    (dolist (col '(3 2 4 1 5 0 6))
      (let ((opp-board (make-move board opponent col)))
        (when (and opp-board (won? opp-board opponent))
          (let ((block-board (make-move board player col)))
            (when block-board
              (return-from find-immediate-block block-board)))))))
  nil)

(defun find-ai-move-column (old-board new-board)
  (loop for col from 0 below +board-columns+
        when (not (equal (nth col old-board) (nth col new-board)))
        return col))

(defun find-winning-cells (board player)
  (dolist (line *all-c4-lines*)
    (when (check-line board player line)
      (return-from find-winning-cells
        (mapcar (lambda (pos) (list (first pos) (second pos))) line))))
  nil)

;;; Move evaluation (for debug)
(defun evaluate-move (board player col)
  (let ((new-board (make-move board player col)))
    (cond
      ((null new-board) nil)
      ((won? new-board player) 50000)  ; Immediate win - exactly 50000
      (t 
       (setf *static-evaluations* 0)
       (let ((result (minimax-a-b-1 new-board 1 (opposite player) 99999 -99999 nil)))
         (if (numberp result) 
             (let ((score (- result)))
               ;; Cap forced wins at 49000 to distinguish from immediate wins
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
  (cl-json:encode-json-to-string
   `((:status . "ok")
     (:active-games . ,(active-game-count))
     (:default-depth . ,+default-depth+))))

(hunchentoot:define-easy-handler (api-new-game :uri "/api/new-game") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((game-id (make-game-id))
         (board (copy-tree *empty-board*)))
    (set-game game-id board +default-depth+)
    (cl-json:encode-json-to-string
     `((:game-id . ,game-id)
       (:board . ,(board-to-json board))
       (:status . "ongoing")
       (:message . "Game started!")))))

(hunchentoot:define-easy-handler (api-new-game-ai-first :uri "/api/new-game-ai-first") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((game-id (make-game-id))
         (board (copy-tree *empty-board*)))
    (setf *max-depth* +default-depth+)
    (setf *static-evaluations* 0)
    ;; Evaluate all moves on empty board (for debug display)
    (let* ((ai-scores (evaluate-all-moves board 'o))
           (new-board (minimax-a-b board 0 'o))
           (ai-col (find-ai-move-column board new-board)))
      (set-game game-id new-board +default-depth+)
      (cl-json:encode-json-to-string
       `((:game-id . ,game-id)
         (:board . ,(board-to-json new-board))
         (:status . "ongoing")
         (:ai-move . ,ai-col)
         (:ai-scores . ,ai-scores)
         (:evaluations . ,*static-evaluations*)
         (:message . "AI moved first!"))))))

(hunchentoot:define-easy-handler (api-move :uri "/api/move") (game_id column)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((board (get-game game_id))
        (col (and column (parse-integer column :junk-allowed t))))
    (cond
      ((null board)
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid game ID"))))
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
            (set-game game_id new-board)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "player_wins")
               (:winning-cells . ,(find-winning-cells new-board 'x))
               (:message . "You win!"))))
           ((drawn? new-board)
            (set-game game_id new-board)
            (cl-json:encode-json-to-string
             `((:board . ,(board-to-json new-board))
               (:status . "draw")
               (:message . "Draw!"))))
           (t
            (setf *max-depth* (get-game-depth game_id))
            (setf *static-evaluations* 0)
            ;; Evaluate all moves BEFORE making the AI move (for debug display)
            (let* ((ai-scores (evaluate-all-moves new-board 'o))
                   (immediate-win-col (let ((win-board (find-immediate-win new-board 'o)))
                                        (when win-board (find-ai-move-column new-board win-board))))
                   (immediate-block-col (let ((block-board (find-immediate-block new-board 'o)))
                                          (when block-board (find-ai-move-column new-board block-board))))
                   (ai-board (or (when immediate-win-col (make-move new-board 'o immediate-win-col))
                                 (when immediate-block-col (make-move new-board 'o immediate-block-col))
                                 (minimax-a-b new-board 0 'o))))
              (set-game game_id ai-board)
              (let ((status (get-game-status ai-board))
                    (ai-col (find-ai-move-column new-board ai-board))
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
                   (:message . ,(if (string= status "ai_wins") "AI wins!" "Your turn")))))))))))))

(hunchentoot:define-easy-handler (api-set-depth :uri "/api/set-depth") (game_id depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((d (and depth (parse-integer depth :junk-allowed t))))
    (cond
      ((or (null d) (< d +min-depth+) (> d +max-depth-limit+))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string 
        `((:error . ,(format nil "Depth must be ~A-~A" +min-depth+ +max-depth-limit+)))))
      ((and game_id (get-game game_id))
       (set-game-depth game_id d)
       (cl-json:encode-json-to-string `((:depth . ,d) (:game-id . ,game_id))))
      (t
       (cl-json:encode-json-to-string `((:depth . ,d)))))))

;;; Debug API
(hunchentoot:define-easy-handler (api-debug :uri "/api/debug") (game_id)
  (setf (hunchentoot:content-type*) "application/json")
  (let ((board (get-game game_id)))
    (cond
      ((null board)
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid game ID"))))
      (t
       (setf *max-depth* (get-game-depth game_id))
       (let* ((ai-scores (evaluate-all-moves board 'o))
              (immediate-win (find-immediate-win board 'o))
              (immediate-block (find-immediate-block board 'o))
              (best-col (when ai-scores
                          (first (first (sort (copy-list ai-scores) 
                                              #'(lambda (a b) 
                                                  (let ((sa (second a)) (sb (second b)))
                                                    (and sa sb (> sa sb))))))))))
         (cl-json:encode-json-to-string
          `((:game-id . ,game_id)
            (:depth . ,(get-game-depth game_id))
            (:ai-scores . ,ai-scores)
            (:best-col . ,best-col)
            (:immediate-win . ,(when immediate-win (find-ai-move-column board immediate-win)))
            (:immediate-block . ,(when immediate-block (find-ai-move-column board immediate-block))))))))))

;;; Server control
(defun start-server (&optional (port 8080))
  (when *acceptor* (hunchentoot:stop *acceptor*))
  (start-cleanup-thread)
  (setf *acceptor* 
        (make-instance 'hunchentoot:easy-acceptor 
                       :port port
                       :document-root #p"/app/static/"))
  (hunchentoot:start *acceptor*)
  (format t "~%=== Connect-4 Server on port ~A ===~%" port))

(defun stop-server ()
  (when *cleanup-thread*
    (bordeaux-threads:destroy-thread *cleanup-thread*)
    (setf *cleanup-thread* nil))
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))

;;; Startup
(start-server 8080)
(loop (sleep 3600))
