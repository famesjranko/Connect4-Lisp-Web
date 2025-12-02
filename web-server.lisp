;;;; Connect-4 Web Server - Stateless Version
;;;; Horizontally scalable - no server-side game state

(require :asdf)
(asdf:load-system :hunchentoot)
(asdf:load-system :cl-json)

;;; Load game logic
(load "src/connect-4.lisp")
(load "src/heuristic.lisp")
(load "src/minimax.lisp")

;;; Constants
(defconstant +default-depth+ 4)
(defconstant +board-rows+ 6)
(defconstant +board-columns+ 7)

;;; Global for evaluation count (per-request)
(defvar *static-evaluations* 0)
(defvar *max-depth* +default-depth+)

;;; Board conversion utilities
(defun json-to-board (json-board)
  "Convert JSON board array to internal format"
  (when json-board
    (loop for col-data in json-board
          collect (loop for cell in col-data
                       collect (cond ((equal cell "X") 'x)
                                    ((equal cell "O") 'o)
                                    (t nil))))))

(defun board-to-json (board)
  "Convert internal board to JSON format"
  (loop for col in board
        collect (loop for cell in col
                     collect (cond ((eq cell 'x) "X")
                                  ((eq cell 'o) "O")
                                  (t nil)))))

(defun valid-board-p (board)
  "Check if board is valid structure"
  (and (listp board)
       (= (length board) +board-columns+)
       (every (lambda (col) 
                (and (listp col) (= (length col) +board-rows+)))
              board)))

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
      ((won? new-board player) 50000)
      (t 
       (setf *static-evaluations* 0)
       (let ((result (minimax-a-b-1 new-board 1 (opposite player) 99999 -99999 nil)))
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
   `((:board . ,(board-to-json (copy-tree *empty-board*)))
     (:status . "ongoing")
     (:message . "Your turn!"))))

(hunchentoot:define-easy-handler (api-new-game-ai-first :uri "/api/new-game-ai-first") (depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((board (copy-tree *empty-board*))
         (d (or (and depth (parse-integer depth :junk-allowed t)) +default-depth+))
         (*max-depth* d)
         (*static-evaluations* 0))
    (let* ((ai-scores (evaluate-all-moves board 'o))
           (new-board (minimax-a-b board 0 'o))
           (ai-col (find-ai-move-column board new-board)))
      (cl-json:encode-json-to-string
       `((:board . ,(board-to-json new-board))
         (:status . "ongoing")
         (:ai-move . ,ai-col)
         (:ai-scores . ,ai-scores)
         (:evaluations . ,*static-evaluations*)
         (:message . "AI moved first!"))))))

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
         (d (or (and depth (parse-integer depth :junk-allowed t)) +default-depth+)))
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
            (let ((*max-depth* d)
                  (*static-evaluations* 0))
              (let* ((ai-scores (evaluate-all-moves new-board 'o))
                   (immediate-win-col (let ((win-board (find-immediate-win new-board 'o)))
                                        (when win-board (find-ai-move-column new-board win-board))))
                   (immediate-block-col (let ((block-board (find-immediate-block new-board 'o)))
                                          (when block-board (find-ai-move-column new-board block-board))))
                   (ai-board (or (when immediate-win-col (make-move new-board 'o immediate-win-col))
                                 (when immediate-block-col (make-move new-board 'o immediate-block-col))
                                 (minimax-a-b new-board 0 'o))))
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
                   (:message . ,(cond ((equal status "ai_wins") "AI wins!")
                                      ((equal status "draw") "Draw!")
                                      (t "Your turn!")))))))))))))))

;;; Debug endpoint - evaluate board without making a move
(hunchentoot:define-easy-handler (api-debug :uri "/api/debug") (depth)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (hunchentoot:raw-post-data :force-text t))
         (json-data (handler-case
                        (when (and body (> (length body) 0))
                          (cl-json:decode-json-from-string body))
                      (error () nil)))
         (board (json-to-board (cdr (assoc :board json-data))))
         (d (or (and depth (parse-integer depth :junk-allowed t)) +default-depth+)))
    (cond
      ((or (null board) (not (valid-board-p board)))
       (setf (hunchentoot:return-code*) 400)
       (cl-json:encode-json-to-string '((:error . "Invalid or missing board state"))))
      (t
       (let ((*max-depth* d)
             (*static-evaluations* 0))
         (let* ((ai-scores (evaluate-all-moves board 'o))
                (immediate-win (find-immediate-win board 'o))
                (immediate-block (find-immediate-block board 'o))
                (best-col (first (first (sort (remove-if-not #'second (copy-list ai-scores))
                                              #'> :key #'second)))))
           (cl-json:encode-json-to-string
            `((:depth . ,d)
              (:ai-scores . ,ai-scores)
              (:best-col . ,best-col)
              (:evaluations . ,*static-evaluations*)
              (:immediate-win . ,(when immediate-win (find-ai-move-column board immediate-win)))
              (:immediate-block . ,(when immediate-block (find-ai-move-column board immediate-block)))))))))))

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
