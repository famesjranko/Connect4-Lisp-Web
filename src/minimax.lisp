#|-----------------------------------------------------------------------------
   Artificial Intelligence, Second Edition
   Elaine Rich and Kevin Knight
   McGraw Hill, 1991

   This code may be freely copied and used for educational or research purposes.
   All software written by Kevin Knight.
   Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
                            MINIMAX SEARCH
                            "minimax.lisp"
----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------
   Minimax game playing.

   This file contains functions for doing game-playing search. This
   program will play any game for which the following functions and
   variables are defined:

      (print-board b)
      (movegen pos player)
      (opposite player)
      (static pos player)
      (won? pos player)
      (drawn? pos)
      (deep-enough pos depth)
      (make-move pos player move)
      *start*

   These functions are implemented for tic-tac-toe in the file
   "tictactoe.lisp".
----------------------------------------------------------------------------|#

;;; Forward declarations to suppress compiler warnings
(declaim (ftype function minimax-a-b-1))
(defvar *static-evaluations* 0)  ; Used for counting evaluations

;; Function MINIMAX performs minimax search from a given position (pos),
;; to a given search ply (depth), for a given player (player). It returns
;; a list of board positions representing what it sees as the best moves for
;; both players. The first element of the list is the value of the board
;; position after the proposed move.

(defun minimax (pos depth player)
  (cond ((deep-enough pos depth)
         (setq *static-evaluations* (+ *static-evaluations* 1))
         (list (static pos player)))
        (t
         (let ((successors (movegen pos player))
               (best-score -99999)
               (best-path nil))
           (cond ((null successors)
                  (setq *static-evaluations* (+ *static-evaluations* 1))
                  (list (static pos player)))
                 (t
                  (loop for succ in successors do
                    (let* ((result-succ (minimax succ (1+ depth)
                                                 (opposite player)))
                           (new-value (- (car result-succ))))
                      (when (> new-value best-score)
                        (setq best-score new-value)
                        (setq best-path succ ))))
                  (cons best-score best-path)))))))

(defvar *static-evaluations* 0)

;; Function MINIMAX-A-B performs minimax search with alpha-beta pruning.
;; It is far more efficient than MINIMAX.

(defun minimax-a-b (pos depth player)
  (minimax-a-b-1 pos depth player 99999 -99999 t))

(defun minimax-a-b-1 (pos depth player use-thresh pass-thresh return-move)
  (cond ((deep-enough pos depth)
         (setq *static-evaluations* (+ *static-evaluations* 1))
         (unless return-move (static pos player)))
        (t
         (let ((successors (movegen pos player))
               (best-move nil)
               (quit nil)
               (new-value nil))
           (declare (dynamic-extent successors))
           (cond ((null successors)
                  (setq *static-evaluations* (+ *static-evaluations* 1))
                  (unless return-move (static pos player)))
                 (t
                  (loop for succ in successors
                        until quit do
                    (setq new-value (- (minimax-a-b-1 succ (1+ depth)
                                                     (opposite player)
                                                     (- pass-thresh)
                                                     (- use-thresh)
                                                     nil)))
                    (when (> new-value pass-thresh)
                      (setq pass-thresh new-value)
                      (setq best-move succ))
                    (when (>= pass-thresh use-thresh) (setq quit t)))
                  (if return-move best-move pass-thresh)))))))

;; Function PLAY allows you to play a game against the computer. Call (play)
;; if you want to move first, or (play t) to let the computer move first.

(defun play (&optional machine-first?)
  (let ((b *start*)
        (next nil))
    (setq *static-evaluations* 0)
    (when machine-first?
      (setq b (minimax-a-b b 0 'o)))
    (do ()
        ((or (won? b 'x) (won? b 'o) (drawn? b))
         (format t "Final position: ~%")
         (print-board b)
         (cond ((won? b 'o) (format t "I win.~%"))
               ((won? b 'x) (format t "You win.~%"))
               (t (format t "Drawn.~%")))
         *static-evaluations*)
      (print-board b)
      (format t "~a Your move: " *static-evaluations*)
      (let ((m (read)))
        (loop until (setq next (make-move b 'x m))
              do (format t "~%~a Illegal move, Try again: " m)
              (setq m (read)))
        (setq b next))
      (when (and (not (drawn? b))
                 (not (won? b 'o))
                 (not (won? b 'x)))
        (print-board b)
        (setq b (minimax-a-b b 0 'o))
        (if (and (not (drawn? b))
                 (not (won? b 'o)))
            (format t "My move: ~%"))))))

(defun self-play ()
  (let ((b *start*))
    (setq *static-evaluations* 0)
    (do ()
        ((or (won? b 'x) (won? b 'o) (drawn? b))
         (format t "Final position: ~%")
         (print-board b)
         (cond ((won? b 'o) (format t "O win.~%"))
               ((won? b 'x) (format t "X win.~%"))
               (t (format t "Drawn.~%")))
         *static-evaluations*)
      (print-board b)
      (format t "~a X move: " *static-evaluations*)
      (setq b (minimax-a-b b 0 'x))
      (when (and (not (drawn? b))
                 (not (won? b 'o))
                 (not (won? b 'x)))
        (print-board b)
        (setq b (minimax-a-b b 0 'o))
        (if (and (not (drawn? b))
                 (not (won? b 'o)))
            (format t "~a 0 move: " *static-evaluations*))))))
