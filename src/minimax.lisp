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
   Minimax game playing with alpha-beta pruning and transposition table.

   This file contains functions for doing game-playing search. This
   program will play any game for which the following functions and
   variables are defined:

      (print-board b)
      (movegen pos player)      - returns list of legal column indices
      (opposite player)
      (won? pos player)
      (drawn? pos)
      (make-move pos player col)
      (board-hash board)        - Zobrist hash
      (hash-after-move hash col row player)
      (get-next-blank board col)
      *start*
----------------------------------------------------------------------------|#

;;; Forward declarations to suppress compiler warnings
(declaim (ftype function minimax-a-b-1))
(defvar *static-evaluations* 0)  ; Used for counting evaluations

;;; ==================
;;; TRANSPOSITION TABLE
;;; ==================

(defconstant +tt-size+ (ash 1 20))  ; 1M entries

(defstruct tt-entry
  (hash 0 :type fixnum)
  (depth 0 :type fixnum)
  (score 0)
  (flag :exact :type (member :exact :lower :upper)))

(defvar *tt* (make-array +tt-size+ :initial-element nil))

(defun tt-clear ()
  (fill *tt* nil))

(defun tt-lookup (hash depth)
  (let* ((idx (mod hash +tt-size+))
         (entry (svref *tt* idx)))
    (when (and entry
               (= (tt-entry-hash entry) hash)
               (>= (tt-entry-depth entry) depth))
      entry)))

(defun tt-store (hash depth score flag)
  (let ((idx (mod hash +tt-size+)))
    (setf (svref *tt* idx)
          (make-tt-entry :hash hash :depth depth :score score :flag flag))))

;;; ==================
;;; MINIMAX WITH ALPHA-BETA AND TT
;;; ==================

;; Function MINIMAX-A-B performs minimax search with alpha-beta pruning.
;; Returns (values best-board best-column best-score).

(defun minimax-a-b (pos depth player)
  (tt-clear)
  (minimax-a-b-1 pos depth player 99999 -99999 t (board-hash pos)))

(defun minimax-a-b-1 (pos depth player use-thresh pass-thresh return-move hash)
  (let ((terminal-score (cond ((won? pos player) 50000)
                              ((won? pos (opposite player)) -50000)
                              ((drawn? pos) 0)
                              ((>= depth *max-depth*) (heuristic pos player))
                              (t nil))))
    (cond (terminal-score
           (incf *static-evaluations*)
           (if return-move
               (values nil nil terminal-score)
               terminal-score))
          (t
           ;; Check transposition table
           (let ((tt-hit (tt-lookup hash (- *max-depth* depth))))
             (when (and tt-hit (not return-move))
               (let ((tt-score (tt-entry-score tt-hit))
                     (tt-flag (tt-entry-flag tt-hit)))
                 (cond ((eq tt-flag :exact)
                        (return-from minimax-a-b-1 tt-score))
                       ((eq tt-flag :lower)
                        (when (>= tt-score use-thresh)
                          (return-from minimax-a-b-1 tt-score))
                        (setq pass-thresh (max pass-thresh tt-score)))
                       ((eq tt-flag :upper)
                        (when (<= tt-score pass-thresh)
                          (return-from minimax-a-b-1 tt-score))
                        (setq use-thresh (min use-thresh tt-score)))))))
           (let ((columns (movegen pos player))
                 (best-move nil)
                 (best-col nil)
                 (quit nil)
                 (new-value nil)
                 (orig-pass-thresh pass-thresh))
             (cond ((null columns)
                    (incf *static-evaluations*)
                    (if return-move
                        (values nil nil 0)
                        0))
                   (t
                    (loop for col in columns
                          until quit do
                      (let* ((row (get-next-blank pos col))
                             (succ (make-move pos player col))
                             (new-hash (hash-after-move hash col row player)))
                        (setq new-value (- (minimax-a-b-1 succ (1+ depth)
                                                         (opposite player)
                                                         (- pass-thresh)
                                                         (- use-thresh)
                                                         nil
                                                         new-hash)))
                        (when (> new-value pass-thresh)
                          (setq pass-thresh new-value)
                          (setq best-move succ)
                          (setq best-col col))
                        (when (>= pass-thresh use-thresh) (setq quit t))))
                    ;; Store in transposition table
                    (unless return-move
                      (let ((flag (cond ((>= pass-thresh use-thresh) :lower)
                                       ((<= pass-thresh orig-pass-thresh) :upper)
                                       (t :exact))))
                        (tt-store hash (- *max-depth* depth) pass-thresh flag)))
                    (if return-move
                        (values best-move best-col pass-thresh)
                        pass-thresh))))))))

;;; ==================
;;; PARALLEL ROOT SEARCH
;;; ==================

;; Search the first (center) move sequentially to establish a good bound,
;; then search remaining moves in parallel. Each thread gets its own TT.

(defun minimax-a-b-parallel (pos player)
  "Parallel root-level alpha-beta. Returns (values best-board best-col best-score)."
  (let* ((columns (movegen pos player))
         (hash (board-hash pos))
         (total-evals 0))
    (when (null columns)
      (return-from minimax-a-b-parallel (values nil nil 0)))
    ;; Search first move sequentially to establish alpha-beta bound
    (let* ((first-col (car columns))
           (first-row (get-next-blank pos first-col))
           (first-board (make-move pos player first-col))
           (first-hash (hash-after-move hash first-col first-row player))
           (*static-evaluations* 0))
      (tt-clear)
      (let ((best-score (- (minimax-a-b-1 first-board 1 (opposite player)
                                          99999 -99999 nil first-hash)))
            (best-col first-col)
            (best-board first-board))
        (incf total-evals *static-evaluations*)
        ;; Search remaining moves in parallel
        (when (cdr columns)
          (let ((futures
                  (loop for col in (cdr columns)
                        collect (let ((c col))
                                  (lparallel:future
                                    (let ((*tt* (make-array +tt-size+ :initial-element nil))
                                          (*static-evaluations* 0)
                                          (*max-depth* *max-depth*))
                                      (let* ((r (get-next-blank pos c))
                                             (b (make-move pos player c))
                                             (h (hash-after-move hash c r player))
                                             (score (- (minimax-a-b-1 b 1 (opposite player)
                                                                      (- best-score) -99999
                                                                      nil h))))
                                        (list c b score *static-evaluations*))))))))
            (dolist (f futures)
              (let ((result (lparallel:force f)))
                (destructuring-bind (col board score evals) result
                  (incf total-evals evals)
                  (when (> score best-score)
                    (setq best-score score)
                    (setq best-col col)
                    (setq best-board board)))))))
        (setq *static-evaluations* total-evals)
        (values best-board best-col best-score)))))

;; Function PLAY allows you to play a game against the computer. Call (play)
;; if you want to move first, or (play t) to let the computer move first.

(defun play (&optional machine-first?)
  (let ((b (copy-board *start*))
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
  (let ((b (copy-board *start*)))
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
