(defparameter *empty-board*
  '((nil nil nil nil nil nil)
    (nil nil nil nil nil nil)
    (nil nil nil nil nil nil)
    (nil nil nil nil nil nil)
    (nil nil nil nil nil nil)
    (nil nil nil nil nil nil)
    (nil nil nil nil nil nil)))

(defparameter *start* *empty-board*)

;; Predicate LEGAL-MOVE? takes a board position (board), a player (player, which
;; is 'x or 'o), and a move (which is a number between 1 and 6 inclusive). It returns
;; T if the move is legal, and nil otherwise.
;(defun legal-move? (pos player move)
;  (eq (nth move pos) nil))

(defun legal-move? (board player move)
  (when (member move '(0 1 2 3 4 5 6))
    (get-next-blank (nth move board))))

(defun get-next-blank (col)
  (cond ((equalp (nth 0 col) nil) 0)
        ((equalp (nth 1 col) nil) 1)
        ((equalp (nth 2 col) nil) 2)
        ((equalp (nth 3 col) nil) 3)
        ((equalp (nth 4 col) nil) 4)
        ((equalp (nth 5 col) nil) 5)
        (t nil)))

;; Function MAKE-MOVE takes a board position (board), a player (player, which
;; is 'x or 'o), and a move (which is a number between 1 and 6 inclusive). If the move
;; is legal it returns a new board position.
;; Note that copy-tree is a built-in function.

(defun make-move (board player move)
  (when (legal-move? board player move)
    (let* ((b (copy-tree board))
           (next-blank (get-next-blank (nth move b))))
      (setf (nth next-blank (nth move b)) player)
      b)))

;; Function MOVEGEN takes a board and a player and generates all legal
;; successor boards, i.e., all possible moves a player could make.

(defun movegen (board player)
  ;; Prioritize center columns for better strategy
  ;; After cons reversal, order becomes: 3, 2, 4, 1, 5, 0, 6 (center first)
  (movegen-helper board player '(6 0 5 1 4 2 3)))

;; MOVEGEN-HELPER Iterates through the complete list of moves.
;; Whenever a legal move is found,that move is cons'ed into the list moves

(defun movegen-helper (board player lst)
  (let ((moves ())) ; local variable moves will be used to store the list
                    ; of possible moves
    (dolist (m lst)
      (when (legal-move? board player m)
        (setf moves (cons (make-move board player m) moves))))
    moves))

;-----------------------------
;;; printing utilities

(defun getval(board i j)
  (nth j (nth i board)))

(defun print-board(board)
  (format t "~%---------------")
  (loop for j from 5 downto 0 do
    (format t "~%|")
    (loop for i from 0 to 6 do
      (format t "~A|" (if (eq (getval board i j) 'NIL)
                          " "
                          (getval board i j))))
    (format t "~%---------------"))
  (format t "~% 0 1 2 3 4 5 6 ~%~%"))

;-----------------------------------------

;; Function WON? returns t is pos is a winning position for player,
;; nil otherwise.

(defun won?(board player)
  (check-for-win board player *all-c4-lines*))

(defun check-for-win(board player lines)
  (cond ((null lines) '())
        ((check-line board player (car lines)) T)
        (t (check-for-win board player (cdr lines)))))

(defun check-line(board player line)
  (let ((1st (first line))
        (2nd (second line))
        (3rd (third line))
        (4th (fourth line)))
    (and (eql (nth (second 1st) (nth (first 1st) board)) player)
         (eql (nth (second 2nd) (nth (first 2nd) board)) player)
         (eql (nth (second 3rd) (nth (first 3rd) board)) player)
         (eql (nth (second 4th) (nth (first 4th) board)) player))))

;; Function DRAWN? returns t if pos is a drawn position, i.e., if there are
;; no more moves to be made.

(defun drawn?(board)
  (not (not-drawn? board)))

(defun not-drawn?(board)
  (or (equalp (nth 5 (nth 0 board)) nil)
      (equalp (nth 5 (nth 1 board)) nil)
      (equalp (nth 5 (nth 2 board)) nil)
      (equalp (nth 5 (nth 3 board)) nil)
      (equalp (nth 5 (nth 4 board)) nil)
      (equalp (nth 5 (nth 5 board)) nil)))

;; Function OPPOSITE returns 'x when given 'o, and vice-versa.

(defun opposite(player)
  (if (eq player 'x) 'o 'x))

;; Function DEEP-ENOUGH takes a board position and a depth and returns
;; t if the search has proceeded deep enough.

(defparameter *max-depth* 1)

(defun deep-enough (board depth)
  (or (won? board 'x)
      (won? board 'o)
      (drawn? board)
      (>= depth *max-depth*)))

(defparameter *all-c4-lines*
  ;;; This is a list of all 69 possible ways to win in.
  ;;; Each element of the list is a list of length four such as
  ;;; ((3 5) (4 5) (5 5) (6 5))
  ;;; Each element of the 69 elements is the column and row
  ;;; For example, the above list indicates that there a line
  ;;; of length four that includes a piece at 3rd column of the 5th row.
  ;;; The 4th column of the 5th row, the 5th column of the 5th row
  ;;; and the 6th column of the 5th row
  ;;; rows and columns start at 0, like arrays in lisp
  '(((0 0) (0 1) (0 2) (0 3))
    ((0 1) (0 2) (0 3) (0 4))
    ((0 2) (0 3) (0 4) (0 5))
    ((1 0) (1 1) (1 2) (1 3))
    ((1 1) (1 2) (1 3) (1 4))
    ((1 2) (1 3) (1 4) (1 5))
    ((2 0) (2 1) (2 2) (2 3))
    ((2 1) (2 2) (2 3) (2 4))
    ((2 2) (2 3) (2 4) (2 5))
    ((3 0) (3 1) (3 2) (3 3))
    ((3 1) (3 2) (3 3) (3 4))
    ((3 2) (3 3) (3 4) (3 5))
    ((4 0) (4 1) (4 2) (4 3))
    ((4 1) (4 2) (4 3) (4 4))
    ((4 2) (4 3) (4 4) (4 5))
    ((5 0) (5 1) (5 2) (5 3))
    ((5 1) (5 2) (5 3) (5 4))
    ((5 2) (5 3) (5 4) (5 5))
    ((6 0) (6 1) (6 2) (6 3))
    ((6 1) (6 2) (6 3) (6 4))
    ((6 2) (6 3) (6 4) (6 5))
    ((0 0) (1 0) (2 0) (3 0))
    ((0 1) (1 1) (2 1) (3 1))
    ((0 2) (1 2) (2 2) (3 2))
    ((0 3) (1 3) (2 3) (3 3))
    ((0 4) (1 4) (2 4) (3 4))
    ((0 5) (1 5) (2 5) (3 5))
    ((1 0) (2 0) (3 0) (4 0))
    ((1 1) (2 1) (3 1) (4 1))
    ((1 2) (2 2) (3 2) (4 2))
    ((1 3) (2 3) (3 3) (4 3))
    ((1 4) (2 4) (3 4) (4 4))
    ((1 5) (2 5) (3 5) (4 5))
    ((2 0) (3 0) (4 0) (5 0))
    ((2 1) (3 1) (4 1) (5 1))
    ((2 2) (3 2) (4 2) (5 2))
    ((2 3) (3 3) (4 3) (5 3))
    ((2 4) (3 4) (4 4) (5 4))
    ((2 5) (3 5) (4 5) (5 5))
    ((3 0) (4 0) (5 0) (6 0))
    ((3 1) (4 1) (5 1) (6 1))
    ((3 2) (4 2) (5 2) (6 2))
    ((3 3) (4 3) (5 3) (6 3))
    ((3 4) (4 4) (5 4) (6 4))
    ((3 5) (4 5) (5 5) (6 5))
    ((0 0) (1 1) (2 2) (3 3))
    ((0 1) (1 2) (2 3) (3 4))
    ((0 2) (1 3) (2 4) (3 5))
    ((1 0) (2 1) (3 2) (4 3))
    ((1 1) (2 2) (3 3) (4 4))
    ((1 2) (2 3) (3 4) (4 5))
    ((2 0) (3 1) (4 2) (5 3))
    ((2 1) (3 2) (4 3) (5 4))
    ((2 2) (3 3) (4 4) (5 5))
    ((3 0) (4 1) (5 2) (6 3))
    ((3 1) (4 2) (5 3) (6 4))
    ((3 2) (4 3) (5 4) (6 5))
    ((0 3) (1 2) (2 1) (3 0))
    ((0 4) (1 3) (2 2) (3 1))
    ((0 5) (1 4) (2 3) (3 2))
    ((1 3) (2 2) (3 1) (4 0))
    ((1 4) (2 3) (3 2) (4 1))
    ((1 5) (2 4) (3 3) (4 2))
    ((2 3) (3 2) (4 1) (5 0))
    ((2 4) (3 3) (4 2) (5 1))
    ((2 5) (3 4) (4 3) (5 2))
    ((3 3) (4 2) (5 1) (6 0))
    ((3 4) (4 3) (5 2) (6 1))
    ((3 5) (4 4) (5 3) (6 2))))
