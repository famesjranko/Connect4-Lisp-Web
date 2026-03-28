;;; Forward declarations to suppress compiler warnings
(declaim (ftype function check-for-win check-line not-drawn?))
(defvar *all-c4-lines* nil)  ; Forward declaration - defined later in file

;;; Board is a 2D array: (make-array '(7 6)) indexed as (aref board col row)
;;; Values: 'x, 'o, or nil (empty). Row 0 = bottom, row 5 = top.

;;; ==================
;;; ZOBRIST HASHING
;;; ==================

;; Random keys for incremental board hashing.
;; *zobrist-keys* is indexed as (aref *zobrist-keys* col row piece-index)
;; where piece-index 0 = 'x, 1 = 'o.
(defparameter *zobrist-keys*
  (let ((keys (make-array '(7 6 2)))
        (state (make-random-state t)))
    (dotimes (c 7)
      (dotimes (r 6)
        (dotimes (p 2)
          (setf (aref keys c r p) (random (ash 1 62) state)))))
    keys))

(defun piece-index (player)
  (if (eq player 'x) 0 1))

(defun board-hash (board)
  "Compute full Zobrist hash for a board position."
  (let ((h 0))
    (dotimes (c 7 h)
      (dotimes (r 6)
        (let ((piece (aref board c r)))
          (when piece
            (setf h (logxor h (aref *zobrist-keys* c r (piece-index piece))))))))))

(defun hash-after-move (hash col row player)
  "Incrementally update hash after placing player at (col, row)."
  (logxor hash (aref *zobrist-keys* col row (piece-index player))))

(defun make-empty-board ()
  (make-array '(7 6) :initial-element nil))

(defun copy-board (board)
  (let ((new (make-array '(7 6))))
    (dotimes (i 42 new)
      (setf (row-major-aref new i) (row-major-aref board i)))))

(defparameter *empty-board* (make-empty-board))
(defparameter *start* (make-empty-board))

(defun legal-move? (board player move)
  (declare (ignore player))
  (and (integerp move) (<= 0 move 6)
       (null (aref board move 5))))

(defun get-next-blank (board col)
  (dotimes (row 6 nil)
    (when (null (aref board col row))
      (return row))))

;; Function MAKE-MOVE takes a board, player, and column index.
;; If the move is legal it returns a new board with the piece placed.

(defun make-move (board player move)
  (when (legal-move? board player move)
    (let* ((b (copy-board board))
           (next-blank (get-next-blank board move)))
      (setf (aref b move next-blank) player)
      b)))

;; Function MOVEGEN returns list of legal column indices, center-first.

(defun movegen (board player)
  (declare (ignore player))
  (loop for col in '(3 2 4 1 5 0 6)
        when (null (aref board col 5))
        collect col))

;-----------------------------
;;; printing utilities

(defun getval(board i j)
  (aref board i j))

(defun print-board(board)
  (format t "~%---------------")
  (loop for j from 5 downto 0 do
    (format t "~%|")
    (loop for i from 0 to 6 do
      (format t "~A|" (let ((v (aref board i j)))
                         (if (null v) " " v))))
    (format t "~%---------------"))
  (format t "~% 0 1 2 3 4 5 6 ~%~%"))

;-----------------------------------------

;; Function WON? returns t if pos is a winning position for player,
;; nil otherwise.

(defun won?(board player)
  (check-for-win board player *all-c4-lines*))

(defun check-for-win(board player lines)
  (cond ((null lines) nil)
        ((check-line board player (car lines)) t)
        (t (check-for-win board player (cdr lines)))))

(defun check-line(board player line)
  (let ((1st (first line))
        (2nd (second line))
        (3rd (third line))
        (4th (fourth line)))
    (and (eql (aref board (first 1st) (second 1st)) player)
         (eql (aref board (first 2nd) (second 2nd)) player)
         (eql (aref board (first 3rd) (second 3rd)) player)
         (eql (aref board (first 4th) (second 4th)) player))))

;; Function DRAWN? returns t if pos is a drawn position, i.e., if there are
;; no more moves to be made.

(defun drawn?(board)
  (not (not-drawn? board)))

(defun not-drawn?(board)
  (dotimes (col 7 nil)
    (when (null (aref board col 5))
      (return t))))

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
  ;;; This is a list of all 69 possible ways to win.
  ;;; Each element is ((col row) (col row) (col row) (col row))
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
