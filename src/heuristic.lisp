;; Name: Andrew McDonald
;; Student#: 17961040
;; Date: 22.09.18

;;;; =====================================================
;;;; COMMON LISP IMPLEMENTATION OF HEURISTIC FOR CONNECT-4
;;;; =====================================================

;;; Forward declarations to suppress compiler warnings
(declaim (ftype function heuristic board-value strategy line-score
                imminent-threat? playable-positions best-strategy?
                preferred-positions strategy-eval? preferred-position-score))
(defvar *default-board-values* nil)
(defvar *strategy-a* nil)
(defvar *strategy-b* nil)
(defvar *strategy-board-template* nil)

;;; =========
;;; HEURISTIC
;;; =========

;; STATIC takes a board and a player as inputs and tests what state the
;; current board is for player, returns relavant value.
;; If board wins, returns 50000.
;; If board loses, returns -50000.
;; If board draws, returns 0.
;; Else, runs heuristic function.

(defun static (board player)
  (cond ((won? board player) 50000)
        ((won? board (opposite player)) -50000)
        ((drawn? board) 0)
        (t (heuristic board player))))

;; Define *defensive-weight* as a global.
;; defensive factor is fine tuned for best defensive/offensive play at depth 1.
;; prioritises preventing a loss over choosing losing possible win move.

(defparameter *defensive-weight* 1.91000008)

;; HEURISTIC takes a board and a player as inputs and returns the value
;; of current players strategy and board-value minus the opposite players
;; relative values.
;; 1. When play positions are equal or near to, strategy has greater effect
;; on play decision - eg. players first move.
;; 2. board-value is weighted toward defensive play.

(defun heuristic (board player)
  (let ((playable (playable-positions board)))
    (- (+ (* *defensive-weight* (board-value board player playable))
          (strategy board player))
       (+ (board-value board (opposite player) playable)
          (strategy board (opposite player))))))

;;; =================
;;; HEURISTIC HELPERS
;;; =================

;; BOARD-VALUE takes a board and a player as inputs and maps the list of
;; of possible win combinations onto the function threat and returns the
;; sum of each evaluation.

(defun board-value (board player playable)
  (loop for line in *all-c4-lines*
        sum (line-score board player line playable)))

;; LINE-SCORE takes a board, a player, and a line containing a possible win
;; combination and returns a value for current board depending on how many
;; pieces are currently in win combination and if the current next move in
;; that combination is playable. If none meet conditions, default return value
;; is 0.
;;
;; Returns: 97 if current playable move in line is a possible win move.
;;          9 if line is a possible win move, at a later move.
;;          2 if current playable move in line is a possible unblocked three move.
;;          1 if current playable move in line is a possible unblocked two move.
;;          0 default.

(defun line-score (board player line playable)
  (let* ((1st (aref board (first (first line)) (second (first line))))
         (2nd (aref board (first (second line)) (second (second line))))
         (3rd (aref board (first (third line)) (second (third line))))
         (4th (aref board (first (fourth line)) (second (fourth line))))
         (combo (list 1st 2nd 3rd 4th))
         (pieces (count player combo))
         (imminent (imminent-threat? line playable))
         (not-blocked (not (member (opposite player) combo))))
    (cond ((and (= pieces 3)
                not-blocked
                playable
                imminent) 97)
          ((and (= pieces 3)
                not-blocked
                playable
                (not imminent)) 9)
          ((and (= pieces 2)
                playable
                not-blocked
                imminent) 3)
          ((and (= pieces 1)
                playable
                not-blocked
                imminent) 0)
          (t 0))))

;; IMMINENT-THREAT? takes a line and playable (simple-vector of 7 row indices).
;; Returns true if any position in the line is the next playable row in its column.

(defun imminent-threat? (line playable)
  (let ((p1 (svref playable (first (first line))))
        (p2 (svref playable (first (second line))))
        (p3 (svref playable (first (third line))))
        (p4 (svref playable (first (fourth line)))))
    (cond ((eql (second (first line)) p1))
          ((eql (second (second line)) p2))
          ((eql (second (third line)) p3))
          ((eql (second (fourth line)) p4)))))

;; STRATEGY takes a board and a player as inputs and tests function BEST-STRATEGY?
;; to determine best current strategy and applies relevant board values.

(defun strategy (board player)
  (let ((bs (best-strategy? board player)))
    (cond ((> bs 3)
           (preferred-positions board player *strategy-a*))
          ((< bs -3)
           (preferred-positions board player *strategy-b*))
          (t (preferred-positions board player *default-board-values*)))))

;; BEST-STRATEGY takes a board and a player as inputs and maps the current board
;; state against the strategy board template. Returns relative strategy value of
;; state for player.

(defun best-strategy? (board player)
  (let ((sum 0))
    (dotimes (col 7 sum)
      (dotimes (row 6)
        (let ((pos (aref board col row))
              (strat (aref *strategy-board-template* col row)))
          (when (eql pos player)
            (cond ((eql strat 'a) (incf sum))
                  ((eql strat 'b) (decf sum)))))))))

;; PREFERRED-POSITIONS takes a board, player, and array of board position
;; values. Returns a relative value for players positions on board.

(defun preferred-positions (board player board-values)
  (let ((sum 0))
    (dotimes (col 7 sum)
      (dotimes (row 6)
        (when (eql (aref board col row) player)
          (incf sum (aref board-values col row)))))))

;;; ===============
;;; HEURISTIC UTILS
;;; ===============

;; Function PLAYABLE-POSITIONS takes a board (2D array) and returns a
;; simple-vector of 7 elements, each being the next blank row in that
;; column (or nil if the column is full).

(defun playable-positions (board)
  (let ((pp (make-array 7)))
    (dotimes (col 7 pp)
      (setf (svref pp col)
            (get-next-blank board col)))))

;;; ================================
;;; BOARD VALUES/POSITIONS TEMPLATES
;;; ================================

;; *DEFAULT-BOARD-VALUES* is the default value template for game board
;; position values for strategic play.

(defparameter *default-board-values* ; best!
  ; relative board strategy values
  (make-array '(7 6) :initial-contents
    '((1 1 1 1 1 1)
      (2 1 1 1 1 1)
      (2 1 1 1 1 1)
      (3 3 3 1 1 1)
      (2 1 1 1 1 1)
      (2 1 1 1 1 1)
      (1 1 1 1 1 1))))

;; *STRATEGY-A* is a value template for game board position values
;; for strategic play.

(defparameter *strategy-a*
  (make-array '(7 6) :initial-contents
    '((0 1 0 1 0 1)
      (1 0 1 0 1 0)
      (0 1 0 1 0 1)
      (1 0 1 0 1 0)
      (0 1 0 1 0 1)
      (1 0 1 0 1 0)
      (0 1 0 1 0 1))))

;; *STRATEGY-B* is a value template for game board position values
;; for strategic play.

(defparameter *strategy-b*
  (make-array '(7 6) :initial-contents
    '((1 0 1 0 1 0)
      (0 1 0 1 0 1)
      (1 0 1 0 1 0)
      (0 1 0 1 0 1)
      (1 0 1 0 1 0)
      (0 1 0 1 0 1)
      (1 0 1 0 1 0))))

;; *STRATEGY-BOARD-TEMPLATE* represents two distinct strategy
;; position templates for the game board.

(defparameter *strategy-board-template*
  ; board strategy template
  (make-array '(7 6) :initial-contents
    '((b a b a b a)
      (a b a b a b)
      (b a b a b a)
      (a b a b a b)
      (b a b a b a)
      (a b a b a b)
      (b a b a b a))))
