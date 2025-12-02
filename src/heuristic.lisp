;; Name: Andrew McDonald
;; Student#: 17961040
;; Date: 22.09.18

;;;; =====================================================
;;;; COMMON LISP IMPLEMENTATION OF HEURISTIC FOR CONNECT-4
;;;; =====================================================

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
  (- (+ (* *defensive-weight* (board-value board player))
        (strategy board player))
     (+ (board-value board (opposite player))
        (strategy board (opposite player)))))

;;; =================
;;; HEURISTIC HELPERS
;;; =================

;; BOARD-VALUE takes a board and a player as inputs and maps the list of
;; of possible win combinations onto the function threat and returns the
;; sum of each evaluation.

(defun board-value (board player)
  (sum (mapcar #'(lambda(x) (line-score board player x)) *all-c4-lines*)))

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

(defun line-score (board player line)
  (let* ((1st (nth (second (first line)) (nth (first (first line)) board)))
         (2nd (nth (second (second line)) (nth (first (second line)) board)))
         (3rd (nth (second (third line)) (nth (first (third line)) board)))
         (4th (nth (second (fourth line)) (nth (first (fourth line)) board)))
         (combo (list 1st 2nd 3rd 4th))
         (pieces (count player combo))
         (playable (playable-positions board))
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

;; IMMINENT-THREAT? takes two lists as input, line and playable. Line contains
;; a list of a possible win combination and playable contains a list of the
;; current open moves available for the board. Possible win combination is mapped
;; to playable and if equal returns true. Meaning, that the current possible win
;; combo is within the range of next possible moves.

(defun imminent-threat? (line playable)
  (let ((playable1 (nth (first (first line)) playable))
        (playable2 (nth (first (second line)) playable))
        (playable3 (nth (first (third line)) playable))
        (playable4 (nth (first (fourth line)) playable)))
    (cond ((eql (second (first line)) (second playable1)))
          ((eql (second (second line)) (second playable2)))
          ((eql (second (third line)) (second playable3)))
          ((eql (second (fourth line)) (second playable4))))))

;; STRATEGY takes a board and a player as inputs and tests function BEST-STRATEGY?
;; to determine best current strategy and applies relevant board values.

(defun strategy (board player)
  (cond ((> (best-strategy? board player) 3)
         (preferred-positions board player *strategy-a*))
        ((< (best-strategy? board player) -3)
         (preferred-positions board player *strategy-b*))
        (t (preferred-positions board player *default-board-values*))))

;; BEST-STRATEGY takes a board and a player as inputs and maps the current board
;; state against the strategy board template. Returns relative strategy value of
;; state for player.

(defun best-strategy? (board player)
  (sum (mapcar #'(lambda(x) (strategy-eval? board player *strategy-board-template* x))
               *board-positions-template*)))

;; STRATEGY-EVAL? takes a board, player, a strategy board template, and a board
;; positions template as inputs and checks the players positions against the the
;; strategy template. Returns a relative weight value +1 or -1 for each player
;; pieces on given strategy position.

(defun strategy-eval? (board player strategy column)
  (let ((pos (nth (second column) (nth (first column) board)))
        (strat (nth (second column) (nth (first column) strategy))))
    (cond ((and (eql strat 'a) (eql pos player)) 1)
          ((and (eql strat 'b) (eql pos player)) -1)
          (t 0))))

;; PREFERRED-POSITIONS takes a board, player, and list representing board position
;; values and is mapped onto function preferred-position? using a list representing
;; board co-ordinates. Returns a relative value for players positions on board.

(defun preferred-positions (board player board-values)
  (sum (mapcar #'(lambda(x) (preferred-position-score board player board-values x))
               *board-positions-template*)))

;; PREFERRED-POSITION-SCORE takes a board, player, board-values, and a board position,
;; checks if player has piece has a on board-position; if yes, returns the relevant
;; position value from board-values.

(defun preferred-position-score (board player board-values pos)
  (let ((play-position (nth (second pos) (nth (first pos) board)))
        (val (nth (second pos) (nth (first pos) board-values))))
    (if (eql play-position player) val 0)))

;;; ===============
;;; HEURISTIC UTILS
;;; ===============

;; Function PLAYABLE-POSITIONS takes a list representing a board and returns a list
;; containing the positions of the first nil elements of the board.

(defun playable-positions (board)
  (mapcar #'list
          '(0 1 2 3 4 5 6)
          (list (position nil (first board))
                (position nil (second board))
                (position nil (third board))
                (position nil (fourth board))
                (position nil (fifth board))
                (position nil (sixth board))
                (position nil (seventh board)))))

;; Function SUM takes a list of numbers and returns the sum of the elements of the
;; list.

(defun sum(lis)
  (cond ((null lis) 0)
        (t (+ (car lis)
              (sum (cdr lis))))))

;;; ================================
;;; BOARD VALUES/POSITIONS TEMPLATES
;;; ================================

;; *DEFAULT-BOARD-VALUES* is the default value template for game board
;; position values for strategic play.

(defparameter *default-board-values* ; best!
  ; relative board strategy values
  '((1 1 1 1 1 1)
    (2 1 1 1 1 1)
    (2 1 1 1 1 1)
    (3 3 3 1 1 1)
    (2 1 1 1 1 1)
    (2 1 1 1 1 1)
    (1 1 1 1 1 1)))

;; *STRATEGY-A* is a value template for game board position values
;; for strategic play.

(defparameter *strategy-a*
  '((0 1 0 1 0 1)
    (1 0 1 0 1 0)
    (0 1 0 1 0 1)
    (1 0 1 0 1 0)
    (0 1 0 1 0 1)
    (1 0 1 0 1 0)
    (0 1 0 1 0 1)))

;; *STRATEGY-B* is a value template for game board position values
;; for strategic play.

(defparameter *strategy-b*
  '((1 0 1 0 1 0)
    (0 1 0 1 0 1)
    (1 0 1 0 1 0)
    (0 1 0 1 0 1)
    (1 0 1 0 1 0)
    (0 1 0 1 0 1)
    (1 0 1 0 1 0)))

;; *STRATEGY-BOARD-TEMPLATE* represents two distinct strategy
;; position templates for the game board.

(defparameter *strategy-board-template*
  ; board strategy template
  '((b a b a b a)
    (a b a b a b)
    (b a b a b a)
    (a b a b a b)
    (b a b a b a)
    (a b a b a b)
    (b a b a b a)))

;; *board-positions-template* is a list of the board positions relative
;; to their position within the array.

(defparameter *board-positions-template*
  '((0 0)(0 1)(0 2)(0 3)(0 4)(0 5)
    (1 0)(1 1)(1 2)(1 3)(1 4)(1 5)
    (2 0)(2 1)(2 2)(2 3)(2 4)(2 5)
    (3 0)(3 1)(3 2)(3 3)(3 4)(3 5)
    (4 0)(4 1)(4 2)(4 3)(4 4)(4 5)
    (5 0)(5 1)(5 2)(5 3)(5 4)(5 5)
    (6 0)(6 1)(6 2)(6 3)(6 4)(6 5)))
