#|-*- mode:lisp -*-|#
(require :uiop)
(load "~/quicklisp/setup.lisp")
;;(ql:quickload "split-sequence")
(ql:quickload "str" :silent t)
(ql:quickload "cl-ppcre" :silent t)

(defvar *day2-str* nil)
(defvar *item-limits* (list 12 13 14))
(defvar *dictionary* (list (cons "red" 0) (cons "green" 1) (cons "blue" 2)) )

(defun parse-string (s)
    (loop for (name . idx) in *dictionary*
        if (string= name s) return idx
    )
)

;; Get the day2 input
(setf *day2-str* (
    uiop:read-file-lines (
        merge-pathnames #p"aoc2023/day2.txt" (user-homedir-pathname)
    )
))

;; a round is a list of (red, green, blue)

;; turn line into game
(defun line_to_games (line)
    (mapcar (lambda (rounds) (
        let ((tokens (str:split ", " rounds))
             (person (list 0 0 0)))
        (mapcar (lambda (token-section) (
            let ((num-label (str:split " " token-section)))
            (setf (elt person (parse-string (second num-label) ) ) (parse-integer (first num-label)))
        )) tokens)
        person
    )) line)
)

;; turn lines into games
(defun lines_to_games (lines)
    (mapcar (lambda (line) (
        let* ((toks (str:split ": " line))
              (game (second (str:split " " (first toks) )) )
              (rest (second toks)))
        (list (parse-integer game) (line_to_games (str:split "; " rest)))
    )) lines)
)

;; does a round use to many items
(defun is_round_valid (round) (
    every (lambda (idx) (
        <= (nth idx round) (nth idx *item-limits*)
    )) (list 0 1 2) 
))

;; do all rounds in a game use an acceptable number of items
(defun is_game_valid (game) (
    every (lambda (round) (
        is_round_valid round
    )) (second game)
))

;; filter by valid games, then sum game ids 
(defun valid_id_sum (games) (
    apply '+ (mapcar (lambda (game) (
        first game
    )) (remove-if-not #'is_game_valid games ))    
))

;; determine the max of an item used over some rounds
(defun max_item (item-idx rounds) ( apply #'max (
    mapcar (lambda (round) (
        nth item-idx round
    )) rounds)
))

;; the maxes for each item in a round
(defun maxes_in_rounds (rounds) (
    mapcar (lambda (item-idx) (
        max_item item-idx rounds
    )) (list 0 1 2))
)

;; the maxes for each item in all games
(defun maxes_in_games (games) (
    mapcar (lambda (game) (
        maxes_in_rounds (second game)
    )) games)
)

;; compute the powers for each max set of items, then sum
(defun sum_powers (games) (
    apply '+ (mapcar (lambda (maxes) (
        apply '* maxes
    )) (maxes_in_games games))    
))



;;(format t "~a ~%" (valid_id_sum (lines_to_games *day2-str*)))
(format t "~a ~%" (sum_powers (lines_to_games *day2-str*)))

