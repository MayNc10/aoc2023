#|-*- mode:lisp -*-|#
(require :uiop)
(load "~/quicklisp/setup.lisp")
(defvar *day1-str* nil)
(defvar *dictionary*
  (loop for i from 1 to 9 collect (cons (format nil "~R" i) i)))

(defun parse-string (s)
    (loop for (name . number) in *dictionary*
        if (string= name s) return number
    )
)

;; Get the day1 input
(setf *day1-str* (
    uiop:read-file-lines (
        merge-pathnames #p"aoc2023/day1.txt" (user-homedir-pathname)
    )
))

;; take split strings, turn them into numbers
(defun lines_to_calibs-p1 (lines)
    (mapcar (lambda (line) (
        let* ((chars (coerce line 'list))
            ;;remove items that aren't numbers
             (chars-filtered (remove-if-not (lambda (c) (digit-char-p c 10)) chars)) )

            (parse-integer 
                (format nil "~a~a" (first chars-filtered) (car (last chars-filtered)) )
            )
    )) lines)
)

;; take split strings, turn them into numbers
(defun lines_to_calibs-p2 (lines)
    (mapcar (lambda (line) (
        let* ((chars (coerce line 'list))
               (digit nil)
               (word nil) 
               (inv_idx nil)
               (len nil)
               )

        (format t "line: ~a ~%" line)

        (parse-integer (format NIL "~a~a" 
            (
                dotimes (idx (length chars))
                ( 
                dotimes (strlen (min 10 (- (length chars) idx))) 
                    (setq len (+ strlen 1))
                    (setq digit (digit-char-p (nth idx chars) 10))
                    (if digit (return digit))
                    (setq word (parse-string (coerce (subseq chars idx (+ idx len) ) 'string) ) )
                    (if word (return word))
                )
                (if digit (return digit))
                (if word (return word))
            )
            (
                dotimes (idx (length chars)) 
                ( 
                dotimes (strlen (min 10 (+ idx 1))) ;; Is idx bc we're reverse indexing 
                    (setq len (+ strlen 1))
                    (setq inv_idx (- (length chars) (+ 1 idx) ) )
                    (setq digit (digit-char-p (nth inv_idx chars) 10))
                    (if digit (return digit))
                    (setq word (parse-string (coerce (subseq chars inv_idx (+ inv_idx len) ) 'string) ) )
                    (if word (return word))
                )
                (if digit (return digit))
                (if word (return word))
            ) 
        ))
    )) lines)
)

(format t "~a ~%" (reduce #'+ (lines_to_calibs-p2 *day1-str*)) )
