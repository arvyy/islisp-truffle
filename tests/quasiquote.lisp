(defun print (obj)
    (format-object (standard-output) obj nil)
    (format-char (standard-output) #\newline))

(print `(1 2))
(print `(1 ,(+ 1 2)))
(print `(1 ,@(progn '(2 3))))

;; nested quasiquote representation seems to be underspecified,
;; use a macro defining macro to portably test nested quasiquote behavior.
(defmacro m1 (a)
  `(defmacro m2 (b)
      `(print '(,,a ,b))))

(m1 1)
(m2 2)