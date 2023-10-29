(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(print
  (let ((x '(1 3 5 7 9)))
     (for ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((not x) sum)
       (print (car x)))))