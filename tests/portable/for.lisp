(requires "testing.lisp")

(defglobal inputs '())
(defglobal sum
  (let ((x '(1 3 5 7 9)))
     (for ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((not x) sum)
       (setq inputs (cons (car x) inputs)))))

(test-equal sum 25)
(test-equal inputs '(9 7 5 3 1))

(format (standard-output) "for.lisp end")
(finish-output (standard-output))
