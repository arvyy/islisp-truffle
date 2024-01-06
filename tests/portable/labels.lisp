(requires "testing.lisp")

(labels ((evenp (n)
           (if (= n 0)
               t
               (oddp (- n 1))))
         (oddp (n)
           (if (= n 0)
               nil
               (evenp (- n 1)))))
    (test-equal (evenp 88) t))

(format (standard-output) "labels.lisp end")
(finish-output (standard-output))
