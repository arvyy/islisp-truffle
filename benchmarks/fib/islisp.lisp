(defun fib (n)
    (if (<= n 1)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(let ((n 0))
    (for ((i 0 (+ i 1)))
         ((>= i 1000) (format (standard-output) "~A~%" n))
      (setf n (+ n (fib 30)))))