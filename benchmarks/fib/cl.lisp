(defun fib (n)
    (if (<= n 1)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

(let ((n 0))
    (do ((i 0 (+ i 1)))
        ((>= i 100) (format t "~A~%" n))
      (setf n (+ n (fib 30)))))
