(defgeneric fold (collection init reducer))

(defmethod fold ((collection <list>) init reducer)
  (for ((list collection (cdr list))
        (result init (funcall reducer result (car list))))
       ((null list) result)))

(defmethod fold ((collection <basic-vector>) init reducer)
  (for ((i 0 (+ i 1))
        (result init (funcall reducer result (elt collection i))))
       ((>= i (length collection)) result)))

(defmethod fold (collection init reducer)
  init)

(defgeneric add (a b))

(defmethod add ((a <integer>) (b <integer>))
    (+ a b))

(defmethod add ((a <string>) (b <string>))
    b)

(defun iota (n)
  (for ((list nil (cons i list))
        (i n (- i 1)))
       ((< i 0) list)))

(defun iota-vector (n)
  (let ((vec (create-vector (+ n 1))))
    (for ((i 0 (+ i 1)))
         ((>= i (length vec)) vec)
      (setf (elt vec i) i))))

(defglobal biglist (iota 1000))
(defglobal bigvec (iota-vector 1000))
(defglobal bigvec-strings (create-vector 1000 "a"))

(defun do-main ()
  (let ((result 0))
    (for ((i 0 (+ i 1)))
         ((> i 50) (format (standard-output) "Result: ~A~%" result))
      (setf result (+ result (fold biglist 0 (lambda (a b) (add a b)))))
      (setf result (+ result (fold bigvec 0 (lambda (a b) (add a b)))))
      (setf result (+ result (length (fold bigvec-strings "" (lambda (a b) (add a b))))))
      )))

(for ((i 0 (+ 1 i)))
     ((> i 1000))
  (do-main))