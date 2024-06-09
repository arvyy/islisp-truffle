(defgeneric fold (collection init reducer))

(defmethod fold ((collection list) init reducer)
  (do ((list collection (cdr list))
       (result init (funcall reducer result (car list))))
      ((null list) result)))

(defmethod fold ((collection simple-vector) init reducer)
  (do ((i 0 (+ i 1))
       (result init (funcall reducer result (elt collection i))))
      ((>= i (length collection)) result)))

(defgeneric add (a b))

(defmethod add ((a integer) (b integer))
    (+ a b))

(defmethod add ((a string) (b string))
    b)

(defun iota (n)
  (do ((list nil (cons i list))
       (i n (- i 1)))
      ((< i 0) list)))

(defun iota-vector (n)
  (let ((vec (make-array (+ n 1))))
    (do ((i 0 (+ i 1)))
        ((>= i (length vec)) vec)
      (setf (elt vec i) i))))

(defglobal biglist (iota 1000))
(defglobal bigvec (iota-vector 1000))
(defglobal bigvec-strings (make-array 1000 :initial-element "a"))

(defun do-main ()
  (let ((result 0))
    (do ((i 0 (+ i 1)))
        ((> i 5) (format t "Result: ~A~%" result))
      (setf result (+ result (fold biglist 0 (lambda (a b) (add a b)))))
      (setf result (+ result (fold bigvec 0 (lambda (a b) (add a b)))))
      (setf result (+ result (length (fold bigvec-strings "" (lambda (a b) (add a b))))))
      )))

(do ((i 0 (+ 1 i)))
    ((> i 1000))
 (do-main))
