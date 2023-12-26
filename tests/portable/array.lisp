(requires "testing.lisp")

(let ((arr #2a((1 2) (3 4))))
  (test-equal (aref arr 0 0) 1)
  (test-equal (aref arr 0 1) 2)
  (test-equal (aref arr 1 0) 3)
  (test-equal (aref arr 1 1) 4)

  (set-aref 5 arr 0 0)
  (test-equal (aref arr 0 0) 5)
  (test-equal (aref arr 0 1) 2)

  (setf (aref arr 0 0) 6)
  (test-equal (aref arr 0 0) 6)
  (test-equal (aref arr 0 1) 2))

(let ((v #(1 2)))
  (test-equal (aref v 0) 1)

  (set-aref 2 v 0)
  (test-equal (aref v 0) 2)
  (setf (aref v 0) 3)
  (test-equal (aref v 0) 3))

(let ((arr (create-array '(2 2) 1)))
  (test-equal (aref arr 0 0) 1)
  (test-equal (aref arr 0 1) 1)
  (test-equal (aref arr 1 0) 1)
  (test-equal (aref arr 1 1) 1)
  (test-equal (general-array*-p arr) t))

(test-equal (general-vector-p (create-array '(1))) t)

(format-object (standard-output) "array.lisp end" nil)