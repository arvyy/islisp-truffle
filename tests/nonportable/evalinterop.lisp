(requires "testing.lisp")

(let ((fun (eval "js" "(function(arg) { return 1 + arg; })")))
  (test-equal (funcall fun 2) 3))

(let ((fun (eval "js" "(function(arr) { return arr[0]; })")))
  (test-equal (funcall fun #(4 5 6)) 4))

(let ((js-array (eval "js" "[1, 2, 3]")))
  (test-equal (instancep js-array (class <truffle-vector>)) t)
  (test-equal (length js-array) 3)
  (test-equal (elt js-array 1) 2)
  (setf (aref js-array 1) 5)
  (test-equal (elt js-array 1) 5))

(let ((obj (eval "js" "({ a: 1 })")))
  (test-equal (instancep obj (class <truffle-object>)) t)
  (test-equal (truffle-object-fields obj) #("a"))
  (test-equal (truffle-object-field obj "a") 1)
  (set-truffle-object-field 2 obj "a")
  (test-equal (truffle-object-field obj "a") 2)
  (setf (truffle-object-field obj "a") 3)
  (test-equal (truffle-object-field obj "a") 3))

(format (standard-output) "evalinterop.lisp end")
(finish-output (standard-output))
