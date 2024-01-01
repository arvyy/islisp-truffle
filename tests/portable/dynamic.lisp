(requires "testing.lisp")

(defdynamic *dyn1* 1)

(test-equal
    (dynamic *dyn1*)
    1)

(dynamic-let ((*dyn1* 2)
              (*dyn2* 3))
  (test-equal (dynamic *dyn1*) 2)
  (test-equal (dynamic *dyn2*) 3)
  (set-dynamic 4 *dyn1*)
  (test-equal (dynamic *dyn1*) 4))

(test-equal (dynamic *dyn1*) 1)

(format (standard-output) "dynamic.lisp end")
(finish-output (standard-output))
