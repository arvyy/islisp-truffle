(requires "testing.lisp")

(let* ((lib (load-native-library "libm.so.6"))
       (libm-sin (native-library-symbol lib "sin" "(DOUBLE):DOUBLE")))
  (test-equal (> (funcall libm-sin 2.0) 0.909) t)
  (test-equal (< (funcall libm-sin 2.0) 0.910) t))

(format (standard-output) "nif.lisp end")
(finish-output (standard-output))
