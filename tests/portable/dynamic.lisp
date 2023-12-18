(defun print (int)
    (format-integer (standard-output) int 10)
    (format-char (standard-output) #\newline))

(defdynamic *dyn1* 1)

(print (dynamic *dyn1*))

(dynamic-let ((*dyn1* 2)
              (*dyn2* 3))
  (print (dynamic *dyn1*))
  (print (dynamic *dyn2*))
  (set-dynamic 4 *dyn1*)
  (print (dynamic *dyn1*)))

(print (dynamic *dyn1*))

(print (dynamic-let () 5))