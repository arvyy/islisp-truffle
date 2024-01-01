(defun print (str)
    (format-object (standard-output) str nil))

(let ((s (create-string-output-stream)))
  (with-standard-output s
    (print "2"))
  (print "1")
  (print (get-output-stream-string s)))
(finish-output (standard-output))
