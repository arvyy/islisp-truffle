(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(let ((s (create-string-output-stream)))
  (format-object s "OK2" nil)
  (print "OK1")
  (print (get-output-stream-string s))
  (print (get-output-stream-string s)) ;; call second time to check it was cleared on first call
  (print "OK3"))

(finish-output (standard-output))