(requires "testing.lisp")

(let ((s (create-string-output-stream)))
  (with-standard-output s
    (format s "2"))
  (test-equal (get-output-stream-string s) "2"))

(format-object (standard-output) "withstandardoutput.lisp end" nil)
(finish-output (standard-output))