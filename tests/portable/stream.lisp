(requires "testing.lisp")

(test-equal (streamp (create-string-input-stream "foo")) t)
(test-equal (streamp (create-string-output-stream)) t)
(test-equal (streamp 123) nil)

(test-equal (input-stream-p (create-string-input-stream "foo")) t)
(test-equal (input-stream-p (create-string-output-stream)) nil)
(test-equal (input-stream-p 123) nil)

(test-equal (output-stream-p (create-string-input-stream "foo")) nil)
(test-equal (output-stream-p (create-string-output-stream)) t)
(test-equal (output-stream-p 123) nil)

(test-equal (output-stream-p (standard-output)) t)
(test-equal (output-stream-p (error-output)) t)
(test-equal (input-stream-p (standard-input)) t)

(format (standard-output) "stream.lisp end")