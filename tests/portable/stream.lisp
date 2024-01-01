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

(let ((stream (create-string-input-stream "a")))
  (test-equal (read-char stream) #\a)
  (test-equal (read-char stream nil) nil))

(let ((stream (create-string-input-stream "a")))
  (test-equal (read-char stream) #\a)
  (test-equal (read-char stream nil 'foo) 'foo))

(let ((stream (create-string-input-stream "a")))
  (test-equal (read-char stream) #\a)
  (block exit
    (with-handler
      (lambda (err)
        (test-equal (instancep err (class <end-of-stream>)) t)
        (return-from exit nil))
      (read-char stream t)
      (test-equal nil t)))) ;; shouldn't get here

(let ((stream (create-string-input-stream "hello #(1 2 3) 123 #\\A")))
  (test-equal (read stream) 'hello)
  (test-equal (read stream) #(1 2 3))
  (test-equal (read stream) 123)
  (test-equal (read stream) #\A)
  (test-equal (read stream nil "the end") "the end"))

(format (standard-output) "stream.lisp end")
(finish-output (standard-output))
