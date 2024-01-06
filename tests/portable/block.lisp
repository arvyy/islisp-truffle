(requires "testing.lisp")

(defun foo ()
    (block x
        (+ 10 (return-from x 6) 22)))

(test-equal (foo) 6)

(format-object (standard-output) "block.lisp end" nil)
(finish-output (standard-output))
