(requires "testing.lisp")

(defglobal foo "OK1")
(test-equal foo "OK1")
(let ((foo "OK2"))
    (test-equal foo "OK2"))
(test-equal foo "OK1")
(setq foo "OK3")
(test-equal foo "OK3")


;; test conditions are properly raised when reading unbound variable
(block exit
    (with-handler
        (lambda (condition)
            (test-equal (instancep condition (class <unbound-variable>)) t)
            (return-from exit nil))
        unbound
        (test-equal nil t)))

(format (standard-output) "defglobal.lisp end")
(finish-output (standard-output))
