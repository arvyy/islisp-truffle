(requires "testing.lisp")

(defun foo1 (a) a)
(test-equal (foo1 "OK1") "OK1")

(defun foo2 (a :rest b)
    (car b))

(test-equal (foo2 "FAIL2" "OK2") "OK2")

(defun foo3 (a &rest b)
    (car b))

(test-equal (foo3 "FAIL3" "OK3") "OK3")

(defun foo4 ()
    "OK4")

(test-equal (foo4) "OK4")

;; test conditions are properly raised on bad arg count
(block exit
    (with-handler
        (lambda (condition)
            (test-equal (instancep condition (class <program-error>)) t)
            (return-from exit nil))
        (foo1)
        (test-equal t nil)))


(format (standard-output) "defun.lisp end")
(finish-output (standard-output))