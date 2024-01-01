(defun print (str)
    (format-object (standard-output) str nil)
    (format-char (standard-output) #\newline))

(defglobal foo "OK1")
(print foo)
(let ((foo "OK2"))
    (print foo))
(print foo)
(setq foo "OK3")
(print foo)


;; test conditions are properly raised when reading unbound variable
(block exit
    (with-handler
        (lambda (condition)
            (if (instancep condition (class <unbound-variable>))
                (print "OK4")
                (print "FAIL4"))
            (return-from exit nil))
        unbound
        (print "FAIL4")))
(finish-output (standard-output))
