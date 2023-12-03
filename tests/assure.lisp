(defmacro test-equal (expr value)
  (let ((actual (gensym)))
    `(let ((,actual ,expr))
        (if (not (equal ,actual ,value))
          (progn
            (format-object (standard-output) ',expr t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Expect: " t)
            (format-object (standard-output) ,value nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Actual: " t)
            (format-object (standard-output) ,actual nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "-------" nil)
            (format-char (standard-output) #\newline))))))

(test-equal (assure <integer> (+ 1 9)) 10)
(test-equal (assure <number> (+ 1 9)) 10)
(block exit
    (with-handler
        (lambda (c)
            (return-from exit 1))
        (assure <number> "string")
        (format (standard-output) "FAIL")))

(test-equal (the <integer> (+ 1 9)) 10)
(test-equal (the <number> (+ 1 9)) 10)
(block exit
    (with-handler
        (lambda (c)
            (return-from exit 1))
        (the <number> "string")
        (format (standard-output) "FAIL")))

(format (standard-output) "assure.lisp end")