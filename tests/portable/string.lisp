(requires "testing.lisp")

(test-equal (create-string 3 #\1) "111")
(test-equal (string= (create-string 3 #\1) "111") t)

(test-equal (string= "abc" "abc") t)
(test-equal (string= "abc" "abb") nil)
(test-equal (string= "abc" "abcd") nil)

(test-equal (string/= "abc" "abc") nil)
(test-equal (string/= "abc" "abb") t)
(test-equal (string/= "abc" "abcd") t)

(test-equal (string> "a" "a") nil)
(test-equal (string> "a" "b") nil)
(test-equal (string> "b" "a") t)
(test-equal (string> "a" "aa") nil)
(test-equal (string> "aa" "a") t)

(test-equal (string>= "a" "a") t)
(test-equal (string>= "a" "b") nil)
(test-equal (string>= "b" "a") t)
(test-equal (string>= "a" "aa") nil)
(test-equal (string>= "aa" "a") t)

(test-equal (string< "a" "a") nil)
(test-equal (string< "a" "b") t)
(test-equal (string< "b" "a") nil)
(test-equal (string< "a" "aa") t)
(test-equal (string< "aa" "a") nil)

(test-equal (string<= "a" "a") t)
(test-equal (string<= "a" "b") t)
(test-equal (string<= "b" "a") nil)
(test-equal (string<= "a" "aa") t)
(test-equal (string<= "aa" "a") nil)

;; sequence; test mutable and immutable separately
(test-equal (elt "789" 0) #\7)
(test-equal (elt (create-string 3 #\7) 0) #\7)
(block exit
    (with-handler
        (lambda (condition)
            (test-equal (instancep condition (class <program-error>)) t)
            (return-from exit nil))
        (elt "123" 4)
        (print "FAIL")))
(block exit
    (with-handler
        (lambda (condition)
            (test-equal (instancep condition (class <program-error>)) t)
            (return-from exit nil))
        (elt (create-string 3) 4)
        (print "FAIL")))

(test-equal (length "ab") 2)
(test-equal (length (create-string 2 #\7)) 2)
(let ((str (create-string 1 #\0)))
  (setf (elt str 0) #\1)
  (test-equal str "1"))

;; append
(test-equal (string-append "12" "34") "1234")
(test-equal (string-append "12" "" "34" "5") "12345")
(test-equal (string-append "12" (create-string 1 #\3)) "123")
(test-equal (string-append) "")

(format-object (standard-output) "string.lisp end" nil)
(finish-output (standard-output))
