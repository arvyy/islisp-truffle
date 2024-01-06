(requires "testing.lisp")

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

;; sequence
(test-equal (elt "789" 0) #\7)
(test-equal (length "ab") 2)

(format-object (standard-output) "string.lisp end" nil)
(finish-output (standard-output))
