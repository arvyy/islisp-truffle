(requires "testing.lisp")

(test-equal (convert #\a <character>) #\a)
(test-equal (convert 97 <character>) #\a)

(test-equal (convert #\a <integer>) 97)
(test-equal (convert 97 <integer>) 97)
(test-equal (convert "97" <integer>) 97)

(test-equal (convert 97 <float>) 97.0)
(test-equal (convert 97.0 <float>) 97.0)
(test-equal (convert "97.0" <float>) 97.0)

(test-equal (convert #\a <symbol>) 'a)
(test-equal (convert 'a <symbol>) 'a)
(test-equal (convert "a" <symbol>) 'a)

(test-equal (convert 97 <string>) "97")
(test-equal (convert 97.0 <string>) "97.0")
(test-equal (convert 'a <string>) "a")
(test-equal (convert "a" <string>) "a")

(test-equal (convert "97" <general-vector>) #(#\9 #\7))
(test-equal (convert #(1 2) <general-vector>) #(1 2))
(test-equal (convert '(1 2) <general-vector>) #(1 2))
(test-equal (convert '() <general-vector>) #())

(test-equal (convert "97" <list>) '(#\9 #\7))
(test-equal (convert #(1 2) <list>) '(1 2))
(test-equal (convert '(1 2) <list>) '(1 2))
(test-equal (convert '() <list>) '())

(format (standard-output) "convert.lisp end")
(finish-output (standard-output))