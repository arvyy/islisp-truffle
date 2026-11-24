(requires
    testing
    module1)

(test-equal (module1-public) "Hello from module2.lisp")

(format (standard-output) "modulestest end")
(finish-output (standard-output))
