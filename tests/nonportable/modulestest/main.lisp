(requires
    "testing.lisp"
    "module1.lisp")

(test-equal (module1-public) "Hello from module2.lisp")

(format (standard-output) "modulestest end")