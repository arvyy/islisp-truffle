(module module1
    (requires foo.module2)
    (provides
        module1-public))

(defun internal () "Hello from module1.lisp")
(defun module1-public () (module2-public))

