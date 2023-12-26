(provides
    module2-public)

(defun internal () "Hello from module2.lisp")
(defun module2-public () (internal))