(requires "testing.lisp")
(requires "builtin/introspection.lisp")

(let* ((m (assure <module> (lookup-module "builtin/truffle.lisp")))
       (exports (list-module-declarations m)))
    (test-equal (length exports) 5))
