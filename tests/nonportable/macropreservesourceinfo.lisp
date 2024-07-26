;; test that use of macros preserves
;; source code location information of sexprs
;; they operate on

(requires "testing.lisp")

(defun foo ()
    (bar))

(defun bar ()
    (funcall asd))

(defmacro my-progn (:rest terms)
  `(progn
     (+ 1 2)
     ,@terms))

(block exit
    (with-handler
        (lambda (condition)
            (report-condition condition (standard-output))
            (return-from exit nil))
        ;; use of my-progn mustn't lose the fact
        ;; the foo call came from line 27
        (my-progn
            (+ 1 2)
            (foo))
        (format (standard-output) "FAIL")))

(format (standard-output) "macropreservesourceinfo.lisp end~%")