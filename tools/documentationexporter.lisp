(requires "builtin/introspection.lisp")

(defun print-entry (e)
    (block print-entry
        (let* ((doc (elt e 2))
               (name (elt e 1))
               (desc (cdr (assoc 'desc doc))))
            (format (standard-output) "=== +~A+~%~%~A~%~%" name desc))))

(defun print-module (name)
    (format (standard-output) "== ~A ~%" name)
    (let* ((m (lookup-module name))
           (entries (list-module-documentations m)))
      (mapc #'print-entry (convert entries <list>))))

(let ((modules '("ROOT" "builtin/truffle.lisp" "builtin/introspection.lisp")))
    (format (standard-output) "= Docs ~%:icons: font~%:toc: left~%:stem: latexmath~%~%")
    (mapc #'print-module modules))
