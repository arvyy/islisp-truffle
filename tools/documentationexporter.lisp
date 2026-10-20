(requires "builtin/introspection.lisp")

(defun assoc* (key alist)
    (let ((e (assoc key alist)))
        (if e
            (cdr e)
            nil)))

(defun print-heading (ns name)
    (format (standard-output) "== [.namspace_~A]_~A_ +~A+~%" ns ns name))

(defun print-value-type (vt)
    (format (standard-output)
"[subs=\"+quotes,+replacements\"]
----
~A
----~%~%" vt))

(defun print-syntax (stx return-type)
    (format (standard-output)
"[subs=\"+quotes,+replacements\"]
----
~A" stx)
    (if return-type
        (format (standard-output) "=> ~A" return-type))
    (format (standard-output) "~%----~%~%"))

(defun print-entry (e)
    (block print-entry
        (let* ((namespace (elt e 0))
               (name (elt e 1))
               (doc (elt e 2))
               (desc (assoc* 'desc doc))
               (stx (assoc* 'syntax doc))
               (return-type (assoc* 'return-type doc))
               (value-type (assoc* 'value-type doc)))
            ;;(format (standard-output) "== +~A+~%~%~A~%~%" name desc)
            (print-heading namespace name)
            (if stx
                (print-syntax stx return-type))
            (if value-type
                (print-value-type value-type))
            (format (standard-output) "~%~%~A~%~%" desc)
            )))

(defun print-module (name)
    (let* ((m (lookup-module name))
           (entries (list-module-documentations m)))
      (mapc #'print-entry (convert entries <list>))))

#|
(let ((modules '("ROOT" "builtin/truffle.lisp" "builtin/introspection.lisp")))
    (format (standard-output) "= Docs ~%:icons: font~%:toc: left~%:stem: latexmath~%~%")
    (mapc #'print-module modules))
|#

(if (= (length (dynamic *args*)) 0)
    (format (error-output) "Missing args.~%Usage:~%    documentationexporter modulename")
    (print-module (elt (dynamic *args*) 0)))
