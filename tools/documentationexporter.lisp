(requires "builtin/introspection.lisp")

(defun assoc* (key alist)
    (let ((e (assoc key alist)))
        (if e
            (cdr e)
            nil)))

(defun print-heading (kind name)
    (format (standard-output) "== +~A+~%~%[.islisp-kind]_~A_~%~%" name kind))

(defun print-value-syntax (name type)
    (format (standard-output)
"[subs=\"verbatim\"]
----
~A => ~A
----~%~%" name type))

(defun print-class-info (abstract superclasses slots)
    (if abstract
        (format (standard-output) "*Abstract*~%~%"))
    (if superclasses
        (progn
            (format (standard-output) ".Superclasses~%")
            (mapc 
                (lambda (cls)
                    (format (standard-output) "* ~A~%" cls))
                superclasses)
            (format (standard-output) "~%")))
    (if slots
        (progn
            (format (standard-output) ".Slots~%")
            (mapc 
                (lambda (slot)
                    (format (standard-output) "* ~A~%" slot))
                slots)
            (format (standard-output) "~%"))))

(defun print-syntax (stx return-type)
    (format (standard-output)
"[subs=\"+quotes\"]
----
~A" stx)
    (if return-type
        (format (standard-output) " => ~A" return-type))
    (format (standard-output) "~%----~%~%"))

(defun print-entry (e)
    (block print-entry
        (let* ((namespace (elt e 0))
               (name (elt e 1))
               (doc (elt e 2))
               (kind (assoc* 'kind doc))
               (desc (assoc* 'desc doc))
               (stx (assoc* 'syntax doc))
               (return-type (assoc* 'return-type doc))
               (value-type (assoc* 'value-type doc))
               (abstract (assoc* 'abstract doc))
               (superclasses (assoc* 'superclasses doc))
               (slots (assoc* 'slots doc)))
            (print-heading kind name)
            (if (equal 'class kind)
                (print-class-info abstract superclasses slots))
            (if stx
                (print-syntax stx return-type))
            (if value-type
                (print-value-syntax name value-type))
            (format (standard-output) "~%~%~A~%~%" desc))))

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
