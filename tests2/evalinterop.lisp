(defmacro test-equal (expr value)
  (let ((actual (gensym)))
    `(let ((,actual ,expr))
        (if (not (equal ,actual ,value))
          (progn
            (format-object (standard-output) ',expr t)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Expect: " t)
            (format-object (standard-output) ,value nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "Actual: " t)
            (format-object (standard-output) ,actual nil)
            (format-char (standard-output) #\newline)
            (format-object (standard-output) "-------" nil)
            (format-char (standard-output) #\newline))))))

(let ((fun (eval "js" "(function(arg) { return 1 + arg; })")))
  (test-equal (funcall fun 2) 3))

(let ((fun (eval "js" "(function(arr) { return arr[0]; })")))
  (test-equal (funcall fun #(4 5 6)) 4))

(format (standard-output) "evalinterop.lisp end")
