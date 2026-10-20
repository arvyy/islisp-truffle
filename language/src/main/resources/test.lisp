(defmacro trace (:rest args)
  (let ((result (gensym)))
    `(let ((,result (,@args)))
       (format (error-output) "TRACE ~A = ~A~%" ',args ,result)
       ,result)))

(defmacro or  (:rest args)
  (if (= 0 (length args))
      nil
      (if (= 1 (length args))
          (car args)
          (let ((first (car args))
                (var (gensym))
                (rest (cdr args)))
            `(let ((,var ,first))
                (if ,var ,var (or ,@rest)))))))

(defmacro and (:rest args)
  (if (= 0 (length args))
      t
      (if (= 1 (length args))
          (car args)
          (let ((first (car args))
                (rest (cdr args)))
            `(if ,first
                 (and ,@rest)
                 nil)))))

(defdynamic
    *stacktrace-formatter*
    (lambda (stream stacktrace)
        (for ((i 0 (+ 1 i))
              (len (length stacktrace)))
             ((>= i len))
          (let* ((el (elt stacktrace i))
                 (name (elt el 0))
                 (start-line (elt el 1)))
            (format stream "    at ~A:~A~%" name start-line)))))

(defun print-stacktrace (stream condition)
    (let ((stacktrace (condition-stacktrace condition)))
        (if stacktrace
            (funcall (dynamic *stacktrace-formatter*) stream stacktrace))))

(defclass <serious-condition> ()
    ((stacktrace :reader condition-stacktrace :writer set-condition-stacktrace)
     (continuable? :reader condition-continuable :writer set-condition-continuable)))

(defclass <error> (<serious-condition>) ())

(defclass <truffle-interop-error> (<error>)
    ((message :reader truffle-interop-error-message :initarg message)))

(defclass <io-error> (<error>)
    ((message :reader io-error-message :initarg message)))

(defclass <arithmetic-error> (<error>) ())
(defclass <division-by-zero> (<arithmetic-error>) ())
(defclass <floating-point-overflow> (<arithmetic-error>) ())
(defclass <floating-point-underflow> (<arithmetic-error>) ())

(defclass <stream-error> (<error>) ())

(defclass <end-of-stream> (<stream-error>) ())

(defclass <simple-error> (<error>)
    ((format-string :reader simple-error-format-string :initarg format-string)
     (format-arguments :reader simple-error-format-arguments :initarg format-arguments)))

(defun error (error-string :rest obj)
    (signal-condition
        (create (class <simple-error>)
                'format-string error-string
                'format-arguments obj)
        nil))

(defun cerror (continue-string error-string :rest obj)
    (signal-condition
        (create (class <simple-error>)
                'format-string error-string
                'format-arguments obj)
        (let ((str (create-string-output-stream)))
          (funcall #'format str continue-string obj)
          (get-output-stream-string str))))

(defclass <program-error> (<error>) ())

(defclass <immutable-binding-error> (<program-error>)
    ((binding :reader immutable-binding-name :initarg binding)))

(defclass <conversion-error> (<error>)
    ((value :reader conversion-error-value :initarg value)
     (to :reader conversion-error-to :initarg to)))

(defclass <index-out-of-range-error> (<program-error>)
    ((bounds :reader index-out-of-range-error-bounds :initarg bounds)
     (actual :reader index-out-of-range-error-actual :initarg actual)))

(defclass <domain-error> (<program-error>)
    ((message :reader domain-error-message :initarg message)
     (object :reader domain-error-object :initarg object)
     (expected-class :reader domain-error-expected-class :initarg expected-class)))

(defclass <arity-error> (<program-error>)
    ((actual :reader arity-error-actual-count :initarg actual)
     (required-min :reader arity-error-required-min :initarg required-min)
     (required-max :reader arity-error-required-max :initarg required-max)))

(defclass <no-next-method-error> (<program-error>) ())

(defclass <undefined-entity> (<program-error>)
    ((name :reader undefined-entity-name :initarg name)
     (namespace :reader undefined-entity-namespace :initarg namespace)))

(defclass <unbound-variable> (<undefined-entity>)
    ((name :reader unbound-variable-name :initarg name)))

(defclass <undefined-function> (<undefined-entity>)
    ((name :reader undefined-function-name :initarg name)))

(defgeneric fill-stacktrace (condition))
(defmethod fill-stacktrace ((condition <serious-condition>))
    t)

(defgeneric report-condition (condition stream))
(defmethod report-condition ((condition <serious-condition>) (stream <stream>))
    nil)

(defmethod report-condition ((condition <undefined-entity>) (stream <stream>))
    (format stream "Undefined entity ~A in namespace ~A~%" (undefined-entity-name condition) (undefined-entity-namespace condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <arity-error>) (stream <stream>))
    (let ((actual (arity-error-actual-count condition))
          (min (arity-error-required-min condition))
          (max (arity-error-required-max condition)))
      (cond
        ((= max min) (format stream "Expected ~A args; received ~A~%" min actual))
        ((and (/= max -1) (> min 0)) (format stream "Expected between ~A and ~A args; received ~A~%" min max actual))
        ((> min 0) (format stream "Expected at least ~A args; received ~A~%" min actual))
        (t (format stream "Expected at most ~A args; received ~A~%" max actual))))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <unbound-variable>) (stream <stream>))
    (format-object stream "Unbound variable: " nil)
    (format-object stream (unbound-variable-name condition) nil)
    (format-char stream #\newline)
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <undefined-function>) (stream <stream>))
    (format-object stream "Undefined function: " nil)
    (format-object stream (undefined-function-name condition) nil)
    (format-char stream #\newline)
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <division-by-zero>) (stream <stream>))
    (format-object stream "Division by zero" nil)
    (format-char stream #\newline)
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <index-out-of-range-error>) (stream <stream>))
    (format stream "Index ~D out of bounds (must be between 0 and ~D exclusive)~%"
            (index-out-of-range-error-actual condition)
            (index-out-of-range-error-bounds condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <conversion-error>) (stream <stream>))
    (format stream "Cannot convert ~A to ~A~%" (conversion-error-value condition) (conversion-error-to condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <no-next-method-error>) (stream <stream>))
    (format stream "No next method available~%")
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <truffle-interop-error>) (stream <stream>))
    (format stream "Error during truffle interop call: ~A~%" (truffle-interop-error-message condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <io-error>) (stream <stream>))
    (format stream "IO error: ~A~%" (io-error-message condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <immutable-binding-error>) (stream <stream>))
    (format stream "Attempted to change immutable binding ~A~%" (immutable-binding-name condition))
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <simple-error>) (stream <stream>))
    (apply #'format stream (simple-error-format-string condition) (simple-error-format-arguments condition))
    (format-char stream #\newline)
    (print-stacktrace stream condition))

(defmethod report-condition ((condition <domain-error>) (stream <stream>))
    (format-object stream (domain-error-message condition) nil)
    (format stream ". Expected a value of class ~S, got ~S.~%"
        (domain-error-expected-class condition)
        (domain-error-object condition))
    (print-stacktrace stream condition))

(defun min (first :rest xs)
  (for ((value first (let ((x (car xs)))
                       (if (< x value)
                           x
                           value)))
        (xs xs (cdr xs)))
       ((null xs) value)))

(defun max (first :rest xs)
  (for ((value first (let ((x (car xs)))
                       (if (> x value)
                           x
                           value)))
        (xs xs (cdr xs)))
       ((null xs) value)))

(defun reciprocal (arg)
  (quotient 1.0 arg))

(defun lcm (a b)
    (if (or (= a 0) (= b 0))
        0
        (quotient (abs (* a b)) (gcd a b))))

(declaim (inline /=))
(defun /= (x1 x2)
  (not (= x1 x2)))

(declaim (inline >=))
(defun >= (x1 x2)
  (not (> x2 x1)))

(declaim (inline <))
(defun < (x1 x2)
  (> x2 x1))

(declaim (inline <=))
(defun <= (x1 x2)
  (not (> x1 x2)))

(declaim (inline char/=))
(defun char/= (x1 x2)
  (not (char= x1 x2)))

(declaim (inline char>=))
(defun char>= (x1 x2)
  (not (char< x1 x2)))

(declaim (inline char>))
(defun char> (x1 x2)
  (char< x2 x1))

(declaim (inline char<=))
(defun char<= (x1 x2)
  (not (char< x2 x1)))

(defun string< (s1 s2)
  (block string<
      (let ((l (min (length s1) (length s2)))
            (shorter (< (length s1) (length s2))))
        (for ((i 0 (+ 1 i)))
             ((>= i l) shorter)
          (if (char< (elt s1 i) (elt s2 i))
            (return-from string< t))
          (if (char> (elt s1 i) (elt s2 i))
            (return-from string< nil))))))

(defun string= (s1 s2)
  (block string=
      (let ((l (length s1)))
        (if (/= (length s1) (length s2))
          (return-from string= nil))
        (for ((i 0 (+ 1 i)))
             ((>= i l) t)
          (if (char/= (elt s1 i) (elt s2 i))
            (return-from string= nil))))))

(declaim (inline string>))
(defun string> (s1 s2)
  (string< s2 s1))

(declaim (inline string>=))
(defun string>= (s1 s2)
  (not (string< s1 s2)))

(declaim (inline string<=))
(defun string<= (s1 s2)
  (not (string< s2 s1)))

(declaim (inline string/=))
(defun string/= (s1 s2)
  (not (string= s1 s2)))

(declaim (inline abs))
(defun abs (x)
  (if (< x 0)
      (- x)
      x))

(declaim (inline numberp))
(defun numberp (obj)
  (instancep obj (class <number>)))

(declaim (inline integerp))
(defun integerp (obj)
  (instancep obj (class <integer>)))

(declaim (inline floatp))
(defun floatp (obj)
  (instancep obj (class <float>)))

(declaim (inline characterp))
(defun characterp (obj)
  (instancep obj (class <character>)))

(declaim (inline consp))
(defun consp (obj)
  (instancep obj (class <cons>)))

(declaim (inline basic-array-p))
(defun basic-array-p (obj)
  (instancep obj (class <basic-array>)))

(declaim (inline basic-array*-p))
(defun basic-array*-p (obj)
  (instancep obj (class <basic-array*>)))

(declaim (inline general-array*-p))
(defun general-array*-p (obj)
  (instancep obj (class <general-array*>)))

(declaim (inline general-vector-p))
(defun general-vector-p (obj)
  (instancep obj (class <general-vector>)))

(declaim (inline streamp))
(defun streamp (obj)
  (instancep obj (class <stream>)))

(declaim (inline truffle-object-p))
(defun truffle-object-p (obj)
  (instancep obj (class <truffle-object>)))

(declaim (inline identity))
(defun identity (obj) obj)

(declaim (inline not))
(defun not (obj)
  (if obj nil t))

(declaim (inline null))
(defun null (obj)
  (if obj nil t))

(defmacro ignore-errors (:rest forms)
  (if (null forms)
      'nil
      (let ((blockid (gensym)))
        `(block ,blockid
            (with-handler
              (lambda (err) (return-from ,blockid nil))
              ,@forms)))))

(defun reverse (list)
  "These functions each return a list whose elements are those of the given list, but in reverse order. An error shall be signaled if list is not a list (error-id. domain-error ).
  @return <list>"
  (for ((list list (cdr list))
        (reversed () (cons (car list) reversed)))
       ((not list) reversed)))

(declaim (inline nreverse))
(defun nreverse (list)
  (reverse list))

(defun eql (a b)
  (if (and (characterp a) (characterp b))
      (char= a b)
      (if (and (numberp a) (numberp b))
          (= a b)
          (eq a b))))

(defun assoc (obj list)
  (block assoc
      (for ((list list (cdr list)))
           ((not list) nil)
        (let ((entry (car list)))
          (if (eql obj (car entry))
              (return-from assoc entry))))))

(defun member (obj list)
  (block member
      (for ((list list (cdr list)))
           ((not list) nil)
         (if (eql obj (car list))
             (return-from member list)))))

(defun append (:rest lists)
  (flet ((append2 (lst1 lst2)
           (for ((lst1 (reverse lst1) (cdr lst1))
                 (lst2 lst2 (cons (car lst1) lst2)))
                ((null lst1) lst2))))
    (cond
      ((null lists) nil)
      ((null (cdr lists)) (car lists))
      (t (append2 (car lists) (apply #'append (cdr lists)))))))

(defun get-internal-run-time () (get-internal-real-time))
(defun internal-time-units-per-second () 1000)

(defmacro define-with-file-macro (macro-name open-function)
    `(defmacro ,macro-name (signature :rest forms)
        (let ((name (elt signature 0))
              (open-args (cdr signature)))
          `(let ((,name (funcall #',',open-function ,@open-args)))
             (unwind-protect
                (progn ,@forms)
                (close ,name))))))

(define-with-file-macro with-open-input-file open-input-file)
(define-with-file-macro with-open-output-file open-output-file)
(define-with-file-macro with-open-io-file open-io-file)

(defun string-append (:rest strings)
  (labels ((fold (acc init lst)
              (for ((state init (funcall acc state (car lst)))
                    (lst lst (cdr lst)))
                    ((not lst) state))))
    (let ((len (fold
                 (lambda (state el) (+ state (length el)))
                 0
                 strings)))
      (for ((res (create-string len) res)
            (lst strings (cdr lst))
            (offset 0 (+ offset (length (car lst)))))
           ((not lst) res)
        (for ((j 0 (+ j 1))
              (str (car lst) str))
             ((>= j (length str)))
          (setf (elt res (+ offset j)) (elt str j)))))))

(defun map-into (dest fn :rest seqs)
  (labels ((args (index)
             (block args
               (if (< (length dest) index)
                   (return-from args nil))
               (mapcar
                (lambda (seq)
                  (if (<= (length seq) index)
                      (return-from args nil)
                      (elt seq index)))
                seqs))))
    (for ((i 0 (+ i 1))
          (next-args (args 0) (args (+ 1 i))))
         ((not next-args) dest)
      ;;(format (error-output) "i = ~A, args = ~A ~%" i next-args)
      (setf (elt dest i) (apply fn next-args)))))

(defun garef (array :rest indices)
  (if (and (not (instancep array (class <general-vector>)))
           (not (instancep array (class <general-array*>))))
    (signal-condition
        (create (class <domain-error>) 'message "Unexpected type" 'object array 'expected-class (class <general-array*>))
        nil))
  (apply #'aref array indices))

(defun set-garef (obj array :rest indices)
  (if (and (not (instancep array (class <general-vector>)))
           (not (instancep array (class <general-array*>))))
    (signal-condition
        (create (class <domain-error>) 'message "Unexpected type" 'object array 'expected-class (class <general-array*>))
        nil))
  (apply #'set-aref obj array indices))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (documentation 'function '-)
    '((kind . function)
      (syntax . "(- x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "Given one argument, x, this function returns its additive inverse. An error shall be signaled if x is not a number (error-id. domain-error).If an implementation supports a -0.0 that is distinct from 0.0, then (- 0.0) returns -0.0; in implementations where -0.0 and 0.0 are not distinct, (- 0.0) returns 0.0.")))

(setf (documentation 'function '*)
    '((kind . function)
      (syntax . "(* x*)")
      (return-type . "<number>")
      (min-args . 0)
      (max-args . nil)
      (desc . "The functions + and \\* return the sum and the product, respectively, of their arguments. If all arguments are integers, the result is an integer. If any argument is a ﬂoat, the result is a ﬂoat. When given no arguments, + returns 0 and \\* returns 1\\. An error shall be signaled if any x is not a number (error-id. domain-error).")))

(setf (documentation 'value '*most-negative-float*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of \\*most-positive-float\\* is the [implementation-dependent](#td%5Fimplementation%5Fdependent) ﬂoating-point number closest to positive infinity.The value of \\*most-negative-float\\* is the [implementation-dependent](#td%5Fimplementation%5Fdependent) ﬂoating-point number closest to negative infinity.")))

(setf (documentation 'value '*most-positive-float*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of \\*most-positive-float\\* is the [implementation-dependent](#td%5Fimplementation%5Fdependent) ﬂoating-point number closest to positive infinity.The value of \\*most-negative-float\\* is the [implementation-dependent](#td%5Fimplementation%5Fdependent) ﬂoating-point number closest to negative infinity.")))

(setf (documentation 'value '*pi*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of this constant is an approximation of π.")))

(setf (documentation 'function '/=)
    '((kind . function)
      (syntax . "(/= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns t if x1 and x2 have mathematically distinct values; otherwise, returns nil. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).")))

(setf (documentation 'function '+)
    '((kind . function)
      (syntax . "(+ x*)")
      (return-type . "<number>")
      (min-args . 0)
      (max-args . nil)
      (desc . "The functions + and \\* return the sum and the product, respectively, of their arguments. If all arguments are integers, the result is an integer. If any argument is a ﬂoat, the result is a ﬂoat. When given no arguments, + returns 0 and \\* returns 1\\. An error shall be signaled if any x is not a number (error-id. domain-error).")))

(setf (documentation 'function '<)
    '((kind . function)
      (syntax . "(< x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "\\>= returns t if x1 is greater than or \\= x2. <= returns t if x1 is less than or \\= x2. \\> returns t if x1 is greater than x2. < returns t if x1 is less than x2.The mathematical values of the arguments are compared. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).")))

(setf (documentation 'function '<=)
    '((kind . function)
      (syntax . "(<= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "\\>= returns t if x1 is greater than or \\= x2. <= returns t if x1 is less than or \\= x2. \\> returns t if x1 is greater than x2. < returns t if x1 is less than x2.The mathematical values of the arguments are compared. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).")))

(setf (documentation 'function '=)
    '((kind . function)
      (syntax . "(= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns t if x1 has the same mathematical value as x2; otherwise, returns nil. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).Note: \\= differs from eql because \\= compares only the mathematical values of its arguments, whereas eql also compares the representations.")))

(setf (documentation 'function '>)
    '((kind . function)
      (syntax . "(> x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "\\>= returns t if x1 is greater than or \\= x2. <= returns t if x1 is less than or \\= x2. \\> returns t if x1 is greater than x2. < returns t if x1 is less than x2.The mathematical values of the arguments are compared. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).")))

(setf (documentation 'function '>=)
    '((kind . function)
      (syntax . "(>= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "\\>= returns t if x1 is greater than or \\= x2. <= returns t if x1 is less than or \\= x2. \\> returns t if x1 is greater than x2. < returns t if x1 is less than x2.The mathematical values of the arguments are compared. An error shall be signaled if either x1 or x2 is not a number (error-id. domain-error).")))

(setf (documentation 'function 'abs)
    '((kind . function)
      (syntax . "(abs x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function abs returns the absolute value of its argument. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'syntax 'and)
    '((kind . syntax)
      (syntax . "(and form*)")
      (return-type . "<object>")
      (desc . "and is the sequential logical “and” (or “∧”). forms are evaluated from left to right until either one of them evaluates to nil or else none are left. If one of them evaluates to nil, then nil is returned from the and; otherwise, the value of the last evaluated form is returned. The form and is equivalent to the following:(and) ≡ 't (and form) ≡ form (and form1 form2 … formn) ≡ (if form1 (and form2 … formn) 'nil)")))

(setf (documentation 'function 'append)
    '((kind . function)
      (syntax . "(append list*)")
      (return-type . "<list>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns the result of appending all of the lists, or () if given no lists. An error shall be signaled if any list is not a list (error-id. domain-error).This function does not modify its arguments. It is [implementation defined](#td%5Fimplementation%5Fdefined) whether and when the result shares structure with its list arguments.An error shall be signaled if the list cannot be allocated (error-id. cannot-create-list).")))

(setf (documentation 'function 'apply)
    '((kind . function)
      (syntax . "(apply function obj* list)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 1)
      (desc . "Applies function to the arguments, obj\\*, followed by the elements of list, if any. It returns the value returned by function.An error shall be signaled if function is not a function (error-id. domain-error). Each obj may be any ISLISP object. An error shall be signaled if list is not a proper list (see §7) (error-id. improper-argument-list).")))

(setf (documentation 'setf 'aref)
    '((kind . setf)
      (syntax . "(setf (aref basic-array z*) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by aref or garef with obj. The returned value is obj. The constraints on the basic-array, the general-array, and the sequence of indices z is the same as for aref and garef.")))

(setf (documentation 'function 'aref)
    '((kind . function)
      (syntax . "(aref basic-array z*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "aref returns the object stored in the component of the basic-array specified by the sequence of integers z. This sequence must have exactly as many elements as there are dimensions in the basic-array, and each one must satisfy 0 ≤ zi < di , di the ith dimension and 0 ≤ i < d, d the number of dimensions. Arrays are indexed 0 based, so the ith row is accessed via the index i − 1.An error shall be signaled if basic-array is not a [basic-array](#figure%5F1) (error-id. domain-error). An error shall be signaled if any z is not a non-negative integer (error-id. domain-error).garef is like aref but an error shall be signaled if its first argument, general-array, is not an object of class [general-vector](#figure%5F1) or of class [general-array\\*](#figure%5F1) (error-id. domain-error).")))

(setf (documentation 'function 'arithmetic-error-operands)
    '((kind . function)
      (syntax . "(arithmetic-error-operands arithmetic-error)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the operation and operands supplied as data when creating the arithmetic-error. An error shall be signaled if arithmetic-error is not a condition of class arithmetic-error (error-id. domain-error).")))

(setf (documentation 'function 'arithmetic-error-operation)
    '((kind . function)
      (syntax . "(arithmetic-error-operation arithmetic-error)")
      (return-type . "<function>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the operation and operands supplied as data when creating the arithmetic-error. An error shall be signaled if arithmetic-error is not a condition of class arithmetic-error (error-id. domain-error).")))

(setf (documentation 'function 'array-dimensions)
    '((kind . function)
      (syntax . "(array-dimensions basic-array)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a list of the dimensions of a given basic-array. An error shall be signaled if basic-array is not a basic-array (error-id. domain-error). The consequences are undefined if the returned list is modified.")))

(setf (documentation 'function 'assoc)
    '((kind . function)
      (syntax . "(assoc obj association-list)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "If assocation-list contains at least one cons whose car is obj (as determined by eql), the first such cons is returned. Otherwise, nil is returned. An error shall be signaled if association-list is not a list of conses (error-id. domain-error).")))

(setf (documentation 'syntax 'assure)
    '((kind . syntax)
      (syntax . "(assure class-name form)")
      (return-type . "<object>")
      (desc . "These forms evaluate form. If form returns, the returned value is returned by the the or assure form. In addition, these forms specify that the value of form is of the class specified by class-name (which must be the name of an existing class).In a the special form, the consequences are undefined if the value of form is not of the class or a subclass of the class designated by class-name. In an assure special form, an error shall be signaled if the value of form is not of the class or a subclass of the class designated by class-name (error-id. domain-error).")))

(setf (documentation 'function 'atan)
    '((kind . function)
      (syntax . "(atan x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the arc tangent of x.The result is a (real) number that lies between −π/2 and π/2 (both exclusive).The following definition for (one-argument) arc tangent determines the range and branch cuts: 

arctan x

\\=

| log (1 + ix) - log (1 - ix) |
| --------------------------- |
| 2i                          |

An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'atan2)
    '((kind . function)
      (syntax . "(atan2 x1 x2)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Given a point (x2, x1) in rectangular coordinates, this function returns the phase of its representation in polar coordinates. If x1 is zero and x2 is negative, the result is positive. If x1 and x2 are both zero, the result is [implementation defined](#td%5Fimplementation%5Fdefined).An error shall be signaled if x is not a number (error-id. domain-error).The value of atan2 is always between −π (exclusive) and π (inclusive) when minus zero is not supported; when minus zero is supported, the range includes −π. Figure 3\\. Quadrant information for atan2 

y Condition

x Condition

Cartesian locus

Range of result

y=0

x>0

Positive x-axis

0

\\*y=+0

x>0

Positive x-axis

+0

\\*y=-0

x>0

Positive x-axis

\\-0

y>0

x>0

Quadrant I

0<result<π/2

y>0

x=0

Positive y-axis

π/2

y>0

x<0

Quadrant II

π/2<result<π

y=0

x<0

Negative x-asis

π

\\*y=+0

x<0

Negative x-asis

π

\\*y=-0

x<0

Negative x-asis

\\-π

y<0

x<0

Quadrand III

\\-π<result<-π/2

y<0

x=0

Negative y-axis

\\-π/2

y<0

x>0

Quadrant IV

\\-π/2<result<0

y=0

x=0

Origin

[implementation defined](#td%5Fimplementation%5Fdefined)

\\*y=+0

x=+0

Origin

+0

\\*y=-0

x=+0

Origin

\\-0

\\*y=+0

x=-0

Origin

π

\\*y=-0

x=-0

Origin

\\-π

The signs of x1 (indicated as y) and x2 (indicated as x) are used to derive quadrant information. Figure 19.1 details various special cases. The asterisk (\\*) indicates that the entry in the figure applies to implementations that support minus zero.")))

(setf (documentation 'function 'atanh)
    '((kind . function)
      (syntax . "(atanh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the hyperbolic arc tangent of x. An error shall be signaled if x is not a number with absolute value less than 1 (error-id. domain-error).")))

(setf (documentation 'function 'basic-array-p)
    '((kind . function)
      (syntax . "(basic-array-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "basic-array-p returns t if obj is a basic-array (instance of class basic-array); otherwise, returns nil. obj may be any ISLISP object.basic-array\\*-p returns t if obj is a basic-array\\* (instance of class basic-array\\*); otherwise, returns nil. obj may be any ISLISP object.general-array\\*-p returns t if obj is a general-array\\* (instance of class general-array\\*); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'basic-array*-p)
    '((kind . function)
      (syntax . "(basic-array*-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "basic-array-p returns t if obj is a basic-array (instance of class basic-array); otherwise, returns nil. obj may be any ISLISP object.basic-array\\*-p returns t if obj is a basic-array\\* (instance of class basic-array\\*); otherwise, returns nil. obj may be any ISLISP object.general-array\\*-p returns t if obj is a general-array\\* (instance of class general-array\\*); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'basic-vector-p)
    '((kind . function)
      (syntax . "(basic-vector-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "basic-vector-p returns t if obj is a basic-vector (instance of class basic-vector); otherwise, returns nil. obj may be any ISLISP object.general-vector-p returns t if obj is a general-vector (instance of class general-vector); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'syntax 'block)
    '((kind . syntax)
      (syntax . "(block name form*)")
      (return-type . "<object>")
      (desc . "The block special form executes each form sequentially from left to right. If the last form exits normally, whatever it returns is returned by the block form.The name in a block form is not evaluated; it must be an identifier. The scope of name is the body form\\*—only a return-from textually contained in some form can exit the block. The extent of name is dynamic.If a return-from is executed, the result-form is evaluated. If this evaluation returns normally, the value it returns is immediately returned from the innermost lexically enclosing block form with the same name.return-from is used to return from a block. name is not evaluated and must be an identifier. A block special form must lexically enclose the occurrence of return-from; the value produced by result-form is immediately returned from the block. The return-from form never returns and does not have a value.An error shall be signaled if an attempt is made to exit a block after it has been exited (error-id. control-error); It is a violation if name is not an identifier. It is a violation if a block with a corresponding name does not exist. See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.")))

(setf (documentation 'function 'call-next-method)
    '((kind . local-function)
      (syntax . "(call-next-method)")
      (return-type . "<object>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The call-next-method function can be used within the body of a method to call the next method.It returns the value returned by the method it calls.The type of method combination used determines which methods can invoke call-next-method and what is the next method to be called.In the case of simple method combination where the method combination type is nil the next method is the next most specific method.The standard method combination type allows call-next-method to be used within primary methods and :around methods. The standard method combination type defines the next method as specified in [§15.3.3.2](#standard%5Fmethod%5Fcombination).call-next-method passes the current method's original arguments to the next method. Neither using setq nor rebinding variables with the same names as parameters of the method affects the values call-next-method passes to the method it calls. The call-next-method function returns the value returned by the method it calls. After call-next-method returns, further computation is possible. The next-method-p function can be used to test whether there is a next method.The functional binding of call-next-method is lexical within the body of the method-defining form; i.e., it is as if it were established by labels. The function object to which the binding refers has indefinite extent.An error shall be signaled if call-next-method is used in methods that do not support it. An error shall be signaled if call-next-method is executed and there is no next method.")))

(setf (documentation 'setf 'car)
    '((kind . setf)
      (syntax . "(setf (car cons) obj)")
      (return-type . "<object>")
      (desc . "Updates the left component of cons with obj. The returned value is obj. An error shall be signaled if cons is not a cons (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'function 'car)
    '((kind . function)
      (syntax . "(car cons)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function car returns the left component of the cons. An error shall be signaled if cons is not a cons (error-id. domain-error).")))

(setf (documentation 'syntax 'case)
    '((kind . syntax)
      (syntax . "(case keyform ((key*) form*)* [(t form*)])")
      (return-type . "<object>")
      (desc . "The case and case-using special forms, called case forms, provide a mechanism to execute a matching clause from a series of clauses based on the value of a dispatching form keyform.The clause to be executed is identified by a set of keys. A key can be any object. If the keylist of the last clause is t the associated clause is executed if no key matches the keyform.keyform is a form to be computed at the beginning of execution of the case form. If the result of evaluating keyform is equivalent to a key, then the forms, if any, in the corresponding clause are evaluated sequentially and the value of the last one is returned as value of the whole case form. case determines match equivalence by using eql; case-using match determines equivalence by using the result of evaluating predform. predform must be a boolean or quasi-boolean function that accepts two arguments, the value returned by keyform and key. If no form exists for a matching key, the case form evaluates to nil. If the value of keyform is different from every key, and there is a default clause, its forms, if any, are evaluated sequentially, and the value of the last one is the result of the case form.The same key (as determined by the match predicate) may occur only once in a case form.")))

(setf (documentation 'syntax 'case-using)
    '((kind . syntax)
      (syntax . "(case-using predform keyform ((key*) form*)* [(t form*)])")
      (return-type . "<object>")
      (desc . "The case and case-using special forms, called case forms, provide a mechanism to execute a matching clause from a series of clauses based on the value of a dispatching form keyform.The clause to be executed is identified by a set of keys. A key can be any object. If the keylist of the last clause is t the associated clause is executed if no key matches the keyform.keyform is a form to be computed at the beginning of execution of the case form. If the result of evaluating keyform is equivalent to a key, then the forms, if any, in the corresponding clause are evaluated sequentially and the value of the last one is returned as value of the whole case form. case determines match equivalence by using eql; case-using match determines equivalence by using the result of evaluating predform. predform must be a boolean or quasi-boolean function that accepts two arguments, the value returned by keyform and key. If no form exists for a matching key, the case form evaluates to nil. If the value of keyform is different from every key, and there is a default clause, its forms, if any, are evaluated sequentially, and the value of the last one is the result of the case form.The same key (as determined by the match predicate) may occur only once in a case form.")))

(setf (documentation 'syntax 'catch)
    '((kind . syntax)
      (syntax . "(catch tag-form form*)")
      (return-type . "<object>")
      (desc . "The special forms catch and throw provide a facility for programming of structured non-local dynamic exits. A catch form and a throw form are said to correspond if the tag-form of the catch and the tag-form of the throw evaluate to the same object, a catch tag. A catch tag may be any object other than a number or a character; the comparison of catch tags uses eq.The catch special form first evaluates the tag-form to produce a catch tag, and then executes each form sequentially from left to right. If execution of the forms finishes normally, whatever is returned by the last form is returned by the catch form.Prior to execution of the forms of a catch form C0, an association between the catch tag T0 and the executing form C0 is dynamically established, upon exit from C0, the association is disestablished. If there was an outer association for the same catch tag T0, it is hidden during the execution of C0's forms; only the most recently established (i.e., innermost) association for T0 is ever visible.If a throw special form is executed, it evaluates the tag-form producing a catch tag T1, and then evaluates the result-form producing a result R1. If there is a corresponding association between T1 and some catch form C1 that is executing, R1 is immediately returned as the value of C1. The throw form can be anywhere in the entire current toplevel scope; it need not be lexically contained within C1.An error shall be signaled if there is no outstanding catcher for a T1 (error-id. control-error). See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.")))

(setf (documentation 'setf 'cdr)
    '((kind . setf)
      (syntax . "(setf (cdr cons) obj)")
      (return-type . "<object>")
      (desc . "Updates the right component of cons with obj. The returned value is obj . An error shall be signaled if cons is not a cons (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'function 'cdr)
    '((kind . function)
      (syntax . "(cdr cons)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function cdr returns the right component of the cons. An error shall be signaled if cons is not a cons (error-id. domain-error).")))

(setf (documentation 'function 'ceiling)
    '((kind . function)
      (syntax . "(ceiling x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the smallest integer that is not smaller than x. That is, x is truncated towards positive infinity. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'cerror)
    '((kind . function)
      (syntax . "(cerror continue-string error-string obj*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "Like error, but the error that it signals is “continuable” (see [continue-condition](#f%5Fcontinue%5Fcondition)). The extra argument continue-string describes what happens if this function returns.This is equivalent to:(signal-condition (create (class simple-error) 'format-string error-string 'format-arguments (list obj\\*)) (let ((str (create-string-output-stream))) (format str continue-string obj\\*) (get-output-stream-string str)))")))

(setf (documentation 'function 'char-index)
    '((kind . function)
      (syntax . "(char-index char string [start-position])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the position of char in string, The search starts from the position indicated by start-position (which is 0-based and defaults to 0). The value returned if the search succeeds is an offset from the beginning of the string, not from the starting point. If the char does not occur in the string, nil is returned. The function char= is used for the comparisons.An error shall be signaled if char is not a character or if string is not a string (error-id. domain-error).")))

(setf (documentation 'function 'char/=)
    '((kind . function)
      (syntax . "(char/= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'char<)
    '((kind . function)
      (syntax . "(char< char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'char<=)
    '((kind . function)
      (syntax . "(char<= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'char=)
    '((kind . function)
      (syntax . "(char= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'char>)
    '((kind . function)
      (syntax . "(char> char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'char>=)
    '((kind . function)
      (syntax . "(char>= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function char= tests whether char1 is the same character as char2. The function char< tests whether char1 is less than char2. The function char<= tests whether char1 is less than or equal to char2. The ordering used is the partial order defined above, extended to a total order on all characters in an [implementation-defined](#td%5Fimplementation%5Fdefined) manner. If the test is satisfied, t is returned; otherwise, nil is returned.Two characters are char/= if and only if they are not char=. Two characters are char> if and only if they are not char<=. Two characters are char>= if and only if they are not char<.An error shall be signaled if either char1 or char2 is not a character (error-id. domain-error).")))

(setf (documentation 'function 'characterp)
    '((kind . function)
      (syntax . "(characterp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a character (instance of class character); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'syntax 'class)
    '((kind . syntax)
      (syntax . "(class class-name)")
      (return-type . "<class>")
      (desc . "Returns the class object that corresponds to the class named class-name.")))

(setf (documentation 'function 'class-of)
    '((kind . function)
      (syntax . "(class-of obj)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the class of which the given obj is a direct instance. obj may be any ISLISP object.")))

(setf (documentation 'function 'close)
    '((kind . function)
      (syntax . "(close stream)")
      (return-type . nil)
      (min-args . 1)
      (max-args . 1)
      (desc . "The function close closes the stream stream. If stream is closed it may no longer be used in input or output operations. Closing a file stream ends the association between the stream and its file. If the stream was already closed this function performs nothing. The result value is [implementation defined](#td%5Fimplementation%5Fdefined). An error shall be signaled if stream is not a stream (error-id. domain-error).")))

(setf (documentation 'syntax 'cond)
    '((kind . syntax)
      (syntax . "(cond (test form*)*)")
      (return-type . "<object>")
      (desc . "Executing the prepared cond, the clauses (test form\\*) are scanned sequentially and in each case the test is evaluated; when a test delivers a non-nil value the scanning process stops and all forms associated with the corresponding clause are sequentially evaluated and the value of the last one is returned. If no test is true, then nil is returned. If no form exists for the successful test then the value of this test is returned.cond obeys the following equivalences: 

(cond) 

≡ 

nil 

(cond (test1) (test2 form2\\*) ...) 

≡ 

(or test1 (cond (test2 form2\\*) ...)) 

(cond (test1 form1+) (test2 form2\\*) ...) 

≡ 

(if test1 (progn form1+) (cond (test2 form2\\*) ...))")))

(setf (documentation 'function 'condition-continuable)
    '((kind . function)
      (syntax . "(condition-continuable condition)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns nil if condition is not continuable, or a string describing the effect of continuing otherwise.")))

(setf (documentation 'function 'cons)
    '((kind . function)
      (syntax . "(cons obj1 obj2)")
      (return-type . "<cons>")
      (min-args . 2)
      (max-args . 2)
      (desc . "This function builds a cons from two objects, with obj1 as its car (or \\`left') part and with obj2 as its cdr (or \\`right') part. An error shall be signaled if the requested cons cannot be allocated (error-id. cannot-create-cons). Both obj1 and obj2 may be any ISLISP object.")))

(setf (documentation 'function 'consp)
    '((kind . function)
      (syntax . "(consp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a cons (instance of class cons); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'continue-condition)
    '((kind . function)
      (syntax . "(continue-condition condition [value]) transfers control and data")
      (return-type . nil)
      (min-args . 1)
      (max-args . 2)
      (desc . "Continues from condition by finding the call to signal-condition and arranging for it to perform a normal return of the value, which defaults to nil. The consequences are undefined if the condition is not continuable.")))

(setf (documentation 'syntax 'convert)
    '((kind . syntax)
      (syntax . "(convert obj class-name)")
      (return-type . "<object>")
      (desc . "Returns an object of class class-name which corresponds to the object obj according to one of the following projections, called a coercion function. The table shows obj along the left-hand column and class-name along the top row (with <>'s in class names omitted here only for textual brevity): 

character

integer

ﬂoat

symbol

string

general-vector

list

character

\\=

I

\\-

I(3)

\\-(4)

\\-

\\-

integer

I

\\=

X

\\-

X(5)

\\-

\\-

ﬂoat

\\-

\\-(1)

\\=

\\-

X(6)

\\-

\\-

symbol

\\-

\\-

\\-

\\=

I(3)

\\-

\\-

string

\\-

X(2)

X(2)

I(3)

\\=

X(7)

X

general-vector

\\-

\\-

\\-

\\-

\\-

\\=

X

list

\\-

\\-

\\-

\\-

\\-

X

\\=

**\\=**

This is the identity function

**X**

This coercion shall be provided

**X(2)**

An error shall be signaled if this coercion is attempted and the string does not contain the textual representation of a number of the target class. In all other respects, this is the same as parse-number.

**X(5)**

This may be the same as the \\~D format directive.

**X(6)**

This may be the same as the \\~G format directive.

**X(7)**

This is the identity if strings are vectors in the implementation.

**I**

This coercion shall be provided, but its definition is [implementation defined](#td%5Fimplementation%5Fdefined). 

**I(3)**

This coercion shall be provided, but its definition is [implementation defined](#td%5Fimplementation%5Fdefined). The coercion depends on the implementation's _neutral_ alphabetic characters (see [§18.1.2](#alphabetic%5Fcase%5Fin%5Fsymbol%5Fnames)).

**–**

An error shall be signaled if this coercion is attempted. 

**–(1)**

Programmers requiring this kind of functionality may wish to consider instead using one of the functions, floor, ceiling, round, or truncate.

**–(4)**

programmers requiring this kind of functionality may wish to consider instead using the following: (create-string 1 obj)

If an implementation provides [implementation-defined](#td%5Fimplementation%5Fdefined) classes, it may also provide [implementation-defined](#td%5Fimplementation%5Fdefined) coercions for the names of those classes using convert.")))

(setf (documentation 'function 'cos)
    '((kind . function)
      (syntax . "(cos x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sin returns the sine of x . The function cos returns the cosine of x. The function tan returns the tangent of x. In each case, x must be given in radians.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'cosh)
    '((kind . function)
      (syntax . "(cosh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sinh returns the hyperbolic sine of x. The function cosh returns the hyperbolic cosine of x. The function tanh returns the hyperbolic tangent of x.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'create)
    '((kind . generic-function)
      (syntax . "(create class {initarg initval }*)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function create creates and returns a new instance of a class. The argument is a class object.The initialization of a new instance consists of several distinct steps, including the following: allocating storage for the instance, filling slots with values, and executing user-supplied methods that perform additional initialization. The last two steps of create are implemented by the generic function initialize-object to provide a mechanism for customizing those steps. The initialization arguments (the initargs and initvals) are given as a single list argument to initialize-object. The instance returned by create is the new instance, which has been modified and returned by initialize-object.")))

(setf (documentation 'function 'create-array)
    '((kind . function)
      (syntax . "(create-array dimensions [initial-element])")
      (return-type . "<basic-array>")
      (min-args . 1)
      (max-args . 2)
      (desc . "This function creates an array of the given dimensions. The dimensions argument is a list of non-negative integers.The result is of class general-vector if there is only one dimension, or of class general-array\\* otherwise.If initial-element is given, the elements of the new array are initialized with this object, otherwise the initialization is [implementation defined](#td%5Fimplementation%5Fdefined).An error shall be signaled if the requested array cannot be allocated (error-id. cannot-create-array).An error shall be signaled if dimensions is not a proper list of non-negative integers (error-id. domain-error). initial-element may be any ISLISP object.")))

(setf (documentation 'function 'create-list)
    '((kind . function)
      (syntax . "(create-list i [initial-element])")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 2)
      (desc . "Returns a list of length i. If initial-element is given, the elements of the new list are initialized with this object; otherwise, the initialization is [implementation defined](#td%5Fimplementation%5Fdefined). An error shall be signaled if the requested list cannot be allocated (error-id. cannot-create-list). An error shall be signaled if i is not a non-negative integer (error-id. domain-error). initial-element may be any ISLISP object.")))

(setf (documentation 'function 'create-string)
    '((kind . function)
      (syntax . "(create-string i [initial-character ])")
      (return-type . "<string>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns a string of length i. If initial-character is given, then the characters of the new string are initialized with this character, otherwise the initialization is [implementation defined](#td%5Fimplementation%5Fdefined). An error shall be signaled if the requested string cannot be allocated (error-id. cannot-create-string). An error shall be signaled if i is not a non-negative integer or if initial-character is not a character (error-id. domain-error).")))

(setf (documentation 'function 'create-string-input-stream)
    '((kind . function)
      (syntax . "(create-string-input-stream string)")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Creates and returns an input stream from the string. An error shall be signaled if string is not a string (error-id. domain-error).")))

(setf (documentation 'function 'create-string-output-stream)
    '((kind . function)
      (syntax . "(create-string-output-stream)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "This function creates and returns a string output stream. The output to a string stream can be retrieved by get-output-stream-string.")))

(setf (documentation 'function 'create-vector)
    '((kind . function)
      (syntax . "(create-vector i [initial-element])")
      (return-type . "<general-vector>")
      (min-args . 1)
      (max-args . 2)
      (desc . "Returns a general-vector of length i. If initial-element is given, the elements of the new vector are initialized with this object, otherwise the initialization is [implementation defined](#td%5Fimplementation%5Fdefined). An error shall be signaled if the requested vector cannot be allocated (error-id. cannot-create-vector). An error shall be signaled if i is not a non-negative integer (error-id. domain-error). initial-element may be any ISLISP object.")))

(setf (documentation 'syntax 'defclass)
    '((kind . syntax)
      (syntax . "(defclass class-name (sc-name*) (slot-spec*) class-opt*)")
      (return-type . "<symbol>")
      (desc . "Where: class-name ::= identifier sc-name ::= identifier slot-spec ::= slot-name | (slot-name slot-opt\\*) slot-name ::= identifier slot-opt ::= :reader reader-function-name | :writer writer-function-name | :accessor reader-function-name | :boundp boundp-function-name | :initform form | :initarg initarg-name initarg-name ::= identifier reader-function-name ::= identifier writer-function-name ::= identifier boundp-function-name ::= identifier class-opt ::= (:metaclass class-name) | (:abstractp abstract-flag) abstract-flag ::= t | nilThe defclass defining form returns the symbol named class-name as its result. The class-name argument is an identifier which becomes the name of the new class. The defining point of the class-name is the end of the defclass defining form.Each superclass name argument sc-name is an identifier that specifies a direct superclass of the new class. The new class will inherit slots and their :reader or :writer or :accessor methods from each of its superclasses. See [§15.1.3](#inheritance%5Fof%5Fslots) for a definition of how slots are inherited, and [§15.2.3](#inheritance%5Fof%5Fmethods) for a definition of how methods are inherited. No sc-name may appear more than once in super class names. It is a violation if the superclasses of any two direct superclasses sc-name have superclasses other than standard-object and object in common unless a metaclass other than standard-class is specified.Each slot-spec argument is the name of the slot or a list consisting of the slot name followed by zero or more slot options. The slot-name argument is an identifier that is syntactically valid for use as an ISLISP variable name. No slot names may appear more than once in slot-spec The following slot options are available: 
* The :reader slot option specifies that an unqualified method with the parameter profile ((x class-name)) is to be defined on the generic function named reader-function-name to retrieve the value of the given slot. The :reader slot option may be specified more than once for a given slot.
* The :writer slot option specifies that an unqualified method with the parameter profile ((y object) (x class-name)) is to be defined on the generic function named writer-function-name to store the value into the slot. The writer-function-name argument is an identifier. The :writer slot option may be specified more than once for a given slot.
* The :accessor slot option specifies that an unqualified method is to be defined on the generic function named reader-function-name to retrieve the value of the given slot. Furthermore, there is a generic function such that (setf (reader-function-name x) y) is equivalent to calling this generic function with first argument y and second argument x. This generic function is extended by a method with the parameter profile ((y object) (x class-name)). The reader-function-name argument is an identifier. The :accessor slot option may be specified more than once for a given slot.
* The :boundp slot option specifies that an unqualified method with the parameter profile ((x class-name)) is to be defined on the generic function named boundp-function-name to test whether the given slot has been given a value. The :boundp slot option may be specified more than once for a given slot.
* The :initform slot option is used to provide a default initial value form to be used in the initialization of the slot. The :initform slot option may be specified once at most for a given slot. This form is evaluated every time it is used to initialize the slot. The lexical scope of the identifiers used in the initialization of the slot is the lexical scope of those identifiers in the defclass form. Note that the lexical scope refers both to variable and to function identifiers. In contrast, the current dynamic bindings used are those existing during activation of create. For more information, see [§15.4.1](#initialize%5Fobject).
* The :initarg slot option declares an initialization argument named initarg-name and specifies that this initialization argument initializes the given slot. If the initialization argument and associated value are supplied in the call to initialize-object, the value will be stored into the given slot and the slot's :initform slot option, if any, is not evaluated. If none of the initialization arguments specified for a given slot has a value, the slot is initialized according to the :initform option, if specified. The consequences are undefined if more than one initialization argument for the same slot is supplied. For more information, see [§15.4.1](#initialize%5Fobject).
The generic functions, to which the methods created by the :reader, :writer, and :accessor slot options belong are called slot accessors.No implementation is permitted to extend the syntax of defclass to allow (slot-name form) as an abbreviation for (slot-name :initform form).Each class option is an option that refers to the class as a whole. The following class options are available: 
* The :metaclass class option is used to specify that instances of the class being defined are to have a different metaclass than the default provided by the system, that is, different from the class standard-class. The class-name argument is the name of the desired metaclass. The :metaclass class option may be specified once at most. It is a violation if built-in-class is specified as the metaclass.
* The :abstractp class option is used to specify that the class is an [abstract class](#td%5Fabstract%5Fclass). If this option is supplied and abstract-flag is t, create will signal an error if an attempt is made to create an instance of this class. If the option is unsupplied, or if abstract-flag is nil, the class is not an [abstract class](#td%5Fabstract%5Fclass). It is a violation if the abstract-flag is supplied but is neither t nor nil.
The following rules of defclass hold for standard classes: 
* The defclass defining form must be in the scope of any superclass identifier it refers to.
* All the superclasses of a class must be defined before an instance of the class can be made.
* Any reference to class-name as a parameter specializer in a defmethod form must be in the scope of class-name. That is, a defmethod form that names a class must textually follow the defclass form that defines that class.
An ISLISP processor may be extended to cover situations where these rules are not obeyed. These extensions shall be [implementation defined](#td%5Fimplementation%5Fdefined).Some slot options are inherited by a class from its superclasses, and some can be shadowed or altered by providing a local slot description. No class options are inherited. For a detailed description of how slots and slot options are inherited, see the section [§15.1.3](#inheritance%5Fof%5Fslots).If no slot accessors are specified for a slot, the slot cannot be accessed.When a class is defined, the order in which its direct superclasses are mentioned in the defining form is important. The new class has a local precedence order, which is a list consisting of the class followed by its direct superclasses in the order mentioned in its defclass defining form.")))

(setf (documentation 'syntax 'defconstant)
    '((kind . syntax)
      (syntax . "(defconstant name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define a named constant in the variable namespace of the current toplevel scope. The scope of name is the entire current toplevel scope except the body form.Although name is globally constant, a variable binding for name can be locally established by a binding form.The result of the evaluation of form is bound to the variable named by name. The binding and the object created as the result of evaluating the second argument are immutable. The symbol named name is returned.")))

(setf (documentation 'syntax 'defdynamic)
    '((kind . syntax)
      (syntax . "(defdynamic name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define a [dynamic variable](#td%5Fdynamic%5Fvariable) identifier in the [dynamic variable namespace](#td%5Fdynamic). The scope of name is the entire current toplevel scope except the body form.The symbol named name is returned.")))

(setf (documentation 'syntax 'defgeneric)
    '((kind . syntax)
      (syntax . "(defgeneric func-spec lambda-list {option | method-desc}*)")
      (return-type . "<symbol>")
      (desc . "Where: func-spec ::= identifier | (setf identifier) lambda-list ::= (var\\* \\[&rest var\\]) | (var\\* \\[:rest var\\]) option ::= (:method-combination {identifier | keyword}) | (:generic-function-class class-name) method-desc ::= (:method method-qualifier\\* parameter-profile form\\*) method-qualifier ::= identifier | keyword parameter-profile ::= ({var | (var parameter-specializer-name)}\\* \\[{&rest | :rest} var\\]) parameter-specializer-name ::= class-name class-name ::= identifier var ::= identifierThe defgeneric defining form is used to define a generic function and to specify options and declarations that pertain to a generic function as a whole.It returns the generic function name func-spec.The scope of the generic function identifier func-spec is the entire current toplevel scope.The lambda-list argument is an ordinary function lambda-list.The following options are provided. A given option may occur only once. 
* The :generic-function-class option specifies that the generic function is to have a different class from the default provided by the system, that is, different from the class standard-generic-function. The class-name argument is the name of a class that can be the class of a generic function.
* The :method-combination option is followed by a symbol or keyword that names a type of method combination. The names of the built-in method combination types are nil and standard.
The method-desc arguments define methods that will belong to the generic function, as if defined by defmethod. The method-qualifier and parameter-profile arguments in a method description are the same as for defmethod. The form arguments specify the method body.If no method descriptions are specified, a generic function with no methods is created. An error shall be signaled if a generic function is called and no methods apply.The lambda-list argument of defgeneric specifies the shape of lambda-lists for the methods on this generic function. All methods on the resulting generic function must have parameter-profiles that are congruent with this shape. For further details on method congruence, see [§15.2.2.2](#congruent%5Flambda%5Flists).Implementations can extend defgeneric to include other [implementation-defined](#td%5Fimplementation%5Fdefined) options.")))

(setf (documentation 'syntax 'defglobal)
    '((kind . syntax)
      (syntax . "(defglobal name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define an identifier in the variable namespace of the current toplevel scope. The scope of name is the entire current toplevel scope except the body form. form is evaluated to compute an initializing value for the variable named name. Therefore, defglobal is used only for defining variables and not for modifying them. The symbol named name is returned.A lexical variable binding for name can still be locally established by a binding form; in that case, the local binding lexically shadows the outer binding of name defined by defglobal.")))

(setf (documentation 'syntax 'defmacro)
    '((kind . syntax)
      (syntax . "(defmacro macro-name lambda-list form*)")
      (return-type . "<symbol>")
      (desc . "Defines a named (toplevel) macro. No implicit block with the macro name is established when the macro-expansion function is invoked. macro-name must be an identifier whose scope is the current toplevel scope in which the defmacro form appears. lambda-list is as defined in [page 23](#s%5Flambda). The definition point of macro-name is the closing parenthesis of the lambda-list.")))

(setf (documentation 'syntax 'defmethod)
    '((kind . syntax)
      (syntax . "(defmethod func-spec method-qualifier* parameter-profile form*)")
      (return-type . "<symbol>")
      (desc . "Where: func-spec ::= identifier | (setf identifier) method-qualifier ::= identifier | keyword parameter-profile ::= ({var | (var parameter-specializer-name)}\\* \\[{&rest | :rest} var\\]) parameter-specializer-name ::= class-name class-name ::= identifier var ::= identifierThe defmethod defining form defines a method on a generic function. It returns the generic function name func-spec.A method-defining form contains the code that is to be executed when the arguments to the generic function cause the method that it defines to be invoked.Preparing a method-defining form for execution causes one of the following cases: 
* It is a violation if the given name func-spec already designates a generic function and this generic function contains a method that agrees with the new one on parameter specializers and qualifiers. For a definition of one method agreeing with another on parameter specializers and qualifiers, see the section [§15.2.2.1](#agreement%5Fon%5Fparameter%5Fspecializers).
* If the given name func-spec designates a generic function and this generic function does not contain a method that agrees with the new one on parameter specializers and qualifiers, the new method is added to the generic function.
* It is a violation if the defmethod defining form is in the scope of a func-spec identifier that does not designate a generic function.
* It is a violation if the given name func-spec does not exist in the current toplevel scope immediately containing the defmethod defining form. Furthermore, it is a violation if a defgeneric form for func-spec does not precede the method-defining form in the text unit being prepared for execution unless the method-defining form is a defclass.
The parameter-profile of the method being defined must be congruent with the lambda-list of the generic function. See [§15.2.2.2](#congruent%5Flambda%5Flists) for a definition of congruence in this context.Each _method-qualifier argument_ is an object that is used as an attribute to the given method by method combination. A method qualifier is a non-nil symbol or keyword. The method combination type further restricts what a method qualifier may be. The standard method combination type allows for unqualified methods or methods whose sole qualifier is one of the keywords :before, :after, :around.The _parameter-profile argument_ is like an ordinary function lambda-list except that the names of required parameters can be replaced by specialized parameters. A specialized parameter is a list of the form (variable-name parameter-specializer-name). Only required parameters may be specialized. A parameter specializer name is an identifier that names a class. If no parameter specializer name is specified for a given required parameter, the parameter specializer defaults to the class named object.The form arguments specify the method body.No two methods with agreeing parameter specializers and qualifiers may be defined for the same generic function. See the section [§15.2.2.1](#agreement%5Fon%5Fparameter%5Fspecializers) for a definition of agreement in this context.A method is not a function and cannot be invoked as a function.")))

(setf (documentation 'syntax 'defun)
    '((kind . syntax)
      (syntax . "(defun function-name lambda-list form*)")
      (return-type . "<symbol>")
      (desc . "The _defun-form_ defines function-name as an identifier in the function namespace; function-name is bound to a function object equivalent to (lambda lambda-list form\\*).The scope of function-name is the whole current toplevel scope. Therefore, the definition of a function admits recursion, occurrences of function-name within the form\\* refer to the function being defined. The binding between function-name and the function object is immutable.defun returns the function name which is the symbol named function-name. The free identifiers in the body form\\* (i.e., those which are not contained in the lambda list) follow the rules of lexical scoping.")))

(setf (documentation 'function 'div)
    '((kind . function)
      (syntax . "(div z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "div returns the greatest integer less than or equal to the quotient of z1 and z2. An error shall be signaled if z2 is zero (error-id. division-by-zero).mod returns the remainder of the integer division of z1 by z2. The sign of the result is the sign of z2. The result lies between 0 (inclusive) and z2 (exclusive), and the difference of z1 and this result is divisible by z2 without remainder.div and mod satisfy:(= z1 (+ (\\* (div z1 z2) z2) (mod z1 z2))) That is, the evaluation of the above form always return t.An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'domain-error-expected-class)
    '((kind . function)
      (syntax . "(domain-error-expected-class domain-error)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the object and expected-class supplied as data when creating the domain-error. An error shall be signaled if domain-error is not a condition of class domain-error (error-id. domain-error).")))

(setf (documentation 'function 'domain-error-object)
    '((kind . function)
      (syntax . "(domain-error-object domain-error)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the object and expected-class supplied as data when creating the domain-error. An error shall be signaled if domain-error is not a condition of class domain-error (error-id. domain-error).")))

(setf (documentation 'setf 'dynamic)
    '((kind . setf)
      (syntax . "(setf (dynamic var) form)")
      (return-type . "<object>")
      (desc . "This special form denotes an assignment to a dynamic variable. This form can appear anywhere that (dynamic var) can appear.form is evaluated and the result of the evaluation is used to change the dynamic binding of var.An error shall be signaled if var has no dynamic value (error-id. unbound-variable). setf of dynamic can be used only for modifying bindings, and not for establishing them.")))

(setf (documentation 'syntax 'dynamic)
    '((kind . syntax)
      (syntax . "(dynamic var)")
      (return-type . "<object>")
      (desc . "This special form denotes a reference to the identifier denoting a dynamic variable. This special form is not allowed in the scope of a definition of var which is not done by defdynamic or dynamic-let.During activation, the current dynamic binding of the variable var is returned that was established most recently and is still in effect. An error shall be signaled if such a binding does not exist (error-id. unbound-variable).")))

(setf (documentation 'syntax 'dynamic-let)
    '((kind . syntax)
      (syntax . "(dynamic-let ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The dynamic-let special form is used to establish dynamic variable bindings. The first subform (the dynamic-let variable list) is a list of pairs (var form). The scope of an identifier var defined by dynamic-let is the current toplevel scope. The extent of the bindings of each var is the extent of the body of the dynamic-let. The dynamic-let special form establishes dynamic variables for all vars.References to a dynamic variable named by var must be made through the dynamic special form.All the initializing forms are evaluated sequentially from left to right, and then the values are associated with the corresponding vars. Using these additional dynamic bindings and the already existing bindings of visible identifiers, the forms body-form\\* are evaluated in sequential order. The returned value of dynamic-let is that of the last body-form of the body (or nil if there is none). The bindings are undone when control leaves the prepared dynamic-let special form.")))

(setf (documentation 'setf 'elt)
    '((kind . setf)
      (syntax . "(setf (elt sequence z) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by elt with obj. The returned value is obj.An error shall be signaled if z is an integer outside of the valid range of indices (error-id. index-out-of-range). An error shall be signaled if sequence is not a basic-vector or a list or if z is not an integer (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'function 'elt)
    '((kind . function)
      (syntax . "(elt sequence z)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Given a sequence and an integer z satisfying 0 ≤ z < (length sequence), elt returns the element of sequence that has index z. Indexing is 0-based; i.e., z \\= 0 designates the first element. An error shall be signaled if z is an integer outside of the mentioned range (error-id. index-out-of-range).An error shall be signaled if sequence is not a basic-vector or a list or if z is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'eq)
    '((kind . function)
      (syntax . "(eq obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "eq and eql test whether obj1 and obj2 are same identical object. They return t if the objects are the same; otherwise, they return nil. Two objects are the same if there is no operation that could distinguish them (without modifying them), and if modifying one would modify the other the same way.For eq, the consequences are [implementation defined](#td%5Fimplementation%5Fdefined) if both obj1 and obj2 are numbers or both are characters. For eql the meaning for numbers and characters is defined as follows: 
* If obj1 and obj2 are numbers, eql tests whether they are direct instances of the same class and have the same value.  
If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns nil. When the syntax -0.0 is read and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns t.
* If obj1 and obj2 are characters, eql tests whether they are the same character (see [char=](#f%5Fchar%5Feq)).")))

(setf (documentation 'function 'eql)
    '((kind . function)
      (syntax . "(eql obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "eq and eql test whether obj1 and obj2 are same identical object. They return t if the objects are the same; otherwise, they return nil. Two objects are the same if there is no operation that could distinguish them (without modifying them), and if modifying one would modify the other the same way.For eq, the consequences are [implementation defined](#td%5Fimplementation%5Fdefined) if both obj1 and obj2 are numbers or both are characters. For eql the meaning for numbers and characters is defined as follows: 
* If obj1 and obj2 are numbers, eql tests whether they are direct instances of the same class and have the same value.  
If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns nil. When the syntax -0.0 is read and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns t.
* If obj1 and obj2 are characters, eql tests whether they are the same character (see [char=](#f%5Fchar%5Feq)).")))

(setf (documentation 'function 'equal)
    '((kind . function)
      (syntax . "(equal obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "This function tests whether obj1 and obj2 are isomorphic—i.e., whether obj1 and obj2 denote the same structure with equivalent values. equal returns t if the test was satisfied, and nil if not. Specifically:If obj1 and obj2 are direct instances of the same class, equal returns t if they are eql. Otherwise (if they are direct instances of the same class but not eql), the result is t if one of the following cases applies: 
* lists: either obj1 and obj2 are both the empty list (i.e., nil), or  
(and (equal (car obj1) (car obj2))  
     (equal (cdr obj1) (cdr obj2))) holds;
* basic arrays:  
(equal (array-dimensions obj1)  
       (array-dimensions obj2))  
holds and for every valid reference (aref obj1 ind1 ...indn)  
(equal (aref obj1 ind1 … indn)  
       (aref obj2 ind1 … indn)) is satisfied.
Otherwise the value is nil.obj1 and obj2 may be any ISLISP objects.")))

(setf (documentation 'function 'error)
    '((kind . function)
      (syntax . "(error error-string obj*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "An error shall be signaled.error-string and the objs are advice to the implementation about how the error message might be textually described (using format), but whether or not that advice is used is [implementation defined](#td%5Fimplementation%5Fdefined).This is equivalent to:(signal-condition (create (class simple-error) 'format-string error-string 'format-arguments (list obj\\*)) nil)")))

(setf (documentation 'function 'error-output)
    '((kind . function)
      (syntax . "(error-output)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function standard-input returns the stream used as the default stream for input functions.The function standard-output returns the stream used as the default stream for output functions.The function error-output returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially [implementation-defined](#td%5Fimplementation%5Fdefined), but can be dynamically bound; see [with-standard-input](#s%5Fwith%5Fstandard%5Finput), [with-standard-output](#s%5Fwith%5Fstandard%5Foutput), and [with-error-output](#s%5Fwith%5Ferror%5Foutput).")))

(setf (documentation 'function 'exp)
    '((kind . function)
      (syntax . "(exp x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns _e_ raised to the power x, where _e_ is the base of the natural logarithm. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'expt)
    '((kind . function)
      (syntax . "(expt x1 x2)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns x1 raised to the power x2. The result will be an integer if x1 is an integer and x2 is a non-negative integer. An error shall be signaled if x1 is zero and x2 is negative, or if x1 is zero and x2 is a zero ﬂoat, or if x1 is negative and x2 is not an integer.")))

(setf (documentation 'function 'file-length)
    '((kind . function)
      (syntax . "(file-length filename element-class)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns the length of the file named by filename, or returns nil if the length cannot be determined. The element-class determines the units. An error shall be signaled if filename is not a string (error-id. domain-error).")))

(setf (documentation 'function 'file-position)
    '((kind . function)
      (syntax . "(file-position stream)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the file position associated with stream.A file position is a non-negative integer that represents a position in the stream. For binary streams, the file position represents the number of preceding bytes in the stream. It is increased by one each time one of the following is done:(read-byte stream) (write-byte z stream) For character streams, the file position is increased by an [implementation-defined](#td%5Fimplementation%5Fdefined) non-negative amount each time one of the following is done:(format stream...) (format-char stream char) (format-float stream float) (format-fresh-line stream) (format-integer stream integer radix) (format-object stream obj escape-p) (format-tab stream column) (report-condition condition stream) (read-char stream) (read-line stream) (read stream) The amount may depend on the output and on the file position itself. It is [implementation-defined](#td%5Fimplementation%5Fdefined) which integer represents the first element of the file. An error shall be signaled if stream is not a stream to or from a file (error-id. domain-error).")))

(setf (documentation 'function 'finish-output)
    '((kind . function)
      (syntax . "(finish-output stream)")
      (return-type . "<null>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Completes any pending output to the destination designated by stream. Waits until the pending output is complete and then returns nil. For instance, pending output might be stored in a buffer; in this case finish-output forces the buffer to be written to the stream's destination. An error shall be signaled if stream is not a stream that can handle output operations (error-id. domain-error).")))

(setf (documentation 'syntax 'flet)
    '((kind . syntax)
      (syntax . "(flet ((function-name lambda-list form*)*) body-form*)")
      (return-type . "<object>")
      (desc . "The flet and labels special forms allow the definition of new identifiers in the function namespace for function objects.In a labels special form the scope of an identifier function-name is the whole labels special form (excluding nested scopes, if any); for the flet special form, the scope of an identifier is only the body-form\\*. Within these scopes, each function-name is bound to a function object whose behavior is equivalent to (lambda lambda-list form\\*), where free identifier references are resolved as follows: 
* For a labels form, such free references are resolved in the lexical environment that was active immediately outside the labels form augmented by the function bindings for the given function-names (i.e., any reference to a function function-name refers to a binding created by the labels).
* For a flet form, free identifier references in the lambda\\-expression are resolved in the lexical environment that was active immediately outside the flet form (i.e., any reference to a function function-name are not visible).
During activation, the prepared labels or flet establishes function bindings and then evaluates each body-form in the body sequentially; the value of the last one (or nil if there is none) is the value returned by the special form activation.No function-name may appear more than once in the function bindings.")))

(setf (documentation 'function 'float)
    '((kind . function)
      (syntax . "(float x)")
      (return-type . "<float>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns x itself if it is an instance of the class float and returns a ﬂoating-point approximation of x otherwise. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'floatp)
    '((kind . function)
      (syntax . "(floatp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a ﬂoat (instance of class float); otherwise, returns nil. The obj may be any ISLISP object.")))

(setf (documentation 'function 'floor)
    '((kind . function)
      (syntax . "(floor x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the greatest integer less than or equal to x. That is, x is truncated towards negative infinity. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'syntax 'for)
    '((kind . syntax)
      (syntax . "(for (iteration-spec*) (end-test result*) form*)")
      (return-type . "<object>")
      (desc . "Where: iteration-spec ::= (var init \\[step\\]) for repeatedly executes a sequence of forms form\\*, called its body. It specifies a set of identifiers naming variables that will be local to the for form, their initialization, and their update for each iteration. When a termination condition is met, the iteration exits with a specified result value.The scope of an identifier var is the body, the steps, the end-test, and the result\\*. A step might be omitted, in which case the effect is the same as if (var init var) had been written instead of (var init). It is a violation if more than one iteration-spec names the same var in the same for form.The for special form is executed as follows: The init forms are evaluated sequentially from left to right. Then each value is used as the initial value of the variable denoted by the corresponding identifier var, and the iteration phase begins.Each iteration begins by evaluating end-test. If the result is nil, the forms in the body are evaluated sequentially (for side-effects). Afterwards, the step-forms are evaluated sequentially order from left to right. Then their values are assigned to the corresponding variables and the next iteration begins. If end-test returns a non-nil value, then the result\\* are evaluated sequentially and the value of the last one is returned as value of the whole for macro. If no result is present, then the value of the for macro is nil.")))

(setf (documentation 'function 'format)
    '((kind . function)
      (syntax . "(format output-stream format-string obj*)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . nil)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-char)
    '((kind . function)
      (syntax . "(format-char output-stream char)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-float)
    '((kind . function)
      (syntax . "(format-float output-stream float)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-fresh-line)
    '((kind . function)
      (syntax . "(format-fresh-line output-stream)")
      (return-type . "<null>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-integer)
    '((kind . function)
      (syntax . "(format-integer output-stream integer radix)")
      (return-type . "<null>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-object)
    '((kind . function)
      (syntax . "(format-object output-stream obj escape-p)")
      (return-type . "<null>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'format-tab)
    '((kind . function)
      (syntax . "(format-tab output-stream column)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function format has the side-effect of printing according to format-string. It returns nil. An error shall be signaled if the output-stream parameter does not satisfy the output-stream-p predicate (error-id. not-an-output-stream). An error shall be signaled if format-string is not a string (error-id. domain-error). The following is a summary of all the available format directives:obj refers to the next item of the set of obj\\* to be processed. 

\\~A

Aesthetic: The obj is any object. obj is printed as it would with \\~S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.

This effect is implemented by (format-object output-stream obj nil).

\\~B

Binary: An error shall be signaled if obj is not an integer. obj is printed in binary radix (radix 2).

This effect is implemented by (format-integer output-stream obj 2).

\\~C

Character: An error shall be signaled if obj is not a character. obj is output directly without any conversion.

This effect is implemented by (format-char output-stream obj).

\\~D

Decimal: An error shall be signaled if obj is not an integer. obj is printed in decimal radix (radix 10).

This effect is implemented by (format-integer output-stream obj 10).

\\~G

General ﬂoating point: An error shall be signaled if obj is not a number. obj is printed as a ﬂoat.

This effect is implemented by (format-float output-stream obj).

\\~O

Octal: An error shall be signaled if obj is not an integer. obj is printed in octal radix (radix 8).

This effect is implemented by (format-integer output-stream obj 8).

\\~nR

Radix: An error shall be signaled if obj is not an integer. obj is printed in radix n (which must be between 2 and 36, inclusive).

This effect is implemented by (format-integer output-stream obj n).

\\~S

S-expression: obj is any object. This format directive outputs the textual representation of obj, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function read.

This effect is implemented by (format-object output-stream obj t).

\\~nT

Tab: output enough spaces to move to column n (where column 0 represents the left margin). If already at or beyond column n, one space is output. If an implementation cannot determine the current column position, the behavior is [implementation defined](#td%5Fimplementation%5Fdefined), but at least one space will be output.

This effect is implemented by (format-tab output-stream n).

\\~X

Hexadecimal: An error shall be signaled if obj is not an integer. obj is printed in hexadecimal radix (radix 16).

This effect is implemented by (format-integer output-stream obj 16).

\\~%

newline: output a #\\\\newline character;

This effect is implemented by (format-char output-stream #\\\\newline).

\\~&

conditional newline: output a #\\\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;

This effect is implemented by (format-fresh-line output-stream).

\\~\\~

tilde: output a tilde (˜).

This effect is implemented by (format-char output-stream #\\\\\\~).")))

(setf (documentation 'function 'funcall)
    '((kind . function)
      (syntax . "(funcall function obj*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "Activates the specified function function and returns the value that the function returns. The _i_th argument (2 ≤ _i_) of funcall becomes the (_i_ − 1)th argument of the function. funcall could have been defined using apply as follows:(defun funcall (function :rest arguments) (apply function arguments)) An error shall be signaled if function is not a function (error-id. domain-error). Each obj may be any ISLISP object.")))

(setf (documentation 'syntax 'function)
    '((kind . syntax)
      (syntax . "(function function-name)")
      (return-type . "<function>")
      (desc . "#'function-name → functionThis special form denotes a reference to the function named by function-name. This special form is used to refer to identifiers defined by function-defining forms, labels, or flet which are not in operator position.(function function-name) can be written as #'function-name.It returns the function object named by function-name.An error shall be signaled if no binding has been established for the identifier in the function namespace of current lexical environment (see [§9.2](#pervasive%5Ferror%5Ftypes)) (error-id. undefined-function). The consequences are undefined if the function-name names a macro or special form.")))

(setf (documentation 'function 'functionp)
    '((kind . function)
      (syntax . "(functionp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a (normal or generic) function; otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'setf 'garef)
    '((kind . setf)
      (syntax . "(setf (garef general-array z*) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by aref or garef with obj. The returned value is obj. The constraints on the basic-array, the general-array, and the sequence of indices z is the same as for aref and garef.")))

(setf (documentation 'function 'garef)
    '((kind . function)
      (syntax . "(garef general-array z*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "aref returns the object stored in the component of the basic-array specified by the sequence of integers z. This sequence must have exactly as many elements as there are dimensions in the basic-array, and each one must satisfy 0 ≤ zi < di , di the ith dimension and 0 ≤ i < d, d the number of dimensions. Arrays are indexed 0 based, so the ith row is accessed via the index i − 1.An error shall be signaled if basic-array is not a [basic-array](#figure%5F1) (error-id. domain-error). An error shall be signaled if any z is not a non-negative integer (error-id. domain-error).garef is like aref but an error shall be signaled if its first argument, general-array, is not an object of class [general-vector](#figure%5F1) or of class [general-array\\*](#figure%5F1) (error-id. domain-error).")))

(setf (documentation 'function 'gcd)
    '((kind . function)
      (syntax . "(gcd z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "gcd returns the greatest common divisor of its integer arguments. The result is a non-negative integer. For nonzero arguments the greatest common divisor is the largest integer z such that z1 and z2 are integral multiples of z.An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'general-array*-p)
    '((kind . function)
      (syntax . "(general-array*-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "basic-array-p returns t if obj is a basic-array (instance of class basic-array); otherwise, returns nil. obj may be any ISLISP object.basic-array\\*-p returns t if obj is a basic-array\\* (instance of class basic-array\\*); otherwise, returns nil. obj may be any ISLISP object.general-array\\*-p returns t if obj is a general-array\\* (instance of class general-array\\*); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'general-vector-p)
    '((kind . function)
      (syntax . "(general-vector-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "basic-vector-p returns t if obj is a basic-vector (instance of class basic-vector); otherwise, returns nil. obj may be any ISLISP object.general-vector-p returns t if obj is a general-vector (instance of class general-vector); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'generic-function-p)
    '((kind . function)
      (syntax . "(generic-function-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a generic function; otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'gensym)
    '((kind . function)
      (syntax . "(gensym)")
      (return-type . "<symbol>")
      (min-args . 0)
      (max-args . 0)
      (desc . "Returns an unnamed symbol. gensym is useful for writing macros. It is impossible for an identifier to name an unnamed symbol.")))

(setf (documentation 'function 'get-internal-real-time)
    '((kind . function)
      (syntax . "(get-internal-real-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "get-internal-real-time returns as an integer the current time in internal time units, relative to an arbitrary time base. The difference between the values of two calls to this function is the amount of elapsed real time (i.e., clock time) between the two calls.get-internal-run-time returns as an integer the current run time in internal time units. The precise meaning of this quantity is [implementation defined](#td%5Fimplementation%5Fdefined). The difference between the values of two calls to this function is the amount of time between the two calls during which computational effort was expended on behalf of the executing program.")))

(setf (documentation 'function 'get-internal-run-time)
    '((kind . function)
      (syntax . "(get-internal-run-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "get-internal-real-time returns as an integer the current time in internal time units, relative to an arbitrary time base. The difference between the values of two calls to this function is the amount of elapsed real time (i.e., clock time) between the two calls.get-internal-run-time returns as an integer the current run time in internal time units. The precise meaning of this quantity is [implementation defined](#td%5Fimplementation%5Fdefined). The difference between the values of two calls to this function is the amount of time between the two calls during which computational effort was expended on behalf of the executing program.")))

(setf (documentation 'function 'get-output-stream-string)
    '((kind . function)
      (syntax . "(get-output-stream-string stream)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a string containing all characters written to stream since the last call to this function or since the creation of the stream, if this function has not been called with stream before. An error shall be signaled if stream is not a stream created with create-string-output-stream (error-id. domain-error).")))

(setf (documentation 'function 'get-universal-time)
    '((kind . function)
      (syntax . "(get-universal-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "Returns an approximation to the current time in Universal Time Format. The units are seconds. Universal Time Format represents time as an integer number of seconds since the beginning (i.e., midnight), January 1, 1900 UT (ignoring leap seconds). If get-universal-time is called twice, the first value shall be less than or equal to the second value.No implementation is required to have a way to verify that the time returned is correct. However, an error shall be signaled if an implementation can determine that the time it would return would not be correct (e.g., it can determine that the clock was never initialized).")))

(setf (documentation 'syntax 'go)
    '((kind . syntax)
      (syntax . "(go tagbody-tag) transfers control")
      (return-type . nil)
      (desc . "tagbody executes the forms sequentially from left to right, discarding their values. If the execution of the last form completes normally, nil is returned by the tagbody special form.The series of tagbody-tags and forms is collectively referred to as the body of a tagbody form. An identifier tagbody-tag that appears at toplevel of the body denotes a tagbody tag that can be used with go to transfer control to that point in the body. Any compound form that appears is taken as a form. Literals are not permitted at the toplevel of a tagbody. No tagbody-tag may appear more than once in the tags in the body.The namespace used for tagbody tags is distinct from that used for block tags.At any point lexically contained in the tagbody a form (go tag) can be used to transfer control to a tag tag that appears among the tagbody-tags, except where a tag is shadowed according to the lexical principle (see [§11.1](#the%5Flexical%5Fprinciple)).A tagbody-tag established by tagbody has lexical scope, but the point in the program to which it refers has dynamic extent. Once tagbody has been exited, it is no longer valid to use go to transfer to any tag in its body.The determination of which elements of the body are tagbody-tags and which are forms is made prior to any macro expansion of that element. If form is a macro form and its macro expansion is a symbol or literal, that atom is treated as a form, not as a tagbody-tag.It is a violation if a tagbody tag is other than an identifier. See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.Note: As a stylistic matter, programmers are not encouraged to use tagbody and go in everyday programming. The primary uses for which these forms are intended are for implementing other control abstractions (using macros), and for the occassional real-world situation that uses unstructured imperative transfer of control (such as a finite state machine).")))

(setf (documentation 'function 'identity)
    '((kind . function)
      (syntax . "(identity obj)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns an object that is the same as obj under eql. obj may be any ISLISP object.")))

(setf (documentation 'syntax 'if)
    '((kind . syntax)
      (syntax . "(if test-form then-form [else-form])")
      (return-type . "<object>")
      (desc . "The test-form is evaluated. If its result is anything non-nil, the then-form is evaluated and its value is returned; otherwise (if the test-form returned nil), the else-form is evaluated and its value is returned.If no else-form is provided, it defaults to nil.")))

(setf (documentation 'syntax 'ignore-errors)
    '((kind . syntax)
      (syntax . "(ignore-errors form*)")
      (return-type . "<object>")
      (desc . "Establishes a handler for error, such that if an error occurs during execution of forms, ignore-errors will immediately return nil. Then it executes forms, returning the value returned by the last form (or nil if there were no forms) if execution terminates normally.")))

(setf (documentation 'function 'initialize-object)
    '((kind . generic-function)
      (syntax . "(initialize-object instance initialization-arguments)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The generic function initialize-object is called by create to initialize a newly created instance. The generic function initialize-object is called with the new instance and a list of initialization arguments.The system-supplied primary method on initialize-object initializes the slots of the instance with values according to the _initialization-arguments_ (an alternating list of initialization argument keywords and values) and the :initform forms of the slots.The instance argument is the object to be initialized. The modified instance is returned as the result. Programmers can define methods for initialize-object to specify actions to be taken when an instance is initialized. If only :after methods are defined, they will be run after the system-supplied primary method for initialization and therefore will not interfere with the default behavior of initialize-object. The consequences are undefined if a programmer-defined primary method for this generic function does not return instance.")))

(setf (documentation 'function 'input-stream-p)
    '((kind . function)
      (syntax . "(input-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a stream that can handle input operations; otherwise, returns nil.")))

(setf (documentation 'function 'instancep)
    '((kind . function)
      (syntax . "(instancep obj class)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns t if obj is an instance (directly or otherwise) of the class class; otherwise, returns nil. obj may be any ISLISP object. An error shall be signaled if class is not a class object (error-id. domain-error).")))

(setf (documentation 'function 'integerp)
    '((kind . function)
      (syntax . "(integerp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is an integer (instance of class integer); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'internal-time-units-per-second)
    '((kind . function)
      (syntax . "(internal-time-units-per-second)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "internal-time-units-per-second returns the number of time units per second for the implementation.")))

(setf (documentation 'function 'isqrt)
    '((kind . function)
      (syntax . "(isqrt z)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the greatest integer less than or equal to the exact positive square root of z. An error shall be signaled if z is not a non-negative integer (error-id. domain-error).")))

(setf (documentation 'syntax 'labels)
    '((kind . syntax)
      (syntax . "(labels ((function-name lambda-list form*)*) body-form*)")
      (return-type . "<object>")
      (desc . "The flet and labels special forms allow the definition of new identifiers in the function namespace for function objects.In a labels special form the scope of an identifier function-name is the whole labels special form (excluding nested scopes, if any); for the flet special form, the scope of an identifier is only the body-form\\*. Within these scopes, each function-name is bound to a function object whose behavior is equivalent to (lambda lambda-list form\\*), where free identifier references are resolved as follows: 
* For a labels form, such free references are resolved in the lexical environment that was active immediately outside the labels form augmented by the function bindings for the given function-names (i.e., any reference to a function function-name refers to a binding created by the labels).
* For a flet form, free identifier references in the lambda\\-expression are resolved in the lexical environment that was active immediately outside the flet form (i.e., any reference to a function function-name are not visible).
During activation, the prepared labels or flet establishes function bindings and then evaluates each body-form in the body sequentially; the value of the last one (or nil if there is none) is the value returned by the special form activation.No function-name may appear more than once in the function bindings.")))

(setf (documentation 'syntax 'lambda)
    '((kind . syntax)
      (syntax . "(lambda lambda-list form*)")
      (return-type . "<function>")
      (desc . "Where: lambda-list ::= (identifier\\* \\[&rest identifier\\]) | (identifier\\* \\[:rest identifier\\]) and where no identifier may appear more than once in lambda-list.Execution of the lambda special form creates a function object.The scope of the identifiers of the lambda-list is the sequence of forms form\\*, collectively referred to as the body.When the prepared function is activated later (even if transported as object to some other activation) with some arguments, the body of the function is evaluated as if it was at the same textual position where the lambda special form is located, but in a context where the lambda variables are bound in the variable namespace with the values of the corresponding arguments. A &rest or :rest variable, if any, is bound to the list of the values of the remaining arguments. An error shall be signaled if the number of arguments received is incompatible with the specified lambda-list (error-id. arity-error).Once the lambda variables have been bound, the body is executed. If the body is empty, nil is returned otherwise the result of the evaluation of the last form of body is returned if the body was not left by a _non-local exit_ (see [§14.7.1](#establishing%5Fand%5Finvoking%5Fnon%5Flocal%5Fexits)).If the function receives a &rest or :rest parameter R, the list L1 to which that parameter is bound has indefinite extent. L1 is newly allocated unless the function was called with apply and R corresponds to the final argument, L2, to that call to apply (or some subtail of L2), in which case it is [implementation defined](#td%5Fimplementation%5Fdefined) whether L1 shares structure with L2.")))

(setf (documentation 'function 'lcm)
    '((kind . function)
      (syntax . "(lcm z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "lcm returns the least common multiple of its integer arguments. gcd and lcm satisfies:(= (\\* (gcd m n) (lcm m n)) (abs (\\* m n))) That is, the evaluation of the above form always return t.An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'length)
    '((kind . function)
      (syntax . "(length sequence)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the length of sequence as an integer greater than or equal to 0.When sequence is a basic-vector, length returns its dimension.When sequence is a list, the result is the number of elements in the list; if an element is itself a list, the elements within this sublist are not counted. In the case of dotted lists, length returns the number of conses at the uppermost level of the list. For example, (length '(a b . c)) ⇒ 2, since '(a b . c) ≡ (cons 'a (cons 'b 'c)).An error shall be signaled if sequence is not a basic-vector or a list (error-id. domain-error).")))

(setf (documentation 'syntax 'let)
    '((kind . syntax)
      (syntax . "(let ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The let special form is used to define a scope for a group of identifiers for a sequence of forms body-form\\* (collectively referred to as the body). The list of pairs (var form)\\* is called the let variable list. The scope of the identifier var is the body.The forms are evaluated sequentially from left to right; then each variable denoted by the identifier var is initialized to the corresponding value. Using these bindings along with the already existing bindings of visible identifiers the body-forms are evaluated. The returned value of let is the result of the evaluation of the last body-form of its body (or nil if there is none).No var may appear more than once in let variable list.Note: Although this form is a special form, one can think of it as a macro whose rewriting rules are as follows: 

(let () body-form\\*)

≡

(progn body-form\\*)

(let ((var1 form1) (var2 form2) … (varn formn)) body-form\\*) 

≡

((lambda (var1 var2 … varn) body-form\\*) form1 form2 … formn)")))

(setf (documentation 'syntax 'let*)
    '((kind . syntax)
      (syntax . "(let* ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The let\\* form is used to define a scope for a group of identifiers for a sequence of forms body-form\\* (collectively referred to as the body). The first subform (the let\\* variable list) is a list of pairs (var form). The scope of an identifier var is the body along with all form forms following the pair (var form) in the let\\* variable list.For each pair (var form) the following is done: form is evaluated in the context of the bindings in effect at that point in the evaluation. The result of the evaluation is bound to its associated variable named by the identifier var. These variable bindings enlarge the set of current valid identifiers perhaps shadowing previous variable bindings (in case some var was defined outside), and in this enlarged or modified environment the body-forms are executed. The returned value of let\\* is the result of the evaluation of the last form of its body (or nil if there is none).Note: Although this form is a special form, one can think of it as a macro whose rewriting rules are as follows: 

(let\\* () body-form\\*)

≡

(progn body-form\\*)

(let\\* ((var1 form1) (var2 form2) … (varn formn)) body-form\\*) 

≡

(let ((var1 form1)) (let ((var2 form2)) … (let ((varn formn)) body-form\\*)...))")))

(setf (documentation 'function 'list)
    '((kind . function)
      (syntax . "(list obj*)")
      (return-type . "<list>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns a new list whose length is the number of arguments and whose elements are the arguments in the same order as in the list\\-form. An error shall be signaled if the requested list cannot be allocated (error-id. cannot-create-list). Each obj may be any ISLISP object.")))

(setf (documentation 'function 'listp)
    '((kind . function)
      (syntax . "(listp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a list (instance of class list); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'log)
    '((kind . function)
      (syntax . "(log x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the natural logarithm of x. An error shall be signaled if x is not a positive number (error-id. domain-error).")))

(setf (documentation 'function 'map-into)
    '((kind . function)
      (syntax . "(map-into destination function sequence*)")
      (return-type . "<sequence>")
      (min-args . 2)
      (max-args . nil)
      (desc . "Destructively modifies destination to contain the results of applying function to successive elements in the sequences. The destination is returned.If destination and each element of sequences are not all the same length, the iteration terminates when the shortest sequence (of any of the sequences or the destination) is exhausted.The calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.An error shall be signaled if destination is not a basic-vector or a list (error-id. domain-error).An error shall be signaled if any sequence is not a basic-vector or a list (error-id. domain-error).")))

(setf (documentation 'function 'mapc)
    '((kind . function)
      (syntax . "(mapc function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcan)
    '((kind . function)
      (syntax . "(mapcan function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcar)
    '((kind . function)
      (syntax . "(mapcar function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcon)
    '((kind . function)
      (syntax . "(mapcon function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapl)
    '((kind . function)
      (syntax . "(mapl function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'maplist)
    '((kind . function)
      (syntax . "(maplist function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given function to sets of arguments determined by the given lists. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ. 

Function

Argument

Result

mapcar

successive elements

successive cons

mapc

successive elements

none (i.e., list1 returned)

mapcan

successive elements

successive destructive append

maplist

successive sublists

successive cons

mapl

successive sublists

none (i.e., list1 returned)

mapcon

successive sublists

successive destructive append

mapcar operates on successive elements of the lists. function is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the shortest list runs out, and excess elements in other lists are ignored. The value returned by mapcar is a list of the results of successive calls to function.mapc is like mapcar except that the results of applying function are not accumulated; list1 is returned.maplist is like mapcar except that function is applied to successive sublists of the lists. function is first applied to the lists themselves, and then to the cdr of each list, and then to the cdr of the cdr of each list, and so on.mapl is like maplist except that the results of applying function are not accumulated; list1 is returned.mapcan and mapcon are like mapcar and maplist respectively, except that the results of applying function are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if function is not a function (error-id. domain-error). An error shall be signaled if any list is not a list (error-id. domain-error).In all cases, the calls to function proceed from left to right, so that if function has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'max)
    '((kind . function)
      (syntax . "(max x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "The function min returns the least (closest to negative infinity) of its arguments. The comparison is done by <.The function max returns the greatest (closest to positive infinity) of its arguments. The comparison is done by \\>.An error shall be signaled if any x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'member)
    '((kind . function)
      (syntax . "(member obj list)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "If list contains at least one occurrence of obj (as determined by eql), the first sublist of list whose car is obj is returned. Otherwise, nil is returned. An error shall be signaled if list is not a list (error-id. domain-error).")))

(setf (documentation 'function 'min)
    '((kind . function)
      (syntax . "(min x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "The function min returns the least (closest to negative infinity) of its arguments. The comparison is done by <.The function max returns the greatest (closest to positive infinity) of its arguments. The comparison is done by \\>.An error shall be signaled if any x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'mod)
    '((kind . function)
      (syntax . "(mod z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "div returns the greatest integer less than or equal to the quotient of z1 and z2. An error shall be signaled if z2 is zero (error-id. division-by-zero).mod returns the remainder of the integer division of z1 by z2. The sign of the result is the sign of z2. The result lies between 0 (inclusive) and z2 (exclusive), and the difference of z1 and this result is divisible by z2 without remainder.div and mod satisfy:(= z1 (+ (\\* (div z1 z2) z2) (mod z1 z2))) That is, the evaluation of the above form always return t.An error shall be signaled if either z1 or z2 is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'next-method-p)
    '((kind . local-function)
      (syntax . "(next-method-p)")
      (return-type . "boolean")
      (min-args . 0)
      (max-args . 0)
      (desc . "The next-method-p function can be used within the body of a method defined by a method-defining form to determine whether a next method exists. The next-method-p function takes no arguments and returns t or nil.The functional binding of next-method-p is lexical within the body of the method-defining form; i.e., it is as if it were established by labels. The function object to which the binding refers has indefinite extent.")))

(setf (documentation 'value 'nil)
    '((kind . constant)
      (value-type . "<null>")
      (desc . "t is a named constant whose value is the symbol t itself. nil is a named constant whose value is the symbol nil itself.")))

(setf (documentation 'function 'not)
    '((kind . function)
      (syntax . "(not obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "This predicate is the logical “not” (or “¬”). It returns t if obj is nil and nil otherwise. obj may be any ISLISP object.")))

(setf (documentation 'function 'nreverse)
    '((kind . function)
      (syntax . "(nreverse list)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions each return a list whose elements are those of the given list, but in reverse order. An error shall be signaled if list is not a list (error-id. domain-error).For reverse, no side-effect to the given list occurs. The resulting list is permitted but not required to share structure with the input list.For nreverse, the conses which make up the top level of the given list are permitted, but not required, to be side-effected in order to produce this new list. nreverse should never be called on a literal object.")))

(setf (documentation 'function 'null)
    '((kind . function)
      (syntax . "(null obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is nil; otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'numberp)
    '((kind . function)
      (syntax . "(numberp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a number (instance of class number); otherwise, returns nil. The obj may be any ISLISP object.")))

(setf (documentation 'function 'open-input-file)
    '((kind . function)
      (syntax . "(open-input-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "open-input-file opens a file for input only. open-output-file opens a file for output only. open-io-file opens a file for both input and output.An error shall be signaled if filename is not a string. The corresponding file is opened in an [implementation-defined](#td%5Fimplementation%5Fdefined) way. These functions return an instance of the stream class connected to the file specified by filename.The element-class can be either the class character (the default) or a positive integer that is a number of bits in a byte to be used for a binary stream. All implementations must support a value of 8 (denoting integer byte values from 0 to 255), but some implementations might support other byte sizes as well.Note: When a binary stream is used, the order of bits within bytes and bytes within words (i.e., whether it is right to left or left to right) are each [implementation defined](#td%5Fimplementation%5Fdefined).")))

(setf (documentation 'function 'open-io-file)
    '((kind . function)
      (syntax . "(open-io-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "open-input-file opens a file for input only. open-output-file opens a file for output only. open-io-file opens a file for both input and output.An error shall be signaled if filename is not a string. The corresponding file is opened in an [implementation-defined](#td%5Fimplementation%5Fdefined) way. These functions return an instance of the stream class connected to the file specified by filename.The element-class can be either the class character (the default) or a positive integer that is a number of bits in a byte to be used for a binary stream. All implementations must support a value of 8 (denoting integer byte values from 0 to 255), but some implementations might support other byte sizes as well.Note: When a binary stream is used, the order of bits within bytes and bytes within words (i.e., whether it is right to left or left to right) are each [implementation defined](#td%5Fimplementation%5Fdefined).")))

(setf (documentation 'function 'open-output-file)
    '((kind . function)
      (syntax . "(open-output-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "open-input-file opens a file for input only. open-output-file opens a file for output only. open-io-file opens a file for both input and output.An error shall be signaled if filename is not a string. The corresponding file is opened in an [implementation-defined](#td%5Fimplementation%5Fdefined) way. These functions return an instance of the stream class connected to the file specified by filename.The element-class can be either the class character (the default) or a positive integer that is a number of bits in a byte to be used for a binary stream. All implementations must support a value of 8 (denoting integer byte values from 0 to 255), but some implementations might support other byte sizes as well.Note: When a binary stream is used, the order of bits within bytes and bytes within words (i.e., whether it is right to left or left to right) are each [implementation defined](#td%5Fimplementation%5Fdefined).")))

(setf (documentation 'function 'open-stream-p)
    '((kind . function)
      (syntax . "(open-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is an open stream; otherwise, returns nil.")))

(setf (documentation 'syntax 'or)
    '((kind . syntax)
      (syntax . "(or form*)")
      (return-type . "<object>")
      (desc . "or is the sequential logical or (or ∨). forms are evaluated from left to right until either one of them evaluates to a non-nil value or else none are left. If one of them evaluates to a non-nil value, then this non-nil value is returned, otherwise nil is returned. The form or is equivalent to the following:(or) ≡ 'nil (or form) ≡ form(or form1 form2 … formn) ≡ ((lambda (var) (if var var (or form2 … formn))) form1) where var does not occur in form2 … formn")))

(setf (documentation 'function 'output-stream-p)
    '((kind . function)
      (syntax . "(output-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a stream that can handle output operations; otherwise, returns nil.")))

(setf (documentation 'function 'parse-error-expected-class)
    '((kind . function)
      (syntax . "(parse-error-expected-class parse-error)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the string and expected-class supplied as data when creating the parse-error. An error shall be signaled if parse-error is not a condition of class parse-error (error-id. domain-error).")))

(setf (documentation 'function 'parse-error-string)
    '((kind . function)
      (syntax . "(parse-error-string parse-error)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the string and expected-class supplied as data when creating the parse-error. An error shall be signaled if parse-error is not a condition of class parse-error (error-id. domain-error).")))

(setf (documentation 'function 'parse-number)
    '((kind . function)
      (syntax . "(parse-number string)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The characters belonging to string are scanned (as if by [read](#f%5Fread)) and if the resulting lexeme is the textual representation of a number, the number it represents is returned.An error shall be signaled if string is not a string (error-id. domain-error). An error shall be signaled if string is not the textual representation of a number (error-id. cannot-parse-number).")))

(setf (documentation 'function 'preview-char)
    '((kind . function)
      (syntax . "(preview-char [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the next character of input-stream, if any. The character is not consumed; the next attempt to peek at or read a character from the stream sees that same character.See [§27.1](#argument%5Fconventions) for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'probe-file)
    '((kind . function)
      (syntax . "(probe-file filename)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if the file specified by filename exists; otherwise, returns nil. An error shall be signaled if filename is not a string (error-id. domain-error).")))

(setf (documentation 'syntax 'progn)
    '((kind . syntax)
      (syntax . "(progn form*)")
      (return-type . "<object>")
      (desc . "This special form allows a series of forms to be evaluated, where normally only one could be used.The result of evaluation of the last form of form\\* is returned. All the forms are evaluated from left to right. The values of all the forms but the last are discarded, so they are executed only for their side-effects. progn without forms returns nil.")))

(setf (documentation 'setf 'property)
    '((kind . setf)
      (syntax . "(setf (property symbol property-name) obj)")
      (return-type . "<object>")
      (desc . "Causes obj to be the new value of the property named property-name asssociated with the symbol symbol. If the property named property-name already exists, its corresponding property value is replaced; otherwise, a new property is created. obj is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. domain-error). obj may be any ISLISP object")))

(setf (documentation 'function 'property)
    '((kind . function)
      (syntax . "(property symbol property-name [obj])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the value of the property named property-name associated with the symbol symbol. If symbol has no property named property-name, obj (which defaults to nil) is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. domain-error). obj may be any ISLISP object")))

(setf (documentation 'syntax 'quote)
    '((kind . syntax)
      (syntax . "(quote obj)")
      (return-type . "<object>")
      (desc . "'obj → objectA quoted expression denotes a reference to an object. This notation is used to include any object in an ISLISP text.The character ' (apostrophe or single quote) is syntax for quotation. That is, (quote a) ≡ 'a.The result of the evaluation of the quote special form is obj.")))

(setf (documentation 'function 'quotient)
    '((kind . function)
      (syntax . "(quotient dividend divisor+)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . nil)
      (desc . "The function reciprocal returns the reciprocal of its argument x; that is, 1/x. An error shall be signaled if x is zero (error-id. division-by-zero).The function quotient, given two arguments dividend and divisor, returns the quotient of those numbers. The result is an integer if dividend and divisor are integers and divisor evenly divides dividend, otherwise it will be a ﬂoat.Given more than two arguments, quotient operates iteratively on each of the divisor1 … divisorn as in dividend / divisor1 / … / divisorn. The type of the result follows from the two-argument case because the three-or-more-argument quotient can be defined as follows:(quotient dividend divisor1 divisor2 …) ≡ (quotient (quotient dividend divisor1) divisor2 …) An error shall be signaled if dividend is not a number (error-id. domain-error). An error shall be signaled if any divisor is not a number (error-id. domain-error). An error shall be signaled if any divisor is zero (error-id. division-by-zero).")))

(setf (documentation 'function 'read)
    '((kind . function)
      (syntax . "(read [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "The function read returns the ISLISP object that is created as the result of reading its textual representation from the stream input-stream.See [§27.1](#argument%5Fconventions) for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'read-byte)
    '((kind . function)
      (syntax . "(read-byte input-stream [eos-error-p [eos-value]])")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Reads a byte from the input-stream and returns it. The number of bits in a byte is determined by the stream element type of the input-stream; see [open-input-file](#f%5Fopen%5Finput%5Ffile).See [§27.1](#argument%5Fconventions) for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'read-char)
    '((kind . function)
      (syntax . "(read-char [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "read-char reads a single character from input-stream and returns the corresponding character object.See [§27.1](#argument%5Fconventions) for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'read-line)
    '((kind . function)
      (syntax . "(read-line [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Reads a line of characters from input-stream and returns them as a string (without the newline character at the end of the line). If an end-of-stream is reached before the next newline character and a non-empty line has been read prior to the end-of-stream, that line is returned.See [§27.1](#argument%5Fconventions) for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'reciprocal)
    '((kind . function)
      (syntax . "(reciprocal x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function reciprocal returns the reciprocal of its argument x; that is, 1/x. An error shall be signaled if x is zero (error-id. division-by-zero).The function quotient, given two arguments dividend and divisor, returns the quotient of those numbers. The result is an integer if dividend and divisor are integers and divisor evenly divides dividend, otherwise it will be a ﬂoat.Given more than two arguments, quotient operates iteratively on each of the divisor1 … divisorn as in dividend / divisor1 / … / divisorn. The type of the result follows from the two-argument case because the three-or-more-argument quotient can be defined as follows:(quotient dividend divisor1 divisor2 …) ≡ (quotient (quotient dividend divisor1) divisor2 …) An error shall be signaled if dividend is not a number (error-id. domain-error). An error shall be signaled if any divisor is not a number (error-id. domain-error). An error shall be signaled if any divisor is zero (error-id. division-by-zero).")))

(setf (documentation 'function 'remove-property)
    '((kind . function)
      (syntax . "(remove-property symbol property-name)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Removes the property property-name associated with symbol and returns the property value of the removed property if there is such a property. If there is no such property, nil is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. domain-error).")))

(setf (documentation 'function 'report-condition)
    '((kind . generic-function)
      (syntax . "(report-condition condition stream)")
      (return-type . "<condition>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Presents a natural language description of condition to stream. This generic function may be specialized for user-defined condition classes.")))

(setf (documentation 'syntax 'return-from)
    '((kind . syntax)
      (syntax . "(return-from name result-form transfers control and data")
      (return-type . nil)
      (desc . "The block special form executes each form sequentially from left to right. If the last form exits normally, whatever it returns is returned by the block form.The name in a block form is not evaluated; it must be an identifier. The scope of name is the body form\\*—only a return-from textually contained in some form can exit the block. The extent of name is dynamic.If a return-from is executed, the result-form is evaluated. If this evaluation returns normally, the value it returns is immediately returned from the innermost lexically enclosing block form with the same name.return-from is used to return from a block. name is not evaluated and must be an identifier. A block special form must lexically enclose the occurrence of return-from; the value produced by result-form is immediately returned from the block. The return-from form never returns and does not have a value.An error shall be signaled if an attempt is made to exit a block after it has been exited (error-id. control-error); It is a violation if name is not an identifier. It is a violation if a block with a corresponding name does not exist. See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.")))

(setf (documentation 'function 'reverse)
    '((kind . function)
      (syntax . "(reverse list)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions each return a list whose elements are those of the given list, but in reverse order. An error shall be signaled if list is not a list (error-id. domain-error).For reverse, no side-effect to the given list occurs. The resulting list is permitted but not required to share structure with the input list.For nreverse, the conses which make up the top level of the given list are permitted, but not required, to be side-effected in order to produce this new list. nreverse should never be called on a literal object.")))

(setf (documentation 'function 'round)
    '((kind . function)
      (syntax . "(round x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the integer nearest to x. If x is exactly halfway between two integers, the even one is chosen. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'set-aref)
    '((kind . function)
      (syntax . "(set-aref obj basic-array z*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "These replace the object obtainable by aref or garef with obj. The returned value is obj. The constraints on the basic-array, the general-array, and the sequence of indices z is the same as for aref and garef.")))

(setf (documentation 'function 'set-car)
    '((kind . function)
      (syntax . "(set-car obj cons)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Updates the left component of cons with obj. The returned value is obj. An error shall be signaled if cons is not a cons (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'function 'set-cdr)
    '((kind . function)
      (syntax . "(set-cdr obj cons)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Updates the right component of cons with obj. The returned value is obj . An error shall be signaled if cons is not a cons (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'syntax 'set-dynamic)
    '((kind . syntax)
      (syntax . "(set-dynamic form var)")
      (return-type . "<object>")
      (desc . "This special form denotes an assignment to a dynamic variable. This form can appear anywhere that (dynamic var) can appear.form is evaluated and the result of the evaluation is used to change the dynamic binding of var.An error shall be signaled if var has no dynamic value (error-id. unbound-variable). setf of dynamic can be used only for modifying bindings, and not for establishing them.")))

(setf (documentation 'function 'set-elt)
    '((kind . function)
      (syntax . "(set-elt obj sequence z)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "These replace the object obtainable by elt with obj. The returned value is obj.An error shall be signaled if z is an integer outside of the valid range of indices (error-id. index-out-of-range). An error shall be signaled if sequence is not a basic-vector or a list or if z is not an integer (error-id. domain-error). obj may be any ISLISP object.")))

(setf (documentation 'function 'set-file-position)
    '((kind . function)
      (syntax . "(set-file-position stream z)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Attempts to change the file position (see [file-position](#f%5Ffile%5Fposition)) of the stream stream to z . If it is not possible to move to the exact position z , some [implementation-defined](#td%5Fimplementation%5Fdefined) motion within the file might still be performed. The value returned is the new file position, which might or might not be z.An error shall be signaled if stream is not a stream to or from a file, or if z is not a non-negative integer (error-id. domain-error).")))

(setf (documentation 'function 'set-garef)
    '((kind . function)
      (syntax . "(set-garef obj general-array z*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "These replace the object obtainable by aref or garef with obj. The returned value is obj. The constraints on the basic-array, the general-array, and the sequence of indices z is the same as for aref and garef.")))

(setf (documentation 'function 'set-property)
    '((kind . function)
      (syntax . "(set-property obj symbol property-name)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Causes obj to be the new value of the property named property-name asssociated with the symbol symbol. If the property named property-name already exists, its corresponding property value is replaced; otherwise, a new property is created. obj is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. domain-error). obj may be any ISLISP object")))

(setf (documentation 'syntax 'setf)
    '((kind . syntax)
      (syntax . "(setf place form)")
      (return-type . "<object>")
      (desc . "This macro is used for generalized assignment.setf takes a place and stores in this place the result of the evaluation of the form form. The place form is not evaluated as a whole entity, but subforms of place are evaluated sequentially from left to right to determine a place to be assigned a value. When place is denoted by an identifier, setf behaves exactly as setq. The returned value is the result of the evaluation of form. The valid places for the setf special form are as follows: 

variables

var

dynamic bindings

([dynamic](#s%5Fdynamic) var)

the components of a basic-array

([aref](#f%5Faref) basic-array z1 … zn)

the components of a general array

([garef](#f%5Fgaref) general-array z1 … zn)

the components of a list

([elt](#f%5Felt) list z)

the components of a vector

([elt](#f%5Felt) basic-vector z)

the left component of a cons

([car](#f%5Fcar) cons)

the right component of a cons

([cdr](#f%5Fcdr) cons)

a property of a symbol

([property](#f%5Fproperty) symbol property)

a slot of an instance of a class

(reader-function-name instance)

A place can also be a macro form that expands (during preparation for execution) into a place or a function application form with operator op for which setf is defined or for which a generic function named (setf op) has been defined. In these last two cases, that function will receive as arguments the new value to be assigned followed by the objects that resulted from evaluating the arguments of the function application form.")))

(setf (documentation 'syntax 'setq)
    '((kind . syntax)
      (syntax . "(setq var form)")
      (return-type . "<object>")
      (desc . "This form represents an assignment to the variable denoted by the identifier. In consequence, the identifier may designate a different object than before, the value of form.The result of the evaluation of form is returned. This result is used to modify the variable binding denoted by the identifier var (if it is mutable). setq can be used only for modifying bindings, and not for establishing a variable. The setq special form must be contained in the scope of var , established by [defglobal](#def%5Fdefglobal), [let](#s%5Flet), [let\\*](#s%5Flet%5Fs), [for](#s%5Ffor), or a [lambda](#s%5Flambda) expression.")))

(setf (documentation 'function 'signal-condition)
    '((kind . function)
      (syntax . "(signal-condition condition continuable)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Invokes the condition handling system on condition.If continuable is nil, the results of attempting to “continue” (see [continue-condition](#f%5Fcontinue%5Fcondition)) are not defined except that the call to signal-condition will not return normally.If continuable is not nil, it will be possible to return from the call to signal-condition (see [continue-condition](#f%5Fcontinue%5Fcondition)). In this case, the specific value of continuable may be a string indicating the effect of continuing, or it may be the symbol t, indicating that an [implementation-defined](#td%5Fimplementation%5Fdefined) string such as \"Continue with no special action.\" is to be used.")))

(setf (documentation 'function 'simple-error-format-arguments)
    '((kind . function)
      (syntax . "(simple-error-format-arguments simple-error)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the format-string and format-arguments supplied as data when creating the simple-error. An error shall be signaled if simple-error is not a condition of class simple-error (error-id. domain-error).")))

(setf (documentation 'function 'simple-error-format-string)
    '((kind . function)
      (syntax . "(simple-error-format-string simple-error)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the format-string and format-arguments supplied as data when creating the simple-error. An error shall be signaled if simple-error is not a condition of class simple-error (error-id. domain-error).")))

(setf (documentation 'function 'sin)
    '((kind . function)
      (syntax . "(sin x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sin returns the sine of x . The function cos returns the cosine of x. The function tan returns the tangent of x. In each case, x must be given in radians.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'sinh)
    '((kind . function)
      (syntax . "(sinh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sinh returns the hyperbolic sine of x. The function cosh returns the hyperbolic cosine of x. The function tanh returns the hyperbolic tangent of x.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'syntax 'special-operator)
    '((kind . syntax)
      (syntax . "(special-operator argument*)")
      (return-type . "<result-class>")
      (desc . nil)))

(setf (documentation 'function 'sqrt)
    '((kind . function)
      (syntax . "(sqrt x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the non-negative square root of x. An error shall be signaled if x is not a non-negative number (error-id. domain-error).")))

(setf (documentation 'function 'standard-input)
    '((kind . function)
      (syntax . "(standard-input)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function standard-input returns the stream used as the default stream for input functions.The function standard-output returns the stream used as the default stream for output functions.The function error-output returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially [implementation-defined](#td%5Fimplementation%5Fdefined), but can be dynamically bound; see [with-standard-input](#s%5Fwith%5Fstandard%5Finput), [with-standard-output](#s%5Fwith%5Fstandard%5Foutput), and [with-error-output](#s%5Fwith%5Ferror%5Foutput).")))

(setf (documentation 'function 'standard-output)
    '((kind . function)
      (syntax . "(standard-output)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function standard-input returns the stream used as the default stream for input functions.The function standard-output returns the stream used as the default stream for output functions.The function error-output returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially [implementation-defined](#td%5Fimplementation%5Fdefined), but can be dynamically bound; see [with-standard-input](#s%5Fwith%5Fstandard%5Finput), [with-standard-output](#s%5Fwith%5Fstandard%5Foutput), and [with-error-output](#s%5Fwith%5Ferror%5Foutput).")))

(setf (documentation 'function 'stream-error-stream)
    '((kind . function)
      (syntax . "(stream-error-stream stream-error)")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the stream supplied as data when creating the stream-error. An error shall be signaled if stream-error is not a condition of class stream-error (error-id. domain-error).")))

(setf (documentation 'function 'stream-ready-p)
    '((kind . function)
      (syntax . "(stream-ready-p input-stream)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if an attempt to obtain the next element from the stream will not cause the processor to have to wait; otherwise, returns nil. An error shall be signaled if stream is not a stream that can handle input operations (error-id. domain-error).")))

(setf (documentation 'function 'streamp)
    '((kind . function)
      (syntax . "(streamp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a stream (instance of class stream); otherwise, returns nil. obj may be any ISLISP object. streamp is unaffected by whether its argument, if an instance of the class stream, is open or closed.")))

(setf (documentation 'function 'string-append)
    '((kind . function)
      (syntax . "(string-append string*)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a single string containing a sequence of characters that results from appending the sequences of characters of each of the strings, or \"\" if given no strings. An error shall be signaled if any string is not a string (error-id. domain-error).This function does not modify its arguments. It is [implementation defined](#td%5Fimplementation%5Fdefined) whether and when the result shares structure with its string arguments.An error shall be signaled if the string cannot be allocated (error-id. cannot-create-string).")))

(setf (documentation 'function 'string-index)
    '((kind . function)
      (syntax . "(string-index substring string [start-position])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the position of the given substring within string. The search starts from the position indicated by start-position (which is 0-based and defaults to 0). The value returned if the search succeeds is an offset from the beginning of the string, not from the starting point. If that substring does not occur in the string, nil is returned. Presence of the substring is done by sequential use of char= on corresponding elements of the two strings.An error shall be signaled if either substring or string is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string/=)
    '((kind . function)
      (syntax . "(string/= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string<)
    '((kind . function)
      (syntax . "(string< string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string<=)
    '((kind . function)
      (syntax . "(string<= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string=)
    '((kind . function)
      (syntax . "(string= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string>)
    '((kind . function)
      (syntax . "(string> string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'string>=)
    '((kind . function)
      (syntax . "(string>= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function string= tests whether string1 is the same string as string2. The function string< tests whether string1 is less than string2. The function string<= tests whether string1 is less than or equal to string2.The ordering used is based on character comparisons.Two strings are string= if they are of the same length, l , and if for every i, where 0 ≤ i < l, (char= (elt string1 i) (elt string2 i)) holds.Two strings string1 and string2 are in order (string<) if in the first position in which they differ the character of string1 is char< the corresponding character of string2, or if the string1 is a proper prefix of string2 (of shorter length and matching in all the characters of string1).Two strings are string<= if they are either string< or they are string=.Two strings are string/= if and only if they are not string=. Two strings are string> if and only if they are not string<=. Two strings are string>= if and only if they are not string<.For these 6 string comparison functions, if the test is satisfied, an [implementation-defined](#td%5Fimplementation%5Fdefined) non-nil value is returned; otherwise, nil is returned.An error shall be signaled if either string1 or string2 is not a string (error-id. domain-error).")))

(setf (documentation 'function 'stringp)
    '((kind . function)
      (syntax . "(stringp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a string (instance of class string); otherwise, returns nil. obj may be any ISLISP object.")))

(setf (documentation 'function 'subclassp)
    '((kind . function)
      (syntax . "(subclassp class1 class2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns t if the class class1 is a subclass of the class class2; otherwise, returns nil. An error shall be signaled if either class1 or class2 is not a class object (error-id. domain-error).")))

(setf (documentation 'function 'subseq)
    '((kind . function)
      (syntax . "(subseq sequence z1 z2)")
      (return-type . "<sequence>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Given a sequence sequence and two integers z1 and z2 satisfying 0 ≤ z1 ≤ z2 ≤ (length sequence), this function returns the subsequence of length z2 − z1, containing the elements with indices from z1 (inclusive) to z2 (exclusive). The subsequence is newly allocated, and has the same class as sequence.An error shall be signaled if the requested subsequence cannot be allocated (error-id. cannot-create-sequence). An error shall be signaled if z1 or z2 are outside of the bounds mentioned (error-id. index-out-of-range). An error shall be signaled if sequence is not a basic-vector or a list, or if z1 is not an integer, or if z2 is not an integer (error-id. domain-error).")))

(setf (documentation 'function 'symbolp)
    '((kind . function)
      (syntax . "(symbolp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is a symbol (instance of class symbol); otherwise, returns nil. The obj may be any ISLISP object.")))

(setf (documentation 'value 't)
    '((kind . constant)
      (value-type . "<symbol>")
      (desc . "t is a named constant whose value is the symbol t itself. nil is a named constant whose value is the symbol nil itself.")))

(setf (documentation 'syntax 'tagbody)
    '((kind . syntax)
      (syntax . "(tagbody {tagbody-tag | form}*)")
      (return-type . "<object>")
      (desc . "tagbody executes the forms sequentially from left to right, discarding their values. If the execution of the last form completes normally, nil is returned by the tagbody special form.The series of tagbody-tags and forms is collectively referred to as the body of a tagbody form. An identifier tagbody-tag that appears at toplevel of the body denotes a tagbody tag that can be used with go to transfer control to that point in the body. Any compound form that appears is taken as a form. Literals are not permitted at the toplevel of a tagbody. No tagbody-tag may appear more than once in the tags in the body.The namespace used for tagbody tags is distinct from that used for block tags.At any point lexically contained in the tagbody a form (go tag) can be used to transfer control to a tag tag that appears among the tagbody-tags, except where a tag is shadowed according to the lexical principle (see [§11.1](#the%5Flexical%5Fprinciple)).A tagbody-tag established by tagbody has lexical scope, but the point in the program to which it refers has dynamic extent. Once tagbody has been exited, it is no longer valid to use go to transfer to any tag in its body.The determination of which elements of the body are tagbody-tags and which are forms is made prior to any macro expansion of that element. If form is a macro form and its macro expansion is a symbol or literal, that atom is treated as a form, not as a tagbody-tag.It is a violation if a tagbody tag is other than an identifier. See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.Note: As a stylistic matter, programmers are not encouraged to use tagbody and go in everyday programming. The primary uses for which these forms are intended are for implementing other control abstractions (using macros), and for the occassional real-world situation that uses unstructured imperative transfer of control (such as a finite state machine).")))

(setf (documentation 'function 'tan)
    '((kind . function)
      (syntax . "(tan x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sin returns the sine of x . The function cos returns the cosine of x. The function tan returns the tangent of x. In each case, x must be given in radians.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'tanh)
    '((kind . function)
      (syntax . "(tanh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function sinh returns the hyperbolic sine of x. The function cosh returns the hyperbolic cosine of x. The function tanh returns the hyperbolic tangent of x.An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'syntax 'the)
    '((kind . syntax)
      (syntax . "(the class-name form)")
      (return-type . "<object>")
      (desc . "These forms evaluate form. If form returns, the returned value is returned by the the or assure form. In addition, these forms specify that the value of form is of the class specified by class-name (which must be the name of an existing class).In a the special form, the consequences are undefined if the value of form is not of the class or a subclass of the class designated by class-name. In an assure special form, an error shall be signaled if the value of form is not of the class or a subclass of the class designated by class-name (error-id. domain-error).")))

(setf (documentation 'syntax 'throw)
    '((kind . syntax)
      (syntax . "(throw tag-form result-form) transfers control and data")
      (return-type . nil)
      (desc . "The special forms catch and throw provide a facility for programming of structured non-local dynamic exits. A catch form and a throw form are said to correspond if the tag-form of the catch and the tag-form of the throw evaluate to the same object, a catch tag. A catch tag may be any object other than a number or a character; the comparison of catch tags uses eq.The catch special form first evaluates the tag-form to produce a catch tag, and then executes each form sequentially from left to right. If execution of the forms finishes normally, whatever is returned by the last form is returned by the catch form.Prior to execution of the forms of a catch form C0, an association between the catch tag T0 and the executing form C0 is dynamically established, upon exit from C0, the association is disestablished. If there was an outer association for the same catch tag T0, it is hidden during the execution of C0's forms; only the most recently established (i.e., innermost) association for T0 is ever visible.If a throw special form is executed, it evaluates the tag-form producing a catch tag T1, and then evaluates the result-form producing a result R1. If there is a corresponding association between T1 and some catch form C1 that is executing, R1 is immediately returned as the value of C1. The throw form can be anywhere in the entire current toplevel scope; it need not be lexically contained within C1.An error shall be signaled if there is no outstanding catcher for a T1 (error-id. control-error). See [§14.7.2](#assuring%5Fdata%5Fconsistency) for other errors.")))

(setf (documentation 'function 'truncate)
    '((kind . function)
      (syntax . "(truncate x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the integer between 0 and x (inclusive) that is nearest to x. That is, x is truncated towards zero. An error shall be signaled if x is not a number (error-id. domain-error).")))

(setf (documentation 'function 'undefined-entity-name)
    '((kind . function)
      (syntax . "(undefined-entity-name undefined-entity)")
      (return-type . "<symbol>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the name and namespace supplied as data when creating the undefined-entity. An error shall be signaled if undefined-entity is not a condition of class undefined-entity (error-id. domain-error).The result of undefined-entity-namespace will be one of the symbols variable, dynamic-variable, function, or class.")))

(setf (documentation 'function 'undefined-entity-namespace)
    '((kind . function)
      (syntax . "(undefined-entity-namespace undefined-entity)")
      (return-type . "<symbol>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the name and namespace supplied as data when creating the undefined-entity. An error shall be signaled if undefined-entity is not a condition of class undefined-entity (error-id. domain-error).The result of undefined-entity-namespace will be one of the symbols variable, dynamic-variable, function, or class.")))

(setf (documentation 'syntax 'unwind-protect)
    '((kind . syntax)
      (syntax . "(unwind-protect form cleanup-form*)")
      (return-type . "<object>")
      (desc . "unwind-protect first evaluates form. Evaluation of the cleanup-forms always occurs, regardless of whether the exit is normal or non-local.If the form exits normally yielding a value R, then if all of the cleanup-forms exit normally the value R is returned by the unwind-protect form.If a non-local exit from form occurs, then the cleanup-forms are executed as part of that exit, and then if all of the cleanup-forms exit normally the original non-local exit continues.The cleanup-forms are evaluated from left to right, discarding the resulting values. If execution of the cleanup-forms finishes normally, exit from the unwind-protect form proceeds as described above. It is permissible for a cleanup-form to contain a non-local exit from the unwind-protect form, subject to the following constraint:An error shall be signaled if during execution of the cleanup-forms of an unwind-protect form, a non-local exit is executed to a destination which has been marked as invalid due to some other non-local exit that is already in progress (see [§14.7.1](#establishing%5Fand%5Finvoking%5Fnon%5Flocal%5Fexits)) (error-id. control-error).Note: Because ISLISP does not specify an interactive debugger, it is unspecified whether or how error recovery can occur interactively if programmatic handling fails. The intent is that if the ISLISP processor does not terminate abnormally, normal mechanisms for non-local exit (return-from, throw, or go) would be used as necessary and would respect these cleanup-forms.")))

(setf (documentation 'function 'vector)
    '((kind . function)
      (syntax . "(vector obj*)")
      (return-type . "<general-vector>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns a new general-vector whose elements are its obj arguments. The length of the newly created vector is, therefore, the number of objs passed as arguments. The vector is indexed by integers ranging from 0 to dimension−1\\. An error shall be signaled if the requested vector cannot be allocated (error-id. cannot-create-vector). Each obj may be any ISLISP object.")))

(setf (documentation 'syntax 'while)
    '((kind . syntax)
      (syntax . "(while test-form body-form*)")
      (return-type . "<null>")
      (desc . "Iterates while the test-form returns a true value. Specifically: 
* test-form is evaluated, producing a value Vt.
* If Vt is nil, then the while form immediately returns nil.
* Otherwise, if Vt is non-nil, the forms body-form\\* are evaluated sequentially (from left to right).
* Upon successful completion of the body-forms\\*, the while form begins again with step 1.")))

(setf (documentation 'syntax 'with-error-output)
    '((kind . syntax)
      (syntax . "(with-error-output stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their stream-form argument to produce a stream s and then evaluate their body forms in a dynamic environment where the corresponding function (standard-input, standard-output, or error-output) returns the stream s. The returned value of each of these forms is the result of the evaluation of the last form of their body (or nil if there is none).")))

(setf (documentation 'syntax 'with-handler)
    '((kind . syntax)
      (syntax . "(with-handler handler form*)")
      (return-type . "<object>")
      (desc . "Evaluates handler, which must yield a function (called the “handler function”). The handler function is established as active handler (see [§29.2](#signaling%5Fand%5Fhandling)) and then the forms are executed. If execution of forms finishes normally, the value of the last form (or nil if there are no forms) is returned.")))

(setf (documentation 'syntax 'with-open-input-file)
    '((kind . syntax)
      (syntax . "(with-open-input-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using open-input-file, open-output-file, or open-io-file, respectively), evaluates the forms, closes the file, and returns the value returned by the last form (or nil if there are no forms).The filename and element-class are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by name (as if a let was used), so the identifier name can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-open-io-file)
    '((kind . syntax)
      (syntax . "(with-open-io-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using open-input-file, open-output-file, or open-io-file, respectively), evaluates the forms, closes the file, and returns the value returned by the last form (or nil if there are no forms).The filename and element-class are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by name (as if a let was used), so the identifier name can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-open-output-file)
    '((kind . syntax)
      (syntax . "(with-open-output-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using open-input-file, open-output-file, or open-io-file, respectively), evaluates the forms, closes the file, and returns the value returned by the last form (or nil if there are no forms).The filename and element-class are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by name (as if a let was used), so the identifier name can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-standard-input)
    '((kind . syntax)
      (syntax . "(with-standard-input stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their stream-form argument to produce a stream s and then evaluate their body forms in a dynamic environment where the corresponding function (standard-input, standard-output, or error-output) returns the stream s. The returned value of each of these forms is the result of the evaluation of the last form of their body (or nil if there is none).")))

(setf (documentation 'syntax 'with-standard-output)
    '((kind . syntax)
      (syntax . "(with-standard-output stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their stream-form argument to produce a stream s and then evaluate their body forms in a dynamic environment where the corresponding function (standard-input, standard-output, or error-output) returns the stream s. The returned value of each of these forms is the result of the evaluation of the last form of their body (or nil if there is none).")))

(setf (documentation 'function 'write-byte)
    '((kind . function)
      (syntax . "(write-byte z output-stream)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Writes z to the output-stream and returns it. An error shall be signaled if z is not an integer in the range appropriate to the stream element type of output-stream or if output-stream is not a stream capable of handling output operations (error-id. domain-error).")))
(setf (documentation 'function '-)
    '((kind . function)
      (syntax . "(- x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "Given one argument, _x_, this function returns its additive inverse. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).If an implementation supports a -0.0 that is distinct from 0.0, then (- 0.0) returns -0.0; in implementations where -0.0 and 0.0 are not distinct, (- 0.0) returns 0.0.")))

(setf (documentation 'function '*)
    '((kind . function)
      (syntax . "(* x*)")
      (return-type . "<number>")
      (min-args . 0)
      (max-args . nil)
      (desc . "The functions *+* and *&#42;* return the sum and the product, respectively, of their arguments. If all arguments are integers, the result is an integer. If any argument is a ﬂoat, the result is a ﬂoat. When given no arguments, *+* returns 0 and *&#42;* returns 1. An error shall be signaled if any _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'value '*most-negative-float*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of *&#42;most-positive-float&#42;* is the implementation-dependent ﬂoating-point number closest to negative infinity.")))

(setf (documentation 'value '*most-positive-float*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of *&#42;most-positive-float&#42;* is the implementation-dependent ﬂoating-point number closest to negative infinity.")))

(setf (documentation 'value '*pi*)
    '((kind . constant)
      (value-type . "<float>")
      (desc . "The value of this constant is an approximation of π.")))

(setf (documentation 'function '/=)
    '((kind . function)
      (syntax . "(/= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns *t* if _x~1~_ and _x~2~_ have mathematically distinct values; otherwise, returns *nil*. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function '+)
    '((kind . function)
      (syntax . "(+ x*)")
      (return-type . "<number>")
      (min-args . 0)
      (max-args . nil)
      (desc . "The functions *+* and *&#42;* return the sum and the product, respectively, of their arguments. If all arguments are integers, the result is an integer. If any argument is a ﬂoat, the result is a ﬂoat. When given no arguments, *+* returns 0 and *&#42;* returns 1. An error shall be signaled if any _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function '<)
    '((kind . function)
      (syntax . "(< x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*>=* returns *t* if _x~1~_ is greater than or *=* _x~2~_. *<=* returns *t* if _x~1~_ is less than or *=* _x~2~_. *>* returns *t* if _x~1~_ is greater than _x~2~_. *<* returns *t* if _x~1~_ is less than _x~2~_.The mathematical values of the arguments are compared. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function '<=)
    '((kind . function)
      (syntax . "(<= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*>=* returns *t* if _x~1~_ is greater than or *=* _x~2~_. *<=* returns *t* if _x~1~_ is less than or *=* _x~2~_. *>* returns *t* if _x~1~_ is greater than _x~2~_. *<* returns *t* if _x~1~_ is less than _x~2~_.The mathematical values of the arguments are compared. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function '=)
    '((kind . function)
      (syntax . "(= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns *t* if _x~1~_ has the same mathematical value as _x~2~_; otherwise, returns *nil*. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).Note: *=* differs from *eql* because *=* compares only the mathematical values of its arguments, whereas *eql* also compares the representations.")))

(setf (documentation 'function '>)
    '((kind . function)
      (syntax . "(> x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*>=* returns *t* if _x~1~_ is greater than or *=* _x~2~_. *<=* returns *t* if _x~1~_ is less than or *=* _x~2~_. *>* returns *t* if _x~1~_ is greater than _x~2~_. *<* returns *t* if _x~1~_ is less than _x~2~_.The mathematical values of the arguments are compared. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function '>=)
    '((kind . function)
      (syntax . "(>= x1 x2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*>=* returns *t* if _x~1~_ is greater than or *=* _x~2~_. *<=* returns *t* if _x~1~_ is less than or *=* _x~2~_. *>* returns *t* if _x~1~_ is greater than _x~2~_. *<* returns *t* if _x~1~_ is less than _x~2~_.The mathematical values of the arguments are compared. An error shall be signaled if either _x~1~_ or _x~2~_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'abs)
    '((kind . function)
      (syntax . "(abs x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *abs* returns the absolute value of its argument. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'syntax 'and)
    '((kind . syntax)
      (syntax . "(and form*)")
      (return-type . "<object>")
      (desc . "*and* is the sequential logical “and” (or “∧”). _forms_ are evaluated from left to right until either one of them evaluates to *nil* or else none are left. If one of them evaluates to *nil*, then *nil* is returned from the *and*; otherwise, the value of the last evaluated _form_ is returned. The form *and* is equivalent to the following:(and)                      ≡ 't
(and _form_)                 ≡ _form_
<span title=\"For the definition of if, see §14.4 below.\">(and _form~1~_ _form~2~_ … _form~n~_) ≡ (if _form~1~_ (and _form~2~_ … _form~n~_) 'nil)</span>
")))

(setf (documentation 'function 'append)
    '((kind . function)
      (syntax . "(append list*)")
      (return-type . "<list>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns the result of appending all of the _lists_, or *()* if given no lists. An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).This function does not modify its arguments. It is implementation defined whether and when the result shares structure with its _list_ arguments.An error shall be signaled if the list cannot be allocated (error-id. _cannot-create-list_).")))

(setf (documentation 'function 'apply)
    '((kind . function)
      (syntax . "(apply function obj* list)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 1)
      (desc . "Applies _function_ to the arguments, _obj&#42;_, followed by the elements of _list_, if any. It returns the value returned by _function_.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). Each _obj_ may be any ISLISP object. An error shall be signaled if _list_ is not a proper list (see §7) (error-id. _improper-argument-list_).")))

(setf (documentation 'setf 'aref)
    '((kind . setf)
      (syntax . "(setf (aref basic-array z*) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by *aref* or *garef* with _obj_. The returned value is _obj_. The constraints on the _basic-array_, the _general-array_, and the sequence of indices _z_ is the same as for *aref* and *garef*.")))

(setf (documentation 'function 'aref)
    '((kind . function)
      (syntax . "(aref basic-array z*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "*aref* returns the object stored in the component of the _basic-array_ specified by the sequence of integers _z_. This sequence must have exactly as many elements as there are dimensions in the _basic-array_, and each one must satisfy 0 ≤ _zi_ < _di_ , _di_ the _i_th dimension and 0 ≤ _i_ < _d_, _d_ the number of dimensions. Arrays are indexed 0 based, so the _i_th row is accessed via the index _i_ − 1.An error shall be signaled if _basic-array_ is not a general-array&#42;</span> (error-id. _domain-error_).")))

(setf (documentation 'function 'arithmetic-error-operands)
    '((kind . function)
      (syntax . "(arithmetic-error-operands arithmetic-error)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _operation_ and _operands_ supplied as data when creating the _arithmetic-error_. An error shall be signaled if _arithmetic-error_ is not a condition of class *<arithmetic-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'arithmetic-error-operation)
    '((kind . function)
      (syntax . "(arithmetic-error-operation arithmetic-error)")
      (return-type . "<function>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _operation_ and _operands_ supplied as data when creating the _arithmetic-error_. An error shall be signaled if _arithmetic-error_ is not a condition of class *<arithmetic-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'array-dimensions)
    '((kind . function)
      (syntax . "(array-dimensions basic-array)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a list of the dimensions of a given _basic-array_. An error shall be signaled if _basic-array_ is not a basic-array (error-id. _domain-error_). The consequences are undefined if the returned list is modified.")))

(setf (documentation 'function 'assoc)
    '((kind . function)
      (syntax . "(assoc obj association-list)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "If _assocation-list_ contains at least one cons whose car is _obj_ (as determined by *eql*), the first such cons is returned. Otherwise, *nil* is returned. An error shall be signaled if _association-list_ is not a list of conses (error-id. _domain-error_).")))

(setf (documentation 'syntax 'assure)
    '((kind . syntax)
      (syntax . "(assure class-name form)")
      (return-type . "<object>")
      (desc . "These forms evaluate _form_. If _form_ returns, the returned value is returned by the *the* or *assure* form. In addition, these forms specify that the value of _form_ is of the class specified by _class-name_ (which must be the name of an existing class).In a *the* special form, the consequences are undefined if the value of _form_ is not of the class or a subclass of the class designated by _class-name_. In an *assure* special form, an error shall be signaled if the value of _form_ is not of the class or a subclass of the class designated by _class-name_ (error-id. _domain-error_).")))

(setf (documentation 'function 'atan)
    '((kind . function)
      (syntax . "(atan x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the arc tangent of _x_.The result is a (real) number that lies between −π/2 and π/2 (both exclusive).The following definition for (one-argument) arc tangent determines the range and branch cuts:
                [cols=\"1,1,1,1,1\"]
|===

| arctan x | = | 
                        
                            
                                log (1 + ix) - log (1 - ix)
                            
                            
                                2i
                            
                         | log (1 + ix) - log (1 - ix) | 2i

| log (1 + ix) - log (1 - ix)

| 2i
|===

</table>
                    </td>
                </tr>
            </tbody>An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'atan2)
    '((kind . function)
      (syntax . "(atan2 x1 x2)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Given a point (_x~2~_, _x~1~_) in rectangular coordinates, this function returns the phase of its representation in polar coordinates. If _x~1~_ is zero and _x~2~_ is negative, the result is positive. If _x~1~_ and _x~2~_ are both zero, the result is implementation defined.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).The value of *atan2* is always between −π (exclusive) and π (inclusive) when minus zero is not supported; when minus zero is supported, the range includes −π.
                <caption>Figure 3. Quadrant information for atan2</caption>
                [cols=\"1,1,1,1\"]
|===
| y Condition | x Condition | Cartesian locus | Range of result


| y=0 | x>0 | Positive x-axis | 0

| &#42;y=+0 | x>0 | Positive x-axis | +0

| &#42;y=-0 | x>0 | Positive x-axis | -0

| y>0 | x>0 | Quadrant I | 0<result<π/2

| y>0 | x=0 | Positive y-axis | π/2

| y>0 | x<0 | Quadrant II | π/2<result<π

| y=0 | x<0 | Negative x-asis | π

| &#42;y=+0 | x<0 | Negative x-asis | π

| &#42;y=-0 | x<0 | Negative x-asis | -π

| y<0 | x<0 | Quadrand III | -π<result<-π/2

| y<0 | x=0 | Negative y-axis | -π/2

| y<0 | x>0 | Quadrant IV | -π/2<result<0

| y=0 | x=0 | Origin | implementation defined

| &#42;y=+0 | x=+0 | Origin | +0

| &#42;y=-0 | x=+0 | Origin | -0

| &#42;y=+0 | x=-0 | Origin | π

| &#42;y=-0 | x=-0 | Origin | -π
|===

The signs of _x~1~_ (indicated as _y_) and _x~2~_ (indicated as _x_) are used to derive quadrant information. Figure 19.1 details various special cases. The asterisk (&#42;) indicates that the entry in the figure applies to implementations that support minus zero.")))

(setf (documentation 'function 'atanh)
    '((kind . function)
      (syntax . "(atanh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the hyperbolic arc tangent of _x_. An error shall be signaled if _x_ is not a number with absolute value less than 1 (error-id. _domain-error_).")))

(setf (documentation 'function 'basic-array-p)
    '((kind . function)
      (syntax . "(basic-array-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "*basic-array-p* returns *t* if _obj_ is a basic-array (instance of class *<basic-array>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*basic-array&#42;-p* returns *t* if _obj_ is a basic-array&#42; (instance of class *<basic-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*general-array&#42;-p* returns *t* if _obj_ is a general-array&#42; (instance of class *<general-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'basic-array*-p)
    '((kind . function)
      (syntax . "(basic-array*-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "*basic-array-p* returns *t* if _obj_ is a basic-array (instance of class *<basic-array>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*basic-array&#42;-p* returns *t* if _obj_ is a basic-array&#42; (instance of class *<basic-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*general-array&#42;-p* returns *t* if _obj_ is a general-array&#42; (instance of class *<general-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'basic-vector-p)
    '((kind . function)
      (syntax . "(basic-vector-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "*basic-vector-p* returns *t* if _obj_ is a basic-vector (instance of class *<basic-vector>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*general-vector-p* returns *t* if _obj_ is a general-vector (instance of class *<general-vector>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'syntax 'block)
    '((kind . syntax)
      (syntax . "(block name form*)")
      (return-type . "<object>")
      (desc . "The *block* special form executes each _form_ sequentially from left to right. If the last form exits normally, whatever it returns is returned by the *block* form.The _name_ in a *block* form is not evaluated; it must be an identifier. The scope of _name_ is the body _form_&#42;—only a *return-from* textually contained in some form can exit the *block*. The extent of _name_ is dynamic.If a _return-from_ is executed, the *result-form* is evaluated. If this evaluation returns normally, the value it returns is immediately returned from the innermost lexically enclosing *block* form with the same name.*return-from* is used to return from a *block*. _name_ is not evaluated and must be an identifier. A *block* special form must lexically enclose the occurrence of *return-from*; the value produced by *result-form* is immediately returned from the *block*. The *return-from* form never returns and does not have a value.An error shall be signaled if an attempt is made to exit a *block* after it has been exited (error-id. _control-error_); It is a violation if _name_ is not an identifier. It is a violation if a *block* with a corresponding _name_ does not exist. See §14.7.2 for other errors.")))

(setf (documentation 'function 'call-next-method)
    '((kind . local-function)
      (syntax . "(call-next-method)")
      (return-type . "<object>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The *call-next-method* function can be used within the body of a method to call the next method.It returns the value returned by the method it calls.The type of method combination used determines which methods can invoke *call-next-method* and what is the next method to be called.In the case of simple method combination where the method combination type is *nil* the next method is the next most specific method.The *standard* method combination type allows *call-next-method* to be used within primary methods and *:around* methods. The *standard* method combination type defines the next method as specified in §15.3.3.2.*call-next-method* passes the current method's original arguments to the next method. Neither using *setq* nor rebinding variables with the same names as parameters of the method affects the values *call-next-method* passes to the method it calls. The *call-next-method* function returns the value returned by the method it calls. After *call-next-method* returns, further computation is possible. The *next-method-p* function can be used to test whether there is a next method.The functional binding of *call-next-method* is lexical within the body of the method-defining form; <span class=\"latin\">i.e.</span>, it is as if it were established by *labels*. The function object to which the binding refers has indefinite extent.An error shall be signaled if *call-next-method* is used in methods that do not support it. An error shall be signaled if *call-next-method* is executed and there is no next method.")))

(setf (documentation 'setf 'car)
    '((kind . setf)
      (syntax . "(setf (car cons) obj)")
      (return-type . "<object>")
      (desc . "Updates the left component of _cons_ with _obj_. The returned value is _obj_. An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_). _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'car)
    '((kind . function)
      (syntax . "(car cons)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *car* returns the left component of the _cons_. An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_).")))

(setf (documentation 'syntax 'case)
    '((kind . syntax)
      (syntax . "(case keyform ((key*) form*)* [(t form*)])")
      (return-type . "<object>")
      (desc . "The *case* and *case-using* special forms, called <span class=\"term\">case forms</span>, provide a mechanism to execute a matching clause from a series of clauses based on the value of a dispatching form _keyform_.The clause to be executed is identified by a set of keys. A _key_ can be any object. If the keylist of the last clause is *t* the associated clause is executed if no key matches the _keyform_._keyform_ is a form to be computed at the beginning of execution of the case form. If the result of evaluating _keyform_ is equivalent to a _key_, then the _forms_, if any, in the corresponding clause are evaluated sequentially and the value of the last one is returned as value of the whole case form. *case* determines match equivalence by using *eql*; *case-using* match determines equivalence by using the result of evaluating _predform_. _predform_ must be a boolean or quasi-boolean function that accepts two arguments, the value returned by _keyform_ and _key_. If no _form_ exists for a matching _key_, the case form evaluates to *nil*. If the value of _keyform_ is different from every _key_, and there is a default clause, its forms, if any, are evaluated sequentially, and the value of the last one is the result of the case form.The same _key_ (as determined by the match predicate) may occur only once in a case form.")))

(setf (documentation 'syntax 'case-using)
    '((kind . syntax)
      (syntax . "(case-using predform keyform ((key*) form*)* [(t form*)])")
      (return-type . "<object>")
      (desc . "The *case* and *case-using* special forms, called <span class=\"term\">case forms</span>, provide a mechanism to execute a matching clause from a series of clauses based on the value of a dispatching form _keyform_.The clause to be executed is identified by a set of keys. A _key_ can be any object. If the keylist of the last clause is *t* the associated clause is executed if no key matches the _keyform_._keyform_ is a form to be computed at the beginning of execution of the case form. If the result of evaluating _keyform_ is equivalent to a _key_, then the _forms_, if any, in the corresponding clause are evaluated sequentially and the value of the last one is returned as value of the whole case form. *case* determines match equivalence by using *eql*; *case-using* match determines equivalence by using the result of evaluating _predform_. _predform_ must be a boolean or quasi-boolean function that accepts two arguments, the value returned by _keyform_ and _key_. If no _form_ exists for a matching _key_, the case form evaluates to *nil*. If the value of _keyform_ is different from every _key_, and there is a default clause, its forms, if any, are evaluated sequentially, and the value of the last one is the result of the case form.The same _key_ (as determined by the match predicate) may occur only once in a case form.")))

(setf (documentation 'syntax 'catch)
    '((kind . syntax)
      (syntax . "(catch tag-form form*)")
      (return-type . "<object>")
      (desc . "The special forms *catch* and *throw* provide a facility for programming of structured non-local dynamic exits. A *catch* form and a *throw* form are said to correspond if the _tag-form_ of the *catch* and the _tag-form_ of the *throw* evaluate to the same object, a <span class=\"term\">catch tag</span>. A <span class=\"term\">catch tag</span> may be any object other than a number or a character; the comparison of _catch tags_ uses *eq*.The *catch* special form first evaluates the _tag-form_ to produce a _catch tag_, and then executes each _form_ sequentially from left to right. If execution of the _forms_ finishes normally, whatever is returned by the last _form_ is returned by the *catch* form.Prior to execution of the forms of a *catch* form _C~0~_, an association between the _catch tag T~0~_ and the executing form _C~0~_ is dynamically established, upon exit from _C~0~_, the association is disestablished. If there was an outer association for the same _catch tag T~0~_, it is hidden during the execution of _C~0~_'s _forms_; only the most recently established (<span class=\"latin\">i.e.</span>, innermost) association for _T~0~_ is ever visible.If a *throw* special form is executed, it evaluates the _tag-form_ producing a _catch tag T~1~_, and then evaluates the _result-form_ producing a result _R~1~_. If there is a corresponding association between _T~1~_ and some *catch* form _C~1~_ that is executing, _R~1~_ is immediately returned as the value of _C~1~_. The *throw* form can be anywhere in the entire current toplevel scope; it need not be lexically contained within _C~1~_.An error shall be signaled if there is no outstanding catcher for a _T~1~_ (error-id. _control-error_). See §14.7.2 for other errors.")))

(setf (documentation 'setf 'cdr)
    '((kind . setf)
      (syntax . "(setf (cdr cons) obj)")
      (return-type . "<object>")
      (desc . "Updates the right component of _cons_ with _obj_. The returned value is _obj_ . An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_). _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'cdr)
    '((kind . function)
      (syntax . "(cdr cons)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *cdr* returns the right component of the _cons_. An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_).")))

(setf (documentation 'function 'ceiling)
    '((kind . function)
      (syntax . "(ceiling x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the smallest integer that is not smaller than _x_. That is, _x_ is truncated towards positive infinity. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'cerror)
    '((kind . function)
      (syntax . "(cerror continue-string error-string obj*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "Like *error*, but the error that it signals is “continuable” (see continue-condition</span>). The extra argument _continue-string_ describes what happens if this function returns.This is equivalent to:(signal-condition
  (create (class *<simple-error>*)
          'format-string error-string
          'format-arguments (list obj&#42;))
  (let ((str (create-string-output-stream)))
    (format str continue-string obj&#42;)
    (get-output-stream-string str)))
")))

(setf (documentation 'function 'char-index)
    '((kind . function)
      (syntax . "(char-index char string [start-position])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the position of _char_ in _string_, The search starts from the position indicated by _start-position_ (which is 0-based and defaults to 0). The value returned if the search succeeds is an offset from the beginning of the string, not from the starting point. If the _char_ does not occur in the _string_, *nil* is returned. The function *char=* is used for the comparisons.An error shall be signaled if _char_ is not a character or if _string_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'char/=)
    '((kind . function)
      (syntax . "(char/= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'char<)
    '((kind . function)
      (syntax . "(char< char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'char<=)
    '((kind . function)
      (syntax . "(char<= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'char=)
    '((kind . function)
      (syntax . "(char= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'char>)
    '((kind . function)
      (syntax . "(char> char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'char>=)
    '((kind . function)
      (syntax . "(char>= char1 char2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *char=* tests whether _char~1~_ is the same character as _char~2~_. The function *char<* tests whether _char~1~_ is less than _char~2~_. The function *char<=* tests whether _char~1~_ is less than or equal to _char~2~_. The ordering used is the partial order defined above, extended to a total order on all characters in an implementation-defined manner. If the test is satisfied, *t* is returned; otherwise, *nil* is returned.Two characters are *char/=* if and only if they are not *char=*. Two characters are *char>* if and only if they are not *char<=*. Two characters are *char>=* if and only if they are not *char<*.An error shall be signaled if either _char~1~_ or _char~2~_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'characterp)
    '((kind . function)
      (syntax . "(characterp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a character (instance of class *<character>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'syntax 'class)
    '((kind . syntax)
      (syntax . "(class class-name)")
      (return-type . "<class>")
      (desc . "Returns the class object that corresponds to the class named _class-name_.")))

(setf (documentation 'function 'class-of)
    '((kind . function)
      (syntax . "(class-of obj)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the class of which the given _obj_ is a direct instance. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'close)
    '((kind . function)
      (syntax . "(close stream)")
      (return-type . nil)
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *close* closes the stream _stream_. If _stream_ is closed it may no longer be used in input or output operations. Closing a file stream ends the association between the stream and its file. If the stream was already closed this function performs nothing. The result value is implementation defined. An error shall be signaled if stream is not a stream (error-id. _domain-error_).")))

(setf (documentation 'syntax 'cond)
    '((kind . syntax)
      (syntax . "(cond (test form*)*)")
      (return-type . "<object>")
      (desc . "Executing the prepared *cond*, the clauses (_test_ _form_&#42;) are scanned sequentially and in each case the _test_ is evaluated; when a _test_ delivers a non-nil value the scanning process stops and all forms associated with the corresponding clause are sequentially evaluated and the value of the last one is returned. If no test is true, then *nil* is returned. If no _form_ exists for the successful _test_ then the value of this _test_ is returned.*cond* obeys the following equivalences:
                [cols=\"1,1,1\"]
|===

| 
                        (cond)
                     | 
                        ≡
                     | 
                        nil
                    

| 
                        (cond (test1) (test2 form2&#42;) ...)
                     | 
                        ≡
                     | 
                        (or test1 (cond (test2 form2&#42;) ...))
                    

| 
                        (cond (test1 form1+) (test2 form2&#42;) ...)
                     | 
                        ≡
                     | 
                        (if test1 (progn form1+) (cond (test2 form2&#42;) ...))
                    
|===

")))

(setf (documentation 'function 'condition-continuable)
    '((kind . function)
      (syntax . "(condition-continuable condition)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *nil* if _condition_ is not continuable, or a string describing the effect of continuing otherwise.")))

(setf (documentation 'function 'cons)
    '((kind . function)
      (syntax . "(cons obj1 obj2)")
      (return-type . "<cons>")
      (min-args . 2)
      (max-args . 2)
      (desc . "This function builds a cons from two objects, with _obj~1~_ as its car (or `left') part and with _obj~2~_ as its cdr (or `right') part. An error shall be signaled if the requested cons cannot be allocated (error-id. _cannot-create-cons_). Both _obj~1~_ and _obj~2~_ may be any ISLISP object.")))

(setf (documentation 'function 'consp)
    '((kind . function)
      (syntax . "(consp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a cons (instance of class *<cons>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'continue-condition)
    '((kind . function)
      (syntax . "(continue-condition condition [value]) transfers control and data")
      (return-type . nil)
      (min-args . 1)
      (max-args . 2)
      (desc . "<q>Continues</q> from _condition_ by finding the call to *signal-condition* and arranging for it to perform a normal return of the value, which defaults to *nil*. The consequences are undefined if the _condition_ is not continuable.")))

(setf (documentation 'syntax 'convert)
    '((kind . syntax)
      (syntax . "(convert obj class-name)")
      (return-type . "<object>")
      (desc . "Returns an object of class _class-name_ which corresponds to the object _obj_ according to one of the following projections, called a <span class=\"term\">coercion function</span>. The table shows _obj_ along the left-hand column and class-name along the top row (with <>'s in class names omitted here only for textual brevity):
                [cols=\"1,1,1,1,1,1,1,1\"]
|===
|  | character | integer | ﬂoat | symbol | string | general-vector | list

| character
| = | I | - | I(3) | -(4) | - | -
| integer
| I | = | X | - | X(5) | - | -
| ﬂoat
| - | -(1) | = | - | X(6) | - | -
| symbol
| - | - | - | = | I(3) | - | -
| string
| - | X(2) | X(2) | I(3) | = | X(7) | X
| general-vector
| - | - | - | - | - | = | X
| list
| - | - | - | - | - | X | =
|===


                <dt><b>=</b></dt>
                <dd>This is the identity function</dd>
                <dt><b>X</b></dt>
                <dd>
                    <dl>
                        <dd>This coercion shall be provided</dd>
                        <dt><b>X(2)</b></dt>
                        <dd>An error shall be signaled if this coercion is attempted and the string does not contain the textual representation of a number of the target class. In all other respects, this is the same as *parse-number*.</dd>
                        <dt><b>X(5)</b></dt>
                        <dd>This may be the same as the *&#126;D* format directive.</dd>
                        <dt><b>X(6)</b></dt>
                        <dd>This may be the same as the *&#126;G* format directive.</dd>
                        <dt><b>X(7)</b></dt>
                        <dd>This is the identity if strings are vectors in the implementation.</dd>
                    </dl>
                </dd>
                <dt><b>I</b></dt>
                <dd>This coercion shall be provided, but its definition is implementation defined.
                    <dl>
                        <dt><b>I(3)</b></dt>
                        <dd>This coercion shall be provided, but its definition is §18.1.2).</dd>
                    </dl>
                </dd>
                <dt><b>–</b></dt>
                <dd>An error shall be signaled if this coercion is attempted.
                    <dl>
                        <dt><b>–(1)</b></dt>
                        <dd>Programmers requiring this kind of functionality may wish to consider instead using one of the functions, *floor*, *ceiling*, *round*, or *truncate*.</dd>
                        <dt><b>–(4)</b></dt>
                        <dd>programmers requiring this kind of functionality may wish to consider instead using the following: (*create-string* 1 _obj_)</dd>
                    </dl>
                </dd>
            If an implementation provides implementation-defined coercions for the names of those classes using convert.")))

(setf (documentation 'function 'cos)
    '((kind . function)
      (syntax . "(cos x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sin* returns the sine of _x_ . The function *cos* returns the cosine of _x_. The function tan returns the tangent of _x_. In each case, _x_ must be given in radians.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'cosh)
    '((kind . function)
      (syntax . "(cosh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sinh* returns the hyperbolic sine of _x_. The function *cosh* returns the hyperbolic cosine of _x_. The function *tanh* returns the hyperbolic tangent of _x_.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'create)
    '((kind . generic-function)
      (syntax . "(create class {initarg initval }*)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function *create* creates and returns a new instance of a _class_. The argument is a class object.The initialization of a new instance consists of several distinct steps, including the following: allocating storage for the instance, filling slots with values, and executing user-supplied methods that perform additional initialization. The last two steps of *create* are implemented by the generic function *initialize-object* to provide a mechanism for customizing those steps. The initialization arguments (the _initargs_ and _initvals_) are given as a single list argument to *initialize-object*. The instance returned by *create* is the new instance, which has been modified and returned by *initialize-object*.")))

(setf (documentation 'function 'create-array)
    '((kind . function)
      (syntax . "(create-array dimensions [initial-element])")
      (return-type . "<basic-array>")
      (min-args . 1)
      (max-args . 2)
      (desc . "This function creates an array of the given _dimensions_. The _dimensions_ argument is a list of non-negative integers.The result is of class *<general-vector>* if there is only one dimension, or of class *<general-array&#42;>* otherwise.If _initial-element_ is given, the elements of the new array are initialized with this object, otherwise the initialization is implementation defined.An error shall be signaled if the requested array cannot be allocated (error-id. _cannot-create-array_).An error shall be signaled if _dimensions_ is not a proper list of non-negative integers (error-id. _domain-error_). _initial-element_ may be any ISLISP object.")))

(setf (documentation 'function 'create-list)
    '((kind . function)
      (syntax . "(create-list i [initial-element])")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 2)
      (desc . "Returns a list of length _i_. If _initial-element_ is given, the elements of the new list are initialized with this object; otherwise, the initialization is implementation defined. An error shall be signaled if the requested list cannot be allocated (error-id. _cannot-create-list_). An error shall be signaled if _i_ is not a non-negative integer (error-id. _domain-error_). _initial-element_ may be any ISLISP object.")))

(setf (documentation 'function 'create-string)
    '((kind . function)
      (syntax . "(create-string i [initial-character ])")
      (return-type . "<string>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns a string of length _i_. If _initial-character_ is given, then the characters of the new string are initialized with this character, otherwise the initialization is implementation defined. An error shall be signaled if the requested string cannot be allocated (error-id. _cannot-create-string_). An error shall be signaled if _i_ is not a non-negative integer or if _initial-character_ is not a character (error-id. _domain-error_).")))

(setf (documentation 'function 'create-string-input-stream)
    '((kind . function)
      (syntax . "(create-string-input-stream string)")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Creates and returns an input stream from the string. An error shall be signaled if string is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'create-string-output-stream)
    '((kind . function)
      (syntax . "(create-string-output-stream)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "This function creates and returns a string output stream. The output to a string stream can be retrieved by *get-output-stream-string*.")))

(setf (documentation 'function 'create-vector)
    '((kind . function)
      (syntax . "(create-vector i [initial-element])")
      (return-type . "<general-vector>")
      (min-args . 1)
      (max-args . 2)
      (desc . "Returns a general-vector of length _i_. If _initial-element_ is given, the elements of the new vector are initialized with this object, otherwise the initialization is implementation defined. An error shall be signaled if the requested vector cannot be allocated (error-id. _cannot-create-vector_). An error shall be signaled if _i_ is not a non-negative integer (error-id. _domain-error_). _initial-element_ may be any ISLISP object.")))

(setf (documentation 'syntax 'defclass)
    '((kind . syntax)
      (syntax . "(defclass class-name (sc-name*) (slot-spec*) class-opt*)")
      (return-type . "<symbol>")
      (desc . "Where:
_class-name_           ::= _identifier_

_sc-name_              ::= _identifier_

_slot-spec_            ::= _slot-name_ | (_slot-name_ _slot-opt_&#42;)

_slot-name_            ::= _identifier_

_slot-opt_             ::= *:reader* _reader-function-name_ |
                         *:writer* _writer-function-name_ |
                         *:accessor* _reader-function-name_ |
                         *:boundp* _boundp-function-name_ |
                         *:initform* _form_ |
                         *:initarg* _initarg-name_
                
                
_initarg-name_         ::= _identifier_

_reader-function-name_ ::= _identifier_

_writer-function-name_ ::= _identifier_

_boundp-function-name_ ::= _identifier_

_class-opt_            ::= (*:metaclass* _class-name_) |
                         (*:abstractp* _abstract-flag_)

_abstract-flag_        ::= *t* | *nil*

                The *defclass* defining form returns the symbol named class-name as its result.
            The _class-name_ argument is an identifier which becomes the name of the new class. The defining point of the _class-name_ is the end of the *defclass* defining form.Each superclass name argument _sc-name_ is an identifier that specifies a direct superclass of the new class. The new class will inherit slots and their :reader or *:writer* or *:accessor* methods from each of its superclasses. See §15.2.3 for a definition of how methods are inherited. No _sc-name_ may appear more than once in super class names. It is a violation if the superclasses of any two direct superclasses _sc-name_ have superclasses other than *<standard-object>* and *<object>* in common unless a metaclass other than *<standard-class>* is specified.Each _slot-spec_ argument is the name of the slot or a list consisting of the slot name followed by zero or more slot options. The _slot-name_ argument is an identifier that is syntactically valid for use as an ISLISP variable name. No slot names may appear more than once in _slot-spec_ The following slot options are available:
                <li id=\"reader\">The *:reader* slot option specifies that an unqualified method with the parameter profile ((*x* _class-name_)) is to be defined on the generic function named _reader-function-name_ to retrieve the value of the given slot. The *:reader* slot option may be specified more than once for a given slot.</li>
                <li id=\"writer\">The *:writer* slot option specifies that an unqualified method with the parameter profile ((*y* *<object>*) (*x* _class-name_)) is to be defined on the generic function named _writer-function-name_ to store the value into the slot. The _writer-function-name_ argument is an identifier. The *:writer* slot option may be specified more than once for a given slot.</li>
                <li id=\"accessor\">The *:accessor* slot option specifies that an unqualified method is to be defined on the generic function named _reader-function-name_ to retrieve the value of the given slot. Furthermore, there is a generic function such that (*setf* (_reader-function-name_ _x_) _y_) is equivalent to calling this generic function with first argument _y_ and second argument _x_. This generic function is extended by a method with the parameter profile ((_y_ *<object>*) (_x_ _class-name_)). The _reader-function-name_ argument is an identifier. The *:accessor* slot option may be specified more than once for a given slot.</li>
                <li id=\"boundp\">The *:boundp* slot option specifies that an unqualified method with the parameter profile ((_x_ _class-name_)) is to be defined on the generic function named _boundp-function-name_ to test whether the given slot has been given a value. The *:boundp* slot option may be specified more than once for a given slot.</li>
                <li id=\"initform\">The *:initform* slot option is used to provide a default initial value form to be used in the initialization of the slot. The *:initform* slot option may be specified once at most for a given slot. This form is evaluated every time it is used to initialize the slot. The lexical scope of the identifiers used in the initialization of the slot is the lexical scope of those identifiers in the defclass form. Note that the lexical scope refers both to variable and to function identifiers. In contrast, the current dynamic bindings used are those existing during activation of create. For more information, see §15.4.1.</li>
                <li id=\"initarg\">The *:initarg* slot option declares an initialization argument named _initarg-name_ and specifies that this initialization argument initializes the given slot. If the initialization argument and associated value are supplied in the call to *initialize-object*, the value will be stored into the given slot and the slot's *:initform* slot option, if any, is not evaluated. If none of the initialization arguments specified for a given slot has a value, the slot is initialized according to the *:initform* option, if specified. The consequences are undefined if more than one initialization argument for the same slot is supplied. For more information, see §15.4.1.</li>
            The generic functions, to which the methods created by the *:reader*, *:writer*, and *:accessor* slot options belong are called slot accessors.No implementation is permitted to extend the syntax of *defclass* to allow (*slot-name* _form_) as an abbreviation for (_slot-name_ *:initform* _form_).Each class option is an option that refers to the class as a whole. The following class options are available:
                <li id=\"metaclass\">The *:metaclass* class option is used to specify that instances of the class being defined are to have a different metaclass than the default provided by the system, that is, different from the class *<standard-class>*. The _class-name_ argument is the name of the desired metaclass. The *:metaclass* class option may be specified once at most. It is a violation if *<built-in-class>* is specified as the metaclass.</li>
                <li id=\"abstractp\">The *:abstractp* class option is used to specify that the class is an abstract class. It is a violation if the _abstract-flag_ is supplied but is neither *t* nor *nil*.</li>
            The following rules of defclass hold for standard classes:
                <li>The *defclass* defining form must be in the scope of any superclass identifier it refers to.</li>
                <li>All the superclasses of a class must be defined before an instance of the class can be made.</li>
                <li>Any reference to class-name as a parameter specializer in a defmethod form must be in the scope of class-name. That is, a defmethod form that names a class must textually follow the defclass form that defines that class.</li>
            An ISLISP processor may be extended to cover situations where these rules are not obeyed. These extensions shall be §15.1.3.If no slot accessors are specified for a slot, the slot cannot be accessed.When a class is defined, the order in which its direct superclasses are mentioned in the defining form is important. The new class has a local precedence order, which is a list consisting of the class followed by its direct superclasses in the order mentioned in its defclass defining form.")))

(setf (documentation 'syntax 'defconstant)
    '((kind . syntax)
      (syntax . "(defconstant name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define a named constant in the variable namespace of the current toplevel scope. The scope of _name_ is the entire current toplevel scope except the body form.Although _name_ is globally constant, a variable binding for _name_ can be locally established by a binding form.The result of the evaluation of _form_ is bound to the variable named by _name_. The binding and the object created as the result of evaluating the second argument are immutable. The symbol named _name_ is returned.")))

(setf (documentation 'syntax 'defdynamic)
    '((kind . syntax)
      (syntax . "(defdynamic name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define a dynamic variable namespace. The scope of _name_ is the entire current toplevel scope except the body form.The symbol named _name_ is returned.")))

(setf (documentation 'syntax 'defgeneric)
    '((kind . syntax)
      (syntax . "(defgeneric func-spec lambda-list {option | method-desc}*)")
      (return-type . "<symbol>")
      (desc . "Where:
_func-spec_                  ::= _identifier_ | (*setf* _identifier_)

_lambda-list_                ::= (_var_&#42; [*&#38;amp;rest* _var_]) |
                               (_var_&#42; [*:rest* _var_])

_option_                     ::= (*:method-combination* {_identifier_ | _keyword_}) |
                               (*:generic-function-class* _class-name_)

_method-desc_                ::= (*:method* _method-qualifier_&#42; _parameter-profile_ _form_&#42;)

_method-qualifier_           ::= _identifier_ | _keyword_

_parameter-profile_          ::= ({_var_ | (_var_ _parameter-specializer-name_)}&#42; [{*&#38;amp;rest* | *:rest*} _var_])

_parameter-specializer-name_ ::= _class-name_

_class-name_                 ::= _identifier_

_var_                        ::= _identifier_
The *defgeneric* defining form is used to define a generic function and to specify options and declarations that pertain to a generic function as a whole.It returns the generic function name _func-spec_.The scope of the generic function identifier _func-spec_ is the entire current toplevel scope.The _lambda-list_ argument is an ordinary function lambda-list.The following options are provided. A given option may occur only once.
                <li>The *:generic-function-class* option specifies that the generic function is to have a different class from the default provided by the system, that is, different from the class *<standard-generic-function>*. The class-name argument is the name of a class that can be the class of a generic function.</li>
                <li>The *:method-combination* option is followed by a symbol or keyword that names a type of method combination. The names of the built-in method combination types are *nil* and *standard*.</li>
            The _method-desc_ arguments define methods that will belong to the generic function, as if defined by *defmethod*. The _method-qualifier_ and _parameter-profile_ arguments in a method description are the same as for *defmethod*. The form arguments specify the method body.If no method descriptions are specified, a generic function with no methods is created. An error shall be signaled if a generic function is called and no methods apply.The _lambda-list_ argument of *defgeneric* specifies the shape of lambda-lists for the methods on this generic function. All methods on the resulting generic function must have parameter-profiles that are congruent with this shape. For further details on method congruence, see implementation-defined options.")))

(setf (documentation 'syntax 'defglobal)
    '((kind . syntax)
      (syntax . "(defglobal name form)")
      (return-type . "<symbol>")
      (desc . "This form is used to define an identifier in the variable namespace of the current toplevel scope. The scope of _name_ is the entire current toplevel scope except the body form.
            _form_ is evaluated to compute an initializing value for the variable named _name_. Therefore, *defglobal* is used only for defining variables and not for modifying them. The symbol named _name_ is returned.A lexical variable binding for _name_ can still be locally established by a binding form; in that case, the local binding lexically shadows the outer binding of name defined by *defglobal*.")))

(setf (documentation 'syntax 'defmacro)
    '((kind . syntax)
      (syntax . "(defmacro macro-name lambda-list form*)")
      (return-type . "<symbol>")
      (desc . "Defines a named (toplevel) macro. No implicit block with the macro name is established when the macro-expansion function is invoked. _macro-name_ must be an identifier whose scope is the current toplevel scope in which the defmacro form appears. _lambda-list_ is as defined in page 23. The definition point of _macro-name_ is the closing parenthesis of the _lambda-list_.")))

(setf (documentation 'syntax 'defmethod)
    '((kind . syntax)
      (syntax . "(defmethod func-spec method-qualifier* parameter-profile form*)")
      (return-type . "<symbol>")
      (desc . "Where:
_func-spec_                  ::= _identifier_ | (*setf* _identifier_)

_method-qualifier_           ::= _identifier_ | _keyword_

_parameter-profile_          ::= ({_var_ | (_var_ _parameter-specializer-name_)}&#42; [{*&#38;amp;rest* | _:rest_} _var_])

_parameter-specializer-name_ ::= _class-name_

_class-name_                 ::= _identifier_

_var_                        ::= _identifier_
The *defmethod* defining form defines a method on a generic function. It returns the generic function name _func-spec_.A method-defining form contains the code that is to be executed when the arguments to the generic function cause the method that it defines to be invoked.Preparing a method-defining form for execution causes one of the following cases:
                <li>It is a violation if the given name _func-spec_ already designates a generic function and this generic function contains a method that agrees with the new one on parameter specializers and qualifiers. For a definition of one method agreeing with another on parameter specializers and qualifiers, see the section §15.2.2.1.</li>
                <li>If the given name _func-spec_ designates a generic function and this generic function does not contain a method that agrees with the new one on parameter specializers and qualifiers, the new method is added to the generic function.</li>
                <li>It is a violation if the *defmethod* defining form is in the scope of a _func-spec_ identifier that does not designate a generic function.</li>
                <li>It is a violation if the given name _func-spec_ does not exist in the current toplevel scope immediately containing the *defmethod* defining form. Furthermore, it is a violation if a *defgeneric* form for _func-spec_ does not precede the method-defining form in the text unit being prepared for execution unless the method-defining form is a *defclass.*</li>
            The parameter-profile of the method being defined must be congruent with the lambda-list of the generic function. See §15.2.2.1 for a definition of agreement in this context.A method is not a function and cannot be invoked as a function.")))

(setf (documentation 'syntax 'defun)
    '((kind . syntax)
      (syntax . "(defun function-name lambda-list form*)")
      (return-type . "<symbol>")
      (desc . "The <i>defun-form</i> defines _function-name_ as an identifier in the function namespace; _function-name_ is bound to a function object equivalent to (*lambda* _lambda-list_ _form_&#42;).The scope of _function-name_ is the whole current toplevel scope. Therefore, the definition of a function admits recursion, occurrences of _function-name_ within the _form_&#42; refer to the function being defined. The binding between _function-name_ and the function object is immutable.*defun* returns the function name which is the symbol named _function-name_. The free identifiers in the body _form_&#42; (<span class=\"latin\">i.e.</span>, those which are not contained in the lambda list) follow the rules of lexical scoping.")))

(setf (documentation 'function 'div)
    '((kind . function)
      (syntax . "(div z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "*div* returns the greatest integer less than or equal to the quotient of _z~1~_ and _z~2~_. An error shall be signaled if _z~2~_ is zero (error-id. _division-by-zero_).*mod* returns the remainder of the integer division of _z~1~_ by _z~2~_. The sign of the result is the sign of _z~2~_. The result lies between 0 (inclusive) and _z~2~_ (exclusive), and the difference of _z~1~_ and this result is divisible by _z~2~_ without remainder.*div* and *mod* satisfy:(= z1 (+ (&#42; (div z~1~ z~2~) z~2~) (mod z~1~ z~2~)))
That is, the evaluation of the above form always return *t*.An error shall be signaled if either _z~1~_ or _z~2~_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'domain-error-expected-class)
    '((kind . function)
      (syntax . "(domain-error-expected-class domain-error)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _object_ and _expected-class_ supplied as data when creating the _domain-error_. An error shall be signaled if _domain-error_ is not a condition of class *<domain-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'domain-error-object)
    '((kind . function)
      (syntax . "(domain-error-object domain-error)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _object_ and _expected-class_ supplied as data when creating the _domain-error_. An error shall be signaled if _domain-error_ is not a condition of class *<domain-error>* (error-id. _domain-error_).")))

(setf (documentation 'setf 'dynamic)
    '((kind . setf)
      (syntax . "(setf (dynamic var) form)")
      (return-type . "<object>")
      (desc . "This special form denotes an assignment to a dynamic variable. This form can appear anywhere that (*dynamic* _var_) can appear._form_ is evaluated and the result of the evaluation is used to change the dynamic binding of _var_.An error shall be signaled if _var_ has no dynamic value (error-id. _unbound-variable_). *setf* of *dynamic* can be used only for modifying bindings, and not for establishing them.
            ")))

(setf (documentation 'syntax 'dynamic)
    '((kind . syntax)
      (syntax . "(dynamic var)")
      (return-type . "<object>")
      (desc . "This special form denotes a reference to the identifier denoting a dynamic variable. This special form is not allowed in the scope of a definition of _var_ which is not done by *defdynamic* or _dynamic-let_.During activation, the current dynamic binding of the variable _var_ is returned that was established most recently and is still in effect. An error shall be signaled if such a binding does not exist (error-id. _unbound-variable_).")))

(setf (documentation 'syntax 'dynamic-let)
    '((kind . syntax)
      (syntax . "(dynamic-let ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The *dynamic-let* special form is used to establish dynamic variable bindings. The first subform (the *dynamic-let* variable list) is a list of pairs (_var_ _form_). The scope of an identifier _var_ defined by *dynamic-let* is the current toplevel scope. The extent of the bindings of each _var_ is the extent of the body of the *dynamic-let*. The *dynamic-let* special form establishes dynamic variables for all _vars_.References to a dynamic variable named by _var_ must be made through the *dynamic* special form.All the initializing forms are evaluated sequentially from left to right, and then the values are associated with the corresponding _vars_. Using these additional dynamic bindings and the already existing bindings of visible identifiers, the forms _body-form_&#42; are evaluated in sequential order. The returned value of *dynamic-let* is that of the last _body-form_ of the body (or *nil* if there is none). The bindings are undone when control leaves the prepared *dynamic-let* special form.")))

(setf (documentation 'setf 'elt)
    '((kind . setf)
      (syntax . "(setf (elt sequence z) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by *elt* with _obj_. The returned value is _obj_.An error shall be signaled if _z_ is an integer outside of the valid range of indices (error-id. _index-out-of-range_). An error shall be signaled if _sequence_ is not a basic-vector or a list or if _z_ is not an integer (error-id. _domain-error_). obj may be any ISLISP object.")))

(setf (documentation 'function 'elt)
    '((kind . function)
      (syntax . "(elt sequence z)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Given a _sequence_ and an integer _z_ satisfying 0 ≤ _z_ < (*length sequence*), *elt* returns the element of _sequence_ that has index _z_. Indexing is 0-based; <span class=\"latin\">i.e.</span>, _z_ = 0 designates the first element. An error shall be signaled if _z_ is an integer outside of the mentioned range (error-id. _index-out-of-range_).An error shall be signaled if _sequence_ is not a basic-vector or a list or if _z_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'eq)
    '((kind . function)
      (syntax . "(eq obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*eq* and *eql* test whether _obj~1~_ and _obj~2~_ are same identical object. They return *t* if the objects are the same; otherwise, they return *nil*. Two objects are the same if there is no operation that could distinguish them (without modifying them), and if modifying one would modify the other the same way.For *eq*, the consequences are implementation defined if both _obj~1~_ and _obj~2~_ are numbers or both are characters. For *eql* the meaning for numbers and characters is defined as follows:
                <li>
                    If _obj~1~_ and _obj~2~_ are numbers, *eql* tests whether they are direct instances of the same class and have the same value.


                    If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns *nil*. When the syntax -0.0 is read and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns *t*.


                </li>
                <li>If _obj~1~_ and _obj~2~_ are characters, *eql* tests whether they are the same character (see char=</span>).</li>
            ")))

(setf (documentation 'function 'eql)
    '((kind . function)
      (syntax . "(eql obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "*eq* and *eql* test whether _obj~1~_ and _obj~2~_ are same identical object. They return *t* if the objects are the same; otherwise, they return *nil*. Two objects are the same if there is no operation that could distinguish them (without modifying them), and if modifying one would modify the other the same way.For *eq*, the consequences are implementation defined if both _obj~1~_ and _obj~2~_ are numbers or both are characters. For *eql* the meaning for numbers and characters is defined as follows:
                <li>
                    If _obj~1~_ and _obj~2~_ are numbers, *eql* tests whether they are direct instances of the same class and have the same value.


                    If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns *nil*. When the syntax -0.0 is read and it is interpreted as the value 0.0 then (eql 0.0 -0.0) returns *t*.


                </li>
                <li>If _obj~1~_ and _obj~2~_ are characters, *eql* tests whether they are the same character (see char=</span>).</li>
            ")))

(setf (documentation 'function 'equal)
    '((kind . function)
      (syntax . "(equal obj1 obj2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "This function tests whether _obj~1~_ and _obj~2~_ are isomorphic—<span class=\"latin\">i.e.</span>, whether _obj~1~_ and _obj~2~_ denote the same structure with equivalent values. *equal* returns *t* if the test was satisfied, and *nil* if not. Specifically:If _obj~1~_ and _obj~2~_ are direct instances of the same class, *equal* returns *t* if they are *eql*. Otherwise (if they are direct instances of the same class but not *eql*), the result is *t* if one of the following cases applies:
                <li>
                    lists: either _obj~1~_ and _obj~2~_ are both the empty list (<span class=\"latin\">i.e.</span>, *nil*), or


<pre>(and (equal (car obj1) (car obj2))
     (equal (cdr obj1) (cdr obj2))) holds;
</pre>
                </li>
                <li>
                    basic arrays:


<pre>(equal (array-dimensions obj~1~)
       (array-dimensions obj~2~))
</pre>
                    holds and for every valid reference (*aref* _obj~1~_ _ind~1~_ ..._ind~n~_)


<pre>(equal (aref obj~1~ ind~1~ … ind~n~)
       (aref obj~2~ ind~1~ … ind~n~)) is satisfied.
</pre>
                </li>
            Otherwise the value is *nil*._obj~1~_ and _obj~2~_ may be any ISLISP objects.")))

(setf (documentation 'function 'error)
    '((kind . function)
      (syntax . "(error error-string obj*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "An error shall be signaled._error-string_ and the _objs_ are advice to the implementation about how the error message might be textually described (using *format*), but whether or not that advice is used is implementation defined.This is equivalent to:(signal-condition
  (create (class *<simple-error>*)
          'format-string error-string
          'format-arguments (list obj&#42;))
          nil)
")))

(setf (documentation 'function 'error-output)
    '((kind . function)
      (syntax . "(error-output)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function *standard-input* returns the stream used as the default stream for input functions.The function *standard-output* returns the stream used as the default stream for output functions.The function *error-output* returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially with-error-output</span>.")))

(setf (documentation 'function 'exp)
    '((kind . function)
      (syntax . "(exp x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns <i>e</i> raised to the power _x_, where <i>e</i> is the base of the natural logarithm. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'expt)
    '((kind . function)
      (syntax . "(expt x1 x2)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns _x~1~_ raised to the power _x~2~_. The result will be an integer if _x~1~_ is an integer and _x~2~_ is a non-negative integer. An error shall be signaled if _x~1~_ is zero and _x~2~_ is negative, or if _x~1~_ is zero and _x~2~_ is a zero ﬂoat, or if _x~1~_ is negative and _x~2~_ is not an integer.")))

(setf (documentation 'function 'file-length)
    '((kind . function)
      (syntax . "(file-length filename element-class)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns the length of the file named by _filename_, or returns *nil* if the length cannot be determined. The _element-class_ determines the units. An error shall be signaled if _filename_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'file-position)
    '((kind . function)
      (syntax . "(file-position stream)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the file position associated with _stream_.A file position is a non-negative integer that represents a position in the _stream_. For binary streams, the file position represents the number of preceding bytes in the _stream_. It is increased by one each time one of the following is done:(read-byte stream)

(write-byte z stream)
For character streams, the file position is increased by an implementation-defined non-negative amount each time one of the following is done:(format stream...)

(format-char stream char)

(format-float stream float)

(format-fresh-line stream)

(format-integer stream integer radix)

(format-object stream obj escape-p)

(format-tab stream column)

(report-condition condition stream)

(read-char stream)

(read-line stream)

(read stream)
The amount may depend on the output and on the file position itself. It is implementation-defined which integer represents the first element of the file. An error shall be signaled if _stream_ is not a stream to or from a file (error-id. _domain-error_).")))

(setf (documentation 'function 'finish-output)
    '((kind . function)
      (syntax . "(finish-output stream)")
      (return-type . "<null>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Completes any pending output to the destination designated by _stream_. Waits until the pending output is complete and then returns *nil*. For instance, pending output might be stored in a buffer; in this case *finish-output* forces the buffer to be written to the stream's destination. An error shall be signaled if stream is not a stream that can handle output operations (error-id. _domain-error_).")))

(setf (documentation 'syntax 'flet)
    '((kind . syntax)
      (syntax . "(flet ((function-name lambda-list form*)*) body-form*)")
      (return-type . "<object>")
      (desc . "The *flet* and *labels* special forms allow the definition of new identifiers in the function namespace for function objects.In a *labels* special form the scope of an identifier _function-name_ is the whole *labels* special form (excluding nested scopes, if any); for the *flet* special form, the scope of an identifier is only the _body-form_&#42;. Within these scopes, each _function-name_ is bound to a function object whose behavior is equivalent to (*lambda* _lambda-list_ _form&#42;_), where free identifier references are resolved as follows:
                <li>For a *labels* form, such free references are resolved in the lexical environment that was active immediately outside the *labels* form augmented by the function bindings for the given _function-names_ (<span class=\"latin\">i.e.</span>, any reference to a function _function-name_ refers to a binding created by the *labels*).</li>
                <li>For a *flet* form, free identifier references in the *lambda*-expression are resolved in the lexical environment that was active immediately outside the *flet* form (<span class=\"latin\">i.e.</span>, any reference to a function _function-name_ are not visible).</li>
            During activation, the prepared *labels* or *flet* establishes function bindings and then evaluates each _body-form_ in the body sequentially; the value of the last one (or *nil* if there is none) is the value returned by the special form activation.No _function-name_ may appear more than once in the function bindings.")))

(setf (documentation 'function 'float)
    '((kind . function)
      (syntax . "(float x)")
      (return-type . "<float>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns _x_ itself if it is an instance of the class *<float>* and returns a ﬂoating-point approximation of _x_ otherwise. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'floatp)
    '((kind . function)
      (syntax . "(floatp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a ﬂoat (instance of class *<float>*); otherwise, returns *nil*. The _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'floor)
    '((kind . function)
      (syntax . "(floor x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the greatest integer less than or equal to _x_. That is, _x_ is truncated towards negative infinity. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'syntax 'for)
    '((kind . syntax)
      (syntax . "(for (iteration-spec*) (end-test result*) form*)")
      (return-type . "<object>")
      (desc . "Where:
_iteration-spec_ ::= (_var_ _init_ [_step_])
*for* repeatedly executes a sequence of forms _form_&#42;, called its _body_. It specifies a set of identifiers naming variables that will be local to the *for* form, their initialization, and their update for each iteration. When a termination condition is met, the iteration exits with a specified result value.The scope of an identifier _var_ is the _body_, the _steps_, the _end-test_, and the _result_&#42;. A _step_ might be omitted, in which case the effect is the same as if (_var_ _init_ _var_) had been written instead of (_var_ _init_). It is a violation if more than one _iteration-spec_ names the same _var_ in the same *for* form.The *for* special form is executed as follows: The _init_ forms are evaluated sequentially from left to right. Then each value is used as the initial value of the variable denoted by the corresponding identifier _var_, and the iteration phase begins.Each iteration begins by evaluating _end-test_. If the result is *nil*, the forms in the _body_ are evaluated sequentially (for side-effects). Afterwards, the _step-forms_ are evaluated sequentially order from left to right. Then their values are assigned to the corresponding variables and the next iteration begins. If _end-test_ returns a non-nil value, then the _result_&#42; are evaluated sequentially and the value of the last one is returned as value of the whole *for* macro. If no result is present, then the value of the for macro is *nil*.")))

(setf (documentation 'function 'format)
    '((kind . function)
      (syntax . "(format output-stream format-string obj*)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . nil)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-char)
    '((kind . function)
      (syntax . "(format-char output-stream char)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-float)
    '((kind . function)
      (syntax . "(format-float output-stream float)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-fresh-line)
    '((kind . function)
      (syntax . "(format-fresh-line output-stream)")
      (return-type . "<null>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-integer)
    '((kind . function)
      (syntax . "(format-integer output-stream integer radix)")
      (return-type . "<null>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-object)
    '((kind . function)
      (syntax . "(format-object output-stream obj escape-p)")
      (return-type . "<null>")
      (min-args . 3)
      (max-args . 3)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'format-tab)
    '((kind . function)
      (syntax . "(format-tab output-stream column)")
      (return-type . "<null>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *format* has the side-effect of printing according to _format-string_. It returns *nil*. An error shall be signaled if the _output-stream_ parameter does not satisfy the *output-stream-p* predicate (error-id. _not-an-output-stream_). An error shall be signaled if _format-string_ is not a string (error-id. _domain-error_). The following is a summary of all the available format directives:_obj_ refers to the next item of the set of _obj_&#42; to be processed.
                <dt>&#126;A</dt>
                <dd>
                    Aesthetic: The _obj_ is any object. _obj_ is printed as it would with &#126;S, but without escape characters. Characters are output directly without any conversion. That is, the output generated using this format directive is suitable for being read by a human reader.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *nil*).


                </dd>
                <dt>&#126;B</dt>
                <dd>
                    Binary: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in binary radix (radix 2).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 2).


                </dd>
                <dt>&#126;C</dt>
                <dd>
                    Character: An error shall be signaled if _obj_ is not a character. _obj_ is output directly without any conversion.


                    This effect is implemented by (*format-char* _output-stream_ _obj_).


                </dd>
                <dt>&#126;D</dt>
                <dd>
                    Decimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in decimal radix (radix 10).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 10).


                </dd>
                <dt>&#126;G</dt>
                <dd>
                    General ﬂoating point: An error shall be signaled if _obj_ is not a number. _obj_ is printed as a ﬂoat.


                    This effect is implemented by (*format-float* _output-stream_ _obj_).


                </dd>
                <dt>&#126;O</dt>
                <dd>
                    Octal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in octal radix (radix 8).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 8).


                </dd>
                <dt>&#126;_n_R</dt>
                <dd>
                    Radix: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in radix _n_ (which must be between 2 and 36, inclusive).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ _n_).


                </dd>
                <dt>&#126;S</dt>
                <dd>
                    S-expression: _obj_ is any object. This format directive outputs the textual representation of _obj_, with escape characters as needed. That is, the output generated using this format directive is suitable for input to the function *read*.


                    This effect is implemented by (*format-object* _output-stream_ _obj_ *t*).


                </dd>
                <dt>&#126;_n_T</dt>
                <dd>
                    Tab: output enough spaces to move to column _n_ (where column 0 represents the left margin). If already at or beyond column _n_, one space is output. If an implementation cannot determine the current column position, the behavior is implementation defined, but at least one space will be output.


                    This effect is implemented by (*format-tab* _output-stream_ _n_).


                </dd>
                <dt>&#126;X</dt>
                <dd>
                    Hexadecimal: An error shall be signaled if _obj_ is not an integer. _obj_ is printed in hexadecimal radix (radix 16).


                    This effect is implemented by (*format-integer* _output-stream_ _obj_ 16).


                </dd>
                <dt>&#126;%</dt>
                <dd>
                    newline: output a #\\newline character;


                    This effect is implemented by (*format-char* _output-stream_ #\\newline).


                </dd>
                <dt>&#126;&#38;amp;</dt>
                <dd>
                    conditional newline: output a #\\newline character if it cannot be determined that the output stream is at the beginning of a fresh line;


                    This effect is implemented by (*format-fresh-line* _output-stream_).


                </dd>
                <dt>&#126;&#126;</dt>
                <dd>
                    tilde: output a tilde (˜).


                    This effect is implemented by (*format-char* _output-stream_ #\\&#126;).


                </dd>
            ")))

(setf (documentation 'function 'funcall)
    '((kind . function)
      (syntax . "(funcall function obj*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "Activates the specified function _function_ and returns the value that the _function_ returns. The <i>i</i>th argument (2 ≤ <i>i</i>) of *funcall* becomes the (<i>i</i> − 1)th argument of the *function*. *funcall* could have been defined using *apply* as follows:(defun funcall (function :rest arguments)
  (apply function arguments))

                An error shall be signaled if _function_ is not a function (error-id. _domain-error_). Each _obj_ may be any ISLISP object.
            ")))

(setf (documentation 'syntax 'function)
    '((kind . syntax)
      (syntax . "(function function-name)")
      (return-type . "<function>")
      (desc . "
                #'<span class=\"arg\">function-name</span> → *<function>*
            This special form denotes a reference to the function named by _function-name_. This special form is used to refer to identifiers defined by function-defining forms, *labels*, or *flet* which are not in operator position.(*function* _function-name_) can be written as #'_function-name_.It returns the function object named by _function-name_.An error shall be signaled if no binding has been established for the identifier in the function namespace of current lexical environment (see §9.2) (error-id. _undefined-function_). The consequences are undefined if the _function-name_ names a macro or special form.")))

(setf (documentation 'function 'functionp)
    '((kind . function)
      (syntax . "(functionp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a (normal or generic) function; otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'setf 'garef)
    '((kind . setf)
      (syntax . "(setf (garef general-array z*) obj)")
      (return-type . "<object>")
      (desc . "These replace the object obtainable by *aref* or *garef* with _obj_. The returned value is _obj_. The constraints on the _basic-array_, the _general-array_, and the sequence of indices _z_ is the same as for *aref* and *garef*.")))

(setf (documentation 'function 'garef)
    '((kind . function)
      (syntax . "(garef general-array z*)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . nil)
      (desc . "*aref* returns the object stored in the component of the _basic-array_ specified by the sequence of integers _z_. This sequence must have exactly as many elements as there are dimensions in the _basic-array_, and each one must satisfy 0 ≤ _zi_ < _di_ , _di_ the _i_th dimension and 0 ≤ _i_ < _d_, _d_ the number of dimensions. Arrays are indexed 0 based, so the _i_th row is accessed via the index _i_ − 1.An error shall be signaled if _basic-array_ is not a general-array&#42;</span> (error-id. _domain-error_).")))

(setf (documentation 'function 'gcd)
    '((kind . function)
      (syntax . "(gcd z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "*gcd* returns the greatest common divisor of its integer arguments. The result is a non-negative integer. For nonzero arguments the greatest common divisor is the largest integer _z_ such that _z~1~_ and _z~2~_ are integral multiples of _z_.An error shall be signaled if either _z~1~_ or _z~2~_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'general-array*-p)
    '((kind . function)
      (syntax . "(general-array*-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "*basic-array-p* returns *t* if _obj_ is a basic-array (instance of class *<basic-array>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*basic-array&#42;-p* returns *t* if _obj_ is a basic-array&#42; (instance of class *<basic-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*general-array&#42;-p* returns *t* if _obj_ is a general-array&#42; (instance of class *<general-array&#42;>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'general-vector-p)
    '((kind . function)
      (syntax . "(general-vector-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "*basic-vector-p* returns *t* if _obj_ is a basic-vector (instance of class *<basic-vector>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.*general-vector-p* returns *t* if _obj_ is a general-vector (instance of class *<general-vector>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'generic-function-p)
    '((kind . function)
      (syntax . "(generic-function-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a generic function; otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'gensym)
    '((kind . function)
      (syntax . "(gensym)")
      (return-type . "<symbol>")
      (min-args . 0)
      (max-args . 0)
      (desc . "Returns an unnamed symbol. *gensym* is useful for writing macros. It is impossible for an identifier to name an unnamed symbol.")))

(setf (documentation 'function 'get-internal-real-time)
    '((kind . function)
      (syntax . "(get-internal-real-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "*get-internal-real-time* returns as an integer the current time in internal time units, relative to an arbitrary time base. The difference between the values of two calls to this function is the amount of elapsed real time (<span class=\"latin\">i.e.</span>, clock time) between the two calls.*get-internal-run-time* returns as an integer the current run time in internal time units. The precise meaning of this quantity is implementation defined. The difference between the values of two calls to this function is the amount of time between the two calls during which computational effort was expended on behalf of the executing program.")))

(setf (documentation 'function 'get-internal-run-time)
    '((kind . function)
      (syntax . "(get-internal-run-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "*get-internal-real-time* returns as an integer the current time in internal time units, relative to an arbitrary time base. The difference between the values of two calls to this function is the amount of elapsed real time (<span class=\"latin\">i.e.</span>, clock time) between the two calls.*get-internal-run-time* returns as an integer the current run time in internal time units. The precise meaning of this quantity is implementation defined. The difference between the values of two calls to this function is the amount of time between the two calls during which computational effort was expended on behalf of the executing program.")))

(setf (documentation 'function 'get-output-stream-string)
    '((kind . function)
      (syntax . "(get-output-stream-string stream)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a string containing all characters written to _stream_ since the last call to this function or since the creation of the _stream_, if this function has not been called with _stream_ before. An error shall be signaled if stream is not a stream created with *create-string-output-stream* (error-id. _domain-error_).")))

(setf (documentation 'function 'get-universal-time)
    '((kind . function)
      (syntax . "(get-universal-time)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "Returns an approximation to the <q>current time</q> in Universal Time Format. The units are seconds. Universal Time Format represents time as an integer number of seconds since the beginning (<span class=\"latin\">i.e.</span>, midnight), January 1, 1900 UT (ignoring leap seconds). If *get-universal-time* is called twice, the first value shall be less than or equal to the second value.No implementation is required to have a way to verify that the time returned is correct. However, an error shall be signaled if an implementation can determine that the time it would return would not be correct (<span class=\"latin\">e.g.</span>, it can determine that the clock was never initialized).")))

(setf (documentation 'syntax 'go)
    '((kind . syntax)
      (syntax . "(go tagbody-tag) transfers control")
      (return-type . nil)
      (desc . "*tagbody* executes the _forms_ sequentially from left to right, discarding their values. If the execution of the last _form_ completes normally, *nil* is returned by the *tagbody* special form.The series of _tagbody-tags_ and _forms_ is collectively referred to as the body of a *tagbody* form. An identifier _tagbody-tag_ that appears at toplevel of the body denotes a <span class=\"term\">tagbody tag</span> that can be used with *go* to transfer control to that point in the body. Any compound form that appears is taken as a _form_. Literals are not permitted at the toplevel of a *tagbody*. No _tagbody-tag_ may appear more than once in the tags in the body.The namespace used for tagbody tags is distinct from that used for *block* tags.At any point lexically contained in the *tagbody* a form (*go* _tag_) can be used to transfer control to a tag _tag_ that appears among the _tagbody-tags_, except where a _tag_ is shadowed according to the lexical principle (see §14.7.2 for other errors.Note: As a stylistic matter, programmers are not encouraged to use *tagbody* and *go* in everyday programming. The primary uses for which these forms are intended are for implementing other control abstractions (using macros), and for the occassional real-world situation that uses unstructured imperative transfer of control (such as a finite state machine).")))

(setf (documentation 'function 'identity)
    '((kind . function)
      (syntax . "(identity obj)")
      (return-type . "<object>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns an object that is the same as obj under eql. obj may be any ISLISP object.")))

(setf (documentation 'syntax 'if)
    '((kind . syntax)
      (syntax . "(if test-form then-form [else-form])")
      (return-type . "<object>")
      (desc . "The _test-form_ is evaluated. If its result is anything non-nil, the _then-form_ is evaluated and its value is returned; otherwise (if the _test-form_ returned *nil*), the _else-form_ is evaluated and its value is returned.If no _else-form_ is provided, it defaults to *nil*.")))

(setf (documentation 'syntax 'ignore-errors)
    '((kind . syntax)
      (syntax . "(ignore-errors form*)")
      (return-type . "<object>")
      (desc . "Establishes a handler for *<error>*, such that if an error occurs during execution of _forms_, *ignore-errors* will immediately return *nil*. Then it executes forms, returning the value returned by the last form (or *nil* if there were no _forms_) if execution terminates normally.")))

(setf (documentation 'function 'initialize-object)
    '((kind . generic-function)
      (syntax . "(initialize-object instance initialization-arguments)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "The generic function *initialize-object* is called by *create* to initialize a newly created instance. The generic function *initialize-object* is called with the new instance and a list of initialization arguments.The system-supplied primary method on *initialize-object* initializes the slots of the instance with values according to the <i>initialization-arguments</i> (an alternating list of initialization argument keywords and values) and the *:initform* forms of the slots.The _instance_ argument is the object to be initialized. The modified instance is returned as the result. Programmers can define methods for *initialize-object* to specify actions to be taken when an instance is initialized. If only *:after* methods are defined, they will be run after the system-supplied primary method for initialization and therefore will not interfere with the default behavior of *initialize-object*. The consequences are undefined if a programmer-defined primary method for this generic function does not return instance.")))

(setf (documentation 'function 'input-stream-p)
    '((kind . function)
      (syntax . "(input-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a stream that can handle input operations; otherwise, returns *nil*.")))

(setf (documentation 'function 'instancep)
    '((kind . function)
      (syntax . "(instancep obj class)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns *t* if _obj_ is an instance (directly or otherwise) of the class _class_; otherwise, returns *nil*. _obj_ may be any ISLISP object. An error shall be signaled if class is not a class object (error-id. _domain-error_).")))

(setf (documentation 'function 'integerp)
    '((kind . function)
      (syntax . "(integerp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is an integer (instance of class *<integer>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'internal-time-units-per-second)
    '((kind . function)
      (syntax . "(internal-time-units-per-second)")
      (return-type . "<integer>")
      (min-args . 0)
      (max-args . 0)
      (desc . "*internal-time-units-per-second* returns the number of time units per second for the implementation.")))

(setf (documentation 'function 'isqrt)
    '((kind . function)
      (syntax . "(isqrt z)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the greatest integer less than or equal to the exact positive square root of _z_. An error shall be signaled if _z_ is not a non-negative integer (error-id. _domain-error_).")))

(setf (documentation 'syntax 'labels)
    '((kind . syntax)
      (syntax . "(labels ((function-name lambda-list form*)*) body-form*)")
      (return-type . "<object>")
      (desc . "The *flet* and *labels* special forms allow the definition of new identifiers in the function namespace for function objects.In a *labels* special form the scope of an identifier _function-name_ is the whole *labels* special form (excluding nested scopes, if any); for the *flet* special form, the scope of an identifier is only the _body-form_&#42;. Within these scopes, each _function-name_ is bound to a function object whose behavior is equivalent to (*lambda* _lambda-list_ _form&#42;_), where free identifier references are resolved as follows:
                <li>For a *labels* form, such free references are resolved in the lexical environment that was active immediately outside the *labels* form augmented by the function bindings for the given _function-names_ (<span class=\"latin\">i.e.</span>, any reference to a function _function-name_ refers to a binding created by the *labels*).</li>
                <li>For a *flet* form, free identifier references in the *lambda*-expression are resolved in the lexical environment that was active immediately outside the *flet* form (<span class=\"latin\">i.e.</span>, any reference to a function _function-name_ are not visible).</li>
            During activation, the prepared *labels* or *flet* establishes function bindings and then evaluates each _body-form_ in the body sequentially; the value of the last one (or *nil* if there is none) is the value returned by the special form activation.No _function-name_ may appear more than once in the function bindings.")))

(setf (documentation 'syntax 'lambda)
    '((kind . syntax)
      (syntax . "(lambda lambda-list form*)")
      (return-type . "<function>")
      (desc . "Where:
_lambda-list_ ::= (_identifier&#42;_ [*&#38;amp;rest* _identifier_]) |
                (_identifier&#42;_ [*:rest* _identifier_])
and where no _identifier_ may appear more than once in _lambda-list_.Execution of the *lambda* special form creates a function object.The scope of the identifiers of the _lambda-list_ is the sequence of forms _form&#42;_, collectively referred to as the _body_.When the prepared function is activated later (even if transported as object to some other activation) with some arguments, the body of the function is evaluated as if it was at the same textual position where the *lambda* special form is located, but in a context where the lambda variables are bound in the variable namespace with the values of the corresponding arguments. A *&#38;amp;rest* or *:rest* variable, if any, is bound to the list of the values of the remaining arguments. An error shall be signaled if the number of arguments received is incompatible with the specified _lambda-list_ (error-id. _arity-error_).Once the lambda variables have been bound, the body is executed. If the body is empty, *nil* is returned otherwise the result of the evaluation of the last form of body is returned if the body was not left by a <i>non-local exit</i> (see implementation defined whether _L~1~_ shares structure with _L~2~_.
            ")))

(setf (documentation 'function 'lcm)
    '((kind . function)
      (syntax . "(lcm z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "*lcm* returns the least common multiple of its integer arguments. *gcd* and *lcm* satisfies:(= (&#42; (gcd m n) (lcm m n)) (abs (&#42; m n)))
That is, the evaluation of the above form always return *t*.An error shall be signaled if either _z~1~_ or _z~2~_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'length)
    '((kind . function)
      (syntax . "(length sequence)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the length of _sequence_ as an integer greater than or equal to 0.When _sequence_ is a basic-vector, *length* returns its dimension.When _sequence_ is a list, the result is the number of elements in the list; if an element is itself a list, the elements within this sublist are not counted. In the case of dotted lists, *length* returns the number of conses at the uppermost level of the list. For example, (*length* '(a b . c)) ⇒ 2, since '(a b . c) ≡ (*cons* 'a (cons 'b 'c)).An error shall be signaled if _sequence_ is not a basic-vector or a list (error-id. _domain-error_).")))

(setf (documentation 'syntax 'let)
    '((kind . syntax)
      (syntax . "(let ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The *let* special form is used to define a scope for a group of identifiers for a sequence of forms _body-form&#42;_ (collectively referred to as the _body_). The list of pairs (_var_ _form_)&#42; is called the *let* variable list. The scope of the identifier _var_ is the _body_.The _forms_ are evaluated sequentially from left to right; then each variable denoted by the identifier _var_ is initialized to the corresponding value. Using these bindings along with the already existing bindings of visible identifiers the _body-forms_ are evaluated. The returned value of *let* is the result of the evaluation of the last _body-form_ of its body (or *nil* if there is none).No _var_ may appear more than once in *let* variable list.Note: Although this form is a special form, one can think of it as a macro whose rewriting rules are as follows:
                [cols=\"1,1,1\"]
|===

| (let () body-form&#42;) | ≡ | (progn body-form&#42;)

| 
                        
                            (let ((var1 form1)
                            (var2 form2)
                            …
                            (varn formn))
                            body-form&#42;)
                        
                     | ≡ | 
                        ((lambda (var1 var2 … varn) body-form&#42;) form1 form2 … formn)
                    
|===

")))

(setf (documentation 'syntax 'let*)
    '((kind . syntax)
      (syntax . "(let* ((var form)*) body-form*)")
      (return-type . "<object>")
      (desc . "The *let&#42;* form is used to define a scope for a group of identifiers for a sequence of forms _body-form_&#42; (collectively referred to as the _body_). The first subform (the *let&#42;* variable list) is a list of pairs (_var_ _form_). The scope of an identifier _var_ is the _body_ along with all _form_ forms following the pair (_var_ _form_) in the *let&#42;* variable list.For each pair (_var_ _form_) the following is done: _form_ is evaluated in the context of the bindings in effect at that point in the evaluation. The result of the evaluation is bound to its associated variable named by the identifier _var_. These variable bindings enlarge the set of current valid identifiers perhaps shadowing previous variable bindings (in case some _var_ was defined outside), and in this enlarged or modified environment the _body-forms_ are executed. The returned value of *let&#42;* is the result of the evaluation of the last form of its body (or *nil* if there is none).Note: Although this form is a special form, one can think of it as a macro whose rewriting rules are as follows:
                [cols=\"1,1,1\"]
|===

| (let&#42; () body-form&#42;) | ≡ | (progn body-form&#42;)

| 
                        (let&#42; ((var1 form1) (var2 form2) … (varn formn)) body-form&#42;)
                     | ≡ | 
                        (let ((var1 form1)) (let ((var2 form2)) … (let ((varn formn)) body-form&#42;)...))
                    
|===

")))

(setf (documentation 'function 'list)
    '((kind . function)
      (syntax . "(list obj*)")
      (return-type . "<list>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns a new list whose length is the number of arguments and whose elements are the arguments in the same order as in the *list*-form. An error shall be signaled if the requested list cannot be allocated (error-id. _cannot-create-list_). Each _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'listp)
    '((kind . function)
      (syntax . "(listp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a list (instance of class *<list>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'log)
    '((kind . function)
      (syntax . "(log x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the natural logarithm of _x_. An error shall be signaled if _x_ is not a positive number (error-id. _domain-error_).")))

(setf (documentation 'function 'map-into)
    '((kind . function)
      (syntax . "(map-into destination function sequence*)")
      (return-type . "<sequence>")
      (min-args . 2)
      (max-args . nil)
      (desc . "Destructively modifies destination to contain the results of applying _function_ to successive elements in the _sequences_. The _destination_ is returned.If _destination_ and each element of _sequences_ are not all the same length, the iteration terminates when the shortest _sequence_ (of any of the _sequences_ or the _destination_) is exhausted.The calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.An error shall be signaled if _destination_ is not a basic-vector or a list (error-id. _domain-error_).An error shall be signaled if any _sequence_ is not a basic-vector or a list (error-id. _domain-error_).")))

(setf (documentation 'function 'mapc)
    '((kind . function)
      (syntax . "(mapc function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcan)
    '((kind . function)
      (syntax . "(mapcan function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcar)
    '((kind . function)
      (syntax . "(mapcar function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapcon)
    '((kind . function)
      (syntax . "(mapcon function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'mapl)
    '((kind . function)
      (syntax . "(mapl function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'maplist)
    '((kind . function)
      (syntax . "(maplist function list+)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Successively applies the given _function_ to sets of arguments determined by the given _lists_. The way in which the arguments are determined, and the way in which the result is accumulated are how these functions differ.
                [cols=\"1,1,1\"]
|===
| Function | Argument | Result


| mapcar | successive elements | successive cons

| mapc | successive elements | none (i.e., list1 returned)

| mapcan | successive elements | successive destructive append

| maplist | successive sublists | successive cons

| mapl | successive sublists | none (i.e., list1 returned)

| mapcon | successive sublists | successive destructive append
|===

*mapcar* operates on successive elements of the _lists_. _function_ is applied to the first element of each _list_, then to the second element of each _list_, and so on. The iteration terminates when the shortest _list_ runs out, and excess elements in other _lists_ are ignored. The value returned by *mapcar* is a list of the results of successive calls to _function_.*mapc* is like *mapcar* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*maplist* is like *mapcar* except that _function_ is applied to successive sublists of the _lists_. _function_ is first applied to the _lists_ themselves, and then to the cdr of each _list_, and then to the cdr of the cdr of each _list_, and so on.*mapl* is like *maplist* except that the results of applying _function_ are not accumulated; _list~1~_ is returned.*mapcan* and *mapcon* are like *mapcar* and *maplist* respectively, except that the results of applying _function_ are combined into a list by the use of an operation that performs a destructive form of append rather than list.An error shall be signaled if _function_ is not a function (error-id. _domain-error_). An error shall be signaled if any _list_ is not a list (error-id. _domain-error_).In all cases, the calls to _function_ proceed from left to right, so that if _function_ has side-effects, it can rely upon being called first on all of the elements with index 0, then on all of those numbered 1, and so on.")))

(setf (documentation 'function 'max)
    '((kind . function)
      (syntax . "(max x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "The function *min* returns the least (closest to negative infinity) of its arguments. The comparison is done by *<*.The function *max* returns the greatest (closest to positive infinity) of its arguments. The comparison is done by *>*.An error shall be signaled if any _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'member)
    '((kind . function)
      (syntax . "(member obj list)")
      (return-type . "<list>")
      (min-args . 2)
      (max-args . 2)
      (desc . "If _list_ contains at least one occurrence of _obj_ (as determined by *eql*), the first sublist of _list_ whose car is _obj_ is returned. Otherwise, *nil* is returned. An error shall be signaled if _list_ is not a list (error-id. _domain-error_).")))

(setf (documentation 'function 'min)
    '((kind . function)
      (syntax . "(min x+)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . nil)
      (desc . "The function *min* returns the least (closest to negative infinity) of its arguments. The comparison is done by *<*.The function *max* returns the greatest (closest to positive infinity) of its arguments. The comparison is done by *>*.An error shall be signaled if any _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'mod)
    '((kind . function)
      (syntax . "(mod z1 z2)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "*div* returns the greatest integer less than or equal to the quotient of _z~1~_ and _z~2~_. An error shall be signaled if _z~2~_ is zero (error-id. _division-by-zero_).*mod* returns the remainder of the integer division of _z~1~_ by _z~2~_. The sign of the result is the sign of _z~2~_. The result lies between 0 (inclusive) and _z~2~_ (exclusive), and the difference of _z~1~_ and this result is divisible by _z~2~_ without remainder.*div* and *mod* satisfy:(= z1 (+ (&#42; (div z~1~ z~2~) z~2~) (mod z~1~ z~2~)))
That is, the evaluation of the above form always return *t*.An error shall be signaled if either _z~1~_ or _z~2~_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'next-method-p)
    '((kind . local-function)
      (syntax . "(next-method-p)")
      (return-type . "boolean")
      (min-args . 0)
      (max-args . 0)
      (desc . "The *next-method-p* function can be used within the body of a method defined by a method-defining form to determine whether a next method exists. The *next-method-p* function takes no arguments and returns *t* or *nil*.The functional binding of *next-method-p* is lexical within the body of the method-defining form; <span class=\"latin\">i.e.</span>, it is as if it were established by *labels*. The function object to which the binding refers has indefinite extent.")))

(setf (documentation 'value 'nil)
    '((kind . constant)
      (value-type . "<null>")
      (desc . "*t* is a named constant whose value is the symbol *t* itself. *nil* is a named constant whose value is the symbol *nil* itself.")))

(setf (documentation 'function 'not)
    '((kind . function)
      (syntax . "(not obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "This predicate is the logical “not” (or “¬”). It returns *t* if _obj_ is *nil* and *nil* otherwise. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'nreverse)
    '((kind . function)
      (syntax . "(nreverse list)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions each return a list whose elements are those of the given _list_, but in reverse order. An error shall be signaled if _list_ is not a list (error-id. _domain-error_).For *reverse*, no side-effect to the given _list_ occurs. The resulting list is permitted but not required to share structure with the input _list_.For *nreverse*, the conses which make up the top level of the given _list_ are permitted, but not required, to be side-effected in order to produce this new list. *nreverse* should never be called on a literal object.")))

(setf (documentation 'function 'null)
    '((kind . function)
      (syntax . "(null obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "<span title=\"If the naming conventions were strictly observed, null would be named nullp; it is named null for historical and compatibility reasons.\">Returns t if _obj_ is *nil*; otherwise, returns *nil*</span>. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'numberp)
    '((kind . function)
      (syntax . "(numberp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a number (instance of class *<number>*); otherwise, returns *nil*. The _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'open-input-file)
    '((kind . function)
      (syntax . "(open-input-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "*open-input-file* opens a file for input only. *open-output-file* opens a file for output only. *open-io-file* opens a file for both input and output.An error shall be signaled if _filename_ is not a string. The corresponding file is opened in an implementation defined.")))

(setf (documentation 'function 'open-io-file)
    '((kind . function)
      (syntax . "(open-io-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "*open-input-file* opens a file for input only. *open-output-file* opens a file for output only. *open-io-file* opens a file for both input and output.An error shall be signaled if _filename_ is not a string. The corresponding file is opened in an implementation defined.")))

(setf (documentation 'function 'open-output-file)
    '((kind . function)
      (syntax . "(open-output-file filename [element-class])")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 2)
      (desc . "*open-input-file* opens a file for input only. *open-output-file* opens a file for output only. *open-io-file* opens a file for both input and output.An error shall be signaled if _filename_ is not a string. The corresponding file is opened in an implementation defined.")))

(setf (documentation 'function 'open-stream-p)
    '((kind . function)
      (syntax . "(open-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns t if obj is an open stream; otherwise, returns nil.")))

(setf (documentation 'syntax 'or)
    '((kind . syntax)
      (syntax . "(or form*)")
      (return-type . "<object>")
      (desc . "*or* is the sequential logical <q>or</q> (or <q>∨</q>). _forms_ are evaluated from left to right until either one of them evaluates to a non-nil value or else none are left. If one of them evaluates to a non-nil value, then this non-nil value is returned, otherwise *nil* is returned. The form *or* is equivalent to the following:(or) ≡ 'nil

(or _form_) ≡ _form_

(or _form~1~_ _form~2~_ … _form~n~_) ≡ ((lambda (var)
                                (if var var (or _form~2~_ … _form~n~_))) _form~1~_)
where var does not occur in _form~2~_ … _form~n~_
")))

(setf (documentation 'function 'output-stream-p)
    '((kind . function)
      (syntax . "(output-stream-p obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a stream that can handle output operations; otherwise, returns *nil*.")))

(setf (documentation 'function 'parse-error-expected-class)
    '((kind . function)
      (syntax . "(parse-error-expected-class parse-error)")
      (return-type . "<class>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _string_ and _expected-class_ supplied as data when creating the _parse-error_. An error shall be signaled if _parse-error_ is not a condition of class *<parse-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'parse-error-string)
    '((kind . function)
      (syntax . "(parse-error-string parse-error)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _string_ and _expected-class_ supplied as data when creating the _parse-error_. An error shall be signaled if _parse-error_ is not a condition of class *<parse-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'parse-number)
    '((kind . function)
      (syntax . "(parse-number string)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The characters belonging to _string_ are scanned (as if by read</span>) and if the resulting lexeme is the textual representation of a number, the number it represents is returned.An error shall be signaled if _string_ is not a string (error-id. _domain-error_). An error shall be signaled if _string_ is not the textual representation of a number (error-id. _cannot-parse-number_).")))

(setf (documentation 'function 'preview-char)
    '((kind . function)
      (syntax . "(preview-char [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the next character of _input-stream_, if any. The character is not consumed; the next attempt to peek at or read a character from the _stream_ sees that same character.See §27.1 for information about how _input-stream_, _eos-error-p_, and _eos-value_ are treated.")))

(setf (documentation 'function 'probe-file)
    '((kind . function)
      (syntax . "(probe-file filename)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if the file specified by _filename_ exists; otherwise, returns *nil*. An error shall be signaled if _filename_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'syntax 'progn)
    '((kind . syntax)
      (syntax . "(progn form*)")
      (return-type . "<object>")
      (desc . "This special form allows a series of forms to be evaluated, where normally only one could be used.The result of evaluation of the last form of _form_&#42; is returned. All the _forms_ are evaluated from left to right. The values of all the _forms_ but the last are discarded, so they are executed only for their side-effects. *progn* without forms returns *nil*.")))

(setf (documentation 'setf 'property)
    '((kind . setf)
      (syntax . "(setf (property symbol property-name) obj)")
      (return-type . "<object>")
      (desc . "Causes _obj_ to be the new value of the property named _property-name_ asssociated with the symbol _symbol_. If the property named _property-name_ already exists, its corresponding property value is replaced; otherwise, a new property is created. _obj_ is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. _domain-error_). obj may be any ISLISP object")))

(setf (documentation 'function 'property)
    '((kind . function)
      (syntax . "(property symbol property-name [obj])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the value of the property named _property-name_ associated with the symbol _symbol_. If _symbol_ has no property named _property-name_, _obj_ (which defaults to *nil*) is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. _domain-error_). obj may be any ISLISP object")))

(setf (documentation 'syntax 'quote)
    '((kind . syntax)
      (syntax . "(quote obj)")
      (return-type . "<object>")
      (desc . "
                '_obj_ → *<object>*
            A quoted expression denotes a reference to an object. This notation is used to include any object in an ISLISP text.The character *'* (apostrophe or single quote) is syntax for quotation. That is, (*quote* a) ≡ *'*a.The result of the evaluation of the *quote* special form is _obj_.")))

(setf (documentation 'function 'quotient)
    '((kind . function)
      (syntax . "(quotient dividend divisor+)")
      (return-type . "<number>")
      (min-args . 2)
      (max-args . nil)
      (desc . "The function *reciprocal* returns the reciprocal of its argument _x_; that is, 1/_x_. An error shall be signaled if _x_ is zero (error-id. _division-by-zero_).The function *quotient*, given two arguments _dividend_ and _divisor_, returns the quotient of those numbers. The result is an integer if _dividend_ and _divisor_ are integers and _divisor_ evenly divides _dividend_, otherwise it will be a ﬂoat.Given more than two arguments, *quotient* operates iteratively on each of the _divisor~1~_ … _divisor~n~_ as in _dividend_ / _divisor~1~_ / … / _divisor~n~_. The type of the result follows from the two-argument case because the three-or-more-argument quotient can be defined as follows:(quotient _dividend_ _divisor~1~_ _divisor~2~_ …)
≡ (quotient (quotient _dividend_ _divisor~1~_) _divisor~2~_ …)
An error shall be signaled if _dividend_ is not a number (error-id. _domain-error_). An error shall be signaled if any _divisor_ is not a number (error-id. _domain-error_). An error shall be signaled if any _divisor_ is zero (error-id. _division-by-zero_).")))

(setf (documentation 'function 'read)
    '((kind . function)
      (syntax . "(read [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "The function *read* returns the ISLISP object that is created as the result of reading its textual representation from the stream _input-stream_.See §27.1 for information about how input-stream, eos-error-p, and eos-value are treated.")))

(setf (documentation 'function 'read-byte)
    '((kind . function)
      (syntax . "(read-byte input-stream [eos-error-p [eos-value]])")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Reads a byte from the _input-stream_ and returns it. The number of bits in a byte is determined by the stream element type of the _input-stream_; see §27.1 for information about how _input-stream_, _eos-error-p_, and _eos-value_ are treated.")))

(setf (documentation 'function 'read-char)
    '((kind . function)
      (syntax . "(read-char [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "*read-char* reads a single character from _input-stream_ and returns the corresponding character object.See §27.1 for information about how _input-stream_, _eos-error-p_, and _eos-value_ are treated.")))

(setf (documentation 'function 'read-line)
    '((kind . function)
      (syntax . "(read-line [input-stream [eos-error-p [eos-value]]])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Reads a line of characters from _input-stream_ and returns them as a string (without the newline character at the end of the line). If an _end-of-stream_ is reached before the next newline character and a non-empty line has been read prior to the end-of-stream, that line is returned.See §27.1 for information about how _input-stream_, _eos-error-p_, and _eos-value_ are treated.")))

(setf (documentation 'function 'reciprocal)
    '((kind . function)
      (syntax . "(reciprocal x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *reciprocal* returns the reciprocal of its argument _x_; that is, 1/_x_. An error shall be signaled if _x_ is zero (error-id. _division-by-zero_).The function *quotient*, given two arguments _dividend_ and _divisor_, returns the quotient of those numbers. The result is an integer if _dividend_ and _divisor_ are integers and _divisor_ evenly divides _dividend_, otherwise it will be a ﬂoat.Given more than two arguments, *quotient* operates iteratively on each of the _divisor~1~_ … _divisor~n~_ as in _dividend_ / _divisor~1~_ / … / _divisor~n~_. The type of the result follows from the two-argument case because the three-or-more-argument quotient can be defined as follows:(quotient _dividend_ _divisor~1~_ _divisor~2~_ …)
≡ (quotient (quotient _dividend_ _divisor~1~_) _divisor~2~_ …)
An error shall be signaled if _dividend_ is not a number (error-id. _domain-error_). An error shall be signaled if any _divisor_ is not a number (error-id. _domain-error_). An error shall be signaled if any _divisor_ is zero (error-id. _division-by-zero_).")))

(setf (documentation 'function 'remove-property)
    '((kind . function)
      (syntax . "(remove-property symbol property-name)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Removes the property _property-name_ associated with _symbol_ and returns the property value of the removed property if there is such a property. If there is no such property, *nil* is returned.An error shall be signaled if either _symbol_ or _property-name_ is not a symbol (error-id. _domain-error_).")))

(setf (documentation 'function 'report-condition)
    '((kind . generic-function)
      (syntax . "(report-condition condition stream)")
      (return-type . "<condition>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Presents a natural language description of _condition_ to _stream_. This generic function may be specialized for user-defined condition classes.")))

(setf (documentation 'syntax 'return-from)
    '((kind . syntax)
      (syntax . "(return-from name result-form transfers control and data")
      (return-type . nil)
      (desc . "The *block* special form executes each _form_ sequentially from left to right. If the last form exits normally, whatever it returns is returned by the *block* form.The _name_ in a *block* form is not evaluated; it must be an identifier. The scope of _name_ is the body _form_&#42;—only a *return-from* textually contained in some form can exit the *block*. The extent of _name_ is dynamic.If a _return-from_ is executed, the *result-form* is evaluated. If this evaluation returns normally, the value it returns is immediately returned from the innermost lexically enclosing *block* form with the same name.*return-from* is used to return from a *block*. _name_ is not evaluated and must be an identifier. A *block* special form must lexically enclose the occurrence of *return-from*; the value produced by *result-form* is immediately returned from the *block*. The *return-from* form never returns and does not have a value.An error shall be signaled if an attempt is made to exit a *block* after it has been exited (error-id. _control-error_); It is a violation if _name_ is not an identifier. It is a violation if a *block* with a corresponding _name_ does not exist. See §14.7.2 for other errors.")))

(setf (documentation 'function 'reverse)
    '((kind . function)
      (syntax . "(reverse list)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions each return a list whose elements are those of the given _list_, but in reverse order. An error shall be signaled if _list_ is not a list (error-id. _domain-error_).For *reverse*, no side-effect to the given _list_ occurs. The resulting list is permitted but not required to share structure with the input _list_.For *nreverse*, the conses which make up the top level of the given _list_ are permitted, but not required, to be side-effected in order to produce this new list. *nreverse* should never be called on a literal object.")))

(setf (documentation 'function 'round)
    '((kind . function)
      (syntax . "(round x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the integer nearest to x. If x is exactly halfway between two integers, the even one is chosen. An error shall be signaled if x is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'set-aref)
    '((kind . function)
      (syntax . "(set-aref obj basic-array z*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "These replace the object obtainable by *aref* or *garef* with _obj_. The returned value is _obj_. The constraints on the _basic-array_, the _general-array_, and the sequence of indices _z_ is the same as for *aref* and *garef*.")))

(setf (documentation 'function 'set-car)
    '((kind . function)
      (syntax . "(set-car obj cons)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Updates the left component of _cons_ with _obj_. The returned value is _obj_. An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_). _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'set-cdr)
    '((kind . function)
      (syntax . "(set-cdr obj cons)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Updates the right component of _cons_ with _obj_. The returned value is _obj_ . An error shall be signaled if _cons_ is not a cons (error-id. _domain-error_). _obj_ may be any ISLISP object.")))

(setf (documentation 'syntax 'set-dynamic)
    '((kind . syntax)
      (syntax . "(set-dynamic form var)")
      (return-type . "<object>")
      (desc . "This special form denotes an assignment to a dynamic variable. This form can appear anywhere that (*dynamic* _var_) can appear._form_ is evaluated and the result of the evaluation is used to change the dynamic binding of _var_.An error shall be signaled if _var_ has no dynamic value (error-id. _unbound-variable_). *setf* of *dynamic* can be used only for modifying bindings, and not for establishing them.
            ")))

(setf (documentation 'function 'set-elt)
    '((kind . function)
      (syntax . "(set-elt obj sequence z)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "These replace the object obtainable by *elt* with _obj_. The returned value is _obj_.An error shall be signaled if _z_ is an integer outside of the valid range of indices (error-id. _index-out-of-range_). An error shall be signaled if _sequence_ is not a basic-vector or a list or if _z_ is not an integer (error-id. _domain-error_). obj may be any ISLISP object.")))

(setf (documentation 'function 'set-file-position)
    '((kind . function)
      (syntax . "(set-file-position stream z)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Attempts to change the file position (see implementation-defined motion within the file might still be performed. The value returned is the new file position, which might or might not be _z_.An error shall be signaled if _stream_ is not a stream to or from a file, or if _z_ is not a non-negative integer (error-id. _domain-error_).")))

(setf (documentation 'function 'set-garef)
    '((kind . function)
      (syntax . "(set-garef obj general-array z*)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . nil)
      (desc . "These replace the object obtainable by *aref* or *garef* with _obj_. The returned value is _obj_. The constraints on the _basic-array_, the _general-array_, and the sequence of indices _z_ is the same as for *aref* and *garef*.")))

(setf (documentation 'function 'set-property)
    '((kind . function)
      (syntax . "(set-property obj symbol property-name)")
      (return-type . "<object>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Causes _obj_ to be the new value of the property named _property-name_ asssociated with the symbol _symbol_. If the property named _property-name_ already exists, its corresponding property value is replaced; otherwise, a new property is created. _obj_ is returned.An error shall be signaled if either symbol or property-name is not a symbol (error-id. _domain-error_). obj may be any ISLISP object")))

(setf (documentation 'syntax 'setf)
    '((kind . syntax)
      (syntax . "(setf place form)")
      (return-type . "<object>")
      (desc . "This macro is used for generalized assignment.*setf* takes a _place_ and stores in this place the result of the evaluation of the form _form_. The _place_ form is not evaluated as a whole entity, but subforms of _place_ are evaluated sequentially from left to right to determine a place to be assigned a value. When _place_ is denoted by an identifier, *setf* behaves exactly as *setq*. The returned value is the result of the evaluation of _form_. The valid places for the *setf* special form are as follows:
                [cols=\"1,1\"]
|===
| variables | var


| dynamic bindings | (dynamic var)

| the components of a basic-array | (aref basic-array z1 … zn)

| the components of a general array | (garef general-array z1 … zn)

| the components of a list | (elt list z)

| the components of a vector | (elt basic-vector z)

| the left component of a cons | (car cons)

| the right component of a cons | (cdr cons)

| a property of a symbol | (property symbol property)

| a slot of an instance of a class | (reader-function-name instance)
|===

A place can also be a macro form that expands (during preparation for execution) into a place or a function application form with operator _op_ for which *setf* is defined or for which a generic function named (*setf* _op_) has been defined. In these last two cases, that function will receive as arguments the new value to be assigned followed by the objects that resulted from evaluating the arguments of the function application form.")))

(setf (documentation 'syntax 'setq)
    '((kind . syntax)
      (syntax . "(setq var form)")
      (return-type . "<object>")
      (desc . "This form represents an assignment to the variable denoted by the _identifier_. In consequence, the _identifier_ may designate a different object than before, the value of _form_.The result of the evaluation of _form_ is returned. This result is used to modify the variable binding denoted by the identifier _var_ (if it is mutable). *setq* can be used only for modifying bindings, and not for establishing a variable. The *setq* special form must be contained in the scope of _var_ , established by lambda</span> expression.")))

(setf (documentation 'function 'signal-condition)
    '((kind . function)
      (syntax . "(signal-condition condition continuable)")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Invokes the condition handling system on _condition_.If _continuable_ is *nil*, the results of attempting to “continue” (see implementation-defined string such as \"Continue with no special action.\" is to be used.")))

(setf (documentation 'function 'simple-error-format-arguments)
    '((kind . function)
      (syntax . "(simple-error-format-arguments simple-error)")
      (return-type . "<list>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _format-string_ and _format-arguments_ supplied as data when creating the _simple-error_. An error shall be signaled if _simple-error_ is not a condition of class *<simple-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'simple-error-format-string)
    '((kind . function)
      (syntax . "(simple-error-format-string simple-error)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _format-string_ and _format-arguments_ supplied as data when creating the _simple-error_. An error shall be signaled if _simple-error_ is not a condition of class *<simple-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'sin)
    '((kind . function)
      (syntax . "(sin x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sin* returns the sine of _x_ . The function *cos* returns the cosine of _x_. The function tan returns the tangent of _x_. In each case, _x_ must be given in radians.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'sinh)
    '((kind . function)
      (syntax . "(sinh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sinh* returns the hyperbolic sine of _x_. The function *cosh* returns the hyperbolic cosine of _x_. The function *tanh* returns the hyperbolic tangent of _x_.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'syntax 'special-operator)
    '((kind . syntax)
      (syntax . "(special-operator argument*)")
      (return-type . "<result-class>")
      (desc . nil)))

(setf (documentation 'function 'sqrt)
    '((kind . function)
      (syntax . "(sqrt x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the non-negative square root of _x_. An error shall be signaled if _x_ is not a non-negative number (error-id. _domain-error_).")))

(setf (documentation 'function 'standard-input)
    '((kind . function)
      (syntax . "(standard-input)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function *standard-input* returns the stream used as the default stream for input functions.The function *standard-output* returns the stream used as the default stream for output functions.The function *error-output* returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially with-error-output</span>.")))

(setf (documentation 'function 'standard-output)
    '((kind . function)
      (syntax . "(standard-output)")
      (return-type . "<stream>")
      (min-args . 0)
      (max-args . 0)
      (desc . "The function *standard-input* returns the stream used as the default stream for input functions.The function *standard-output* returns the stream used as the default stream for output functions.The function *error-output* returns the stream used as the default stream for warnings and non-interactive error messages.The value returned by each of these functions is initially with-error-output</span>.")))

(setf (documentation 'function 'stream-error-stream)
    '((kind . function)
      (syntax . "(stream-error-stream stream-error)")
      (return-type . "<stream>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the _stream_ supplied as data when creating the _stream-error_. An error shall be signaled if *stream-error* is not a condition of class *<stream-error>* (error-id. _domain-error_).")))

(setf (documentation 'function 'stream-ready-p)
    '((kind . function)
      (syntax . "(stream-ready-p input-stream)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if an attempt to obtain the next element from the _stream_ will not cause the processor to have to wait; otherwise, returns *nil*. An error shall be signaled if _stream_ is not a stream that can handle input operations (error-id. _domain-error_).")))

(setf (documentation 'function 'streamp)
    '((kind . function)
      (syntax . "(streamp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a stream (instance of class *<stream>*); otherwise, returns *nil*. _obj_ may be any ISLISP object. *streamp* is unaffected by whether its argument, if an instance of the class *<stream>*, is open or closed.")))

(setf (documentation 'function 'string-append)
    '((kind . function)
      (syntax . "(string-append string*)")
      (return-type . "<string>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns a single string containing a sequence of characters that results from appending the sequences of characters of each of the _strings_, or \"\" if given no _strings_. An error shall be signaled if any _string_ is not a string (error-id. _domain-error_).This function does not modify its arguments. It is implementation defined whether and when the result shares structure with its _string_ arguments.An error shall be signaled if the string cannot be allocated (error-id. _cannot-create-string_).")))

(setf (documentation 'function 'string-index)
    '((kind . function)
      (syntax . "(string-index substring string [start-position])")
      (return-type . "<object>")
      (min-args . 2)
      (max-args . 3)
      (desc . "Returns the position of the given _substring_ within _string_. The search starts from the position indicated by _start-position_ (which is 0-based and defaults to 0). The value returned if the search succeeds is an offset from the beginning of the string, not from the starting point. If that _substring_ does not occur in the _string_, *nil* is returned. Presence of the _substring_ is done by sequential use of *char=* on corresponding elements of the two strings.An error shall be signaled if either _substring_ or _string_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string/=)
    '((kind . function)
      (syntax . "(string/= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string<)
    '((kind . function)
      (syntax . "(string< string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string<=)
    '((kind . function)
      (syntax . "(string<= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string=)
    '((kind . function)
      (syntax . "(string= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string>)
    '((kind . function)
      (syntax . "(string> string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'string>=)
    '((kind . function)
      (syntax . "(string>= string1 string2)")
      (return-type . "quasi-boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "The function *string=* tests whether _string~1~_ is the same string as _string~2~_. The function *string<* tests whether _string~1~_ is less than _string~2~_. The function *string<=* tests whether _string~1~_ is less than or equal to _string~2~_.The ordering used is based on character comparisons.Two strings are *string=* if they are of the same length, _l_ , and if for every _i_, where 0 ≤ _i_ < _l_, (*char=* (*elt* _string~1~_ _i_) (*elt* _string~2~_ _i_)) holds.Two strings _string~1~_ and _string~2~_ are in order (*string<*) if in the first position in which they differ the character of _string~1~_ is *char<* the corresponding character of _string~2~_, or if the _string~1~_ is a proper prefix of _string~2~_ (of shorter length and matching in all the characters of _string~1~_).Two strings are _string<=_ if they are either *string<* or they are *string=*.Two strings are *string/=* if and only if they are not *string=*. Two strings are *string>* if and only if they are not *string<=*. Two strings are *string>=* if and only if they are not *string<*.For these 6 string comparison functions, if the test is satisfied, an implementation-defined non-nil value is returned; otherwise, *nil* is returned.An error shall be signaled if either _string~1~_ or _string~2~_ is not a string (error-id. _domain-error_).")))

(setf (documentation 'function 'stringp)
    '((kind . function)
      (syntax . "(stringp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a string (instance of class *<string>*); otherwise, returns *nil*. _obj_ may be any ISLISP object.")))

(setf (documentation 'function 'subclassp)
    '((kind . function)
      (syntax . "(subclassp class1 class2)")
      (return-type . "boolean")
      (min-args . 2)
      (max-args . 2)
      (desc . "Returns *t* if the class _class~1~_ is a subclass of the class _class~2~_; otherwise, returns *nil*. An error shall be signaled if either _class~1~_ or _class~2~_ is not a class object (error-id. _domain-error_).")))

(setf (documentation 'function 'subseq)
    '((kind . function)
      (syntax . "(subseq sequence z1 z2)")
      (return-type . "<sequence>")
      (min-args . 3)
      (max-args . 3)
      (desc . "Given a sequence _sequence_ and two integers _z~1~_ and _z~2~_ satisfying 0 ≤ _z~1~_ ≤ _z~2~_ ≤ (*length* _sequence_), this function returns the subsequence of length _z~2~_ − _z~1~_, containing the elements with indices from _z~1~_ (inclusive) to _z~2~_ (exclusive). The subsequence is newly allocated, and has the same class as _sequence_.An error shall be signaled if the requested subsequence cannot be allocated (error-id. _cannot-create-sequence_). An error shall be signaled if _z~1~_ or _z~2~_ are outside of the bounds mentioned (error-id. _index-out-of-range_). An error shall be signaled if _sequence_ is not a basic-vector or a list, or if _z~1~_ is not an integer, or if _z~2~_ is not an integer (error-id. _domain-error_).")))

(setf (documentation 'function 'symbolp)
    '((kind . function)
      (syntax . "(symbolp obj)")
      (return-type . "boolean")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns *t* if _obj_ is a symbol (instance of class *<symbol>*); otherwise, returns *nil*. The _obj_ may be any ISLISP object.")))

(setf (documentation 'value 't)
    '((kind . constant)
      (value-type . "<symbol>")
      (desc . "*t* is a named constant whose value is the symbol *t* itself. *nil* is a named constant whose value is the symbol *nil* itself.")))

(setf (documentation 'syntax 'tagbody)
    '((kind . syntax)
      (syntax . "(tagbody {tagbody-tag | form}*)")
      (return-type . "<object>")
      (desc . "*tagbody* executes the _forms_ sequentially from left to right, discarding their values. If the execution of the last _form_ completes normally, *nil* is returned by the *tagbody* special form.The series of _tagbody-tags_ and _forms_ is collectively referred to as the body of a *tagbody* form. An identifier _tagbody-tag_ that appears at toplevel of the body denotes a <span class=\"term\">tagbody tag</span> that can be used with *go* to transfer control to that point in the body. Any compound form that appears is taken as a _form_. Literals are not permitted at the toplevel of a *tagbody*. No _tagbody-tag_ may appear more than once in the tags in the body.The namespace used for tagbody tags is distinct from that used for *block* tags.At any point lexically contained in the *tagbody* a form (*go* _tag_) can be used to transfer control to a tag _tag_ that appears among the _tagbody-tags_, except where a _tag_ is shadowed according to the lexical principle (see §14.7.2 for other errors.Note: As a stylistic matter, programmers are not encouraged to use *tagbody* and *go* in everyday programming. The primary uses for which these forms are intended are for implementing other control abstractions (using macros), and for the occassional real-world situation that uses unstructured imperative transfer of control (such as a finite state machine).")))

(setf (documentation 'function 'tan)
    '((kind . function)
      (syntax . "(tan x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sin* returns the sine of _x_ . The function *cos* returns the cosine of _x_. The function tan returns the tangent of _x_. In each case, _x_ must be given in radians.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'tanh)
    '((kind . function)
      (syntax . "(tanh x)")
      (return-type . "<number>")
      (min-args . 1)
      (max-args . 1)
      (desc . "The function *sinh* returns the hyperbolic sine of _x_. The function *cosh* returns the hyperbolic cosine of _x_. The function *tanh* returns the hyperbolic tangent of _x_.An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'syntax 'the)
    '((kind . syntax)
      (syntax . "(the class-name form)")
      (return-type . "<object>")
      (desc . "These forms evaluate _form_. If _form_ returns, the returned value is returned by the *the* or *assure* form. In addition, these forms specify that the value of _form_ is of the class specified by _class-name_ (which must be the name of an existing class).In a *the* special form, the consequences are undefined if the value of _form_ is not of the class or a subclass of the class designated by _class-name_. In an *assure* special form, an error shall be signaled if the value of _form_ is not of the class or a subclass of the class designated by _class-name_ (error-id. _domain-error_).")))

(setf (documentation 'syntax 'throw)
    '((kind . syntax)
      (syntax . "(throw tag-form result-form) transfers control and data")
      (return-type . nil)
      (desc . "The special forms *catch* and *throw* provide a facility for programming of structured non-local dynamic exits. A *catch* form and a *throw* form are said to correspond if the _tag-form_ of the *catch* and the _tag-form_ of the *throw* evaluate to the same object, a <span class=\"term\">catch tag</span>. A <span class=\"term\">catch tag</span> may be any object other than a number or a character; the comparison of _catch tags_ uses *eq*.The *catch* special form first evaluates the _tag-form_ to produce a _catch tag_, and then executes each _form_ sequentially from left to right. If execution of the _forms_ finishes normally, whatever is returned by the last _form_ is returned by the *catch* form.Prior to execution of the forms of a *catch* form _C~0~_, an association between the _catch tag T~0~_ and the executing form _C~0~_ is dynamically established, upon exit from _C~0~_, the association is disestablished. If there was an outer association for the same _catch tag T~0~_, it is hidden during the execution of _C~0~_'s _forms_; only the most recently established (<span class=\"latin\">i.e.</span>, innermost) association for _T~0~_ is ever visible.If a *throw* special form is executed, it evaluates the _tag-form_ producing a _catch tag T~1~_, and then evaluates the _result-form_ producing a result _R~1~_. If there is a corresponding association between _T~1~_ and some *catch* form _C~1~_ that is executing, _R~1~_ is immediately returned as the value of _C~1~_. The *throw* form can be anywhere in the entire current toplevel scope; it need not be lexically contained within _C~1~_.An error shall be signaled if there is no outstanding catcher for a _T~1~_ (error-id. _control-error_). See §14.7.2 for other errors.")))

(setf (documentation 'function 'truncate)
    '((kind . function)
      (syntax . "(truncate x)")
      (return-type . "<integer>")
      (min-args . 1)
      (max-args . 1)
      (desc . "Returns the integer between 0 and _x_ (inclusive) that is nearest to _x_. That is, _x_ is truncated towards zero. An error shall be signaled if _x_ is not a number (error-id. _domain-error_).")))

(setf (documentation 'function 'undefined-entity-name)
    '((kind . function)
      (syntax . "(undefined-entity-name undefined-entity)")
      (return-type . "<symbol>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _name_ and _namespace_ supplied as data when creating the _undefined-entity_. An error shall be signaled if _undefined-entity_ is not a condition of class *<undefined-entity>* (error-id. _domain-error_).The result of *undefined-entity-namespace* will be one of the symbols *variable*, *dynamic-variable*, *function*, or *class*.")))

(setf (documentation 'function 'undefined-entity-namespace)
    '((kind . function)
      (syntax . "(undefined-entity-namespace undefined-entity)")
      (return-type . "<symbol>")
      (min-args . 1)
      (max-args . 1)
      (desc . "These functions return the _name_ and _namespace_ supplied as data when creating the _undefined-entity_. An error shall be signaled if _undefined-entity_ is not a condition of class *<undefined-entity>* (error-id. _domain-error_).The result of *undefined-entity-namespace* will be one of the symbols *variable*, *dynamic-variable*, *function*, or *class*.")))

(setf (documentation 'syntax 'unwind-protect)
    '((kind . syntax)
      (syntax . "(unwind-protect form cleanup-form*)")
      (return-type . "<object>")
      (desc . "*unwind-protect* first evaluates _form_. Evaluation of the _cleanup-forms_ always occurs, regardless of whether the exit is normal or non-local.If the _form_ exits normally yielding a value _R_, then if all of the _cleanup-forms_ exit normally the value _R_ is returned by the *unwind-protect* form.If a non-local exit from _form_ occurs, then the _cleanup-forms_ are executed as part of that exit, and then if all of the _cleanup-forms_ exit normally the original non-local exit continues.The _cleanup-forms_ are evaluated from left to right, discarding the resulting values. If execution of the _cleanup-forms_ finishes normally, exit from the *unwind-protect* form proceeds as described above. It is permissible for a _cleanup-form_ to contain a non-local exit from the *unwind-protect* form, subject to the following constraint:An error shall be signaled if during execution of the _cleanup-forms_ of an *unwind-protect* form, a non-local exit is executed to a destination which has been marked as invalid due to some other non-local exit that is already in progress (see §14.7.1) (error-id. _control-error_).Note: Because ISLISP does not specify an interactive debugger, it is unspecified whether or how error recovery can occur interactively if programmatic handling fails. The intent is that if the ISLISP processor does not terminate abnormally, normal mechanisms for non-local exit (*return-from*, *throw*, or *go*) would be used as necessary and would respect these _cleanup-forms_.")))

(setf (documentation 'function 'vector)
    '((kind . function)
      (syntax . "(vector obj*)")
      (return-type . "<general-vector>")
      (min-args . 0)
      (max-args . nil)
      (desc . "Returns a new general-vector whose elements are its _obj_ arguments. The length of the newly created vector is, therefore, the number of _objs_ passed as arguments. The vector is indexed by integers ranging from 0 to dimension−1. An error shall be signaled if the requested vector cannot be allocated (error-id. _cannot-create-vector_). Each _obj_ may be any ISLISP object.")))

(setf (documentation 'syntax 'while)
    '((kind . syntax)
      (syntax . "(while test-form body-form*)")
      (return-type . "<null>")
      (desc . "Iterates while the _test-form_ returns a true value. Specifically:
                <li>_test-form_ is evaluated, producing a value _V~t~_.</li>
                <li>If _Vt_ is *nil*, then the *while* form immediately returns *nil*.</li>
                <li>Otherwise, if _Vt_ is non-nil, the forms _body-form_&#42; are evaluated sequentially (from left to right).</li>
                <li>Upon successful completion of the _body-forms_&#42;, the *while* form begins again with step 1.</li>
            ")))

(setf (documentation 'syntax 'with-error-output)
    '((kind . syntax)
      (syntax . "(with-error-output stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their _stream-form_ argument to produce a stream _s_ and then evaluate their body _forms_ in a dynamic environment where the corresponding function (*standard-input*, *standard-output*, or *error-output*) returns the stream _s_. The returned value of each of these _forms_ is the result of the evaluation of the last _form_ of their body (or *nil* if there is none).")))

(setf (documentation 'syntax 'with-handler)
    '((kind . syntax)
      (syntax . "(with-handler handler form*)")
      (return-type . "<object>")
      (desc . "Evaluates _handler_, which must yield a function (called the “handler function”). The handler function is established as active handler (see §29.2) and then the forms are executed. If execution of _forms_ finishes normally, the value of the last _form_ (or *nil* if there are no forms) is returned.")))

(setf (documentation 'syntax 'with-open-input-file)
    '((kind . syntax)
      (syntax . "(with-open-input-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using *open-input-file*, *open-output-file*, or *open-io-file*, respectively), evaluates the _forms_, closes the file, and returns the value returned by the last _form_ (or *nil* if there are no forms).The _filename_ and _element-class_ are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by _name_ (as if a let was used), so the identifier _name_ can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-open-io-file)
    '((kind . syntax)
      (syntax . "(with-open-io-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using *open-input-file*, *open-output-file*, or *open-io-file*, respectively), evaluates the _forms_, closes the file, and returns the value returned by the last _form_ (or *nil* if there are no forms).The _filename_ and _element-class_ are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by _name_ (as if a let was used), so the identifier _name_ can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-open-output-file)
    '((kind . syntax)
      (syntax . "(with-open-output-file (name filename [element-class]) form*)")
      (return-type . "<object>")
      (desc . "Each of these special forms opens a stream to a file (using *open-input-file*, *open-output-file*, or *open-io-file*, respectively), evaluates the _forms_, closes the file, and returns the value returned by the last _form_ (or *nil* if there are no forms).The _filename_ and _element-class_ are evaluated and passed as arguments to the appropriate file-opening function. The stream created by opening the file is bound to the variable named by _name_ (as if a let was used), so the identifier _name_ can be used to refer to the stream.The stream is closed on exit, whether or not exit from these special forms is normal. For this reason, these special forms are usually preferred over the corresponding functions for opening and closing files.")))

(setf (documentation 'syntax 'with-standard-input)
    '((kind . syntax)
      (syntax . "(with-standard-input stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their _stream-form_ argument to produce a stream _s_ and then evaluate their body _forms_ in a dynamic environment where the corresponding function (*standard-input*, *standard-output*, or *error-output*) returns the stream _s_. The returned value of each of these _forms_ is the result of the evaluation of the last _form_ of their body (or *nil* if there is none).")))

(setf (documentation 'syntax 'with-standard-output)
    '((kind . syntax)
      (syntax . "(with-standard-output stream-form form*)")
      (return-type . "<object>")
      (desc . "These special forms first evaluate their _stream-form_ argument to produce a stream _s_ and then evaluate their body _forms_ in a dynamic environment where the corresponding function (*standard-input*, *standard-output*, or *error-output*) returns the stream _s_. The returned value of each of these _forms_ is the result of the evaluation of the last _form_ of their body (or *nil* if there is none).")))

(setf (documentation 'function 'write-byte)
    '((kind . function)
      (syntax . "(write-byte z output-stream)")
      (return-type . "<integer>")
      (min-args . 2)
      (max-args . 2)
      (desc . "Writes _z_ to the _output-stream_ and returns it. An error shall be signaled if _z_ is not an integer in the range appropriate to the stream element type of _output-stream_ or if _output-stream_ is not a stream capable of handling output operations (error-id. _domain-error_).")))

