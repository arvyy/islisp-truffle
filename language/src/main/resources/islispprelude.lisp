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

(defclass <arithmetic-error> (<error>) ())
(defclass <division-by-zero> (<arithmetic-error>) ())
(defclass <floating-point-overflow> (<arithmetic-error>) ())
(defclass <floating-point-underflow> (<arithmetic-error>) ())

(defclass <stream-error> (<error>) ())

(defclass <end-of-stream> (<stream-error>) ())

(defclass <simple-error> (<error>)
    ((format-string :reader simple-error-format-string :initarg format-string)
     (format-arguments :reader simple-error-format-arguments :initarg simple-error-format-arguments)))

(defun error (error-string :rest obj)
    (signal-condition
        (create (class <simple-error>)
                'format-string error-string
                'format-arguments obj)
        nil))

(defclass <program-error> (<error>) ())

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

(defclass <undefined-entity> (<program-error>) ())

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

(defun /= (x1 x2)
  (not (= x1 x2)))

(defun >= (x1 x2)
  (or (> x1 x2) (= x1 x2)))

(defun < (x1 x2)
  (> x2 x1))

(defun <= (x1 x2)
  (or (< x1 x2) (= x1 x2)))

(defun char/= (x1 x2)
  (not (char= x1 x2)))

(defun char>= (x1 x2)
  (or (char> x1 x2) (char= x1 x2)))

(defun char> (x1 x2)
  (char< x2 x1))

(defun char<= (x1 x2)
  (or (char< x1 x2) (char= x1 x2)))

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

(defun string> (s1 s2)
  (string< s2 s1))

(defun string>= (s1 s2)
  (or (string> s1 s2) (string= s1 s2)))

(defun string<= (s1 s2)
  (or (string< s1 s2) (string= s1 s2)))

(defun string/= (s1 s2)
  (not (string= s1 s2)))

(defun abs (x)
  (if (< x 0)
      (- x)
      x))

(defun numberp (obj)
  (instancep obj (class <number>)))

(defun integerp (obj)
  (instancep obj (class <integer>)))

(defun characterp (obj)
  (instancep obj (class <character>)))

(defun consp (obj)
  (instancep obj (class <cons>)))

(defun basic-array-p (obj)
  (instancep obj (class <basic-array>)))

(defun basic-array*-p (obj)
  (instancep obj (class <basic-array*>)))

(defun general-array*-p (obj)
  (instancep obj (class <general-array*>)))

(defun general-vector-p (obj)
  (instancep obj (class <general-vector>)))

(defun streamp (obj)
  (instancep obj (class <stream>)))

(defun truffle-object-p (obj)
  (instancep obj (class <truffle-object>)))

(defun identity (obj) obj)

(defun not (obj)
  (if obj nil t))

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
  (for ((list list (cdr list))
        (reversed () (cons (car list) reversed)))
       ((not list) reversed)))

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