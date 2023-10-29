(defclass <serious-condition> ()
    ((stacktrace :reader condition-stacktrace :writer set-condition-stacktrace)
     (continuabl? :reader condition-continuable :writer set-condition-continuable)))

(defclass <error> (<serious-condition>) ())

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
    (let ((stacktrace (condition-stacktrace condition)))
        (if stacktrace
          (let ((i 0)
                (len (length stacktrace)))
            (while (< i len)
              (format-object stream (elt stacktrace i) nil)
              (format-char stream #\newline)
              (setf i (+ i 1)))))))

(defmethod report-condition ((condition <unbound-variable>) (stream <stream>))
    (format-object stream "Unbound variable: " nil)
    (format-object stream (unbound-variable-name condition) nil)
    (format-char stream #\newline)
    (call-next-method))

(defun /= (x1 x2)
  (not (= x1 x2)))

(defun >= (x1 x2)
  (or (> x1 x2) (= x1 x2)))

(defun < (x1 x2)
  (> x2 x1))

(defun <= (x1 x2)
  (or (< x1 x2) (= x1 x2)))

(defun abs (x)
  (if (< x 0)
      (- x)
      x))

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

(defun numberp (obj)
  (instancep obj (class <number>)))

(defun integerp (obj)
  (instancep obj (class <integer>)))

(defun characterp (obj)
  (instancep obj (class <character>)))

(defun consp (obj)
  (instancep obj (class <cons>)))

(defun identity (obj) obj)

(defun not (obj)
  (if obj nil t))

(defun null (obj)
  (if obj nil t))

(defmacro or  (:rest args)
  (if (= 0 (length args))
      nil
      (if (= 1 (length args))
          (car args)
          (let ((first (car args))
                (var (gensym))
                (rest (cdr args)))
            `(let ((,var ,first))
                (if ,var ,var (or ,@rest))) ))))
