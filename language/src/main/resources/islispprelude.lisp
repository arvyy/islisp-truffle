(defclass <serious-condition> ()
    ((stacktrace :reader condition-stacktrace :writer set-condition-stacktrace)))

(defclass <error> (<serious-condition>) ())

(defclass <program-error> (<error>) ())

(defclass <arity-error> (<program-error>)
    ((actual :reader arity-error-actual-count :initarg actual)
     (required-min :reader arity-error-required-min :initarg required-min)
     (required-max :reader arity-error-required-max :initarg required-max)))

(defgeneric fill-in-condition-stacktrace (condition))
(defmethod fill-in-condition-stacktrace ((condition <condition>))
    t)

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
