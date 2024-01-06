(requires "testing.lisp")

(defmacro incf (var value)
    `(setq ,var (+ ,var ,value)))

(defun foo ()
 (let ((val 0))
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val))
(test-equal (foo) 15)

(format-object (standard-output) "tagbody.lisp end" nil)
(finish-output (standard-output))
