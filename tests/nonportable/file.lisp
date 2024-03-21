;; nonportable, because string -> file resolution is implementation dependent.

(requires "testing.lisp")

(test-equal t (probe-file "../tests/nonportable/file.txt"))
(test-equal nil (probe-file "../tests/nonportable/doesntexist"))

(let ((stream (open-input-file "../tests/nonportable/file.txt" 2))
      (position nil)
      (datum nil))
  (setf position (file-position stream))
  (test-equal t (integerp position))
  (setf datum (read stream))
  (test-equal '(1 2 3) datum)
  (test-equal nil (= position (file-position stream)))
  (set-file-position stream position)
  (setf datum (read stream))
  (test-equal '(1 2 3) datum)
  (setf datum (read stream))
  (test-equal 'a datum)
  )