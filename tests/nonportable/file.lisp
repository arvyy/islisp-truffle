;; nonportable, because string -> file resolution is implementation dependent.
(requires "testing.lisp")

(delete-file "../tests/nonportable/file.txt")
(delete-file "../tests/nonportable/file2.txt")
(delete-file "../tests/nonportable/file.dat")

;; writing text
(let ((out (open-output-file "../tests/nonportable/file.txt")))
  (format out "(1 2 3)a")
  (finish-output out)
  (close out))

;; probing
(test-equal t (probe-file "../tests/nonportable/file.txt"))
(test-equal nil (probe-file "../tests/nonportable/doesntexist"))

;; reading text
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
  (close stream))

;; writing bytes
(let ((out (open-output-file "../tests/nonportable/file.dat")))
  (write-byte 200 out)
  (finish-output out)
  (close out))

;; reading bytes
(let ((in (open-input-file "../tests/nonportable/file.dat"))
      (b nil))
  (setf b (read-byte in))
  (test-equal 200 b)
  (test-equal nil (read-byte in nil))
  (close in))

;; util macros
(let ((ran nil)
      (stream* nil))
    (with-open-input-file (stream "../tests/nonportable/file.txt")
        (setf ran t)
        (setf stream* stream)
        (test-equal '(1 2 3) (read stream))
        (test-equal nil (closed-p stream)))
    (test-equal t (closed-p stream*))
    (test-equal t ran))

;; util macros
(let ((ran nil)
      (stream* nil))
    (with-open-output-file (stream "../tests/nonportable/file.txt")
        (format stream "(1 2 3)a")
        (finish-output stream)
        (setf ran t)
        (setf stream* stream)
        (test-equal nil (closed-p stream)))
    (test-equal t (closed-p stream*))
    (test-equal t ran))

;; test input+output at once
(let ((stream (open-io-file "../tests/nonportable/file2.txt"))
      (position nil))
  (setf position (file-position stream))
  (format stream "test")
  (set-file-position stream position)
  (test-equal 'test (read stream))
  (close stream))