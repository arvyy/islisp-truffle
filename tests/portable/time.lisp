(requires "testing.lisp")

;; frivolous function to fake compution for long enough that at least some time passes.
;; prints empty string so that runtime cannot optimize action away
(defun fake-busy-fn (n)
  (for ((i n (- i 1)))
       ((= i 0) t)
   (format (standard-output) "")))

(test-equal (integerp (internal-time-units-per-second)) t)

(let ((t1 (get-internal-real-time))
      (t2 (get-internal-run-time))
      (t3 (get-universal-time)))
  (test-equal (integerp t1) t)
  (test-equal (integerp t2) t)
  (test-equal (integerp t3) t)

  (fake-busy-fn 10000)

  (test-equal (< t1 (get-internal-real-time)) t)
  (test-equal (< t2 (get-internal-run-time)) t)
  ;; not too meaningful to test, since its granularity is in seconds, so it will fail with small fake-busy-fn,
  ;; and test will be time-wasteful with large fake-busy-fn.
  ;;(test-equal (< t3 (get-universal-time)) t)
  )

(format-object (standard-output) "time.lisp end" nil)
(finish-output (standard-output))