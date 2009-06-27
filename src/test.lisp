(in-package :dysfunkycom-test)

(defun approximately-equal (x y &optional (error-toleration 0.01d0))
  (and (< (abs (/ (- x y) x)) error-toleration)
       (< (abs (/ (- x y) y)) error-toleration)))

(deftest hohman-test ()
  (let ((result (dysfunkycom::hohmann 6678d3 42164d3)))
    (is (approximately-equal (first result) 2.42d3)
	(approximately-equal (second result) 1.46d3))))

(deftest calc-angle-between-vectors-test ()
  (is (dysfunkycom::calc-angle-between-vectors 1 1 1 -1)
      (/ pi 2)))

