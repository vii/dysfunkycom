(in-package :dysfunkycom-test)

(defun approximately-equal (x y &optional (error-toleration 0.01))
  (and (< (abs (/ (- x y) x)) error-toleration)
       (< (abs (/ (- x y) y)) error-toleration)))

(deftest hohman-test ()
  (let ((result (dysfunkycom::hohmann 6678d3 42164d3)))
    (is (approximately-equal (first result) 2.42d3)
	(approximately-equal (second result) 1.46d3))))


