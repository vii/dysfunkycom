(in-package :dysfunkycom)

(defun calc-unit-tangent-vector (x y)
  )

(defun hohmann (r1 r2)
  "Inputs:
- r1 is the radius of current orbit
- r2 is the radius of target orbit

Outputs: a list of double-floats
- initial-delta-v
- final-delta-v
- estimated time
 "
  (let ((initial-delta-v
	 (* (sqrt (/ +g-m-earth+ r1))
	    (- (sqrt (/ (* 2d0 r2)
			(+ r1 r2)))
	       1d0)))
	(final-delta-v
	 (* (sqrt (/ +g-m-earth+ r2))
	    (- 1d0
	       (sqrt (/ (* 2d0 r1)
			(+ r1 r2))))))
	(estimated-time
	 (* pi
	    (sqrt (/ (expt (+ r1 r2) 3)
		     (* 8d0 +g-m-earth+))))))
    (list initial-delta-v
	  final-delta-v
	  estimated-time)))

(defun hohmann-controller (simulator)
  (let ((first-time-p t)
	(dVx 0d0)
	(dVy 0d0))
    (labels ((done-p (&rest args)
	       (declare (ignorable args))
	       nil))
      (iter (for output = (coerce (funcall simulator dVx dVy) 'list))
	    (for (score fuel x y r) = output)
	    (while (not (apply #'done-p (list score fuel x y r))))
	    ;; calculate inputs for actuator
	    (when first-time-p
	      (let* ((r2 r)
		     (r1 (sqrt (+ (* x x) (* y y))))
		     (result (hohmann r1 r2)))
		(apply #'format t "~&Result for Hohmann method: dV1 = ~a, dV2 = ~a; estimated arrival time: ~a~%" result)
		;; (setf dVx ...
;; 		      dVy ...)
		))
	    ;; (when (reach-the-target-orbit)
;; 	      (setf dVx ...
;; 		    dVy ...))
	    ;; step states
	    (setf first-time-p nil)
	    (finally (return 'done))))))
