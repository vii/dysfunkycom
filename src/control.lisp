(in-package :dysfunkycom)

(defparameter *trace-simulation-outputs* nil)

(defun hohmann (r1 r2)
  "Inputs:
- r1 (meters) is the radius of current orbit
- r2 (meters) is the radius of target orbit

Outputs: a list of double-floats
- initial-delta-v (m/sec)
- final-delta-v (m/sec)
- estimated time (sec)
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
  (let ((thrusts '()))
    (handler-case
	(labels ((run (simulator dVx dVy)
		   (push (list dVx dVy) thrusts)
		   (let ((output (funcall simulator dVx dVy)))
		     (when *trace-simulation-outputs*
		       (describe-output-1 output))
		     output))
		 (done-p (output)
		   (destructuring-array-bind (score nil x y r) output 
		     (assert (not (minusp score)))
		     (approximately-equal
		      (sqrt (+ (* x x) (* y y)))
		      r
		      0.000001))))	; TODO: check the epsilon
	  (let (approximated-direction)
	    ;; 1. estimate the direction
	    (let* ((init-output (run simulator 0d0 0d0))
		   (x1 (aref init-output 2))
		   (y1 (aref init-output 3))) 
	      (setf approximated-direction
		    (iter (for output = (run simulator 0d0 0d0)) 
			  (for x2 = (aref output 2))
			  (for y2 = (aref output 3)) 
			  ;; TODO: check epsilons
			  (while (and (approximately-equal x1 x2)
				      (approximately-equal y1 y2)))
			  (finally
			   (return (vec (- x2 x1) (- y2 y1)))))))
	    ;; 2. run the hohmann method
	    (destructuring-array-bind (score nil x y r)
		(run simulator 0d0 0d0)	; the first time
	      (assert (not (minusp score))) 
	      (let* ((direction (adjust-direction (calc-unit-tangent-vector (vec x y))
						  approximated-direction))
		     (r2 r)
		     (r1 (sqrt (+ (* x x) (* y y))))
		     (result (hohmann r1 r2)) 
		     (init-dV (vscale direction (first result)))
		     (final-dV (vscale direction (- (second result)))))
		(format t "~&Direction vector: ~a~%" direction)
		(format t "~&Result for Hohmann method: ~% - initial-dV = ~a, ~% - final-dV = ~a; ~% - estimated arrival time: ~a~%" init-dV final-dV (third result))
		;; initial impulse
		(format t "~&Give initial impulse: (~a, ~a)~%" (vx init-dV) (vy init-dV))
		(run simulator (vx init-dV) (vy init-dV))
		;; wait until reach perigee
		(iter (for output = (run simulator 0d0 0d0))
		      (when *trace-simulation-outputs*
			(destructuring-array-bind (nil nil x y r) output
			  (format t "~&Difference of radius to the target orbit: ~a~%"
				  (- r (sqrt (+ (* x x) (* y y)))))))
		      (until (done-p output)))
		;; final impulse
		(format t "~&Give final impulse: (~a, ~a)~%" (vx final-dV) (vy final-dV))
		(run simulator (vx final-dV) (vy final-dV))))))
      (error (c)
	(format t "~&Satellite failed with program error: ~a~%" c)
	(nreverse thrusts)))))

