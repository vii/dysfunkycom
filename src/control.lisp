(in-package :dysfunkycom)

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
  (labels ((done-p (output)
	     (destructuring-bind (score fuel x y r) output
	       (declare (ignorable score fuel))
	       (approximately-equal
		(sqrt (+ (* x x) (* y y)))
		r
		0.000001))))		; TODO: check the epsilon
    (let (direction)
      ;; 1. estimate the direction
      (let* ((init-output (funcall simulator 0d0 0d0))
	     (x1 (aref init-output 2))
	     (y1 (aref init-output 3))
	     (approximated-direction
	      (iter (for output = (funcall simulator 0d0 0d0))
		    (for x2 = (aref output 2))
		    (for y2 = (aref output 3))
		    ;; TODO: check epsilons
		    (while (and (approximately-equal x1 x2)
				(approximately-equal y1 y2)))
		    (finally
		     (return (vec (- x2 x1) (- y2 y1))))))) 
	(setf direction
	      (adjust-direction (calc-unit-tangent-vector (vec x1 y1))
				approximated-direction)))
      ;; 2. run the hohmann method
      (destructuring-bind (score fuel x y r)
	  (funcall simulator 0d0 0d0)	; the first time
	(declare (ignorable score fuel))
	(let* ((r2 r)
	       (r1 (sqrt (+ (* x x) (* y y))))
	       (result (hohmann r1 r2)) 
	       (init-dV (vscale direction (first result)))
	       (final-dV (vscale direction (- (second result)))))
	  (apply #'format t "~&Result for Hohmann method: dV1 = ~a, dV2 = ~a; estimated arrival time: ~a~%" result)
	  ;; initial impulse
	  (funcall simulator (vx init-dV) (vy init-dV))
	  ;; wait until reach perigee
	  (iter (for output = (coerce (funcall simulator 0d0 0d0) 'list))
		(until (done-p output)))
	  ;; final impulse
	  (funcall simulator (vx final-dV) (vy final-dV))
	  'done)))))
