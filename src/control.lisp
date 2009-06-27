(in-package :dysfunkycom)

(defparameter *trace-simulation-outputs* nil)
(defparameter *max-runs* 3000000)

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
    (values initial-delta-v
	    final-delta-v
	    estimated-time)))

(defun estimate-target-radius (sim)
  (let ((simulator (make-simple-simulator-func (copy-sim sim))))
    (let ((output (funcall simulator)))
      (destructuring-array-bind (nil nil x y tx ty)
	  output
	(d (- tx x) (- ty y))))))

(defun problem-1-target-radius (sim)
  (let ((simulator (make-simple-simulator-func (copy-sim sim))))
    (destructuring-array-bind (nil nil nil nil r)
	(funcall simulator)
      r)))

(defun position-and-direction (sim)
  (let ((simulator (make-simple-simulator-func (copy-sim sim))))
    (destructuring-array-bind (nil nil x0 y0)
	(funcall simulator)
      (destructuring-array-bind (nil nil x1 y1)
	  (funcall simulator)
	(values x0 y0 (- x1 x0) (- y1 y0))))))

(defun position-and-direction-target (sim &optional (target-offset 4))
  (let ((simulator (make-simple-simulator-func (copy-sim sim))))
    (flet ((pos ()
	    (let ((oport (funcall simulator)))
	      (destructuring-array-bind (nil nil x y)
					oport
					(values (- (elt oport target-offset) x) (- (elt oport (1+ target-offset)) y))))))
      (multiple-value-bind (x0 y0)
	  (pos)
	(multiple-value-bind (x1 y1)
	  (pos)
	  (values x0 y0 (- x1 x0) (- y1 y0)))))))

(defun problem-1-controller (sim &optional (r (problem-1-target-radius sim)))
  (labels ((boost (speed)
	     (let ((vec (vscale (multiple-value-bind (x y vx vy)
				    (position-and-direction sim)
				  (adjust-direction
				   (calc-unit-tangent-vector (vec x y))
				   (vec vx vy))) speed)))
	       (sim-step sim (vx vec) (vy vec))))
	   (done? ()
	     (destructuring-array-bind (score nil x y) 
		 (sim-step sim)
	       (assert (not (minusp score)))
	       (approximately-equal
		(d x y)
		r
		0.00001))))
    (multiple-value-bind (x y)
	(position-and-direction sim)
      (multiple-value-bind (init-dv end-dv)
	  (hohmann (d x y) r)
	(boost (- init-dv))
	(loop until (done?))
	(boost (- end-dv))
	(values (reverse (sim-thrusts sim)) (sim-time sim))))))

(defun problem-1-controller-burn (sim &key (r (problem-1-target-radius sim)) (fuel 9000))
  "This does not work

To see the earth disappear

 (visualise-scenario \"/home/john/Programs/dysfunkycom/orbit-code/bin1.obf\" 1004d0 :frames (let ((sim (make-simulator \"/home/john/Programs/dysfunkycom/orbit-code/bin1.obf\" 1004d0))) (problem-1-controller-burn sim)))
"
  (labels ((boost (speed &key (evx 0d0) (evy 0d0))
	     (let ((vec (vscale (multiple-value-bind (x y vx vy)
				    (position-and-direction sim)
				  (adjust-direction
				   (calc-unit-tangent-vector (vec x y))
				   (vec vx vy))) speed)))
	       (sim-step sim (+ (vx vec) evx) (+ (vy vec) evy))
	       (values (vx vec) (vy vec))))
	   (done? ()
	     (destructuring-array-bind (score nil x y) 
		 (sim-step sim)
	       (assert (not (minusp score)))
	       (approximately-equal
		(d x y)
		r
		0.0001))))
    (multiple-value-bind (x y)
	(position-and-direction sim)
      (multiple-value-bind (init-dv end-dv)
	  (hohmann (d x y) r)
	(let ((fuel-used (+ (abs init-dv) (abs end-dv))))
	  (let ((extra (/ (- fuel fuel-used) 2)))
	    (multiple-value-bind (vx vy)
		(boost (- (+ extra init-dv)))
	      (loop until (done?))
	      (flet ((s (x)
		       (/ (* -1 x (abs extra)) (abs (+ extra init-dv)))))
		(boost (- end-dv) :evx (s vx) :evy (s vy)))))
	  (values (reverse (sim-thrusts sim)) (sim-time sim)))))))
	

(defun estimate-real-orbital-period (sim)
  (let ((t0 (sim-time sim)))
      (multiple-value-bind (x0 y0)
	  (position-and-direction sim)
	(let ((a0 (angle x0 y0)))
	  (labels (
		   (norm (angle)
		     angle)
		   (before (x target)
		     (plusp (- x target)))
		   (wait-for-angle (target) 
		     (let ((tar (norm target)))
		       (loop 
			     for last = nil then angle
			     for angle = (destructuring-array-bind (score nil x y) 
								   (sim-step sim)
								   (assert (not (minusp score)))
								   (norm (angle x y)))
			     thereis (and last 
					  (not (eq (before last tar) (before angle tar))))))))
	    (wait-for-angle (+ a0 pi))
	    (wait-for-angle a0))
	  (- (sim-time sim) t0)))))

(defun problem-2-calc-jump (sim)
  (let ((target-radius (estimate-target-radius sim)))
    (let ((tmpsim (copy-sim sim)))
      (multiple-value-bind (x0 y0)
	  (position-and-direction tmpsim)
	(problem-1-controller tmpsim target-radius)
	(multiple-value-bind (x1 y1)
	    (position-and-direction tmpsim)
	  (values
	    (sim-time tmpsim)		; hohmann time
	    (angle x0 y0)		; initial angle
	    (angle x1 y1)		; final angle
	    nil ;; (estimate-real-orbital-period tmpsim)
	    target-radius		; target radius
	    ))))))

(defun problem-2-chaser (sim &key (range 500) (min-fuel 100))
  (let ((ax 0d0) (ay 0d0))
    (iter (for output = (sim-step sim ax ay))
	  (setf ax 0d0 ay 0d0)
	  (destructuring-array-bind (score fuel nil nil xo yo) output
				    (until (plusp score))
				    (let ((d (d xo yo)))
				      (unless (> range d)
					(setf ax (/ xo d range)
					      ay (/ yo d range)))
				      (cl-user::debug-state d xo yo ax ay)

				      (when (>= (+ min-fuel (d ax ay)) fuel)
					 (setf ax 0d0 ay 0d0)))))))

  (values (reverse (sim-thrusts sim)) (sim-time sim)))

(defun problem-2-controller (sim)
  (multiple-value-bind (x0 y0 vx0 vy0)
      (position-and-direction-target sim)
    (multiple-value-bind (hohmann-time init-angle end-angle _ target-radius)
	(problem-2-calc-jump sim)
      (declare (ignorable _))
      ;; 1. wait to the right position
      (let* ((radius (d x0 y0))
	     (angular-velocity (/ (norm (vec vx0 vy0)) radius))
	     (period (orbital-period radius)) ; TODO: check period and hohmann-time
	     (triggering-angle (normalize-angle
				(- (- end-angle init-angle)
				   (* angular-velocity hohmann-time)))))
	(iter (for output = (sim-step sim))
	      (destructuring-array-bind (nil nil x y xo yo) output
		(let ((angle-to-opponent
		       (normalize-angle
			(calc-angle-between-vectors
			 (vec x y)
			 (vec (- xo x) (- yo y))))))

		  (when (> 0.0001 (abs (- angle-to-opponent triggering-angle)))
		    (leave))))))

      ;; 2. hohmann
      (problem-1-controller sim target-radius)
      (problem-2-chaser sim)
      )))

;;; previous implementation
;;; not used now
(defun hohmann-controller (sim)
  (let ((thrusts '())
	(last-x 0d0)
	(last-y 0d0)
	(nruns 0)
	(simulator (make-simple-simulator-func sim)))
    (handler-case
	(labels ((run (simulator dVx dVy)
		   (when (>= nruns *max-runs*)
		     (error "Too many simulations!"))
		   (incf nruns)
		   (push (list dVx dVy) thrusts)
		   (let ((output (funcall simulator dVx dVy)))
		     (when *trace-simulation-outputs*
		       (describe-output-1 output))
		     (destructuring-array-bind (nil nil x y nil) output
		       (display-the-velocity x y))
		     output))
		 (done-p (output)
		   (destructuring-array-bind (score nil x y r) output 
		     (assert (not (minusp score)))
		     (approximately-equal
		      (sqrt (+ (* x x) (* y y)))
		      r
		      0.0000001)))	;TODO: check the epsilon
		 (display-the-velocity (x y)
		   (when *trace-simulation-outputs*
		     (format t "~&Velocity of last second: (~a, ~a)~%"
			     (- x last-x)
			     (- y last-y)))
		   (setf last-x x
			 last-y y)))
	  (let (approximated-direction)
	    ;; 1. estimate the direction
	    (let* ((init-output (run simulator 0d0 0d0))
		   (x1 (aref init-output 2))
		   (y1 (aref init-output 3)))
	      (setf last-x x1
		    last-y y1)
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
	      (let* ((r2 r)
		     (r1 (sqrt (+ (* x x) (* y y))))
		     (hohmann-result (multiple-value-list (hohmann r1 r2))))
		(format t "~&Result for Hohmann method: ~% - initial-dV-scalar = ~a, ~% - final-dV-scalar = ~a; ~% - estimated arrival time: ~a~%" (first hohmann-result) (second hohmann-result) (third hohmann-result))
		;; initial impulse
		(let* ((direction (adjust-direction
				   (calc-unit-tangent-vector (vec x y))
				   approximated-direction))
		       (init-dV (vscale direction (first hohmann-result))))
		  (format t "~&Current direction: ~a~%" direction)
		  (format t "~&Give initial impulse: (~a, ~a)~%" (vx init-dV) (vy init-dV))
		  (run simulator (- (vx init-dV)) (- (vy init-dV))))
		;; wait until the satellite reachs perigee
		(iter (for output = (run simulator 0d0 0d0))
		      (when *trace-simulation-outputs*
			(destructuring-array-bind (nil nil x y r) output
			  (format t "~&Difference of radius to the target orbit: ~a~%"
				  (- r (sqrt (+ (* x x) (* y y)))))))
		      (until (done-p output)))
		;; final impulse
		(destructuring-array-bind (nil nil x y nil) (run simulator 0d0 0d0)
		  (let* ((direction (adjust-direction
				     (calc-unit-tangent-vector (vec x y))
				     approximated-direction))
			 (final-dV (vscale direction (- (second hohmann-result)))))
		    (format t "~&Give final impulse: (~a, ~a)~%" (vx final-dV) (vy final-dV))
		    (run simulator (- (vx final-dV)) (- (vy final-dV)))))
		;; wait until simulator terminates
		(iter (for output = (run simulator 0d0 0d0))
		      (destructuring-array-bind (score nil nil nil nil) output
			(while (zerop score)))
		      (finally
		       (format t "~&Time spent: ~a seconds~%" (length thrusts))
		       (format t "~&The final score is: ~a~%" (aref output 0))))
		;; result
		(nreverse thrusts)))))
      (error (c)
	(format t "~&Time spent: ~a seconds~%" (length thrusts))
	(format t "~&Satellite failed with program error: ~a~%" c) 
	(nreverse thrusts)))))
