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

(defun estimate-target-radius (sim &optional (target (sim-target sim)))
  (sat-r target))

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
		 (values (- x (elt oport target-offset))
			 (- y (elt oport (1+ target-offset))))))))
      (multiple-value-bind (x0 y0)
	  (pos)
	(multiple-value-bind (x1 y1)
	    (pos) 
	  (values x0 y0 (- x1 x0) (- y1 y0)))))))

(defun problem-1-controller (sim &optional (r (problem-1-target-radius sim)))
  (push (list 0 0 r) *show-orbits*)
  (push (list 0 0 (sat-r (sim-us sim))) *show-orbits*)
  (multiple-value-bind (x y)
      (position-and-direction sim)
    (labels ((boost (speed)
	       (let ((vec (vscale (multiple-value-bind (x y vx vy)
				      (position-and-direction sim)
				    (adjust-direction
				     (calc-unit-tangent-vector (vec x y))
				     (vec vx vy))) speed)))
		 (sim-step sim (vx vec) (vy vec))))
	     #+ignore
	     (done? ()
	       (destructuring-array-bind (score nil x1 y1) 
		   (sim-step sim)
		 (assert (zerop score))
		 (let ((vx (- x1 x)) (vy (- y1 y)))
		   (prog1 (/= (signum (- (d x1 y1) r))
			      (signum (- (d (+ x1 (* 1 vx)) (+ y1 (* 1 vy))) r)))
		     (setf x x1 y y1))))))
      (multiple-value-bind (init-dv end-dv estimated-time)
	  (hohmann (d x y) r)
	(declare (ignorable end-dv))
	(boost (- init-dv))
	(loop repeat (- (round estimated-time) 2)
	      for output = (sim-step sim)
	      ;; until (done?)
	      finally (return output))
	(multiple-value-bind (_ __ vx vy)
	    (position-and-direction sim)
	  (declare (ignorable _ __))
	  (boost (- (d vx vy) (sqrt (/ +g-m-earth+ r))))) 
	(values (reverse (sim-thrusts sim)) (sim-time sim))))))

(defun controller-hohmann-jump-not-stopping (sim r)
  (let ((x (sat-x (sim-us sim))) (y (sat-y (sim-us sim))))
    (labels ((boost (speed)
	       (let ((vec (vscale (multiple-value-bind (x y vx vy)
				      (position-and-direction sim)
				    (adjust-direction
				     (calc-unit-tangent-vector (vec x y))
				     (vec vx vy))) speed)))
		 (sim-step sim (vx vec) (vy vec))))
	     #+ignore
	     (done? ()
	       (destructuring-array-bind (score nil x1 y1) 
		   (sim-step sim)
		 (assert (zerop score))
		 (let ((vx (- x1 x)) (vy (- y1 y)))
		   (prog1 (/= (signum (- (d x1 y1) r))
			      (signum (- (d (+ x1 (* 1 vx)) (+ y1 (* 1 vy))) r)))
		     (setf x x1 y y1))))))
      (multiple-value-bind (init-dv end-dv estimated-time)
	  (hohmann (d x y) r)
	(declare (ignorable end-dv))
	(boost (- init-dv))
	estimated-time))))

(defun problem-3-controller-jump (sim r)
  (let ((estimated-time (controller-hohmann-jump-not-stopping sim r)))
    (sim-repeat-step sim (- (round estimated-time) *chaser-lookahead*))
    (values (reverse (sim-thrusts sim)) (sim-time sim))))
	

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

(defun problem-2-calc-jump (sim target)
  (let ((target-radius (estimate-target-radius sim target))
	(tmpsim (copy-sim sim)))
    (multiple-value-bind (x0 y0)
	(let* ((us (sim-us tmpsim)))
	  (values (sat-x us) (sat-y us)))
      (problem-1-controller tmpsim target-radius)
      (multiple-value-bind (x1 y1)
	  (let* ((us (sim-us tmpsim)))
	    (values (sat-x us) (sat-y us)))
	(values
	  (sim-time tmpsim)   ; hohmann time
	  (angle x0 y0)	      ; initial angle
	  (angle x1 y1)	      ; final angle
	  nil		      ;; (estimate-real-orbital-period tmpsim)
	  target-radius	      ; target radius
	  )))))

(defun estimate-our-states-using-sim (sim steps)
  (let ((sim (copy-sim sim)))
    (sim-repeat-step sim steps)
    (position-and-direction sim)))

(defun estimate-target-states-using-sim (sim steps)
  (let ((sim (copy-sim sim)))
    (sim-repeat-step sim steps)
    (position-and-direction-target sim)))

(defun problem-2-chaser (sim &optional (catch-up-time 100))
  (multiple-value-bind (xte0 yte0)
      (estimate-target-states-using-sim sim catch-up-time)
    (multiple-value-bind (xe0 ye0)
	(estimate-our-states-using-sim sim catch-up-time)
      (let* ((distance-vec (vec (- xte0 xe0) (- yte0 ye0)))
	     (distance (norm distance-vec))
	     (direction (normalize-vector distance-vec))
	     (a (/ (* 2 distance) (^2 catch-up-time)))
	     (accel-vec (vscale direction a)))
	(format t "~&Time: ~a~%" (sim-time sim))
	(format t "Distance (before chasing): ~a~%" distance)
	(sim-step sim (- (vx accel-vec)) (- (vy accel-vec)))
	(loop repeat (1- catch-up-time)
	      do (sim-step sim 0d0 0d0))
	(multiple-value-bind (x y vx vy)
	    (position-and-direction sim)
	  (multiple-value-bind (xt yt)
	      (position-and-direction-target sim)
	    (format t "Position estimation error: ~A~%" (v- (vec x y) (vec xe0 ye0)))
	    (let* ((direction (normalize-vector (vec vx vy)))
		   (dir-wanted (adjust-direction (calc-unit-tangent-vector (vec x y))
						 direction))
		   (v-wanted (sqrt (/ +g-m-earth+ (d x y))))
		   (dv (v- (vscale dir-wanted v-wanted) (vec vx vy))))
	      (progn
		(sim-step sim (- (vx dv)) (- (vy dv)))
		(format t "Distance remained: ~A~%" (d (- xt x) (- yt y)))
		(d (- xt x) (- yt y))))))))))

(defun problem-2-controller (sim &optional (target (sim-target sim)))
  (multiple-value-bind (x0 y0 vx0 vy0)
      (let* ((target (elt (sim-sats sim) 1)))
	(values (sat-x target) (sat-y target)
		(sat-vx target) (sat-vy target)))
    (multiple-value-bind (hohmann-time init-angle end-angle _ target-radius)
	(problem-2-calc-jump sim target) 
      (declare (ignorable _))
      ;; 1. wait to the right position
      (let* ((radius (d x0 y0))
	     (angular-velocity (/ (norm (vec vx0 vy0)) radius))
	     (triggering-angle 
	      (normalize-angle
	       (+
		(* angular-velocity hohmann-time)
		(- end-angle init-angle)
		))))
	(iter (for output = (sim-step sim))
	      (let* ((x (sat-x (sim-us sim)))
		     (y (sat-y (sim-us sim)))
		     (xo (sat-x target))
		     (yo (sat-y target)) 
		     (angle-to-opponent
		      (normalize-angle 
		       (calc-angle-between-vectors
			(vec x y)
			(vec xo yo)))))
		(when (approximately-equal angle-to-opponent
					   triggering-angle
					   0.001)
		  (leave)))))

      (print 'leave)

      ;; 2. hohmann
      (let ((estimated-time (controller-hohmann-jump-not-stopping sim target-radius)))
	(sim-repeat-step sim (- (round estimated-time) *chaser-lookahead*))

	(chaser-controller sim :target target)

	(values (reverse (sim-thrusts sim)) (sim-time sim))))))

(defun enemy-semi-major-axis (sim target)
  (let ((points (iter (for time below 1000)
		      (collect (enemy-position-later sim target time )))))
    (destructuring-bind (center a b phi)
	(ellipse-from-quadratic (estimate-ellipse points))
      (declare (ignore center phi))
      (max a b))))

(defun enemy-period (sim target)
  (handler-case (nth-value 2 (estimate-apogee-and-period sim target))
    (error () 
      (orbital-period (enemy-semi-major-axis (copy-sim sim) target)))))

(defun enemy-position-later (sim target n)
  (mapcar '- (multiple-value-list (sim-pos-at-time sim target (+ (sim-time sim) (floor n))))))

(defun full-hohmann-time (x0 y0 e-x e-y sim)
  (list (iter (for output = (sim-step sim))
	      (for x = (aref output 2)) (for y = (aref output 3))
	      (for angle = (- (normalize-angle (calc-angle-between-vectors (vec x y) (vec e-x e-y))) pi))
	      (repeat (ceiling (orbital-period (d x0 y0))))
	      (finding (sim-time sim) maximizing (abs angle)))
	(multiple-value-bind (dv1 dv2 secs) (hohmann (d x0 y0) (d e-x e-y))
	  (declare (ignore dv1 dv2))
	  secs)))

(defun estimate-target-radius-iteratively (sim target &optional (iterations 20) (max-periods 50))
  (prog1 (iter (with (x y) = (destructuring-array-bind (nil nil x y)
				 (funcall (make-simple-simulator-func (copy-sim sim)))
			       (list x y)))
	       (with enemy-period = (enemy-period sim target))
	       (with our-period = (orbital-period (d x y)))
	       (for i from iterations above 0)
	       (format t "~d..." i)
	       (for time upfrom 1 by (/ enemy-period (1+ iterations)))
	       (for (e-x e-y) = (enemy-position-later sim target time)) 
	       (for (wait-time hohmann-time) = (full-hohmann-time x y e-x e-y (copy-sim sim)))
	       (for full-wait-time = (iter (for period from 0 to max-periods)
					   (for deviation upfrom (- time wait-time hohmann-time) by enemy-period)
					   (finding (- (+ time (* period enemy-period)) hohmann-time)
						    minimizing (mod deviation our-period))))
	       (finding (list (d e-x e-y) full-wait-time) minimizing full-wait-time))
    (terpri)))

;;; New idea:
;;; - calculate the time steps ti when the enemy will be at P = enemy(t0) [t0, t0+enemy_period, ...]
;;; - calculate the time steps Ti when I will be at the position from where I can jump to P
;;; - select the one where |ti - Ti| is minimal, trying with different t0s
;;; - go there (initial thrust by Hohmann)
;;; - stay on orbit (thrust such that our velocity matches the enemy's)

(defun problem-3-controller (sim &optional (target (sim-target sim)))
  (destructuring-bind (target-radius wait) 
      (estimate-target-radius-iteratively sim target)
    (push (list 0 0 target-radius) *show-orbits*)
    (sim-repeat-step sim wait)
    ;; TODO: get on ellipse orbit instead
    (problem-3-controller-jump sim target-radius)

    (chaser-controller sim)

    (values (reverse (sim-thrusts sim)) (sim-time sim))))


(defun controller-ellipse-jumper (sim &key (target (sim-target sim)) (iterations 1))
  (destructuring-bind (target-radius wait) 
      (estimate-target-radius-iteratively sim target iterations)
    (push (list 0 0 target-radius) *show-orbits*)
    (sim-repeat-step sim wait)

    (controller-hohmann-jump-not-stopping sim target-radius)))


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


#+ignore
(defun circular-orbit-appraoching-method-controller (sim &key (seconds-between-impulse 10))
  (labels ((run ()
	     (multiple-value-bind (xt0 yt0 vxt0 vyt0)
		 (position-and-direction-target sim)
	       (multiple-value-bind (x0 y0 vx0 vy0)
		   (position-and-direction sim)
		 (let* ((r (d x0 y0))
			(rt (d xt0 yt0))
			(dr (- rt r))
			(d-theta (calc-angle-between-vectors (vec x0 y0) (vec xt0 yt0)))
			(d-omega (- (/ (norm (vec vx0 vy0)) r)
				    (/ (norm (vec vxt0 vyt0)) rt)))
			(catch-up-time (/ d-theta d-omega)))
		   (declare (ignorable r rt dr d-theta d-omega catch-up-time))
		   ;; TODO: think about cases that d-theta > pi, and r > rt
		   (multiple-value-bind (xte0 yte0 vxte0 vyte0)
		       (estimate-satellite-states xt0 yt0 vxt0 vyt0 0d0 0d0 seconds-between-impulse)
		     (let* ((direction (normalize-vector (v- (vec xte0 yte0) (vec x0 y0))))
			    (distance (d (- xt0 x0) (- yt0 y0))) 
			    (delta-V (d (- vxt0 vx0) (- vyt0 vy0)))
			    (k1 1d0)
			    (k2 1d0)
			    (a 1d0)
			    (b 1d0)
			    (g_v (* k1 (if (> delta-V 0d0) (sqrt delta-V) (- (sqrt delta-V)))))
			    (t-esti (* 2d0 (/ delta-V g_v)))
			    (f_dis (* k2 (sqrt (/ distance t-esti))))
			    (accel-wanted (vscale direction (+ (* a g_v) (* b f_dis)))) 
			    (g (vscale (normalize-vector (vec (- x0) (- y0)))
				       (/ +g-m-earth+ (^2 r))))
			    (accel-to-apply (v- accel-wanted g)))
		       (assert (>= t-esti 0d0))
		       (format t "~&~%Simulation Time: ~a~{~&~a : ~a~}~%"
			       (sim-time sim)
			       (list :satellite-pos (vec x0 y0)
				     :target-pos (vec xt0 yt0)
				     :satellite-speed (vec vx0 vy0)
				     :target-speed (vec vxt0 vyt0)
				     :estimated-next-target-position (vec xte0 yte0)
				     :estimated-next-target-speed (vec vxte0 vyte0)
				     :direction direction
				     :distance distance
				     :delta-V delta-V
				     :g_v g_v
				     :t-esti t-esti
				     :f_dis f_dis
				     :accel-wanted accel-wanted
				     :g g
				     :accel-to-apply accel-to-apply
				     )) 
		       (sim-step sim (vx accel-to-apply) (vy accel-to-apply))
		       (loop repeat (1- seconds-between-impulse)
			     while (zerop (sim-score sim))
			     do (sim-step sim 0d0 0d0)))))))))
    (loop do (run)
	  until (not (zerop (sim-score sim))))
    (values (reverse (sim-thrusts sim)) (sim-time sim))))


(defun controller-stabilise-to-circular-orbit (sim &key no-wait)
  (sim-check sim)
  (symbol-macrolet ((us (sim-us sim)))
    (labels (
	     (scale (speed)
	     (let ((vec (vscale (multiple-value-bind (x y vx vy)
				    (position-and-direction sim)
				  (adjust-direction
				   (calc-unit-tangent-vector (vec x y))
				     (vec vx vy))) speed)))
	       (values (vx vec) (vy vec))))
	     (stabilise ()
	       (let ((vx (sat-vx us))
		   (vy (sat-vy us)))
		 (multiple-value-bind (tvx tvy)
		     (scale (orbital-speed (sat-r us)))
		   (sim-step sim (- (- tvx vx)) (- (- tvy vy)))))))
      (stabilise)))
  (unless no-wait (sim-repeat-step sim 10)))

(defun controller-wait-and-stabilise-to-circular-orbit (sim &key (step 100) (scale 10d0))
  (sim-check sim)
  (controller-stabilise-to-circular-orbit sim)
  #+ignore
  (symbol-macrolet ((us (sim-us sim)))
    (labels (
	     (scale (speed)
		    (let ((vec (vscale (multiple-value-bind (x y vx vy)
					   (position-and-direction sim)
					 (adjust-direction
					  (calc-unit-tangent-vector (vec x y))
					  (vec vx vy))) speed)))
		      (values (vx vec) (vy vec))))
	     (stabilise-calc ()
			     (let ((vx (sat-vx us))
				   (vy (sat-vy us)))
			       (multiple-value-bind (tvx tvy)
				   (scale (orbital-speed (sat-r us)))
				 (values (- (- tvx vx)) (- (- tvy vy))))))
	     (stabilise ()
			(multiple-value-bind (ax ay)
			    (stabilise-calc)
			  (sim-step sim (/ ax scale) (/ ay scale)))))
      (sim-repeat-step sim step)
      (loop repeat (round scale)
	    do (stabilise)
	    (sim-repeat-step sim step))
      (controller-stabilise-to-circular-orbit sim)
      (sim-repeat-step sim step))))

(defun stablize-to-circular-orbit (sim)
  (sim-check sim)
  (multiple-value-bind (x y vx vy)
      (let ((sim (copy-sim sim)))
	(sim-step sim 0d0 0d0)
	(let ((us (sim-us sim)))
	  (values (sat-x us) (sat-y us) (sat-vx us) (sat-vy us))))
    (let* ((direction (normalize-vector (vec vx vy)))
	   (dir-wanted (adjust-direction (calc-unit-tangent-vector (vec x y))
					 direction))
	   (v-wanted (sqrt (/ +g-m-earth+ (d x y))))
	   (dv (v- (vscale dir-wanted v-wanted) (vec vx vy))))
      ;; stablize
      (progn
	(sim-step sim (- (vx dv)) (- (vy dv)))))))
