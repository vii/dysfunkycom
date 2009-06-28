(in-package #:dysfunkycom)

(defun problem-1-controller-superburn (sim &key (r (problem-1-target-radius sim)) (fuel 10000))
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
		 (sim-step sim (- (- tvx vx)) (- (- tvy vy))))))
	     (done? ()
	       (sim-step sim)
	       (let ((vx (sat-vx us)) (vy (sat-vy us))
		     (x1 (sat-x us))
		     (y1 (sat-y us)))
		 (or
		  (minusp (sat-vr us))
		  (not (zerop (sim-score sim))) 
		  (/= (signum (- (d x1 y1) r))
		      (signum (- (d (+ x1 (* 1 vx)) (+ y1 (* 1 vy))) r))))))
	     (boost (burn)
	       (let* ((burn (- burn)) (init-r (sat-r us)) (x (sat-x us)) (y (sat-y us)))
		 (sim-step sim (* burn (/ x init-r)) (* burn (/ y init-r)))
		 (loop until (done?))
		 (stabilise)
		 (sim-step sim)
		 (sim-step sim)
		 (stabilise)))
	     (attempt (burn)
	       (let ((old sim))
		 (setf sim (copy-sim old))

		 (boost burn)
		 (when (approximately-equal (sat-r us) r)
		   (loop repeat 20000 
			 do (sim-step sim) 
			 until (not (zerop (sim-score sim)))))

		 (prog1 (sim-score sim) 
		   (setf sim old)))))
      (let ((min (/ fuel 2)) (max fuel))
	(boost (iter (for i below 20)
		     (for burn = (/ (+ max min) 2))
		     (for result = (attempt burn))
		     (if (= -1 result) 
			 (setf max burn)
			 (setf min burn))
		     (finding burn maximizing result))))))

  (values (reverse (sim-thrusts sim)) (sim-time sim)))
