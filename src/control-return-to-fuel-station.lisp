(in-package :dysfunkycom)

(defun stablize-to-circular-orbit (sim)
  (multiple-value-bind (x y vx vy)
      (let ((sim (copy-sim sim)))
	(sim-step sim 0d0 0d0)
	(position-and-direction sim))
    (multiple-value-bind (xt yt)
	(let ((sim (copy-sim sim)))
	  (sim-step sim 0d0 0d0)
	  (position-and-direction-target sim))
      (let* ((direction (normalize-vector (vec vx vy)))
	     (dir-wanted (adjust-direction (calc-unit-tangent-vector (vec x y))
					   direction))
	     (v-wanted (sqrt (/ +g-m-earth+ (d x y))))
	     (dv (v- (vscale dir-wanted v-wanted) (vec vx vy))))
	;; stablize
	(progn
	  (sim-step sim (- (vx dv)) (- (vy dv))))))))

(defun problem-2-controller-pr-4 (sim &optional (target (sim-target sim)))
  (multiple-value-bind (x0 y0 vx0 vy0)
      (position-and-direction-target sim)
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
	      (destructuring-array-bind (nil nil x y xo yo) output
		(let ((angle-to-opponent
		       (normalize-angle 
			(calc-angle-between-vectors
			 (vec x y)
			 (vec (- x xo) (- y yo))))))
		  (when (approximately-equal angle-to-opponent
					     triggering-angle
					     0.001)
		    (leave))))))

      (print 'leave)

      ;; 2. hohmann

      (problem-1-controller sim target-radius)

      ;; 3. feedback loop for adjustment
      (loop for dis = (problem-2-chaser sim)
	    until (< dis 300d0))


      ;; return val
      (values (reverse (sim-thrusts sim)) (sim-time sim)))))

(defun return-back-to-fuelstation-controller (sim)
  (let* ((fuel-station (sim-fuelstation sim))
	 (r-fuel-station (d (sat-x fuel-station) (sat-y fuel-station)))
	 (us (sim-us sim))
	 (r-us (d (sat-x us) (sat-y us))))
    (print (list r-us r-fuel-station))
    (print 'stablize)
    (controller-stabilise-to-circular-orbit sim)
    (print 'return-to-half-radius-between-fuelstation-and-us)
    (problem-1-controller sim (/ (+ r-us r-fuel-station) 2d0))
    (print 'stablize)
    (controller-stabilise-to-circular-orbit sim)
    (print 'return-to-fuel-station)
    (problem-2-controller-pr-4 sim fuel-station)
    (print 'stablize)
    (controller-stabilise-to-circular-orbit sim)))

(defun test-return-back-to-fuelstation-controller (sim)
  (let* ((us (sim-us sim))
	 (r-us (d (sat-x us) (sat-y us))))
    (print r-us)
    (print 'leave)
    (problem-1-controller sim (* r-us 2d0))
    (print 'return)
    (return-back-to-fuelstation-controller sim)))
