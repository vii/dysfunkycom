(in-package :dysfunkycom)

;;; hohmann
(let* ((init-output (funcall simulator 0d0 0d0))
       (x1 (aref init-output 2))
       (y1 (aref init-output 3)))
  (setf approximated-direction
	(iter (for output = (funcall simulator 0d0 0d0))
	      (for x2 = (aref output 2))
	      (for y2 = (aref output 3))
	      (while (and (approximately-equal x1 x2)
			  (approximately-equal y1 y2)))
	      (finally
	       (return (vec (- x2 x1) (- y2 y1)))))))

(let ((first-time-p t)
      (dVx 0d0)
      (dVy 0d0))
  (iter (for output = (coerce (funcall simulator dVx dVy) 'list))
	(setf dVx 0d0 dVy 0d0)		; clear the inputs
	(for (score fuel x y r) = output)
	(until (done-p output))
	;; calculate inputs for actuator
	(when first-time-p
	  (let* ((r2 r)
		 (r1 (sqrt (+ (* x x) (* y y))))
		 (result (hohmann r1 r2)))
	    (apply #'format t "~&Result for Hohmann method: dV1 = ~a, dV2 = ~a; estimated arrival time: ~a~%" result)
	    (setf dVx 
		  dVy )
	    ))
	;; (when (reach-the-target-orbit)
	;; 	      (setf dVx ...
	;; 		    dVy ...))
	;; step states
	(setf first-time-p nil)
	(finally (return 'done))))

(in-package :dysfunkycom)

(defun meet-and-greet-controller (simulator)
  (let ((thrusts '()) 
	(nruns 0))
    (handler-case
	(labels ((run (simulator dVx dVy)
		   (when (>= nruns *max-runs*)
		     (error "Too many simulations!"))
		   (incf nruns)
		   (push (list dVx dVy) thrusts)
		   (let ((output (funcall simulator dVx dVy)))
		     (when *trace-simulation-outputs*
		       (describe-output-1 output)) 
		     output))
		 (done-p (output)
		   (destructuring-array-bind (score nil x y xo yo) output 
		     (assert (not (minusp score)))
		     (approximately-equal (d x y)
					  (d xo yo)
					  0.0000001))) ;TODO: tune epsilon
		 )
	  (let (approximated-direction
		approximated-direction-opponent
		latest-x
		latest-y
		velocity)
	    ;; 1. estimations
	    (let* ((init-output (run simulator 0d0 0d0))) 
	      (destructuring-array-bind (nil nil x1 y1 xo1 yo1) init-output
		(multiple-value-setq
		    (approximated-direction approximated-direction-opponent)
		  (iter (for output = (run simulator 0d0 0d0)) 
			(for x2 = (aref output 2))
			(for y2 = (aref output 3))
			(progn
			  (when (iterate::first-time-p)
			    (setf velocity
				  (vec (- x2 x1) (- y2 y1))))
			  (setf latest-x x2
				latest-y y2))
			(for xo2 = (aref output 4))
			(for yo2 = (aref output 5))
			;; TODO: tune epsilons
			(while (or (and (approximately-equal x1 x2)
					(approximately-equal y1 y2))
				   (and (approximately-equal xo1 xo2)
					(approximately-equal yo1 yo2))))
			(finally
			 (return (values (vec (- x2 x1) (- y2 y1))
					 (vec (- xo2 xo1) (- yo2 yo1)))))))))
	    ;; wait for the opponent to reach the right position
	    (let* (time-to-reach-opponents-orbit ;FIXME:
		   angle-when-reaches-opponents-orbit
		   (angular-velocity (/ (norm velocity) (d latest-x latest-y)))
		   (period (/ (* 2 pi) angular-velocity))
		   (triggering-angle (- angle-when-reaches-opponents-orbit
					(* angular-velocity time-to-reach-opponents-orbit))))
	      (iter (for output = (run simulator 0d0 0d0))
		    (destructuring-array-bind (nil nil x y xo yo) output
		      (let ((angle-to-opponent
			     (calc-angle-between-vectors
			      (vec x y)
			      (vec xo yo))))
			(when (approximately-equal angle-to-opponent
						   triggering-angle
						   0.00000001) ;TODO: tune epsilon
			  (leave))))))
	    ;; 3. run the hohmann method
	    (destructuring-array-bind (score nil x y xo yo)
		(run simulator 0d0 0d0)	; the first time
	      (let* ((r2 (d xo yo))
		     (r1 (sqrt (+ (* x x) (* y y))))
		     (hohmann-result (hohmann r1 r2)))
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
			(destructuring-array-bind (nil nil x y xo yo) output
			  (format t "~&Difference of radius to the target orbit: ~a~%"
				  (- (d xo yo) (d x y)))))
		      (until (done-p output)))
		;; final impulse
		(destructuring-array-bind (nil nil x y nil nil)
		    (run simulator 0d0 0d0)
		  (let* ((direction (adjust-direction
				     (calc-unit-tangent-vector (vec x y))
				     approximated-direction))
			 (final-dV (vscale direction (- (second hohmann-result)))))
		    (format t "~&Give final impulse: (~a, ~a)~%" (vx final-dV) (vy final-dV))
		    (run simulator (- (vx final-dV)) (- (vy final-dV)))))
		;; wait until simulator terminates
		(iter (for output = (run simulator 0d0 0d0))
		      (destructuring-array-bind (score nil nil nil nil nil) output
			(while (zerop score)))
		      (finally
		       (format t "~&Time spent: ~a seconds~%" (length thrusts))
		       (format t "~&The final score is: ~a~%" (aref output 0))))
		;; result
		(nreverse thrusts)))))
      (error (c)
	(format t "~&Satellite failed with program error: ~a~%" c)
	(format t "~&Time spent: ~a seconds~%" (length thrusts))
	(nreverse thrusts)))))


(defun problem-2-chaser (sim)
  (labels ((run ()
	     (multiple-value-bind (xt0 yt0 vxt0 vyt0)
		 (position-and-direction-target sim)
	       (multiple-value-bind (x0 y0 vx0 vy0)
		   (position-and-direction sim)
		 (let* ((r (d x0 y0))
			(direction (normalize-vector (v- (vec xt0 yt0) (vec x0 y0))))
			(distance (d (- xt0 x0) (- yt0 y0)))
			(accel-wanted (vscale direction (/ distance 100000d0))) ; TODO: tune
			(g (vscale (normalize-vector (vec (- x0) (- y0)))
				   (/ +g-m-earth+ (^2 r))))
			(accel-to-apply (v- accel-wanted g)))
		   ;; (print accel-to-apply)
		   ;; (print (list (list x0 y0) (list xt0 yt0) distance accel-to-apply))
		   (print (list distance accel-to-apply))
		   #+ignore
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
				 :g g
				 :accel-wanted accel-wanted
				 :accel-to-apply accel-to-apply
				 )) 
		   (sim-step sim (- (vx accel-to-apply)) (- (vy accel-to-apply)))
		   )))))
    (loop do (run)
	  until (not (zerop (sim-score sim))))))

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

(defun return-back-to-fuelstation-controller (sim)
  (let* ((fuel-station (sim-fuelstation sim))
	 (r-fuel-station (d (sat-sx fuel-station) (sat-sy fuel-station)))
	 (us (sim-us sim))
	 (r-us (d (sat-sx us) (sat-sy us))))
    (controller-stabilise-to-circular-orbit sim)
    (problem-1-controller sim (/ (+ r-us r-fuel-station) 2d0))
    (controller-stabilise-to-circular-orbit sim)
    (problem-2-controller sim fuel-station)
    (controller-stabilise-to-circular-orbit sim)))

(defun give-initial-states (sim var-name steps)
  (set var-name
       (loop repeat steps
	     collect (sim-sats sim)
	     do (sim-step sim))))


(defun 4001-controller (sim)
  (let* ((sats (sort (sim-sats sim) #'< :key 'sat-r)))
    (iter (for i from 3 below 4)
	  (for s = (elt sats i))
	  (problem-2-controller sim s)	; won't work
	  (stablize-to-circular-orbit sim))))

(defun problem-4-controller-huang (sim)
  (iter (with satellites = '(0 1 2 3 4 5 6 7 8 9 10))
	(while satellites)
	(for nearest = (nearest-satellite satellites sim))
	(go-for-satellite nearest sim)
	(when (<= (length satellites) (1+ (- 11 *get-only*)))
	  (controller-stabilise-to-circular-orbit sim)
	  (leave))
	(refuel-controller sim)
	(for prev-score first 0 then score)
	(for score = (sim-score sim))
	(if (sat-done nearest)
	    (progn (tformat sim "Meeting was successful.~%")
		   (setf satellites (remove (sat-name nearest) satellites)))
	    (tformat sim "Meeting was NOT successful.~%"))
	(while (>= score 0))
	(when (fuel-low-p sim)
	  (refuel-controller sim))
	(while (>= score 0))
	(finally (return (values (reverse (sim-thrusts sim)) (sim-time sim))))))

