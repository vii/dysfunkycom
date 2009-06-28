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
