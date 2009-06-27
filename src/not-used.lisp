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

