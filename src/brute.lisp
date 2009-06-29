(in-package #:dysfunkycom)

(defun hohmann-time (r1 r2)
  (nth-value 2 (hohmann r1 r2)))

(defun problem-3-controller-brute (sim &key (target (sim-target sim)) 
				   (end-condition #'chaser-condition-non-zero-score))
  (let ((time (controller-brute-jumper sim :target target :end-condition #'chaser-condition-non-zero-score)))
    (loop repeat (- (round time) 900)
	  do (sim-step sim))
    (chaser-controller sim :closing-condition end-condition)

    (values (reverse (sim-thrusts sim)) (sim-time sim))))

(defun problem-3-controller-suicide (sim)
  (chaser-controller sim)
  
  (values (reverse (sim-thrusts sim)) (sim-time sim)))


(defun controller-brute-jumper (sim &key (target (sim-target sim)))
  (let* ((r (sat-r (sim-us sim))) (t0 (sim-time sim))
	(angle0 (sat-angle (sim-us sim)))
	(w (sat-circular-orbit-vangle (sim-us sim))))
    (destructuring-bind (wait-time target-r)
	(iter (for last-time first 0 then time-base)
	      (for time-base first 1000 then (* time-base 2))
	      (for result =
		   (iter (for time from last-time below time-base by 1)
			 (multiple-value-bind (ex ey)
			     (sim-pos-at-time sim target (+ t0 time))
			   (multiple-value-bind (init-dv end-dv jump-time) 
			       (hohmann r (d ex ey))
			     (declare (ignore end-dv))
			     (let ((our-angle (+ angle0 (* (- time jump-time) w))))
			       (when (and (plusp (- time jump-time)) 
					  (> 0.01
					     (abs (normalize-vangle (- our-angle (angle ex ey) pi)))))
				 (finding (list (- time jump-time) (d ex ey)) 
					  minimizing init-dv)))))))
	      (while (null result))
	      (finally (return result)))
      (loop until (>= (sim-time sim) (+ t0 wait-time))
	 do (sim-step sim))
      (controller-hohmann-jump-not-stopping sim target-r))))
