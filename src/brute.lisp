(in-package #:dysfunkycom)

(defun hohmann-time (r1 r2)
  (nth-value 2 (hohmann r1 r2)))

(defun problem-3-controller-brute (sim &key (target (sim-target sim)) end-condition)
  (let* ((r (sat-r (sim-us sim))) (t0 (sim-time sim))
	(angle0 (sat-angle (sim-us sim)))
	(w (sat-circular-orbit-vangle (sim-us sim))))
    (destructuring-bind (wait-time target-r)
	(iter (for time from 0 below 10000 by 1)
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
			       minimizing init-dv))))))
      (loop until (>= (sim-time sim) (+ t0 wait-time))
	 do (sim-step sim))
      (problem-3-controller-jump sim target-r)
      
      (chaser-controller sim :closing-condition end-condition)

      (values (reverse (sim-thrusts sim)) (sim-time sim)))))

(defun problem-3-controller-suicide (sim)
  (chaser-controller sim)
  
  (values (reverse (sim-thrusts sim)) (sim-time sim)))
