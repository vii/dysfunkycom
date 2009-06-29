(in-package #:dysfunkycom)

(defun hohmann-time (r1 r2)
  (nth-value 2 (hohmann r1 r2)))

(defun problem-3-controller-brute (sim)
  (let* ((r (sat-r (sim-us sim))) (t0 (sim-time sim))
	(angle0 (sat-angle (sim-us sim)))
	(w (sat-circular-orbit-vangle (sim-us sim))))
    (destructuring-bind (wait-time target-r our-angle ex ey)
	(iter (for time from 0 below 200000 by 1)
	      (multiple-value-bind (ex ey)
		  (sim-pos-at-time sim (sim-target sim) (+ t0 time))
		(let ((jump-time (hohmann-time r (d ex ey))))
		  (when (plusp (- time jump-time))
		    (let ((our-angle (+ angle0 (* (- time jump-time) w))))
		      (finding (list (- time jump-time) (d ex ey) our-angle ex ey) 
			       minimizing (abs (normalize-vangle (- our-angle (angle ex ey) pi)))))))))
      (cl-user::debug-state w (orbital-speed r) (normalize-angle (sat-angle (sim-us sim))))
      (cl-user::debug-state wait-time target-r)
      (loop until (>= (sim-time sim) (+ t0 wait-time))
	    do (sim-step sim))
      (cl-user::debug-state (sim-time sim) (normalize-angle our-angle) (normalize-angle (sat-angle (sim-us sim))))
      (problem-3-controller-jump sim target-r)
      (cl-user::debug-state ex ey (sat-x (sim-target sim)) (sat-y (sim-target sim)))
      
      (chaser-controller sim)

      (values (reverse (sim-thrusts sim)) (sim-time sim)))))

(defun problem-3-controller-suicide (sim)
  (chaser-controller sim)
  
  (values (reverse (sim-thrusts sim)) (sim-time sim)))
