(in-package #:dysfunkycom)

(defun hohmann-time (r1 r2)
  (nth-value 2 (hohmann r1 r2)))

(defun problem-3-controller-brute (sim &key (target (sim-target sim)) (jumper #'controller-brute-jumper)
				   (end-condition (chaser-condition-non-changing-score sim)))
  (let ((time (funcall jumper sim :target target)))
    (loop repeat (- (round time) *chaser-lookahead*)
	  do (sim-step sim))
    (chaser-controller sim :target target :closing-condition end-condition)

    (values (reverse (sim-thrusts sim)) (sim-time sim))))

(defun problem-3-controller-touch (sim &key (target (sim-target sim)) (jumper #'controller-brute-jumper))
  (let* ((time (funcall jumper sim :target target)) (lookahead (/ time 5)))
    (sim-repeat-step sim (- (round time) lookahead))
    (chaser-controller-touch sim :target target :step lookahead)

    (values (reverse (sim-thrusts sim)) (sim-time sim))))

(defun problem-3-controller-suicide (sim)
  (chaser-controller sim)
  (values (reverse (sim-thrusts sim)) (sim-time sim)))

(defun controller-strange-ellipse-jumper-and-wait (sim &key (target (sim-target sim)))
  (let ((start-time (sim-time sim)))
    (destructuring-bind (target-radius wait)
	(estimate-target-radius-iteratively sim target)
      (push (list 0 0 target-radius) *show-orbits*)
      (iter (repeat wait) (sim-step sim))
      (problem-3-controller-jump sim target-radius)
      (- (sim-time sim) start-time))))

(defun controller-brute-jumper-and-touch (sim &key (target (sim-target sim)))
  (let* ((time (controller-brute-jumper sim :target target)) (lookahead *chaser-lookahead*))
    (sim-repeat-step sim (- (round time) lookahead))
    (chaser-controller-touch sim :target target :step lookahead)))

(defun brute-jump-attempt (sim r t0 time period  angle0 target w)
  (multiple-value-bind (ex ey)
      (sim-pos-at-time sim target (+ t0 time))
    (let ((target-r (d ex ey)))
     (multiple-value-bind (init-dv end-dv jump-time) 
	 (hohmann r target-r)
       (declare (ignore end-dv init-dv))
       (when (> time jump-time)
	 (let ((our-angle (+ angle0 (* (mod (- time jump-time) period) w))))
	   (values 
	    (normalize-vangle (- our-angle (angle ex ey) pi))
	    (- time jump-time)
	    target-r)))))))

(defun periodic-bisect-opt (function initial-period-estimate &key 
			    (initial-slice-width (/ initial-period-estimate 5000)) 
			    (max-val 0.5)
			    (epsilon 1))
  (labels (
	   (try (x)
	     (let ((val (funcall function x)))
	       val))
	   (initial-range ()
	     (let ((initial-slice 
		    (loop for i from initial-slice-width by initial-slice-width 
			  thereis (and (try i) i))))
	      (loop 
		    for last-val = nil then val
		    for last-real-val = nil then real-val
		    for last-x = nil then x
		    for x from initial-slice by initial-slice-width
		    for real-val = (try x)
		    for val = (signum real-val)
		    do
		    (when (zerop val)
		      (return (values val val x x)))
		    (when (and last-val (> max-val (abs real-val)) (> max-val (abs last-real-val)) (= (- val) last-val))
		      (return (values last-val val x last-x)))))))
    (multiple-value-bind (min-val max-val min max)
	(initial-range)
      (loop do 
	    (when (>= epsilon (abs (- min max)))
	      (return min))
	    (let* ((x (/ (+ min max) 2))
		   (attempt (try x)))
	      (cond ((zerop attempt)
		     (return x))
		    ((= (signum attempt) min-val)
		     (setf max x))
		    ((= (signum attempt) max-val)
		     (setf min x))))))))

(defun controller-brute-jumper (sim &key (target (sim-target sim)))
  (let* ((r (sat-r (sim-us sim))) (t0 (sim-time sim))
	 (angle0 (sat-angle (sim-us sim)))
	 (period (orbital-period r))
	(w (sat-circular-orbit-vangle (sim-us sim))))
    (labels ((attempt (time)
	       (brute-jump-attempt sim r t0 time period angle0 target w)))
      (let ((time (periodic-bisect-opt #'attempt period)))
	(multiple-value-bind (angle wait-time target-r)
	    (attempt time)
	  (declare (ignorable angle))
	  (loop repeat (floor wait-time) do (sim-step sim))
	  (controller-hohmann-jump-not-stopping sim target-r))))))
