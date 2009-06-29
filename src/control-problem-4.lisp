(in-package :dysfunkycom)

(defun find-sat-by-id (id sim)
  (find id (sim-sats sim) :test (lambda (name sat) (and (numberp (sat-name sat)) (= (sat-name sat) name)))))

(defun nearest-satellite (id-list sim)
  (let ((sats (mapcar (lambda (id) (find-sat-by-id id sim)) id-list))
	(us (sim-us sim)))
    (iter (for sat in sats)
	  (finding sat minimizing (sat-distance sat us)))))

(defun go-for-satellite (sat sim &optional (time 10) (max-thrust 1000))
  (multiple-value-bind (x y)
      (sim-pos-at-time sim (sim-us sim) (+ (sim-time sim) time))
    (multiple-value-bind (ex ey)
	(sim-pos-at-time sim sat (+ (sim-time sim) time))
      (let ((us (sim-us sim)))
	(if (< (d (- x ex) (- y ey)) (sat-distance sat us))
	    (iter (repeat time) (sim-step sim))
	    (let* ((x (sat-x us)) (y (sat-y us))
		   (vx (sat-vx us)) (vy (sat-vy us))
		   (dir (vec (- ex x vx) (- ey y vy))))
	      (when (> (norm dir) max-thrust)
		(setf dir (v* (normalize-vector dir) max-thrust)))
	      (sim-step sim (vx dir) (vy dir))
	      (iter (repeat (1- time)) (sim-step sim))))))))

(defun fuel-low-p (sim)
  'TODO
  nil)

(defun refuel-controller (sim)
  'TODO)

(defun problem-4-controller (sim)
  (iter (with satellites = '(0 1 2 3 4 5 6 7 8 9 10))
	(while satellites)
	(for nearest = (nearest-satellite satellites sim))
	(go-for-satellite nearest sim)
	(for prev-score first 0 then score)
	(for score = (sim-score sim))
	(while (>= score 0))
	(when (> score prev-score)
	  (setf satellites (remove nearest satellites)))
	(when (fuel-low-p sim)
	  (refuel-controller sim))
	(finally (return (values (reverse (sim-thrusts sim)) (sim-time sim))))))


(defun problem-4-controller-go-to-moon (sim)
  (problem-2-controller sim (sim-moon sim)))