(in-package #:dysfunkycom)

(defun find-sat-by-id (id sim)
  (find id (sim-sats sim) :test (lambda (name sat) (and (numberp (sat-name sat)) (= (sat-name sat) name)))))

(defun nearest-satellite (id-list sim)
  (let ((sats (mapcar (lambda (id) (find-sat-by-id id sim)) id-list))
	(us (sim-us sim)))
    (iter (for sat in sats)
	  (finding sat minimizing (sat-distance sat us)))))

(defun problem-4-jump (sim target &optional (steps 10))
  (problem-3-controller-brute sim :target target
			      :end-condition (let ((counter 0)) (lambda () (>= (incf counter) steps))))
  #+nil
  (destructuring-bind (target-radius wait) 
      (estimate-target-radius-iteratively sim target)
    (push (list 0 0 target-radius) *show-orbits*)
    (iter (repeat wait) (sim-step sim))

    (problem-3-controller-jump sim target-radius)

    (let ((us (sim-us sim)) (e (sim-target sim)))
      (let ((vx (sat-vx us)) (vy (sat-vy us))
	    (evx (sat-vx e)) (evy (sat-vy e)))
	(sim-step sim (- vx evx) (- vy evy))))

    (chaser-controller sim :target target :step steps)))

(defun fuel-low-p (sim) ; TODO
  (declare (ignore sim))
  t)

(defun refuel-controller (sim)
  (let ((sim (copy-sim sim)))
    (sim-step sim)
    (format t "Fuel before meeting with station: ~f~%" (sim-fuel sim)))
  (problem-1-controller sim (* 2 (sat-r (sim-fuelstation sim))))
  (problem-4-jump sim (sim-fuelstation sim) 1)
  (format t "Distance from fuel station: ~f~%" (sat-distance (sim-fuelstation sim) (sim-us sim)))
  (format t "Fuel after meeting with station: ~f~%" (sim-fuel sim)))

(defun go-for-satellite (sat sim &optional (time 10) (max-thrust 1000))
  (problem-4-jump sim sat)
  #+nil
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

(defun problem-4-controller (sim)
  (iter (with satellites = '(0 1 2 3 4 5 6 7 8 9 10))
	(while satellites)
	(for nearest = (nearest-satellite satellites sim))
	(when (fuel-low-p sim)
	  (refuel-controller sim))
	(go-for-satellite nearest sim)
	(for prev-score first 0 then score)
	(for score = (sim-score sim))
	(when (/= score prev-score)
	  (format t "New score: ~f~%" score)
	  (setf satellites (remove nearest satellites)))
	(while (>= score 0))
	(finally (return (values (reverse (sim-thrusts sim)) (sim-time sim))))))


(defun problem-4-controller-go-to-moon (sim)
  (problem-1-controller sim (/ (sat-r (sim-moon sim)) 4))
  (sim-step sim)
  (sim-step sim)
  (problem-3-controller sim (sim-moon sim))
  sim)