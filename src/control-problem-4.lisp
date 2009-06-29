(in-package #:dysfunkycom)

(defun find-sat-by-id (id sim)
  (find id (sim-sats sim) :test (lambda (name sat) (and (numberp (sat-name sat)) (= (sat-name sat) name)))))

(defun nearest-satellite (id-list sim)
  (let ((sats (mapcar (lambda (id) (find-sat-by-id id sim)) id-list))
	(us (sim-us sim)))
    (iter (for sat in sats)
	  (finding sat minimizing (sat-distance sat us)))))

(defun problem-4-jump (sim target &key end-condition chasing-steps)
  (problem-3-controller-brute sim :target target
			      :end-condition (or end-condition
						 (let ((counter 0))
						   (lambda () (>= (incf counter) chasing-steps))))))

(defun fuel-low-p (sim) ; TODO
  (declare (ignore sim))
  t)

(defun refuel-controller (sim)
  (let ((sim (copy-sim sim)))
    (sim-step sim)
    (format t "Fuel before meeting with station: ~f~%" (sim-fuel sim)))
  (problem-1-controller sim (* 2 (sat-r (sim-fuelstation sim))))
  (problem-4-jump sim (sim-fuelstation sim)
		  :end-condition (let ((fuel (sim-fuel sim)))
				   (lambda () (> (sim-fuel sim) fuel))))
  (format t "Distance from fuel station: ~f~%" (sat-distance (sim-fuelstation sim) (sim-us sim)))
  (format t "Fuel after meeting with station: ~f~%" (sim-fuel sim)))

(defun go-for-satellite (sat sim)
  (stablize-to-circular-orbit sim)
  (problem-4-jump sim sat :chasing-steps 10))

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


