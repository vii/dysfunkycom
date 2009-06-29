(in-package #:dysfunkycom)

(defun find-sat-by-id (id sim)
  (find id (sim-sats sim) :test (lambda (name sat) (and (numberp (sat-name sat)) (= (sat-name sat) name)))))

(defun nearest-satellite (id-list sim)
  (let ((sats (mapcar (lambda (id) (find-sat-by-id id sim)) id-list))
	(us (sim-us sim)))
    (iter (for sat in sats)
	  (finding sat minimizing (sat-distance sat us)))))

(defun problem-4-jump (sim target)
  (controller-brute-jumper-and-touch sim :target target))

(defun fuel-low-p (sim)
  (< (sim-fuel sim) 5000))

(defun tformat (sim str &rest args)
  (apply #'format t (concatenate 'string "[~d] " str) (sim-time sim) args))

(defun turn-if-needed (sim)
  (let ((fuel-speed (sat-vangle (sim-fuelstation sim)))
	(our-speed (sat-vangle (sim-us sim))))
    (when (minusp (* fuel-speed our-speed))
      (tformat sim "Turn around (fuel: ~f).~%" (sim-fuel sim))
      (change-direction-and-stablise sim))))

(defun refuel-controller (sim)
  (sim-check sim)
  (let ((sim (copy-sim sim)))
    (sim-step sim)
    (tformat sim "Fuel before meeting with station: ~f~%" (sim-fuel sim)))
  (tformat sim "Stabilizing orbit.~%")
  (sim-check sim)
  (controller-wait-and-stabilise-to-circular-orbit sim)
  (sim-check sim)
  (turn-if-needed sim)
  (problem-1-controller sim (* 2 (sat-r (sim-fuelstation sim))))
  (problem-4-jump sim (sim-fuelstation sim))
  (tformat sim "Distance from fuel station: ~f~%" (sat-distance (sim-fuelstation sim) (sim-us sim)))
  (tformat sim "Fuel after meeting with station: ~f~%" (sim-fuel sim))
  (tformat sim "Stabilizing orbit.~%")
  (sim-check sim)
  (controller-stabilise-to-circular-orbit sim :no-wait t)
  (sim-check sim)
  (tformat sim "Fuel before re-fueling: ~f~%" (sim-fuel sim))
  (sim-check sim)
  (chaser-controller-touch sim :target (sim-fuelstation sim))
  (sim-check sim)
  (tformat sim "Fuel after re-fueling: ~f~%" (sim-fuel sim))
  (tformat sim "Stabilizing orbit.~%")
  (controller-stabilise-to-circular-orbit sim))

(defun change-direction-and-stablise (sim)
  (let* ((us (sim-us sim)))
    (sim-step sim (* 2 (sat-vx us)) (* 2 (sat-vy us)))
    (controller-stabilise-to-circular-orbit sim)))

(defun refuel-controller-4004 (sim)
  (let ((sim (copy-sim sim)))
    (sim-step sim)
    (tformat sim "Fuel before meeting with station: ~f~%" (sim-fuel sim)))
  (problem-1-controller sim (* 20 (sat-r (sim-fuelstation sim))))
  (change-direction-and-stablise sim)
  (problem-4-jump sim (sim-fuelstation sim)) 
  (tformat sim "Distance from fuel station: ~f~%" (sat-distance (sim-fuelstation sim) (sim-us sim)))
  (tformat sim "Fuel after meeting with station: ~f~%" (sim-fuel sim)))

(defun go-for-satellite (sat sim)
  (tformat sim "Stabilizing orbit.~%")
  (controller-stabilise-to-circular-orbit sim)
  (tformat sim "Going for satellite ~d (fuel: ~f).~%" (sat-name sat) (sim-fuel sim))
  (problem-4-jump sim sat)
  (tformat sim "Fuel after meeting with satellite: ~f~%" (sim-fuel sim)))

(defparameter *get-only* 11)

(defun problem-4-controller (sim)
  (push (list 0 0 (* 5 (sat-r (sim-fuelstation sim)))) *show-orbits*)
  (iter (with satellites = '(0 1 2 3 4 5 6 7 8 9 10))
	(while satellites)
	(when (<= (length satellites) (- 11 *get-only*))
	  (controller-stabilise-to-circular-orbit sim)
	  (leave))
	(for nearest = (nearest-satellite satellites sim))
	(go-for-satellite nearest sim)
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

(defun problem-4-fixed-order-controller (order)
  (lambda (sim)
    (iter (for id in order)
	  (for sat = (and (numberp id) (find-sat-by-id id sim)))
	  (if sat
	      (go-for-satellite sat sim)
	      (refuel-controller sim)))
    (controller-stabilise-to-circular-orbit sim)
    (values (reverse (sim-thrusts sim)) (sim-time sim))))

(defun next-satellite (sim)
  (iter (for sat in-sequence (sim-sats sim))
	(unless (sat-done sat)
	  (finding sat minimizing (- (sat-r sat) (sat-r (sim-us sim)))))))

(defun problem-4-controller-hack (sim)
  (loop until (> 1000 (sim-fuel sim)) do
	(controller-brute-jumper-and-touch sim  :target (next-satellite sim))
	(controller-stabilise-to-circular-orbit sim)))
 