(in-package :dysfunkycom)

(defun stablize-to-circular-orbit (sim)
  (multiple-value-bind (x y vx vy)
      (let ((sim (copy-sim sim)))
	(sim-step sim 0d0 0d0)
	(position-and-direction sim))
    (multiple-value-bind (xt yt)
	(let ((sim (copy-sim sim)))
	  (sim-step sim 0d0 0d0)
	  (position-and-direction-target sim))
      (let* ((direction (normalize-vector (vec vx vy)))
	     (dir-wanted (adjust-direction (calc-unit-tangent-vector (vec x y))
					   direction))
	     (v-wanted (sqrt (/ +g-m-earth+ (d x y))))
	     (dv (v- (vscale dir-wanted v-wanted) (vec vx vy))))
	;; stablize
	(progn
	  (sim-step sim (- (vx dv)) (- (vy dv))))))))

(defun return-back-to-fuelstation-controller (sim)
  (let* ((fuel-station (sim-fuelstation sim))
	 (r-fuel-station (d (sat-sx fuel-station) (sat-sy fuel-station)))
	 (us (sim-us sim))
	 (r-us (d (sat-sx us) (sat-sy us))))
    (controller-stabilise-to-circular-orbit sim)
    (problem-1-controller sim (/ (+ r-us r-fuel-station) 2d0))
    (controller-stabilise-to-circular-orbit sim)
    (problem-2-controller sim fuel-station)
    (controller-stabilise-to-circular-orbit sim)))
