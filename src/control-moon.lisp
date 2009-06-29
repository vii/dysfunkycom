(in-package #:dysfunkycom)

(defun orbital-speed-moon (r &optional (a r))
  (sqrt (* +G+ +m-moon+ (- (/ 2 r) (/ 1 a)))))

(defun position-and-direction-moon (sim)
  (let ((moon (sim-moon sim)))
    (let ((x0 (sat-osx moon))
	  (x1 (sat-sx moon))
	  (y0 (sat-osy moon))
	  (y1 (sat-sy moon)))
      (values x0 y0 (- x1 x0) (- y1 y0)))))
  
(defun controller-stabilise-to-circular-orbit-around-moon (sim)
  (symbol-macrolet ((us (sim-us sim)) (moon (sim-moon sim)))
    (labels (
	     (scale (speed)
	     (let ((vec (vscale 
			 (multiple-value-bind (x y vx vy)
			     (position-and-direction-moon sim)
			   (adjust-direction
			    (calc-unit-tangent-vector (vec x y))
			    (vec vx vy))) speed)))
	       (values (vx vec) (vy vec))))
	     (stabilise ()
	       (let ((vx (sat-vsx moon))
		   (vy (sat-vsy moon)))
		 (multiple-value-bind (tvx tvy)
		     (scale (orbital-speed-moon (d (sat-sx moon) (sat-sy moon))))
		   (sim-step sim (- (- tvx vx)) (- (- tvy vy)))))))
      (stabilise))))

(defun problem-4-controller-go-to-moon (sim)
  (problem-1-controller sim (/ (sat-r (sim-moon sim)) 4))
  (sim-step sim)
  (sim-step sim)

  (let ((t0 (sim-time sim)) 
	(after-time (controller-ellipse-jumper sim :target (sim-moon sim))))
    (sim-step sim)
    (sim-step sim)
    
    (loop do 
	  (sim-step sim)
	  (assert (not (minusp (sim-score sim))))
	  (when (> (sat-r (sim-us sim)) (* 0.98 +orbital-radius-moon+))
	    (controller-stabilise-to-circular-orbit-around-moon sim)
	    (return)))
    sim))
