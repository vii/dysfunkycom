(in-package #:dysfunkycom)

(defparameter *chaser-lookahead* 900)

(defun chaser-condition-non-changing-score (sim)
  (let ((original-score (sim-score sim)))
    (lambda (sim) (/= (sim-score sim) original-score))))

(defun chaser-controller (sim 
			  &key (target (sim-target sim)) (step *chaser-lookahead*) (range 1000) (small-step 100)
			  (mini-step 10)
			  (closing-condition (chaser-condition-non-changing-score sim)))
  (declare (optimize debug))
  (labels ((pos-after-step ()
	     (let* ((new (copy-sim sim)) 
		    (target (sim-similar-sat new target)))
	       (loop repeat step do (sim-step new))
	       (assert (and (not (minusp (sim-score new)))
			    (not (minusp (sim-fuel new)))))
	       (values (sat-sx target) (sat-sy target)))))
    (loop do
	 (when (funcall closing-condition sim)
	   (return))
	  (multiple-value-bind (sx sy)
	      (pos-after-step)
	    (cond ((> range (d sx sy))
		   (loop repeat small-step do
			 (sim-step sim)))
		  (t
		   (sim-step sim (/ sx step) (/ sy step))
		   (loop repeat mini-step do (sim-step sim))))))))

(defun chaser-controller-touch (sim 
			  &key (target (sim-target sim)) (step *chaser-lookahead*) (range 900) (mini-step 10))
  (cl-user::debug-state (sim-time sim))
  (labels ((pos-after-step ()
	     (let* ((new (copy-sim sim)) 
		    (target (sim-similar-sat new target)))
	       (sim-repeat-step new step)
;; 	       (assert (not (minusp (sim-score new))))
	       (values (sat-sx target) (sat-sy target)))))
    (loop do
	  (assert (> step 2))
	  (sim-check sim)
	  (multiple-value-bind (sx sy)
	      (pos-after-step)
	    (cond ((> range (d sx sy))
		   (assert (> +radius-earth+ (d sx sy)))
		   (sim-repeat-step sim step)
		   (assert (> range (d (sat-sx target) (sat-sy target))))
		   (return))
		  (t
		   (cl-user::debug-state (sim-time sim) (/ sx step) (/ sy step) step)
		   (sim-step sim (/ sx step) (/ sy step))
		   (decf step)
		   (sim-repeat-step sim mini-step)
		   (decf step mini-step)))))
      (cl-user::debug-state (sim-time sim) 'out)

    ))
