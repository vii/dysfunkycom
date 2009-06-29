(in-package #:dysfunkycom)

(defun chaser-condition-non-zero-score (sim) 
  (not (zerop (sim-score sim))))

(defun chaser-controller (sim 
			  &key (target (sim-target sim)) (step 900) (range 500) (small-step 100)
			  (closing-condition #'chaser-condition-non-zero-score))
  (declare (optimize debug))
  (labels ((pos-after-step ()
	     (let* ((new (copy-sim sim)) 
		    (target (sim-similar-sat new target)))
	       (loop repeat step do (sim-step new))
	       (assert (not (minusp (sim-score new))))
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
		   (sim-step sim (/ sx step) (/ sy step))))))))
