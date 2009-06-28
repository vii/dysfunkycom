(in-package #:dysfunkycom)


(defun estimate-apogee-and-period (sim sat)
  (let ((time 0))
   (labels (
	    (next ()
	      (multiple-value-prog1 
		  (poscache-pos-at-time (sim-poscache sim) sat time)
		(assert (> 1000000 (incf time)))))
	    (wait-for-apogee ()
	      (let ((old-r nil)
		    sign)
		(loop do
		      (multiple-value-bind (x y) (next)
			(let ((r (d x y)))
			  (when old-r
			    (let ((new-sign (signum (- r old-r))))
			      (when (and sign (= sign 1) (/= new-sign 1))
				(return-from wait-for-apogee (values x y)))
			      (unless (zerop new-sign)
				(setf sign new-sign))))
			  (setf old-r r))))))
	    (period ()
	      (let ((start-time time))
		(wait-for-apogee)
		(- time start-time ))))
     (multiple-value-bind 
	   (x y)
	 (wait-for-apogee)
       (values x y (period) (d x y))))))
