(in-package #:dysfunkycom)

(defparameter *show-orbits* nil "A list of orbits to be displayed.")

(defstruct visat
  (sx 0d0 :type double-float)
  (sy 0d0 :type double-float)
  name
  (color sdl:*red*) 
  vx
  vy)

(defun make-example-func ()
  (let ((satspos 
	 (loop repeat 100 
	       for i from 0
	       collect
	       (list (make-visat :name 'plus
				 :sx (* i +radius-earth+)
				 :sy (* i 100)
				 :vx 1
				 :vy 1
				 )
		     
		     (make-visat :name 'half-minus
				 :sx (* i +radius-earth+)
				 :sy (- (* 100 i))
				 :vx 1
				 :vy (- 1)
				 )))))
       (lambda ()
	 (pop satspos))))

(defun make-visualise-oport-1 (func &optional frames)
  (let ((time 0) (frame (pop frames)))
   (lambda ()
     (let ((ax 0d0) (ay 0d0))
       (when (and frame (= time (first frame)))
	 (loop for (control val) in (rest frame)
	       do (cond 
		    ((= control 2) (setf ax val))
		    ((= control 3) (setf ay val))
		    (t (assert (= #x3e80 control))
		       (assert (zerop time)))))
	 (setf frame (pop frames)))
       (let ((oport (funcall func ax ay)))
	 (incf time)
	 (when oport
	   (destructuring-array-bind 
	     (score fuel x y tx ty)
	     oport
	     (declare (ignore fuel))
	     (check-type x double-float)
	     (when (= time 1)
	       (setf *show-orbits* (list (list 0 0 (norm (vec x y))) (list 0 0 (norm (vec tx ty))))))
	     (cond ((zerop score)
		    (list (make-visat :name "dysfunc" :sx x :sy y)))
		   (t 
		    (format *debug-io* "Finishing because score is ~A~%" score)
		    nil)))))))))

(defun make-visualise-oport-2 (func &optional frames)
  (let ((time 0) (frame (pop frames)))
   (lambda ()
     (let ((ax 0d0) (ay 0d0))
       (when (and frame (= time (first frame)))
	 (loop for (control val) in (rest frame)
	       do (cond 
		    ((= control 2) (setf ax val))
		    ((= control 3) (setf ay val))
		    (t (assert (= #x3e80 control))
		       (assert (zerop time)))))
	 (setf frame (pop frames)))
       (let ((oport (funcall func ax ay)))
	 (incf time)
	 (when oport
	   (destructuring-array-bind 
	    (score fuel x y tx ty)
	    oport
	    (declare (ignore fuel))
	    (check-type x double-float)
	    (cond ((zerop score)
		   (list (make-visat :name "dysfunc" :sx x :sy y)
			 (make-visat :name "target" :sx (- x tx) :sy (- y ty) :color sdl:*green*)))
		  (t 
		    (format *debug-io* "Finishing because score is ~A~%" score)
		    nil)))))))))


(defun make-visualise-oport-4 (func &optional frames)
  (let ((time 0) (frame (pop frames)))
   (lambda ()
     (let ((ax 0d0) (ay 0d0))
       (when (and frame (= time (first frame)))
	 (loop for (control val) in (rest frame)
	       do (cond 
		    ((= control 2) (setf ax val))
		    ((= control 3) (setf ay val))
		    (t (assert (= #x3e80 control))
		       (assert (zerop time)))))
	 (setf frame (pop frames)))
       (let ((oport (funcall func ax ay)))
	 (incf time)
	 (when oport
	   (destructuring-array-bind 
	    (score fuel x y fx fy)
	    oport
	    (declare (ignore fuel))
	    (check-type x double-float)
	     
	    (cond ((zerop score)
		   (let ((sats (list 
				(make-visat :name "dysfunc" :sx x :sy y) 
				(make-visat :name "fuel" :sx (- x fx) :sy (- y fy) :color sdl:*blue*)
				(make-visat :name "moon" :sx ( - x (elt oport #x64)) :sy ( - y (elt oport #x65)) :color sdl:*yellow*)
				))) 
			 (loop for k below 12
			       do (push (make-visat :name (format nil "~A" k) :sx (- x (elt oport (+ 7 (* 3 K))))
						    :sy (- y (elt oport (+ 8 (* 3 K))))  :color sdl:*green*) sats))
			 sats
			 ))
		  (t 
		    (format *debug-io* "Finishing because score is ~A~%" score)
		    nil)))))))))

(defun visualise-draw-text (text &key (x 10) (y 10) (font sdl:*default-font*) (surface sdl:*default-surface*) (fg-color sdl:*white*) (bg-color sdl:*black*))
  (sdl:render-string-shaded text 
			    fg-color bg-color :font font :cache t :free t)
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))

(defun visualise (func &key (earth-color (sdl:color :r 20 :g 100 :b 100)) 
		  (earth-radius +radius-earth+) (visat-radius 2)
		  (window-width 0) (window-height 0) (playing-steps 100)
		  (playing-time-scale 0.0001d0))
  (declare (optimize debug safety))
  (let* (satspos (time 0) (scale (* 2 +radius-earth+)) playing window)
    (labels (
	     (rescale ()
	       (labels ((max-one (func)
			  (max (* 2 +radius-earth+) (loop for visat in satspos
							  maximizing (abs (funcall func visat))))))
		 (macrolet ((maybe-scale (var func)
			      (with-gensyms (real-scale)
				`(let ((,real-scale (coerce (max-one ,func) 'double-float)))
				   (if (or (> ,var (* 1.5 ,real-scale))
					   (< ,var (* 1.2 ,real-scale)))
				       (+ 1 (* 1.3 ,real-scale))
				       ,var)))))
		   (setf scale (max (maybe-scale scale #'visat-sx) (maybe-scale scale #'visat-sy))))))
	     (xform-x (x)
	       (round (let ((width (sdl:width window)))
			(/ (+ width (* (/ x scale) width)) 2d0))))
	     (xform-y (y)
	       (round (let ((height (sdl:height window)))
			(/ (+ height (* (/ y scale) height)) 2d0))))
	     (xform-radius (r)
	       (round (let ((dim (max (sdl:width window) (sdl:height window))))
			(* (/ r scale 2) dim))))
	     (next-step ()
	       (loop repeat (if playing (+ playing-steps (* time playing-time-scale)) 1) do
		     (one-step)))
	     (one-step ()
	       (setf satspos (funcall func))
	       (unless satspos
		 (return-from visualise time))
	       (incf time))
	     (skip-frames (n)
	       (loop repeat n do (one-step)))
	     (crashing-into-earth ()
	       (iter (for visat in satspos)
		     (thereis (>= (* 1.01d0 (^2 +radius-earth+))
			       (+ (^2 (visat-sx visat))
				  (^2 (visat-sy visat)))))))
	     (draw ()
	       (sdl:clear-display (if (crashing-into-earth) sdl:*red* sdl:*black*))
	       (rescale)
	       (iter (for (x y r) in *show-orbits*)
		     (sdl:draw-circle-* (xform-x x) (xform-y y) (xform-radius r)
					:color (sdl:color :r 255 :g 255 :b 0)))
	       (sdl:draw-filled-circle-* (xform-x 0) (xform-y 0)
					 (xform-radius earth-radius)
					 :color earth-color)
	       (loop for visat in satspos do
		     (sdl:draw-filled-circle-* (xform-x (visat-sx visat)) (xform-y (visat-sy visat))
					       visat-radius
					       :color (visat-color visat))
		     #- (and) (visualise-draw-text (format nil "~A ~,3E" (visat-name visat) (d (visat-sx visat) (visat-sy visat)))
					  :x (xform-x (visat-sx visat)) :y (xform-y (visat-sy visat))
					  :fg-color (sdl:any-color-but-this (visat-color visat))
					  :bg-color (visat-color visat))
		     )
	       (visualise-draw-text (format nil "T = ~A scale = ~,3E log10scale = ~D" time scale (round (log scale 10))))
	       (sdl:update-display))
	     (window ()
	       (setf window (sdl:window window-width window-height
				  :title-caption "dysfunkycom"
				  :icon-caption "ICFP 2009"
				  :flags '(sdl:sdl-resizable)))
	       	(unless window
		  (error "~&Unable to create a SDL window~%"))
		window))
      (sdl:with-init ()
	(sdl:initialise-default-font sdl:*font-10x20*)
	(window)
	(setf (sdl:frame-rate) 0)

	;; Enable key repeat. Set to default values.
	(sdl:enable-key-repeat nil nil)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-resize-event (:w w :h h)
			       (setf window-width w
				     window-height h)
			       (window))
	  (:key-down-event
	    (:key key)
	    (cond 
	      ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
	      ((sdl:key= key :sdl-key-q) (skip-frames 100))
	      ((sdl:key= key :sdl-key-w) (skip-frames 500))
	      ((sdl:key= key :sdl-key-e) (skip-frames 1000))
	      ((sdl:key= key :sdl-key-r) (skip-frames 5000))
	      ((sdl:key= key :sdl-key-t) (skip-frames 10000))
	      ((sdl:key= key :sdl-key-y) (skip-frames 50000))
	      ((sdl:key= key :sdl-key-space)
	       (setf playing (not playing)))
	      (t 	
	       (next-step)
	       (draw))))
	  (:video-expose-event () (sdl:update-display))
	  (:idle ()
		 (when playing
		   (next-step))
		 (draw)))))))
	       


(defun controller-for-scenario (scenario)
  (cond ((> 2000 scenario) 
	 'problem-1-controller)
	((> 3000 scenario)
	 'problem-2-controller)))

(defun visualiser-for-scenario (scenario)
  (cond ((> 2000 scenario) 
	 'make-visualise-oport-1)
	((> 3000 scenario)
	 'make-visualise-oport-2)
	(t 'make-visualise-oport-4)))

(defvar *orbit-code-dir* 
  (with-standard-io-syntax (format nil "~A/../orbit-code/"  
				   #.(directory-namestring *compile-file-truename*))))


(defun file-for-scenario (scenario)
  (merge-pathnames
   (with-standard-io-syntax (format nil "bin~D.obf" (floor (/ scenario 1000))))
   *orbit-code-dir*))

(defun visualise-scenario (scenario &key frames (controller (controller-for-scenario scenario)) 
			   (visualiser-func (visualiser-for-scenario scenario)) (file (file-for-scenario scenario)))
  (let* ((scenario (coerce scenario 'double-float))
	 (sim (make-simulator file scenario))
	 (*show-orbits* nil))
    (visualise
     (funcall visualiser-func (make-simple-simulator-func sim) (or frames (when controller (funcall controller (copy-sim sim))))))))

(defun visualise-submission (filename)
  (multiple-value-bind (frames scenario team) (read-submission filename)
    (declare (ignore team))
    (let* ((visualiser-func (visualiser-for-scenario scenario))
	   (file (file-for-scenario scenario))
	   (sim (make-simulator file scenario))
	   (*show-orbits* nil))
      (visualise (funcall visualiser-func (make-simple-simulator-func sim) frames)))))
