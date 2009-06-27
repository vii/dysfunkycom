(in-package #:dysfunkycom)

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

(defun make-visualise-oport (func &optional frames)
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
	     (score fuel x y)
	     (coerce oport 'list)
	     (declare (ignore fuel))
	     (check-type x double-float)
	     
	     (cond ((zerop score)
		    (list (make-visat :name "dysfunc" :sx x :sy y)))
		   (t 
		    (format *debug-io* "Finishing because score is ~A~%" score)
		    nil)))))))))



(defun visualise-draw-text (text &key (x 10) (y 10) (font sdl:*default-font*) (surface sdl:*default-surface*) (fg-color sdl:*white*) (bg-color sdl:*black*))
  (sdl:render-string-shaded text 
			    fg-color bg-color :font font :cache t :free t)
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))

(defun visualise (func &key (earth-color (sdl:color :r 20 :g 100 :b 100)) 
		  (earth-radius +radius-earth+) (visat-radius 10)
		  (window-width 1000) (window-height 1000) (playing-steps 100))
  (declare (optimize debug safety))
  (let* (satspos (time 0) (scale (* 2 +radius-earth+)) playing)
    (labels (
	     (rescale ()
	       (labels ((max-one (func)
			  (max (* 2 +radius-earth+) (loop for visat in satspos
							  maximizing (abs (funcall func visat))))))
		 (macrolet ((maybe-scale (var func)
			      (with-gensyms (real-scale)
				`(let ((,real-scale (coerce (max-one ,func) 'double-float)))
				   (if (or (> ,var (* 4 ,real-scale))
					   (< ,var (* 2 ,real-scale)))
				       (+ 1 (* 3 ,real-scale))
				       ,var)))))
		   (setf scale (max (maybe-scale scale #'visat-sx) (maybe-scale scale #'visat-sy))))))
	     (xform-x (x)
	       (round (let ((width (sdl:width sdl:*default-display*)))
			(/ (+ width (* (/ x scale) width)) 2d0))))
	     (xform-y (y)
	       (round (let ((height (sdl:height sdl:*default-display*)))
			(/ (+ height (* (/ y scale) height)) 2d0))))
	     (xform-radius (r)
	       (round (let ((dim (max (sdl:width sdl:*default-display*) (sdl:height sdl:*default-display*))))
			(* (/ r scale 2) dim))))
	     (next-step ()
	       (loop repeat (if playing playing-steps 1) do
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
	       (sdl:draw-filled-circle-* (xform-x 0) (xform-y 0)
					 (xform-radius earth-radius)
					 :color earth-color)
	       (loop for visat in satspos do
		     (sdl:draw-filled-circle-* (xform-x (visat-sx visat)) (xform-y (visat-sy visat))
					       visat-radius
					       :color (visat-color visat))
		     (visualise-draw-text (format nil "~A ~A" (visat-name visat) (d (visat-sx visat) (visat-sy visat)))
					  :x (xform-x (visat-sx visat)) :y (xform-y (visat-sy visat))
					  :fg-color (sdl:any-color-but-this (visat-color visat))
					  :bg-color (visat-color visat))
		     )
	       (visualise-draw-text (format nil "step = ~A scale = ~A" time scale))
	       (sdl:update-display)))
      (sdl:with-init ()
	(sdl:initialise-default-font sdl:*font-10x20*)
	(unless (sdl:window window-width window-height
			    :title-caption "dysfunkycom"
			    :icon-caption "ICFP 2009"
			    :flags '(sdl:sdl-hw-surface sdl:sdl-resizable))
	  (error "~&Unable to create a SDL window~%"))
	(setf (sdl:frame-rate) 0)

	;; Enable key repeat. Set to default values.
	(sdl:enable-key-repeat nil nil)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event
	    (:key key)
	    (cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
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
	       


(defun visualise-scenario (file scenario)
  (visualise
   (make-visualise-oport (make-simulator file scenario) (thrusts->frames scenario (hohmann-controller (make-simulator file scenario))))))