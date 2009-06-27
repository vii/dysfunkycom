(in-package #:dysfunkycom)

(defstruct visat
  sx
  sy
  name
  vx
  vy)

(defun make-example-func ()
  (let ((satspos 
	 (loop repeat 100 
	       for i from 0
	       collect
	       (list (make-visat :name 'plus
				 :sx (* i 100)
				 :sy (* i 100)
				 :vx 1
				 :vy 1
				 )
		     
		     (make-visat :name 'half-minus
				 :sx (* i 100)
				 :sy (- (* 100 i))
				 :vx 1
				 :vy (- 1)
				 )))))
       (lambda ()
	 (pop satspos))))

(defun visualise-draw-text (text &key (x 10) (y 10) (font sdl:*default-font*) (surface sdl:*default-surface*))
  (sdl:render-string-shaded text 
			    sdl:*white* sdl:*black* :font font :cache t :free t)
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))

(defun visualise (func &key (earth-color (sdl:color :r 20 :g 100 :b 100)) 
		  (earth-radius 100) (visat-radius 10)
		  (window-width 1000) (window-height 1000))
  (let (satspos (time 0) (x-scale 1) (y-scale 1))
    (labels (
	     (rescale ()
	       (labels ((max-one (func)
			  (loop for visat in satspos
				maximizing (abs (funcall func visat)))))
		 (macrolet ((maybe-scale (var func)
			      (with-gensyms (real-scale)
				`(let ((,real-scale (coerce (max-one ,func) 'double-float)))
				   (when (or (> ,var (* 4 ,real-scale))
					     (< ,var (* 2 ,real-scale)))
				     (setf ,var (+ 1 (* 3 ,real-scale))))))))
		   (maybe-scale x-scale #'visat-sx)
		   (maybe-scale y-scale #'visat-sy))))
	     (xform-x (x)
	       (round (let ((width (sdl:width sdl:*default-display*)))
			(/ (+ width (* (/ x x-scale) width)) 2d0))))
	     (xform-y (y)
	       (round (let ((height (sdl:height sdl:*default-display*)))
			(/ (+ height (* (/ y y-scale) height)) 2d0))))
	     (next-step ()
	       (setf satspos (funcall func))
	       (unless satspos
		 (return-from visualise time))
	       (incf time)
	       (rescale))
	     (draw ()
	       (sdl:clear-display sdl:*black*)
	       (sdl:draw-filled-circle-* (xform-x 0) (xform-y 0)
					 earth-radius
					 :color earth-color)
	       (loop for visat in satspos do
		     (sdl:draw-filled-circle-* (xform-x (visat-sx visat)) (xform-y (visat-sy visat))
					       visat-radius
					       :color sdl:*red*))
	       (visualise-draw-text (format nil "step = ~A x-scale = ~A y-scale = ~A" time x-scale y-scale))
	       (sdl:update-display)))
      (sdl:with-init ()
	(unless (sdl:window window-width window-height
			    :title-caption "dysfunkycom"
			    :icon-caption "ICFP 2009"
			    :flags '(sdl:sdl-hw-surface sdl:sdl-resizable))
	  (error "~&Unable to create a SDL window~%"))

	;; Enable key repeat. Set to default values.
	(sdl:enable-key-repeat nil nil)
	(sdl:with-events ()
	  (:quit-event () t)
	  (:key-down-event
	   (:key key)
	   (cond ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event))
		 (t 	
		  (next-step)
		  (draw)
		  (sdl:update-display))))
	  (:video-expose-event () (sdl:update-display))
	  (:idle ()
		 (draw)))))))
	       
