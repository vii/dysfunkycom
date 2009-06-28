(in-package #:dysfunkycom)

(defparameter *show-orbits* nil "A list of orbits to be displayed.")

(defun visualise-draw-text 
    (text &key (x 10) (y 10) (font sdl:*default-font*) (surface sdl:*default-surface*) (fg-color sdl:*white*) (bg-color sdl:*black*))
  (sdl:render-string-shaded text 
			    fg-color bg-color :font font :cache t :free t)
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))

(defvar *visualise-window-height* 0)
(defvar *visualise-window-width* 0)

(defun sat-color (sat)
  (case (sat-name sat)
    (:us sdl:*red*)
    (:fuel sdl:*blue*)
    (:moon sdl:*yellow*)
    (t sdl:*green*)))

(defun draw-sat-radius (sat)
  (case (sat-name sat)
    (:us 200000)
    (:moon 1737400)
    (t 100000)))

(defun visualise (sim &key (earth-color (sdl:color :r 20 :g 100 :b 100))
		  frames
		  (earth-radius +radius-earth+) 
		  (window-width *visualise-window-width*) (window-height *visualise-window-height*) (playing-steps 100)
		  (playing-time-scale 0.0001d0))
  (declare (optimize debug safety))
  (let* ((time 0) (scale (* 2 +radius-earth+)) playing window (frame (pop frames)))
    (labels (
	     (rescale ()
	       (labels ((max-one (func)
			  (max (* 2 +radius-earth+) (loop for sat across (sim-sats sim)
							  maximizing (abs (funcall func sat))))))
		 (macrolet ((maybe-scale (var func)
			      (with-gensyms (real-scale)
				`(let ((,real-scale (coerce (max-one ,func) 'double-float)))
				   (if (or (> ,var (* 1.5 ,real-scale))
					   (< ,var (* 1.2 ,real-scale)))
				       (+ 1 (* 1.3 ,real-scale))
				       ,var)))))
		   (setf scale (max (maybe-scale scale #'sat-x) (maybe-scale scale #'sat-y))))))
	     (window-scale ()
	       (/ (* 2 scale)
		  (min window-height window-width)))
	     (xform-x (x)
	       (round (let ((width window-width))
			(+ (/ width 2) (/ x (window-scale))))))
	     (xform-y (y)
	       (round (let ((height window-height))
			(+ (/ height 2) (/ y (window-scale))))))
	     (xform-radius (r)
	       (ceiling r (window-scale)))
	     (next-step ()
	       (loop repeat (if playing (+ playing-steps (* time playing-time-scale)) 1) do
		     (one-step)))
	     (one-step ()
	       (let ((ax 0d0) (ay 0d0))
		 (when (and frame (= time (first frame)))
		   (loop for (control val) in (rest frame)
			 do (cond 
			      ((= control 2) (setf ax val))
		    ((= control 3) (setf ay val))
		    (t (assert (= #x3e80 control))
		       (assert (zerop time)))))
		   (setf frame (pop frames)))
		 (sim-step sim ax ay))
	       (unless (zerop (sim-score sim))
		 (return-from visualise (values (sim-score sim) sim)))
	       (incf time))
	     (skip-frames (n)
	       (loop repeat n do (one-step)))
	     (crashing-into-earth ()
	       (iter (for sat in-sequence (sim-sats sim))
		     (thereis (>= (* 1.01d0 (^2 +radius-earth+))
			       (+ (^2 (sat-x sat))
				  (^2 (sat-y sat)))))))
	     (draw ()
	       (sdl:clear-display (if (crashing-into-earth) sdl:*red* sdl:*black*))
	       (rescale)
	       (iter (for (x y r) in *show-orbits*)
		     (sdl:draw-circle-* (xform-x x) (xform-y y) (xform-radius r)
					:color (sdl:color :r 255 :g 255 :b 0)))
	       (sdl:draw-filled-circle-* (xform-x 0) (xform-y 0)
					 (xform-radius earth-radius)
					 :color earth-color)
	       (loop for sat across (sim-sats sim) do
		     (sdl:draw-filled-circle-* (xform-x (sat-x sat)) (xform-y (sat-y sat))
					       (xform-radius (draw-sat-radius sat))
					       :color (sat-color sat))
		     #- (and) (visualise-draw-text (format nil "~A ~,3E" (sat-name sat) (sat-r sat))
					  :x (xform-x (sat-x sat)) :y (xform-y (sat-y sat))
					  :fg-color (sdl:any-color-but-this (sat-color sat))
					  :bg-color (sat-color sat))
		     )
	       (visualise-draw-text (format nil "T = ~As (~$ days) scale = ~,3E log10scale = ~D" time (/ time (* 24 60 60)) scale (round (log scale 10))))
	       (sdl:update-display))
	     (window ()
	       (setf window (sdl:window window-width window-height
				  :title-caption "dysfunkycom"
				  :icon-caption "ICFP 2009"
				  :flags '(sdl:sdl-resizable)))
	       	(unless window
		  (error "~&Unable to create a SDL window~%"))
		(setf window-width (sdl:width window)
		      window-height (sdl:height window))
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
			       (window)
			       (rescale))
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
	 'problem-2-controller)
	((> 4000 scenario)
	 'problem-3-controller)))

(defvar *orbit-code-dir* 
  (with-standard-io-syntax (format nil "~A/../orbit-code/"  
				   #.(directory-namestring *compile-file-truename*))))


(defun file-for-scenario (scenario)
  (merge-pathnames
   (with-standard-io-syntax (format nil "bin~D.obf" (floor (/ scenario 1000))))
   *orbit-code-dir*))

(defun visualise-scenario (scenario &key frames (controller (controller-for-scenario scenario)) 
			   (file (file-for-scenario scenario)))
  (let* ((scenario (coerce scenario 'double-float))
	 (sim (make-simulator file scenario))
	 (*show-orbits* nil))
    (visualise sim :frames 
	       (or frames (when controller (funcall controller (copy-sim sim)))))))

(defun visualise-submission (filename)
  (multiple-value-bind (frames scenario) (read-submission filename)
    (visualise-scenario scenario :frames frames)))
