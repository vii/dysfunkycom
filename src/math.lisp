(in-package :dysfunkycom)


;;; vector
(defstruct (vec (:conc-name v)
	     (:constructor vec (x y))
	     (:type (vector double-float)))
  (x 0d0 :type double-float)
  (y 0d0 :type double-float))

(defun v+ (v1 v2)
  (vec (+ (vx v1) (vx v2))
       (+ (vy v1) (vy v2))))

(defun v- (v1 v2)
  (vec (- (vx v1) (vx v2))
       (- (vy v1) (vy v2))))

(defun vdot* (v1 v2)
  (+ (* (vx v1) (vx v2))
     (* (vy v1) (vy v2))))

(defun vscale (v a)
  (vec (* (vx v) a)
       (* (vy v) a)))

(defun norm (v)
  (sqrt (+ (* #1=(vx v) #1#)
	   (* #2=(vy v) #2#))))

(defun normalize-vector (v)
  (let* ((x (vx v))
	 (y (vy v))
	 (r (sqrt (float (+ (* x x) (* y y)) 0d0))))
    (vec (/ x r) (/ y r))))

(defun normalize-angle (angle)
  (mod angle (* 2 pi)))

;; Angular velocity should keep the sign . . .
(defun normalize-vangle (angle)
  (- (mod (+ pi angle) (* 2 pi)) pi))

(defun calc-angle-between-vectors (v1 v2)
  (let* ((v1 (normalize-vector v1))
	 (v2 (normalize-vector v2)) 
	 (angle (- (atan (vy v2) (vx v2))
		   (atan (vy v1) (vx v1)))))
    angle))

(defun calc-unit-tangent-vector (v)
  (normalize-vector (vec (vy v) (- (vx v)))))

(defun adjust-direction (v vref)
  (let ((angle (calc-angle-between-vectors v vref)))
    (if (or (> angle #.(/ pi 2)) (< angle #.(- (/ pi 2))))
	(vec (- (vx v)) (- (vy v)))
	v)))


;;; misc
(defun approximately-equal (x y &optional (error-toleration 0.01d0))
  (and (< (abs (/ (- x y) x)) error-toleration)
       (< (abs (/ (- x y) y)) error-toleration)))


(declaim (inline d))
(defun d (x y)
  (sqrt (+ (^2 x) (^2 y))))

(declaim (inline angle))
(defun angle (x y)
  (atan y x))


;;; Ellipse fitting

(defun estimate-ellipse (xy-pairs)
  "This function needs at least 6 pairs, the more the better \(and the slower)."
  (let* ((n (length xy-pairs))
	 (A (make-array (list n 5) :element-type 'double-float))
	 (b (make-array (list n 1) :element-type 'double-float :initial-element 1d0)))
    (iter (for i upfrom 0)
	  (for (x y) in xy-pairs)
	  (mapc (lambda (j v) (setf (aref A i j) v))
		'(0 1 2 3 4) (list (* x x) (* x y) (* y y) x y)))
    (lu-solver:least-squares A b)))

(defun centralise-ellipse (a b c d e)
  (let* ((x (/ (- (* 2 c d) (* b e)) (- (* b b) (* 4 a c))))
	 (y (/ (- (* 2 a e) (* b d)) (- (* b b) (* 4 a c))))
	 (f (- 1 (* a x x) (* b x y) (* c y y) (* d x) (* e y))))
    (values (list x y) (list (/ a f) (/ b f) (/ c f)))))

#+nil ; this one is for reverse rotation
(defun straighten-ellipse (a b c)
  (if (< (min (abs (/ b a)) (abs (/ b c))) 1d-5)
      (values 0 (list a c))
      (let* ((phi (/ (atan b (- a c)) -2))
	     (cos (cos phi))
	     (sin (sin phi))
	     (a (+ (* a cos cos) (- (* b cos sin)) (* c sin sin)))
	     (c (+ (* a sin sin) (* b cos sin) (* c cos cos))))
	(values phi (list a c)))))

(defun straighten-ellipse (a b c)
  (if (< (min (abs (/ b a)) (abs (/ b c))) 1d-5)
      (values 0 (list a c))
      (let* ((phi (/ (atan b (- c a)) -2))
	     (cos (cos phi))
	     (sin (sin phi))
	     (a (+ (* a cos cos) (* b cos sin) (* c sin sin)))
	     (c (+ (* a sin sin) (- (* b cos sin)) (* c cos cos))))
	(values phi (list a c)))))

(defun normalise-ellipse-scale (a c)
  (list (/ (sqrt a)) (/ (sqrt c))))

(defun ellipse-from-quadratic (coefficients)
  (multiple-value-bind (center coefficients)
      (apply #'centralise-ellipse (coerce coefficients 'list))
    (multiple-value-bind (phi coefficients)
	(apply #'straighten-ellipse coefficients)
      (destructuring-bind (a b)
	  (apply #'normalise-ellipse-scale coefficients)
	(list center a b phi)))))

(defun ellipse-point (center a b phi angle)
  (list (+ (first center)
	   (* a (cos angle) (cos phi))
	   (- (* b (sin angle) (sin phi))))
	(+ (second center)
	   (* a (cos angle) (sin phi))
	   (* b (sin angle) (cos phi)))))

(defun generate-ellipse-test-points (center a b phi n)
  "Generates N random ellipse points."
  (iter (repeat n)
	(for alpha = (random (* 2 pi)))
	(collect (ellipse-point center a b phi alpha))))

(defun generate-ellipse-test-files (center a b phi &optional (n 6))
  (let ((points1 (generate-ellipse-test-points center a b phi 500))
	(points2 (generate-ellipse-test-points center a b phi n)))
    (with-open-file (s "/tmp/ellipse" :direction :output :if-exists :supersede)
      (format s "~{~{~f~^ ~}~%~}" points1))
    (with-open-file (s "/tmp/ellipse2" :direction :output :if-exists :supersede)
      (format s "~{~{~f~^ ~}~%~}"
	      (apply #'generate-ellipse-test-points
		     (append (ellipse-from-quadratic (estimate-ellipse points2))
			     '(500)))))))

;;; (generate-ellipse-test-files '(3 4) 2.3 1.7 (/ pi 6))
