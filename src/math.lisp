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

(defun calc-angle-between-vectors (v1 v2)
  (let* ((v1 (normalize-vector v1))
	 (v2 (normalize-vector v2)) 
	 (angle (acos (vdot* v1 v2))))
    (assert (<= 0d0 angle pi))
    angle))

(defun calc-unit-tangent-vector (v)
  (normalize-vector (vec (vy v) (- (vx v)))))

(defun adjust-direction (v vref)
  (let ((angle (calc-angle-between-vectors v vref)))
    (if (> angle #.(/ pi 2))
	(vec (- (vx v)) (- (vy v)))
	v)))


;;; misc
(defun approximately-equal (x y &optional (error-toleration 0.01d0))
  (and (< (abs (/ (- x y) x)) error-toleration)
       (< (abs (/ (- x y) y)) error-toleration)))


(declaim (inline d))
(defun d (x y)
  (sqrt (+ (^2 x) (^2 y))))


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

;;; Commented lines are for reverse rotation
(defun rotate-vector (vec alpha)
  (let ((x (first vec)) (y (second vec))
	(c (cos alpha)) (s (sin alpha)))
;;     (list (+ (* c x) (* s y)) (- (* c y) (* s x)))
    (list (- (* c x) (* s y)) (+ (* s x) (* c y)))
    ))

;;; Commented lines are for reverse rotation
(defun ellipse-from-quadratic (coefficients)
  (destructuring-bind (a b c d e) (coerce coefficients 'list)
    (flet ((ellipse (a c d e phi)
	     (let ((f (+ 1 (/ (* d d) (* 4 a)) (/ (* e e) (* 4 c)))))
	       (list (rotate-vector (list (/ d (* -2 a)) (/ e (* -2 c))) phi)
		     (sqrt (abs (/ f a))) (sqrt (abs (/ f c))) phi))))
      (if (< (min (abs (/ b a)) (abs (/ b c))) 1d-5)
	  (ellipse a c d e 0d0)
	  (let* (
;; 		 (phi (/ (atan b (- c a)) 2))
		 (phi (/ (atan b (- a c)) 2))
		 (cos (cos phi))
		 (sin (sin phi))
;; 		 (a (+ (* a cos cos) (- (* b cos sin)) (* c sin sin)))
;; 		 (c (+ (* a sin sin) (* b cos sin) (* c cos cos)))
;; 		 (d (- (* d cos) (* e sin)))
;; 		 (e (+ (* d sin) (* e cos)))
		 (a (+ (* a cos cos) (* b cos sin) (* c sin sin)))
		 (c (+ (* a sin sin) (- (* b cos sin)) (* c cos cos)))
		 (d (+ (* e sin) (* d cos)))
		 (e (- (* e cos) (* d sin)))
		 )
	    (ellipse a c d e phi))))))

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
