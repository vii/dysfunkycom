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

