(in-package #:dysfunkycom)

(declaim (inline ^2))
(defun ^2 (x)
  (* x x))

(declaim (inline ^3))
(defun ^3 (x)
  (* x x x))

(defun accel-magnitude (sx sy)
  (/ +G-m-earth+ (+ (^2 sx) (^2 sy))))

(defun orbital-speed (r &optional (a r))
  (sqrt (* +g-m-earth+ (- (/ 2 r) (/ 1 a)))))

(defun orbital-period (radius)
  (* ( * 2 pi) (sqrt (/ (^3 radius) +g-m-earth+))))
