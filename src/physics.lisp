(in-package #:dysfunkycom)

(declaim (inline ^2))
(defun ^2 (x)
  (* x x))

(defun accel-magnitude (sx sy)
  (/ +G-m-earth+ (+ (^2 sx) (^2 sy))))
