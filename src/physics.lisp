(in-package #:dysfunkycom)

(defun accel-magnitude (sx sy)
  (/ +G-m-earth+ (+ (* sx sx) (* sy sy))))
