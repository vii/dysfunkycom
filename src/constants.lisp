(in-package #:dysfunkycom)

(defconstant +G+ 6.67428d-11)
(defconstant +m-earth+ 6.0d24)
(defconstant +radius-earth+ 6.357d6)

(defconstant +G-m-earth+ (* +G+ +m-earth+))

(defconstant +m-moon+ 7.347d22)
(defconstant +radius-moon+ 1.738d6)
(defconstant +orbital-radius-moon+ 3.84399d8)