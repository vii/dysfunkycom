(defpackage #:dysfunkycom
    (:use #:cl #:iterate #:alexandria))


(defpackage :dysfunkycom-test
    (:use :cl :iterate :alexandria :dysfunkycom))

(in-package #:dysfunkycom)
(defparameter *show-orbits* nil "A list of orbits to be displayed.")

