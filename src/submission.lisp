(in-package #:dysfunkycom)

(defparameter *team-id* 663)

(defun write-le-int (n buf offset &optional (bits 32))
  (let ((bytes (ceiling bits 8)))
    (loop for i upfrom offset repeat bytes
	  for value = n then (ash value -8)
       do (setf (aref buf i) (ldb (byte 8 0) value)))))

(defun write-le-double (d buf offset)
  (write-le-int (ieee-floats:encode-float64 d) buf offset 64))
(declaim (inline write-le-double))

(defun submission (scenario frames)
  "FRAMES is a list of frames where the input ports have changed.
The format is:
\(\(time1 \(port1 value1) \(port2 value2) ...)
 \(time2 \(...))
 ...
 \(time-last))

Example:
\(submission 1001d0 '\(\(0 \(16000 1001d0)) \(1 \(2 1d0) \(3 0.5d0)) \(3 \(2 0d0)) \(5)))"
  (let* ((length (+ 12 ; header
		   (* 8 (length frames)) ; frame headers
		   (* 12 (reduce #'+ (mapcar (lambda (frame) (1- (length frame))) frames))))) ; port mappings
	 (output (make-array length :element-type '(unsigned-byte 8))))
    (write-le-int #xcafebabe output 0)
    (write-le-int *team-id* output 4)
    (write-le-int (floor scenario) output 8)
    (iter (with offset = 12)
	  (for frame in frames)
	  (write-le-int (first frame) output offset) (incf offset 4)
	  (write-le-int (1- (length frame)) output offset) (incf offset 4)
	  (iter (for (port value) in (rest frame))
		(write-le-int port output offset) (incf offset 4)
		(write-le-double value output offset) (incf offset 8)))
    output))

(defun write-submission (scenario frames filename)
  (with-open-file (s filename :direction :output :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (write-sequence (submission scenario frames) s)))
