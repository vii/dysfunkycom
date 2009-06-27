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

(defun thrusts->frames (scenario thrusts)
  "Takes a seq of thrusts from time 1 until the time when score changed."
  (append `((0 (16000 ,scenario)))
	  (iter (for i upfrom 1)
		(for last-vx first 0d0 then vx)
		(for last-vy first 0d0 then vy)
		(for (vx vy) in thrusts)
		(unless (and (= vx last-vx) (= vy last-vy))
		  (let ((frame `(,i ,(if (= vx last-vx) nil `(2 ,vx))
				    ,(if (= vy last-vy) nil `(3 ,vy)))))
		    (collect (remove-if-not #'identity frame) into acc)))
		(finally (return (append acc (list (list i))))))))

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

;;; Questions
;;; - do we want to do anything in frame 0 (other than the scenario setup)?
;;; - do we have to include the scenario setup here?
;;; - are the initial thrusts (0 0) ? (so if we want to give (0 0) we can skip that frame)
(defun write-submission (scenario thrusts filename)
  "THRUSTS contains pairs of doubles, of _every_ frame from frame _1_.
The last frame should be the first one that gives a non-zero score.
Example:
\(write-submission 1001d0 '\(\(1.0d0 0.5d0) \(1.0d0 0.5d0) \(0.0d0 0.5d0) \(0.0d0 0.5d0)) \"submission-test\")"
  (with-open-file (s filename :direction :output :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (write-sequence (submission scenario (thrusts->frames scenario thrusts)) s)))

;;; Submission example:
;;; (hohmann-controller (make-simulator "bin1.obf" 1004d0))
;;; => a lot of thrust data...
;;; (write-submission 1004d0 (rest *) "dysfunkycom/submissions/1004.1")
