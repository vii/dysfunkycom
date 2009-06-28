(in-package #:dysfunkycom)

(defun compile-instruction (insn rd)
  (declare (type (unsigned-byte 32) insn)
           (type (unsigned-byte 14) rd))
  (let ((op (ldb (byte 4 28) insn)))
    (if (plusp op)
        (let ((r1 (ldb (byte 14 14) insn)) ; D-type
              (r2 (ldb (byte 14  0) insn)))
          (ecase op
            ((1 2 3 4)
               `(setf (aref-mem ,rd)
                      (,(ecase op
                          (1 '+)
                          (2 '-)
                          (3 '*)
                          (4 `(lambda (r1 r2)
                                (if (= r2 0d0) 0d0 (/ r1 r2)))))
                        (aref-mem ,r1) (aref-mem ,r2))))
            (5 `(setf (aref-oport ,r1) (aref-mem ,r2)))
            (6 `(setf (aref-mem ,rd)
                      (let ((r1 (aref-mem ,r1))
                            (r2 (aref-mem ,r2)))
                        (if (= (status) 1) r1 r2))))))

        (let ((op  (ldb (byte  4 24) insn)) ; S-type
              (imm (ldb (byte 3 21) insn)) ; (byte 10 14) for the whole IMM
              (r1  (ldb (byte 14  0) insn)))
          (ecase op
            (0)
            (1 `(setf (status)
                      (if (,(ecase imm
                              (0 '<)
                              (1 '<=)
                              (2 '=)
                              (3 '>=)
                              (4 '>))
                            (aref-mem ,r1) 0d0)
                          1
                          0)))
            ((2 3 4)
               `(setf (aref-mem ,rd)
                      ,(ecase op
                         (2 `(sqrt (aref-mem ,r1)))
                         (3 `(aref-mem ,r1))
                         (4 `(aref-iport ,r1))))))))))

(deftype vm-mem (&optional (length (ash 1 14)))
  `(simple-array double-float (,length)))

(defmacro with-debug-vm-compile (&body body)
  `(let ((insn-counter 0)) 
     (labels (
	      (msg (format &rest args)
		(format *debug-io* "~D: ~?~&" insn-counter format args))
	      ((setf aref-oport) (val i)
		(msg "Changing oport ~D to ~A" i val)
	       (setf (aref oport i) val))
	      (status ()
		(msg "Reading status ~A" status)
		status)
	      ((setf status) (val)
		(msg "Changing status from ~A to ~A" status val)
	       (setf status val))
	     (aref-iport (i)
	       (msg "Reading from iport ~D the value ~A" i (aref iport i))
	       (aref iport i))
	      (aref-mem (i)
		(msg "Reading from memory ~D the value ~A" i (aref mem i))
		(aref mem i))
	     ((setf aref-mem) (val i)
	       (msg "Changing memory ~D from ~A to ~A" i (aref mem i) val)
	      (setf (aref mem i) val)))
      ,@(loop for stmt in body
	      collect `(msg "---- executing ~A" ',stmt)
	      collect stmt
	      collect `(incf insn-counter)))))

(defmacro with-fast-vm-compile (&body body)
  `(macrolet ((aref-oport (i)
		`(aref oport ,i))
	      (aref-iport (i)
		`(aref iport ,i))
	      (aref-mem (i)
		`(aref mem ,i))
	      (status ()
		`status))
     ,@(remove-if 'not body)))

(defun compile-instructions (instructions)
  (declare (type (simple-array (unsigned-byte 32) 1) instructions))
  `(lambda (mem iport oport)
     (declare (type vm-mem mem iport oport)
              (optimize speed (safety 0)))
     (let ((status 0))
	(declare (type fixnum status))
	(with-fast-vm-compile
	    ,@(loop for rd upfrom 0
		    for insn across instructions
		    for form = (compile-instruction insn rd)
		    collect form)))
       oport))

(defun read-le-int (buf offset &optional (bits 32))
  (let ((bytes (ceiling bits 8)))
    (loop for i downfrom (+ offset bytes -1) repeat bytes
	  for value = (aref buf i) then (+ (* value 256) (aref buf i))
	  finally (return value))))

(defun read-le-double (buf offset)
  (ieee-floats:decode-float64 (read-le-int buf offset 64)))
(declaim (inline read-le-double))

(defun load-program-go (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (buf (make-array len :element-type '(unsigned-byte 8)))
           (nframes (ceiling len 12))
           (insns (make-array nframes :element-type '(unsigned-byte 32)))
           (data  (make-array (ash 1 14) :element-type 'double-float)))
      (read-sequence buf s)
      (format *debug-io* "~&~D frames from ~A~&" nframes file)
      (dotimes (i nframes)
	(let ((base (* i 12)))
	  (setf (values (aref insns i) (aref data i))
		(if (evenp i)
		    (values (read-le-int buf (+ base 8))
			    (read-le-double buf base))
		    (values (read-le-int buf base)
			    (read-le-double buf (+ base 4)))))))
      (let ((lisp (compile-instructions insns)))
	(values (eval lisp) data lisp)))))

(defvar *orbit-programs* (make-hash-table :test 'equalp))

(defun load-program (file)
  (values-list
   (or (gethash file *orbit-programs*)
       (let ((results (multiple-value-list (load-program-go file))))
	 (setf (gethash file *orbit-programs*) results)))))

(defun test-run (filename scenario)
  (multiple-value-bind (program initial-data) (load-program filename)
    (let ((memory (copy-seq initial-data))
	  (input-port (make-array (ash 1 14) :element-type 'double-float :initial-element 0d0))
	  (output-port (make-array (ash 1 14) :element-type 'double-float :initial-element 0d0)))
      (setf (elt input-port #x3e80) scenario)
      (funcall program memory input-port output-port))))

(defun describe-output (oport list)
  (loop for name in list
	for val across oport
	do (format t "~A = ~A~&" name val)))

(defun describe-output-1 (oport)
  (describe-output oport '(Score Fuel Sx Sy Radius))
  oport)

(defstruct (sim (:copier %copy-sim)) 
  program
  memory
  input-port
  output-port
  thrusts
  (time 0)
  sats
  scenario)

(defun copy-sim (sim)
  (let ((new (%copy-sim sim)))
    (sim-freshen new)
    new))

(defstruct sat
  name
  x
  y
  ox
  oy

  done
  oport-offset)

(defun sat-vx (sat)
  (with-slots (x ox)
      sat
      (- x ox)))
(defun sat-vy (sat)
  (with-slots (y oy)
      sat
      (- y oy)))
(defun sat-angle (sat)
  (with-slots (y x)
      sat
      (angle x y)))
(defun sat-r (sat)
  (with-slots (y x)
      sat
      (d x y)))
(defun sat-or (sat)
  (with-slots (oy ox)
      sat
    (d ox oy)))

(defun sat-oangle (sat)
  (with-slots (oy ox)
      sat
    (angle ox oy)))

(defun sat-vangle (sat)
  (normalize-vangle (- (sat-angle sat) (sat-oangle sat))))

(defun make-sats-for-scenario (scenario sim)
  (let (sats)
    (labels ((add (sat)
	       (push sat sats)))
      (add (make-sat :name :us))
      
      (cond ((> scenario 4000)
	     (add (make-sat :name :fuel :oport-offset 4 :done t))
	     (loop for k below 12
		   do (add (make-sat :name k :oport-offset (+ 7 (* 3 k))))))
	    ((> scenario 2000)
	     (add (make-sat :name 0 :oport-offset 4))))
      (let ((copy (copy-sim sim)))
	(setf (sim-sats copy) (coerce (reverse sats) 'vector))
	(sim-step copy)
	(sim-step copy)
	(setf (sim-sats sim) (sim-sats copy))))))

(defun sim-freshen (sim)
  (with-slots 
	(memory input-port output-port thrusts sats)
      sim
    (setf memory (copy-seq memory)
	  input-port (copy-seq input-port)
	  output-port (copy-seq output-port)
	  thrusts (copy-seq thrusts)
	  sats (copy-seq sats))
    sim))

(defun make-simulator (filename scenario)
  (multiple-value-bind (compiled initial-data) (load-program filename)
    (let ((memory (copy-seq initial-data))
	  (input-port (make-array (ash 1 14) :element-type 'double-float :initial-element 0d0))
	  (output-port (make-array (ash 1 14) :element-type 'double-float :initial-element 0d0)))
      (setf (elt input-port #x3e80) scenario)
      (let ((sim
	     (make-sim :program compiled
		       :input-port input-port
		       :output-port output-port
		       :memory memory
		       :scenario scenario)))
	(make-sats-for-scenario scenario sim)
	sim))))

(defun sim-score (sim)
  (with-slots 
	(output-port)
      sim
    (elt output-port 0)))

(defun sim-fuel (sim)
  (with-slots 
	(output-port)
      sim
    (elt output-port 1)))

(defun sim-update-sats (sim)
  (let ((oport (sim-output-port sim)))
    (destructuring-array-bind 
     (nil nil x y)
     oport
     (loop for sat across (sim-sats sim) do
	   (setf (sat-ox sat) (sat-x sat)
		 (sat-oy sat) (sat-y sat))
	   (if (sat-oport-offset sat)
	       (setf (sat-x sat) (- x (elt oport (sat-oport-offset sat)))
		     (sat-y sat) (- y (elt oport (1+ (sat-oport-offset sat)))))
	       (setf (sat-x sat) x (sat-y sat) y))))))

(defun sim-step (sim &optional (ax 0d0) (ay 0d0))
  (with-slots (program memory input-port output-port thrusts time)
      sim
    (setf (elt input-port 2) ax
	  (elt input-port 3) ay)
    (unless (and (zerop ax) (zerop ay))
	(push `(,time ,@(unless (zerop ax) `((2 ,ax))) ,@(unless (zerop ay) `((3 ,ay)))) thrusts))
    (funcall program memory input-port output-port)
    (incf time)
    (sim-update-sats sim)
    output-port))

(defun make-simple-simulator-func (sim)
  (lambda (&optional (ax 0d0) (ay 0d0))
    (sim-step sim ax ay)))



;;; (test-run "bin1.obf" 1001d0)
