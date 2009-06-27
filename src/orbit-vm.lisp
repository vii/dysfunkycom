(in-package :dysfunkycom)

(defun compile-instruction (insn rd)
  (declare (type (unsigned-byte 32) insn)
           (type (unsigned-byte 14) rd))
  (let ((op (ldb (byte 4 28) insn)))
    (if (plusp op)
        (let ((r1 (ldb (byte 14 14) insn))
              (r2 (ldb (byte 14  0) insn)))
          (ecase op
            ((1 2 3 4)
               `(setf (aref mem ,rd)
                      (,(ecase op
                          (1 '+)
                          (2 '-)
                          (3 '*)
                          (4 `(lambda (r1 r2)
                                (if (= r2 0d0) 0d0 (/ r1 r2)))))
                        (aref mem ,r1) (aref mem ,r2))))
            (5 `(setf (aref oport ,r1) (aref mem ,r2)))
            (6 `(setf (aref mem ,rd)
                      (let ((r1 (aref mem ,r1))
                            (r2 (aref mem ,r2)))
                        (if (= status 1) r1 r2))))))
        (let ((op  (ldb (byte  4 24) insn))
              (imm (ldb (byte 4 20) insn)) ; (byte 10 14) for the whole IMM
              (r1  (ldb (byte 14  0) insn)))
          (ecase op
            (0)
            (1 `(setf status
                      (if (,(ecase imm
                              (0 '<)
                              (1 '<=)
                              (2 '=)
                              (3 '>=)
                              (4 '>))
                            (aref mem ,r1) 0d0)
                          1
                          0)))
            ((2 3 4)
               `(setf (aref mem ,rd)
                      ,(ecase op
                         (2 `(sqrt (aref mem ,r1)))
                         (3 `(aref mem ,r1))
                         (4 `(aref iport ,r1))))))))))

(deftype vm-mem (&optional (length (ash 1 14)))
  `(simple-array double-float (,length)))

(defun compile-instructions (instructions)
  (declare (type (simple-array (unsigned-byte 32) 1) instructions))
  `(lambda (mem iport oport)
     (declare (type vm-mem mem iport oport)
              (optimize speed (safety 0)))
     (let ((status 0))
       (declare (type fixnum status))
       ,@(loop for rd upfrom 0
               for insn across instructions
               for form = (compile-instruction insn rd)
               when form
                 collect form)
       oport)))

(defun read-le-int (buf offset &optional (bits 32))
  (let ((bytes (ceiling bits 8)))
    (loop for i downfrom (+ offset bytes -1) repeat bytes
	  for value = (aref buf i) then (+ (* value 256) (aref buf i))
	  finally (return value))))

(defun read-le-double (buf offset)
  (ieee-floats:decode-float64 (read-le-int buf offset 64)))
(declaim (inline read-le-double))

(defun load-program (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (buf (make-array len :element-type '(unsigned-byte 8)))
           (nframes (ceiling len 12))
           (insns (make-array nframes :element-type '(unsigned-byte 32)))
           (data  (make-array (ash 1 14) :element-type 'double-float)))
      (read-sequence buf s)
      (dotimes (i nframes)
	(let ((base (* i 12)))
	  (setf (values (aref insns i) (aref data i))
		(if (evenp i)
		    (values (read-le-int buf (+ base 8))
			    (read-le-double buf base))
		  (values (read-le-int buf base)
			  (read-le-double buf (+ base 4)))))))
      (values (compile-instructions insns) data))))

(defun test-run (filename scenario)
  (multiple-value-bind (program initial-data) (load-program filename)
    (let ((memory (copy-seq initial-data))
	  (input-port (make-array (ash 1 14) :element-type 'double-float))
	  (output-port (make-array (ash 1 14) :element-type 'double-float)))
      (setf (elt input-port #x3e80) scenario)
      (funcall (eval program) memory input-port output-port))))

;;; (test-run "bin1.obf" 1001d0)
