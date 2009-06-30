;; Peter's code for demo 

(in-package #:dysfunkycom)

(setf *visualiser-text-fn*
      (lambda (sat)
	(format nil "~A" (sat-name sat))))

(defun demo-1 ()
  (let ((*visualise-max-radius* 5d7)
	(*auto-zoom* nil))
    (visualise-scenario 1001d0)
    (visualise-scenario 1002d0)
    (visualise-scenario 1003d0)
    (visualise-scenario 1004d0)))

(defun demo-2 ()
  (visualise-scenario 2001d0)
  (visualise-scenario 2002d0)
  (visualise-scenario 2003d0)
  (visualise-scenario 2004d0))

(defun demo-3 ()
  (visualise-submission "submissions/chase-3001.1")
  (visualise-submission "submissions/chase-3002.1")
  (visualise-submission "submissions/submit-3003.osf")
  (visualise-submission "submissions/chase-3004.1"))

(defun demo-4 ()
  (let ((*visualise-max-radius* 1d8)
	(*auto-zoom* nil))
    (visualise-submission "./submissions/4001.r3")
    (visualise-submission "./submissions/4002.r6")
    (visualise-submission "./submissions/4003.r2")
    (visualise-submission "./submissions/4004.r1")))

