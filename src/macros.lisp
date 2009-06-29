(in-package #:dysfunkycom)

(defmacro destructuring-array-bind ((&rest names) array &body body)
  (once-only (array)
   `(let ,(loop for name in names
		for i from 0
		when name
		collect `(,name (elt ,array ,i))
		)
      ,@body)))

(defmacro debug-state (&rest vars)
  `(let ((*print-lines* ,(* 10 (length vars) )) 
	   (*print-pretty* t) 
	   (*print-length* 100) 
	   (*print-case* :downcase)
	   (*print-level* 100))
     (fresh-line *debug-io*)
     ,@(loop for v in vars 
	       collect 
	     (typecase v
	       (null)
	       ((or symbol list)
		`(format *debug-io* "~A = ~S "
			 ',v ,v))
		 (t
		  `(format *debug-io* "~A " ,v))))
     (fresh-line *debug-io*)
     (values)))
