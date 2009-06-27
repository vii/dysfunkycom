(in-package #:dysfunkycom)

(defmacro destructuring-array-bind ((&rest names) array &body body)
  (once-only (array)
   `(let ,(loop for name in names
		for i from 0
		when name
		collect `(,name (elt ,array ,i))
		)
      ,@body)))
