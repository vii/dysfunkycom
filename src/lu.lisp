;; -*- mode: lisp; syntax: common-lisp -*-

(in-package :cl-user)

(defpackage :lu-solver
  (:use :common-lisp :iterate)
  (:export :inverse :solve :least-squares :least-norm))

(in-package :lu-solver)

(defconstant +tiny+ 1.0e-20
  "A very small number.")

(define-modify-macro mulf (mult) * "Multiply the arg by a number.")

;;; by Marc LeBrun
(defun copy-array (array)
  "Copies an array with `adjust-array'."
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims
                              :element-type (array-element-type array)
                              :displaced-to array)
                  dims)))

(defun swap-rows (array i j)
  "Swaps (destructively) the rows I and J in ARRAY."
  (iter (for k from 0 below (array-dimension array 1))
	(rotatef (aref array i k) (aref array j k))))

(defun lu-decomposition (a)
  "Replaces a square matrix A with the LU decomposition of a rowwise
   permutation of itself, returning the reversed list of swapped
   rows. This routine is used in combination with LU-BACK-SUBSTITUTION
   to solve linear equations or invert a matrix."
  (let* ((n (array-dimension a 0))
         (a (copy-array a))
         (scaling (make-array n))
         (swaps nil))
    (iter (for i from 0 below n)
          (let ((max (iter (for j from 0 below n)
			   (maximize (abs (aref a i j))))))
            (if (= max 0.0)
                (error "Singular matrix.")
                (setf (aref scaling i) (/ 1.0 max)))))
    (iter (for j from 0 below n)
	  (with max = nil)
          (iter (for i from 0 below j)
                (decf (aref a i j)
                      (iter (for k from 0 below i)
			    (sum (* (aref a i k) (aref a k j))))))
          (iter (for i from j below n)
		(with big = 0.0)
                (let ((tmp (* (aref scaling i)
                              (abs (decf (aref a i j)
                                         (iter (for k from 0 below j)
                                               (sum (* (aref a i k)
						       (aref a k j)))))))))
                  (when (>= tmp big)
                    (setf big tmp
			  max i))))
          (unless (= j max)
            (swap-rows a j max)
            (push (list j max) swaps)
            (rotatef (aref scaling j) (aref scaling max)))
          (when (= (aref a j j) 0)
            (setf (aref a j j) +tiny+))
          (let ((tmp (/ 1.0 (aref a j j))))
            (iter (for i from (1+ j) below n)
                  (mulf (aref a i j) tmp))))
    (values a (nreverse swaps))))

(defun lu-back-substitution (a swaps b)
  "Solves the set of n linear equations A·X = B. Here A is input, not
   as the matrix A but rather as its LU decomposition, determined by
   the routine LU-DECOMPOSITION. SWAPS is input as the permutation
   list returned by LU-DECOMPOSITION. B is input as the right-hand
   side vector, and returns with the solution vector X. This routine
   takes into account the possibility that b will begin with many zero
   elements, so it is efficient for use in matrix inversion."
  (let ((n (array-dimension a 0))
        (b (copy-array b)))
    (dolist (s swaps)
      (rotatef (aref b (first s)) (aref b (second s))))
    (iter (for i from 0 below n)
	  (with first-non-zero = nil)
          (if first-non-zero
              (decf (aref b i)
                    (iter (for j from first-non-zero below i)
                          (sum (* (aref a i j) (aref b j)))))
              (unless (= (aref b i) 0.0)
                (setf first-non-zero i))))
    (iter (for i from (1- n) downto 0)
          (setf (aref b i)
                (/ (- (aref b i)
                      (iter (for j from (1+ i) below n)
                            (sum (* (aref a i j) (aref b j)))))
                   (aref a i i))))
    b))

(defun inverse (a)
  (multiple-value-bind (lu swaps) (lu-decomposition a)
    (let* ((n (array-dimension a 0))
	   (b (make-array n))
	   (result (make-array (list n n))))
      (iter (for j from 0 below n)
	    (dotimes (i n) (setf (elt b i) 0.0d0))
	    (setf (elt b j) 1.0d0)
	    (for solved = (lu-back-substitution lu swaps b))
	    (dotimes (i n) (setf (aref result i j) (elt solved i))))
      result)))

(defun solve (a b)
  (multiple-value-bind (lu swaps) (lu-decomposition a)
    (lu-back-substitution lu swaps b)))

(defun least-squares (x y)
  "Solve (X^T X)b = X^T y, where X is an (n,m) matrix and Y is an (n,1) matrix."
  (solve
   (matrix:multiplication (matrix:transpose x) x)
   (matrix:to-vector (matrix:multiplication (matrix:transpose x) y))))

(defun least-norm (x y)
  "Solve (X X^T)b = y, where X is an (n,m) matrix and Y is an (n,1) matrix,
and return X^T b."
  (let* ((xt (matrix:transpose x))
	 (b (solve (matrix:multiplication x xt) (matrix:to-vector y))))
    (matrix:to-vector (matrix:multiplication xt (matrix:from-vector b)))))
