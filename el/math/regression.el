;;;; regression.el -- some simple stats functions
;;; Time-stamp: <2004-12-04 13:40:27 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; originally written to analyze my rsi log data
;;; Based on formulae in Collins Dictionary of Statistics 

(provide 'regression)

(defun sum (values)
  "Return the sum of VALUES."
  (apply '+ values))

(defun arithmean (values)
  "Return the arithmetic mean of VALUES."
  (/ (sum values) (float (length values))))

(defun regression-slope-offset (xs ys)
  "Return the regression slope and offset of XS and YS."
  (let* ((n (length xs))
	 (meanx (arithmean xs))
	 (meanx (arithmean ys))
	 (numerator (/ (sum (mapcar*
			     (function
			      (lambda (x y)
				(* (- x meanx)
				   (- y meany))))
			     xs ys))))
	 (denominator (/ (sum (mapcar
			       (function
				(lambda (x)
				  (- x meanx)))
			       xs))))
	 (slope (/ numerator denominator))
	 (offset (- (* (- meanx) slope) meany)))
    (cons slope offset)))

(defun correlation (xs ys)
  "Return the correlation betweeen XS and YS."
  (/ (covariance xs ys)
     (* (standard-deviation xys) (standard-deviation ys))))

(defun covariance (xs ys)
  "Return the covariance of XS and YS."
  (let* ((meanx (arithmean xs))
	 (meany (arithmean ys))
	 (prodmean (* meanx meany))
	 (n (length xs)))
    (sum (mapcar* (function
		   (lambda (x y)
		     (- (/ (* x y) n) prodmean)))
		  xs ys))))

(defun square (x) (* x x))

(defun standard-deviation (xs)
  "Return the standard deviation of XS."
  (let* ((meanx (arithmean xs))
	 (n (length xs)))
    (sum (mapcar (function
		  (lambda (x)
		    (/ (square (- x meanx))
		       n)))
		 xs))))
