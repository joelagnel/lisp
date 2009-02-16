;;; fastfuzz.el --- Compensate fast for floating-point roundoff error

;; Copyright (C) 1998 Will Mengarini

;; Author: Will Mengarini <seldon@eskimo.com>
;; URL: <http://www.eskimo.com/~seldon>
;; Created: Su 15 Mar 98
;; Version: 0.20, Mo 04 May 98
;; Keywords: extensions, float, floating point, fuzz

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Try this with C-x C-e:
;;   (= (+ 0.1 0.2) 0.3)
;; Running on an Intel 486/50 with hardware floating point, GNU 19.34.1
;; returns `nil' because of a general problem with the imprecise
;; representation of floating point numbers.  This package implements
;; functions that compensate for that.  For a general discussion of the
;; issue, see the Info node elisp|Numbers|Comparison of Numbers.

;; To use this package, first you'll need to copy this file to a directory
;; that appears in your load-path.  `load-path' is the name of a variable
;; that contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; Then, put
;;   (require 'fastfuzz)
;; in your .emacs file.  That will make these functions available:
;;   fastfuzzy= (x y)
;;   fastfuzzy<= (x y)
;;   fastfuzzy/= (x y)
;;   fastfuzzy< (x y)
;;   fastfuzzy>= (x y)
;;   fastfuzzy> (x y)
;;   fastfuzzyzerop (x)
;;   fastfuzzyplusp (x)
;;   fastfuzzyminusp (x)
;;   fastfuzzyintegerp (x)
;;   fastfuzzywholenump (x)
;; Each has a meaning analogous to the corresponding bare function, except
;; that numbers are considered equal if they're "close enough", which is
;; defined in terms of this package's global variable `fastfuzz'.

;; The difference between this package and fuzz.el is that this package
;; defines equality without using division, so it's faster, but doesn't work
;; at extreme orders of magnitude.  For example, on an Intel 486,
;;   (let ((fuzz     1e-14)
;;         (fastfuzz 1e-14))
;;     (and (fastfuzzy=  1e-13 1.1e-13)
;;          (not (fuzzy= 1e-13 1.1e-13))
;;          (fuzzy=          1e11 (/ (* 1e11 3.3) (+ 1.1 2.2)))
;;          (not (fastfuzzy= 1e11 (/ (* 1e11 3.3) (+ 1.1 2.2))))))
;; An example of an appropriate application for fastfuzz.el would be checking
;; whether probabilities sum to 1.0.

;; You might need a different `fastfuzz' for your machine.  To find out, try
;; M-x fastfuzz-selftest.  If you get an error, try increasing `fastfuzz'
;; with M-x set-variable.  When you find a value that works, put a line like
;;   (setq fastfuzz 1.0e-4)
;; in your .emacs file.

;; If you need a different fastfuzz for a particular application, you could
;; localize the variable `fastfuzz' in a `let' form, or make it buffer-local.

;; Note that this package doesn't share fuzz.el's property that
;; every dyadic comparison function is defined in terms of
;; `fastfuzzy=' (mutatis mutandi), so you can't just overwrite that
;; one function to modify the package's definition of equality.

;;; Code:

;;;###autoload
(defvar fastfuzz 1.0e-6
  "*Two floats with magnitudes closer than this are considered fastfuzzy=.")

;;;###autoload
(defsubst fastfuzzyzerop (x)
  "Return t if NUMBER is within `fastfuzz' of zero."
  (<= (abs x) fastfuzz))

;;;###autoload
(defsubst fastfuzzyplusp (x)
  "Return t if NUMBER is positive and not within `fastfuzz' of zero."
  (> x fastfuzz))

;;;###autoload
(defsubst fastfuzzyminusp (x)
  "Return t if NUMBER is negative and not within `fastfuzz' of zero."
  (< x (- fastfuzz)))

;;;###autoload
(defsubst fastfuzzy= (x y)
  "Return t if 2 args are nearly equal; defined using variable `fastfuzz'."
  (<= (abs (- x y)) fastfuzz))

;;;###autoload
(defsubst fastfuzzy/= (x y)
  "Return t if 2 args aren't nearly equal; defined using variable `fastfuzz'."
  (> (abs (- x y)) fastfuzz))

;;;###autoload
(defsubst fastfuzzy> (x y)
  "Return t if first arg is greater than second, and they're not nearly equal."
  (> (- x y) fastfuzz))

;;;###autoload
(defsubst fastfuzzy<= (x y)
  "Return t if first arg is less than second, or they're nearly equal."
  (<= (- x y) fastfuzz))

;;;###autoload
(defsubst fastfuzzy< (x y)
  "Return t if first arg is less than second, and they're not nearly equal."
  (> (- y x) fastfuzz))

;;;###autoload
(defsubst fastfuzzy>= (x y)
  "Return t if first arg is greater than second, or they're nearly equal."
  (<= (- y x) fastfuzz))

;;;###autoload
(defsubst fastfuzzyintegerp (x)
  "Return t if ARG is close enough to an integer to be construed as one."
  (fastfuzzy= (fround x) x))

;;;###autoload
(defsubst fastfuzzywholenump (x)
  "Return t if ARG is close enough to a whole number to be construed as one."
  (and (fastfuzzyintegerp x) (fastfuzzy>= x 0.0)))

;;; Selftest:
;; 
;; Use `delete-rectangle', orthodoxily bound to C-x r d, to uncomment this
;; code for automated regression testing.  It's commented out only to save
;; space (at RMS's request) in released Emacs; if you're going to hack it you
;; probably want to leave the selftest enabled.
;; 
;; (require 'cl)
;; 
;; (defun fastfuzz-selftest ()
;;   "Test the functions in fastfuzz.el: `fastfuzzy=', etc.
;; Signal an error if a test fails.
;; This selftest is not automatically run when fastfuzz.el is loaded,
;; so if you're on a new machine, or building a new Emacs,
;; you might want to run it by hand with \\[fastfuzz-selftest]."
;;   ;; See discussion of naming convention in fuzz.el's fuzz-selftest.
;;   (interactive)
;; 
;;   ;; This test suite is nothing like a comprehensive one; it just
;;   ;; tries to catch the most egregious brain farts.
;;   ;; Contributions would be welcomed.
;;   ;; With C-x C-e, this code
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.1 .2 .3])
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.4 .5 .6])
;;   ;;   (mapcar (lambda (x) (format "%.18g" x)) [.7 .8 .9])
;;   ;; may be useful in getting an idea how to construct problem expressions.
;; 
;;   (assert      (fastfuzzyplusp    0.01))
;;   (assert      (fastfuzzyminusp  -0.01))
;; 
;;   (assert (not (fastfuzzyzerop    0.01)))
;;   (assert (not (fastfuzzyzerop   -0.01)))
;; 
;;   (assert      (fastfuzzy= (* (/ 10.0 3.0) 3.0) 10.0))
;;   (assert      (fastfuzzy= (* (/ 01.0 3.0) 3.0) 01.0))
;; 
;;   (assert      (fastfuzzy=  (+ 0.1 0.2) 0.3))
;;   (assert      (fastfuzzy<= (+ 0.1 0.2) 0.3))
;;   (assert      (fastfuzzy>= (+ 0.1 0.2) 0.3))
;;   (assert (not (fastfuzzy/= (+ 0.1 0.2) 0.3)))
;;   (assert (not (fastfuzzy>  (+ 0.1 0.2) 0.3)))
;;   (assert (not (fastfuzzy<  (+ 0.1 0.2) 0.3)))
;; 
;;   (assert      (fastfuzzy= 1.1 (/ (/ (* 4.0 1.1) 2.0) 2.0)))
;; 
;;   (assert      (fastfuzzy<= 0.0 0.0))
;;   (assert      (fastfuzzy<= 0.0 0.1))
;; 
;;   (assert (not (fastfuzzy>  0.0 0.0)))
;;   (assert (not (fastfuzzy>  0.0 0.1)))
;; 
;;   (assert      (fastfuzzy>= 1.0 1.0))
;;   (assert      (fastfuzzy>= 1.1 1.0))
;; 
;;   (assert (not (fastfuzzy<  1.0 1.0)))
;;   (assert (not (fastfuzzy<  1.1 1.0)))
;; 
;;   (assert      (fastfuzzy<  +2.0 +3.0))
;;   (assert      (fastfuzzy<= +2.0 +3.0))
;;   (assert (not (fastfuzzy>  +2.0 +3.0)))
;;   (assert (not (fastfuzzy>= +2.0 +3.0)))
;; 
;;   (assert (not (fastfuzzy<  +2.0 -3.0)))
;;   (assert (not (fastfuzzy<= +2.0 -3.0)))
;;   (assert      (fastfuzzy>  +2.0 -3.0))
;;   (assert      (fastfuzzy>= +2.0 -3.0))
;; 
;;   (assert      (fastfuzzy<  -2.0 +3.0))
;;   (assert      (fastfuzzy<= -2.0 +3.0))
;;   (assert (not (fastfuzzy>  -2.0 +3.0)))
;;   (assert (not (fastfuzzy>= -2.0 +3.0)))
;; 
;;   (assert (not (fastfuzzy<  -2.0 -3.0)))
;;   (assert (not (fastfuzzy<= -2.0 -3.0)))
;;   (assert      (fastfuzzy>  -2.0 -3.0))
;;   (assert      (fastfuzzy>= -2.0 -3.0))
;; 
;;   (assert      (fastfuzzyzerop      fastfuzz))
;;   (assert      (fastfuzzyintegerp   fastfuzz))
;;   (assert      (fastfuzzywholenump  fastfuzz))
;; 
;;   (assert      (fastfuzzyzerop      0.0))
;;   (assert      (fastfuzzyintegerp   0.0))
;;   (assert      (fastfuzzywholenump  0.0))
;; 
;;   (assert      (fastfuzzyintegerp   1.0))
;;   (assert      (fastfuzzywholenump  1.0))
;; 
;;   (assert      (fastfuzzyintegerp  -1.0))
;; 
;;   (message "Fastfuzz selftest successful")
;; 
;;   ) ;for C-x C-e: (fastfuzz-selftest)
;;
;;; End of selftest

(provide 'fastfuzz)

;;; fastfuzz.el ends here