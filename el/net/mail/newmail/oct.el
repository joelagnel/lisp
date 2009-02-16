;;; oct.el --- some GNU octave functions in elisp.
;; Time-stamp: <2005-12-12 05:35:54 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: oct.el
;; Package: oct
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords: GNU Octave, matlab
;; Version: 0.0
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst oct-home-page
  "http://gnufans.net/~deego/emacspub/lisp-mine/oct")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 


;; Our functions will also be able to pseudo-return multiple argouts,
;; we will implement oct--return some day as needed..

;; See also:


;; Quick start:
(defconst oct-quick-start
  "Help..."
)

(defun oct-quick-start ()
  "Provides electric help from variable `oct-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert oct-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst oct-introduction
  "I love the brevity/flexibility of GNU octave.  oct.el implements
\(inefficiently) a *few* common octave functions.  Thus, any of the
arguments to oct-+ can be a number, a vector, or a matrix. 

For oct.el, an example of  row vector is '(1 2 3), a column vector is 
'((1) 
  (2) 
  (3)) 

and a matrix is 
'( (1 2 3)
   (2 3 4))

Each of oct.el's functions, oct-foo seeks to perform the exact same
behavior as that of the corrresponding octave function foo.  Many are
incomplete---i.e. do not handle all possible cases of vectors/matrices
for their arguments.  For documentation on any ocave function, just
(apt-get) install octave2.1*, fire up octave, and type help foo; also
look at octave info files. 

There's no matrix-multiplication here (yet).  BTW, there was one
matrix.el posted here a few years ago.

If you are not into GNU Octave, probably the only useful function here
might be some utilitiess like oct-corr (correlation) or oct-std
\(standard deviation) --- viz. just apply them to lists. 

Octav is huge, and growing. So, this library will never be complete,
nor am I working currently on it. Which is why i should go ahead and post
whatever I have here. :)  " )

;;;###autoload
(defun oct--introduction ()
  "Provides electric help from variable `oct--introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert oct--introduction) nil) "*doc*"))

;;; Commentary:
(defconst oct--commentary
  "
matrix [1 2 3; 4 5 6; 7 8 9] should be represented as 
\((1 2 3) (4 5 6) (7 8 9)) here. 

As in octave, a matrix [1] can be represented equivalently as 1, (1)
or ((1)).

Note that we emulate octave and NOT the matlab-like 'octave
--traditional', and the two do differ in some rare aspects. " )


(defun oct--commentary ()
  "Provides electric help from variable `oct--commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert oct--commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst oct--new-features
  "Help..."
)

(defun oct--new-features ()
  "Provides electric help from variable `oct--new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert oct--new-features) nil) "*doc*"))

;;; TO DO:
(defconst oct--todo
  "Help..."
)

(defun oct--todo ()
  "Provides electric help from variable `oct--todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert oct--todo) nil) "*doc*"))

(defconst oct-version "0.0")
(defun oct-version (&optional arg)
   "Display oct's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "oct version %s" oct-version))
    (message "oct version %s" oct-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup oct nil
  "The group oct."
  :group 'applications)
(defcustom oct-before-load-hooks nil
  "Hooks to run before loading oct."
  :group 'oct)
(defcustom oct-after-load-hooks nil
  "Hooks to run after loading oct."
  :group 'oct)
(run-hooks 'oct-before-load-hooks)

(defcustom oct-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'oct)
(defcustom oct-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'oct)
(defcustom oct-y-or-n-p-function 'oct-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `oct-y-or-n-p'."
  :type 'function
  :group 'oct)
(defcustom oct-n-or-y-p-function 'oct-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `oct-n-or-y-p'."
  :type 'function
  :group 'oct)
(defun oct-message (points &rest args)
  "Signal message, depending on POINTS andoct-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points oct-verbosity))
    (apply #'message args)))
(defun oct-y-or-n-p (add prompt)
  "Query or assume t, based on `oct-interactivity'.
ADD is added to `oct-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add oct-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun oct-n-or-y-p (add prompt)
  "Query or assume t, based on `oct-interactivity'.
ADD is added to `oct-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add oct-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defun oct--make-matrix (element &optional n m )
  (unless m (setq m 1))
  (unless n (setq n 1))
  (setq m (round m))
  (setq n (round n))
  (cond
   ((or (< m 0) (< n 0))
    (error " zeros: can't create a matrix with negative dimensions"))
   ((or (= m 0) (= n 0))
    nil)
   (t (make-list n 
		 (make-list m element)))))


(defun oct--vectorize (element)
  "Converts an elt to a list, if isn't one already"
  (let* ((eltt (oct--matricize element))
	 (size (oct-size eltt))
	 (numrows (first size))
	 (numcols (second size)))
    (cond
     ((= numrows 1) (first eltt))
     ((= numcols 1) (mapcar 'first eltt))
     (t (error "This is a matrix. Can't vectorize. ")))))


(defun oct--elementize (element)
  (let* ((eltt (oct--matricize element))
	 (size (oct-size eltt)))
    (unless (equal size '(1 1))
      (error "not an element"))
    (caar eltt)))

(defun oct--matricize (eltt)
  "will convert a vector to a Nx1 matrix.  As does octave:
a(1)=1, a(2)=1, size(a).  Does not check for sizes for lists."
  (cond
   ((numberp eltt) (list (list eltt)))
   ((null eltt) '(()))
   ((listp eltt)
    (let ((fir (first eltt)))
      (cond
       ((null fir)
	(if (every 'null eltt) '(())
	  (error "Unequal sizes")))
       ((every 'numberp eltt)
	(mapcar '(lambda (arg) (list arg)) eltt))
       ((every 'listp eltt)
	eltt)
       (t (error "How could i have reached here?")))))
   (t (error "shouldn't have reached here. internal oct.el error"))))

(defun oct--minimize (elt &optional vecp)
  "Counterpart to oct--matricize. 
When rowp is true, will vectorize its stuff when possible."
  (let* ((eltma (oct--matricize elt))
	 (sz (oct-size eltma)))
    (cond
     ((equal sz '(1 1))
      (caar eltma))
     ((and vecp (= (second sz) 1))
      (mapcar 
       (lambda (arg) (first arg))
       eltma))
     ((and vecp (= (first sz) 1))
      (first eltma))
     (t eltma))))

(defun oct--equal (&rest args)
  (cond
   ((<= (length args) 1) t)
   (t (let 
	  ((fir (first args)))
	(every 'identity
	 (mapcar '(lambda (arg)
		    (equal arg fir))
		 (cdr args)))))))

(defun oct--operator (function args default)
  (cond
   ((null args) default)
   ((= (length args) 1) (first args))
   (t
    (let* ((a (first args))
	   (b (second args))
	   (c (oct--matricize a))
	   (d (oct--matricize b))
	   (sizec (oct-size a))
	   (sized (oct-size d)))
      (cond
       ((equal sizec '(1 1))
	(setq c (oct--make-matrix (caar c) (first sized) (second
							  sized))))
       ((equal sized '(1 1))
	(setq d (oct--make-matrix (caar d) (first sizec) (second
							  sizec))))
       (t 'noop))
      (oct--operator
       function
       (cons (oct--mapmatrix function c d) (cddr args))
       default)))))

(defun oct--mapmatrix (function mat1 mat2)
  "used by oct--operator"
  (mapcar*
   '(lambda (list1 list2)
      (mapcar* function list1 list2))
   mat1 mat2))



;;;###autoload
(defun oct--remove-minus-in-string (str)
  "Replace - to minus in string. 
Octave can't handle filenames with - in them. "
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "-" nil t)
      (replace-match "Minus" nil t))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;====================================================

(defun oct-zeros (&optional n m)
  (oct--make-matrix 0 n m))
(defun oct-ones (&optional n m)
  (oct--make-matrix 1 n m))


(defun oct-sum (x &optional n)
  "
if n = 1, sum along columns.  1 is the default.
if n = 2, sum along rows.

If no n provided, and x happens to be a vector along any dimension,
perform the sum in any case. "
  (oct--minimize
   (let* 
       ((xx (oct--matricize x))
	(size (oct-size xx))
	(nrows (first size))
	(ncols (second size)))
     (when (null n)
       (cond
	((= nrows 1) (setq n 2))
	(t (setq n 1))))
     (cond
      ((= n 1)
       (list
	(apply 
	 'mapcar*
	 (lambda (&rest elements)
	   (apply '+ elements))
	 xx)))
      ((= n 2)
       (mapcar
	'(lambda (list)
	   (list (apply '+ list)))
	xx))
      (t (error "Improper second argument to oct-sum. "))))))
       


(defun oct-size (a &optional n)
  "When given N, returns row dimension if N = 1, else column
dimension. "
  (cond
   ((equal n 1)
    (first (oct-size a )))
   ((equal n 2)
    (second (oct-size a)))
   (t
    (let* 
	((b (oct--matricize a))
	 (sizes (mapcar 'length b))
	 (numcolumns (first sizes)))
      (unless 
	  (oct--equal numcolumns) (error "unequal sizes"))
      (list (length sizes) numcolumns)))))

(defun oct-rows (a)
  (oct-size a 1))
(defun oct-columns (a)
  (oct-size a 2))

(defun oct-length (a)
  (apply 'max (oct-size a)))


(defun oct-.* (&rest args)
  (oct--operator '* args 1))

(defun oct-/ (x n)
  "not general enough yet.  n can only be a number.
moreover, converts everything to float."
  (oct--operator 
   (lambda (a b)
     (/ (float a) b))
   (list x n) 
   1))

(defun oct-+ (&rest args)
  (oct--operator '+ args 0))
(defalias 'oct-add 'oct-+)

(defun oct-- (&rest args)
  (oct--operator
   '-
   (if (= (length args) 1)
       (cons 0 args)
     args)
   0))
(defalias 'oct-subtract 'oct--)
(defalias 'oct-sub 'oct--)





(defun oct-corr (x y)
  "This does need 2 matrices as of right now. 


In fact, currently, just takes a list x and a list y and returns the
corr coeff. 

When implemented, will be Just like octave:
If X is has dimensions M and Nx, and Y has dimensions M and Ny, 
then the returned matrix Z has dimensions Nx and Ny.  
And Z(Nx, Ny) = corr bet. X(:,Nx) and between Y(:,Ny).  "
  
  (let* 
      ((xa (oct--vectorize x))
       (ya (oct--vectorize y))
       (n (oct-length xa))
       (nn (float n))
       (sumxy (oct--elementize (oct-sum (oct-.* xa ya))))
       (sumxx (oct--elementize (oct-sum (oct-.* xa xa))))
       (sumyy (oct--elementize (oct-sum (oct-.* ya ya))))
       (sumx (oct--elementize (oct-sum xa)))
       (sumy (oct--elementize (oct-sum ya))))
    (/ (- sumxy (/ (* sumy sumx) nn))
       (sqrt 
	(* (- sumxx (/ (* sumx sumx) nn))
	   (- sumyy (/ (* sumy sumy) nn)))))))
  

(defun oct-complement (x)
  "is like the ' in octave"
  (let ((xx (oct--matricize x))
	(yy nil))
    (while (caar xx)
      (push
       (mapcar 'first xx) yy)
      (setq xx (mapcar 'cdr xx)))
    (reverse yy)))
	 

(defun oct-sumsq (x)
  "Works only for vectors right now."
  (let ((xv (oct--matricize x)))
    (oct--elementize (oct-sum (oct-.* xv xv)))))
    
    
(defun oct-mean (x)
  "
no second argument yet. works only for vectors."
  (let ((xv (oct--vectorize x)))
    (oct--minimize
     (oct-/ (oct-sum x) (oct-length x)))))


(defun oct-sqrt (x)
  "only numbers as of now."
  (let ((xe (oct--elementize x)))
    (sqrt xe)))

(defun oct-std (x)
  (let* ((xv (oct--vectorize x))
	 (mean (oct-mean xv))
	 (nm1 (- (oct-length xv) 1)))
    (sqrt
     (/ (float (oct-sumsq (oct-- xv  mean)))
	nm1))))


(defun oct-tanh (x)
  (cond
   ((listp x)
    (mapcar 'oct-tanh x))
   ((> 1 x)
    (/
     (float (- 1  (exp (* -2 x))))
     (float (+ 1  (exp (* -2 x))))))
   (t
    (/ 
     (float (- (exp (* 2 x)) 1))
     (float (+ (exp (* 2 x)) 1))))))


(defun oct-atanh (x)
  (cond
   ((listp x)
    (mapcar 'oct-atanh x))
   (t
    (* 0.5 
       (log (/ (float (+ 1 x))
	       (- 1 x)))))))



;; (defun oct-colon (x y)
;;   (if (<= x y)
;;       (cons x (oct-colon (+ x 1) y))
;;     nil))


(defun oct-colon (x y &optional z)
  (when (consp x) (setq x (car x)))
  (when (consp y) (setq y (car y)))
  (when (consp z) (setq z (car z)))
  (let* ((t2 (or z y))
	 (skip (if z y 1))
	 ;;(t1 x)
	 (upp (not (minusp skip))))
    (if upp
	(loop for i from x to t2 by skip collect i)
	(loop for i downfrom x to t2 by (- skip) collect i))))




(defun oct-sign (x)
  (if (listp x)
    (mapcar 'oct-sign x)
    (cond
     ((> x 0) 1)
     ((< x 0) -1)
     (t 0))))

  
(defun oct-transpose (x)
  (oct--minimize
   (apply #'mapcar* #'list
	  (oct--matricize x))))

(defun oct-histcount-nonnumber (x)
  "No counterpart in octave.  probably doesn't even belong in this
  file.. but it is just so useful..

  Takes all unique elements of x, and returns an assoc list.. each
  cadr being the number of times that element occurs in x.
  Works only when x is a single list at this time. 

 Elements of x need not be numeric.  For that case, we will (some day)
 write a fcn whose output is better suited to numbers.  

 This fcn is currently implemented inefficiently.
 See also utils-group.
"
  (let* ((y (remove-duplicates x :test #'equal))
	 (z (mapcar 
	     (lambda (elt)
	       (count elt x :test #'equal)) y)))
    (mapcar* (lambda (a b) (list a b)) y z)))

(provide 'oct)
(run-hooks 'oct-after-load-hooks)



;;; oct.el ends here
