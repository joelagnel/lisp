;;; big.el --- big integers, without emacs' upper bound thing.
;; Time-stamp: <2003-02-13 00:25:58 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: big.el
;; Package: big
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.0
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar big-home-page  "http://www.glue.umd.edu/~deego")


 
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
 

;; See also:


;; Quick start:
(defvar big-quick-start
  "Help..."
)

(defun big-quick-start ()
  "Provides electric help for function `big-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert big-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar big-introduction
  "Functional, but has not been deemed ready to post to g.e.s yet.
big.el --- big integers, without emacs' upper bound thing.

Because of the peculiar way in which / works in emacs, our convention
shall be to try to ensure that the sign of each number in the \"big\"
number is always the same..
"
)

(defun big-introduction ()
  "Provides electric help for function `big-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert big-introduction) nil) "*doc*"))

;;; Commentary:
(defvar big-commentary
  "Help...



note for programmer\(s\) : Since the last digit is never multiplied by
anything, it is wrong to keep multiplying by big-max until nil is
reached.. the right way is to stop when there's one element remaining
in the list.. accordingly, nil is never a right representation for 0.."
)

(defun big-commentary ()
  "Provides electric help for function `big-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert big-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar big-new-features
  "Help..."
)

(defun big-new-features ()
  "Provides electric help for function `big-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert big-new-features) nil) "*doc*"))

(defvar big-version "0.0")

;; TODO:
;; each big number that is represented as a list should have 'big as
;; its first element. 

;; in progress.
;; permits calculations on big integers.

;; when the integer is too big to be represented in elisp, it can be
;; represented as a big..


;; the second entry is equivalent to N times its value.. and so on..
;; where N is thus 1 + the bigst number that can be represented by
;; the first entry. N is usually taken to be = 65536.  (see the
;; documentation of funciton current-time, for example).

;; this big.el is very crude right now..

;;==========================================
;;; Code:

;; NB: the largest number possible in emacs on 32-bit machines (like
;; nickel) is 2^27-1, on some 64-bit machines (like erie), it seems to
;; be 2^59 - 1.  All the same, the code here tries to ensure it will
;; work on any machine..  Moreover, the format is chosen to be the
;; same that emacs chooses for time..  viz. 16 significant bits per
;; entry of the list..


(defvar big-max 65536
"The real max for smalls is really one less than this.."
)

(defun big-as (num)
  "  Is a final dressing of the number..
converts it to big or small form as necessary.. and if big, remove
any beginning zeros..
"
  (if (listp num)
      (if (null num) 0
	(if (= (length num) 1)
	    (car num)
	  (if (zerop (first num))
	      (big-as (cdr num))
	    num)))
    (if (>= (abs num) big-max)
	(big-to-big num)
      num)))

(defun big-to-big (number)
  (if  (listp number)
      number
    (append 
     (if (>= number big-max) (big-to-big (/ number big-max)) nil)
     (list (% number big-max)))))

(defun big-to-small (big)
  (if (not (listp big))
      big
    (if (null big) big
      (if (= (length big) 1)
	  (car big)
	(+ (* big-max (car big))
	   (big-to-small (cdr big)))))))

(defalias 'big-add 'big-+)

(defun big-+ (&rest numbers)
  "External.."
  (if (null numbers) 0
    (if (= (length numbers) 1)
	(big-as (car numbers))
      (apply 'big-+ (big-add-2 (car numbers) (cadr numbers))
	      (cddr numbers)))))


(defun big-add-2 (a b)
  (big-add-2-big (big-to-big a)
		 (big-to-big b)))

(defun big-add-2-big (a b)
  (big-ensure-sign 
   (if (< (length a) (length b))
       (big-add-2-big-internal
	b a)
     (big-add-2-big-internal a b))))
  

(defun big-add-2-big-internal (b a)
  "b's length is always >= a's..."
  (let ((lena (length a))
	(lenb (length b)))
    (if (zerop lena)
	b
      (if (zerop (nth (- lena 1) a))
	  (append 
	   (big-add-2-big-internal
	    (subseq b 0 (- lenb 1))
	    (subseq a 0 (- lena 1)))
	   (last b))
	(big-add-2-big-internal 
	 (big-add-big-to-small-internal b (car (last a)))
	 (append (subseq a 0 (- lena 1)) (list 0)))))))


(defun big-add-big-to-small (b s)
  "small should please be guaranteed to be small.."
  (big-ensure-sign
   (big-add-big-to-small-internal b s)))

(defun big-add-big-to-small-internal (b s)
  (let* ((digb (car (last b)))
	 (sum (+ digb s))
	 (quot (% sum big-max))
	 (rem (/ sum big-max))
	 (lenb (length b)))
    (if (zerop rem)
	(append (subseq b 0 (- lenb 1)) (list quot))
      (append 
       (big-add-big-to-small-internal (subseq b 0 (- lenb 1) rem))
       (list quot)))))





(defun big-ensure-sign (num &optional force)
  "if force, then please assume force.."
  (if (or (null num) (numberp num)) num
    (cond 
     ((minusp (car num))
      (big-neg (big-ensure-sign (big-neg num))))
     ((zerop (car num))
      (big-ensure-sign (cdr num)))
     ((plusp (car num))
      (big-ensure-sign-plus num))
     (t (error "what sort of a number is this?")))))

(defun big-ensure-sign-plus (num)
  (let ((numsub (subseq num 0 (- (length num) 1)))
	(numlast (car (last num))))
    (if (null numsub)
	num
      (if (plusp numlast)
	  (append (big-ensure-sign-plus numsub)  (list numlast))
	(append (big-add-big-to-small 
		 numsub -1)
		(list (+ big-max numlast)))))))

(defun big-neg (num)
  (if (or (not (listp num)) (null num)) num
    (cons (- (car num))
	  (big-neg (cdr num)))))
  
		 


(defvar big-day 
  '(1 20864)
  "in seconds  is 86400")

(defvar big-2-days
  '(2 41728))

(defun big-mul-small (big small)
  "Small please be a small.. "
  (let ((a (/ small 2048))
	(b (% small 2048)))
    (let ((bb (big-mul-small-too-small big b)))
      (if (zerop a)
	  b
	(big-add b
		 (big-mul-small-too-small
		  (big-mul-small-too-small big a)
		  2048))))))

(defun big-mul-small-too-small (big small)
  "only multiplies big to small, small better be less then big-max..
even then, works only when small is <= 2^11..  so we still need to
modify this function.. see big-mul-small now.."

  (if (= (length big) 0) big
      (if (= (length big) 1)
	  (big-to-big (* (car big) small))
	(big-add-2-big
	 (big-to-big (* (car (last big)) small))
	 (append (big-mul-small-too-small (subseq big 0 (- (length big) 1))
			  small) (list 0))))))



	    
(provide 'big)

;;; list.el ends here
