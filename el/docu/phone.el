;; -*- auto-recompile: t -*-
;;; phone.el ---  number-->letter and letter-->number
;; Time-stamp: <2001-07-26 11:35:59 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: phone.el
;; Package: phone
;; Author: Deepak Goel <dgoel@wam.umd.edu>
;; Thanks:  Kai.Grossjohann@CS.Uni-Dortm
;; Version: 0.2
;; Requires: 'cl
;; Namespaces: phone-, l2n-, n2l.
;; Modes provides: l2n, n2l

;; For latest version:
(defvar phone-home-page  "http://www.glue.umd.edu/~deego")


 
;; This file is NOT part of GNU Emacs.
 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defvar phone-quick-start
  "Help..."
)

(defun phone-quick-start ()
  "Provides electric help for function `phone-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert phone-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar phone-introduction
  "This is my first \(minor\) mode.  Suggestions, hints and specially
coding tips most welcome.  Lots of help from Kai appreciated.

This provides l2n-mode, and n2l-mode.

In the former, letters are converted to numbers \(as on phone\) as you
type them.  In the latter, nothing is done as you type \(since each
number can correspond to several letters\).

Pressing TAB cycles through the various choices for the preceding-char.

I seemed to 'need' such a mode when updating my phonebook etc :\)

"
)

(defun phone-introduction ()
  "Provides electric help for function `phone-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert phone-introduction) nil) "*doc*"))

;;; Commentary:
(defvar phone-commentary
  "See M-x phone-introduction.
"
)

(defun phone-commentary ()
  "Provides electric help for function `phone-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert phone-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar phone-new-features
  "phone itself is new right now :\) "
)

(defun phone-new-features ()
  "Provides electric help for function `phone-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert phone-new-features) nil) "*doc*"))

(defvar phone-version "0.2")

;;==========================================
;;; Code:


(defvar n2l-modeline-string " N->L"
  "The string displayed in the mode line when in n2l mode.")


(defvar l2n-modeline-string " L->N"
  "The string displayed in the mode line when in l2n mode.")







(defun l2n-insert-command ()
  ""
  (interactive)
  (self-insert-command 1)
  (phone-cycle-options 'default)
  )

(defun n2l-insert-command ()
  ""
  (interactive)
  (self-insert-command 1)
  (phone-cycle-options 'default)
  )

(easy-mmode-define-minor-mode 
 n2l-mode
  "Toggle l2n mode.
With arg, turn l2n mode on iff arg is positive.

When this mode is active, entering numerals shall lead to the
entering of the various possible associated letters.  You can cycle
through the various choices (numbers/letters) by pressing TAB.
"
  nil
 n2l-modeline-string 
 (cons
  'keymap
  (cons
   (cons 9 'phone-cycle-options)
   (let ((n2l-tmplist nil))
     (do 
	 ;;from 1--9
	 ((i 49 (+ i 1)))
	 ((> i 57) nil)
       (add-to-list 'n2l-tmplist (cons 
				  i
				  'n2l-insert-command)))
     n2l-tmplist))))

(easy-mmode-define-minor-mode
 l2n-mode
  "Toggle l2n mode.
With arg, turn l2n mode on iff arg is positive.

When this mode is active, entering letters shall lead to the
entering of the various possible associated numbers.  You can cycle
through the various choices (numbers/letters) by pressing TAB.
"
  nil  l2n-modeline-string
  (cons
   'keymap
   (append
    (list (cons 9 'phone-cycle-options))
    (let ((l2n-tmplist nil))
      (do 
	  ((i 65 (+ i 1)))
	  ((> i 90) nil)
	(add-to-list 'l2n-tmplist (cons 
					 i
					 'l2n-insert-command)))
      l2n-tmplist)
    (let ((l2n-tmplist nil))
      (do 
	  ((i 97 (+ i 1)))
	  ((> i 122) nil)
	(add-to-list 'l2n-tmplist (cons 
					 i
					 'l2n-insert-command)))
      l2n-tmplist))))


;;;###autoload
(defun phone-cycle-options (&optional default)
  "Cycle through the various possible replacements for the preceding
character.

The character is replaced by this replacement.
 
 With optional non-nil argument DEFAULT, it instead reads a replacement for
the char from phone-initial-map."
  (interactive "P")
  (let ((new-char
	 (if default
	     (phone-cycle-options-from-map 
	      (preceding-char)
	      (if l2n-mode l2n-map
		(if n2l-mode n2l-map))
	      'assoc-map-p) ; else no map: nil
	   (phone-cycle-options-from-map (preceding-char) phone-map))))
    (when new-char
      (backward-delete-char-untabify 1)
      (insert new-char))))
	


(defun phone-cycle-options-from-map (elt map &optional assoc-map-p)
  "Cycle ELT through one of the various possibilities
idendified in the map.

MAP is a list, each element of which is of the form choices.
CHOICES itself is a list of elements.  If for any CHOICES, the
preceding-char matches an element from choices, the succeeding
element from CHOICES will be substituted in place of previous character.

If the optional argument ASSOC-MAP-P is nol-nil, the map is assumed to
be an assoc-map.  Which means that each element of the map is assumed
to be of the form (ELT choices).  Then, the preceding char is assoced
through the map to match a particular ELT.  If, for that ELT, choices
is a non-list, choices is assumed to be character and is the
replacement, else choices is assumed to be a list of substitutions.
In that case, we search for out element among choices.  If it matches
a member of choices, the following member is the replacement, else the
first member of choices is the desired replacement.
THIS SECOND CHOICE: OF ALLOWING CHOICES TO BE A LIST HAS NOT YET BEEN
IMPLEMENETED, SINCE IT IS NOT NEEDED FOR CURRENT PURPOSES.
REQUIRES 'CL..
"
  (if assoc-map-p
      (cadr (assoc elt map))
    (require 'cl)
    (let*	   
	((choice (first (member-if  
			 (lambda (choices)
			   (member elt choices))
			 map)))
	 (tmpc (cadr (member elt choice))))
      (if (null tmpc)
	  (car choice)
	tmpc))))

  

    

(defun phone-maptree (function list)
  "Like mapcar, except that list can be a nested..
Highly recursive.. so non-optimal..
"
  (if (null list) 
      list
    (cons
     (let ((fir (car list)))
       (if (listp fir)
	   (phone-maptree function fir)
	 (funcall function fir)))
     (phone-maptree function (cdr list)))))


(defvar l2n-map
  (phone-maptree 'string-to-char
	   '(
	     ("a" "2")
	     ("b" "2")
	     ("c" "2")
	     ("d" "3")
	     ("e" "3")
	     ("f" "3")
	     ("g" "4")
	     ("h" "4")
	     ("i" "4")
	     ("j" "5")
	     ("k" "5")
	     ("l" "5")
	     ("m" "6")
	     ("n" "6")
	     ("o" "6")
	     ("p" "7")
	     ("q" "7")
	     ("r" "7")
	     ("s" "7")
	     ("t" "8")
	     ("u" "8")
	     ("v" "8")
	     ("w" "9")
	     ("x" "9")
	     ("y" "9")
	     ("z" "9")
	     ("A" "2")
	     ("B" "2")
	     ("C" "2")
	     ("D" "3")
	     ("E" "3")
	     ("F" "3")
	     ("G" "4")
	     ("H" "4")
	     ("I" "4")
	     ("J" "5")
	     ("K" "5")
	     ("L" "5")
	     ("M" "6")
	     ("N" "6")
	     ("O" "6")
	     ("P" "7")
	     ("Q" "7")
	     ("R" "7")
	     ("S" "7")
	     ("T" "8")
	     ("U" "8")
	     ("V" "8")
	     ("W" "9")
	     ("X" "9")
	     ("Y" "9")
	     ("Z" "9")
	     ))
  "The initial map.. these are the substitutions made initially.. in n2l-mode
Then on, you cycle through the general phone-map by pressing TAB..")

(defvar n2l-map nil
  "The initial map.. these are the substitutions made initially.. in n2l-mode
Then on, you cycle through the general phone-map by pressing TAB..
Since we have several choices, we choose to make NO
substitutions... that is why this map is nil")

	   

(defvar phone-map
  (phone-maptree 'string-to-char
	   '(
	     ("2" "A" "B" "C" "a" "b" "c")
	     ("3" "D" "E" "F" "d" "e" "f")
	     ("4" "G" "H" "I" "g" "h" "i")
	     ("5" "J" "K" "L" "j" "k" "l")
	     ("6" "M" "N" "O" "m" "n" "o")
	     ("7" "P" "Q" "R" "S" "p" "q" "r" "s")
	     ("8" "T" "U" "V" "t" "u" "v")
	     ("9" "W" "X" "Y" "Z" "w" "x" "y" "z")
	     )))



(provide 'phone)

;;; phone.el ends here
