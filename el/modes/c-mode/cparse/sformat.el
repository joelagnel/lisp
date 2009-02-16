;;; sformat - souped up format
;;;
;;; Author: Eric Ludlam (zappo@gnu.ai.mit.edu)
;;; Version: 1.0
;;; Keywords: extensions
;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;

;;; Commentary:
;;;
;;; In some applications configuration strings have % options in them
;;; which permit special strings to be inserted.  There are many other
;;; programs which would also benifit from such configuration, but do
;;; not have it due to the time required to build such an extension.
;;; Sformat fills that need making the creation of such functions
;;; quite simple.

;;; $Id: sformat.el,v 1.1 1994/09/10 02:48:40 zappo Exp $
;;;
;;; History
;;;
;;; eml 8/17/94
;;; Added positional data, and ability to handle lambda functions in
;;; options list for more general use.
;;;

(defvar Sformat-default-format 'Sformat-default-format-method
  "Function used when the input param is a string and not a function.
This function must conform to the following parameters:
1 - string to be formatted
2 - A1
3 - A2
4 - A1 default or fn
5 - A2 default or fn")

(defvar Sformat-formatting nil
  "Do not change.  This flags when Sformat is currently formatting a
string for lambda functions which may need to know.")

(defun Sformat-point ()
  "Function which can be called from functions passed to Sformat which
will return the offset from the beginning of the formatted string
where the selected % option occurs."
  (length newstr))

(defun Sformat-column ()
  "Function which can be called from functions passed to Sformat which
will return the current column (distance from last \n) in the created
string so far."
  (let ((ts newstr))
    (while (string-match "\\(\n\\)" ts)
      (setq ts (substring ts (match-end 1))))
    (length ts)))

(defun Sformat (extensions fmt &rest args)
  "Provide a simple means of formatting strings with special %
options.  This will use EXTENSIONS to fill out FMT, and then pass the
result to #<subr format> with args.  EXTENSIONS is of the form:
      '( (?F local-filename A1default A2default)
         (?U local-username) )

where F is the character after a %, and 'local-filename is a variable
or function.  If it is a function, it must be able to take 2 numeric
arguments. The args can be used for whatever purpose you desire for
the function.  A string or variable holding a string will have it's
value inserted just as Sformat-string-word-format would cut it up.
This action can be modified by changing what the variable
Sformat-default-format points to.  A1default and A2default can be
either default values for A1 or A2, or symbols to be used when cutting
this specific string into little pieces.  Numbers are formatted as per
%d with A#defaults being used (numeric only).  Lambda functions passed
in directly as lists will be evaled with no parameters.  Anything else
will be inserted as %S would with A#defaults being used (numeric
only).

Viable formats would be:
   %-10v         - 10 chars, pad left
   %.1v %:1v     - first word
   %10.2v %10:2v - 10 chars, pad right for first 2 words

   where v is some format character.  Note that . and : are interchangeable

      (Sformat extensions fmt &rest args)"

  ;; verify arguments
  (if (not (listp extensions))
      (signal 'wrong-type-argument (list 'listp extensions)))
  (if (not (stringp fmt))
      (signal 'wrong-type-argument (list 'stringp fmt)))

  (let ((Sformat-formatting t)		;Yes, we are formatting something
	(cnt 0)				;position in string
	(tl nil)			;temp list of extensions
	(ln (length fmt))		;length of fmt string
	(tc nil)			;temp char
	(newstr "")			;the new string
	(pcnt nil)			;% symbol flag
	(dot nil)			;. symbol flag
	(neg1 nil)			;- symbol flag on arg1
	(neg2 nil)			;- symbol flag on arg2
	(A1 nil)			;arg 1
	(A2 nil))			;arg 2
    (while (< cnt ln)
      (setq tc (aref fmt cnt))
      (if (not pcnt)
	  (if (= tc ?%)
	      (setq pcnt t)
	    (setq newstr (concat newstr (char-to-string tc))))
	(cond
	 ((or (= tc ?.) (= tc ?:))	;. such as %1.2F
	  (if dot
	      (error "Too many . or : in %% formatter!")
	    (setq dot t)))
	 ((= tc ?-)			;- such as %-1F
	  (if dot
	      (if A2 (error "Cannot use '-' in middle of numeric arg")
		(setq neg2 t))
	    (if A1 (error "cannot use '-' in middle of numeric arg")
	      (setq neg1 t))))
	 ((and (<= tc ?9) (>= tc ?0))	;number arg
	  (if dot
	      (progn
		(if (not A2) (setq A2 0))
		(setq A2 (+ (* A2 10) (- tc ?0))))
	    (if (not A1) (setq A1 0))
	    (setq A1 (+ (* A1 10) (- tc ?0)))))
	 (t				;the F in %F
	  (setq tl extensions)
	  ;; negafy A1 and A2 if need be.
	  (if (and neg1 A1) (setq A1 (- A1)))
	  (if (and neg2 A2) (setq A2 (- A2)))
	  ;; scan the list of extensions
	  (while (and tl 
		      (integer-or-marker-p (car (car tl)))
		      (not (= (car (car tl)) tc)))
	    (setq tl (cdr tl)))
	  ;; if we don't find it, pass through verbatim
	  (if (not tl)
	      (let ((tmpstr (concat "%"
				    (if A1 (format "%d" A1))
				    (if A2 (format ".%d" A2))
				    (char-to-string tc))))
		(setq newstr (concat newstr tmpstr)))
	    (if (not (integer-or-marker-p (car (car tl))))
		(error "Invalid extensions list passed to Sformat"))

	    (if (and (not A1) (numberp (car (cdr (cdr (car tl))))))
		(setq A1 (car (cdr (cdr (car tl))))))
	    (if (and (not A2) (numberp (car (cdr (cdr (cdr (car tl)))))))
		(setq A2 (car (cdr (cdr (cdr (car tl)))))))

	    (let* ((v (car (cdr (car tl))))
		   (sym (if (symbolp v) (eval v) v))
		   (tmpstr (cond
			    ((and (symbolp sym) (fboundp sym))
			     (funcall sym A1 A2))
			    ((and (listp sym) (equal (car sym) 'lambda))
			     (funcall sym))
			    ((stringp sym)
			     (funcall Sformat-default-format 
				      sym A1 A2
				      (car (cdr (cdr (car tl))))
				      (car (cdr (cdr (cdr (car tl)))))))
			    ((numberp sym)
			     (format (concat "%"
					     (if A1 (format "%d" A1))
					     (if A2 (format ".%d" A2))
					     "d")
				     sym))
			    (t
			     (format (concat "%"
					     (if A1 (format "%d" A1))
					     (if A2 (format ".%d" A2))
					     "S")
				     sym)))))
	      (setq newstr (concat newstr tmpstr))))
	  (setq A1 nil)
	  (setq A2 nil)
	  (setq neg1 nil)
	  (setq neg2 nil)
	  (setq dot nil)
	  (setq pcnt nil)
	  )
	 )
	)
      (setq cnt (1+ cnt))
      )
    (eval (cons 'format (cons newstr args)))
    ))

(defun Sformat-default-format-method (str A1 A2 A1def A2def)
  "Format routine used when the format method is a string."
  ;; check for numbers in defaults, and nil them if need be
  (if (numberp A1def) (setq A1def nil))
  (if (numberp A2def) (setq A2def nil))
  (Sformat-string-word-format str A1 A2 A1def A2def)
  )

;;; The next few routines are for support to make writing your own
;;; formating routines much easier.

(defun Sformat-string-word-format (str A1 A2 method1 method2)
  "Support routine which will adjust STR the given string to A1 specs
by the letter as specified by METHOD1, and A2 specs based on words,
where a word is terminated by method2 regexp.  A1 formatting always overrides
A2 for length.  If A1 is negative, pad right, else pad left to fill to
A1 length.

   Values of METHOD1 are: 
   'fill-only    - If (length STR) < A1, pad (left or right), but do 
                  not shorten
   'shorten-only - If (length STR) > A1, cut back, but do not pad to
                  make STR A1 characters
   nil, 'both    - If STR is too short, pad, if too long, shorten.

   Values of METHOD2 are:
   nil, \"[a-zA-Z0-9_]*\"  - cut by word, where a word includes numbers
                             and '_'
   string (regexp)         - trim out given white space replacing with
                             one space, with A2 words in string
   'preceeding-space       - if A2, the add space to beginning of str

   Other notes:

   The word trimmer automatically always leaves white-space in front
of each word, thus choochoo.dmc.com => choochoo.dmc.com, not choochoo dmc com.

"

  (if (not method1) (setq method1 'both)) 
  (if (not method2) (setq method2 "[a-zA-Z0-9_]*"))

   (let* ((pad nil)
	  (newstr nil)
	  (rstr nil)
	  (A1fl (and A1 (< A1 0)))
	 )
     (if (and A1 (numberp A1))
	 (setq A1 (abs A1)))

     ;; first, cut by A2, if A2 exists.
     (if (or (not A2) (not (stringp method2)))
	 (setq newstr str)
       (let ((notrim (progn
		       (string-match "\\(\\[\\)" method2)
		       (concat
			(substring method2 0 (match-end 1))
			"^"
			(substring method2 (match-end 1)))
		       )))
	 (while (and (< 0 A2) ( string-match (concat notrim 
						     "\\(" 
						     method2
						     "\\)")
					     str))
	   (if newstr 
	       (setq newstr (concat newstr
				     (substring str 0 (match-end 1))))
	     (setq newstr (substring str (match-beginning 1)
				     (match-end 1))))
	   (setq str (substring str (match-end 1)))
	   (setq A2 (1- A2)))))
     ;; Now, cut up newstr by A1 specs!
     (cond
      ((stringp method2)
       (if (not A1)
	   (setq rstr newstr)
	 ;; fill specifications
	 (setq pad (substring 
		    "                                                      "
		    0 (- A1 (length newstr))))
	 (if (and (< (length newstr) A1) 
		  (or (eq method1 'both) 
		      (eq method1 'fill-only)))
	     (if A1fl
		 (setq rstr (concat newstr pad))
	       (setq rstr (concat pad newstr))))
	 ;; cut specifications
	 (if (and (> (length newstr) A1)
		  (or (eq method1 'both) 
		      (eq method1 'shorten-only)))
	     (setq rstr (substring newstr 0 A1)))))
      ((and (eq (eval method2) 'preceeding-space)
	    (integerp A2)
	    (not (eq A2 0))
	    (> (length newstr) 0))
       (setq rstr (concat " " newstr)))
      (t
       (setq rstr newstr)))
     
     rstr)
   )

;;; end of lisp
(provide 'sformat)

