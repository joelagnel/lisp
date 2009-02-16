;;; EMPI-UTILS.EL --- Utilities for building backends for EMPI

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	12 May 2004
;; Version: 	1.0
;; Keywords:	empi, music

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <andyetitmoves@gmail.com>) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; empi-utils|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Utilities for building backends for EMPI
;; |$Date: 2004/05/12 10:57:01 $|$Revision: 1.1 $|~/packages/empi-utils.el

;;; Code:

(defun empi-add-prop-noargref (elt prop ctx cmd &rest args)
  (cond
   (nil nil)
   ((stringp elt) (list elt))
   ((numberp elt) (list (number-to-string elt)))
   ((or (vectorp elt) (listp elt))
    (let (ret)
      (setq ret (apply 'nconc (mapcar '(lambda (item)
					 (apply 'empi-add-prop item
						prop ctx cmd args)) elt)))
      (if (vectorp elt)
	  (setq ret (list (apply 'concat ret))))
      ret))
   ((functionp elt) (apply 'empi-add-prop-noargref (apply elt ctx cmd args)
			   elt prop ctx cmd args))
   ((symbolp elt) (list (symbol-value elt)))
   (t (error "Input argument %s%s in handler %s invalid" elt
	     (if (symbolp prop)
		 (concat " for property " (symbol-name prop)) "") ctx))))

(defun empi-add-prop (elt prop ctx cmd &rest args)
  (if (and (numberp elt) (not (= elt 0)))
      (let ((arg (nth (1- (abs elt)) args)))
	(or arg (< elt 0)
	    (error "Argument reference %s%s in handler %s out of range" elt
		   (if (symbolp prop)
		       (concat " for property " (symbol-name prop)) "") ctx))
	(and arg (list (prin1-to-string arg t))))
    (apply 'empi-add-prop-noargref elt prop ctx cmd args)))

(defun empi-get-handler (ctx cmd)
  (let ((cmdh (or (get ctx cmd) (get ctx :defhandler))))
    (if (listp cmdh)
	(or (cdr cmdh) (setq cmdh (cons (car cmdh) (get ctx :defnilout))))
      (setq cmdh (cons cmdh (get ctx :defout))))
    cmdh))

(defun empi-build-arg-list (prog ctx cmd &rest args)
  (let (inlist)
    (nconc prog (apply 'empi-add-prop (get ctx :prefix) :prefix ctx cmd args))
    (setq inlist
	  (apply 'empi-add-prop-noargref (car (empi-get-handler ctx cmd))
		 cmd ctx cmd args))
    (if inlist
	(mapc '(lambda (item)
		 (nconc prog (apply 'empi-add-prop item cmd ctx cmd args)))
	      inlist)
      (mapc '(lambda (item) (nconc prog (apply 'empi-add-prop-noargref item
					       cmd ctx cmd args))) args))))

(defun match-beginning-data (subexp data)
  (nth (* 2 subexp) data))

(defun match-end-data (subexp data)
  (nth (1+ (* 2 subexp)) data))

(defun match-string-data (num data &optional string)
  (if (match-beginning-data num data)
      (if string
	  (substring string (match-beginning-data num data)
		     (match-end-data num data))
	(buffer-substring (match-beginning-data num data)
			  (match-end-data num data)))))

(defmacro push-front2 (l arg1 arg2)
  `(setq ,l (cons ,arg1 (cons ,arg2 ,l))))

(defmacro push-valid-pair (l arg1 arg2)
  `(and ,arg2 (push-front2 ,l ,arg1 ,arg2)))

(defmacro -- (num)
  `(setq ,num (1- ,num)))

(defun empi-transform-output-form (item $< $? data)
   (cond
    ((listp item)
      (mapcar '(lambda (elt) (empi-transform-output-form elt $< $? data)) item))
    ((integerp item)
     (or (match-string-data (abs item) data $<)
	 (if (> item 0) (throw 'empi-format-nil-subexp nil) (- item))))
    (t item)))

(defsubst empi-format-output-item (item $< $? data)
  (eval (empi-transform-output-form item $< $? data)))

(defun empi-format-output (str pexit ctx cmd &rest args)
  (let ((cell (cdr (empi-get-handler ctx cmd))))
  (cond
   ((not cell) (and (= pexit 0) str))
   ((vectorp cell)
    (if (string-match (aref cell 0) str)
	(if (= (length cell) 1) str
	  (let ((data (copy-sequence (match-data))))
	    (if (= (length cell) 2)
		(catch 'empi-format-nil-subexp
		  (setq pexit (empi-format-output-item (aref cell 1)
						       str pexit data))
		  (and pexit (if (listp pexit) (list cmd pexit) pexit)))
	      (let ((i (1- (length cell))) out)
		(or (= (logand i 1) 0) (-- i))
		(while (> i 0)
		  (catch 'empi-format-nil-subexp
		    (push-valid-pair out (or (aref cell (1- i)) cmd)
				     (empi-format-output-item (aref cell i)
							      str pexit data)))
		  (setq i (- i 2)))
		out))))))
   ((functionp cell) (apply cell str pexit ctx cmd args))
   ((listp cell) (list cmd cell))
   (t cell))))

(provide 'empi-utils)

;;; EMPI-UTILS.EL ends here
