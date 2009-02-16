;;; mapcar2while.el --- byte-optimizer for replacing mapcar with while form.

;; Copyright (C) 2001 Katsumi Yamaoka

;; Author: Katsumi Yamaoka  <yamaoka@jpl.org>
;; Created: 2001/03/02
;; Revised: 2001/03/12
;; Keywords: bytecomp, for-effect, funcall, mapc, mapcar, optimize, while

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program replaces the `mapcar' form with the `while'-loop form
;; at byte-compile time.  However, it will only replace the function
;; name `mapcar' with `mapc' instead of the use of `while'-loop form,
;; if it has the only purpose of a side effect and the function `mapc'
;; is built-in.  This makes a program run faster if you are in luck.
;; There are some ways to use this:
;;
;; * M-x byte-compile-file manually:
;;	You may load (M-x load-file) this file in advance.
;;
;; * Modifying a Makefile:
;;
;;	EMACS = emacs
;;	FLAGS = -batch -q -no-site-file -l mapcar2while
;;
;; * Load this file from an installer program (e.g. dgnushack.el):
;;
;;	(require 'mapcar2while)
;;
;; * Load this file from a program file:
;;
;;	(eval-when-compile (require 'mapcar2while))
;;
;; * Insert the contents of this file in a program file directly:
;;	In this case, you should enclose the whole contents with
;;	`eval-when-compile'.
;;
;; By the way, if you set `mapcar2while-verbose' to non-nil, this
;; program will work verbosely.  Otherwise you can see how it works
;; using `pp' as follows:
;;
;; (pp (byte-optimize-mapcar '(mapcar FN SEQ)))
;;  => (let ((seq (append SEQ nil))
;;           result)
;;       (while seq
;;         (setq result (cons (funcall FN (car seq)) result)
;;               seq (cdr seq)))
;;       (nreverse result))
;;
;; (pp (byte-for-effect-optimize-mapcar '(mapcar FN SEQ)))
;;  => (let ((seq (append SEQ nil)))
;;       (while seq
;;         (funcall FN (car seq))
;;         (setq seq (cdr seq))))

;;; Code:

(defvar mapcar2while-verbose nil
  "*If it is non-nil, report how optimizations are being done.")

(defvar mapcar2while-inhibit-optimization nil
  "*If it is non-nil, optimizations for `mapcar' will not be done.")

(defconst mapcar2while-has-built-in-mapc
  (and (fboundp 'mapc)
       (subrp (symbol-function 'mapc)))
  "T means this Emacs has the built-in function `mapc'.")

(require 'cl);; Needed for the function `gensym'.

(eval-and-compile (autoload 'pp-to-string "pp"))

(put 'mapcar 'byte-optimizer 'byte-optimize-mapcar)
(put 'mapcar 'byte-for-effect-optimizer 'byte-for-effect-optimize-mapcar)

(defun byte-optimize-mapcar (form)
  "Convert `mapcar' form into `while'-loop form with return value."
  (if mapcar2while-inhibit-optimization
      form
    (let* ((function (cadr form))
	   (sequence (caddr form))
	   (seq
	    (cond
	     ((or (null sequence)
		  (equal '(quote nil) sequence))
	      nil)
	     ((or (and (symbolp sequence)
		       (string-match "\\(^\\|-\\)a?lists?$"
				     (symbol-name sequence)))
		  (and (eq 'quote (car-safe sequence))
		       (consp (cadr sequence)))
		  (and (eq 'setq (car-safe sequence))
		       (eq 'quote (car-safe (caddr sequence)))
		       (consp (car (cdaddr sequence))))
		  (memq (if (eq 'setq (car-safe sequence))
			    (car-safe (caddr sequence))
			  (car-safe sequence))
			'(append
			  assoc assq buffer-list cons copy-alist
			  delete delq directory-files frame-list
			  list make-list mapcar member memq nconc
			  nreverse overlay-properties overlays-at
			  overlays-in process-list rassoc rassq
			  reverse ring-elements sort window-list)))
	      sequence)
	     (t
	      `(append ,sequence nil))))
	   (outform
	    (cond
	     ((null seq)
	      nil)
	     ((or (and (memq (car-safe function) (list 'function 'quote))
		       (eq 'lambda (car-safe (cadr function)))
		       (setq function (cadr function)))
		  (eq 'lambda (car-safe function)))
	      (let ((def (if (cdddr function)
			     (cons 'progn (cddr function))
			   (caddr function)))
		    (args (cadr function))
		    (--seq-- (gensym "--seq--"))
		    (--result-- (gensym "--result--")))
		(cond
		 ((eq '&rest (car args))
		  (let ((arg (cadr args)))
		    `(let ((,--seq-- ,seq)
			   ,arg ,--result--)
		       (while ,--seq--
			 (setq ,arg (list (car ,--seq--))
			       ,--result-- (cons  ,def ,--result--)
			       ,--seq-- (cdr ,--seq--)))
		       (nreverse ,--result--))))
		 ((eq '&optional (car args))
		  `(let ((,--seq-- ,seq)
			 ,(cadr args) ,--result--
			 ,@(delq '&rest (cddr args)))
		     (while ,--seq--
		       (setq ,(cadr args) (car ,--seq--)
			     ,--result-- (cons ,def ,--result--)
			     ,--seq-- (cdr ,--seq--)))
		     (nreverse ,--result--)))
		 (t
		  `(let ((,--seq-- ,seq)
			 ,(car args) ,--result--
			 ,@(delq '&rest (delq '&optional (cdr args))))
		     (while ,--seq--
		       (setq ,(car args) (car ,--seq--)
			     ,--result-- (cons ,def ,--result--)
			     ,--seq-- (cdr ,--seq--)))
		     (nreverse ,--result--))))))
	     (t
	      (when (eq 'quote (car-safe function))
		(setcar function 'function))
	      `(let ((seq ,seq)
		     result)
		 (while seq
		   (setq result (cons (funcall ,function (car seq))
				      result)
			 seq (cdr seq)))
		 (nreverse result))))))
      (when mapcar2while-verbose
	(let ((pp-escape-newlines t)
	      print-level print-length)
	  (byte-compile-log-1 (format "\n;;FROM\n%s\n;;TO\n%s"
				      (pp-to-string form)
				      (pp-to-string outform)))))
      outform)))

(defun byte-for-effect-optimize-mapcar (form)
  "Convert `mapcar' form into `while'-loop form without return value.
If the function `mapc' is built-in, it only converts `mapcar' to
`mapc' in specified FORM."
  (if mapcar2while-inhibit-optimization
      form
    (let ((outform
	   (if mapcar2while-has-built-in-mapc
	       (cons 'mapc (cdr form))
	     (let* ((function (cadr form))
		    (sequence (caddr form))
		    (seq
		     (cond
		      ((or (null sequence)
			   (equal '(quote nil) sequence))
		       nil)
		      ((or (and (symbolp sequence)
				(string-match "\\(^\\|-\\)a?lists?$"
					      (symbol-name sequence)))
			   (and (eq 'quote (car-safe sequence))
				(consp (cadr sequence)))
			   (and (eq 'setq (car-safe sequence))
				(eq 'quote (car-safe (caddr sequence)))
				(consp (car (cdaddr sequence))))
			   (memq (if (eq 'setq (car-safe sequence))
				     (car-safe (caddr sequence))
				   (car-safe sequence))
				 '(append
				   assoc assq buffer-list cons copy-alist
				   delete delq directory-files frame-list
				   list make-list mapcar member memq nconc
				   nreverse overlay-properties overlays-at
				   overlays-in process-list rassoc rassq
				   reverse ring-elements sort window-list)))
		       sequence)
		      (t
		       `(append ,sequence nil)))))
	       (cond
		((null seq)
		 nil)
		(mapcar2while-has-built-in-mapc
		 (cons 'mapc (cdr form)))
		((or (and (memq (car-safe function) (list 'function 'quote))
			  (eq 'lambda (car-safe (cadr function)))
			  (setq function (cadr function)))
		     (eq 'lambda (car-safe function)))
		 (let ((def (if (cdddr function)
				(cons 'progn (cddr function))
			      (caddr function)))
		       (args (cadr function))
		       (--seq-- (gensym "--seq--")))
		   (cond
		    ((eq '&rest (car args))
		     (let ((arg (cadr args)))
		       `(let ((,--seq-- ,seq)
			      ,arg)
			  (while ,--seq--
			    (setq ,arg (list (car ,--seq--))
				  ,--seq-- (cdr ,--seq--))
			    ,def))))
		    ((eq '&optional (car args))
		     `(let ((,--seq-- ,seq)
			    ,(cadr args)
			    ,@(delq '&rest (cddr args)))
			(while ,--seq--
			  (setq ,(cadr args) (car ,--seq--)
				,--seq-- (cdr ,--seq--))
			  ,def)))
		    (t
		     `(let ((,--seq-- ,seq)
			    ,(car args)
			    ,@(delq '&rest (delq '&optional (cdr args))))
			(while ,--seq--
			  (setq ,(car args) (car ,--seq--)
				,--seq-- (cdr ,--seq--))
			  ,def))))))
		(t
		 (when (eq 'quote (car-safe function))
		   (setcar function 'function))
		 `(let ((seq ,seq))
		    (while seq
		      (funcall ,function (car seq))
		      (setq seq (cdr seq))))))))))
      (when mapcar2while-verbose
	(let ((pp-escape-newlines t)
	      print-level print-length)
	  (byte-compile-log-1 (format "\n;;FROM\n%s\n;;TO\n%s"
				      (pp-to-string form)
				      (pp-to-string outform)))))
      outform)))

(provide 'mapcar2while)

;; mapcar2while.el ends here
