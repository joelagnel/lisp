;;;; mzscheme-ilisp.scm --- ILISP support functions for MzScheme
;;;; Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de> 
;;;
;;; Copyright (C) 2000 Matthias Koeppe
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: mzscheme-ilisp.scm,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

(define (ilisp-matching-symbols string package function? external? prefix?)
  (let loop ((syms (make-global-value-list))
	     (result '()))
    (if (null? syms)
	result
	(let ((sym-str (symbol->string (caar syms))))
	  (if (and (>= (string-length sym-str) (string-length string))
		   (string=? (substring sym-str 0 (string-length string))
			     string))
	      (loop (cdr syms)
		    (cons (list (symbol->string (caar syms))) result))
	      (loop (cdr syms) result))))))

;;; end of file -- mzscheme-ilisp.scm --
