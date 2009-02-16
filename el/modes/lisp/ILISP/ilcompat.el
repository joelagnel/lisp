;;; -*- Mode: Emacs-Lisp -*-

;;; ilcompat.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilcompat.el,v 1.5 2002/10/25 12:13:49 kevinrosenberg Exp $

(require 'cl)

;;; Global definitions/declarations

(defconst +ilisp-emacs-version-id+
  (cond ((string-match "XEmacs" emacs-version)
	 'xemacs)
	((string-match "Lucid" emacs-version)
	 (if (string-match "^19.[0-7][^0-9]" emacs-version)
	     'lucid-19
	   'lucid-19-new))
	((string-match "^19" emacs-version)
	 'fsf-19)
	((string-match "^20" emacs-version)
	 'fsf-20)
	((string-match "^21" emacs-version)
	 'fsf-21)
	(t 'fsf-18))
  "The major version of (X)Emacs ILISP is running in.
Declared as '(member fsf-19 fsf-19 fsf-20 fsf-21 lucid-19 lucid-19-new xemacs).
Set in ilcompat.el.")


(defconst +ilisp-emacs-minor-version-number+
  (cond ((eq +ilisp-emacs-version-id+ 'fsf-18) 59)
	((or  (eq +ilisp-emacs-version-id+ 'lucid-19)
	      (eq +ilisp-emacs-version-id+ 'lucid-19-new)
	      )
	 12)			      ; Does emacs-minor-version work?
	((eq +ilisp-emacs-version-id+ 'xemacs) 14)
	(t emacs-minor-version))
  "The minor version of (X)Emacs ILISP is running in.
Set in ilcompat.el.")


;;; Load Emacs version specific compatibility modules

(cond ((or (eq +ilisp-emacs-version-id+ 'lucid-19)
	   (eq +ilisp-emacs-version-id+ 'lucid-19-new))
       (load "illuc19"))
      ((eq +ilisp-emacs-version-id+ 'xemacs) (load "ilxemacs"))
      ((eq +ilisp-emacs-version-id+ 'fsf-18) (load "ilfsf18"))
      ((eq +ilisp-emacs-version-id+ 'fsf-19) (load "ilfsf19"))
      ((eq +ilisp-emacs-version-id+ 'fsf-20) (load "ilfsf20"))
      ((eq +ilisp-emacs-version-id+ 'fsf-21) (load "ilfsf21"))
      )


;;; Misc. bug work-arounds and compatibility bindings

(unless (eval-when-compile (ignore-errors (last '(a . b))))
  ;; From Emacs 19.34's cl.el.
  (defun last (x &optional n)
    "Returns the last link in the list LIST.
With optional argument N, returns Nth-to-last link (default 1)."
    (if n
        (let ((m 0) (p x))
          (while (consp p) (incf m) (pop p))
          (if (<= n 0) p
            (if (< n m) (nthcdr (- m n) x) x)))
      (while (consp (cdr x)) (pop x))
      x)))


(cond ((fboundp 'buffer-substring-no-properties)
       ;; nothing to do (emacs 19-21, xemacs 21)
       )
      ((fboundp 'set-text-properties)
       ;; I believe this is xemacs 20 and before
       (defun buffer-substring-no-properties (start end)
	 (let ((string (buffer-substring start end)))
	   (set-text-properties 0 (length string) nil string)
	   string)))
      (t
       ;; hope that buffer-substring works okay on this platform ;-)
       (defun buffer-substring-no-properties (start end)
	 (buffer-substring start end))))
      
  
;;; Epilogue

(provide 'ilcompat)

;;; end of file -- compat.el --
