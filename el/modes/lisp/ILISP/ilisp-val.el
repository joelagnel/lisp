;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-val.el --
;;; ILISP buffer value interface
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-val.el,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

(defun ilisp-value (variable &optional no-error-p)
  "Return the value of VARIABLE in the ILISP buffer.
If NO-ERROR-P is NIL, then an error will be signalled if VARIABLE is nil."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (let ((value (eval variable)))
      (if value
	  value
	  (if no-error-p
	      nil
	      (error "%s is not defined." variable))))))


(defun set-ilisp-value (variable value)
  "Set the value of VARIABLE in the ILISP buffer."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (set variable value)))

;;; end of file -- ilisp-val.el --
