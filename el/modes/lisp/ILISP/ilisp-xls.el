;;; -*-Mode: Emacs-Lisp-*-

;;; ilisp-xls.el --
;;; ILISP Xlisp and Xlisp-Stat dialect definition
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-xls.el,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

;;; Thanks to John Walker for supplying this file.


(defdialect xlisp "Xlisp" ilisp
  (setq ilisp-load-command "(load \"%s\")"
        ilisp-last-command "*")
  )

(if (not xlisp-program) (setq xlisp-program "xlisp"))

;;;%%Xlisp-Stat

(defdialect xlispstat "Xlisp-Stat" xlisp
  (setq ilisp-binary-extension "fsl"
	;; ilisp-describe-command "(help %s)"))
	))

(if (not xlispstat-program) (setq xlispstat-program "xlispstat"))

;;; endo of file -- ilisp-xls.el --
