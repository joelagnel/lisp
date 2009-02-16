;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mak.el --
;;; This file is used by make to compile ILISP.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-mak.el,v 1.11 2003/04/10 18:50:33 rgrjr Exp $

(require 'cl)

(message "ILISP Compilation: starting.")

(if (not (file-exists-p "ilcompat.el"))
    (error "ILISP Compilation: compatibility file 'ilcompat.el' non existent.")
  (progn
    (push "." load-path)
    (load "ilcompat.el")		; Need to load this beforehand
					; to use the +ilisp-emacs-version-id+
					; constant.
    (message ";;; Emacs Version %s" +ilisp-emacs-version-id+)

    (when (eq +ilisp-emacs-version-id+ 'xemacs)
      (setq load-path (cons "../xemacs-base" load-path)))

    (if (eq +ilisp-emacs-version-id+ 'fsf-18)
        (load "comint-v18")
      ;; (load "comint")
      (require 'comint))

    ;; Try to generate bytecodes for emacs 19.
    ;; I am no expert on the Byte Compiler.  Anyone who is please send
    ;; me mail.
    ;; Marco Antoniotti <marcoxa@icsi.berkeley.edu>

    ;; (if (eq +ilisp-emacs-version-id+ 'fsf-18)
    ;;	(setq byte-compile-emacs18-compatibility t))


    ;; Compile compatibility files
    
    (cond ((or (eq +ilisp-emacs-version-id+ 'lucid-19)
               (eq +ilisp-emacs-version-id+ 'lucid-19-new))
           (byte-compile-file "illuc19.el"))
          ((eq +ilisp-emacs-version-id+ 'xemacs)
           (byte-compile-file "ilxemacs.el"))
          ((eq +ilisp-emacs-version-id+ 'fsf-21)
           (byte-compile-file "ilfsf21.el"))
          ((eq +ilisp-emacs-version-id+ 'fsf-20)
           (byte-compile-file "ilfsf20.el"))
          ((eq +ilisp-emacs-version-id+ 'fsf-19)
           (byte-compile-file "ilfsf19.el"))
          ((eq +ilisp-emacs-version-id+ 'fsf-18)
           (byte-compile-file "ilfsf18.el"))
          (t (error "ILISP Compilation: unrecognized Emacs version %s"
                    +ilisp-emacs-version-id+)))
    (byte-compile-file "ilcompat.el")

    ;; Other files in the distribution.

    (let ((files '("completer"
		   "comint-ipc"
		   "bridge"
                   ;; not integrated yet!
		   ;; "custom-ilisp"
		   "ilisp-def"
		   "ilisp-sym"
		   "ilisp-inp"
		   "ilisp-ind"

		   "ilisp-mouse"
		   "ilisp-prc"
		   "ilisp-val"
		   "ilisp-out"
		   "ilisp-mov"
		   "ilisp-key"
		   "ilisp-prn"
		   "ilisp-low"
		   "ilisp-doc"
		   "ilisp-ext"
		   "ilisp-mod"
		   "ilisp-dia"
		   "ilisp-cmt"
		   "ilisp-rng"
		   "ilisp-hnd"
		   "ilisp-utl"
		   "ilisp-cmp"
		   "ilisp-kil"
		   "ilisp-snd"
		   "ilisp-xfr"
		   "ilisp-hi"
		   "ilisp-aut"
		   "ilisp-mnb"
		   "ilisp-src"

		   ;; ILD Support.
		   "ild"

		   ;; Dialects.
		   "ilisp-cl"
		   "ilisp-ccl"
		   "ilisp-cmu"
		   "ilisp-sbcl"
		   "ilisp-chs"
		   "ilisp-acl"
		   "ilisp-kcl"
		   "ilisp-hlw"
		   "ilisp-luc"
		   "ilisp-xls"
		   "ilisp-openmcl"
		   "ilisp-sch"
		   "ilisp-cl-easy-menu"
		   "ilisp-scheme-easy-menu"
		   "ilisp-imenu"
		   "extra/hyperspec"
		   "extra/cltl2"

		   )))
      (dolist (f files)
        (byte-compile-file (format "%s.el" f) 0))
      ;;Main mode file
      (byte-compile-file "ilisp.el")
      (message "Done compiling and loading ILISP."))))

;;; end of file -- ilisp-mak.el --
