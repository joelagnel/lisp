;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-ccl.el --

;;; This file is part of ILISP.
;;; Version: 5.12
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996, 1997, 1998, 1999 Marco Antoniotti and Rick Campbell
;;;		  2000 Reini Urban
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'majordomo@cons.org' to be included in the
;;; ILISP mailing list. 'ilisp@cons.org' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.

;;;
;;; Dialect definition for Corman Common Lisp by Roger Corman 
;;; Since 1.4 (fixed with 1.41) there is a debugger with corman.

(require 'cl)

;;; cormanlisp --
;;;
;;; Notes:
;;; 2000-09-08 16:01:20 rurban
;;;   created, based on chs and acl
;;; Problems: 
;;;  * win32 pathdelims get lost on C-c l, but a manual load works.
;;;  * subsequent invocations load the next lisp (clisp in my case) 
;;;    instead of corman.

; Hint:
; Best is to load cl-ilisp.lisp, ilisp-pkg.lisp and cormanlisp.lisp 
; with all required corman patches into cormanlisp at first and save 
; the image.
;
; On cygwin (X)Emacs we have to convert the filenames passed to the lisp.
; ilisp-hi.el:   file-name-hack for elisp
; cl-ilisp.lisp: ilisp-w32-fix-filenames for lisp
; ilisp-cl-easy-menu.el
;   added a Debugger menu section for XEmacs (easy-menu)

; Old ILISP Patches for 5.11:
;   http://xarch.tu-graz.ac.at/autocad/lisp/cormanlisp/ilisp-ccl-5.11.zip
; Cormanlisp fixes: (required for at least CCL 1.5)
;   http://xarch.tu-graz.ac.at/autocad/lisp/cormanlisp/ccl-1.5-patches.zip

; Todo: custom vars for these.
; We really should query the registry. The dll is registered there, 
; so we would also know the version.
; A XEmacs dynamic w32reg emodule is almost ready:
;   http://xarch.tu-graz.ac.at/autocad/lsp_tools/ntemacs/emodules/w32reg/w32reg.c

; define these in ~/.ilisp
;(setq *cormanlisp-dir* "P:/CORMAN~1/CORMAN~1.5/")
(unless (boundp '*cormanlisp-dir*)
  (setq *cormanlisp-dir* "C:/PROGRAM~1/CORMAN~1/CORMAN~1.5/"))

(unless (boundp 'cormanlisp-program)
  (setq cormanlisp-program
	(concatenate 'string *cormanlisp-dir* "clconsole.exe" 
		     " -image " *cormanlisp-dir* "CormanLisp.img")))

;(defvar ilisp-cormanlisp-init-file
;	(concatenate 'string *cormanlisp-dir* "init.lisp"))
(defvar ilisp-cormanlisp-init-file "cormanlisp.lisp")

(defdialect cormanlisp "CormanLisp" common-lisp
  (ilisp-load-init 'ilisp-package-kludge ilisp-cl-ilisp-package-file)
  (ilisp-load-init 'common-lisp ilisp-cl-ilisp-init-file)
  (ilisp-load-init 'cormanlisp ilisp-cormanlisp-init-file)
  (setq
     ilisp-error-regexp  "\\(ILISP:[^\n]*\\)\\|\\(^;;; An error occurred\\)"
     ilisp-find-source-command  "(ilisp:ilisp-source-files \"%s\" \"%s\" \"%s\")"
     ilisp-reset "(debug::debugger-continue)"
     ilisp-block-command "(progn %s)"
     ;; cl overrides
     ilisp-inspect-command nil		 ; no inspector
     ilisp-load-no-compile-query t       ; don't ask "Compile first"
;     ilisp-binary-extension nil ; "fasl" ; avoid compilation
;     ilisp-compile-file-command nil      ; avoid compilation

;     ilisp-*use-frame-for-output* nil	  ; this should go to .ilisp
;     ilisp-bindings-*bind-space-p* nil    ; this should go to .ilisp
     ;; default
     comint-prompt-regexp "^?*"
     ;; comint-prompt-regexp "^\\([0-9]+\\. Break \\[[0-9]+\\]> \\|^[^>]*> \\)"
     comint-fix-error ":C 1"
     comint-continue ":C"
   )

  ;; ILD Support. NIL values mean that more work is needed or that the
  ;; particular command is not available

  (setq ild-abort-string ":QUIT"
	ild-continue-string ":C 1"
	ild-next-string ":NEXT"
	ild-next-string-arg nil
	ild-previous-string ":PREVIOUS"
	ild-previous-string-arg nil
	ild-top-string ":TOP"
	ild-bottom-string ":BOTTOM"
	ild-backtrace-string ":BACKTRACE"
	ild-locals-string nil
	ild-local-string-arg nil
	;ild-return-string "return"
	;ild-retry-string "redo"
	;ild-trap-on-exit-string "break+"
	))

(provide 'ilisp-ccl)

;;; end of file -- ilisp-chs.el --
