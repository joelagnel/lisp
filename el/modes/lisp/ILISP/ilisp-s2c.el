;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-s2c.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-s2c.el,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

;From: Jeffrey Mark Siskind <qobi@cs.toronto.edu>
;To: ilisp@cons.org
;Subject: ILisp 5.5 and Scheme->C
;Reply-To: Qobi@cs.toronto.edu
;Date: 	Thu, 15 Dec 1994 22:55:05 -0500

;Is anybody using ILisp 5.5 with Scheme->C? I don't know much about the
;internals of ILisp and have created a defdialect by analogy with the other
;defdialect forms.

(defdialect qsci "Qobi Scheme->C" scheme
 (setq comint-fix-error ":X"
       ilisp-reset ":A"
       comint-continue ":C"
       comint-interrupt-regexp ">>Interrupt:"
       ilisp-eval-command
       "(begin (eval (read (open-input-string \"%s\"))) \"%s\" \"%s\")"
       ilisp-package-command "%s"	;needs work
       ilisp-block-command "(begin %s)"
       ilisp-load-command "(loadq \"%s\")"
       ilisp-load-or-send-command "(begin \"%s\" (ld \"%s\"))"
       ild-abort-string ":A"
       ild-continue-string ":C"
       ild-next-string ":N"
       ild-previous-string ":P"
       ild-top-string ":<"
       ild-bottom-string ":>"
       ild-backtrace-string ":B")
 (ilisp-load-init 'qsci "/u/qobi/emacs/qsci"))

(cond ((or (equal (system-name) "qobi.ai")
           (equal (system-name) "dvp.cs")
           (equal (system-name) "qew.cs"))
       (setq qsci-program "/u/qobi/bin/sun4/5.3/qsci"))
      (t (setq qsci-program "/u/qobi/bin/sun4/4.1.2/qsci")))

;The strange thing is that sometimes it works and sometimes it doesn't. And I
;am having difficulty figuring out what I am doing wrong. I should mention that
;I am using a customized version of Scheme->C (qsci) that has my own debugger
;instead of the default one. My debugger provides Lucid-like commands for
;moving up and down the stack, displaying backtraces and locals, aborting,
;continuing, etc. I will give any interested party a copy of my enhancements to
;Scheme->C. I also use the debugger with ILD, my extension to ILisp 5.5 that
;provides a uniform set of single keystroke commands for accessing the
;different CommonLisp/Scheme debuggers. That explains the ild-* bindings above.

;Here are my questions: What are the appropriate values for comint-fix-error,
;ilisp-reset, comint-continue, comint-interrupt-regexp, ilisp-eval-command,
;ilsip-package-command, ilisp-block-command, ilisp-load-command,
;and ilisp-load-or-send-command. What exactly should these control strings do?
;What % arguments do they take. The minimum functionality I would like to have
;is the ILisp commands c-z l and c-z e. Later on I would like to add c-z a,
;m-TAB, and m-. but I realize that I'll need to add hooks in Scheme->C for
;these. I would ideally like to modify c-z D and c-z A to look things
;up in R4RS.

;    Jeff (home page http://www.cdf.toronto.edu/DCS/Personal/Siskind.html)

;;; end of file -- ilisp-scc.el --
