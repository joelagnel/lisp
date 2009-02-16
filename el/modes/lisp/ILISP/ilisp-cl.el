;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cl.el --
;;; ILISP Common Lisp dialect definition
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-cl.el,v 1.10 2002/09/24 21:39:53 anisotropy9 Exp $

(defvar ilisp-cl-ilisp-package-file "ilisp-pkg.lisp")

(defvar ilisp-cl-ilisp-init-file "cl-ilisp.lisp")


;;; common-lisp --
;;;
;;; Notes:
;;; 19990806 Martin Atzmueller
;;; Added several package related entries.
;;;
;;; 19990806 Marco Antoniotti
;;; MAJOR CHANGE. Changed the name of the dialect to COMMON-LISP

(defdialect common-lisp "Common LISP"
  ilisp
  (setq ilisp-load-or-send-command 
	"(or (and (load \"%s\" :if-does-not-exist nil) t)
             (and (load \"%s\" :if-does-not-exist nil) t))")

  ;; The following line is an incredible kludge to bypass the behavior
  ;; of ilisp-load-init and to stick the package file in front of
  ;; everything.
  ;; Check what ilisp-load-init does to understand why I am forced to
  ;; do this.
  ;; Marco Antoniotti 19941122
  (ilisp-load-init 'ilisp-package-kludge ilisp-cl-ilisp-package-file)

  ;; 19990912 Marco Antoniotti
  ;; Changed the argument below from 'clisp to 'common-lisp.
  (ilisp-load-init 'common-lisp ilisp-cl-ilisp-init-file)
  (setq ilisp-package-separator-regexp
	":+"

	ilisp-package-command
	;;; "(nth-value 0 (ignore-errors (let ((*package* *package*)) %s (package-name *package*))))"
          "(let ((*package* *package*)) (nth-value 0 (ignore-errors %s (package-name *package*))))"

        ilisp-no-package-in-core-regexp
        "^nil"

        ilisp-fallback-package
        ':common-lisp-user

	ilisp-in-package-command-string
	"in-package"
         
	ilisp-defpackage-command-string
	"defpackage"

	ilisp-package-name-command
	"(package-name *package*)"

	ilisp-in-package-command
	"(in-package %S)"

         ilisp-hash-form-regexp
         "\\(^[ \t]*#[+-].\\)\\|\\(^[ \t]*(\\(.*::?\\)?defpackage[ \t\n]\\)\\|\\(^[ \t]*(\\(.*::?\\)?in-package[ \t\n]*\\)"

	ilisp-last-command
	"*"

	ilisp-save-command
	"(progn (ilisp:ilisp-save) %s\n)"

	ilisp-restore-command
	"(ilisp:ilisp-restore)"

	ilisp-block-command
	"(progn %s\n)"

	ilisp-eval-command
	"(ilisp:ilisp-eval \"%s\" \"%s\" \"%s\")"

	ilisp-defvar-regexp
	"(defvar[ \t\n]")

  (setq ilisp-defvar-command 
	"(ilisp:ilisp-eval \"(let ((form '%s)) (progn (makunbound (second form)) (eval form)))\" \"%s\" \"%s\")")

  (setq ilisp-compile-command
	"(ilisp:ilisp-compile \"%s\" \"%s\" \"%s\")"

	ilisp-describe-command
	"(ilisp:ilisp-describe \"%s\" \"%s\")"

	ilisp-inspect-command
	"(ilisp:ilisp-inspect \"%s\" \"%s\")"

	ilisp-arglist-command
	"(ilisp:ilisp-arglist \"%s\" \"%s\")")

  (setq ilisp-documentation-types
	'(("function") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))

  (setq ilisp-documentation-command
	"(ilisp:ilisp-documentation \"%s\" \"%s\" \"%s\")")

  (setq ilisp-macroexpand-1-command 
	"(ilisp:ilisp-macroexpand-1 \"%s\" \"%s\")")

  (setq ilisp-macroexpand-command
	"(ilisp:ilisp-macroexpand \"%s\" \"%s\")")

  (setq ilisp-complete-command 
	"(ilisp:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")

  (setq ilisp-locator 'lisp-locate-clisp)

  (setq ilisp-source-types 
	'(("function") ("macro") ("variable")
	  ("structure") ("type")
	  ("setf") ("class")
	  ("(qualifiers* (class ...))")))

  (setq ilisp-callers-command
	"(ilisp:ilisp-callers \"%s\" \"%s\")"

	ilisp-trace-command
	"(ilisp:ilisp-trace \"%s\" \"%s\" \"%s\")"

	ilisp-untrace-command
	"(ilisp:ilisp-untrace \"%s\" \"%s\")")

  (setq ilisp-directory-command
	"(namestring *default-pathname-defaults*)"

	ilisp-set-directory-command
	"(setq *default-pathname-defaults* (parse-namestring \"%s\"))")

  ;; Note:
  ;; 19990912 Marco Antoniotti
  ;; This is probably the best and simplest way to fix things.
  ;; cfr. Hannu Koivisto's posting on 'ilisp@cons.org'.

  (setf ilisp-binary-command
	"(pathname-type (compile-file-pathname \"ILISP-DUMMY-STRING\"))"

	ilisp-init-binary-command ilisp-binary-command)

  (setq ilisp-load-command
	"(load \"%s\")")

  (setq ilisp-compile-file-command 
	"(ilisp:ilisp-compile-file \"%s\" \"%s\")")

  (setq ilisp-print-info-message-command
	"(ilisp:ilisp-print-info-message \"%s\" \"%s\")" ))

;;; end of file -- ilisp-cl.el --
