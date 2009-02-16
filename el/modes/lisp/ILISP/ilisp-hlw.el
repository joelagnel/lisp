;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-hlw.el --
;;; ILISP LispWorks Common Lisp dialect definition
;;;
;;; Independently written by:
;;;
;;; Jason Trenouth: jason@harlequin.co.uk
;;; Qiegang Long: qlong@cs.umass.edu
;;;
;;; Upgraded for LW 4 and bugs fixed by Pekka P. Pirinen
;;; (pekka@harlequin.co.uk) and later merged together by Jason.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-hlw.el,v 1.3 2002/08/23 21:40:09 anisotropy9 Exp $


(defvar ilisp-lispworks-init-file "lispworks.lisp")

;; may use Qiegang's instead? "[-A-Z]+ [0-9]+ : \\([0-9]+\\) >"

(defun lispworks-break-level (prompt)
  (let ((position nil))
    (if (and prompt (setq position (string-match ": [0-9]+" prompt)))
	(string-to-int (substring prompt (+ 2 position)))
      0)))


;;; lispworks-check-prompt --
;;;
;;; Notes:
;;; 19990806 Marco Antoniotti
;;; Many Changes form Harlequin and other sources have been
;;; included. Mostly dealing with LW 4.x.

(defun lispworks-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (<= (lispworks-break-level new) (lispworks-break-level old)))

;;; Qiegang's prompt matcher "^\\([-A-Z]+ [0-9]+ >\\)\\|\\([-A-Z]+ [0-9]+ : [0-9]+ >\\) "
;;; Qiegang's error matcher "\\(ILISP:[^\"]*\\)\\|\\(Error: [^\n]*\\)\\|\\(Break.[^\n]*\\)"

(defdialect lispworks "LispWorks"
  common-lisp
  (ilisp-load-init 'lispworks ilisp-lispworks-init-file)
  (setq comint-fix-error ":a"
	;; ilisp-reset ":a" ; LW doesn't have a multi-level abort yet.
	ilisp-reset ":a :t" ; LW 4 (In LW 3.2 this only anorts one level)
	comint-continue ":c"
	comint-interrupt-regexp  "Break\\.\n.*")
  (setq comint-prompt-status 
	(function (lambda (old line)
	  (comint-prompt-status old line 'lispworks-check-prompt))))
  ;; <cl> or package> at top-level
  ;; [0-9c] <cl> or package> in error
  ;; (setq comint-prompt-regexp "^\\(\\[[0-9]*c*\\] \\|\\)\\(<\\|\\)[^>]*> ")
  (setq comint-prompt-regexp "^[A-Z=][-a-z0-9A-Z:= ]*[$%#>]+ *") 

  ;; 19990806 Marco Antoniotti
  ;; You may want to use the commented one instead.
  ;; (setq ilisp-error-regexp "ILISP:[^\"]*\\|Error: [^\n]*\\|Break\\.[^\n]*")
  (setq ilisp-error-regexp "ILISP [0-9]* : [0-9]* > ")

  (setq ilisp-source-types (append ilisp-source-types '(("any"))))
  (setq ilisp-directory-command "(lw:current-pathname)")
  (setq ilisp-set-directory-command "(hcl:change-directory \"%s\")")
  (setq ilisp-find-source-command 
	"(ilisp:ilisp-source-files \"%s\" \"%s\" \"%s\")")

  ;;; 19990806 Unknown Author (blame Marco Antoniotti for this)
  (setq ilisp-package-command
 	"(let ((*package* *package*)
                #+LispWorks3 (lw::*handle-old-in-package* :quiet)
                #-LispWorks3 (hcl:*handle-old-in-package* :quiet))
            %s (package-name *package*))")

  ;;; 19990806 Unknown Author (blame Marco Antoniotti for this)
  ;;;
  ;;; Notes:
  ;;; 19990806 Marco Antoniotti
  ;;; I inserted these here, but I have not throughly checked them.
  ;;; In particular, the value for ILISP-RESTORE-COMMAND looks funny.
  (setq ilisp-save-command ":ilisp-send %s"
	ilisp-restore-command (function (lambda ())))

  ;; Note:
  ;; 19990920
  ;; The global definition should now take care to find out the
  ;; proper extension.  See file 'ilisp-cl.el'.
  ;; (setq ilisp-binary-command "system::*binary-file-type*")
  ;; (setq ilisp-init-binary-command "system::*binary-file-type*")
  
  )


(unless lispworks-program
  (setq lispworks-program "lispworks"))

(provide 'ilisp-lw)

;;; end of file -- ilisp-hlw.el --
