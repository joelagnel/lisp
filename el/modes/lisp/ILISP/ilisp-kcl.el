;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-kcl.el --
;;; ILISP Kyoto Common Lisp dialect definition and derivative.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-kcl.el,v 1.5 2002/02/24 16:02:23 amoroso Exp $

;;;%%%KCL--these dialects by Tom Emerson
;;; kcl-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

(defun kcl-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match ">+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match ">+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect kcl "Kyoto Common LISP" common-lisp
  (setq comint-prompt-regexp "^>+"
        ilisp-error-regexp "Error: [^\n]*"
        ilisp-binary-extension "o"
        ilisp-init-binary-extension "o"
	ilisp-binary-command "\"o\""
        comint-fix-error ":q"
        comint-continue ":r"
	comint-prompt-status
	(function
	 (lambda (old line)
	   (comint-prompt-status old line 'kcl-check-prompt)))))
(if (not kcl-program) (setq kcl-program "kcl"))

;;;%%%AKCL
(defdialect akcl "Austin Kyoto Common LISP" kcl
  (setq comint-prompt-regexp "^[-A-Z]*>+")

  ;; ILD Support

  (setq ild-abort-string ":q"
	ild-continue-string ":r"
	ild-next-string ":up"
	ild-next-string-arg ":up %s"
	ild-previous-string ":down"
	ild-previous-string-arg ":down %s"
	ild-top-string ":down 1000000"
	ild-bottom-string ":up 1000000"
	ild-backtrace-string ":bt"
	ild-locals-string ":fr"
	ild-local-string-arg ":loc %s"
	ild-return-string ":r"
	ild-retry-string nil		; needs work
	ild-trap-on-exit-string nil	; needs work
	)
   )
(if (not akcl-program) (setq akcl-program "akcl"))


;;;%%%IBCL
(defdialect ibcl "Ibuki Common LISP" kcl
  (setq comint-prompt-regexp "^[-A-Z]*>+\\|^[-A-Z]* ->"
        comint-interrupt-regexp ">>Condition: Terminal Interrupt"
        comint-continue ":q"
        ilisp-reset ":q!"
        ilisp-error-regexp ">>Error:[^\n]*"))
(if (not ibcl-program) (setq ibcl-program "ibcl"))


;;; GCL and ECL (at least) have slightly different compilers and
;;; runtimes, hence we need to provide different extensions for their
;;; init files.
;;; Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19951028.

;;; GCL -- I assume it is exactly as AKCL.
;;; Should check whether it is similar to IBUKI.

(defdialect gcl "GNU Common LISP" akcl
  (setq comint-prompt-regexp "^>+"
	ilisp-binary-extension "o"
        ilisp-init-binary-extension "gcl.o"
	ilisp-binary-command "\"o\""
	ilisp-init-binary-command "\"gcl.o\""
	)

  ;; ILD Support

  (setq ild-abort-string ":q"
	ild-continue-string ":r"
	ild-next-string ":up"
	ild-next-string-arg ":up %s"
	ild-previous-string ":down"
	ild-previous-string-arg ":down %s"
	ild-top-string ":down 1000000"
	ild-bottom-string ":up 1000000"
	ild-backtrace-string ":bt"
	ild-locals-string ":fr"
	ild-local-string-arg ":loc %s"
	ild-return-string ":r"
	ild-retry-string nil		; needs work
	ild-trap-on-exit-string nil	; needs work
	)
  )

(if (not gcl-program) (setq gcl-program "gcl"))


;;; ECL -- Beppe Attardi's developments over AKCL
;;;        Currently maintained by Juan Jose Garcia-Ripoll

(defdialect ecl "EcoLisp Common LISP" akcl
  (setq comint-prompt-regexp "^\\([A-Z].*\\)?>+ "
        ilisp-error-regexp "Broken at [^\n]*"
        comint-fix-error ":pop\n(progn (terpri) (values))") ; kludge

  ;; ILD Support.

  (setq ild-abort-string ":q"
	ild-continue-string ":r"
	ild-next-string ":up"
	ild-next-string-arg ":up %s"
	ild-previous-string ":down"
	ild-previous-string-arg ":down %s"
	ild-top-string ":down 1000000"
	ild-bottom-string ":up 1000000"
	ild-backtrace-string ":bt"
	ild-locals-string ":fr"
	ild-local-string-arg ":loc %s"
	ild-return-string ":r"
	ild-retry-string nil		; needs work
	ild-trap-on-exit-string nil	; needs work
	)
  )

(if (not ecl-program) (setq ecl-program "ecl"))

;;; end of file -- ilisp-kcl.el --
