;;; -*- Mode: Emacs-Lisp -*-

;;; ILD: A common Common Lisp debugger user interface for ILisp.
;;;   ---Jeffrey Mark Siskind

;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ild.el,v 1.7 2003/05/21 15:12:08 bill_clementson Exp $

;;; Keystroke c-u? What it does
;;; ---------------------------------------------------------
;;; m-a            Abort
;;; m-c            Continue
;;; c-m-n     *    Next stack frame
;;; c-m-p     *    Previous stack frame
;;; c-c <          Top stack frame
;;; c-c >          Bottom stack frame
;;; m-b            Backtrace
;;; c-m-d          Display all locals
;;; c-m-l     *    Display particular local
;;; c-c r          Return
;;; c-m-r          Retry
;;; c-x t          Trap on exit
;;; c-c L          Select Lisp interaction buffer
;;; c-z c-s        Sets compiler options for maximally debuggablity
;;; c-z c-f        Sets compiler options for fastest but least debuggable code

;;; Dependencies
;;; We really just need ILISP Key management.
;;; 19990615 Marco Antoniotti

;;; (require 'ilisp)
(require 'ilisp-key)

(deflocal ild-abort-string nil)
(deflocal ild-continue-string nil)
(deflocal ild-step-string nil)
(deflocal ild-step-string-arg nil)
(deflocal ild-next-string nil)
(deflocal ild-next-string-arg nil)
(deflocal ild-previous-string nil)
(deflocal ild-previous-string-arg nil)
(deflocal ild-top-string nil)
(deflocal ild-bottom-string nil)
(deflocal ild-backtrace-string nil)
(deflocal ild-locals-string nil)
(deflocal ild-local-string-arg nil)
(deflocal ild-return-string nil)
(deflocal ild-retry-string nil)
(deflocal ild-trap-on-exit-string nil)

(defun ild-debugger-command (string)
 (process-send-string (get-buffer-process (current-buffer))
		      (format "%s\n" string)))

(defun ild-prompt ()
 (save-excursion
  (beginning-of-line)
  (comint-skip-prompt)
  (eobp)))

(defun ild-abort ()
 (interactive)
 (if ild-abort-string
     (ild-debugger-command ild-abort-string)
     (beep)))

(defun ild-continue (&optional arg)
 (interactive "P")
 (if (ild-prompt)
     (if ild-continue-string
	 (ild-debugger-command ild-continue-string)
	 (beep))
     (if arg (capitalize-word arg) (capitalize-word 1))))

(defun ild-step (&optional arg)
 (interactive "P")
 (if arg
     (if ild-step-string-arg
	 (ild-debugger-command (format ild-step-string-arg arg))
	 (beep))
     (if ild-step-string
	 (ild-debugger-command ild-step-string)
	 (beep))))

(defun ild-next (&optional arg)
 (interactive "P")
 (if arg
     (if ild-next-string-arg
	 (ild-debugger-command (format ild-next-string-arg arg))
	 (beep))
     (if ild-next-string
	 (ild-debugger-command ild-next-string)
	 (beep))))

(defun ild-previous (&optional arg)
 (interactive "P")
 (if arg
     (if ild-previous-string-arg
	 (ild-debugger-command (format ild-previous-string-arg arg))
	 (beep))
     (if ild-previous-string
	 (ild-debugger-command ild-previous-string)
	 (beep))))

(defun ild-top (&optional arg)
 (interactive "P")
 (if ild-top-string
     (ild-debugger-command ild-top-string)
     (beep)))

(defun ild-bottom (&optional arg)
 (interactive "P")
 (if ild-bottom-string
     (ild-debugger-command ild-bottom-string)
     (beep)))

(defun ild-backtrace (&optional arg)
 (interactive "P")
 (if (ild-prompt)
     (if ild-backtrace-string
	 (ild-debugger-command ild-backtrace-string)
	 (beep))
     (if arg (backward-word arg) (backward-word 1))))

(defun ild-locals (&optional arg)
 (interactive "P")
 (if ild-locals-string
     (ild-debugger-command ild-locals-string)
     (beep)))

(defun ild-local (&optional arg)
 (interactive "P")
 (if arg
     (if ild-local-string-arg
	 (ild-debugger-command (format ild-local-string-arg arg))
	 (beep))
     (if ild-locals-string
	 (ild-debugger-command ild-locals-string)
	 (beep))))

(defun ild-return ()
 (interactive)
 (if ild-return-string
     (ild-debugger-command ild-return-string)
     (beep)))

(defun ild-retry ()
 (interactive)
 (if ild-retry-string
     (ild-debugger-command ild-retry-string)
     (beep)))

(defun ild-trap-on-exit (&optional arg)
 (interactive "P")
 (if ild-trap-on-exit-string
     (ild-debugger-command ild-trap-on-exit-string)
     (beep)))

(defun fast-lisp ()
 "Use the production compiler."
 (interactive)
 (ilisp-send "(progn (proclaim '(optimize (speed 3) (safety 0) (space 0) (compilation-speed 0) (debug 0))) #+akcl (use-fast-links t))"))

(defun slow-lisp ()
 "Use the development compiler."
 (interactive)
 (ilisp-send "(progn (proclaim '(optimize (speed 0) (safety 3) (space 3) (compilation-speed 3) (debug 3))) #+akcl (use-fast-links nil))"))

(defun select-lisp ()
  "Select the lisp buffer in one window mode"
  (interactive)
  (cond ((and (member* ilisp-buffer (buffer-list)
		       :key #'buffer-name
		       :test #'equal)
	      (get-buffer-process (get-buffer ilisp-buffer)))
	 (delete-other-windows)
	 (switch-to-buffer ilisp-buffer))
	(t (lucid)			; put your favorite Lisp here
	   (delete-other-windows))))

;;; This fixes a bug in ILISP 4.1
;;;
;;; Note:
;;; 19990818 Marco Antoniotti
;;; Fixed in the proper place.

;(defun defkey-ilisp (key command &optional inferior-only)
; "Define KEY as COMMAND in ilisp-mode-map and lisp-mode-map unless
;optional INFERIOR-ONLY is T.  If the maps do not exist they will be
;created.  This should only be called after ilisp-prefix is set to the
;desired prefix."
; (if (not ilisp-mode-map) (ilisp-bindings))
; (define-key ilisp-mode-map key command)
; (if (not inferior-only) (define-key lisp-mode-map key command)))

;;; This is a convenient command since c-Z c-W doesn't default to the whole
;;; buffer if there is no region

(defun compile-buffer ()
 "Compile the current buffer"
 (interactive)
 (compile-region-and-go-lisp (point-min) (point-max)))

(defkey-ilisp "\M-a"    'ild-abort         t  "\C-c\C-b\C-a")
(defkey-ilisp "\M-c"    'ild-continue      t  "\C-c\C-b\C-c")
(defkey-ilisp "\C-\M-s" 'ild-step          t  "\C-c\C-b\C-s")
(defkey-ilisp "\C-\M-n" 'ild-next          t  "\C-c\C-b\C-n")
(defkey-ilisp "\C-\M-p" 'ild-previous      t  "\C-c\C-b\C-p")
(defkey-ilisp "\C-c<"   'ild-top           t  "\C-c\C-b\C-v")
(defkey-ilisp "\C-c>"   'ild-bottom        t  "\C-c\C-b\C-w")
(defkey-ilisp "\M-b"    'ild-backtrace     t  "\C-c\C-b\C-b")
(defkey-ilisp "\C-\M-d" 'ild-locals        t  "\C-c\C-b\C-d")
(defkey-ilisp "\C-\M-l" 'ild-local         t  "\C-c\C-b\C-l")
(defkey-ilisp "\C-cr"   'ild-return        t  "\C-c\C-b\C-m")
(defkey-ilisp "\C-\M-r" 'ild-retry         t  "\C-c\C-b\C-r")
(defkey-ilisp "\C-xt"   'ild-trap-on-exit  t  "\C-c\C-b\C-x")
(ilisp-safe-define-key global-map "\C-cL" 'select-lisp "\C-c\C-va")
(ilisp-bind-ilisp-key-for-map lisp-mode-map  "\C-f" 'fast-lisp "\C-b\C-f")
(ilisp-bind-ilisp-key-for-map ilisp-mode-map "\C-f" 'fast-lisp "\C-b\C-f")
(ilisp-bind-ilisp-key-for-map lisp-mode-map  "\C-s" 'slow-lisp "\C-b\C-g")
(ilisp-bind-ilisp-key-for-map ilisp-mode-map "\C-s" 'slow-lisp "\C-b\C-g")

;;; end of file -- ild.el --
