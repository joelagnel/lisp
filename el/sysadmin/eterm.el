;;; Debugging info for self: Saved through ges-version 1.5dev
;;; From: Luke Gorrie <luke@bluetail.com>
;;; Subject: eterm.el 1.0
;;; Newsgroups: gnu.emacs.sources
;;; Date: 26 Jul 2002 16:23:24 +0200

;;; eterm --- Emacs alternative to xterm shells
;;; Version 1.0, July 2002, by Luke Gorrie <luke@bluetail.com>
;;; Written and tested in GNU Emacs 21

;; Emacs frames with shells, as an alternative to xterms.
;;
;; The main command `eterm' brings up a new frame with a new shell in
;; it, and closing the window kills the shell. `eterm-select' can get
;; you back to the shell after you go a'wanderin' to other buffers in
;; your eterm.
;;
;; If you have gnuserv installed, you might like to combine this with
;; an "eterm" shell script like:
;;
;;     #!/bin/sh
;;     gnudoit '(eterm)' >/dev/null
;;
;; And perhaps a Sawfish command:
;; 
;;     (defun eterm ()
;;       (interactive)
;;       (system "gnudoit '(eterm)' >/dev/null &"))

(defvar eterm-frame-width 80)
(defvar eterm-frame-height 26)

(defvar eterm-shell-buffer nil)
(make-variable-frame-local 'eterm-shell-buffer)

(defun eterm ()
  "Start a new \"eterm\".
This creates a frame and opens a new shell buffer in it. The shell is
automatically killed when the frame closes.

If you visit some other buffers from your eterm, you can get back to
the shell with `eterm-select'."
  (interactive)
  (select-frame (new-frame `((width  . ,eterm-frame-width)
			     (height . ,eterm-frame-height))))
  (let ((shell (shell (generate-new-buffer-name "*eterm*"))))
    ;; This is how you bind a frame-local variable
    (modify-frame-parameters (selected-frame)
			     `((eterm-shell-buffer . ,shell))))
  (make-local-variable 'delete-frame-hook)
  (add-hook 'delete-frame-hook
	    (lambda (frame)
	      (condition-case nil
		  (kill-buffer eterm-shell-buffer)
		(error nil))))
  (delete-other-windows)
  (bury-buffer (current-buffer)))

(defun eterm-select ()
  "Select the current frame's *eterm* buffer in the full window."
  (interactive)
  (if eterm-shell-buffer
      (progn (switch-to-buffer eterm-shell-buffer)
	     (delete-other-windows))
    (message "No eterm buffer for this frame.")))

(provide 'eterm)


