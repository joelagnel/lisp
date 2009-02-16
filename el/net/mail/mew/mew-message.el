;;; mew-message.el --- Message mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message info
;;;

(defvar mew-minfo-list '("summary" "eom" "veil-to" "veil-cc"))

(mew-blinfo-defun 'mew-minfo mew-minfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message mode
;;;

(defun mew-message-mode ()
  "\\<mew-message-mode-map>
Mew Message mode:: major mode for display a message.
The keys that are defined for this mode are:

\\[mew-message-next-page]	Scroll up this message.
\\[mew-message-prev-page]	Back-scroll this message.
\\[mew-message-next-msg]	Display a message or a part below.
\\[mew-message-prev-msg]	Display a message or a part above.
\\[mew-message-goto-summary]	Get back to Summary mode.
\\[mew-message-reply]	Answer to this message. A new draft is prepared in Draft mode. 
	Mew automatically decides To: and Cc:.
\\[mew-message-reply-with-citation]	Answer to this message. A new draft is prepared in Draft mode. 
	Mew automatically decides To: and Cc: and cites the body.
\\[mew-message-forward]	Forward this message to a third person. A new draft is prepared in 
	Draft mode and this message is automatically attached.
"
  (interactive)
  (setq major-mode 'mew-message-mode)
  (setq mode-name mew-mode-name-message)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-message-mode-map)
  (setq buffer-read-only t)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter mew-page-delimiter)
  (mew-message-setup-decoration)
  (mew-message-toolbar-update)
  (run-hooks 'mew-message-mode-hook))

(defun mew-message-mode-line (fld msg)
  (unless msg
    (setq msg (mew-current-get-msg (mew-frame-id))))
  (when msg
    (setq mode-line-process (format " %s/%s" fld msg))))

(defun mew-message-next-page (&optional lines)
  "Scroll up this message. Return 'nil' if more pages. Otherwise, return 't'."
  (interactive)
  (move-to-window-line -1)
  (if (save-excursion
        (end-of-line)
        (and (pos-visible-in-window-p) (eobp)))
      ;; Nothing in this page.
      (if (or (null mew-break-pages)
	      (save-excursion
		(save-restriction
		  (widen) (forward-line) (eobp)))) ;; Real end of buffer?
	  t
	;; Go to the next page.
	(mew-message-narrow-to-page 1)
	nil)
    ;; More in this page.
    (condition-case nil
	(scroll-up lines)
      (end-of-buffer
       (goto-char (point-max))
       (message "End of buffer")))
    nil))

(defun mew-message-prev-page (&optional lines)
  "Back-scroll this message. Return 'nil' if more pages. 
Otherwise, return 't'."
  (interactive)
  (move-to-window-line 0)
  (if (save-excursion
	(beginning-of-line)
	(and (pos-visible-in-window-p) (bobp)))
      ;; Nothing in this page.
      (if (or (null mew-break-pages)
	      (save-restriction
		(widen) (bobp))) ;; Real beginning of buffer?
	  t
	;; Go to the previous page.
	(mew-message-narrow-to-page -1)
	nil)
    ;; More in this page.
    (condition-case nil
	(scroll-down lines)
      (beginning-of-buffer
       (goto-char (point-min))
       (message "Beginning of buffer")))
    nil))

(defun mew-message-narrow-to-page (&optional arg)
  (setq arg (if arg (prefix-numeric-value arg) 0))
  (save-excursion
    (condition-case nil
	(forward-page -1) ;; Beginning of the current page.
      (beginning-of-buffer ()))
    (widen)
    (cond
     ((> arg 0)	(forward-page arg))
     ((< arg 0) (forward-page (1- arg))))
    (forward-page)
    (let* ((end (point)))
      (forward-page -1)
      (if (and (eolp) (not (bobp)))
	  (forward-line))
      (narrow-to-region (point) end))))

(defun mew-message-goto-summary ()
  "Get back to Summary mode."
  (interactive)
  (let* ((fld (mew-minfo-get-summary))
	 (msg (mew-current-get-msg (mew-frame-id)))
	 (part (mew-current-get-part (mew-frame-id))))
    (if (not (get-buffer fld))
	(message "No Summary mode for %s" fld)
      (if (get-buffer-window fld)
	  (select-window (get-buffer-window fld))
	(mew-summary-switch-to-folder fld))
      (cond
       ((and msg part (mew-decode-syntax-p))
	(setq part (concat "^.. +"
			   (regexp-quote (mapconcat 'int-to-string part "."))
			   " "))
	(goto-char (mew-decode-syntax-begin))
	(re-search-forward part (mew-decode-syntax-end) t)
	(beginning-of-line)
	(mew-summary-display nil))
       (msg
	(mew-summary-move-and-display msg))))))

(defun mew-message-reply ()
  "Answer to this message. A new draft is prepared in Draft mode. 
Mew automatically decides To: and Cc:."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-reply))

(defun mew-message-reply-with-citation ()
  "Answer to this message. A new draft is prepared in Draft mode. 
Mew automatically decides To: and Cc: and cites the body."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-reply-with-citation))

(defun mew-message-forward ()
  "Forward this message to a third person. A new draft is prepared in 
Draft mode and this message is automatically attached."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-forward))

(defun mew-message-resend ()
  "\\<mew-message-mode-map>
Resend this message with Resent-To:. It is strongly 
discouraged to use this command since beginners are always 
confused. Please use '\\[mew-message-forward]' instead."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-resend))
 
(defun mew-message-next-msg (&optional arg)
  "Display a message or a part below."
  (interactive "p")
  (let* ((swin (previous-window))
	 (mbuf (current-buffer)))
    (select-window swin) ;; for the next forward-line
    (when (mew-summary-or-virtual-p)
      (forward-line arg) ;; minus arg results in prev
      (mew-summary-display 'redisplay))
    ;; for window config
    (select-window (get-buffer-window mbuf))))
      
(defun mew-message-prev-msg (&optional arg)
  "Display a message or a part above."
  (interactive "p")
  (mew-message-next-msg (- arg)))

(provide 'mew-message)

;;; Copyright Notice:

;; Copyright (C) 1996-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-message.el ends here
