;;; mew-toolbar-frame.el	-*-Emacs-Lisp-*-
;;;
;;; Commentary:
;;; Run Mew from toolbar in a separate frame.
;;;
;;; Keywords:
;;; Mew Xemacs ToolBar Frame
;;;
;;; Time-stamp: <99/07/02 16:37:17 jado@sophia>

;;; How to use:
;;; Require it when initialize.

;;; Code:

(provide 'mew-toolbar-frame)

;;;

(setq toolbar-mail-reader 'Mew)
(setq toolbar-mail-commands-alist
      (cons '(Mew . toolbar-mew) toolbar-mail-commands-alist))

(defvar toolbar-mail-use-separate-frame t
  "*Whether Mew is invoked in a separate frame.")
(defvar toolbar-mail-frame nil
  "The frame in which Mew is displayed.")
(defvar toolbar-mail-frame-plist nil
  "*The properties of the frame in which mail is displayed.")
(define-obsolete-variable-alias 'toolbar-mail-frame-properties
  'toolbar-mail-frame-plist)

(defun toolbar-mew ()
  "Run Mew in a separate frame."
  (interactive)
  (if (not toolbar-mail-use-separate-frame)
      (mew)
    (unless (frame-live-p toolbar-mail-frame)
      (setq toolbar-mail-frame (make-frame toolbar-mail-frame-plist))
      (add-hook 'mew-suspend-hook
	(lambda ()
	  (when (frame-live-p toolbar-mail-frame)
	    (if (cdr (frame-list))
		(delete-frame toolbar-mail-frame))
	    (setq toolbar-mail-frame nil))))
      (add-hook 'mew-quit-hook
	(lambda ()
	  (when (frame-live-p toolbar-mail-frame)
	    (if (cdr (frame-list))
		(delete-frame toolbar-mail-frame))
	    (setq toolbar-mail-frame nil))))
      (select-frame toolbar-mail-frame)
      (mew))
    (when (framep toolbar-mail-frame)
      (when (frame-iconified-p toolbar-mail-frame)
        (deiconify-frame toolbar-mail-frame))
      (select-frame toolbar-mail-frame)
      (raise-frame toolbar-mail-frame))))

;;; Copyright Notice:

;; Copyright (C) 1999 Mew developing team.
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

;;; mew-toolbar-frame.el ends here
