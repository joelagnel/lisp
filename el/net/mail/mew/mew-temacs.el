;;; mew-temacs.el --- Environment of Text Emacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 13, 1997

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menu setting
;;;

(easy-menu-define
 mew-summary-mode-menu
 mew-summary-mode-map
 "Menu used in Summary mode."
 mew-summary-mode-menu-spec)

(easy-menu-define
 mew-message-mode-menu
 mew-message-mode-map
 "Menu used in Message mode."
 mew-message-mode-menu-spec)

(easy-menu-define
 mew-draft-mode-menu
 mew-draft-mode-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

(easy-menu-define
 mew-header-mode-menu
 mew-header-mode-map
 "Menu used in Header mode."
 mew-header-mode-menu-spec)

(easy-menu-define
 mew-draft-header-menu
 mew-draft-header-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

(easy-menu-define
 mew-draft-attach-menu
 mew-draft-attach-map
 "Menu used in Draft mode."
 mew-draft-mode-menu-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toolbar functions
;;;

(defvar mew-summary-toolbar nil)
(defvar mew-draft-toolbar nil)

(defun mew-summary-setup-decoration () nil)
(defun mew-message-setup-decoration () nil)
(defun mew-draft-setup-decoration   () nil)
(defun mew-header-setup-decoration  () nil)

(defsubst mew-summary-toolbar-update () nil)
(defsubst mew-message-toolbar-update () nil)
(defsubst mew-draft-toolbar-update   () nil)
(defsubst mew-header-toolbar-update  () nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of messages
;;;

(defsubst mew-message-set-end-of-message ()
  (overlay-put (mew-minfo-get-eom) 'before-string mew-end-of-message-string))

(defsubst mew-message-set-end-of-part ()
  (overlay-put (mew-minfo-get-eom) 'before-string mew-end-of-part-string))

(defsubst mew-message-set-end-of-nil ()
  (overlay-put (mew-minfo-get-eom) 'before-string nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Veil
;;;

(defun mew-header-veil-make ()
  (let ((ov (mew-overlay-make 1 1)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'before-string mew-header-veil-string)
    (delete-overlay ov) ;; detach from the buffer
    ov))

(defun mew-toggle-header-veil (ov)
  (cond
   ((overlay-get ov 'invisible)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'before-string nil))
   (t
    (overlay-put ov 'invisible t)
    (overlay-put ov 'before-string mew-header-veil-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image
;;;

(defun mew-image-inline-p (format)
  nil)

(defun mew-mime-image (cache begin end format)
  ;; dummy
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transient
;;;

(defun mew-mark-active-p ()
  (and transient-mark-mode mark-active))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X Face
;;;

(defun mew-x-face-create ()
  )

(defun mew-x-face-display (xface)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Profile
;;;

(defun mew-profile-init ()
  (when (and mew-profile mew-profile-functions-list)
    (require 'profile)
    (let ((profile-functions-list mew-profile-functions-list))
      (profile-functions))))

(defun mew-profile-results ()
  (if (and mew-profile mew-profile-functions-list)
      (let ((profile-buffer mew-buffer-debug))
	(profile-results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/SSH/TLS notification
;;;

(defvar mew-secure-format " [Sec]")
(defvar mew-secure-format2 nil)

(provide 'mew-temacs)

;;; Copyright Notice:

;; Copyright (C) 1997-2005 Mew developing team.
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

;;; mew-temacs.el ends here
