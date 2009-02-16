;;; mew-gemacs.el --- Environment of Graphical Emacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jun 22, 2000

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

(defun mew-toolbar-make (map alist)
  (let ((tool-bar-map (make-sparse-keymap)) ;; for tool-bar-add-item-from-menu
	(data-directory mew-icon-directory)
	a)
    (while alist
      (setq a (car alist))
      (setq alist (cdr alist))
      (if (fboundp 'tool-bar-local-item-from-menu)
	  ;; Emacs 21.3.50 or later
	  (tool-bar-local-item-from-menu (car a) (cdr a) tool-bar-map map)
	;; Emacs 21.3 or earlier
	;; The target map is tool-bar-map
	(tool-bar-add-item-from-menu (car a) (cdr a) map)))
    tool-bar-map))

(defun mew-summary-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-summary-mode-map mew-summary-toolbar-spec))))

(defun mew-message-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-message-mode-map mew-message-toolbar-spec))))

(defun mew-draft-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-draft-mode-map mew-draft-toolbar-spec))))

(defun mew-header-setup-decoration ()
  (if mew-icon-p
      (set (make-local-variable 'tool-bar-map)
	   (mew-toolbar-make mew-header-mode-map mew-header-toolbar-spec))))

(defsubst mew-summary-toolbar-update ())
(defsubst mew-message-toolbar-update ())
(defsubst mew-draft-toolbar-update ())
(defsubst mew-header-toolbar-update ())

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
  ;; display-graphic-p
  (and window-system (image-type-available-p format)))

(defun mew-mime-image (cache begin end format)
  (message "Loading image...")
  (set-buffer (mew-buffer-message))
  (mew-elet
   (condition-case nil
       (insert-image
	(create-image (save-excursion
			(set-buffer cache)
			(string-as-unibyte
			 (mew-buffer-substring begin end)))
		      format t))
     (error ())))
  (goto-char (point-min))
  (message "Loading image...done"))

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
  (create-image
   (string-as-unibyte (mew-buffer-substring (point-min) (point-max)))
   nil t))

(defun mew-x-face-display (xface)
  (save-excursion
    (goto-char (point-min))
    (let ((regex2 (concat "^\\(" mew-from: "\\).*")))
      (when (re-search-forward regex2 nil t)
	(goto-char (match-end 1))
	(insert-image xface)))))

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

(defvar mew-secure-format nil)

;; Emacs 21.x has a bug that an image cannot be displayed 
;; if it is specified in mode-line-process.

(defvar mew-secure-format2
  (if (display-graphic-p)
      (let ((data-directory mew-icon-directory))
	(concat " " (propertize "Sec" 'display
				(find-image '((:type xpm :file "mew-lock.xpm" :ascent center))))))
    " [Sec]"))

(provide 'mew-gemacs)

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

;;; mew-gemacs.el ends here
