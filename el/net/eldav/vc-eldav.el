;;; vc-eldav.el --- VC backend for eldav.

;; Copyright (C) 2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: WebDAV

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commenty:
;;

;;; Commentary:
;;

(require 'eldav)

;;; Code:

;; vc backend
(defun vc-eldav-state (file)
  (if (file-exists-p file)
      (let* ((eldav-inhibit-propfind-cache t)
	     (locks (eldav-lockinfo-list (eldav-file-name-url file))))
	(cond
	 ((null locks) 'up-to-date)
	 ((member eldav-lock-identifier
		  (mapcar 'eldav-lockinfo-owner locks))
	  'edited)
	 (t (eldav-lockinfo-owner (car locks)))))
    'up-to-date))

(defun vc-eldav-checkout-model (file)
  'locking)

(defun vc-eldav-workfile-version (file)
  "1.0")

(defun vc-eldav-checkin (file rev comment)
  ;; Release.
  (let ((url (eldav-file-name-url file)))
    (dolist (lockinfo (eldav-lockinfo-list-search 'owner eldav-lock-identifier
						  (eldav-lockinfo-list url)))
      (eldav-unlock url (eldav-lockinfo-token lockinfo)))))

(defun vc-eldav-checkout (file &optional editable rev destfile)
  ;; LOCK the file.
  (eldav-lock (eldav-file-name-url file)))

(defun vc-eldav-revert (file &optional contents-done)
  (unless contents-done
    (insert-file-contents file)))

(defun vc-eldav-print-log (file)
  ;; do nothing.
  )

(defun vc-eldav-diff (file &optional rev1 rev2)
  ;; do nothing.
  )

(defun vc-eldav-workfile-unchanged-p (file)
  )

(defun vc-eldav-dav-responsible-p (url)
  t)

(defun vc-dav-could-register (url)
  t)

(defun vc-eldav-steal-lock (file rev)
  (let ((url (eldav-file-name-url file)))
    (dolist (lockinfo (eldav-lockinfo-list url))
      (eldav-unlock url (eldav-lockinfo-token lockinfo)))))

(provide 'vc-eldav)

;;; vc-eldav.el ends here
