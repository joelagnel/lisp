;;; message-multiple-frames.el --- enable to use multiple message frames

;; Copyright (C) 2002, 2003, 2004, 2005, 2006 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: gnus, message, frame

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

;;; Commentary:

;; For instance, install this file in the directory somewhere which is
;; listed in `load-path', put the following line in your ~/.gnus.el file:
;;
;;(require 'message-multiple-frames)
;;
;; and you will be able to use multiple message frames.  If you use
;; gnuclient to launch the message frame, set the `gnuserv-frame'
;; variable to t or use the "-batch" option to the gnuclient command.
;; Nothing special is needed for using emacsclient.

;;; Code:

(require 'gnus-win)
(require 'message)

(eval-when-compile
  (defvar gnus-delay-header)
  (defvar mml-preview-buffer))

(let* ((default
	 ;; Winodw layout for normal message frames.
	 '(vertical
	   ((width . 80) (height . 40)
	    ;;(left . -1) (top . 1)
	    (user-position . t))
	   (message 1.0 point)))
       (bug
	;; Window layout for a gnus-bug frame.
	;; Note that multiple gnus-bug frames are not supported.
	'(vertical
	  ((width . 80) (height . 40)
	   ;;(left . -1) (top . 1)
	   (user-position . t))
	  (if gnus-bug-create-help-buffer '("*Gnus Help Bug*" 0.5))
	  ("*Gnus Bug*" 1.0 point)))
       (config
	`(frame
	  1.0
	  (progn
	    (setq gnus-frame-list nil) ;; 2004-03-23
	    (if (buffer-live-p gnus-summary-buffer)
		(if (get-buffer gnus-article-buffer)
		    (car (cdr (assq 'article gnus-buffer-configuration)))
		  (car (cdr (assq 'summary gnus-buffer-configuration))))
	      (car (cdr (assq 'group gnus-buffer-configuration)))))
	  ,default))
       (settings '(compose-bounce forward mail-bounce message post
				  reply reply-yank)))
  (while settings
    (gnus-add-configuration (list (car settings) config))
    (setq settings (cdr settings)))
  (setcdr (nthcdr 2 (setq config (copy-sequence config))) (list bug))
  (gnus-add-configuration (list 'bug config)))

(defvar message-delete-frame-anyway t
  "*Non-nil means frame deletion is done even if there are other windows.")

;; 2005-04-28
(defadvice message-pop-to-buffer (before pop-to-gnus-frame activate)
  "Make sure the Gnus frame exists and is selected."
  (let ((window (selected-window))
	windows)
    (if window
	(unless (with-current-buffer (window-buffer window)
		  (or (memq major-mode '(gnus-group-mode
					 gnus-summary-mode
					 gnus-article-mode))
		      ;; 2006-06-15
		      (string-equal (buffer-name) "*Gnus Help Bug*")))
	  (save-excursion
	    (walk-windows
	     (lambda (w)
	       (set-buffer (window-buffer w))
	       (cond ((eq major-mode 'gnus-summary-mode)
		      (push (cons 0 w) windows))
		     ((eq major-mode 'gnus-article-mode)
		      (push (cons 1 w) windows))
		     ((eq major-mode 'gnus-group-mode)
		      (push (cons 2 w) windows))))
	     'ignore-minibuf 'visible))
	  (select-frame (if (setq window (cdar (sort windows
						     'car-less-than-car)))
			    (window-frame window)
			  (make-frame))))
      (select-frame (make-frame)))))

(defun message-kill-buffer-and-frame ()
  "Kill the current message buffer and the dedicated frame."
  (interactive)
  (let ((actions
	 (cons
	  `(with-current-buffer ,(current-buffer)
	     (setq buffer-file-name nil)
	     (kill-buffer (current-buffer))
	     ,(if (or message-delete-frame-anyway
		      (eq (selected-window) (next-window)))
		  (list 'delete-frame (selected-frame))))
	  message-kill-actions))
	(draft-article message-draft-article)
	(auto-save-file-name buffer-auto-save-file-name)
	(file-name buffer-file-name)
	(modified (buffer-modified-p)))
    (with-temp-buffer
      (let ((message-kill-actions actions)
	    (message-draft-article draft-article)
	    (buffer-auto-save-file-name auto-save-file-name)
	    (buffer-file-name file-name))
	(set-buffer-modified-p modified)
	(message-kill-buffer)))))

(substitute-key-definition 'message-kill-buffer
			   'message-kill-buffer-and-frame
			   message-mode-map)

(add-hook
 'gnus-configure-windows-hook
 (lambda nil
   (when (eq major-mode 'message-mode)
     (let* ((frame (selected-frame))
	    (action
	     `(if (and (eq (selected-frame) ,frame)
		       (or message-delete-frame-anyway
			   (eq (selected-window) (next-window))))
		  (delete-frame ,frame))))
       (setq gnus-frame-list (delq frame gnus-frame-list))
       (setq message-exit-actions (cons action message-exit-actions))
       (setq message-postpone-actions
	     (cons action message-postpone-actions))
       ;; The following hook will be used when the buffer deletion
       ;; is invoked by `kill-buffer' or from the buffer menu.
       (gnus-make-local-hook 'kill-buffer-hook)
       (add-hook
	'kill-buffer-hook
	`(lambda nil
	   (let ((auto-save (and buffer-auto-save-file-name
				 (file-exists-p
				  buffer-auto-save-file-name)))
		 (actions message-exit-actions))
	     (when (and (or auto-save
			    (and buffer-file-name
				 (file-exists-p buffer-file-name)))
			(yes-or-no-p (concat "Remove the backup file"
					     (if (buffer-modified-p)
						 " too")
					     "? ")))
	       (when (and buffer-auto-save-file-name
			  (file-exists-p buffer-auto-save-file-name))
		 (condition-case nil
		     (delete-file buffer-auto-save-file-name)
		   (error)))
	       (message-disassociate-draft))
	     (message-do-actions actions))
	   (let* ((frame ,frame)
		  window)
	     (when (and (frame-live-p frame)
			(setq window
			      (get-buffer-window (current-buffer)
						 frame))
			(or message-delete-frame-anyway
			    (eq window (next-window window))))
	       (delete-frame frame))))
	t t))
     (set-window-start (selected-window) (point-min)))))

;; Don't popup a message frame when sending a queued message.
(add-hook
 'gnus-message-setup-hook
 (lambda nil
   (if (or (memq this-command '(gnus-draft-send-message
				gnus-draft-send-all-messages
				gnus-group-send-queue))
	   (and (featurep 'gnus-delay)
		(save-excursion
		  (save-restriction
		    (widen)
		    (message-narrow-to-headers)
		    (re-search-forward
		     (concat "^" (regexp-quote gnus-delay-header)
			     ":\\s-+")
		     nil t)))))
       (let ((config (copy-sequence gnus-buffer-configuration)))
	 (set (make-local-variable 'gnus-buffer-configuration)
	      (cons '(forward (vertical 1.0 (message 1.0 point)))
		    (delq (assq 'forward config) config)))
	 (set (make-local-variable 'gnus-configure-windows-hook)
	      nil)))))

;; Run `mml-preview' on a new frame.  2005-02-08
(gnus-add-configuration
 '(mml-preview
   (frame 1.0
	  (progn
	    (setq gnus-frame-list nil) ;; 2005-04-19
	    '(vertical 1.0 (message 1.0)))
	  (vertical ((width . 80) (height . 40)
		     ;;(left . -1) (top . 1)
		     (user-position . t))
		    (mml-preview 1.0 point)))))

(add-hook
 'gnus-configure-windows-hook
 (lambda nil
   (when (and (boundp 'mml-preview-buffer)
	      (eq (current-buffer) mml-preview-buffer))
     (setq gnus-frame-list (delq (selected-frame) gnus-frame-list))
     (gnus-make-local-hook 'kill-buffer-hook)
     (add-hook 'kill-buffer-hook
	       `(lambda nil
		  (let ((frame ,(selected-frame))
			window)
		    (when (and (frame-live-p frame)
			       (setq window (get-buffer-window
					     ,mml-preview-buffer
					     frame))
			       (or message-delete-frame-anyway
				   (eq window (next-window window))))
		      (delete-frame frame))))
	       t t))))

(provide 'message-multiple-frames)

;;; message-multiple-frames.el ends here
