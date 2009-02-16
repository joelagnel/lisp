;;; mew-xemacs.el --- Environment of XEmacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 20, 1997

;;; Code:

(defface mew-bitmap '((t (:background "white" :foreground "black")))
  "Face for bitmap."
  :group 'mew-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic functions
;;;

(defun mew-icon-make (sym file)
  (let* ((name (concat "mew-icon-" (symbol-name sym)))
	 (var (intern-soft name))
	 (path (expand-file-name (concat file ".xpm") mew-icon-directory)))
    (set var (toolbar-make-button-list path))))

(defun mew-menu-get-ent (spec func)
  (let (s ret)
    (catch 'loop
      (while spec
	(setq s (car spec))
	(setq spec (cdr spec))
	(cond
	 ((vectorp s)
	  (if (eq (aref s 1) func) (throw 'loop (setq ret s))))
	 ((listp s)
	  (setq ret (mew-menu-get-ent s func))
	  (if ret (throw 'loop nil))))))
    ret))

(defun mew-toolbar-make (spec alist)
  (let (a file icon path s ret)
    (while alist
      (setq a (car alist))
      (setq alist (cdr alist))
      (setq file (concat (cdr a) ".xpm"))
      (setq path (expand-file-name file mew-icon-directory))
      (setq icon (toolbar-make-button-list path))
      (setq s (mew-menu-get-ent spec (car a)))
      (if s (setq ret (cons (vector icon (aref s 1) (aref s 2) (aref s 0)) ret))))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toolbar and button setting
;;;

(cond
 (mew-icon-p
  (let ((spec mew-icon-spec))
    (while spec
      (mew-icon-make (car (car spec)) (cdr (car spec)))
      (setq spec (cdr spec))))

  (defvar mew-icon-separate-spec (list [mew-icon-separate nil nil ""]))

  (defvar mew-summary-toolbar
    (mew-toolbar-make mew-summary-mode-menu-spec mew-summary-toolbar-spec))

  (defvar mew-message-toolbar
    (mew-toolbar-make mew-message-mode-menu-spec mew-message-toolbar-spec))

  (defvar mew-draft-toolbar
    (mew-toolbar-make mew-draft-mode-menu-spec mew-draft-toolbar-spec))

  (defvar mew-header-toolbar
    (mew-toolbar-make mew-header-mode-menu-spec mew-header-toolbar-spec))

  (define-key toolbar-map 'button3   'pressed-and-activate-toolbar-button)
  (define-key toolbar-map 'button3up 'release-and-activate-toolbar-button)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Button functions
;;;

(defun mew-summary-button ()
  "Call back function for toolbar of Summary mode. 
If event is button 1, show a part.
If event is button 3, show a menu."
  (interactive)
  (let* ((fld (mew-summary-folder-name))
	 (msg (mew-summary-message-number))
	 (part (mew-syntax-number))
	 (nums (mew-syntax-number-to-nums part))
	 (button (event-button last-command-event))
	 (cache (mew-cache-hit fld msg 'must-hit)))
    (mew-summary-goto-part msg part)
    (mew-summary-recenter)
    (cond
     ((= button 1)
      (mew-summary-show-part cache nums))
     ((= button 3)
      (popup-menu mew-summary-mode-toolbar-menu)))))

(defun mew-summary-show-part (cache nums)
  "Show a part according to the clicked icon."
  (interactive)
  (if (null nums) 
      (message "No message")
    (let ((win (selected-window)))
      (mew-summary-toggle-disp-msg 'on)
      (unwind-protect
	  (progn
	    (mew-summary-toggle-disp-msg 'on)
	    (mew-window-configure 'message)
	    ;; message buffer
	    (mew-summary-display-part cache nums))
	(select-window win)))))

(defun mew-draft-button ()
  "Call back function for toolbar of Draft mode. 
If event is button 1, show a part.
If event is button 3, show a menu."
  (interactive)
  (let ((nums (mew-syntax-nums))
	(button (event-button last-command-event)))
    (mew-attach-goto-number 'here nums)
    (cond
     ((= button 1)
      (mew-draft-show-attach nums))
     ((= button 3)
      (popup-menu mew-draft-mode-toolbar-menu)))))

;; This is a toy at present. Support only CT: Image/*.
;; To make Summary and Draft symmetric, left button click on icon
;; should display the attachment. 
(defun mew-draft-show-attach (nums)
  "Show a part according to the clicked icon."
  (interactive)
  (let ((case-fold-search t)
	(str (toolbar-button-help-string last-pressed-toolbar-button))
	(image-extent (extent-at (point-max) nil nil nil 'at))
	ct)
    (if (and image-extent (glyphp image-extent))
	(mew-overlay-delete image-extent))
    (when (string-match "(\\(.*\\))" str)
      (setq ct (match-string 1 str))
      (if (mew-ct-imagep ct)
	  (let* ((subdir (mew-attach-expand-path mew-encode-syntax nums))
		 (syntax (mew-syntax-get-entry mew-encode-syntax nums))
		 (name (mew-syntax-get-file syntax))
		 (ename (if (string= subdir "") name (concat subdir name)))
		 (file (expand-file-name ename (mew-attachdir)))
		 (attr (mew-ctdb-prog (mew-ctdb-by-ct ct)))
		 (program (nth 0 attr))
		 (options (nth 1 attr))
		 (async   (nth 2 attr))
		 (format (cond
			  ((and (string-match "jpeg" ct)
				(valid-image-instantiator-format-p 'jpeg))
			   'jpeg)
			  ((and (string-match "gif" ct)
				(valid-image-instantiator-format-p 'gif)) 
			   'gif)
			  ((and (string-match "xbm" ct)
				(valid-image-instantiator-format-p 'xbm))
			   'xbm)
			  ((and (string-match "xpm" ct)
				(valid-image-instantiator-format-p 'xpm))
			   'xpm)
			  ((and (string-match "png" ct)
				(valid-image-instantiator-format-p 'png))
			   'png)
			  (t nil)))
		 glyph) 
	    (if format
		(progn
		  (message "Loading image...")
		  (setq glyph (make-glyph (vector format :file file)))
		  (if (eq format 'xbm)
		      (set-glyph-property glyph 'face 'mew-bitmap))
		  (set-extent-end-glyph
		   (mew-overlay-make (point-max) (point-max)) glyph)
		  (message "Loading image...done"))
	      (if (and (stringp program) (mew-which-exec program))
		  (if async
		      (mew-mime-start-process program options file)
		    (mew-mime-call-process program options file)))))))))

(defun pressed-and-activate-toolbar-button (event)
  "A replacement function 'press-toolbar-button' so that
popup menu can be implemented."
  (interactive "_e")
  (or (button-press-event-p event)
      (error "%s must be invoked by a mouse-press" this-command))
  (let ((button (event-toolbar-button event)) callback)
    (when (toolbar-button-p button)
      (setq last-pressed-toolbar-button button)
      (if (and (setq callback (toolbar-button-callback button))
	       (or (eq callback 'mew-summary-button)
		   (eq callback 'mew-draft-button)))
	  (when (toolbar-button-enabled-p button)
	    ;; (setq toolbar-active t) is meaningless... why?
	    (setq this-command callback)
	    (if (symbolp callback)
		(call-interactively callback)
	      (eval callback)))
	;; emulate press-toolbar-button
	(setq this-command last-command)
	(setq toolbar-active t)
	(set-toolbar-button-down-flag button t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Toolbar functions
;;;

(defun mew-summary-setup-decoration ()
  (if (featurep 'scrollbar)
      (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (mew-summary-toolbar-update)
  (when mew-icon-p
    (set-buffer-menubar current-menubar)
    (add-submenu nil mew-summary-mode-menu-spec)
    (easy-menu-add mew-summary-mode-menu-spec)))

(defun mew-message-setup-decoration () nil) ;; xxx

(defun mew-draft-setup-decoration ()
  (mew-draft-toolbar-update)
  (when mew-icon-p
    (set-buffer-menubar current-menubar)
    (add-submenu nil mew-draft-mode-menu-spec)
    (easy-menu-add mew-draft-mode-menu-spec)))

(defun mew-header-setup-decoration ()
  (mew-header-toolbar-update)
  (when mew-icon-p
    (set-buffer-menubar current-menubar)
    (add-submenu nil mew-header-mode-menu-spec)
    (easy-menu-add mew-header-mode-menu-spec)))

(defsubst mew-summary-toolbar-update ()
  (if mew-icon-p
      (set-specifier default-toolbar
		     (cons (current-buffer) mew-summary-toolbar))))

(defsubst mew-message-toolbar-update ()
  (if mew-icon-p
      (set-specifier default-toolbar
		     (cons (current-buffer) mew-message-toolbar))))

(defsubst mew-draft-toolbar-update ()
  (if mew-icon-p
      (set-specifier default-toolbar
		     (cons (current-buffer) mew-draft-toolbar))))

(defsubst mew-header-toolbar-update ()
  (if mew-icon-p
      (set-specifier default-toolbar
		     (cons (current-buffer) mew-header-toolbar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of messages
;;;

(defvar mew-x-emacs-end-of-message nil)
(defvar mew-x-emacs-end-of-part nil)

(defsubst mew-message-set-end-of-message ()
  (unless (glyphp mew-x-emacs-end-of-message)
    (setq mew-x-emacs-end-of-message
	  (make-glyph
	   (vector 'string :data mew-end-of-message-string))))
  (overlay-put (mew-minfo-get-eom) 'end-glyph mew-x-emacs-end-of-message))

(defsubst mew-message-set-end-of-part ()
  (unless (glyphp mew-x-emacs-end-of-part)
    (setq mew-x-emacs-end-of-part
	  (make-glyph
	   (vector 'string :data mew-end-of-part-string))))
  (overlay-put (mew-minfo-get-eom) 'end-glyph mew-x-emacs-end-of-part))

(defsubst mew-message-set-end-of-nil ()
  (overlay-put (mew-minfo-get-eom) 'end-glyph nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Veil
;;;

(defvar mew-x-emacs-header-veil nil)

(defun mew-header-veil-make ()
  (unless (glyphp mew-x-emacs-header-veil)
    (setq mew-x-emacs-header-veil
	  (make-glyph
	   (vector 'string :data mew-header-veil-string))))
  (let ((ov (mew-overlay-make 1 1)))
    (overlay-put ov 'invisible t)
    (overlay-put ov 'end-glyph mew-x-emacs-header-veil)
    (delete-overlay ov) ;; detach from the buffer
    ov))

(defun mew-toggle-header-veil (ov)
  (cond
   ((overlay-get ov 'invisible)
    (overlay-put ov 'invisible nil)
    (overlay-put ov 'end-glyph nil))
   (t
    (overlay-put ov 'invisible t)
    (overlay-put ov 'end-glyph mew-x-emacs-header-veil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image
;;;

(defun mew-image-inline-p (format)
  ;; XEmacs checks window-system by itself
  (valid-image-instantiator-format-p format))

(defun mew-mime-image (cache begin end format)
  (message "Loading image...")
  (cond 
   ((eq format 'xbm)
    (let (glyph)
      (save-excursion
	(set-buffer cache)
	(setq glyph (mew-xemacs-make-xbm-glyph begin end)))
      (set-buffer (mew-buffer-message))
      (mew-elet
       (set-extent-end-glyph
	(mew-overlay-make (point-min) (point-min)) glyph))))
   ((eq format 'gif)
    (let (glyph)
      (save-excursion
	(set-buffer cache)
	(setq glyph (mew-xemacs-make-gif-glyph begin end)))
      (set-buffer (mew-buffer-message))
      (mew-elet
       (if glyph
	   (set-extent-end-glyph
	    (mew-overlay-make (point-min) (point-min)) glyph)
	 (insert "Cannot display this GIF image.\n")
	 (insert "Please install \"gifsicle\".\n")
	 (insert "\thttp://www.lcdf.org/gifsicle/\n")))))
   (t
    (set-buffer (mew-buffer-message))
    (mew-elet
     (set-extent-end-glyph
      (mew-overlay-make (point-min) (point-min))
      (make-glyph (vector 
		   format
		   :data
		   (save-excursion
		     (set-buffer cache)
		     (mew-buffer-substring 
		      begin end))))))))
  (message "Loading image...done"))

;; XEmacs depends on libX when it converts XBM to glyph.
;; The convert function can handle a file only, not a buffer.
;; So, we have to use a temporary file.

(defun mew-xemacs-make-xbm-glyph (beg end)  
  (let ((tfile (mew-make-temp-name)) glyph)
    (mew-flet
     (write-region beg end tfile nil 'no-msg)
     (setq glyph (make-glyph (vector 'xbm :file tfile)))
     (set-glyph-property glyph 'face 'mew-bitmap)
     (mew-delete-file tfile)
     glyph)))

;; Fix Animation GIF and Interlaced GIF

(defvar mew-prog-gifsicle "gifsicle")

(defun mew-xemacs-make-gif-glyph (beg end)
  (let ((prog (mew-which-exec mew-prog-gifsicle))
	(image (mew-buffer-substring beg end))
	should-be-non-interlaced should-unoptimize size1 size2)
    (when prog
      (with-temp-buffer
	(mew-plet
	 (insert image)
	 (goto-char (point-min))
	 (when (looking-at "GIF8[79]")
	   (call-process-region (point-min) (point-max)
				prog
				t t nil "--info")
	   (goto-char (point-min))
	   ;; Check whether a `data' is interlaced.
	   (setq should-be-non-interlaced
		 (re-search-forward
		  "  \\+ image #[0-9]+ \\([0-9]+x[0-9]+\\).* interlaced"
		  nil t))
	   (goto-char (point-min))
	   ;; Check whether a `data' is optimized or larger than
	   ;; the value of `w3m-animated-gif-maximum-size'.
	   (when (looking-at ".+ \\([0-9]+\\) images\r?$")
	     (setq size1 (string-to-number (mew-match-string 1)))
	     (forward-line 1)
	     (unless (and (looking-at ".+ \\([0-9]+\\)x\\([0-9]+\\)\r?$")
			  (natnump (setq size1 (* size1
						  (string-to-number
						   (mew-match-string 1)))))
			  (natnump (setq size1 (* size1
						  (string-to-number
						   (mew-match-string 2))))))
	       ;; It should be truncated to be only one frame.
	       (setq should-unoptimize "#0"))
	     (setq size1 nil)
	     (while (and (not should-unoptimize)
			 (re-search-forward
			  "  \\+ image #[0-9]+ \\([0-9]+x[0-9]+\\)"
			  nil t))
	       (if size1
		   (if (string-equal size1 (setq size2 (mew-match-string 1)))
		       (setq size1 size2)
		     (setq should-unoptimize "--unoptimize"))
		 (setq size1 (mew-match-string 1)))))
	   (mew-erase-buffer)
	   (insert image)
	   (when (or should-unoptimize should-be-non-interlaced)
	     ;; Unoptimize anyway.
	     (if should-unoptimize
		 (call-process-region (point-min) (point-max)
				      prog
				      t t nil should-unoptimize
				      "--no-interlace")
	       (call-process-region (point-min) (point-max)
				    prog
				    t t nil "--no-interlace")))
	   (goto-char (point-min))
	   (unless (looking-at "GIF8[79]")
	     ;; Unoptimization is failed. :-<
	     ;; Attempt to extract the first frame.
	     (mew-erase-buffer)
	     (insert image)
	     (call-process-region (point-min) (point-max)
				  prog
				  t t nil "#0" "--no-interlace")))
	 ;; buffer is fixed gif
	 (setq image (mew-buffer-substring (point-min) (point-max)))))
      (make-glyph (vector 'gif :data image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transient
;;;

(defun mew-mark-active-p () zmacs-region-active-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X Face
;;;

(defun mew-x-face-create ()
  (mew-xemacs-make-xbm-glyph (point-min) (point-max)))

(defun mew-x-face-display (xface)
  (save-excursion
    (goto-char (point-min))
    (let ((regex2 (concat "^\\(" mew-from: "\\).*"))
	  overlay)
      (when (re-search-forward regex2 nil t)
	(setq overlay (mew-overlay-make (match-end 1) (match-end 1)))
	(set-extent-end-glyph overlay xface)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Profile
;;;

(defun mew-profile-init ()
  (when (and mew-profile mew-profile-functions-list)
    (require 'elp)
    (let ((elp-function-list mew-profile-functions-list))
      (elp-instrument-list))))

(defun mew-profile-results ()
  (if (and mew-profile mew-profile-functions-list)
      (let ((elp-results-buffer mew-buffer-debug))
	(elp-results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/SSH/TLS notification
;;;

(defvar mew-secure-format
  (if window-system
      (let ((data-directory mew-icon-directory))
	(make-glyph [xpm :file "mew-lock.xpm"]))
    " [Sec]"))

(defvar mew-secure-format2 nil)

(provide 'mew-xemacs)

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

;;; mew-xemacs.el ends here
