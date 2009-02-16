;;; mew-demo.el --- Startup demo for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew-vars)

(defvar mew-logo nil)

(defconst mew-icon-mew "Mew.png")
(defconst mew-icon-mew-mono "Mew.xbm")
(defconst mew-icon-mew-mule-bitmap-image "Mew.img")

(defconst mew-hello-message 
"

Welcome to Mew world.

Mew -- Messaging in the Emacs World

%s

Copyright (C) 1994-2005 Kazu Yamamoto

Please send comments to Kazu@Mew.org.

"
)

(defconst mew-demo-string
'(
 "/\\\\ - \\\\/"

 "-\\\\ - \\\\/" "\\\\\\ - \\\\/" "|\\\\ - \\\\/" "/\\\\ - \\\\/"
 "-\\\\ - \\\\/" "\\\\\\ - \\\\/" "|\\\\ - \\\\/" "/\\\\ - \\\\/"

 "/|\\ - \\\\/"  "//\\ - \\\\/" "/-\\ - \\\\/" "/\\\\ - \\\\/"
 "/|\\ - \\\\/"  "//\\ - \\\\/"  "/-\\ - \\\\/" "/\\\\ - \\\\/"

 "/\\| - \\\\/" "/\\/ - \\\\/" "/\\- - \\\\/" "/\\\\ - \\\\/" 
 "/\\| - \\\\/" "/\\/ - \\\\/" "/\\- - \\\\/" "/\\\\ - \\\\/"

 "/\\\\ - |\\/" "/\\\\ - /\\/" "/\\\\ - -\\/" "/\\\\ - \\\\/"
 "/\\\\ - |\\/" "/\\\\ - /\\/" "/\\\\ - -\\/" "/\\\\ - \\\\/"

 "/\\\\ - \\|/" "/\\\\ - \\//" "/\\\\ - \\-/" "/\\\\ - \\\\/"
 "/\\\\ - \\|/" "/\\\\ - \\//" "/\\\\ - \\-/" "/\\\\ - \\\\/"

 "/\\\\ - \\\\-" "/\\\\ - \\\\\\" "/\\\\ - \\\\|" "/\\\\ - \\\\/"
 "/\\\\ - \\\\-" "/\\\\ - \\\\\\" "/\\\\ - \\\\|" "/\\\\ - \\\\/"
 )
)

(defun mew-hello ()
  (let ((time mew-demo-sit-for)
	(left-margin 0)
	(fill-column (window-width))
	indent)
    (if (not (integerp time)) (setq time 0))
    (insert (format mew-hello-message mew-version))
    (center-region (point-min) (point-max))
    (if (null mew-demo-picture)
	(mew-hello-text time)
      (cond
       ((and mew-xemacs-p window-system
	     (valid-image-instantiator-format-p 'png))
	(goto-char (point-max))
	(mew-flet
	 (setq mew-logo
	       (make-glyph (vector 'png ':file 
				   (expand-file-name mew-icon-mew
						     mew-icon-directory)))))
	(indent-to (startup-center-spaces mew-logo))
	(set-extent-end-glyph (mew-overlay-make (point) (point)) mew-logo)
	(goto-char (point-min))
	(mew-redraw time))
       ((and mew-gemacs-p window-system
	     (fboundp 'image-type-available-p)
	     (image-type-available-p 'png))
	(goto-char (point-max))
	(setq mew-logo (create-image 
			(expand-file-name mew-icon-mew mew-icon-directory)))
	(setq indent (/ (- (window-width)
			   (truncate (car (image-size mew-logo)))) 2))
	(setq indent (max 0 indent))
	(insert (make-string indent mew-sp))
	(insert-image mew-logo)
	(goto-char (point-min))
	(mew-redraw time))
       ((and (featurep 'bitmap) window-system)
	(or
	 (condition-case nil
	     (mew-insert-file-contents
	      (expand-file-name
	       mew-icon-mew-mule-bitmap-image mew-icon-directory))
	   (file-error nil))
	 (condition-case nil
	     (bitmap-insert-xbm-file
	      (expand-file-name mew-icon-mew-mono mew-icon-directory))
	   (file-error nil)))
	(center-region (point-min) (point-max))
	(mew-redraw time))
       (t
	(mew-hello-text time))))))

(defun mew-hello-text (time)
  (insert "/\\\\ - \\\\/")
  (center-line)
  (end-of-line)
  (insert (make-string (1- (- (window-width) (current-column))) mew-sp))
  (end-of-line)
  (mew-redraw time)
  (if (and mew-demo window-system) (mew-demo)))

(defun mew-demo (&optional string)
  (let* ((list (or string mew-demo-string))
	 (wl (window-width))
	 (ul (length (regexp-quote "/\\ - \\/")))
	 (pl (/ (- wl ul) 2))
	 (pre (make-string pl mew-sp))
	 (suf (make-string (1- (- (- wl pl) ul)) mew-sp)))
    (save-window-excursion
      (while list
	(mew-demo-print (car list) pre suf)
	(mew-demo-loop)
	(setq list (cdr list))))))

(defun mew-demo-print (string prefix suffix)
  (goto-char (point-max))
  (let ((end (point)))
    (beginning-of-line)
    (delete-region (point) end))
  (insert prefix string suffix))

(defun mew-demo-loop ()
  (mew-redraw 0.02))

(provide 'mew-demo)

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

;;; mew-demo.el ends here
