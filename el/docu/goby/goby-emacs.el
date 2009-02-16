;; Goby: goby-emacs.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug  9, 2003

;;; Commentary:

;; Home page: http://www.mew.org/~kazu/proj/goby/

;;; Code:

(require 'goby-vars)

(defalias 'goby-match-string 'match-string-no-properties) ;; to save length

(defmacro goby-image-safe (&rest body)
  `(let ((coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (auto-image-file-mode nil))
     ,@body))

(defun goby-get-extent (pos)
  (get-text-property pos 'display))

(defun goby-extent-image-p (pos)
  (eq (car (goby-get-extent pos)) 'image))

(defun goby-extent-space-p (pos)
  (eq (car (goby-get-extent pos)) 'space))

(defun goby-extent-p (pos)
  (or (goby-extent-image-p pos) (goby-extent-space-p pos)))

(defun goby-put-image (beg end image &optional nodefault)
  (let ((after-change-functions nil))
    (unless nodefault
      (put-text-property beg end 'face 'default))
    (if image (put-text-property beg end 'display image))))

(defun goby-line-over-p (&optional pos)
  (goto-char (or pos (point-min)))
  (= (vertical-motion 1) 1))

(defun goby-center-line (&optional no-insert)
  "Centerize the line."
  (interactive)
  (let ((after-change-functions nil)
	(lim (window-width))
	beg mid end prop)
    (beginning-of-line)
    (setq beg (point))
    (if no-insert
	(forward-char (length goby-centering-string))
      (if (and (setq prop (get-text-property (point) 'display))
	       (eq (car prop) 'space))
	  (delete-region (point) (+ (point) (length goby-centering-string))))
      (insert goby-centering-string))
    (setq mid (point))
    (put-text-property beg mid 'face 'default)
    (end-of-line)
    (setq end (point))
    (if (= mid end)
	(delete-region beg mid)
      (let ((i 0) j beg1 spaces)
	(narrow-to-region beg end)
	;; If the line contains images only (without text), 
	;; vertical-motion works wrong. So, use "[" as text,
	;; and put the space property onto "]".
	(setq beg1 (1+ beg)) ;; "]"
	;; binary search twice
	(setq j (/ (+ lim i) 2))
	(put-text-property beg1 mid 'display `(space :width ,j))
	(if (goby-line-over-p)
	    (setq lim j)
	  (setq i j))
	(setq j (/ (+ lim i) 2))
	(put-text-property beg1 mid 'display `(space :width ,j))
	(if (goby-line-over-p)
	    (setq lim j)
	  (setq i j))
	;; linear search
	(catch 'loop
	  (while t
	    (put-text-property beg1 mid 'display `(space :width ,i))
	    (if (or (>= i lim) (goby-line-over-p))
		(throw 'loop (setq i (1+ i)))) ;; length of "["
	    (setq i (1+ i))))
	(widen)
	(setq spaces (/ (1- i) 2))
	(beginning-of-line)
	(put-text-property beg mid 'display `(space :width ,spaces))))))

(if (fboundp 'read-directory-name)
    (defalias 'goby-read-directory-name 'read-directory-name)
  (defun goby-read-directory-name (prompt)
    (read-file-name prompt default-directory default-directory)))

(defun goby-set-face-attribute (face family height color italicp)
  (set-face-attribute face nil :family family)
  (set-face-attribute face nil :height height)
  ;; Emacs 21.3 does not have a good rendering mechanism for 'bold.
  (if goby-use-bold (set-face-attribute face nil :weight 'bold))
  (if italicp (set-face-attribute face nil :slant 'italic))
  (set-face-attribute face nil :foreground color))

(defun goby-x-window-manager (arg)
  (when (and goby-use-advanced-window-manager (fboundp 'x-send-client-message))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 arg)))

(defun goby-decorate-initial-frame (width height fringe)
  (modify-frame-parameters
   (selected-frame)
   `((width  . ,width)
     (height . ,height)
     (top  . (+ ,goby-window-manager-top-position))
     (left . (+ ,goby-window-manager-left-position))
     (left-fringe  . ,fringe)
     (right-fringe . ,fringe)
     (name . ,goby-frame)
     (tool-bar-lines . nil)
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil)
     (foreground-color . ,goby-foreground-color)
     (background-color . ,goby-background-color)
     (cursor-color     . ,goby-cursor-color)
     (mouse-color      . ,goby-pointer-color)))
  (make-variable-frame-local 'face-font-rescale-alist)
  (modify-frame-parameters (selected-frame) '((face-font-rescale-alist . nil)))
  (goby-x-window-manager '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (run-hooks 'goby-decorate-initial-frame-hook))

(defvar goby-old-pointer-shape nil)
(defvar goby-old-fringe-face nil)
(defvar goby-old-modeline-face nil)

(defun goby-decorate-view-frame (height)
  ;; Emacs does not allow the cursor to have the same color as the
  ;; background.
  (when (boundp 'x-pointer-shape)
    (setq goby-old-pointer-shape x-pointer-shape)
    (setq x-pointer-shape goby-view-pointer-shape))
  (modify-frame-parameters
   (selected-frame)
   `((cursor-type . (bar . 0))
     (name . "")
     (mouse-color . ,goby-view-pointer-color)
     (top  . (+ ,goby-window-manager-view-top-position))
     (left . (+ ,goby-window-manager-view-left-position))
     (height . ,(+ height goby-window-manager-bottom-margin))))
  (goby-x-window-manager '(1 "_NET_WM_STATE_FULLSCREEN" 0))
  (copy-face 'fringe 'goby-old-fringe-face)
  (set-face-attribute
   'fringe (selected-frame) :foreground goby-fringe-foreground-color)
  (set-face-attribute
   'fringe (selected-frame) :background goby-fringe-background-color)
  (copy-face 'mode-line 'goby-old-modeline-face)
  (set-face-attribute
   'mode-line (selected-frame) :foreground goby-modeline-foreground-color)
  (set-face-attribute
   'mode-line (selected-frame) :background goby-modeline-background-color)
  (set-face-attribute
   'mode-line (selected-frame) :box nil))

(defun goby-clean-view-frame (height)
  (when (boundp 'x-pointer-shape)
    (setq x-pointer-shape goby-old-pointer-shape))
  (setq goby-old-pointer-shape nil)
  (modify-frame-parameters
   (selected-frame)
   `((cursor-type . box)
     (name . ,goby-frame)
     (mouse-color . ,goby-pointer-color)
     (height . ,height)
     (top  . (+ ,goby-window-manager-top-position))
     (left . (+ ,goby-window-manager-left-position))))
  (goby-x-window-manager '(0 "_NET_WM_STATE_FULLSCREEN" 0))
  (cond
   ((string< emacs-version "22")
    (set-face-attribute
     'fringe (selected-frame)
     :foreground (face-attribute 'goby-old-fringe-face :foreground))
    (set-face-attribute
     'fringe (selected-frame)
     :background (face-attribute 'goby-old-fringe-face :background)))
   (t
    (copy-face 'goby-old-fringe-face 'fringe)))
  (copy-face 'goby-old-modeline-face 'mode-line)
  (setq goby-old-fringe-face nil)
  (setq goby-old-modeline-face nil))

(provide 'goby-emacs)

;;; Copyright Notice:

;; Copyright (C) 2003 Kazu Yamamoto
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
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; goby-emacs.el ends here
