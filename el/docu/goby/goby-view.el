;; Goby: goby-view.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug  9, 2003

;;; Commentary:

;; Home page: http://www.mew.org/~kazu/proj/goby/

;;; Code:

(require 'goby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer local variables
;;;

(defvar goby-buffer-height nil)
(defvar goby-buffer-end nil)
(defvar goby-buffer-page-end nil)

(mapcar 'make-variable-buffer-local
	(list 'goby-buffer-height
	      'goby-buffer-end
	      'goby-buffer-page-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View mode
;;;

(defun goby-view-mode (&optional screen-dump)
  "Enter Goby View mode."
  (interactive)
  (setq goby-buffer-height (frame-parameter (selected-frame) 'height))
  (setq major-mode 'goby-view-mode)
  (setq mode-name goby-view-mode-name)
  (use-local-map goby-view-mode-map)
  (if menu-bar-mode (modify-frame-parameters nil (list '(menu-bar-lines . 0))))
  (run-hooks 'goby-view-mode-enter-hook)
  (goby-decorate-view-frame goby-buffer-height)
  (run-hooks 'goby-view-mode-enter-hook2)
  (setq goby-buffer-end (point-max))
  (forward-page -1)
  (unless (= (point) 1) (forward-line))
  (goby-narrow-to-page)
  (goby-decorate-page screen-dump)
  (unless screen-dump (goby-first-pause))
  (setq buffer-read-only t)
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands for View mode
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Page
;;;

(defun goby-narrow-to-page ()
  (let ((beg (point)))
    (forward-page) ;; just after C-L
    (unless (= (point) (point-max)) (beginning-of-line)) ;; no \n at the end
    (narrow-to-region beg (point))
    (setq goby-buffer-page-end (point-max))))

(defun goby-first-pause ()
  (let* ((regex (regexp-quote goby-pause-string)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regex nil t)
	  (narrow-to-region (point-min) (point))))))

(defun goby-next-pause ()
  (let* ((beg (point-min))
	 (end (point-max))
	 (regex (regexp-quote goby-pause-string))
	 new-end)
    (save-excursion
      (widen)
      (goto-char end)
      (if (re-search-forward regex goby-buffer-page-end t)
	  (setq new-end (point))
	(setq new-end goby-buffer-page-end))
      (narrow-to-region beg new-end))))

(defun goby-next-page (&optional screen-dump)
  "Go to the next page."
  (interactive)
  (cond
   ((= (point-max) goby-buffer-end)
    (let ((visible-bell t)) (ding)))
   ((= (point-max) goby-buffer-page-end)
    (goto-char (point-max)) ;; just before C-L
    (widen)
    (forward-line)
    (goby-narrow-to-page)
    (goby-decorate-page screen-dump)
    (unless screen-dump (goby-first-pause)))
   (t
    (goby-next-pause))))

(defun goby-prev-page ()
  "Go to the previous page."
  (interactive)
  (if (= (point-min) 1)
      (let ((visible-bell t)) (ding))
    (goto-char (point-min))
    (widen)
    (forward-page -2)
    (unless (= (point) 1) (forward-line))
    (goby-narrow-to-page)
    (goby-decorate-page)
    (goby-first-pause)))

(defun goby-first-page ()
  "Go to the first page."
  (interactive)
  (widen)
  (goto-char (point-min))
  (goby-narrow-to-page)
  (goby-decorate-page)
  (goby-first-pause))

(defun goby-last-page ()
  "Go to the last page."
  (interactive)
  (widen)
  (goto-char (point-max))
  (forward-page -1)
  (unless (= (point) 1) (forward-line))
  (goby-narrow-to-page)
  (goby-decorate-page)
  (goby-first-pause))

(defun goby-decorate-page (&optional no-mouse-face)
  (let ((after-change-functions nil)
	(buffer-read-only nil)
	beg)
    (set-window-start (selected-window) (point-min))
    (unless no-mouse-face
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (looking-at "[ \t]+") (goto-char (match-end 0)))
	(if (and (or (goby-extent-image-p (point))
		     (goby-extent-space-p (point)))
		 (setq beg (next-property-change (point))))
	    (goto-char beg))
	(setq beg (point))
	(catch 'loop
	  (while (not (eolp))
	    (if (goby-extent-p (point))
		(throw 'loop nil))
	    (forward-char))
	  (if (< beg (point))
	      (put-text-property beg (point) 'mouse-face 'goby-view-mouse)))
	(forward-line))
      (set-buffer-modified-p nil))
    (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Quit
;;;

(defun goby-view-quit ()
  "Quit Goby View mode and return to Goby Edit mode."
  (interactive)
  (widen)
  (setq buffer-read-only nil)
  (remove-text-properties (point-min) (point-max) '(mouse-face nil))
  (if menu-bar-mode (modify-frame-parameters nil (list '(menu-bar-lines . 1))))
  (run-hooks 'goby-view-mode-exit-hook)
  (goby-clean-view-frame goby-buffer-height)
  (run-hooks 'goby-view-mode-exit-hook2)
  (setq goby-buffer-height nil)
  (setq goby-buffer-end nil)
  (funcall goby-major-mode)
  (goby-mode 'noimage)
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Isearch
;;;

(defun goby-isearch-forward ()
  "Incremental forward search for Goby View mode."
  (interactive)
  (goby-isearch 'isearch-forward))

(defun goby-isearch-backward ()
  "Incremental backward search for Goby View mode."
  (interactive)
  (goby-isearch 'isearch-backward))

(defun goby-isearch (func)
  (let ((height (frame-parameter (selected-frame) 'height)))
    (modify-frame-parameters
     (selected-frame)
     `((height . ,(- height goby-window-manager-bottom-search-margin))
       (cursor-type . box)))
    (widen)
    (funcall func)
    (forward-page -1)
    (unless (= (point) 1) (forward-line))
    (modify-frame-parameters
     (selected-frame)
     `((height . ,height)
       (cursor-type . (bar . 0))))
    (goby-narrow-to-page)
    (goby-decorate-page)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Screen dump
;;;

(defun goby-dump-screen ()
  "Create HTML files with screen dumps."
  (interactive)
  (let* ((goby-buf (current-buffer))
	 (name (buffer-name goby-buf))
	 dir N)
    (if (not (goby-which-exec goby-prog-capture))
	(message "\"%s\" not found" goby-prog-capture)
      (when (setq dir (goby-dump-check-directory))
	(message "Creating HTML files...")
	(sit-for 1)
	(message "")
	(save-excursion
	  (setq N (goby-dump-images dir)))
	(goby-dump-htmls dir N name)
	(message "Creating HTML files...done")))))

(defun goby-dump-check-directory ()
  (let ((dir (goby-read-directory-name "Dump directory: ")))
    (if (file-exists-p dir)
	(unless (file-directory-p dir)
	  (setq dir nil)
	  (message "A file exists."))
      (if (y-or-n-p (format "Create %s? " dir))
	  (make-directory dir 'parents)
	(setq dir nil)))
    dir))

(defun goby-dump-images (dir)
  (let* ((end (point-max))
	 (i 0))
    ;; the first page
    (goto-char (point-min))
    (goby-view-mode 'screen-dump)
    (goby-dump-file dir i)
    (setq i (1+ i))
    ;; the second page and later
    (while (/= (point-max) end)
      (goby-next-page 'screen-dump)
      (goby-dump-file dir i)
      (setq i (1+ i)))
    (goby-view-quit)
    i))

(defun goby-prog-capture-args-for-darwin (file)
  (list "-S" "-x" file))

(defun goby-prog-capture-args-for-unix (file)
  (let* ((pixel-width (display-pixel-width))
	 (pixel-height (display-pixel-height))
	 (geom (format "%dx%d+0+0" pixel-width pixel-height)))
    (list "-silent" "-window" "root" "-crop" geom file)))

(defun goby-dump-file (dir i)
  (let* ((default-directory dir)
	 (lfile (format goby-dump-large-file i))
	 (sfile (format goby-dump-small-file i))
	 (tfile (concat (make-temp-name "goby") ".png")))
    (setq lfile (expand-file-name lfile dir))
    (setq sfile (expand-file-name sfile dir))
    (setq tfile (expand-file-name tfile dir))
    (sit-for 0.1)
    (apply 'call-process goby-prog-capture nil nil nil
	   (funcall goby-prog-capture-args tfile))
    (goby-resize-file tfile lfile goby-dump-large-width)
    (goby-resize-file tfile sfile goby-dump-small-width)
    (delete-file tfile)))

(defun goby-resize-file (fromfile tofile width)
  (let ((topnm (goby-get-topnm fromfile)))
    (with-temp-buffer
      (goby-image-safe
       (set-buffer-multibyte nil)
       (call-process topnm fromfile '(t nil))
       (call-process-region (point-min) (point-max) goby-prog-pnmscale
			    t '(t nil) nil
			    "-xsize" (format "%d" width))
       (call-process-region (point-min) (point-max) goby-prog-pnmtopng
			    t '(t nil) nil)
       (write-region (point-min) (point-max) tofile nil 'no-msg)))))

(defun goby-dump-html-header (name)
  (insert
   "<?xml version=\"1.0\" encoding=\"us-ascii\"?>\n"
   "<!DOCTYPE html \n"
   "     PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
   "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
   "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"
   "<head>\n"
   "<meta http-equiv=\"content-type\" content=\"text/html; charset=us-ascii\" />\n"
   "<title>" name "</title>\n"
   "</head>\n"
   "<body>\n"
   "\n"
   "<p>\n"
   "Title: " name "<br />\n"
   "Author: " (user-login-name) "<br />\n"
   "Date: " (format-time-string "%a, %d %b %Y" (current-time)) "<br />\n"
   "</p>\n"
   "<hr />\n"))

(defun goby-dump-html-header2 (name prevpage nextpage)
  (insert
   "<?xml version=\"1.0\" encoding=\"us-ascii\"?>\n"
   "<!DOCTYPE html \n"
   "     PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
   "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
   "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"
   "<head>\n"
   "<meta http-equiv=\"content-type\" content=\"text/html; charset=us-ascii\" />\n"
   "<title>" name "</title>\n"
   "</head>\n"
   "<body>\n"
   "<p>\n"
   (if prevpage
       (format "<a href=\"%s\">[prev]</a>" prevpage)
     "[prev]")
   " "
   "<a href=\"" goby-dump-index-file "\">[up]</a>"
   " "
   (if nextpage
       (format "<a href=\"%s\">[next]</a>" nextpage)
     "[next]")
   "\n</p>\n"
   "<hr />\n"))

(defun goby-dump-html-tailer ()
  (insert
   "<hr />\n"
   "<p>\n"
   "Created with <a href=\"" goby-home-page "\">Goby</a>\n"
   "</p>\n"
   "</body>\n"
   "</html>\n"))

(defun goby-dump-htmls (dir N name)
  (goby-dump-html-index dir N name)
  (let ((i 0))
    (while (< i N)
      (goby-dump-html-file dir i name (/= i 0) (/= i (1- N)))
      (setq i (1+ i)))))

(defun goby-dump-html-index (dir N name)
  (let ((file (expand-file-name goby-dump-index-file dir))
	(i 0)
	page sfile)
    (with-temp-buffer
      (goby-dump-html-header name)
      (while (< i N)
	(setq page (format goby-dump-html-file i))
	(setq sfile (format goby-dump-small-file i))
	(insert (format "<a href=\"%s\"><img src=\"%s\" width=\"%d\" height=\"%d\" alt=\"slide%d\" title=\"slide%d\" /></a>\n" page sfile goby-dump-small-width goby-dump-small-height i i))
	(setq i (1+ i)))
      (goby-dump-html-tailer)
      (let ((buffer-file-coding-system nil)) ;; us-ascii
	(write-region (point-min) (point-max) file nil 'no-msg)))))

(defun goby-dump-html-file (dir i name prev next)
  (let ((file (expand-file-name (format goby-dump-html-file i) dir))
	(lfile (format goby-dump-large-file i))
	(prevpage (if prev (format goby-dump-html-file (1- i))))
	(nextpage (if next (format goby-dump-html-file (1+ i)))))
    (with-temp-buffer
      (goby-dump-html-header2 name prevpage nextpage)
      (insert (format "<img src=\"%s\" width=\"%d\" height=\"%d\" alt=\"slide%d\" title=\"slide%d\" />\n"lfile goby-dump-large-width goby-dump-large-height i i))
      (goby-dump-html-tailer)
      (let ((buffer-file-coding-system nil)) ;; us-ascii
	(write-region (point-min) (point-max) file nil 'no-msg)))))

(provide 'goby-view)

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

;;; goby-view.el ends here
