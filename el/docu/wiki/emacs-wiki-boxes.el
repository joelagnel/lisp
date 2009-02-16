;;; emacs-wiki-boxes.el --- Boxing system for emacs-wiki.el

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; based on tp-ew-boxes.el but radically simplified
;; Author: Ulrik Jensen <terryp@daimi.au.dk>
;; Author: Ole Arndt <ole@sugarshark.com>
;; Keywords: wiki 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar emacs-wiki-boxes-queued-boxes nil
  "A list of boxes to create.")

(defun emacs-wiki-boxes-markup (string)
  "Markup string using EmacsWiki markup, and return the results."
  (let ((project emacs-wiki-current-project))
    ;; make sure the markup is done in the right project, despite being in a temp buffer
    (with-temp-buffer
      (let* ((emacs-wiki-current-project project)
	     (emacs-wiki-publishing-header "<--!STARTHERE!-->")
	     (emacs-wiki-publishing-footer "<--!ENDHERE!-->"))
	(insert string)
	(goto-char (point-min))
	(emacs-wiki-replace-markup "BOX!")
	;; find the locations
	(let ((beg (point-min))
	      (end (point-max)))
	  (goto-char beg)
	  (search-forward emacs-wiki-publishing-header)
	  ;; emacs-wiki.el will *most* likely have inserted <p> here now.
	  (setq beg (point))
	  (search-forward emacs-wiki-publishing-footer)
	  ;; and right before this, will be a </p>
	  (setq end (- (point) (length emacs-wiki-publishing-footer)))
	  ;; return the results -- but we really should handle those
	  ;; <p>'s somehow.. remove them! as well as newlines!
	  (replace-regexp-in-string "%23" "#" (buffer-substring beg end)))))))

(defun emacs-wiki-boxes-create-box (beg end &optional highlightp)
  "Create a box."
  (unless highlightp
    (add-to-list 'emacs-wiki-boxes-queued-boxes (buffer-substring beg end) t)
    (delete-region beg end)))

(defun emacs-wiki-boxes-render-boxes ()
  "Insert DIV-tags for all boxes on the page."  
  (let ((result ""))
    (while emacs-wiki-boxes-queued-boxes
      (setq result (concat result
			   "<div class=\"soapbox\">\n"
			   (emacs-wiki-boxes-markup (car emacs-wiki-boxes-queued-boxes))
			   "\n</div>\n<br>\n"))
      (setq emacs-wiki-boxes-queued-boxes (cdr emacs-wiki-boxes-queued-boxes)))
    result))

(add-to-list 'emacs-wiki-markup-tags
	     '("box" t nil nil emacs-wiki-boxes-create-box))

(provide 'emacs-wiki-boxes)
;;; emacs-wiki-boxes.el ends here
