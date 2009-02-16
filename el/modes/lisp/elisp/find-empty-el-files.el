;;;; find-empty-el-files.el -- find elisp files that don't have any elisp in them
;;; Time-stamp: <2005-01-18 19:14:58 jcgs>
;;
;; Copyright (C) 2004  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;; 
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(provide 'find-empty-el-files)

;; This is mostly for looking for files that are completely commented
;; out

;;;###autoload
(defun find-live-elisp-in-file (&optional report-either-way)
  "Report whether there is any elisp in the current buffer."
  (interactive)
  (save-excursion
    (let ((found (second (parse-partial-sexp (point-min)
					     (point-max)
					     1))))
      (if found
	  (if report-either-way (message "%s has lisp in it" buffer-file-name))
	(message "%s has no lisp in it" buffer-file-name))
      (if found nil buffer-file-name))))

(require 'edit-tree)

;;;###autoload
(defun find-empty-elisp-files (dir)
  (interactive "DDirectory to check: ")
  (let ((files nil))
    (apply-command-to-tree dir "\\.el$" 
			   (lambda ()
			     (let ((file (find-live-elisp-in-file)))
			       (if file (push file files))))
			   nil)
    (with-output-to-temp-buffer "*Empties*"
      (dolist (file files)
	(princ (format "%s\n" file))))))

;;; end of find-empty-el-files.el
