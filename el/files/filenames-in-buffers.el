;;;; filenames-in-buffers.el
;;; Time-stamp: <2005-11-16 11:34:08 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'filenames-in-buffers)

(defconst filename-non-constituent "[^-a-zA-Z0-9/_.~+#]"
  "Regexp for characters which are not part of a filename.")

(defun file-named-at-point (pt)
  "Returns filename that the point PT is on."
  (save-excursion
    (if (re-search-backward filename-non-constituent (point-min) t)
	(progn
	  (forward-char 1)
	  (let ((start (point)))
	    (if (re-search-forward filename-non-constituent (point-max) t)
		(progn
		  (backward-char 1)
		  (expand-file-name
		   (buffer-substring-no-properties start (point))))
	      (error "Could not find likely end of filename"))))
      (error "Could not find likely start of filename"))))

(defun line-numbered-after-point (pt)
  "Returns line number after the point PT is on, if there is one."
  (save-excursion
    (if (re-search-forward filename-non-constituent (point-max) t)
	(progn
	  (backward-char 1)
          (let ((start (point)))
	    (if (looking-at ":\\([0-9]+\\)")
		(string-to-int (match-string 1))
	      nil)))
      nil)))

(defun delete-file-at-point-no-inspection ()
  "Delete the file named at point."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (if (y-or-n-p (format "Delete %s? " file))
	(progn
	  (delete-file file)
	  (message "Deleted %s" file))
      (message "did not delete %s" file))))

(defun file-name-existing (name prompt)
  "Return NAME if it refers to an existing file, else ask user insistently."
  (if (file-exists-p name)
      name
    (read-file-name prompt
		    (file-name-directory name)
		    (file-name-nondirectory name)
		    t
		    (file-name-nondirectory name))))

(defvar find-file-at-point-hooks nil
  "A list of functions to try finding the file named at point.
If one of them returns non-nil, it is taken to be the full name of the file found.
If it is a cons, is the full name dotted with the line number. ")

;;;; one of these has now appeared in emacs itself
;; (defun find-file-at-point (p)
;;   "Find the file named at point."
;;   (interactive "d")
;;   (when (interactive-p)
;;     (push '(find-file-at-point (point))
;; 	  command-history))
;;   (let* ((filename (file-named-at-point p))
;; 	 (line-number (line-numbered-after-point p))
;; 	 (found-name (run-hook-with-args-until-success 'find-file-at-point-hooks filename)))
;;     (message "filename=%S line-number=%S found-name=%S" filename line-number found-name)
;;     (if found-name
;; 	(if (consp found-name)
;; 	    (setq filename (car found-name)
;; 		  line-number (cdr found-name))
;; 	  (setq filename found-name)))
;;     (find-file (file-name-existing filename "Find file: "))
;;     (when line-number
;;       (goto-line line-number))))

(defun find-file-at-point-other-window ()
  "Find the file named at point."
  (interactive)
  (ffap-other-window)
;;   (interactive "d")
;;   (let ((filename (file-named-at-point p))
;; 	(line-number (line-numbered-after-point p))
;; 	(found-name (run-hook-with-args-until-success 'find-file-at-point-hooks filename)))
;;     (if found-name
;; 	(if (consp found-name)
;; 	    (setq filename (car found-name)
;; 		  line-number (cdr found-name))
;; 	  (setq filename found-name)))
;;     (find-file-other-window-beside (file-name-existing filename
;; 						       "Find file: "))
;;     (when line-number
;;       (goto-line line-number)))
)

(defun delete-file-at-point-with-inspection ()
  "Delete the file named at point."
  (interactive)
  (let ((file (file-name-existing (thing-at-point 'filename)
				  "Delete file: ")))

    (save-window-excursion
      (set-buffer (get-buffer-create " *deletion sampler*"))
      (erase-buffer)
      (insert-file-contents file)
      (with-output-to-temp-buffer "*Deletion sample*"
	(princ "File ")
	(princ file)
	(princ " begins:\n")
	(princ (buffer-substring (point-min) (min (point-max) 100))))
      (if (y-or-n-p (format "Delete %s? " file))
	  (progn
	    (delete-file file)
	    (message "Deleted %s" file))
	(message "did not delete %s" file)))))

;;; end of filenames-in-buffers.el
