;;; file-fns.el --- functions for querying about or acting on files

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 1999, 2006 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: file-fns.el,v 1.6 2006/05/16 02:24:26 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

;;;###autoload
(defvar nf-backup-buffer-mtime-interval 3600
 "*Number of seconds from last backup before a new numbered backup file is\
 automatically created for the current buffer.

This variable is used by `nf-backup-buffer-if-mtime-elapsed', which see.")

;; This function uses `file-newest-backup', which first appears in Emacs 20.
(defun nf-backup-buffer-if-mtime-elapsed ()
"Check whether a new backup of this file should be made.

Reset the backed-up status flag for this buffer if backups are enabled and
`nf-backup-buffer-mtime-interval' seconds have elapsed since the last
backup file was made.  \(The variable `version-control' ultimately
determines whether backups are created.\)

This function should be added to `before-save-hook' to be effective."
  (condition-case err
      (let* ((bname   (file-newest-backup (buffer-file-name)))
             (mtime   (and bname (nth 5 (file-attributes bname))))
             (elapsed (and mtime (time-subtract (current-time) mtime))))
        (when (and elapsed
                   (>= (time-to-seconds elapsed)
                       nf-backup-buffer-mtime-interval))
          (setq buffer-backed-up nil)))
    (error (message "(ignored error: %s)" err))))

;;(add-hook 'before-save-hook 'nf-backup-buffer-if-mtime-elapsed)

;;;###autoload
(defun file-in-pathlist-p (file path)
  "Return path of FILE if FILE is found anywhere in PATH.
FILE is a string.  PATH is a list of strings."
  (let ((found nil)
        (f nil))
    (while (and (not found) path)
      (setq f (concat (file-name-as-directory (car path)) file))
      (and (file-exists-p f)
           (setq found f))
      (setq path (cdr path)))
    found))

;;;###autoload
(defun file-plain-p (file)
  "Returns `t' if FILE is a plain file.
That means it is not a directory, symlink, character-special device, named
pipe, socket, etc."
  (and (stringp file)
       (let ((mode (nth 8 (file-attributes file))))
         (and mode
              (= ?- (aref mode 0))))))

;;;###autoload
(defun file-name-completions-in-path (name-regexp path-list
                                                  &optional predicate filter)
  "Return an obarray containing file name completions.
All file names matching NAME-REGEXP, located in directories listed in
PATH-LIST, which satisfy optional arg PREDICATE, are put into the obarray
after being filtered through optional FILTER for potential edits.

If NAME-REGEXP is nil, then all files are candidates.

PREDICATE and FILTER should be functions which take one argument, a string
representing a file name."
  (let ((completions (make-vector 3 0))
        (files nil))
    (while path-list
      (setq files (directory-files (car path-list) nil name-regexp t))
      (while files
        (cond ((or (null predicate)
                   (funcall predicate (car files)))
               (intern (if filter
                           (funcall filter (car files))
                         (car files))
                       completions)))
        (setq files (cdr files)))
      (setq path-list (cdr path-list)))
    completions))

;;;###autoload
(defun insert-tail-of-file-contents (file bytes)
  "Insert the last N bytes of FILE.
If the file is smaller than N, just insert the entire file."
  (interactive "fFile name: \nnLast N bytes of file to insert: ")
  (setq file (expand-file-name file))
  (let* ((attr (file-attributes file))
         (size (nth 7 attr)))
    (and (= size -1)
         (signal 'overflow-error (list file size attr)))
    (if (> bytes size)
        (insert-file-contents file)
      (insert-file-contents file nil (- size bytes)))))

;;;###autoload
(defun make-autosave-for-buffer-before-kill-p ()
  (cond ((and (buffer-modified-p)
              (yes-or-no-p "This buffer is modified; make autosave? "))
         (make-local-variable 'kill-buffer-hook)
         (add-hook 'kill-buffer-hook
                   (function
                    (lambda ()
                      ;; Do an auto save, then set the auto save file name
                      ;; to nil to prevent kill-buffer from deleting it.
                      ;; For some reason setting delete-auto-save-files nil
                      ;; seems not to work.
                      (if (< (emacs-version-major) 19)
                          (do-auto-save)
                        ;; In emacs 19, just autosave the current buffer.
                        (do-auto-save nil t))
                      (setq buffer-auto-save-file-name nil)))))
        ;; The kill-buffer-query-functions have the semantics that if any
        ;; return nil, then do not kill the buffer.  If they all return t,
        ;; then do kill.
        (t t)))

;;;###autoload
(defun make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (null current-mode)
             (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))

(provide 'file-fns)

;;; file-fns.el ends here.
