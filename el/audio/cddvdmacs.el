;;; cdvdmacs.el --- Burn CDs and DVDs with external tools

;; Copyright (C) 2007  Markus Triska

;; Author: Markus Triska <markus.tri...@gmx.at>
;; Keywords: processes

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This lets you quickly burn files and directories to CD or DVD.
;; Copy cdvdmacs.el to your load-path and add to your .emacs:

;;     (require 'cdvdmacs)

;; M-x cdvdmacs-add adds a file or directory to the project. A new
;;     project is created if none exists. In dired, "M-i a" adds the
;;     file at point, and "M-i m" adds all marked files.
;; M-x cdvdmacs-create-iso creates an ISO image of the project.
;; M-x cdvdmacs-burn-cd and ...-dvd burn the project on-the-fly.
;; C-d and DEL in the speedbar frame remove a file from the project.

;; Burning commands are currently hard-coded; adapt them if necessary.

;;; Code:

(require 'speedbar)
(require 'dired)

(defconst cdvdmacs-version "0.1")

(defvar cdvdmacs-speedbar-key-map (speedbar-make-specialized-keymap))
(defvar cdvdmacs-redraw-speedbar t)
(defvar cdvdmacs-buffer nil)

(defgroup cdvdmacs nil
  "Burn CDs and DVDs"
  :group 'processes)

;;;###autoload
(defcustom cdvdmacs-cdrecord-device "/dev/dvd"
  "Device for dev=... switch of cdrecord."
  :group 'cdvdmacs
  :type 'string)

;;;###autoload
(defcustom cdvdmacs-cdrecord-gracetime "10"
  "Value for -gracetime switch of cdrecord. Values below 2 are
not recommended."
  :group 'cdvdmacs
  :type 'string)

;;;###autoload
(defcustom cdvdmacs-growisofs-device "/dev/dvd"
  "Target device for growisofs and dvd+rw-format."
  :group 'cdvdmacs
  :type 'string)

(defun cdvdmacs-dummy (&rest args)
  "A dummy function ignoring its arguments.")

(defmacro cdvdmacs-in-speedbar-buffer (&rest forms)
  `(with-current-buffer speedbar-buffer
     (speedbar-with-writable
       ,@forms
       )))

(defun cdvdmacs-speedbar-buttons (&rest args)
  (when cdvdmacs-redraw-speedbar
    (erase-buffer)
    (dolist (file (mapcar #'cdvdmacs-speedbar-file-format
                          (cdvdmacs-file-list)))
      (speedbar-insert-generic-list 0 `((,file))
                                     'cdvdmacs-dummy 'cdvdmacs-dummy))
    (setq cdvdmacs-redraw-speedbar nil)))

(defun cdvdmacs-redraw-speedbar ()
  "Redraw the speedbar, synchronising it with the project's buffer."
  (interactive)
  (setq cdvdmacs-redraw-speedbar t)
  (speedbar-update-contents)
  (cdvdmacs-project-size))

(defun cdvdmacs-graft-point (file)
  "For a directory, the final part. For a file, the file name."
  (cond ((file-directory-p file)
         (string-match ".*/\\(.*\\)" file) (match-string 1 file))
        (t (file-name-nondirectory file))))

(defun cdvdmacs-speedbar-file-format (file)
  (format "%s (%s)" (cdvdmacs-graft-point file)
          (cdvdmacs-nice-size (cdvdmacs-size-of file))))

(defun cdvdmacs-nice-size (size)
  "Make a number of bytes (integer or float) more readable."
  (cond ((< size 1e3) (format "%.2f b" size))
        ((< size 1e6) (format "%.2f KB" (/ (float size) 1e3)))
        ((< size 1e9) (format "%.2f MB" (/ (float size) 1e6)))
        (t (format "%.2f GB" (/ (float size) 1e9)))))

;;;###autoload
(defun cdvdmacs-dired-add-marked ()
  "Add the files and directories marked in dired to the project."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (ignore-errors (cdvdmacs-add file t)))
  (cdvdmacs-project-size))

;;;###autoload
(defun cdvdmacs-add (file &optional no-total)
  "Add a file or directory to the project."
  (interactive "fFile or directory: ")
  (unless (or (file-directory-p file)
              (file-regular-p file))
    (error "Not a regular file or directory"))
  (unless (buffer-live-p cdvdmacs-buffer) (cdvdmacs-init))
  (setq file (expand-file-name file))
  (if (string= (substring file -1) "/")
      (setq file (substring file 0 -1)))
  (if (with-current-buffer cdvdmacs-buffer
          (save-excursion
            (goto-char (point-min))
            (re-search-forward
             (format "^%s\n" (regexp-quote file)) nil t)))
        (error "%s is already included" file))
  (with-current-buffer cdvdmacs-buffer
    (goto-char (point-max))
    (insert (format "%s\n" file)))
  (cdvdmacs-in-speedbar-buffer
   (goto-char (point-max))
   (beginning-of-line)
   (speedbar-insert-generic-list 0
                                 `((,(cdvdmacs-speedbar-file-format file)))
                                 'cdvdmacs-dummy 'cdvdmacs-dummy))
  (unless no-total (cdvdmacs-project-size)))

;;;###autoload
(defun cdvdmacs-dired-add ()
  (interactive)
  (cdvdmacs-add (dired-get-filename))
  (cdvdmacs-project-size))

(defun cdvdmacs-remove-from-project ()
  "Remove file at point (in speedbar) from project."
  (interactive)
  (let ((line (line-number-at-pos)))
    (cdvdmacs-in-speedbar-buffer
     (kill-whole-line 1))
    (with-current-buffer cdvdmacs-buffer
      (save-excursion
        (cdvdmacs-goto-line line)
        (kill-whole-line 1))))
  (cdvdmacs-project-size))

(defun cdvdmacs-size-of (file)
  "Size of file or directory in bytes, as floating point number."
  (if (file-regular-p file)
      (float (nth 7 (file-attributes file)))
    (let ((stack (list file))
          (size 0.0))
      (while stack
        (setq cur (pop stack))
        (cond ((member (file-name-nondirectory cur) '("." "..")) nil)
              ((file-regular-p cur)
               (setq size (+ size (float (nth 7 (file-attributes cur))))))
              ((file-directory-p cur)
               (setq stack (append (directory-files cur t nil t) stack)))))
      size)))

(defun cdvdmacs-project-size ()
  "Show total size of files in the project."
  (interactive)
  (let ((size 0.0))
    (dolist (file (cdvdmacs-file-list))
      (setq size (+ size (cdvdmacs-size-of file))))
    (message "Total: %s" (cdvdmacs-nice-size size))))

(defun cdvdmacs-clear-project ()
  "Remove all files from the project."
  (interactive)
  (with-current-buffer cdvdmacs-buffer (erase-buffer))
  (cdvdmacs-in-speedbar-buffer (erase-buffer))
  (cdvdmacs-project-size))

(defun cdvdmacs-create-iso (iso-file)
  "Create an ISO file containing the files in the project."
  (interactive "FISO-file: ")
  (if (or (not (file-exists-p iso-file))
          (y-or-n-p "File exists. Overwrite? "))
      (let ((files (mapcar (lambda (f)
                             (concat (cdvdmacs-graft-point f) "=" f))
                           (cdvdmacs-file-list))))
        (switch-to-buffer "*mkisofs*")
        (erase-buffer)
        (apply #'start-process
               "mkisofs" (current-buffer) "mkisofs" "-R" "-J" "-graft-points"
               "-o" (expand-file-name iso-file) files))))

;;;###autoload
(defun cdvdmacs-blank-cd ()
  "Blank a CD with the \"fast\" method."
  (interactive)
  (switch-to-buffer "*cdrecord*")
  (erase-buffer)
  (shell-command (concat "cdrecord dev=" cdvdmacs-cdrecord-device
                         " blank=fast gracetime=" cdvdmacs-cdrecord-gracetime
                         " &") (current-buffer)))

(defun cdvdmacs-blank-dvd ()
  "Blank a DVD using dvd+rw-format."
  (interactive)
  (switch-to-buffer "*dvd+rw-format*")
  (erase-buffer)
  (shell-command (concat "dvd+rw-format -force " cdvdmacs-growisofs-device " &")
                 (current-buffer)))

(defun cdvdmacs-burn-cd ()
  "Burn the project to CD on-the-fly."
  (interactive)
  (let ((files
         (mapconcat (lambda (f)
                      (let ((quoted (shell-quote-argument f)))
                        (concat (cdvdmacs-graft-point quoted) "=" quoted)))
                    (cdvdmacs-file-list) " ")))
    (switch-to-buffer "*cdrecord*")
    (erase-buffer)
    (shell-command
     (concat "mkisofs -J -R -graft-points " files
             " | cdrecord -gracetime=" cdvdmacs-cdrecord-gracetime
             " -v fs=6m dev=" cdvdmacs-cdrecord-device
             " - &") (current-buffer))))

(defun cdvdmacs-burn-dvd ()
  "Burn the project to DVD on-the-fly."
  (interactive)
  (let ((files (mapcar (lambda (file)
                         (concat (cdvdmacs-graft-point file) "=" file))
                       (cdvdmacs-file-list))))
    (switch-to-buffer "*growisofs*")
    (erase-buffer)
    (apply #'start-process "growisofs" (current-buffer) "growisofs" "-R" "-J"
           "-graft-points" "-Z" cdvdmacs-growisofs-device files)))

;;;###autoload
(defun cdvdmacs-burn-iso (iso-file)
  "Burn an ISO image to disk."
  (interactive "fISO file to burn: ")
  (switch-to-buffer "*cdrecord*")
  (erase-buffer)
  (shell-command
   (concat "cdrecord -gracetime=" cdvdmacs-cdrecord-gracetime
           " -v fs=6m dev=" cdvdmacs-cdrecord-device
           " " (expand-file-name iso-file) " &") (current-buffer)))

(defun cdvdmacs-file-list ()
  "Obtain a list of file strings from cdvdmacs-buffer.
Lines with non-existent files are removed."
  (let (files)
    (with-current-buffer cdvdmacs-buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "#")
            (let ((file (substring (thing-at-point 'line) 0 -1)))
              (if (file-exists-p file)
                  (push file files)
                (kill-whole-line 1))))
          (forward-line))))
    (reverse files)))

(defun cdvdmacs-open-this-file ()
  "Open file on current line in speedbar.
This is useful in combination with extview.el, openwith.el etc."
  (interactive)
  (let ((line (line-number-at-pos)))
    (with-current-buffer cdvdmacs-buffer
      (save-excursion
        (cdvdmacs-goto-line line)
        (find-file (substring (thing-at-point 'line) 0 -1))))))

(defun cdvdmacs-goto-line (line)
  "Like goto-line, and ignoring comments."
  (goto-char (point-min))
  (let ((cur 1))
    (while (or (< cur line) (looking-at "#"))
      (forward-line)
      (unless (looking-at "#") (setq cur (1+ cur))))))

;;;###autoload
(defun cdvdmacs-load-from-buffer (&optional buffer)
  "Propagate speedbar with files listed in BUFFER.
If BUFFER is nil or omitted, use the current buffer. This can be
used to load previously stored project files."
  (interactive)
  (setq cdvdmacs-buffer (or buffer (current-buffer)))
  (cdvdmacs-init))

;; The project is stored in cdvdmacs-buffer as a list of files and
;; directories, one per line. You can thus easily save project files
;; as text files and later load them using cdvdmacs-load-from-buffer.
;; The buffer content must always be synchronised with the speedbar:
;; If you manually remove or comment out files, do M-x cdvdmacs-init
;; or M-x cdvdmacs-redraw-speedbar to refresh the speedbar.

;;;###autoload
(defun cdvdmacs-init ()
  "Create or update the speedbar, and create the file listing buffer.
If the file listing buffer exists, refresh speedbar contents."
  (interactive)
  (unless (buffer-live-p cdvdmacs-buffer)
    (setq cdvdmacs-buffer (get-buffer-create "*cdvdmacs*")))
  (with-current-buffer cdvdmacs-buffer
    (kill-all-local-variables)
    (setq major-mode 'cdvdmacs-mode
          mode-name "cdvdmacs")
    (make-local-variable 'comment-start)
    (setq comment-start "#")
    (make-local-variable 'comment-end)
    (setq comment-end "")
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (setq font-lock-defaults '(nil nil nil nil))
    (speedbar-frame-mode 1)
    (speedbar-change-initial-expansion-list "cdvdmacs")
    (cdvdmacs-redraw-speedbar)
    (setq speedbar-update-flag nil)))

;;;###autoload
(defun cdvdmacs-version ()
  "Show version information of cdvdmacs."
  (interactive)
  (message "Using version %s of cdvdmacs." cdvdmacs-version))

(define-key cdvdmacs-speedbar-key-map "\C-d" 'cdvdmacs-remove-from-project)
(define-key cdvdmacs-speedbar-key-map [delete] 'cdvdmacs-remove-from-project)
(define-key cdvdmacs-speedbar-key-map [return] 'cdvdmacs-open-this-file)
(speedbar-add-expansion-list '("cdvdmacs" nil cdvdmacs-speedbar-key-map
                               cdvdmacs-speedbar-buttons))

(define-key dired-mode-map "\M-ia" 'cdvdmacs-dired-add)
(define-key dired-mode-map "\M-im" 'cdvdmacs-dired-add-marked)

(provide 'cdvdmacs)
;;; cdvdmacs.el ends here 