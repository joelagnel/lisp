;;; Saved through ges-version 0.3.3dev at 2003-02-25 14:36
;;; ;;; From: Ivar Rummelhoff <ivarru@math.uio.no>
;;; ;;; Subject: Upd: continuous-undo-mode
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Tue, 25 Feb 2003 19:40:30 +0100
;;; ;;; Organization: University of Oslo, Norway

;;; --=-=-=

;;; Thanks for the comments, everyone!  I don't know when I will have time
;;; to develop it further, but I have at least improved the documentation.
;;; As before: Use at your own risk!

;;; Ivar


;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=continuous-undo.el
;;; Content-Description: Continuous undo mode

;;; ;;; continuous-undo.el --- Save undo information between sessions

;;; ;; Copyright (C) 2003 Ivar Rummelhoff

;;; ;; Author: Ivar Rummelhoff <ivarru@math.uio.no>
;;; ;; Maintainer: Ivar Rummelhoff <ivarru@math.uio.no>
;;; ;; Created: 18 February 2003
;;; ;; Time-stamp: <2003-02-25 19:39:27 ivarru>
;;; ;; Keywords: Undo

;;; ;; This file is NOT part of GNU Emacs.

;;; ;; GNU Emacs is free software; you can redistribute it and/or modify
;;; ;; it under the terms of the GNU General Public License as published by
;;; ;; the Free Software Foundation; either version 2, or (at your option)
;;; ;; any later version.

;;; ;; GNU Emacs is distributed in the hope that it will be useful,
;;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; ;; GNU General Public License for more details.

;;; ;; You should have received a copy of the GNU General Public License
;;; ;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; ;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; ;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is experimental software!  USE AT YOUR OWN RISK!!
;; (Unless you understand the code, you should probably not use it.)

;; This buffer local minor mode will save the undo information of the
;; visited file in a separate file whenever the contents of a buffer
;; is saved.  To use this mode add

;;  (require 'continuous-undo)

;; to your .emacs file and evaluate it (e.g. by restarting emacs).
;; Now if you include something like

;;  ;;; Local Variables: ***
;;  ;;; eval: (continuous-undo-mode t) ***
;;  ;;; End: ***

;; at the end of a file, the undo information of the previous session
;; will be restored whenever the file is visited.  In order to
;; activate `continuous-undo-mode' the first time, you must type

;;  M-x continuous-undo-mode [ret]

;; or kill the buffer and revisit the file.

;;; History:

;; Short

;;; Code:

(defvar continuous-undo-prefix ".continuous-undo."
  "'continuous-undo-mode' will use this prefix for the names of undo files.
\(files containing undo information\)")

(defun continuous-undo-filename (name)
  "Determine the name of the file containing the undo information of NAME."
  (concat
   (file-name-directory name)
   continuous-undo-prefix
   (file-name-nondirectory name)))

(defun continuous-undo-strip (str)
  "Make a copy of STR without any text properties."
  (let ((s (concat str)))
    (set-text-properties 0 (length str) nil s)
    s))

(defun continuous-undo ()
  "Save the the undo information for this buffer."
  (let ((list buffer-undo-list))
    (with-temp-file (continuous-undo-filename (buffer-file-name))
      (let ((standard-output (current-buffer)))
        (princ "(setq buffer-undo-list '")
        (if (not (listp list))
            (prin1 list)
          (princ "(")
          (dolist (elt list)
            (cond
             ((numberp elt))
             ((not (consp elt)) (prin1 elt) (princ "\n"))
             ((markerp (car elt)))
             ((stringp (car elt))
              (princ "(")
              (prin1 (continuous-undo-strip (car elt)))
              (princ " . ")
              (prin1 (cdr elt))
              (princ ")\n"))
             (t  (prin1 elt) (princ "\n"))))
          (princ ")"))
        (princ ")")))))

(defun continuous-undo-load ()
  "Load the undo information for this buffer."
  (let ((name (continuous-undo-filename
               (buffer-file-name))))
    (when (file-exists-p name)
      (if (not (file-exists-p (buffer-file-name)))
          (continuous-undo-delete)
        (let ((mod1 (nth 5 (file-attributes (buffer-file-name))))
              (mod2 (nth 5 (file-attributes name))))
          (if (or (< (car mod1) (car mod2))
                  (and (= (car mod1) (car mod2))
                       (<= (cadr mod1) (cadr mod2))))
              (load-file name)
            (continuous-undo-delete)))))))

(defun continuous-undo-delete ()
  "Delete the file containing undo information for this buffer."
  (let ((name (continuous-undo-filename
               (buffer-file-name))))
    (if (file-exists-p name)
        (delete-file name))))



;;;; Continuous-Undo mode  (a minor mode)

(defvar continuous-undo-mode nil)
;; The mode variable
;; (non-nil when Continuous-Undo mode is turned on.)

(defvar continuous-undo-mode-hook nil
  "Functions to run whenever Continuous-Undo mode is turned on.")

(defvar continuous-undo-mode-leave-hook nil
  "Functions to run whenever Continuous-Undo mode is turned off.")

(defun continuous-undo-mode (&optional arg)
  "Toggle Continuous-Undo mode.
With ARG turn Continuous-Undo mode on if and only if ARG is positive."
  (interactive "P")
  (let ((on-p (if arg (> (prefix-numeric-value arg) 0)
		(not continuous-undo-mode))))
    (make-local-variable 'continuous-undo-mode)
    (cond
     ;; Turn mode on
     (on-p
      (setq continuous-undo-mode t)
      (add-hook 'after-save-hook 'continuous-undo nil t)
      (unless (buffer-modified-p)
        (continuous-undo-load))
      (run-hooks 'continuous-undo-mode-hook))
     ;; Turn mode off
     (t
      (when continuous-undo-mode
	(setq continuous-undo-mode nil)
        (remove-hook 'after-save-hook 'continuous-undo t)
        (continuous-undo-delete)
	(run-hooks 'continuous-undo-mode-leave-hook))))
    (force-mode-line-update)))

(unless (assq 'continuous-undo-mode minor-mode-alist)
  (push (list 'continuous-undo-mode " CU")
	minor-mode-alist))

(provide 'continuous-undo)

;;; continuous-undo.el ends here

;;; --=-=-=--

