;;; openwith.el --- Automatically open files with external programs

;; Copyright (C) 2007  Markus Triska

;; Author: Markus Triska <markus.tri...@gmx.at>
;; Keywords: files, processes

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

;; This lets you associate external applications with files so that
;; you can open them via C-x C-f, with RET in dired, etc.

;; Copy this file to your load-path and add to your .emacs:

;;     (require 'openwith)

;; To customize associations, try:

;; M-x customize-variable RET openwith-associations RET

;;; Code:

(defconst openwith-version "0.8e")

(defgroup openwith nil
  "Associate external applications with file extensions."
  :group 'files
  :group 'processes)

(defcustom openwith-associations
  '(("\\.pdf\\'" "acroread" (file))
    ("\\.mp3\\'" "xmms" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))
  "Associations of file patterns to external programs.
File pattern is a regular expression describing the files to
associate with a program. The program arguments are a list of
strings and symbols and are passed to the program on invocation,
where the symbol 'file' is replaced by the file to be opened."
  :group 'openwith
  :type '(repeat (list (regexp :tag "Files")
                       (string :tag "Program")
                       (sexp :tag "Parameters"))))

(defcustom openwith-confirm-invocation nil
  "Ask for confirmation before invoking external programs."
  :group 'openwith
  :type 'boolean)

(defvar openwith-registered nil)
(defvar openwith-active t)

(defun openwith-file-handler (operation &rest args)
  (if (and openwith-active
           (not (buffer-modified-p))
           (zerop (buffer-size))
           (catch 'openwith-done
             (dolist (oa openwith-associations)
               (let (match)
                 (save-match-data
                   (setq match (string-match (car oa) (car args))))
                 (when match
                   (let ((params (mapcar (lambda (x)
                                           (if (eq x 'file)
                                               (car args)
                                             (format "%s" x))) (nth 2 oa))))
                     (when (or (not openwith-confirm-invocation)
                               (y-or-n-p
                                (format "%s %s? " (cadr oa)
                                        (mapconcat #'identity params " "))))
                       (apply #'start-process "openwith-process" nil
                              (cadr oa) params)
                       (kill-buffer nil)
                       (throw 'openwith-done t))))))))
      (error "Opened in external program") ; inhibit further actions
    (let ((inhibit-file-name-handlers
           (cons 'openwith-file-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))

;;;###autoload
(defun openwith-init ()
  "Register `openwith-file-handler' for insert-file-contents and all files."
  (interactive)
  (unless openwith-registered
    (put 'openwith-file-handler 'safe-magic t)
    (put 'openwith-file-handler 'operations '(insert-file-contents))
    (add-to-list 'file-name-handler-alist '("" . openwith-file-handler))
    (setq openwith-registered t)))

;;;###autoload
(defun openwith-version ()
  "Print version of openwith."
  (interactive)
  (message "Using version %s of openwith" openwith-version))

;;;###autoload
(defun openwith-toggle ()
  "Toggle whether openwith is active."
  (interactive)
  (setq openwith-active (not openwith-active))
  (if openwith-active
      (message "openwith enabled.")
    (message "openwith disabled.")))

(openwith-init)

(provide 'openwith)
;;; openwith.el ends here 