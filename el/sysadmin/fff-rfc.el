;;; fff-rfc.el --- locate IETF RFC (Request For Comments) texts quickly

;; Copyright (C) 1996, 97, 99, 2001 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: fff-rfc.el,v 1.3 2001/09/20 22:59:11 friedman Exp $

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(require 'fff)

(defvar fff-rfc-path
  '("/com/doc/rfc"
    "/opt/local/doc/rfc"
    "/usr/local/doc/rfc"
    "/ftp@ftp.rfc-editor.org:/in-notes/"
    ;;"/ftp@ftp.isi.edu:/in-notes/"
    "/ftp@ftp.ietf.org:/rfc/")
  "*Path in which to search for RFCs")

(defvar fff-rfc-extensions
  '("" ".gz" ".bz2" ".Z")
  "*Possible filename extensions on RFC files.
A transparent decompressor library such as `crypt++' or `jka-compr'
may be required if the found file is compressed.")

(defvar fff-rfc-view-mode
  (if (fboundp 'mview-mode)
      'mview-mode
    'view-mode)
  "*Major mode to use for visited RFC")

;;;###autoload
(defun fff-find-rfc (num)
  "Visit the RFC number NUM.
The RFC text is searched in the patch defined by `fff-rfc-path'.
The file may have one of the extensions enumerated in
`fff-rfc-extensions' \(which see\)."
  (interactive "sFind RFC: ")
  (save-match-data
    (cond ((stringp num)
           (and (string-match "^rfc-?" num)
                (setq num (substring num (match-end 0))))
           (and (string-match "^[0-9.+---]+$" num)
                (setq num (string-to-int num))))))
  (let* ((names (fff-suffix (cond ((numberp num)
                                   (list (format "rfc%d.txt" num)
                                         (format "rfc%.4d.txt" num)))
                                  ((member num '("index"))
                                   (format "rfc-%s.txt" num))
                                  ((member num '("author" "title"))
                                   (format "rfc-by-%s.txt" num))
                                  (t
                                   (format "rfc%s.txt" num)))
                            fff-rfc-extensions))
         (found (fff-files-in-directory-list names fff-rfc-path t)))
    (cond (found
           (message "%s" (car found))
           (find-file (car found))
           (setq buffer-read-only t)
           (funcall fff-rfc-view-mode)))))

(defun fff-rfc-install-map ()
  "Install the fff rfc keymap."
  (interactive)
  (fff-install-map)
  (fff-define-key "\C-r" 'fff-find-rfc "Find RFC"))

(provide 'fff-rfc)

;;; fff-rfc.el ends here.
