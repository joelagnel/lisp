;;; grep-buffers.el --- grep through buffers (a la 'moccur')
;;; Copyright (C) 2004, Scott Frazer <frazers67@yahoo.com>

;; grep-buffers.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This is version 1.2 as of 19 Jul 2004

;;; Commentary:

;; This code lets you grep through all loaded buffers that have a file
;; associated with them.  It's similar to 'moccur' and it's many variants, but
;; uses the standard compilation-mode interface, i.e. next-error,
;; previous-error, etc. all work.

(defvar grep-buffers-buffer-name "*grep-buffers*"
  "grep-buffers buffer name.")

(defvar grep-buffers-regexp-history nil
  "Regexp history for grep-buffers.")

;; Grep buffers

(defun grep-buffers ()
  "Grep buffers that have file names associated with them."
  (interactive)
  (let ((buffers (sort (buffer-list)
                       (function (lambda (elt1 elt2)
                                   (string< (downcase (buffer-name elt1))
                                            (downcase (buffer-name elt2)))))))
        (regexp (symbol-near-point)))
    (setq regexp (read-string (format "grep buffers for [%s]: " regexp)
                              nil 'grep-buffers-regexp-history regexp))
    (add-to-list 'grep-buffers-regexp-history regexp)
    (get-buffer-create grep-buffers-buffer-name)
    (save-excursion
      (set-buffer grep-buffers-buffer-name)
      (erase-buffer)
      (display-buffer grep-buffers-buffer-name)
      (insert (format "grep buffers for '%s' ...\n\n" regexp))
      (mapcar (lambda (x)
                (when (buffer-file-name x)
                  (set-buffer x)
                  (save-excursion
                    (save-match-data
                      (goto-char (point-min))
                      (while (re-search-forward regexp nil t)
                        (let ((line (count-lines 1 (point)))
                              (substr (buffer-substring (point-at-bol)
                                                        (point-at-eol))))
                          (save-excursion
                            (set-buffer grep-buffers-buffer-name)
                            (insert (format "%s:%d:%s\n" x line substr))))
                        (goto-char (point-at-eol)))))))
              buffers)
      (set-buffer grep-buffers-buffer-name)
      (goto-char (point-max))
      (insert "\ngrep finished\n")
      (set-buffer-modified-p nil)
      (compilation-mode)
      (set (make-local-variable 'compilation-parse-errors-function)
           'grep-buffers-parse-matches))))

;; Parse matches

(defun grep-buffers-parse-matches (limit-search find-at-least)
  "Parse the grep buffer for matches.
See variable `compilation-parse-errors-function' for interface."
  (save-excursion
    (set-buffer grep-buffers-buffer-name)
    (goto-char (point-min))
    (setq compilation-error-list nil)
    (while (re-search-forward "^\\(.+?\\):\\([0-9]+?\\):\\(.+?\\)$" nil t)
      (let ((buffer-of-match (match-string 1))
            (line-of-match (string-to-number (match-string 2))))
        (setq compilation-error-list
              (nconc compilation-error-list
                     (list (cons
                            (save-excursion
                              (beginning-of-line)
                              (point-marker))
                            (save-excursion
                              (set-buffer buffer-of-match)
                              (goto-line line-of-match)
                              (beginning-of-line)
                              (point-marker))))))))))

(provide 'grep-buffers)

;;; grep-buffers.el ends here
