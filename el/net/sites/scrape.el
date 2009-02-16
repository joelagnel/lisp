;;; scrape.el --- Emacs Lisp utilities for web scraping

;; Copyright (C) 2005  Edward O'Connor

;; Author: Edward O'Connor <ted@evdb.com>
;; Keywords: convenience

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

;; This is a simple set of utilities that help to do website scraping in
;; Emacs Lisp. We shell out to HTML Tidy to do some of the dirty work.

;; You should use tidy.el if you're looking to use Tidy to clean up HTML
;; you're composing in Emacs.


;;; History:
;; 2005-10-27: initial version.

;;; Code:

(require 'url)
(require 'xml)

(defvar scrape-tidy-command "tidy -asxhtml -bq"
  "If non-null, how to invoke HTML Tidy.")

(defun scrape-region (start end)
  "Scrape the HTML between points START and END."
  (save-restriction
    (widen)
    (narrow-to-region start end)
    (save-excursion
      (when scrape-tidy-command
        (shell-command-on-region (point-min) (point-max)
                                 scrape-tidy-command nil t))
      (goto-char (point-min))
      (re-search-forward "^<html" nil t)
      (goto-char (line-beginning-position))
      (delete-region (point-min) (point))
      (condition-case nil
          (car (xml-parse-region (point) (point-max)))
        (error nil)))))

(defun scrape-buffer (&optional buffer)
  "Scrape the HTML residing in BUFFER.
If BUFFER is unspecified, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (scrape-region (point-min) (point-max))))

(defun scrape-string (string)
  "Scrape the HTML residing in STRING."
  (with-temp-buffer
    (insert string)
    (scrape-buffer)))

(defvar scrape-debug nil)

(defun scrape-url (url)
  "Retrieve URL, run it through HTML Tidy, and return parsed XML."
  (let ((url-package-name "scrape.el"))
    (let ((buffer (url-retrieve-synchronously url)))
      (when buffer
        (unwind-protect
            (with-current-buffer buffer
              (delete-region (point-min) url-http-end-of-headers)
              (scrape-buffer))
          (unless scrape-debug
            (kill-buffer buffer)))))))

(provide 'scrape)
;;; scrape.el ends here
