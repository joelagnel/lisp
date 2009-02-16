;;; linuxproc.el --- file handlers for /proc

;; Copyright (C) 1999, 2001, 2005 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Status: Works in Emacs 19 and 20.
;;         Semi-obsolete in Emacs 21.
;;         Will not work in XEmacs.
;; Keywords: linux, proc, extensions
;; Created: 1999-10-30

;; $Id: linuxproc.el,v 1.4 2005/06/13 15:35:25 friedman Exp $

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

;; Note: Some parts of this file handler are unnecessary in Emacs 21.1 and
;; later (which finally handles zero-length files in the normal way by just
;; reading them to eof; see Gerd Moellmann's change of 2001-04-18).  But if
;; you are using an older version of Emacs, read on.

;; Linux /proc files are not ordinary files and cannot by visited by emacs
;; in the normal way.  This handler makes it possible to view, edit, and
;; (when allowed) save the contents of /proc files with emacs.

;; Linux /proc files are also inconsistent about returning modification
;; times: some files will always return the current time as the mod time,
;; some only change when there is new data, others never change after the
;; proc entry is first created (for an example of the last case, see
;; /proc/mdstat in kernel versions 2.6.10 or 2.6.11).  It's difficult to
;; tell what behavior is most useful in these situations.  Some files can
;; be written to modify kernel parameters and the modtime for these should
;; not appear to change or you get "file on disk is newer" warnings in
;; emacs.  Other times, e.g. with auto-revert-mode, the file modtime should
;; be updated so that the buffer display keeps up with the latest contents.
;;
;; The heuristic employed here is that writable files are considered not to
;; have changed on disk (so that saving changes is less annoying), but
;; read-only files are always newer (so that auto-revert, etc. work).

;; This package cannot be autoloaded.  To use it, put the following in your
;; .emacs:
;;
;;     (require 'linuxproc)

;; Thanks to Ralph Schleicher <rs@purple.ul.bawue.de> for discovering a
;; method of reading files in /proc by abusing the load reader.

;;; Code:

(defvar linuxproc-file-handler-regexp "^/proc/")

(defvar linuxproc-op-handler-alist
  '((insert-file-contents        . linuxproc-insert-file-contents)
    (verify-visited-file-modtime . linuxproc-verify-visited-file-modtime)))

;;;###autoload
(defun linuxproc-install ()
  "Enable Linux /proc file handler."
  (interactive)
  (or (rassq 'linuxproc-handler-fn file-name-handler-alist)
      (setq file-name-handler-alist
            (cons (cons linuxproc-file-handler-regexp
                        'linuxproc-handler-fn)
                  file-name-handler-alist))))

(defun linuxproc-uninstall ()
  "Disable Linux /proc file handler."
  (interactive)
  (let (elt)
    (while (setq elt (rassq 'linuxproc-handler-fn file-name-handler-alist))
      (setq file-name-handler-alist
            (delq elt file-name-handler-alist)))))

;; Return a version of the file-name-handler-alist minus handlers for
;; linuxproc.
(defun linuxproc-fnh-alist-sans-linuxproc ()
  (let ((alist (copy-sequence file-name-handler-alist))
        elt)
    (while (setq elt (rassq 'linuxproc-handler-fn alist))
      (setq alist (delq elt alist)))
    alist))

(defun linuxproc-handler-fn (op &rest args)
  (let ((inhibit-file-name-handlers
         (cons 'linuxproc-handler-fn
               (and (eq inhibit-file-name-operation op)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation op))
    (apply (or (cdr (assq op linuxproc-op-handler-alist)) op) args)))

(defun linuxproc-insert-file-contents
  (filename &optional visit beg end replace)
  (cond ((and (string-lessp emacs-version "21") ; v21 can use default handler
              (save-match-data
                ;; Some /proc entries are just symlinks to files elsewhere in
                ;; the filesystem.  If this is not a true /proc entry, do not
                ;; use this special handler.
                (string-match linuxproc-file-handler-regexp
                              (file-truename filename))))
         (let ((load-read-function 'linuxproc-read-function)
               (load-source-file-function nil)
               (after-load-alist nil)
               ;; This is necessary to keep lread.c:openp from returning -2
               ;; as the file descriptor number because it believes the
               ;; file requires a special handler for file-exists-p.
               ;; Fload then complains "Failure to create stdio stream"
               ;; in 21.2 when it tries to fdopen this descriptor.
               ;; We could just bind inhibit-file-name-operation, but
               ;; there's no telling whether future versions might use
               ;; multiple operations and we don't really need a special
               ;; handler here at all anyway.
               (file-name-handler-alist (linuxproc-fnh-alist-sans-linuxproc))
               (load-path '("."))
               (load-history nil)
               (default-enable-multibyte-characters nil)
               insertion-beg insertion-end)
           (save-excursion
             (and replace
                  (delete-region (point-min) (point-max)))
             (setq insertion-beg (point))
             (cond ((or beg end)
                    (save-restriction
                      (narrow-to-region (point) (point))
                      (load filename nil t t)
                      (goto-char (point-min))
                      (and beg
                           (> beg 0)
                           (delete-region (point-min) (+ (point-min) beg -1)))
                      (and end
                           (>= (- (point-max) (point-min)) end)
                           (goto-char (point-max))
                           (delete-region (+ (point-min) (- end (or beg 0)))
                                          (point-max)))
                      (goto-char (point-max))))
                   ((load filename nil t t)))
             (setq insertion-end (point))
             (and visit (linuxproc-visited-file-setup filename)))
           (list (expand-file-name filename)
                 (- insertion-end insertion-beg))))
        (t
         (insert-file-contents filename visit beg end replace))))

(defun linuxproc-read-function (&optional stream)
  (let (char)
    (while (not (< (setq char (funcall stream)) 0))
      (insert char))))

(defun linuxproc-verify-visited-file-modtime (buffer)
  ;; Some /proc entries are just symlinks to files elsewhere in
  ;; the filesystem.  If this is not a true /proc entry, do not
  ;; use this special handler.
  (cond ((save-excursion
           (set-buffer buffer)
           (save-match-data
             (string-match linuxproc-file-handler-regexp
                           (file-truename buffer-file-name))))
         ;; Always return t for writable /proc files, nil for read-only
         ;; files.  Files in /proc can get their time stamp updated merely
         ;; by doing a stat on them.  Therefore, the real
         ;; verify-visited-file-modtime would report that the file on disk
         ;; is newer than the buffer's visited modtime, which can be
         ;; irritating when trying to do a save.
         (file-writable-p (buffer-file-name buffer)))
        ((verify-visited-file-modtime buffer))))

(defun linuxproc-visited-file-setup (filename)
  (or buffer-file-name (setq buffer-file-name filename))
  (make-local-variable 'local-write-file-hooks)
  (add-hook 'local-write-file-hooks 'linuxproc-write-contents)
  (make-local-variable 'version-control)
  (setq version-control 'never)
  (make-local-variable 'make-backup-files)
  (setq make-backup-files nil)
  (auto-save-mode -1))

(defun linuxproc-write-contents  ()
  (save-restriction
    (widen)
    (clear-visited-file-modtime)
    (write-region (buffer-string) nil buffer-file-name nil t)
    (set-visited-file-modtime (nth 5 (file-attributes buffer-file-name)))))

(linuxproc-install)

(provide 'linuxproc)

;;; linuxproc.el ends here
