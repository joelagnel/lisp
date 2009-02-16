;;; order-head.el --- re-arrange mail headers into pleasing order

;;; Copyright (C) 1992, 1993, 1995, 1997, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: mail, extensions
;; Created: 1992

;; $Id: order-head.el,v 1.9 1999/03/21 09:12:29 friedman Exp $

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

;; This package used to be called mail-reorder-headers.el,
;; but that name is too long for some filesystems.

;; Only an anal retentive pedant would actually use this package.

;;; Code:

(require 'rfc822)
(require 'fmailutils)

(defvar mail-reorder-headers-preferred
  '("From" "To" "Cc" "Bcc" "Fcc" "Subject" "Reply-To" "Date" "Message-Id")
  "*A list of headers to sort with decreasing priority.
Any headers not listed are sorted last.")

(defvar mail-reorder-headers-address-headers
  '("To" "Cc" "Bcc" "Resent-To" "Reply-To")
  "*A list of headers that are commonly used to supply address lists.")

(defvar mail-reorder-headers-delete nil
  "*A list of headers to delete entirely.")

(defvar mail-reorder-headers-concat-identical-address-headers t
  "*If non-nil, concatenate the contents of duplicate address headers.
That is, if there are two address headers with the same name, their
contents are merged into a single header, separated by commas.

Only those headers listed in `mail-reorder-headers-address-headers' are
examined.

If this variable is nil, headers will be sorted but not concatenated.")

(defvar mail-reorder-headers-use-rfc822 nil
  "*If non-nil, canonicalize address headers.
That is, remove newlines, excess spaces, and comments; and add commas in
the right places.")


(defmacro mail-reorder-headers-save-undo-boundary (&rest body)
  "Evaluate BODY, avoiding creating new undo boundaries from buffer excursions.

When a form switches buffers and performs insertions or deletions in that
other buffer, the next insertion or deletion in the current buffer will
create a new undo boundary.

That behavior can be undesirable when the user issues a single command but
has to perform multiple undo commands to undo all its effects.
Use this macro to prevent that from happening."
  (` (prog1
         ;; This body may do some insertion and/or deletion in another
         ;; buffer.  Doing so will cause last_undo_buffer to be that buffer
         ;; instead of our current buffer.
         (progn (,@ body))
       ;; This dynamic let preserves our buffer's real undo list.
       (let ((buffer-undo-list nil)
             (buffer-read-only nil))
         ;; Do an insertion to cause the unwanted undo boundary,
         ;; and to set last_undo_buffer to the current buffer.
         (insert "\146\156\157\162\144")
         ;; Undo the insertion, leaving the buffer as it was.  Now that
         ;; last_undo_buffer is the current buffer, later insertions will not
         ;; cause an undo boundary in the original undo list.
         (primitive-undo 1 buffer-undo-list)))))

;; indent like save-excursion
(put 'mail-reorder-headers-save-undo-boundary 'lisp-indent-function 0)

(defsubst mail-reorder-headers-rfc822-addresses (address-list)
  (mail-reorder-headers-save-undo-boundary
    (rfc822-addresses (mapconcat 'identity address-list ", "))))


;;;###autoload
(defun mail-reorder-headers ()
  "Sort mail headers according to variable `mail-reorder-headers-preferred'.
Leave the order of any other headers at the end in their original order.

Headers listed in `mail-reorder-headers-delete' are removed completely.

The variable `mail-reorder-header-concat-identical-address-headers' specifies
whether duplicate address headers are concatenated.
The variable `mail-reorder-headers-use-rfc822' specifies whether to
canonicalize address lists."
  (interactive)
  (let ((position-data (mail-reorder-headers-save-position)))
    (mail-reorder-headers-delete mail-reorder-headers-delete)
    (mail-reorder-headers-restore-headers
     (mail-reorder-headers-collect-headers
      (mail-reorder-headers-get-header-names
       (mapcar 'capitalize mail-reorder-headers-preferred))))
    (mail-reorder-headers-restore-position position-data)))

;; Save position of point relative to current header so that after deleting
;; and restoring headers, point can be restored to the same relative position.
;; If point is not currently in a header, the absolute position can be
;; saved and restored.
(defun mail-reorder-headers-save-position ()
  (let* ((current-header (fmailutils-current-header))
         (saved-point (point))
         (contents-offset
          (and current-header
               (- saved-point (fmailutils-current-header-start)))))
    (if current-header
        (list current-header contents-offset saved-point)
      saved-point)))

(defun mail-reorder-headers-restore-position (data)
  (cond ((consp data)
         (if (fmailutils-position-on-field (nth 0 data) 'soft)
             (goto-char (+ (fmailutils-current-header-start)
                           (nth 1 data)))
           (goto-char (nth 2 data))))
        (t
         (goto-char data))))

;; Delete headers we never want.
(defun mail-reorder-headers-delete (headers)
  (while headers
    (fmailutils-remove-header (car headers) 'all-occurrences)
    (setq headers (cdr headers))))

;; Get list of existing header names and sort them so that preferred
;; headers have priority.  The order is least->most preferred.
(defun mail-reorder-headers-get-header-names (preferred)
  (let ((existing (mapcar 'capitalize (fmailutils-get-header-names 'unique)))
        (names nil))
    (while (and preferred existing)
      (and (mail-reorder-headers-string-member (car preferred) existing)
           (setq names (cons (car preferred) names)
                 existing (mail-reorder-headers-string-delete
                           (car preferred) existing)))
      (setq preferred (cdr preferred)))
    (while existing
      (setq names (cons (car existing) names))
      (setq existing (cdr existing)))
    names))

;; For each header in list, save the name and contents of the header in
;; an alist, then delete all occurences of header from the buffer.
;; An alist of the form ((header contents ...) ...)  is returned.
(defun mail-reorder-headers-collect-headers (header-names)
  (let ((saved-alist nil)
        header-contents header)
    (while header-names
      (setq header (car header-names))
      (setq header-contents (fmailutils-get-header-contents header))
      (setq header-names (cdr header-names))
      (cond (header-contents
             (setq saved-alist
                   (cons (cons header header-contents)
                         saved-alist))
             (fmailutils-remove-header header 'all-occurences))))
    saved-alist))

(defun mail-reorder-headers-restore-headers (saved-alist)
  (let ((address-headers (mapcar 'capitalize
                                 mail-reorder-headers-address-headers))
        header contents)
    (while saved-alist
      (setq header (car (car saved-alist)))
      (setq contents (cdr (car saved-alist)))
      (setq saved-alist (cdr saved-alist))
      (cond ((null contents))
            ((and mail-reorder-headers-concat-identical-address-headers
                  (mail-reorder-headers-string-member header address-headers))
             (and mail-reorder-headers-use-rfc822
                  (setq contents
                        (mail-reorder-headers-rfc822-addresses contents)))
             (setq contents (mapconcat 'identity contents ", "))
             (fmailutils-put-unique-header header contents 'force-replace))
            (t
             (while contents
               (fmailutils-put-header header (car contents))
               (setq contents (cdr contents))))))))

;; In emacs 19.34, `member' cannot tell if two strings are equal if one of
;; the strings contains text properties, so we must iterate over the list
;; with string-equal.
;; Too bad about the performance hit.
(defun mail-reorder-headers-string-member (x y)
  (while (and y (not (string-equal x (car y))))
    (setq y (cdr y)))
  y)

(defun mail-reorder-headers-string-delete (elt list)
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (string-equal elt (car l))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (string-equal elt (car list))
      (cdr list)
    list))

;; Make a keybinding for the single command we defined.
(define-key mail-mode-map "\C-c\C-f\C-r" 'mail-reorder-headers)

(provide 'order-head)
(provide 'mail-reorder-headers) ; name prior to revision 1.5

;;; order-head.el ends here.
