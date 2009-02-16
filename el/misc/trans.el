;;; trans.el --- translation of characters via multibyte table or coding system

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: lisp, i18n

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides an equivalent of `translate-region' that works with
;; multibyte characters.

;; Example, translating Latin-1 soft hyphens to hyphens and
;; non-breakable spaces to spaces throughout a buffer:
;;  (translate-multibyte-region
;;   (point-min) (point-max) (make-translation-table
;;                            '((?\x8ad . ?-) (?\x8a0 . ?\ ))))

;; This is an unusual application of CCL, using a scratch coding
;; system to do the work.

;; Also provides vaguely-related command `recode-coding-region' which
;; transcodes the region into what would be decoded by a specified
;; coding system.  This is useful for things like interconverting
;; between characters from the iso8859 and mule-unicode charsets with
;; unification on encoding provided by ucs-tables.el.

;;; Code:

(defun translate-multibyte-region (beg end table)
  "Translate region according to TABLE.
TABLE is a symbol naming a translation table made by
`define-translation-table'.  Non-interactively, TABLE may also be an
actual translation table, e.g. made by `make-translation-table'.

This does general translation of unibyte or multibyte characters,
unlike `translate-region' which only translates unibyte characters.
It works via a scratch CCL coding system `multibyte-translate'."
  (interactive "r\nSTranslation table name: ")
  (unless enable-multibyte-characters
    (error "Buffer is unibyte"))
  (let ((tab (if (symbolp table)
		 (get table 'translation-table)
	       table))
	(id (get 'multibyte-translate 'translation-table-id)))
    (unless (and (char-table-p tab)
		 (eq 'translation-table (char-table-subtype tab)))
      (error "`%s' isn't a translation table" table))
    (if id
	;; Frob the table in the existing entry for the symbol we use
	;; in the CCL.  (That's what the CCL internals look at.)
	(aset translation-table-vector id (cons 'multibyte-translate tab))
      ;; Otherwise, we need to generate one.
      (define-translation-table 'multibyte-translate tab)))
  (unless (coding-system-p 'multibyte-translate)
    ;; Trivial CCL coding system to do the translation.
    (define-ccl-program multibyte-translate
      ;; The buffer magnification needs to be able to accommodate
      ;; single bytes converted to the maximum size of multibyte
      ;; character (four for private 2-dimensional charsets).
      '(4
	((loop
	  (read-multibyte-character r0 r1)
	  (translate-character multibyte-translate r0 r1)
	  (write-multibyte-character r0 r1)
	  (repeat))))
      "Simply apply translation table `multibyte-translate'.  Internal use.")
    (make-coding-system 'multibyte-translate 4 ?*
			"Internal use."
			'(multibyte-translate . multibyte-translate)
			'()
			'unix)		; avoid subsidiaries
    ;; Avoid it in completions.
    (pop coding-system-alist))
  ;; Apply the translation.  (It could just as well be a decode
  ;; operation.)
  (set-buffer-multibyte nil)
  (encode-coding-region beg end 'multibyte-translate)
  (set-buffer-multibyte t))

(defun recode-coding-region (beg end coding-system)
  "Re-code the region BEG to END using the given CODING-SYSTEM.
First check that CODING-SYSTEM can encode the region.  Then encode it
and decode it using CODING-SYSTEM.

This can be useful to normalize the Emacs charset(s) for the region,
e.g. to convert from iso-8859 to mule-unicode-... (coding system
utf-8) or between the compatible subsets of Latin-1 and Latin-9.  The
latter requires character unification to be on -- see library
`ucs-tables.el'."
  (interactive "r\nzCoding system: ")
  (unless (member (coding-system-base coding-system)
		  (find-coding-systems-region beg end))
    (error "%s can't encode the region" coding-system))
  (save-restriction
    (narrow-to-region beg end)
  (encode-coding-region (point-min) (point-max) coding-system)
  (decode-coding-region (point-min) (point-max) coding-system)))

(provide 'trans)
;;; trans.el ends here
