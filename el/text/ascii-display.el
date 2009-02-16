;;; ascii-display.el --- Highlight special ASCII characters in another buffer

;; Copyright (C) 2000 Colin Walters

;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: Colin Walters <walters@verbum.org>
;; URL: http://web.verbum.org/~walters
;; Keywords: ASCII
;; Version: 1.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

;;; Commentary:

;; Just use 'M-x ascii-display-buffer' to see special characters
;; highlighted.  You can also do 'M-x ascii-display-file' to display a
;; file.

;; *** IMPORTANT NOTE ON CODING SYSTEMS ***

;; You should be aware of the interaction between Emacs' coding
;; systems and this package.  In particular, Emacs canonicalizes
;; Macintosh and MS-DOS line-ending conventions to Unix-style newlines
;; internally, and converts them back when writing a file.
;; ascii-display.el tries to show you what the file will actually be,
;; by noticing that the buffer is in a special coding system, and
;; displaying the EOL conventions for that coding system.  This is a
;; lie, because there aren't *really* carriage returns (or whatever)
;; in the buffer.

;; In summary, ascii-display tries to do the right thing, but it is no
;; more than a bad hack.  Don't be surprised if it fails, or gives
;; unexpected results.

;; *** END IMPORTANT NOTE ON CODING SYSTEMS ***

;; Please send improvements or comments to the maintainer.

;;; Compatibility:

;; Developed and tested on GNU Emacs 20.6.2.

;;; ChangeLog:

;; Version 1.3:
;; * Attempt to display the end-of-line convention of the file or
;;   buffer's coding system.  Add function 'ascii-display-file'.

;; Version 1.2:
;; * Make faces more easily customizable.  Thanks to
;;   Diego Calvanese <calvanese@dis.uniroma1.it>.

;; Version 1.1:
;; * Add support for displaying in multiple faces.

;; Version 1:
;; * Initial release.

;;; Code:

(defgroup ascii-display '()
  "Utility to highlight ASCII special characters, like tab and newline."
  :group 'files)

(defun ascii-display-file (file)
  "Display a file's ASCII special characters in a separate buffer.  See
`ascii-display-buffer' for details."
  (interactive "fASCII display file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (ascii-display-buffer (current-buffer))))

(defun ascii-display-buffer (buf)
  "Display the current buffer's ASCII special characters in a separate buffer.
Tabs (ASCII code 0x09) are underlined.
Newlines (ASCII code 0x0A) are displayed with a bold \\n.
Carriage Returns (ASCII code 0x0D) are displayed with a bold \\r.
All other non-printing ASCII characters are displayed with their
two-digit hexadecimal code in bold.

Please note that if the buffer is in a special coding system like (DOS) or (Mac),
this function will attempt to display the end-of-line conventions for these systems,
even though Emacs is not using these formats internally.  See the comment at the top
of the source code for this package."
  (interactive "bASCII display buffer: ")
  (with-current-buffer buf
    (cond ((string-match "mac$" (symbol-name buffer-file-coding-system))
	   ;; dynamic scoping: whee!
	   (let ((ascii-display-eol-convention "\\r"))
	     (message "ascii-display: displaying as Macintosh codes")
	     (ascii-display-region (point-min) (point-max))))
	  ((string-match "dos$" (symbol-name buffer-file-coding-system))
	   (let ((ascii-display-eol-convention "\\r\\n"))
	     (message "ascii-display: displaying as MS-DOS codes")
	     (ascii-display-region (point-min) (point-max))))
	  (t
	   (ascii-display-region (point-min) (point-max))))))

(defcustom ascii-display-full nil
  "If non-nil, then force ascii-display to check the buffer \
for every special ASCII character.  Defaults to nil."
  :type 'boolean
  :group 'ascii-display)

(defcustom ascii-display-minimum
  '(9 10 13)
  "List of integer ASCII codes. If `ascii-display-full' is nil, then \
only check for these ASCII characters."
  :type '(repeat integer)
  :group 'ascii-display)  

(defcustom ascii-display-default-face 'bold
  "The face to use for highlighting by default."
  :type 'face
  :group 'ascii-display)

(defcustom ascii-display-tab-face 'underline
  "The face to use for highlighting TAB characters."
  :type 'face
  :group 'ascii-display)

(defcustom ascii-display-alist
  '((0 "00" ascii-display-default-face t)
   (1 "01" ascii-display-default-face t)
   (2 "02" ascii-display-default-face t)
   (3 "03" ascii-display-default-face t)
   (4 "04" ascii-display-default-face t)
   (5 "05" ascii-display-default-face t)
   (6 "06" ascii-display-default-face t)
   (7 "07" ascii-display-default-face t)
   (8 "08" ascii-display-default-face t)
   (9 "" ascii-display-tab-face nil)
   (10 "" ascii-display-default-face nil)
   (11 "0B" ascii-display-default-face t)
   (12 "0C" ascii-display-default-face t)
   (13 "\\r" ascii-display-default-face t)
   (14 "0E" ascii-display-default-face t)
   (15 "0F" ascii-display-default-face t)
   (16 "10" ascii-display-default-face t)
   (17 "11" ascii-display-default-face t)
   (18 "12" ascii-display-default-face t)
   (19 "13" ascii-display-default-face t)
   (20 "14" ascii-display-default-face t)
   (21 "15" ascii-display-default-face t)
   (22 "16" ascii-display-default-face t)
   (23 "17" ascii-display-default-face t)
   (24 "18" ascii-display-default-face t)
   (25 "19" ascii-display-default-face t)
   (26 "1A" ascii-display-default-face t)
   (27 "1B" ascii-display-default-face t)
   (28 "1C" ascii-display-default-face t)
   (29 "1D" ascii-display-default-face t)
   (30 "1E" ascii-display-default-face t)
   (31 "1F" ascii-display-default-face t))
"Alist of ASCII codes.
The first element is the string to display before the ASCII code.
The second element is the face to use to highlight the inserted string and the character.
If the third element is t, then the actual character is not displayed, but any inserted
string will still be highlighted."
  :type '(repeat (list integer string face boolean))
  :group 'ascii-display)

(defvar ascii-display-eol-convention "\\n"
  "The default string for line endings.  You shouldn't need to set this.")

(defun ascii-display-region (pt mk)
  "Display the ASCII special characters in the region in a separate buffer.
Tabs (ASCII code 0x09) are displayed with an underlined region.
Newlines (ASCII code 0x0A) are displayed with a bold \\n.
Carriage Returns (ASCII code 0x0D) are displayed with a bold \\r.
All other non-printing ASCII characters are displayed with their two-digit
hexadecimal code in bold.

This function does *not* attempt to display the end-of-line conventions
for the buffer.  See `ascii-display-buffer' for that."
  (interactive "r")
  (let ((outbuf (get-buffer-create "*Ascii-Display*"))
	(contents (buffer-substring-no-properties pt mk))
	(display-alist nil))
    (with-current-buffer outbuf
      ;; font-lock-mode is buffer-local, and we don't want it
      (setq font-lock-mode nil)
      (setq buffer-read-only nil)
      ;; copied from gnus-kill-all-overlays
      (let ((overlays (nconc (car (overlay-lists)) (cdr (overlay-lists)))))
	(while overlays
	  (delete-overlay (car overlays))
	  (setq overlays (cdr overlays))))
      (erase-buffer)
      (insert contents)
      (let ((ascii-display-alist (if ascii-display-full
				     ascii-display-alist
				   (mapcar #'(lambda (e)
					       (assoc e ascii-display-alist))
					   ascii-display-minimum))))
	(while ascii-display-alist
	  (let ((curchar (caar ascii-display-alist))
		(display-str (car (cdar ascii-display-alist)))
		(face (cadr (cdar ascii-display-alist)))
		(invis (car (cddr (cdar ascii-display-alist)))))
	    (goto-char (point-min))
	    (while (search-forward (string curchar) nil t)
	      (backward-char 1)
	      (let ((start (point)))
		;; ghetto hack alert
		(if (eq curchar 10)
		    (insert ascii-display-eol-convention)
		  (insert display-str))
		(let ((end (point)))
		  ;; skip the character
		  (forward-char 1)
		  (let ((over (make-overlay start (+ end 1))))
		    ;; Emacs 20 display engine doesn't like highlighting newlines
		    (when (eq curchar 10)
		      (move-overlay over start end))
		    (when invis
		      (move-overlay over
				    (overlay-start over)
				    (- (overlay-end over) 1))
		      (let ((hider (make-overlay (overlay-end over)
						 (+ (overlay-end over) 1))))
			(overlay-put hider 'invisible t)))
		    (overlay-put over 'face (symbol-value face)))))))
	  (setq ascii-display-alist (cdr ascii-display-alist))))
      (setq buffer-read-only t)
      (display-buffer outbuf))))

(provide 'ascii-display)

;; ascii-display.el ends here
