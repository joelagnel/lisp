;;; buffer-fns.el --- functions for modifying buffer contents or display

;; Copyright (C) 1991, 92, 93, 94, 95, 96, 97, 98, 99, 2000, 2006 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: buffer-fns.el,v 1.19 2006/06/06 02:01:47 friedman Exp $

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
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package defines a number of other unrelated functions, except that
;; they all more or less have to do with buffer manipulation, buffer or
;; modeline display, or lisp evaluation in buffers.  The main reason for
;; creating this package was to get these functions out of my .emacs file.

;;; Code:

(require 'list-fns)
(require 'emacs-variants)


;;; functions for operating on rectangles

(defun apply-on-rectangle-region-points (fun beg end &rest args)
  "Like `apply-on-rectangle', but pass points in the buffer instead of columns."
  (apply-on-rectangle
   (lambda (bcol ecol)
     (apply fun
            (progn
              (move-to-column bcol 'coerce)
              (point))
            (progn
              (move-to-column ecol 'coerce)
              (prog1
                  (point)
                (beginning-of-line)))
            args))
   beg end))

;;;###autoload
(defun downcase-rectangle (beg end)
  "Convert the marked rectangle to lower case."
  (interactive "r")
  (apply-on-rectangle-region-points 'downcase-region beg end))

;;;###autoload
(defun upcase-rectangle (beg end)
  "Convert the marked rectangle to upper case."
  (interactive "r")
  (apply-on-rectangle-region-points 'upcase-region beg end))


;;; buffer-percentage-mode functions

;;;###autoload
(defvar buffer-percentage-mode t
  "*If non-nil, display buffer percentage in mode line.
This variable is used by `set-default-mode-line-format.'")

;;;###autoload
(defvar buffer-percentage-mode-line-format
  '(buffer-percentage-mode ("" (-3 . "%p") " "))
  "*Buffer percentage mode line format.")

;;;###autoload
(defun buffer-percentage-mode (&optional prefix)
  "Toggle buffer-percentage-mode (see variable docstring).
If called with a positive prefix argument, always enable.
If called with a negative prefix argument, always disable.
If called with no prefix argument, toggle current state."
  (interactive "P")
  (setq buffer-percentage-mode
        (cond ((null prefix)
               (not buffer-percentage-mode))
              (t
               (>= (prefix-numeric-value prefix) 0)))))

;;;###autoload
(defun buffer-percentage-mode-install ()
  (let ((existing)
        (mlf (default-value 'mode-line-format))
        (member-car-fn (lambda (elt obj)
                         (and (consp obj)
                              (eq elt (car obj))))))
    (cond ((member 'buffer-percentage-mode-line-format mlf))
          ((member-by 'buffer-percentage-mode mlf member-car-fn))
          ((setq existing (member '(-3 . "%p") mlf))
           (setcar existing 'buffer-percentage-mode-line-format))
          ((setq existing (member-by 'column-number-mode mlf member-car-fn))
           (setcdr existing (cons 'buffer-percentage-mode-line-format
                                  (cdr existing)))))))


;;; other mode line hacks

;;;###autoload
(defvar buffer-directory-file-name nil
  "Pretty-printed form of directory in which current file resides.")
(make-variable-buffer-local 'buffer-directory-file-name)
(put 'buffer-directory-file-name 'permanent-local t)

;; This function must return nil or it will prevent buffers from ever
;; being saved if it's called by write-file-hooks (read documentation
;; for this hook to find out why).
;;;###autoload
(defun set-buffer-directory-file-name ()
  (interactive)
  (let ((name (buffer-file-name)))
    (and name
         (setq buffer-directory-file-name
               (pretty-directory-file-name (file-name-directory name)))))
  nil)

;; For sh-mode (sh-script.el)
;;;###autoload
(defun abbreviate-sh-mode-name ()
  ;; If prior to emacs 19.31, mode-line-process always has the shell name.
  (cond (mode-line-process
         (setq mode-name mode-line-process)
         (setq mode-line-process nil))
        ;; Otherwise, find it ourselves.
        ((save-match-data
           (save-excursion
             (let ((interp nil))
               (goto-char (point-min))
               (end-of-line)
               (setq interp (buffer-substring (point-min) (point)))
               (cond
                ((string-match "^#![ \t]*\\([^ \t\n]+\\)" interp)
                 (setq interp (matching-substring 1 interp))
                 (setq mode-name
                       (format "[%s]" (file-name-nondirectory interp)))))))))))

;;;###autoload
(defun toggle-mode-line-inverse-video (&optional current-only)
  (interactive)
  (cond ((fboundp 'set-face-attribute)
         (let ((onp (face-attribute 'modeline :inverse-video))
               (dt (cdr (assq 'display-type (frame-parameters)))))
           (set-face-attribute 'modeline
                               (and current-only (selected-frame))
                               :inverse-video (not onp))
           ;; This should be toggled on mono frames; in color frames, this
           ;; must always be t to use the face attribute.
           (setq mode-line-inverse-video (or (eq dt 'color) (not onp)))
           (force-mode-line-update (not current-only))))
        (t
         (setq mode-line-inverse-video (not mode-line-inverse-video))
         (force-mode-line-update (not current-only)))))

;;;###autoload
(defun bell-flash-mode-line ()
  "Effect ringing bell by flashing mode line momentarily.
In emacs 20.1 or later, you can use the variable `ring-bell-function'
to declare a function to run in order to ring the emacs bell."
  (let ((localp (local-variable-p 'mode-line-inverse-video)))
    (or localp
        (make-local-variable 'mode-line-inverse-video))
    (toggle-mode-line-inverse-video t)
    (sit-for 0 50)
    ;; Set it back because it may be a permanently local variable.
    (toggle-mode-line-inverse-video t)
    (or localp
        (kill-local-variable 'mode-line-inverse-video))))


;;; Lisp evaluation and variable modification functions

;;;###autoload
(defun make-local-copied-variables (&rest symlist)
  "Make all variables SYM1, SYM2, ... SYMn buffer-local in the current buffer.
If the variable is already buffer-local and is a sequence, copy it \(in case
any subsequences are shared\).  Otherwise, initialize variable with a copy
of the global default.

Return SYM1, for compatibility with `make-local-hook'.

Caveat: obarrays will not be copied properly by this function; use
make-local-obarray from obarray-fns.el for those instead."
  (let ((first (car symlist))
        sym already-buffer-local)
    (while symlist
      (setq sym (car symlist))
      (setq already-buffer-local (assq sym (buffer-local-variables)))
      (cond
       ((and already-buffer-local
             (sequencep (symbol-value sym)))
        (set sym (copy-alist (symbol-value sym))))
       (already-buffer-local)
       ((boundp sym)
        (make-local-variable sym)
        (if (sequencep (default-value sym))
            (set sym (copy-alist (default-value sym)))
          (set sym (default-value sym))))
       (t
        (make-local-variable sym)))
      (setq symlist (cdr symlist)))
    first))

;;;###autoload
(defun eval-page ()
  "Eval region in current buffer delimited by page markers.
If there is no explicit page beginning or end, point-min or point-max are
  used, respectively.
Page markers are specified with the regexp `page-delimiter'."
  (interactive)
  (let ((beg (point-min))
        (end (point-max))
        (opoint (point)))
    (save-match-data
      (and (re-search-forward page-delimiter nil t)
           (setq end (match-beginning 0)))
      (goto-char opoint)
      (and (re-search-backward page-delimiter nil t)
           (setq beg (match-end 0)))
      (goto-char opoint))
    (eval-region beg end)))

;;;###autoload
(defun eval-pretty-print-last-sexp ()
  (interactive)
  (require 'pp)
  (insert "\n")
  (pp-eval-last-sexp t))

;;;###autoload
(defun set-tab-stop-width (width)
"Sets tab-stop-list to list of tab stops separated by WIDTH
characters, up to, but not exceeding, 120."
  (setq tab-stop-list nil)
  (let ((tab-stop-counter width))
    (while (<= tab-stop-counter 120)
      (progn
        (setq tab-stop-list (cons tab-stop-counter tab-stop-list))
        (setq tab-stop-counter (+ tab-stop-counter width))))
    (setq tab-stop-list (nreverse tab-stop-list))))

;;;###autoload
(defun buffer-gc-undo (&optional buffer)
  "Wipe undo list for BUFFER, but leave it enabled.
Defaults to the current buffer."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (buffer-disable-undo buffer)
  (buffer-enable-undo buffer))

;;;###autoload
(defun flush-all-undo-lists ()
  (interactive)
  (save-excursion
    (let ((l (buffer-list)))
      (while l
        (set-buffer (car l))
        (and (consp buffer-undo-list)
             (setq buffer-undo-list nil))
        (setq l (cdr l))))))

;;;###autoload
(defun current-buffer-disable-undo ()
  (interactive)
  (buffer-disable-undo (current-buffer)))


;;; Miscellaneous utility functions

;; Copied from 19.29 simple.el `current-word', then modified very slightly.
;;;###autoload
(defun current-word-region (&optional strict)
  "Return the beginning and ending points of word point is on.
This returns a cons containing the offsets in the buffer delimiting the
beginning and ending point of the current word.

If optional arg STRICT is non-nil, return nil unless point is within
or adjacent to a word."
  (save-excursion
    (let ((oldpoint (point)) (start (point)) (end (point)))
      (skip-syntax-backward "w_") (setq start (point))
      (goto-char oldpoint)
      (skip-syntax-forward "w_") (setq end (point))
      (if (and (eq start oldpoint) (eq end oldpoint))
	  ;; Point is neither within nor adjacent to a word.
	  (and (not strict)
	       (progn
		 ;; Look for preceding word in same line.
		 (skip-syntax-backward "^w_"
				       (save-excursion (beginning-of-line)
						       (point)))
		 (if (bolp)
		     ;; No preceding word in same line.
		     ;; Look for following word in same line.
		     (progn
		       (skip-syntax-forward "^w_"
					    (save-excursion (end-of-line)
							    (point)))
		       (setq start (point))
		       (skip-syntax-forward "w_")
		       (setq end (point)))
		   (setq end (point))
		   (skip-syntax-backward "w_")
		   (setq start (point)))
		 (cons start end)))
	(cons start end)))))

;;;###autoload
(defun nuke-all-overlays (beg end)
  "Eliminate all overlays in marked region of current buffer.
This only removes overlays, not text properties."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((ovls (overlays-at (point)))
              (next-change (or (next-overlay-change (point))
                               (point-max))))
          (while ovls
            (delete-overlay (car ovls))
            (setq ovls (cdr ovls)))
          (goto-char next-change))))))

;;;###autoload
(defun nuke-all-text-properies (beg end)
  "Eliminate all text properties in marked region of current buffer.
This only removes text properties, not overlays."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((inhibit-read-only t)
              (plist (text-properties-at (point)))
              (next-change (or (next-property-change (point) (current-buffer))
                               (point-max))))
          (remove-text-properties (point) next-change plist (current-buffer))
          (goto-char next-change))))))

;; Poor man's html formatter.
;;;###autoload
(defun nuke-html-tags (beg end)
  (interactive "r")
  (let ((table '(("\n"                               . nil)
                 ("<p>"                              . "\n\n")
                 ("<br>"                             . "\n")
                 ("</?h[0-9]>"                       . "\n\n")
                 ("</?blockquote>"                   . "\n\n")
                 ("&nbsp;"                           . " ")
                 ("\\(&[^ <]*;\\)\\|\\(<[^>]*>\\)" . nil)))
        re sub)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (while table
          (setq re (car (car table)))
          (setq sub (cdr (car table)))
          (setq table (cdr table))

          (goto-char (point-min))
          (cond (sub
                 (while (re-search-forward re nil t)
                   (replace-match sub)))
                (t
                 (while (re-search-forward re nil t)
                   (delete-region (match-beginning 0) (match-end 0))))))))))

;;;###autoload
(defun messages ()
  "Display message log buffer, if it exists."
  (interactive)
  (let* ((variant (emacs-variant))
         (buffer-name (cond
                       ((eq variant 'emacs)  "*Messages*")
                       ((eq variant 'xemacs) " *Message-Log*")
                       (t (error "This emacs variant has no message log."))))
         (buf (get-buffer buffer-name))
         (curbuf (current-buffer))
         (curwin (selected-window))
         winbuf)
    (cond (buf
           (unwind-protect
               (progn
                 (setq winbuf (display-buffer buf))
                 (select-window winbuf)
                 (set-buffer buf)
                 (goto-char (point-max))
                 (recenter -1))
             (select-window curwin)
             (set-buffer curbuf)))
          (t
           (message "Message log is empty.")))))

;;;###autoload
(defun insert-numbers (start end &optional padp)
  "Insert the numbers from START to END (inclusive) in the current buffer.
Each is inserted on a separate line.  START may be less than END, in which
case counting is backward.

If given a prefix argument or optional arg PADP is non-nil, pad all numbers
with sufficient leading zeros so they are the same width."
  (interactive "nStart: \nnEnd: \nP")
  (let ((add-func (if (<= start end) '1+ '1-))
        (comp-func (if (<= start end) '<= '>=))
        (i start)
        (fmt (and padp (format "%%.%dd"
                               (length (int-to-string (max (abs start)
                                                           (abs end))))))))
    (while (funcall comp-func i end)
      (insert (if fmt (format fmt i) (int-to-string i)) "\n")
      (setq i (funcall add-func i)))))

;;;###autoload
(defun new-marker (pos &optional buffer insertion-type)
  "Copy existing marker, or make a new one from point.
POS may be a marker, in which case the marker is copied verbatim.
Otherwise, args POS and BUFFER are like those used by `set-marker'.
Arg INSERTION-TYPE is like that used by `set-marker-insertion-type',
which is present in Emacs 19.30 and later."
  (let ((new-marker nil))
    (cond ((markerp pos)
           (setq new-marker (copy-marker pos))
           (and buffer
                (set-marker new-marker (marker-position pos) buffer)))
          (t
           (setq new-marker (make-marker))
           (set-marker new-marker pos buffer)))
    (and (fboundp 'set-marker-insertion-type)
         (set-marker-insertion-type new-marker insertion-type))
    new-marker))

;;;###autoload
(defun occur-long-lines (&optional width)
  "Display all lines longer than WIDTH characters.
With no argument, the width used is the same as `fill-column'.
With a positive prefix argument, display all lines with width equal to or
greater than that many characters."
  (interactive "P")
  (cond ((null width)
         (setq width fill-column))
        ((consp width)
         (setq width (car width))))
  (let ((re (concat "^" (make-string width ?.))))
    (save-match-data
      (cond ((save-excursion
               (re-search-forward re nil t))
             (occur re))
            (t
             (message "No lines >= %d characters" width))))))

;;;###autoload
(defun zippify-region (beg end &optional rand-limit)
  "Randomly capitalize certain words in the region.
From Lisp, wants BEG and END.
Optional third arg RAND-LIMIT means capitalize roughly one out of
every RAND-LIMIT words."
  (interactive "rp")
  (or rand-limit (setq rand-limit 8))
  (save-excursion
    (goto-char beg)
    (if (bobp) nil (forward-word -1) (forward-word 1))
    (while (< (point) end)
      (if (zerop (random rand-limit))
          (upcase-word 1)
        (forward-word 1)))))


;;; Misc buffer functions

;;;###autoload
(defun kill-all-buffers (&optional confirm)
  "Attempt to kill all buffers.
When called interactively, prompt for confirmation.
When called from lisp, always kill all buffers.

Some buffers might not actually be killed, depending on the value of their
`kill-buffer-query-functions' (which see)."
  (interactive (list (yes-or-no-p "Are you sure you want to kill all buffers? ")))
  (cond ((and (interactive-p)
              (not confirm))
         (message "Not killing buffers"))
        (t
         (mapc 'kill-buffer (buffer-list)))))


;;; Modifications to buffer commands.
;;; These have no autoload cookie because they do not define new commands,
;;; just modify existing ones.

;; This is defined but not activated by default.
(defadvice capitalize-word (before upcase-before-middle)
  "If point is in the middle of a downcased word, no argument is
just like \\[negative-argument] \\[capitalize-word]."
  (interactive "p")
  (if (and (= 1 (ad-get-arg 0))
           (looking-at "\\B\\w")
           (save-excursion
             (string= (current-word) (downcase (current-word)))))
      (ad-set-arg 0 -1)))

(defadvice rename-buffer (before interactive-edit-buffer-name activate)
  "Prompt for buffer name supplying current buffer name for editing."
  (interactive
   (list (let ((minibuffer-local-completion-map
                (copy-keymap minibuffer-local-completion-map)))
           (define-key
             minibuffer-local-completion-map " " 'self-insert-command)
           (completing-read "Rename current buffer to: "
                            (mapcar (lambda (buffer)
                                      (list (buffer-name buffer)))
                                    (buffer-list))
                            nil
                            nil
                            (if (string-lessp "19" emacs-version)
                                (cons (buffer-name) 0)
                              (buffer-name))))
         current-prefix-arg)))


;;; display-table functions

;; These are representational glyphs for characters present in the PalmOS
;; and various W32 font maps.  Since I don't have fonts from these systems
;; on my X desktop (and I use a tty for emacs much of the time anwyay),
;; these ascii glyphs are an aid to reading documents native to these other
;; systems which are basically just a superset of iso8859-1.
;; For bitmap frames, I may try to use unicode glyphs if available.
(defvar disptable-w32/palmos-8bit-glyph-map
  '((?\x80 nil      ?\x20ac)  ; euro sign; european monetary union currency symbol
    (?\x86 nil      ?\x2020)  ; {t}  dagger, obelisk, obelus, long cross
    (?\x87 nil      ?\x2021)  ; {tt} double dagger, diesis, double obelisk
    (?\x8a nil      ?\x0160)  ; latin capital letter S with caron (inverted ^)
    (?\x8c "{OE}"   ?\x0152)  ; latin capital ligature OE
    (?\x82 "{,}"    ?\x201A)  ; single low-9 quotation mark
    (?\x83 "{f}"    ?\x0192)  ; latin small letter f with hook; florin (netherlands currency)
    (?\x84 "{,,}"   ?\x201E)  ; double low-9 quotation mark
    (?\x85 "{...}"  ?\x2026)  ; horizontal elipsis
    (?\x88 "{^}"    ?\x2038)  ; caret
    (?\x89 "{o/oo}" ?\x2030)  ; per mille sign
    (?\x8b "{<}"    ?\x2039)  ; single left-pointing angle quotation mark
    (?\x8d nil      ?\x2662)  ; white diamond suit
   ;(?\x8d nil      ?\x2666)  ; black diamond suit
   ;(?\x8e nil      ?\x2667)  ; white club suit
    (?\x8e nil      ?\x2663)  ; black club suit, shamrock
    (?\x8f nil      ?\x2661)  ; white heart suit
   ;(?\x8f nil      ?\x2665)  ; black heart suit
   ;(?\x90 nil      ?\x2664)  ; white spade suit
    (?\x90 nil      ?\x2660)  ; black spade suit
    (?\x91 "{`}"    ?\x2018)  ; left single quotation mark
    (?\x92 "{'}"    ?\x2019)  ; right single quotation mark
    (?\x93 "{``}"   ?\x201C)  ; left double quotation mark
    (?\x94 "{''}"   ?\x201D)  ; right double quotation mark
    (?\x95 "{.}"    ?\x2022)  ; bullet
   ;(?\x95 "{.}"    ?\x2024)  ; one dot leader
   ;(?\x95 "{.}"    ?\x2027)  ; hyphenation point
    (?\x96 "{-}"    ?\x2013)  ; en dash
    (?\x97 "{--}"   ?\x2014)  ; em dash
    (?\x98 "{~}"    ?\x223c)  ; tilde operator; varies with, proportional to
    (?\x99 "{tm}"   ?\x2122)  ; trade mark sign
    (?\x9a nil      ?\x0161)  ; {vs} latin small letter s with caron (inverted ^)
    (?\x9b "{>}"    ?\x203a)  ; single right-pointing angle quotation mark
    (?\x9c "{oe}"   ?\x0153)  ; latin small ligature oe
    (?\x9f "{:Y}"   ?\x0178)  ; latin capital letter Y with diaresis
    (?\xa0 "{ }"    ?\x2423)) ; no-break space, &nbsp; for ucs, use "open box"
  "Map for various upper 8-bit values to 7-bit ascii or iso10646 glyphs.
These are representational glyphs for characters present in the PalmOS
\(and W32?\) default character set.  They are inserted into a display table
via `disptable-insert-w32/palmos-8bit-glyphs' \(which see\).

This table is an alist of the form ((char 7bit ucs) ...) where `char' is an
8-bit character, `7bit' is a 7bit-only representation of the character, and
`ucs' is a unicode character value, e.g.

    \(?\\x97  \"{--}\"   ?\\x2014\)  ; em dash

The 7bit and ucs entries can be any of `nil', a string, a vector of
characters, or a unicode character code (e.g. U+2014 can be represented in lisp as
\"?\\x2014\" or \"8212\".")

(defun disptable-insert-w32/palmos-8bit-glyphs (disp-table &optional map ucs-p)
  "Insert glyphs for PalmOS 8-bit characters into DISP-TABLE.
The glyphs are obtained from `disptable-w32/palmos-8bit-glyph-map' unless
the optional arg MAP is provided.

By default a 7-bit string representation (if any) of the character is
presented.  However the optional arg UCS-P non-nil means use a multibyte
utf8 character glyph in its place.  It probably only makes sense to use
these on a bitmap display where iso10646 charset fonts are available."
  (or map (setq map disptable-w32/palmos-8bit-glyph-map))
  (let ((tbl disp-table)
        c g)
    (while map
      (setq c   (car (car map))
            g   (nth (if ucs-p 2 1) (car map))
            map (cdr map))
      (cond ((or (null g) (vectorp g))
             (aset tbl c g))
            ((stringp g)
             (aset tbl c (string-to-vector g)))
            ((and (>= g 0) (< g 256))
             (aset tbl c (vector g)))
            ((fboundp 'decode-char)
             (aset tbl c (vector (decode-char 'ucs g))))))))

(provide 'buffer-fns)

;;; buffer-fns.el ends here.
