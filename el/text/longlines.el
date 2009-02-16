;;; longlines.el --- automatically wrap long lines -*- coding: iso-8859-1; -*-

;; Copyright (C) 2000, 2001 by Free Software Foundation, Inc.

;; Author: Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;;         Alex Schroeder <alex@gnu.org>
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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some text editors save text files with long lines, and they
;; automatically break these lines at whitespace, without actually
;; inserting any newline characters.  When doing `M-q' in Emacs, you
;; are inserting newline characters.  This file provides a file format
;; which automatically fills the long lines when reading a file and
;; unfills the lines when saving the file.

;; There is also a mode function, so you can do stuff like the
;; following:
;;
;; (add-to-list 'auto-mode-alist '("\\.ll\\'" . longlines-mode))
;;
;; This means that all `.ll' files are assumed to have long lines
;; which will be decoded on reading an encoded on writing.

;; If you want to check wether it worked or not, invoke
;; `longlines-show-hard-newlines'.
;;
;; If you want to prevent certain lines from being wrapped, customize
;; `longlines-nowrap-regexp'

;; Selective Display

;; Longlines mode cannot deal with selective display.  In some cases it
;; may be possible to still get usable results.

;; Folding Mode

;; Use the following code to make sure folding-mode is switched off
;; while longlines-mode does its setup.

;; (defadvice longlines-mode-on (around longlines-folding activate)
;;   "Do the right then when `folding-mode' is active."
;;   (let ((refold (and (boundp 'folding-mode)
;; 		     folding-mode)))
;;     (when refold
;;       (folding-mode -1))
;;     ad-do-it
;;     (when refold
;;       (folding-mode 1))))

;;; Code:

(require 'easy-mmode); for easy-mmode-define-minor-mode
(when (not (fboundp 'overlays-in))
  (require 'overlay)); XEmacs needs this

(defconst longlines-version "$Id: longlines.el,v 1.10 2001/07/13 00:03:40 alex Exp $"
  "Version information.")

;; Customization

(defgroup longlines nil
  "Automatic wrapping of long lines when loading files."
  :group 'fill)

(defcustom longlines-nowrap-when-yanking nil
  "*Determines wether long lines will be wrapped when yanked.
Note that this will only wrap the yanked region using
`fill-region-as-paragraph'.  It will not wrap the paragraph into which
the region was yanked.  Therefore, if testing superficially, you might
get the impression that this option has no effect."
  :group 'longlines
  :type 'boolean)

(defcustom longlines-show-hard-newlines nil
  "*Determines wether hard newlines are shown by default."
  :group 'longlines
  :type 'boolean)

(defcustom longlines-show-effect '(before-string . "¶")
  "Determines how hard newlines are made visible."
  :group 'longlines
  :type '(choice
	  (const :tag "Pi" (before-string . "¶"))
	  (const :tag "Default Face" (face . longlines-visible-face))
	  (cons :tag "Custom Face"
		:value (face . longlines-visible-face)
		(const :tag "Property" face) face)
	  (cons :tag "Property" 
		:value (face . longlines-visible-face)
		(symbol :tag "Property") (sexp :tag "Value"))))

;; When adding support for specific modes (eg. LaTeX), then you
;; should use the following in the respective mode hook:
;; (add-hook 'latex-mode-hook (lambda ()
;;   (set (make-local-variable 'longlines-nowrap-regexp) REGEXP))

(defcustom longlines-nowrap-regexp "\\S-\\(\\s-\\s-\\s-\\|\t\\)"
  "*Regexp matching lines that shall not be wrapped."
  :group 'longlines
  :type 'regexp)

;; Mode

(easy-mmode-define-minor-mode longlines-mode
  "Longlines mode automatically wraps long lines into paragraphs.
The variable `longlines-show-hard-newlines' determines
wether hard newlines will be shown when lonlines mode is
entered."
  nil " ll")

;;;###autoload
(defun longlines-mode-on ()
  "Turn longlines mode on.
This automatically wraps lines using `longlines-wrap'."
  (interactive)
  (when selective-display
    (setq longlines-mode nil)
    (error "longlines-mode is incompatible with selective-display"))
  (add-to-list 'buffer-file-format 'longlines)
  (let ((mod (buffer-modified-p)))
    (longlines-wrap (point-min) (point-max))
    (set-buffer-modified-p mod))
  (when (and longlines-show-hard-newlines
	     (not longlines-showing))
    (longlines-show-hard-newlines)))

;;;###autoload
(defun longlines-mode-off ()
  "Turn longlines mode off.
This automatically unwraps lines using `longlines-unwrap'."
  (interactive)
  (setq buffer-file-format (delete 'longlines buffer-file-format))
  (longlines-unshow-hard-newlines)
  (let ((mod (buffer-modified-p)))
    (longlines-unwrap (point-min) (point-max))
    (set-buffer-modified-p mod)))

(add-hook 'longlines-mode-on-hook 'longlines-mode-on)
(add-hook 'longlines-mode-off-hook 'longlines-mode-off)

;; Showing the effect of hard newlines in the buffer

(defvar longlines-showing nil
  "Whether the current buffer is showing hard newlines.")
(make-variable-buffer-local 'longlines-showing)

(defface longlines-visible-face
  '((t (:background "red")))
  "Face used to make hard newlines visible in `longlines-mode'.")

(defun longlines-show-hard-newlines (&optional arg)
  "Make hard newlines visible by adding a face.
With optional argument ARG, make the hard newlines invisible again
by calling `longlines-unshow-hard-newlines'."
  (interactive "P")
  (if arg
      (longlines-unshow-hard-newlines)
    (save-excursion
      (setq longlines-showing t)
      ;; We want to show every single newline.  Therefore every single
      ;; newline must get its own overlay.  No spanning of multiple
      ;; newlines with one overlay.
      (let ((pos (text-property-any (point-min) (point-max) 'hard t)))
	(while pos
	  (longlines-show-region pos (1+ pos))
	  (setq pos (text-property-any (1+ pos) (point-max) 'hard t)))))))

(defun longlines-show-region (start end)
  "Make region between START and END visible."
  (unless (let ((os (overlays-in start end))
		o exists)
	    (while (and os (not exists))
	      (setq o (car os)
		    os (cdr os))
	      (when (overlay-get o 'longlines)
		(setq exists t)))
	    exists)
    (let ((o (make-overlay start end nil t)))
      (overlay-put o (car longlines-show-effect) (cdr longlines-show-effect))
      (overlay-put o 'longlines t)
      (overlay-put o 'evaporate t))))

(defun longlines-unshow-hard-newlines ()
  "Make hard newlines invisible again."
  (interactive)
  (setq longlines-showing nil)
  (let ((os (overlays-in (point-min) (point-max)))
	o)
    (while os
      (setq o (car os)
	    os (cdr os))
      (when (overlay-get o 'longlines)
	(delete-overlay o)))))

;; Wrapping and unwrapping the paragraphs.

(defun longlines-wrap-p (start end)
  "Return non-nil if region between START and END may be wrapped."
  (save-excursion
    (goto-char start)
    (not (re-search-forward longlines-nowrap-regexp end t))))

(defun longlines-wrap (start end &optional nowrap)
  "Wrap long lines in the region from START to END.
All newlines will be turned into hard newlines.  When optional argument
NOWRAP is nil, the lines also wrapped.  When yanking text, for example,
we don't want to wrap it automatically because it might as well be
source code.  Users can do that themselves if necessary using \\[fill-paragraph]."
  (interactive "rP")
  (let (pos)
    (use-hard-newlines 1 'never)
    (save-excursion
      (goto-char start)
      (while (search-forward "\n" end t)
        (set-hard-newline-properties (match-beginning 0) (match-end 0)))
      (when (not nowrap)
	(goto-char start)
	(setq pos (point))
	(while (and (zerop (forward-line 1))
		    (<= (point) end))
	  (when (longlines-wrap-p pos (point))
	    (fill-region-as-paragraph pos (point)))
	  (setq pos (point))))
      (setq longlines-mode t)
      pos)))

(defun longlines-unwrap (start end &optional buffer)
  "Unwrap long lines in the region from START to END.
The region is assumed to contain short lines and soft and hard newlines.
Soft newlines and any following whitespace on the next line will be
replaced with exactly one space.

The optional argument BUFFER will be ignored.  It is assumed to exist
when the function is called via `format-alist'."
  (interactive "r")
  (save-excursion
    (goto-char start)
    ;; match \r in order to deal with selective display
    (while (re-search-forward "[\n\r][ \t]*" end t)
      (unless (get-text-property (match-beginning 0) 'hard)
        (replace-match " ")))
    (max end (point))))

;; Adding newlines

(defadvice newline (after newline-shows-hard-ones activate)
  "When a hard newline is inserted, it will also be highlighted.
This happens only if `longlines-showing' is non-nil."
  (save-excursion
    (when (and longlines-showing
	       (search-backward "\n" nil t))
      (longlines-show-region (match-beginning 0) (match-end 0)))))

;; Killing

(defadvice kill-new (around kill-new-from-longlines-buffer activate)
  "Before killing, care is taken to pass all lines through 
`longlines-unwrap' if the source buffer is in `longlines-mode'."
  (when longlines-mode
    (with-temp-buffer
      (insert string)
      (longlines-unwrap (point-min) (point-max))
      (setq string (buffer-string))))
  ad-do-it)

;; Yanking

(defadvice yank (after yank-into-longlines-buffer activate)
  "After yanking, care is taken to pass all lines through 
`longlines-wrap' if the target buffer is in `longlines-mode'."
  (when longlines-mode
    (longlines-wrap (region-beginning) (region-end)
		    longlines-nowrap-when-yanking)
    (longlines-show-hard-newlines)))

(when (fboundp 'yank-clipboard-selection);; for XEmacs
  (defadvice yank-clipboard-selection (after yank-clipboard-selection-into-longlines-buffer activate)
    "After yanking, care is taken to pass all lines through 
`longlines-wrap' if the target buffer is in `longlines-mode'."
    (when longlines-mode
      (longlines-wrap (region-beginning) (region-end))
      (longlines-show-hard-newlines))))

;; Loading and saving

(add-to-list 'format-alist
             (list 'longlines           ;name
                   "Automatically wrap long lines." ;doc
                   nil                  ;regexp
                   'longlines-wrap      ;decode
                   'longlines-unwrap    ;encode
                   t                    ;modify
                   nil))                ;mode function

(provide 'longlines)

;;; longlines.el ends here
