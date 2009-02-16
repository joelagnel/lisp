;;; buff-menu+.el --- Extensions to `buff-menu.el'.
;;
;; Filename: buff-menu+.el
;; Description: Extensions to `buff-menu.el'
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Mon Sep 11 10:29:56 1995
;; Version: 21.0
;; Last-Updated: Mon Apr 10 16:26:55 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 1488
;; URL: http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el
;; Keywords: mouse, local, convenience
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-cmds', `frame-fns', `misc-cmds',
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `buff-menu.el'.
;;    New bindings & fonts & menu. Directional column sorting.
;;
;;  `Buffer-menu-mouse-3-menu' popup menu added.
;;  New prefix arg options for `buffer-menu'.
;;
;;
;;  Faces defined here:
;;
;;    `buffer-menu-headings', `buffer-menu-current-buffer',
;;    `buffer-menu-view-mark', `buffer-menu-delete-mark',
;;    `buffer-menu-save-mark', `buffer-menu-modified-mark',
;;    `buffer-menu-read-only-mark', `buffer-menu-buffer-name',
;;    `buffer-menu-mode', `buffer-menu-size', `buffer-menu-time',
;;    `buffer-menu-file-name'.
;;
;;
;;  Options (variables) defined here:
;;
;;    `buffer-menu-buffer-name', `buffer-menu-current-buffer',
;;    `buffer-menu-delete-mark', `buffer-menu-file-name',
;;    `buffer-menu-font-lock-keywords', `buffer-menu-headings',
;;    `buffer-menu-mode', `buffer-menu-modified-mark',
;;    `buffer-menu-read-only-mark', `buffer-menu-save-mark',
;;    `buffer-menu-size', `buffer-menu-time', `buffer-menu-view-mark'.
;;
;;
;;  Functions defined here:
;;
;;    `Buffer-menu-mouse-3-menu', `Buffer-menu-mouse-delete',
;;    `Buffer-menu-mouse-execute',
;;    `Buffer-menu-fontify-and-adjust-frame',
;;    `Buffer-menu-mouse-modified', `Buffer-menu-mouse-other-window',
;;    `Buffer-menu-mouse-save', `Buffer-menu-mouse-unmark'.
;;
;;
;;  ***** NOTE: The following user option (variable) defined in
;;              `buff-menu.el' has been REDEFINED HERE:
;;
;;  `Buffer-menu-sort-column' - Default value is 1.
;;                              Should always be numeric now.
;;
;;
;;  ***** NOTE: The following hook defined in `buff-menu.el'
;;              has been REDEFINED HERE:
;;
;;  `buffer-menu-mode-hook' - Fontifies buffer and fits its frame.
;;
;;
;;  ***** NOTE: The following functions defined in `buff-menu.el'
;;              have been REDEFINED HERE:
;;
;;  `buffer-menu' -
;;     1. Different help message.
;;     2. Prefix ARG =< 0 now means list (all) buffers alphabetically.
;;        (It used to mean the same as ARG > 0.)
;;        Prefix ARG >= 0 means list just file buffers.
;;     3. Use pop-to-buffer instead of switch-to-buffer.
;;  `Buffer-menu-beginning' - Protected with `boundp' for Emacs 20.
;;  `Buffer-menu-execute' - Deletes windows (frame) when kills buffer.
;;  `Buffer-menu-make-sort-button' -
;;     1. If same column as last sort, flip direction of sort.
;;     2. Column header face indicates sort direction.
;;     3. CRM is indicated by COLUMN = 1, not by nil COLUMN.
;;  `Buffer-menu-mode' -
;;     1. Doc string reflects new bindings.
;;     2. mouse-face on whole line, not just buffer name.
;;     3. Compatible with Emacs prior to Emacs 22 also.
;;  `Buffer-menu-select' - When Buffer Menu is `window-dedicated-p',
;;                         uses `pop-to-buffer' to display.
;;  `Buffer-menu-sort' -
;;     1. Allow negative COLUMN. Allow COLUMN = 1 or -1.
;;     2. When COLUMN = `Buffer-menu-sort-column', then flip that.
;;     3. Added message at end indicating the kind of sort.
;;  `list-buffers-noselect' - Change sort direction if same column.
;;                          - Add sort button for CRM (visited order).
;;                          - Bug fix: Temporarily set
;;                            `window-dedicated-p' to nil to allow
;;                            revert-buffer. (Emacs 21 only)
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `buff-menu.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "buff-menu" '(require 'buff-menu+))
;;
;;  Note: This file must be saved with encoding UTF-8 or equivalent,
;;  because it contains an em-dash character.
;;
;;  TO DO:
;;
;;  Make sort column buttons extend from one to the other (not just on the text).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/04/10 dadams
;;      list-buffers-noselect: Updated Emacs 22 version wrt latest CVS version.
;; 2006/01/07 dadams
;;      Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Added defgroup 'Buffer-Menu-Plus.  Added :link.
;; 2005/11/04 dadams
;;     Added: Buffer-menu-sort-button-map, Buffer-menu-sort-by-column, if not available in Emacs.
;; 2005/11/01 dadams
;;     Buffer-menu-make-sort-button: Updated to reflect latest CVS version:
;;       Added text property: column.  Use Buffer-menu-sort-button-map.
;;       Mention mouse-1 in :help.
;; 2005/07/08 dadams
;;     Buffer-menu-fontify-and-adjust-frame: Wrapped in save-*excursion's.
;; 2005/07/04 dadams
;;     Buffer-menu-fontify-and-adjust-frame: Fixed typo: boundp -> fboundp.
;; 2005/06/22 dadams
;;     Use defface for faces now.
;;     Renamed faces: *-face to *.
;;     No longer require def-face-const.el.
;; 2005/06/21 dadams
;;     list-buffers-noselect: Emacs 22 renamed Buffer-menu-buffer-face to Buffer-menu-buffer.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/12/05 dadams
;;     Buffer-menu-execute, Buffer-menu-mouse-execute: minor corrections.
;; 2004/11/30 dadams
;;     Added Time column (with sorting).
;;       Added buffer-menu-time-face.
;;       Buffer-menu-sort, Buffer-menu-make-sort-button, list-buffers-noselect,
;;     list-buffers-noselect: Major changes: Time column, updated to CVS version (new 2nd arg) etc.
;;     buffer-menu-font-lock-keywords: Rewrote for time etc.
;;     Sort CRM column also now.
;;     Buffer-menu-fontify-and-fit-frame renamed to Buffer-menu-fontify-and-adjust-frame.
;;       Added raise-frame (but Emacs bug, so raise doesn't work on Windows).
;;       turn-on-font-lock, instead of font-lock-fontify-buffer.
;;     Removed defvar ;;;###autoload's.
;; 2004/11/23 dadams
;;     buffer-menu-mode-hook: call font-lock-fontify-buffer.
;;     buffer-menu: Do not call font-lock-fontify-buffer.
;;     Added Buffer-menu-fontify-and-fit-frame. Hook fits frame too.
;;     Require fit-frame.el.
;;     Removed Buffer-menu-revert.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/11/19 dadams
;;     Protected tests of Buffer-menu-use-header-line with boundp for older versions.
;; 2004/11/13 dadams
;;     buffer-menu-font-lock-keywords: overwrite for *-mode-face, *-size-face, and *-file-name-face.
;; 2004/10/17 dadams
;;     Updated to include Daniel Pfeiffer's fix to buff-menu.el of bug I reported on losing
;;       marks when you sort columns:
;;         1) Added Buffer-menu-revert-function, 2) Added (and modified) Buffer-menu-beginning,
;;         3) Use Buffer-menu-beginning in Buffer-menu-execute and Buffer-menu-select,
;;         4) Buffer-menu-sort: incorporated Daniel's mark-saving code.
;;     Note: when the new version comes out (from CVS),  I will 1) update list-buffers-noselect
;;       to new version that uses 4-arg version of format-mode-line and 2) remove new definition
;;       of Buffer-menu-revert-function added here now.
;; 2004/10/16 dadams
;;     Added directional column sorting, with highlighting:
;;       Added: Buffer-menu-make-sort-button, Buffer-menu-sort.
;;       list-buffers-noselect: Add sort button for CRM. Sort directionally.
;;     Only require cl.el when compile.
;;     Buffer-menu-revert: Fontify for Emacs 21 also (needed after revert).
;; 2004/10/15 dadams
;;     Buffer-menu-mode: Don't skip first two lines if Buffer-menu-use-header-line.
;; 2004/10/13 dadams
;;     Updated for Emacs 21:
;;       buffer-menu-font-lock-keywords, Buffer-menu-mode, Buffer-menu-execute, Buffer-menu-select
;;       Added list-buffers-noselect for Emacs 21 (bug fix).
;;       require cl.el only when compile on Emacs 20.
;;     Added Buffer-menu-revert: Fontifies.
;; 2004/07/21 dadams
;;     Buffer-menu-mode: Don't set Buffer-menu-buffer-column unless < Emacs 20.
;; 2001/01/02 dadams
;;     Protect undefine-killer-commands via fboundp.
;; 1999/08/26 dadams
;;     1. Added: buffer-menu-*-face's, buffer-menu-font-lock-keywords.
;;     2. Add buffer-menu-font-lock-keywords to buffer-menu-mode-hook.
;; 1997/03/21 dadams
;;     Buffer-menu-execute, Buffer-menu-mouse-execute:
;;       Only use kill-buffer-and-its-windows if fboundp.
;; 1996/07/01 dadams
;;     buffer-menu: Prefix arg =< 0 sorts alphabetically now.
;; 1996/07/01 dadams
;;     Added redefinition of Buffer-menu-select.
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/01/25 dadams
;;     1. kill-buffer -> kill-buffer-and-its-windows.
;;     2. Buffer-menu-mode: Put mouse-face on whole buffer line.
;; 1996/01/12 dadams
;;     Redefined buffer-menu.
;; 1996/01/09 dadams
;;     kill-buffer -> kill-buffer-delete-frames
;; 1995/12/28 dadams
;;     Buffer-menu-mouse-3-menu: Corrected by adding temp local var.
;; 1995/12/14 dadams
;;     1. Highlight buffer line when mouse-3 menu displayed.
;;        Added Buffer-menu-overlay.
;;     2. mouse-3 menu is reduced to non-buffer-specifics when not on a buffer line.
;; 1995/12/13 dadams
;;     Added Buffer-menu-mouse-3-menu.  Use it instead of Buffer-menu-mouse-3-map.
;; 1995/12/13 dadams
;;     1) Put back Buffer-menu-select, in place of Buffer-menu-mouse-other-window.
;;     2) Added menu on mouse-3: Added: Buffer-menu-mouse-3-map,
;;        Buffer-menu-mouse-execute, Buffer-menu-mouse-modified,
;;        Buffer-menu-mouse-delete, Buffer-menu-mouse-save,
;;        Buffer-menu-mouse-unmark.
;; 1995/09/11 dadams
;;     Buffer-menu-mode: Added bindings list to doc string.
;; 1995/09/11 dadams
;;     Redefined Buffer-menu-execute: deletes frame w/ kill.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

 ;; Cannot do (require 'buff-menu), because `buff-menu.el' does no `provide'.
 ;; Don't want to do a (load-library "buff-menu") either, because it wouldn't
 ;; allow doing (eval-after-load "buff-menu" '(progn (require 'buff-menu+)))

(eval-when-compile (require 'cl)) ;; case, (plus, for Emacs 20: push, pop, dolist,
                                  ;;        and, for Emacs <20: cadr, when, unless)

(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'misc-cmds nil t) ;; (no error if not found): kill-buffer-and-its-windows
(require 'fit-frame nil t) ;; (no error if not found): fit-frame


;; To quiet the byte compiler:
(unless (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (defvar Buffer-menu-use-header-line)
  (defvar Buffer-menu-files-only)
  (defvar Buffer-menu-mode-width)
  (defvar Buffer-menu-buffer+size-width)
  (defvar header-line-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; REPLACES ORIGINAL in `buff-menu.el'.
;;
;; Protect Buffer-menu-files-only with boundp (for Emacs 20).
;;
(defun Buffer-menu-revert-function (ignore1 ignore2)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;; We can not use save-excursion here.  The buffer gets erased.
  (let ((opoint (point))
	(eobp (eobp))
	(ocol (current-column))
	(oline (progn (move-to-column 4)
		      (get-text-property (point) 'buffer)))
	(prop (point-min))
	;; do not make undo records for the reversion.
	(buffer-undo-list t))
    (list-buffers-noselect (and (boundp 'Buffer-menu-files-only) Buffer-menu-files-only))
    (if oline
	(while (setq prop (next-single-property-change prop 'buffer))
	  (when (eq (get-text-property prop 'buffer) oline)
	    (goto-char prop)
	    (move-to-column ocol)))
      (goto-char (if eobp (point-max) opoint)))))

(defun Buffer-menu-fontify-and-adjust-frame ()
  "Use for `buffer-menu-mode-hook'.  Fontify, fit and raise frame."
  (save-window-excursion
    (save-excursion
      (pop-to-buffer "*Buffer List*")
      (when (< emacs-major-version 21) (make-local-variable 'font-lock-defaults)) ; Automatic >= 21.
      (setq font-lock-defaults '(buffer-menu-font-lock-keywords t))
      (turn-on-font-lock)
      (when (and (fboundp 'fit-frame) (one-window-p t)) (fit-frame))
      (raise-frame))))

;; Fontify buffer, then fit and raise its frame.
(add-hook 'buffer-menu-mode-hook 'Buffer-menu-fontify-and-adjust-frame)



;; REPLACES ORIGINAL in `buff-menu.el'.
;; Treat Emacs 20 too.
;;
(defun Buffer-menu-beginning ()
  (goto-char (point-min))
  (unless (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
    (forward-line)))



;; REPLACES ORIGINAL in `buff-menu.el'.
;; Initial value is 1, not nil. It should always be numeric.
;; This is a defconst because `buff-menu.el' is preloaded.
;; Otherwise, it would be just (defvar Buffer-menu-sort-column 1)
;;
;; This is updated when you click a column heading.
(defconst Buffer-menu-sort-column
  (if (or (not (boundp 'Buffer-menu-sort-column)) (null Buffer-menu-sort-column))
      1
    Buffer-menu-sort-column)
  "*Sorted by (1) visit, (2) buffer, (3) size, (4) time, (5) mode, (6) file.")


;;; Undefine some bindings that would try to modify a buffer-menu buffer.
;;; Their key sequences will then appear to the user as available for
;;; local (Buffer Menu) definition.
(when (fboundp 'undefine-killer-commands)
  (undefine-killer-commands Buffer-menu-mode-map (current-global-map)))


;;; Faces used to fontify buffer.

(defgroup Buffer-Menu-Plus nil
  "Enhancements to buffer menu"
  :prefix "buffer-menu-" :group 'Buffer-menu :group 'convenience
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
buff-menu+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/buff-menu+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/BufferMenu#BufferMenuPlus")
  :link '(emacs-commentary-link :tag "Commentary" "buff-menu+")
  )

(defface buffer-menu-headings
  '((t (:foreground "Orange" :background "DarkGreen")))
  "*Face used for headings in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-headings 'buffer-menu-headings)

(defface buffer-menu-current-buffer
  '((t (:foreground "Red" :background "Aquamarine")))
  "*Face used for current buffer mark in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-current-buffer 'buffer-menu-current-buffer)

(defface buffer-menu-view-mark
  '((t (:foreground "Red" :background "Aquamarine")))
  "*Face used for buffers to view mark (>) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-view-mark 'buffer-menu-view-mark)

(defface buffer-menu-delete-mark
  '((t (:foreground "Aquamarine" :background "Red")))
  "*Face used for buffers to delete mark (D) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-delete-mark 'buffer-menu-delete-mark)

(defface buffer-menu-save-mark
  '((t (:foreground "Orange" :background "Blue")))
  "*Face used for buffers to save mark (S) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-save-mark 'buffer-menu-save-mark)

(defface buffer-menu-modified-mark
  '((t (:foreground "DarkOrange")))
  "*Face used for modified buffers mark (*) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-modified-mark 'buffer-menu-modified-mark)

(defface buffer-menu-read-only-mark
  '((t (:foreground "Yellow")))
  "*Face used for read-only buffers mark (%) in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-read-only-mark 'buffer-menu-read-only-mark)

(defface buffer-menu-buffer-name
  '((t (:foreground "Blue")))
  "*Face used for buffer names in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-buffer-name 'buffer-menu-buffer-name)
;; Redefine standard face `Buffer-menu-buffer' as `buffer-menu-buffer-name'.
(put 'Buffer-menu-buffer 'face-alias 'buffer-menu-buffer-name)

(defface buffer-menu-mode
  '((t (:foreground "DarkGreen")))
  "*Face used for buffer modes in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-mode 'buffer-menu-mode)

(defface buffer-menu-size
  '((t (:foreground "DarkRed")))
  "*Face used for buffer sizes in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-size 'buffer-menu-size)

(defface buffer-menu-time
  '((t (:foreground "DarkGoldenrod4")))
  "*Face used for buffer time in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-time 'buffer-menu-time)

(defface buffer-menu-file-name
  '((t (:foreground "DarkMagenta")))
  "*Face used for file names in *Buffer List* buffer."
  :group 'Buffer-Menu-Plus
  :group 'font-lock-highlighting-faces)
(defvar buffer-menu-file-name 'buffer-menu-file-name)



(defvar buffer-menu-font-lock-keywords
  (list
   ;; Headings -  Should this test instead be
   ;; (or (not (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)?
   ;; I don't think so; I think that CRM was introduced at the same time as
   ;; `Buffer-menu-use-header-line.'
   (if (not (boundp 'Buffer-menu-use-header-line))
       (list "^\\( M.*\\)" 1 'buffer-menu-headings)
     (list "^\\(CRM.*\\)" 1 'buffer-menu-headings))
   (list "^....\\(.+\\)[ \t\n][0-9]" 1 'buffer-menu-buffer-name) ; Buffer name
   (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
       (list "^.*[ \t][0-9]+[ \t]+\\(.* \\(AM\\|PM\\)\\)?\\([^/\n]+\\)"
             (list 1 'buffer-menu-time t t) ; Time
             (list 3 'buffer-menu-mode t t)) ; Mode
     (list "^.*[ \t][0-9]+[ \t]+\\([^/\n]+\\)" 1 'buffer-menu-mode t t))
   (list "^.*[ \t]\\([0-9]+\\)[ \t]+[^/\n]+" 1 'buffer-menu-size t t) ; Size
   (list "\\(/.*\\)$" 1 'buffer-menu-file-name t t) ; File name
   (list "^\\([.]\\)" 1 'buffer-menu-current-buffer t t) ; Current buffer mark (.)
   (list "^\\(>\\)" 1 'buffer-menu-view-mark t t) ; To view mark (>)
   (list "^\\(D\\)" 1 'buffer-menu-delete-mark t t) ; Deletion flag (D)
   ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
   (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
       (list "^..\\(S\\)" 1 'buffer-menu-save-mark t t) ; Save flag (S)
     (list "^.\\(S\\)" 1 'buffer-menu-save-mark t t))
   ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
   (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
       (list "^..\\([*]\\)" 1 'buffer-menu-modified-mark t t) ; Buffer-modified-p (*)
     (list "^.\\([*]\\)" 1 'buffer-menu-modified-mark t t))
   ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
   (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
       (list "^.\\(%\\)" 1 'buffer-menu-read-only-mark t t) ; Read-only-p (%)
     (list "^..\\(%\\)" 1 'buffer-menu-read-only-mark t t))
   ) "*Expressions to highlight in Buffer Menu mode.")

;; Available in Emacs 22.
(unless (boundp 'Buffer-menu-sort-button-map)
  (defvar Buffer-menu-sort-button-map
    (let ((map (make-sparse-keymap)))
      ;; This keymap handles both nil and non-nil values for
      ;; Buffer-menu-use-header-line.
      (define-key map [header-line mouse-1] 'Buffer-menu-sort-by-column)
      (define-key map [header-line mouse-2] 'Buffer-menu-sort-by-column)
      (define-key map [mouse-2] 'Buffer-menu-sort-by-column)
      (define-key map [follow-link] 'mouse-face)
      (define-key map "\C-m" 'Buffer-menu-sort-by-column)
      map)
    "Local keymap for Buffer menu sort buttons."))

(unless (fboundp 'Buffer-menu-sort-by-column)
  (defun Buffer-menu-sort-by-column (&optional e)
    "Sort the buffer menu by the column clicked on."
    (interactive (list last-input-event))
    (if e (mouse-select-window e))
    (let* ((pos (event-start e))
           (obj (posn-object pos))
           (col (if obj
                    (get-text-property (cdr obj) 'column (car obj))
                  (get-text-property (posn-point pos) 'column))))
      (Buffer-menu-sort col))))

;; REPLACES ORIGINAL in `buff-menu.el':
;;   1. Different help message.
;;   2. Prefix ARG =< 0 now means list all buffers alphabetically.
;;      (It used to mean the same as ARG > 0.)
;;      Prefix ARG >= 0 means list just file buffers.
;;   3. Use pop-to-buffer instead of switch-to-buffer.
;;;###autoload
(defun buffer-menu (&optional arg)
  "Make a menu of buffers so you can save, delete or select them.
By default (no or null prefix arg), the buffers are listed in order of
last access.  With a non-nil prefix ARG:
  ARG >= 0   => Only buffers visiting files are listed.
  ARG =< 0   => The buffers are listed alphabetically.
 (ARG =  0   => Only buffers visiting files, listed alphabetically.)

You can click a column heading to sort by that column.  Clicking again
reverses the sort direction.  The current sort column is indicated by
an underlined or overlined column heading.

Type `?' in buffer \"*Buffer List*\" to get help on available commands.
Type `q' there to quit the buffer menu.

The first column has a `.' for the buffer you came from.
The R column has a `%' if the buffer is read-only.
The M column has a `*' if the buffer is modified,
  or `S' if you have marked it for saving.
After this come the buffer name, its size in characters,
its major mode, and the visited file name (if any)."
  (interactive "P")
  (let ((num-arg (prefix-numeric-value arg)))
    (if (and arg (< num-arg 0))
        (list-buffers)
      (list-buffers arg))
    (let ((newpoint (save-excursion (set-buffer "*Buffer List*") (point))))
      (pop-to-buffer "*Buffer List*")
      (when (and arg (not (> num-arg 0))) ; Sort lines after header.
        (let ((buffer-read-only nil))
          (goto-char (point-min)) (forward-line 2) (forward-char 4) ; Header.
          (sort-columns nil (point)
                        (save-excursion (goto-char (point-max))
                                        (when (bolp) (backward-char 1))
                                        (point)))))
      (goto-char newpoint)))
  (message "Help: ?;   Menu: mouse-3;   Show: v;   Mark: u,m,s,d;   \
Save/Delete: x;   Misc: g,~,%%,t"))


;; REPLACES ORIGINAL in `buff-menu.el':
;; 1. Doc string reflects new bindings.
;; 2. mouse-face on whole line, not just buffer name.
;; 3. Compatible with Emacs prior to Emacs 22 also.
;;
;;;###autoload
(defun Buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
In Buffer menu mode, chars do not insert themselves, but are commands.
\\<Buffer-menu-mode-map>
\(\"Current line\" here is the line of the text cursor or the mouse.)

Also, pressing `mouse-3' on a buffer name in this mode provides a
popup menu that duplicates most of the functions below.


Display buffers:
---------------
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select], \\[Buffer-menu-this-window] -- Select current line's \
buffer.
\\[Buffer-menu-mark]\t-- Mark current line's buffer `>' to be displayed (via \
`\\[Buffer-menu-select]').
\\[Buffer-menu-select]\t-- Show buffers marked `>'.  Select current line's \
buffer.
\\[Buffer-menu-1-window]\t-- Select current line's buffer (only) in a \
full-frame window.
\\[Buffer-menu-2-window]\t-- Select current line's buffer in one window.
\t   Display previous buffer in a second window.
\\[Buffer-menu-switch-other-window]\t-- Display current line's buffer in \
another window.  No select.
\\[Buffer-menu-view] -- select current line's buffer, but in view-mode.
\\[Buffer-menu-view-other-window] -- select that buffer in
  another window, in view-mode.
\\[Buffer-menu-toggle-files-only] -- toggle whether the menu displays only file buffers.

Mark/Unmark buffers to be Saved/Deleted:
---------------------------------------
\\[Buffer-menu-save]\t-- Mark current line's buffer `S' to be saved.    \
Cursor down.
\\[Buffer-menu-delete]\t-- Mark current line's buffer `D' to be deleted.  \
Cursor down.
\\[Buffer-menu-delete-backwards]\t-- Mark current line's buffer `D' to be \
deleted.  Cursor up.
\\[Buffer-menu-unmark]\t-- Unmark current line.  Cursor down. (Prefix arg: \
Cursor up.)
\\[Buffer-menu-backup-unmark]\t-- Cursor up, then unmark line.

Save/Delete buffers:
-------------------
\\[Buffer-menu-execute]\t-- Save / Delete marked buffers (marks `S', `D').

Miscellaneous:
-------------
\\[Buffer-menu-revert] -- Update the list of buffers.
\\[Buffer-menu-not-modified]\t-- Clear modified-flag on current line's buffer.
\\[Buffer-menu-toggle-read-only]\t-- Toggle read-only status of current \
line's buffer.
\\[Buffer-menu-visit-tags-table]\t-- `visit-tags-table' using current line's \
buffer.


Bindings in Buffer Menu mode:
----------------------------

\\{Buffer-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map Buffer-menu-mode-map)
  (setq major-mode 'Buffer-menu-mode)
  (setq mode-name "Buffer Menu")
  (save-excursion
    (goto-char (point-min))
    (when (< emacs-major-version 20) ; Hardcoded to 4, starting in Emacs 20
      (search-forward "Buffer")
      (backward-word 1)
      (setq Buffer-menu-buffer-column (current-column)))
    (when (or (not (boundp 'Buffer-menu-use-header-line)) (not Buffer-menu-use-header-line))
      (forward-line 2)) ; First two lines are title, unless use header line.
    (while (not (eobp))
      (put-text-property (point)
                         (save-excursion (end-of-line) (point))
                         'mouse-face 'highlight)
      (forward-line 1)))
  (set (make-local-variable 'revert-buffer-function)
       'Buffer-menu-revert-function)
  ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
  (when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
    (set (make-local-variable 'buffer-stale-function)
         #'(lambda (&optional noconfirm) 'fast)))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
  (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
      (run-mode-hooks 'buffer-menu-mode-hook)
    (run-hooks 'buffer-menu-mode-hook)))



;; REPLACES ORIGINAL in `buff-menu.el':
;; 1. Deletes frame when kills buffer.
;; 2. Compatible with Emacs prior to Emacs 22 also.
;;
;;;###autoload
(defun Buffer-menu-execute ()
  "Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and \
`\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]')."
  (interactive)
  (save-excursion
    (Buffer-menu-beginning)
    ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
    (while (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
               (re-search-forward "^..S" nil t)
             (re-search-forward "^.S" nil t))
      (let ((modp nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp (buffer-modified-p)))
        (let ((buffer-read-only nil))
          (delete-char -1)
          (insert (if modp ?* ? ))))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer (current-buffer))
          (buffer-read-only nil))
      (while (re-search-forward "^D" nil t)
        (forward-char -1)
        (let ((buf (Buffer-menu-buffer nil)))
          (or (eq buf nil) (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf))))
          (if (and buf (buffer-name buf))
              (progn (delete-char 1) (insert ? ))
            (delete-region (point) (progn (forward-line 1) (point)))
            (unless (bobp)
              (forward-char -1))))))))



;; REPLACES ORIGINAL in `buff-menu.el':
;; When Buffer Menu is `window-dedicated-p', uses `pop-to-buffer' to display.
;;
;;;###autoload
(defun Buffer-menu-select ()
  "Select this line's buffer; also display buffers marked with `>'.
You can mark buffers with command `\\<Buffer-menu-mode-map>\\[Buffer-menu-mark]'.
If the window is `window-dedicated-p', then another window is used;
else, all windows previously in the frame are replaced by this one."
  (interactive)
  (let ((buff (Buffer-menu-buffer t))
        (menu (current-buffer))
        (others ())
        tem)
    (Buffer-menu-beginning)
    (while (re-search-forward "^>" nil t)
      (setq tem (Buffer-menu-buffer t))
      (let ((buffer-read-only nil)) (delete-char -1) (insert ?\ ))
      (or (eq tem buff) (memq tem others) (setq others (cons tem others))))
    (setq others (nreverse others))
    (cond ((window-dedicated-p (selected-window)) ; Can't split dedicated win.
           (pop-to-buffer buff)
           (unless (eq menu buff) (bury-buffer menu))
           (while others
             (pop-to-buffer (car others))
             (pop others)))
          (t
           (setq tem (/ (1- (frame-height)) (1+ (length others))))
           (delete-other-windows)
           (switch-to-buffer buff)
           (unless (eq menu buff) (bury-buffer menu))
           (if (equal (length others) 0)
               (progn
;;;              ;; Restore previous window configuration before displaying
;;;              ;; selected buffers.
;;;              (if Buffer-menu-window-config
;;;                  (progn (set-window-configuration
;;;                            Buffer-menu-window-config)
;;;                         (setq Buffer-menu-window-config nil)))
                 (switch-to-buffer buff))
             (while others
               (split-window nil tem)
               (other-window 1)
               (switch-to-buffer (car others))
               (setq others (cdr others)))
             (other-window 1))))))      ;back to the beginning!      ; Back to the beginning.



;; REPLACES ORIGINAL in `buff-menu.el'.
;; Allow negative COLUMN.  Allow COLUMN = 1 or -1.
;; When COLUMN = `Buffer-menu-sort-column', then flip `Buffer-menu-sort-column'.
;; Message at end.
;;
;;;###autoload
(defun Buffer-menu-sort (column)
  "Sort the buffer menu by COLUMN.
Consecutive executions of the same COLUMN reverse the sort order."
  (interactive "P")
  (when column
    (setq column (prefix-numeric-value column))
    (when (= column 0) (setq column 1))
    (when (> column 6) (setq column 6))
    (when (< column -6) (setq column -6)))
  (if (equal Buffer-menu-sort-column column)
      (setq Buffer-menu-sort-column (- column))
    (setq Buffer-menu-sort-column column))
  (let (buffer-read-only l buf m1 m2)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
	(when (buffer-live-p (setq buf (get-text-property (+ (point) 4) 'buffer)))
	  (setq m1 (char-after)
		m1 (if (memq m1 '(?> ?D)) m1)
		m2 (char-after (+ (point) 2))
		m2 (if (eq m2 ?S) m2))
	  (if (or m1 m2)
	      (push (list buf m1 m2) l)))
	(forward-line)))
    (Buffer-menu-revert)
    (setq buffer-read-only)
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
	(when (setq buf (assq (get-text-property (+ (point) 4) 'buffer) l))
	  (setq m1 (cadr buf)
		m2 (cadr (cdr buf)))
	  (when m1
	    (delete-char 1)
	    (insert m1)
	    (backward-char 1))
	  (when m2
	    (forward-char 2)
	    (delete-char 1)
	    (insert m2)))
	(forward-line))))
  (message "Buffers are now sorted %s%s."
           (case (abs column)
             (1 "by time of first visit")
             (2 "by buffer name")
             (3 "by size")
             (4 "by time of last display")
             (5 "by major-mode name")
             (otherwise "by associated file (including path)"))
           (if (natnump Buffer-menu-sort-column) ", ascending" ", descending")))



;; REPLACES ORIGINAL in `buff-menu.el'.
;; If same column as last sort, then flip direction of sort.
;; CRM is indicated by COLUMN = 1, not by nil COLUMN.
;; Apply different face to sort column heading, depending on direction.
;;
;;;###autoload
(defun Buffer-menu-make-sort-button (name button-column)
  (let ((the-sort-column-p nil))
    (when (equal button-column (abs Buffer-menu-sort-column))
      (setq the-sort-column-p t)
      (setq button-column (- button-column)))
    (propertize name
                'column button-column
                'help-echo
                (case (abs button-column)
                  ((1 2) (if Buffer-menu-use-header-line
                             "mouse-1, mouse-2: sort by visited order"
                           "mouse-2, RET: sort by visited order"))
                  (4 "mouse-1, mouse-2: sort by time of last display/access")
                  (t (if Buffer-menu-use-header-line
                         (concat "mouse-1, mouse-2: sort by " (downcase name))
                       (concat "mouse-2, RET: sort by " (downcase name)))))
                'mouse-face 'highlight
                (when the-sort-column-p 'face)
                (when the-sort-column-p
                  (if (natnump Buffer-menu-sort-column)
                      '(:underline t)
                    '(:overline t)))
                'keymap Buffer-menu-sort-button-map)))


;; REPLACES ORIGINAL in `buff-menu.el'
;; Add sort buttons for CRM and Time also.
;; The test for column 1 (CRM) is =1, not null.
;; Sort direction depends on sign of `Buffer-menu-sort-column'.
;; Temporarily sets `window-dedicated-p' to nil when it does the
;;   `set-window-buffer'.  Otherwise, *Buffer List* cannot be dedicated.
;;
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
;;;###autoload
(when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (defun list-buffers-noselect (&optional files-only buffer-list)
    "Create and return a buffer with a list of names of existing buffers.
The buffer is named `*Buffer List*'.
Note that buffers with names starting with spaces are omitted.
Non-null optional arg FILES-ONLY means mention only file buffers.

If BUFFER-LIST is non-nil, it should be a list of buffers;
it means list those buffers and no others.

For more information, see the function `buffer-menu'."
    (let* ((old-buffer (current-buffer))
           (standard-output standard-output)
           (mode-end (make-string (- Buffer-menu-mode-width 2) ? ))
           (header (concat (Buffer-menu-make-sort-button "CRM" 1) " "
                           (Buffer-menu-buffer+size
                            (Buffer-menu-make-sort-button "Buffer" 2)
                            (Buffer-menu-make-sort-button "Size" 3))
                           "  "
                           (Buffer-menu-make-sort-button "Time" 4) "             "
                           (Buffer-menu-make-sort-button "Mode" 5) mode-end
                           (Buffer-menu-make-sort-button "File" 6) "\n"))
           list desired-point name buffer-time mode file)
      (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
        (let ((pos 0))
          ;; Turn whitespace chars in the header into stretch specs so
          ;; they work regardless of the header-line face.
          (while (string-match "[ \t\n]+" header pos)
            (setq pos (match-end 0))
            (put-text-property (match-beginning 0) pos 'display
                               ;; Assume fixed-size chars in the buffer.
                               (list 'space :align-to pos)
                               header)))
        ;; REMOVED:
        ;; Try to better align the one-char headers.
        ;; (put-text-property 0 3 'face 'fixed-pitch header)

        ;; Add a "dummy" leading space to align the beginning of the header
        ;; line with the beginning of the text (rather than with the left
        ;; scrollbar or the left fringe). -Stef
        (setq header (concat (propertize " " 'display '(space :align-to 0))
                             header)))
      (with-current-buffer (get-buffer-create "*Buffer List*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq standard-output (current-buffer))
        (unless (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)

          ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version)
          ;; after 22.x release
          ;;
          ;; The EM dash character here means that we need to save this file as UTF-8.
          (let ((underline
                 (if (and (or (string-match "22." emacs-version)
                              (string-match "21.3.50" emacs-version))
                          (char-displayable-p ?â€”))
                     ?â€”                 ; EM dash
                   ?-)))                ; regular dash
            (insert header
                    (apply 'string
                           (mapcar (lambda (c)
                                     (if (memq c '(?\n ?\ )) c underline))
                                   header)))))
;;;;           (insert header (propertize "---" 'face 'fixed-pitch) " ")
;;;;           (insert (Buffer-menu-buffer+size "------" "----"))
;;;;           (insert "  ----" mode-end "----\n")
;;;;           (put-text-property 1 (point) 'intangible t))
        (if buffer-list
            (setq list buffer-list)
          ;; Collect info for every buffer we're interested in.
          (dolist (buffer (or buffer-list
                              (buffer-list
                               (when Buffer-menu-use-frame-buffer-list
                                 (selected-frame)))))
            (with-current-buffer buffer
              (let ((name (buffer-name))
                    (file buffer-file-name))
                (unless (and (not buffer-list)
                             (or
                              ;; Don't mention internal buffers.
                              (and (string= (substring name 0 1) " ") (null file))
                              ;; Maybe don't mention buffers without files.
                              (and files-only (not file))
                              (string= name "*Buffer List*")))
                  ;; Otherwise output info.
                  (let ( ;; Need to record two values for time: numerical time value, for
                        ;; sorting, and string time value, for display.
                        (buffer-time (cons (or (float-time buffer-display-time) 0)
                                           (if buffer-display-time
                                               (format-time-string
                                                "%_3a %_2k:%02M:%02S %_2p"
                                                buffer-display-time)
                                             "               ")))
                        (mode (concat
                               ;; These calls to format-mode-line take a 4th
                               ;; arg, `buffer', in 22.x
                               (if (string-match "22." emacs-version)
                                   (format-mode-line mode-name nil nil buffer)
                                 (format-mode-line mode-name))
                               (and mode-line-process
                                    (if (string-match "22." emacs-version)
                                        (format-mode-line mode-line-process
                                                          nil nil buffer)
                                      (format-mode-line mode-line-process)))))
                        (bits (string
                               (if (eq buffer old-buffer) ?. ?\ )
                               ;; Handle readonly status.  The output buffer
                               ;; is special cased to appear readonly; it is
                               ;; actually made so at a later date.
                               (if (or (eq buffer standard-output)
                                       buffer-read-only)
                                   ?% ?\ )
                               ;; Identify modified buffers.
                               (if (buffer-modified-p) ?* ?\ )
                               ;; Space separator.
                               ?\ )))
                    (unless file
                      ;; No visited file.  Check local value of
                      ;; list-buffers-directory.
                      (when (and (boundp 'list-buffers-directory)
                                 list-buffers-directory)
                        (setq file list-buffers-directory)))
                    (push (list buffer bits name (buffer-size) buffer-time mode file)
                          list))))))
          ;; Preserve the original buffer-list ordering, just in case.
          (setq list (nreverse list)))
        ;; Place the buffers's info in the output buffer, sorted if necessary.
        (dolist (buffer
                  (let* ((descending-p (natnump Buffer-menu-sort-column))
                         (Buffer-menu-sort-column (abs Buffer-menu-sort-column)))
                    (sort list
                          (cond ((eq Buffer-menu-sort-column 3) ; Size
                                 (if descending-p
                                     (lambda (a b) (< (nth 3 a) (nth 3 b)))
                                   (lambda (a b) (< (nth 3 b) (nth 3 a)))))
                                ((eq Buffer-menu-sort-column 4) ; Time (value)
                                 (if descending-p
                                     (lambda (a b) (< (car (nth 4 a)) (car (nth 4 b))))
                                   (lambda (a b) (< (car (nth 4 b)) (car (nth 4 a))))))
                                (t
                                 (if descending-p
                                     (lambda (a b)
                                       (string< (nth Buffer-menu-sort-column a)
                                                (nth Buffer-menu-sort-column b)))
                                   (lambda (a b)
                                     (string< (nth Buffer-menu-sort-column b)
                                              (nth Buffer-menu-sort-column a)))))))))
          (if (eq (car buffer) old-buffer)
              (setq desired-point (point)))
          (insert (cadr buffer)
                  ;; Put the buffer name into a text property
                  ;; so we don't have to extract it from the text.
                  ;; This way we avoid problems with unusual buffer names.
                  (Buffer-menu-buffer+size
                   (nth 2 buffer)
                   (int-to-string (nth 3 buffer))
                   `(buffer-name ,(nth 2 buffer)
                     buffer ,(car buffer)
                     ,(if (or (string-match "22." emacs-version)
                              (string-match "21.3.50" emacs-version))
                          'font-lock-face
                          'face) ,(if (facep 'Buffer-menu-buffer-face)
                                      'Buffer-menu-buffer-face ; < Emacs 22
                                      'Buffer-menu-buffer) ; Emacs 22
                     mouse-face highlight
                     help-echo "mouse-2: select this buffer"))
                  "  "
                  (cdr (nth 4 buffer)) "  " ; Time
                  (if (> (length (nth 5 buffer)) Buffer-menu-mode-width) ; Mode
                      (substring (nth 5 buffer) 0 Buffer-menu-mode-width)
                    (nth 5 buffer)))
          (when (nth 6 buffer)
            (indent-to (+ Buffer-menu-buffer-column Buffer-menu-buffer+size-width
                          Buffer-menu-mode-width 17 4) 1)
            (princ (abbreviate-file-name (nth 6 buffer))))
          (princ "\n"))
        (Buffer-menu-mode)
        (when (and (boundp 'Buffer-menu-use-header-line) Buffer-menu-use-header-line)
          (setq header-line-format header))
        ;; DESIRED-POINT doesn't have to be set; it is not when the
        ;; current buffer is not displayed for some reason.
        (and desired-point
             (goto-char desired-point))
        (setq Buffer-menu-files-only files-only)
        (set-buffer-modified-p nil)
        (current-buffer)))))


(define-key Buffer-menu-mode-map [down-mouse-3] 'Buffer-menu-mouse-3-menu)
(define-key Buffer-menu-mode-map [mouse-3] 'ignore)

;; Another way, but it shows the menu even if not on a buffer line,
;; and it doesn't show it if on the line but not on the buffer name itself.
;;(defvar Buffer-menu-mouse-3-map (make-sparse-keymap "Buffers"))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-execute]
;;  '("Execute: Save/Delete Marked Buffers" . Buffer-menu-mouse-execute))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-modified]
;;  '("Mark as Modified/Unmodified (*)" . Buffer-menu-mouse-modified))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-delete]
;;  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-save]
;;  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-unmark]
;;  '("Unmark Buffer" . Buffer-menu-mouse-unmark))
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-select]
;;  '("Select Buffer" . Buffer-menu-mouse-select))

;; Used to highlight buffer name's line during popup of Mouse-3 menu.
(defvar Buffer-menu-overlay nil)

;;;###autoload
(defun Buffer-menu-mouse-3-menu (event)
  "Pop up menu for Mouse-3 for buffer listed in buffer menu."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
         bol eol temp
         (buffer-name
          (save-excursion
            (set-buffer (window-buffer (posn-window mouse-pos)))
            (save-excursion
              (goto-char (posn-point mouse-pos))
              (save-excursion
                (setq bol (progn (beginning-of-line) (point)))
                (setq eol (progn (end-of-line) (point))))
              (if Buffer-menu-overlay   ; Don't recreate if exists.
                  (move-overlay Buffer-menu-overlay bol eol (current-buffer))
                (setq Buffer-menu-overlay (make-overlay bol eol))
                (overlay-put Buffer-menu-overlay 'face 'region))
              (setq temp (and (not (eobp)) (Buffer-menu-buffer nil)))
              ;; Nil if mouse is not on a buffer name.
              (and temp (buffer-name temp)))))) ; temp no longer used.
    (sit-for 0)
    (let ((selection
           (x-popup-menu
            event
            (list
             "Menu"
             (if buffer-name
                 (list
                  buffer-name
                  '("Select Buffer" . Buffer-menu-mouse-select)
                  '("Unmark Buffer" . Buffer-menu-mouse-unmark)
                  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save)
                  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete)
                  '("Mark as Modified/Unmodified (*)" .
                    Buffer-menu-mouse-modified)
                  '("--")               ; Separator: next not buffer-specific.
                  '("Execute: Save/Delete Marked Buffers" .
                    Buffer-menu-mouse-execute))
               (list "" '("Execute: Save/Delete Marked Buffers" .
                          Buffer-menu-mouse-execute)))))))
      (when Buffer-menu-overlay (delete-overlay Buffer-menu-overlay))
      (and selection (call-interactively selection)))))

;; Don't need this if use dedicated frame for buffer menu.
;;;###autoload
(defun Buffer-menu-mouse-other-window (event)
  "Select, in another window, the buffer on whose line you click."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun Buffer-menu-mouse-unmark (event)
  "Cancel all requested operations on buffer."
  (interactive "e")
  (let (buffer)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq buffer (Buffer-menu-buffer t))))
    (select-window (posn-window (event-end event)))
    (goto-char (posn-point (event-end event)))
    (beginning-of-line)
    (if (looking-at " [-M]")            ;header lines
        (ding)
      (let* ((mod (buffer-modified-p buffer))
             (readonly (save-excursion (set-buffer buffer) buffer-read-only))
             (buffer-read-only nil))
        (delete-char 3)
        (insert (if readonly (if mod " *%" "  %") (if mod " * " "   ")))))
    (beginning-of-line)))

;;;###autoload
(defun Buffer-menu-mouse-save (event)
  "Mark buffer to be saved.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
        (insert ?S)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-delete (event)
  "Mark buffer to be deleted.
Actual deletion is done via `\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (if (looking-at " [-M]")              ;header lines
      (ding)
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)))
  (beginning-of-line))

;;;###autoload
(defun Buffer-menu-mouse-modified (event)
  "Mark buffer as unmodified (no changes to save) if modified, and vice versa."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (beginning-of-line)
  (forward-char 1)
  (let ((buffer-read-only nil)
        modified-p)
    (save-excursion
      (set-buffer (Buffer-menu-buffer t))
      (set-buffer-modified-p (not (buffer-modified-p))))
    (cond ((= ?\* (char-after (point)))
           (delete-char 1)
           (insert ?\ ))
          (t
           (delete-char 1)
           (insert ?\*))))
  (beginning-of-line))


;;;###autoload
(defun Buffer-menu-mouse-execute (event)
  "Save and/or delete buffers marked `S' or `D', respectively.
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\
\\[Buffer-menu-save]' and `\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and \
`\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]')."
  (interactive "e")
  (select-window (posn-window (event-end event)))
  (save-excursion
    (Buffer-menu-beginning)
    ;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
    (while (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
               (re-search-forward "^..S" nil t)
             (re-search-forward "^.S" nil t))
      (let ((modp nil))
        (save-excursion
          (set-buffer (Buffer-menu-buffer t))
          (save-buffer)
          (setq modp (buffer-modified-p)))
        (let ((buffer-read-only nil))
          (delete-char -1)
          (insert (if modp ?* ? ))))))
  (save-excursion
    (Buffer-menu-beginning)
    (let ((buff-menu-buffer (current-buffer))
          (buffer-read-only nil))
      (while (re-search-forward "^D" nil t)
        (forward-char -1)
        (let ((buf (Buffer-menu-buffer nil)))
          (or (eq buf nil) (eq buf buff-menu-buffer)
              (save-excursion (if (fboundp 'kill-buffer-and-its-windows)
                                  (kill-buffer-and-its-windows buf)
                                (kill-buffer buf))))
          (if (and buf (buffer-name buf))
              (progn (delete-char 1) (insert ? ))
            (delete-region (point) (progn (forward-line 1) (point)))
            (unless (bobp) (forward-char -1))))))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'buff-menu+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buff-menu+.el ends here
