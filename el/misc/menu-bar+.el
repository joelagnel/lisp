;;; menu-bar+.el --- Extensions to `menu-bar.el'.
;;
;; Filename: menu-bar+.el
;; Description: Extensions to `menu-bar.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Thu Aug 17 10:05:46 1995
;; Version: 21.1
;; Last-Updated: Fri May 19 21:09:32 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 2780
;; URL: http://www.emacswiki.org/cgi-bin/wiki/menu-bar+.el
;; Keywords: internal, local, convenience
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `faces', `faces+', `fit-frame',
;;   `frame-cmds', `frame-fns', `help+', `highlight', `info',
;;   `info+', `menu-bar', `misc-cmds', `misc-fns', `replace+',
;;   `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32browser-dlgopen'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `menu-bar.el'.  Redefines the default menu bar.
;;
;;  Main differences:
;;
;;    1. Menus "Search", "Frames" and "Do Re Mi" were added.
;;    2. Menus "File", "Edit", & "Help" were changed.
;;    3. Menu order was changed.
;;    3. Buffer-local menus are separated from global menus via "||".
;;
;;  Functions defined here:
;;
;;    `describe-menubar', `fill-paragraph-ala-mode',
;;    `nonincremental-repeat-word-search-backward',
;;    `nonincremental-repeat-word-search-forward'
;;
;;  Macros defined here:
;;
;;    `menu-item-any-version', `menu-bar-make-toggle-any-version'.
;;
;;  Menu variables defined here:
;;
;;    `menu-bar-apropos-menu', `menu-bar-describe-menu',
;;    `menu-bar-divider-menu', `menu-bar-edit-fill-menu',
;;    `menu-bar-edit-region-menu', `menu-bar-edit-sort-menu',
;;    `menu-bar-emacs-lisp-manual-menu', `menu-bar-emacs-manual-menu',
;;    `menu-bar-frames-menu', `menu-bar-print-menu',
;;    `menu-bar-search-replace-menu', `menu-bar-search-tags-menu',
;;    `menu-bar-whereami-menu'.
;;
;;
;;  ***** NOTE: The following functions defined in `menu-bar.el' have
;;              been REDEFINED HERE:
;;
;;  `kill-this-buffer' -        Deletes buffer's windows as well, if
;;                              `sub-kill-buffer-and-its-windows'.
;;  `menu-bar-select-buffer' -  1. Uses -other-frame.
;;                              2. defun -> defsubst.
;;
;;  `menu-bar-options-save' - Added options are saved (>= Emacs 21).
;;
;;  `menu-bar-select-frame' - Use Emacs 22 version for Emacs 20.
;;
;;
;;  ***** NOTE: The following variables defined in `menu-bar.el' have
;;              been REDEFINED HERE:
;;
;;  `menu-bar-edit-menu', `menu-bar-file(s)-menu',
;;  `menu-bar-manuals-menu', `menu-bar-search-menu'.
;;
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `menu-bar.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "menu-bar" '(require 'menu-bar+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/05/19 dadams
;;     menu-bar-options-save: Updated to latest Emacs 22 definition. Added Emacs 21 definition.
;; 2005/11/08 dadams
;;     Added to menu-bar-edit-menu: undo, cut, copy, paste, select paste, clear,
;;       separator-edit-delete-lines.
;;     Added to menu-bar-search-tags-menu: set-tags-name, apropos-tags, separator-tags-misc,
;;       separator-tags-regexp, next-tag-other-frame, 
;;     Added: yank-menu, menu-bar-next-tag-other-frame, menu-bar-select-frame.
;; 2005/10/23 dadams
;;     Removed references to menu-bar-files-menu - test version, not boundp menu-bar-file-menu.
;;     Still keep "files" in menu-bar-final-items for version < 21; else wrong order.
;; 2005/08/02 dadams
;;     Added to Do Re Mi menu: doremi-all-faces-fg, doremi-all-frames-bg.
;; 2005/06/14 dadams
;;     For Emacs 22: menu-bar-files-menu -> menu-bar-file-menu.
;;     No longer redefine File(s) menu from scratch, removing default bindings.
;;     Open File and Open Directory: Don't use other frame, except in Emacs < 22.
;;     Don't bother to rename File menu items (suggested renamings to emacs-devel@gnu.org.
;;     menu-bar-edit-menu: defvar -> setq.
;;     menu-bar-final-items: Use default order.
;; 2005/05/28 dadams
;;     Protected menu-bar-last-search-type with boundp (thanks to Tim Johnson for the report).
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/10 dadams
;;     Added: set[-all]-frame-alist-parameter[s]-from-frame.
;; 2005/01/25 dadams
;;     Added: menu-bar-make-toggle-any-version, menu-bar-options-save.
;;     Added to Options menu: doremi-push-frame-config-for-cmds-flag, inhibit-fit-frame-flag,
;;           autofit-frames-flag, thumbify-instead-of-iconify-flag, replace-w-completion-flag.
;; 2005/01/20 dadams
;;     Removed: exit-with-confirmation.
;; 2005/01/09 dadams
;;     Renamed: doremi-bg-rgb to doremi-bg, doremi-face-bg-rgb to doremi-face-bg,
;;              doremi-face-fg-rgb to doremi-face-fg.
;; 2005/01/02 dadams
;;     Added doremi-marks, doremi-global-marks.
;; 2004/12/28 dadams
;;     Added doremi-face-fg-rgb, doremi-face-bg-rgb, doremi-*-separator.
;; 2004/12/11 dadams
;;     Added doremi-thumbnail-frames.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;;     Require info+.el for all versions of Emacs.
;; 2004/11/16 dadams
;;     Removed requires of files that redefine std cmds, since std versions available.
;; 2004/10/12 dadams
;;     Added require of replace+.el for Emacs 21 also.
;; 2004/10/01 dadams
;;     Updated for Emacs 21 also.
;;     Added macro menu-item-any-version.
;; 2004/09/26 dadams
;;     Use new Do Re Mi names and files.
;; 2004/09/20 dadams
;;     Use adjust-bg-rgb instead of adjust-bg-color.
;; 2004/09/11 dadams
;;     Reflected move of commands from doremi.el to doremi-frm.el
;; 2004/09/10 dadams
;;     Replaced dlgopen.el with w32browser-dlgopen.el.
;; 2004/09/07 dadams
;;     Added doremi menu.
;; 2004/03/19 dadams
;;     Added to menu-bar-frames-menu: tile-frames-[horizontally|vertically].
;; 2000/09/27 dadams
;;     1. Added to Files menu: execute-extended-command, repeat-complex-command.
;;     2. Removed help-frame condition on show-*Help*-buffer.
;; 1999/10/07 dadams
;;     Added show-calendar and separator to Tools menu.
;; 1999/10/01 dadams
;;     Added: menu-bar-divider-menu.  Use it for [menu-bar divider].
;; 1999/09/02 dadams
;;     kill-this-buffer: use sub-kill-buffer-and-its-windows.
;; 1999/08/25 dadams
;;     1. Added Frames menu.  Changed Help to ? menu.
;;     2. Commented out menu-bar-print-menu.
;; 1999/04/08 dadams
;;     Added to help menu: help-for-help.
;; 1999/04/07 dadams
;;     1. Bound apropos stuff regardless of (fboundp 'apropos).
;;     2. Corrected help menu order.
;; 1999/04/07 dadams
;;     1. Added to help menu: help-on-click, save-*Help*-buffer.
;;     2. apropos-symbol->apropos; super-apropos-symbol->apropos-documentation.
;; 1999/04/06 dadams
;;     Added *highlight*-region fns to Edit->Region submenu.
;; 1999/04/02 dadams
;;     Only add "Show *Help* Buffer" if help-frame.
;; 1999/03/26 dadams
;;     Added vc-ediff to ediff menu (when fboundp).
;; 1999/03/23 dadams
;;     Added: ediff-revision, vc-diff.
;; 1999/03/17 dadams
;;     1. Moved Replace menu to be a Search submenu.
;;     2. Reordered Edit menu.
;;     3. Removed default Help items (duplicate).
;; 1996/04/26 dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/24 dadams
;;     Added edit-options to Edit menu.
;; 1996/04/22 dadams
;;     1. menu-bar-edit-menu:  Added: flush-lines, keep-lines.
;;     2. menu-bar-edit-region-menu:  Added: (un)tabify-region, center-region,
;;        indent-rigidly-region, abbrevs-region, macro-region.
;; 1996/04/04 dadams
;;     1. Added fill-paragraph-ala-mode.
;;     2. Edit menu:
;;        a. Added yank-secondary and select-all to Edit menu.
;;        d. Added Edit submenus Fill, Region, Sort, Highlight.
;; 1996/03/18 dadams
;;     Added vc-diff to menu-bar-ediff-menu.
;; 1996/03/12 dadams
;;     Added diff and reordered ediff menu.
;; 1996/03/08 dadams
;;     Added redefinition of kill-this-buffer.
;; 1996/02/08 dadams
;;     Added: save-*Help*-buffer, describe-syntax, locate-library,
;;            finder-by-keyword, view-emacs-lisp-news.
;; 1996/01/26 dadams
;;     no-op -> %$>disabled@!^ (Shouldn't be bound command, else binding is shown.)
;; 1996/01/25 dadams
;;     menu-bar-help-menu: Added Emacs FAQ.
;; 1996/01/17 dadams
;;     apropos -> apropos-symbol, super-apropos -> super-apropos-symbol.
;; 1995/09/11 dadams
;;     Bookmarks added to Search menu.
;; 1995/08/29 dadams
;;     1) Added to Search menu: grep, occur.
;;     2) Put tags searches on submenu of Search.
;; 1995/08/23 dadams
;;     Changed menu-bar-final-items order.
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

(require 'menu-bar)

(and (< emacs-major-version 21);; dolist (plus, for Emacs <20: when, unless)
     (eval-when-compile (require 'cl)))
(when (eq system-type 'windows-nt)
  (require 'w32browser-dlgopen nil t)) ;; (no error if not found): dlgopen-open-files
                                       ;; `w32browser-dlgopen.el' is based on `dlgopen.el'
                                       ;; by Binu Jose Philip

;;; These libraries are not yet tested on Emacs 21.
(when (< emacs-major-version 21)
  (require 'help+ nil t) ;; (no error if not found): help-on-click/key
  (require 'unaccent nil t)) ;; (no error if not found): unaccent-region

(require 'info+ nil t) ;; (no error if not found): menu-bar-read-lispref, info-emacs-manual,
(require 'replace+ nil t) ;; (no error if not found): query-replace-w-options,
(require 'highlight nil t) ;; (no error if not found): highlight-region,
                           ;; highlight-regexp-region, unhighlight-region
(require 'misc-cmds nil t) ;; (no error if not found):
                           ;; yank-secondary, kill-buffer-and-its-windows,
(require 'apropos+ nil t) ;; (no error if not found): apropos-user-options

;; To quiet the Emacs 20 byte compiler
(defvar menu-bar-goto-menu)

;;;;;;;;;;;;;;;;;;;;



;; Note: COMMAND must be a command (`commandp'); it cannot be an expression.
(defmacro menu-item-any-version (item-string command &rest keywords)
  "Return a valid `menu-item' spec.  Usable for Emacs 20 and later.
ITEM-STRING, COMMAND, and KEYWORDS are as for `define-key'.
KEYWORDS are not used for Emacs 20."
  (if (or (< emacs-major-version 21) (null keywords))
      `(cons ,item-string ',command)
    `'(menu-item ,item-string ,command ,@keywords)))


;; REPLACES ORIGINAL in `menu-bar.el':
;; Use Emacs 22 definition.
;; Emacs 20 version fails when last-command-event is the name of the frame.
;;
(when (< emacs-major-version 21)
  (defun menu-bar-select-frame ()
    (interactive)
    (let (frame)
      (dolist (f (frame-list))
        (when (equal last-command-event (frame-parameter f 'name))
          (setq frame f)))
      ;; FRAME can be nil when user specifies the selected frame.
      (setq frame (or frame (selected-frame)))
      (make-frame-visible frame)
      (raise-frame frame)
      (select-frame frame))))


;; REPLACES ORIGINAL in `menu-bar.el':
;; Uses -other-frame.  defun -> defsubst.
(defsubst menu-bar-select-buffer ()
  "Switch to `last-command-event' buffer in other frame."
  (interactive) (switch-to-buffer-other-frame last-command-event)) ;`files+.el'


;; REPLACES ORIGINAL MENU-BAR.

;;; Main MENU-BAR entries.
;; Divider before standard menus.
;;;###autoload
(defvar menu-bar-divider-menu (make-sparse-keymap "Divider"))
(define-key global-map [menu-bar divider] (cons "||" menu-bar-divider-menu))
(define-key menu-bar-divider-menu [menu-bar-divider-hint]
  '("<-- Current mode menus to left.   ||   Common menus to right -->"
    . describe-menubar))

;;;###autoload
(defun describe-menubar ()
  "Explain the menu bar, in general terms."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (substitute-command-keys
            "To the right of the menu bar divider (\"||\") are the general menus
that always appear in every buffer.  To the left of this symbol, there
may also be additional menus that are specific to the buffer's mode
\(use `\\[describe-mode]' for information on a buffer's mode).

The general menus are as follows:

    Buffers  File  Tools  Edit  Frames  Do Re Mi  Help

Use the \"Frames\" menu to resize, tile, and hide/show frames.
Use the \"Do Re Mi\" menu to incrementally change things.
The \"Help\" menu extends the \"Help\" menu described in the Emacs manual (`\\[info]').

For information on a menu item, use the \"This\" item in the \"Describe\"
submenu of the \"Help\" menu."))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      (help-mode)
      (buffer-string))))                ; Return the text we displayed.


;; REPLACES ORIGINAL defined in `menu-bar.el'.
(setq menu-bar-edit-menu (make-sparse-keymap "Edit"))
(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))


;; REPLACES ORIGINAL menuus defined in `menu-bar.el'.
;; These are all moved to new top-level Search menu.
(if (< emacs-major-version 21)
    (global-unset-key [menu-bar search])
  (global-unset-key [menu-bar edit search])
  (global-unset-key [menu-bar edit separator-search])
  (global-unset-key [menu-bar edit replace])
  (global-unset-key [menu-bar edit goto])
  (global-unset-key [menu-bar edit bookmark])
  (global-unset-key [menu-bar edit separator-bookmark]))
(defconst menu-bar-search-menu (make-sparse-keymap "Search"))
(define-key global-map [menu-bar search]  (cons "Search" menu-bar-search-menu))


(when (or (featurep 'doremi-frm) (featurep 'doremi-cmd))
  (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
  (define-key global-map [menu-bar doremi] (cons "Do Re Mi" menu-bar-doremi-menu)))

(when (or (featurep 'frame-cmds) (featurep 'fit-frame))
  (defvar menu-bar-frames-menu (make-sparse-keymap "Frames"))
  (define-key global-map [menu-bar frames] (cons "Frames" menu-bar-frames-menu)))

;; Change main menu-bar order.
(setq menu-bar-final-items
      (append (if (< emacs-major-version 21)
                  '(divider files edit buffer tools search mule)
                '(divider file edit options buffer tools search))
              (and (boundp 'menu-bar-frames-menu) '(frames))
              (and (boundp 'menu-bar-doremi-menu) '(doremi))
              '(help-menu)))

;;; FRAMES menu.
(when (or (featurep 'frame-cmds) (featurep 'fit-frame))
  (define-key menu-bar-frames-menu [set-all-params-from-frame]
    (menu-item-any-version
     "Set All Frame Parameters from Frame" set-all-frame-alist-parameters-from-frame
     :help "Set frame parameters of a frame to their current values in frame"))
  (define-key menu-bar-frames-menu [set-params-from-frame]
    (menu-item-any-version
     "Set Frame Parameter from Frame..." set-frame-alist-parameter-from-frame
     :help "Set parameter of a frame alist to its current value in frame"))
  (define-key menu-bar-frames-menu [separator-set-params] '("--"))
  (define-key menu-bar-frames-menu [tile-frames-vertically]
    (menu-item-any-version
     "Tile Frames Vertically..." tile-frames-vertically
     :help "Tile all visible frames vertically"))
  (define-key menu-bar-frames-menu [tile-frames-horizontally]
    (menu-item-any-version
     "Tile Frames Horizontally..." tile-frames-horizontally
     :help "Tile all visible frames horizontally"))
  (define-key menu-bar-frames-menu [separator-adjust] '("--"))
  (define-key menu-bar-frames-menu [iconify-everything]
    (menu-item-any-version
     "Iconify All Frames" iconify-everything
     :help "Iconify all frames of session at once"))
  (define-key menu-bar-frames-menu [show-hide]
    (menu-item-any-version
     "Hide Frames / Show Buffers" show-hide
     :help "Show, if only one frame visible; else hide."))
  (define-key menu-bar-frames-menu [fit-frame]
    (menu-item-any-version
     "Fit This Frame" fit-frame  ; Defined in `fit-frame.el'.
     :help "Resize frame to fit its selected window")))

;;; DO RE MI menu.
(when (featurep 'doremi-cmd)
  (define-key menu-bar-doremi-menu [doremi-global-marks]
    (menu-item-any-version "Global Marks" doremi-global-marks
                           :help "Successively cycle among global marks"))
  (define-key menu-bar-doremi-menu [doremi-marks]
    (menu-item-any-version "Marks in Buffer" doremi-marks
                           :help "Successively cycle among marks in this buffer"))
  (define-key menu-bar-doremi-menu [doremi-bookmarks]
    (menu-item-any-version "Bookmarks" doremi-bookmarks
                           :help "Successively cycle among bookmarks"))
  (define-key menu-bar-doremi-menu [doremi-buffers]
    (menu-item-any-version "Buffers" doremi-buffers
                           :help "Successively cycle among buffers")))
(when (featurep 'thumb-frm)
    (define-key menu-bar-doremi-menu [doremi-thumbnail-frames]
    (menu-item-any-version "Fisheye Frame" doremi-thumbnail-frames
                           :help "Cycle among frames using fisheye")))
(when (and (boundp 'menu-bar-doremi-menu) (featurep 'frame-cmds))
  (define-key menu-bar-doremi-menu [save-frame-config]
    (menu-item-any-version
     "Save Frame Configuration" save-frame-config
     :help "Save current frame configuration (M-x jump-to-frame-config-register restores)")))
(when (featurep 'doremi-frm)
  (define-key menu-bar-doremi-menu [doremi-frame-configs]
    (menu-item-any-version "Frame Configurations" doremi-frame-configs
                           :help "Cycle among frame configurations recorded"))
  (define-key menu-bar-doremi-menu [doremi-frame-configs-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-font]
    (menu-item-any-version "Font" doremi-font
                           :help "Successively cycle among fonts, choosing by name"))
  (define-key menu-bar-doremi-menu [doremi-font-size]
    (menu-item-any-version "Font Size (Zoom)" doremi-font-size
                           :help "Change font size for frame incrementally"))
  (define-key menu-bar-doremi-menu [doremi-face-bg]
    (menu-item-any-version
     "Face Background..." doremi-face-bg
     :help "Change background color of a face incrementally"))
  (define-key menu-bar-doremi-menu [doremi-all-faces-fg]
    (menu-item-any-version
     "All Faces - Foreground..." doremi-all-faces-fg
     :help "Change foreground color of all faces incrementally"))
  (define-key menu-bar-doremi-menu [doremi-face-fg]
    (menu-item-any-version
     "Face Foreground..." doremi-face-fg
     :help "Change foreground color of a face incrementally"))
  (define-key menu-bar-doremi-menu [doremi-all-frames-bg]
    (menu-item-any-version
     "All Frame Backgrounds..." doremi-all-frames-bg
     :help "Change background of all frames incrementally"))    
  (define-key menu-bar-doremi-menu [doremi-bg]
    (menu-item-any-version
     "Frame Background..." doremi-bg
     :help "Change frame background color incrementally")))
(when (featurep 'doremi-cmd)
  (define-key menu-bar-doremi-menu [doremi-color-themes]
    (menu-item-any-version "Color Themes" doremi-color-themes
                           :help "Successively cycle among color themes")))
(when (featurep 'doremi-frm)
  (define-key menu-bar-doremi-menu [doremi-frame-params-separator] '("--"))
  (define-key menu-bar-doremi-menu [doremi-frame-vertically]
    (menu-item-any-version "Move Frame Vertically" doremi-frame-vertically
                           :help "Move current frame up/down incrementally"))
  (define-key menu-bar-doremi-menu [doremi-frame-horizontally]
    (menu-item-any-version "Move Frame Horizontally" doremi-frame-horizontally
                           :help "Move current frame left/right incrementally"))
  (define-key menu-bar-doremi-menu [doremi-frame-height]
    (menu-item-any-version "Frame Height" doremi-frame-height
                           :help "Change height of current frame incrementally"))
  (define-key menu-bar-doremi-menu [doremi-frame-width]
    (menu-item-any-version "Frame Width" doremi-frame-width
                           :help "Change width of current frame incrementally")))


;;; FILES menu.
;;
(when (< emacs-major-version 21)
  ;; Use `dlgopen-open-files' if available; else use `find-file-other-frame'.
  (define-key menu-bar-file-menu [open-file]
    (if (and (fboundp 'dlgopen-open-files) (eq system-type 'windows-nt))
        (menu-item-any-version "Open File..." dlgopen-open-files
                               :enable (not (window-minibuffer-p
                                             (frame-selected-window menu-updating-frame)))
                               :help "Read a file into an Emacs buffer")
      (menu-item-any-version "Open File..." find-file-other-frame
                             :enable (not (window-minibuffer-p
                                           (frame-selected-window menu-updating-frame)))
                             :help "Read a file into an Emacs buffer")))
  ;; Use other frame.
  (define-key menu-bar-file-menu [dired]
    (menu-item-any-version "Open Directory..." dired-other-frame
                           :help "Read a directory; operate on its files (Dired)"))
  (put 'dlgopen-open-files 'menu-enable '(not (window-minibuffer-p
                                               (frame-selected-window menu-updating-frame))))
  (put 'find-file-other-frame 'menu-enable '(not (window-minibuffer-p
                                                  (frame-selected-window menu-updating-frame))))
  (put 'dired-other-frame 'menu-enable '(not (window-minibuffer-p
                                              (frame-selected-window menu-updating-frame)))))

(define-key-after menu-bar-file-menu [exec-cmd]
  (menu-item-any-version "Execute Command" execute-extended-command
                         :help "Prompts for a command to execute") 'separator-exit)
(define-key-after menu-bar-file-menu [repeat-cmd]
  (menu-item-any-version "Repeat Earlier Command" repeat-complex-command
                         :help "Edit and re-evaluate last complex command") 'exec-cmd)

(define-key-after menu-bar-file-menu [separator-execute]
  '("--") 'repeat-cmd)
(define-key-after menu-bar-file-menu [exit-emacs]
  (menu-item-any-version "Exit Emacs" save-buffers-kill-emacs
                         :help "Save unsaved buffers, then exit") 'separator-execute)


;; REPLACES ORIGINAL in `menu-bar.el':
;; Deletes buffer's windows as well.  defun -> defsubst.
(defun kill-this-buffer ()
"Delete the current buffer and delete all of its windows."
  (interactive)
  (if (and (boundp 'sub-kill-buffer-and-its-windows) ; In `setup-keys.el'.
           sub-kill-buffer-and-its-windows
           (fboundp 'kill-buffer-and-its-windows))
      (kill-buffer-and-its-windows (current-buffer)) ;`misc-cmds.el'
    (kill-buffer (current-buffer))))    ; <-- original defn.


;; EDIFF submenu of TOOLS
(when (fboundp 'vc-ediff)
  (define-key menu-bar-tools-menu [compare]
    (menu-item-any-version "Compare" menu-bar-ediff-menu ; Remove "(Ediff)".
                           :help "Display differences between files/directories")))
(define-key menu-bar-ediff-menu [ediff-revision] ; Defined in `vc+.el'.
  (menu-item-any-version "File with Revision..." vc-ediff
                         :help "Compare file versions using `ediff'"))
(define-key-after menu-bar-ediff-menu [vc-diff] ; Defined in `vc+.el'.
  (menu-item-any-version "File with Revision using Diff" vc-diff
                         :help "Display diffs between file versions using `diff'")
  'ediff-revision)
(define-key-after menu-bar-ediff-menu [diff]
  (menu-item-any-version "Two Files using Diff..." diff ; `diff+.el'
                         :help "Display diffs between two files using `diff'")
  'ediff-files)

(define-key menu-bar-edit-menu [undo]
  (menu-item-any-version "Undo" undo
	      :enable (and (not buffer-read-only)
			   (not (eq t buffer-undo-list))
			   (if (eq last-command 'undo)
			       pending-undo-list
			     (consp buffer-undo-list)))
	      :help "Undo last operation"))
(when (< emacs-major-version 21)
  (put 'undo 'menu-enable '(and (not buffer-read-only) (not (eq t buffer-undo-list))
                            (if (eq last-command 'undo)
                                pending-undo-list
                              (consp buffer-undo-list)))))
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-cut] '("--") 'undo)

(define-key-after menu-bar-edit-menu [cut]
  (menu-item-any-version "Cut" kill-region
	      :enable (and mark-active (not buffer-read-only))
	      :help
	      "Cut (kill) text in region between mark and current position")
  'separator-edit-cut)
(when (< emacs-major-version 21)
  (put 'kill-region 'menu-enable '(and mark-active (not buffer-read-only))))

(define-key-after menu-bar-edit-menu [copy]
  (menu-item-any-version "Copy" menu-bar-kill-ring-save
	      :enable mark-active
	      :help "Copy text in region between mark and current position"
	      :keys "\\[kill-ring-save]")
  'cut)
(when (< emacs-major-version 21) (put 'menu-bar-kill-ring-save 'menu-enable 'mark-active))

(define-key-after menu-bar-edit-menu [paste]
  (menu-item-any-version "Paste" yank
	      :enable (and
		       ;; Emacs compiled --without-x doesn't have
		       ;; x-selection-exists-p.
		       (fboundp 'x-selection-exists-p)
		       (x-selection-exists-p) (not buffer-read-only))
	      :help "Paste (yank) text most recently cut/copied")
  'copy)
(define-key-after menu-bar-edit-menu [yank-secondary]
  (menu-item-any-version "Paste Secondary" yank-secondary ; In `misc-cmds.el'
                         :enable (x-selection-exists-p 'SECONDARY)
                         :help "Paste the secondary selection at the cursor position")
  'paste)
(when (< emacs-major-version 21)
  (put 'yank-secondary 'menu-enable '(x-selection-exists-p 'SECONDARY)))

(defvar yank-menu (cons "Select Yank" nil))
(fset 'yank-menu (cons 'keymap yank-menu))

(define-key-after menu-bar-edit-menu [select-paste]
  (menu-item-any-version "Select and Paste" yank-menu
	      :enable (and (cdr yank-menu) (not buffer-read-only))
	      :help "Paste (yank) text cut or copied earlier")
  'yank-secondary)

(define-key-after menu-bar-edit-menu [clear]
  (menu-item-any-version "Clear" delete-region
                         :enable (and mark-active (not buffer-read-only)
                                      (not (mouse-region-match)))
                         :help "Delete the text in region between mark and current position")
  'select-paste)
(define-key-after menu-bar-edit-menu [mark-whole-buffer]
  (menu-item-any-version "Select All" mark-whole-buffer
                         :help "Select everything in buffer (for a subsequent cut/copy)")
  'clear)

;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-delete-lines] '("--") 'mark-whole-buffer)

(define-key-after menu-bar-edit-menu [flush-lines] ; Defined in `replace+.el'.
  (menu-item-any-version
   "Delete Matching Lines..." flush-lines
   :help "Delete all lines after cursor that match a regular expression")
  'separator-edit-delete-lines)
(define-key-after menu-bar-edit-menu [keep-lines] ; Defined in `replace+.el'.
  (menu-item-any-version
   "Delete Non-Matching Lines..." keep-lines
   :help "Delete all lines after cursor that do not match a regular expression")
  'flush-lines)
;;--------------------
(define-key-after menu-bar-edit-menu [separator-edit-select-all] '("--") 'keep-lines)
(defvar menu-bar-edit-fill-menu (make-sparse-keymap "Fill"))
(define-key-after menu-bar-edit-menu [props]
  (menu-item-any-version "Text Properties" facemenu-menu
                         :help "Change properties of text in region")
  'separator-edit-select-all)
(define-key-after menu-bar-edit-menu [fill]
  (cons "Fill" menu-bar-edit-fill-menu) 'props)
(defvar menu-bar-edit-region-menu (make-sparse-keymap "Edit Region"))
(defalias 'menu-bar-edit-region-menu (symbol-value 'menu-bar-edit-region-menu))
(define-key-after menu-bar-edit-menu [region]
  (cons "Edit Region" menu-bar-edit-region-menu) 'fill)
(defvar menu-bar-edit-sort-menu (make-sparse-keymap "Sort Region"))
(defalias 'menu-bar-edit-sort-menu (symbol-value 'menu-bar-edit-sort-menu))
(define-key-after menu-bar-edit-menu [sort]
  (cons "Sort Region" menu-bar-edit-sort-menu) 'region)

;; EDIT FILL submenu.
(define-key menu-bar-edit-fill-menu [fill-nonuniform-para]
  (menu-item-any-version "Fill Non-Uniform ¶'s" fill-nonuniform-paragraphs
                         :enable (and mark-active (not buffer-read-only))
                         :help "Fill paragraphs in selection, allowing varying indentation"))
(when (< emacs-major-version 21)
  (put 'fill-nonuniform-paragraphs 'menu-enable 'mark-active))
(define-key menu-bar-edit-fill-menu [fill-indiv-para]
  (menu-item-any-version "Fill Uniform ¶'s" fill-individual-paragraphs
                         :enable (and mark-active (not buffer-read-only))
                         :help "Fill paragraphs of uniform indentation within selection"))
(when (< emacs-major-version 21)
  (put 'fill-individual-paragraphs 'menu-enable 'mark-active))
(define-key menu-bar-edit-fill-menu [fill-region]
  (menu-item-any-version "Fill ¶'s" fill-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Fill text in region to fit between left and right margin"))
(define-key menu-bar-edit-fill-menu [fill-para]
  (menu-item-any-version "Fill ¶" fill-paragraph-ala-mode
                         :help "Fill the paragraph, doing what `M-q' does (if bound)"))

(defun fill-paragraph-ala-mode (&optional arg)
  "Do whatever `M-q' does, if it is bound.  Else, `fill-paragraph'.
Normally, this fills a paragraph according to the current major mode.
For example, in C Mode, `M-q' is normally bound to `c-fill-paragraph',
and in Lisp Mode, `M-q' is normally bound to `lisp-fill-paragraph'.
ARG means justify as well as fill."
  (let ((map (current-local-map)))
    (or (and map (funcall (lookup-key map "\M-q") arg))
        (funcall (lookup-key (current-global-map) "\M-q") arg)
        (fill-paragraph arg))))

;; EDIT REGION submenu.
(when (fboundp 'unaccent-region)
  (define-key menu-bar-edit-region-menu [unaccent-region]
    (menu-item-any-version "Unaccent" unaccent-region ; Defined in `unaccent'.
                           :enable (and mark-active (not buffer-read-only))
                           :help "Replace accented chars in selection by unaccented chars")))
(when (< emacs-major-version 21)
  (put 'unaccent-region 'menu-enable 'mark-active))
(define-key menu-bar-edit-region-menu [capitalize-region]
  (menu-item-any-version "Capitalize" capitalize-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Capitalize (initial caps) words in the selection"))
(when (< emacs-major-version 21)
  (put 'capitalize-region 'menu-enable 'mark-active))
(define-key menu-bar-edit-region-menu [downcase-region]
  (menu-item-any-version "Downcase" downcase-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Make words in the selection lower-case"))
(when (< emacs-major-version 21)
  (put 'downcase-region 'menu-enable 'mark-active))
(define-key menu-bar-edit-region-menu [upcase-region]
  (menu-item-any-version "Upcase" upcase-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Make words in the selection upper-case"))
(when (< emacs-major-version 21)
  (put 'upcase-region 'menu-enable 'mark-active))
;;--------------------
(when (fboundp 'highlight-region)
  (define-key menu-bar-edit-region-menu [separator-chars] '("--"))
  (define-key menu-bar-edit-region-menu [unhighlight-region]
    (menu-item-any-version "Unhighlight" unhighlight-region
                           :enable (and mark-active (not buffer-read-only))
                           :help "Remove highlighting (faces) in region"))
  (when (< emacs-major-version 21)
    (put 'unhighlight-region 'menu-enable 'mark-active))
  (define-key menu-bar-edit-region-menu [highlight-regexp-region]
    (menu-item-any-version
     "Highlight Regexp..." highlight-regexp-region
     :enable (and mark-active (not buffer-read-only))
     :help "Highlight parts of selection that match a regular expression"))
  (when (< emacs-major-version 21)
    (put 'highlight-regexp-region 'menu-enable 'mark-active))
  (define-key menu-bar-edit-region-menu [highlight-region]
    (menu-item-any-version "Highlight..." highlight-region
                           :help "Highlight all text in the selection"))
  (when (< emacs-major-version 21)
    (put 'highlight-region 'menu-enable 'mark-active)))
;;--------------------
(define-key menu-bar-edit-region-menu [separator-highlight] '("--"))
(define-key menu-bar-edit-region-menu [untabifyn]
  (menu-item-any-version
   "Untabify" untabify
   :enable mark-active
   :help "Convert all tabs in region (selection) to multiple spaces"))
(when (< emacs-major-version 21) (put 'untabify 'menu-enable 'mark-active))

(define-key menu-bar-edit-region-menu [tabify-region]
  (menu-item-any-version
   "Tabify" tabify
   :enable mark-active
   :help "Convert multiple spaces in region to tabs when possible"))
(when (< emacs-major-version 21) (put 'tabify 'menu-enable 'mark-active))

(define-key menu-bar-edit-region-menu [comment-region]
  (menu-item-any-version "(Un)Comment" comment-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Comment or uncomment each line in the selection"))
(when (< emacs-major-version 21)
  (put 'comment-region 'menu-enable '(and mark-active comment-start)))

(define-key menu-bar-edit-region-menu [center-region]
  (menu-item-any-version "Center" center-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Center each nonblank line that starts in the selection"))
(when (< emacs-major-version 21) (put 'center-region 'menu-enable 'mark-active))

(define-key menu-bar-edit-region-menu [indent-rigidly-region]
  (menu-item-any-version "Rigid Indent" indent-rigidly
                         :enable (and mark-active (not buffer-read-only))
                         :help "Indent each line that starts in the selection"))
(when (< emacs-major-version 21) (put 'indent-rigidly 'menu-enable 'mark-active))

(define-key menu-bar-edit-region-menu [indent-region]
  (menu-item-any-version "Column/Mode Indent" indent-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Indent each nonblank line in the selection"))
(when (< emacs-major-version 21) (put 'indent-region 'menu-enable 'mark-active))

;;--------------------
(define-key menu-bar-edit-region-menu [separator-indent] '("--"))
(define-key menu-bar-edit-region-menu [abbrevs-region]
  (menu-item-any-version "Expand Abbrevs" expand-region-abbrevs
                         :enable (and mark-active (not buffer-read-only))
                         :help "Expand each abbrev in the selection (with confirmation)"))
(when (< emacs-major-version 21) (put 'expand-region-abbrevs 'menu-enable 'mark-active))

(define-key menu-bar-edit-region-menu [macro-region]
  (menu-item-any-version "Exec Keyboard Macro" apply-macro-to-region-lines ; In `macros+.el'.
                         :enable (and last-kbd-macro mark-active (not buffer-read-only))
                         :help "Run keyboard macro at start of each line in selection"))
(when (< emacs-major-version 21)
  (put 'apply-macro-to-region-lines 'menu-enable '(and last-kbd-macro mark-active)))

;; EDIT SORT submenu.
(define-key menu-bar-edit-sort-menu [sort-regexp-fields]
  (menu-item-any-version "Regexp Fields..." sort-regexp-fields
                         :enable (and last-kbd-macro mark-active (not buffer-read-only))
                         :help "Sort the selection lexicographically"))
(when (< emacs-major-version 21) (put 'sort-regexp-fields 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-pages]
  (menu-item-any-version "Pages" sort-pages
                         :enable (and mark-active (not buffer-read-only))
                         :help "Sort pages in the selection alphabetically"))
(when (< emacs-major-version 21) (put 'sort-pages 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-paragraphs]
  (menu-item-any-version "Paragraphs" sort-paragraphs
                         :enable (and mark-active (not buffer-read-only))
                         :help "Sort paragraphs in the selection alphabetically"))
(when (< emacs-major-version 21) (put 'sort-paragraphs 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-numeric-fields]
  (menu-item-any-version "Numeric Field" sort-numeric-fields
                         :enable (and mark-active (not buffer-read-only))
                         :help "Sort lines in selection numerically by the Nth field"))
(when (< emacs-major-version 21) (put 'sort-numeric-fields 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-fields]
  (menu-item-any-version "Field" sort-fields
                         :enable (and mark-active (not buffer-read-only))
                         :help "Sort lines in selection lexicographically by the Nth field"))
(when (< emacs-major-version 21) (put 'sort-fields 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-columns]
  (menu-item-any-version
   "Columns" sort-columns
   :enable (and mark-active (not buffer-read-only))
   :help "Sort lines in selection alphabetically, by a certain range of columns"))
(when (< emacs-major-version 21) (put 'sort-columns 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [sort-lines]
  (menu-item-any-version "Lines" sort-lines
                         :enable (and mark-active (not buffer-read-only))
                         :help "Sort lines in selection alphabetically"))
(when (< emacs-major-version 21) (put 'sort-lines 'menu-enable 'mark-active))

(define-key menu-bar-edit-sort-menu [reverse-region]
  (menu-item-any-version "Reverse" reverse-region
                         :enable (and mark-active (not buffer-read-only))
                         :help "Reverse the order of the selected lines"))
(when (< emacs-major-version 21) (put 'reverse-region 'menu-enable 'mark-active))


;;; SEARCH menu.

(defun nonincremental-repeat-word-search-forward ()
  "Search forward for the previous search string."
  (interactive)
  (word-search-forward (car search-ring)))

(defun nonincremental-repeat-word-search-backward ()
  "Search backward for the previous search string."
  (interactive)
  (word-search-backward (car search-ring)))

(define-key menu-bar-search-menu [reminder6] '(" " . %$>disabled@!^))
(define-key menu-bar-search-menu [reminder5]
  (cons (substitute-command-keys
         "  Incr. Regexp Search: \\[isearch-forward-regexp], \
\\[isearch-backward-regexp]") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder4]
  (cons (substitute-command-keys
         " Word Search: \\[isearch-forward] RET C-w, \\[isearch-backward] \
RET C-w") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder3]
  (cons (substitute-command-keys
         "Incr. Search: \\[isearch-forward], \\[isearch-backward]  \
(\\[isearch-forward] C-h: Help)") '%$>disabled@!^))
(define-key menu-bar-search-menu [reminder2]
  '("           ** Reminder **" . %$>disabled@!^))
(define-key menu-bar-search-menu [reminder1] '(" " . %$>disabled@!^))
(put '%$>disabled@!^ 'menu-enable '(not t))
;;--------------------
(when (boundp 'menu-bar-i-search-menu)
  (define-key menu-bar-search-menu [i-search]
    (menu-item-any-version
     "Incremental Search" menu-bar-i-search-menu
     :help "Incremental Search finds partial matches while you type the search \
string.\nIt is most convenient from the keyboard.  Try it!")))
(define-key menu-bar-search-menu [separator-search-multiple] '("--"))
(when (>= emacs-major-version 21)
  (define-key menu-bar-search-menu [goto]
    (cons "Go To" menu-bar-goto-menu)))
(define-key menu-bar-search-menu [bookmark]
  (menu-item-any-version
   "Bookmarks" menu-bar-bookmark-map
   :help "Record buffer positions (\"bookmarks\"), and jump between them"))
(defvar menu-bar-search-tags-menu (make-sparse-keymap "Tags"))
(defalias 'menu-bar-search-tags-menu
  (symbol-value 'menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [tags]
  (cons "Tags" menu-bar-search-tags-menu))
(define-key menu-bar-search-menu [occur]
  (menu-item-any-version
   "Occurrences..." occur
   :help "Show lines in buffer that contain a match for regular expression"))
(define-key menu-bar-search-menu [grep]
  (menu-item-any-version
   "Grep..." grep
   :help "Run `grep' and collect output for navigating to match lines"))
;;--------------------
(define-key menu-bar-search-menu [separator-search-replace] '("--"))
(defvar menu-bar-search-replace-menu (make-sparse-keymap "Replace"))
(defalias 'menu-bar-search-replace-menu (symbol-value 'menu-bar-search-replace-menu))
(define-key menu-bar-search-menu [replace]
  (cons "Replace" menu-bar-search-replace-menu))
;;--------------------
(define-key menu-bar-search-menu [separator-search-word] '("--"))
(define-key menu-bar-search-menu [repeat-word-search-back]
  (menu-item-any-version "             Again" nonincremental-repeat-word-search-backward
                         :help "Search backward again for the same word"))
(define-key menu-bar-search-menu [word-search-back]
  (menu-item-any-version "     Backward..." word-search-backward
                         :help "Search backward, ignoring differences in puncuation"))
(define-key menu-bar-search-menu [repeat-word-search-fwd]
  (menu-item-any-version "             Again" nonincremental-repeat-word-search-forward
                         :help "Search forward again for the same word"))
(define-key menu-bar-search-menu [word-search-fwd]
  (menu-item-any-version "Word Forward..." word-search-forward
                         :help "Search forward, ignoring differences in puncuation"))
;;--------------------
(define-key menu-bar-search-menu [separator-search-re] '("--"))
(define-key menu-bar-search-menu [repeat-regexp-back]
  (menu-item-any-version "               Again" nonincremental-repeat-re-search-backward
                         :help "Search forward again for the same regular expression"))
(define-key menu-bar-search-menu [re-search-backward]
  (menu-item-any-version "       Backward..." nonincremental-re-search-backward
                         :help "Search backward for a regular expression"))
(define-key menu-bar-search-menu [repeat-regexp-fwd]
  (menu-item-any-version "               Again" nonincremental-repeat-re-search-forward
                         :help "Search forward again for the same regular expression"))
(define-key menu-bar-search-menu [re-search-forward]
  (menu-item-any-version "Regexp Forward..." nonincremental-re-search-forward
                         :help "Search forward for a regular expression"))
;;--------------------
(define-key menu-bar-search-menu [separator-search] '("--"))
(define-key menu-bar-search-menu [repeat-search-back]
  (menu-item-any-version
   "               Again" nonincremental-repeat-search-backward
   :enable (or (not (boundp 'menu-bar-last-search-type))
               (and (eq menu-bar-last-search-type 'string) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
   :help "Repeat last search backward"))
(when (< emacs-major-version 21)
  (put 'nonincremental-repeat-search-backward 'menu-enable
       '(or (not (boundp 'menu-bar-last-search-type))
         (and (eq menu-bar-last-search-type 'string) search-ring)
         (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))))

(define-key menu-bar-search-menu [search-backward]
  (menu-item-any-version "String Backward..." nonincremental-search-backward
                         :help "Search backward for a string"))
(define-key menu-bar-search-menu [repeat-search-fwd]
  (menu-item-any-version
   "               Again" nonincremental-repeat-search-forward
   :enable (or (not (boundp 'menu-bar-last-search-type))
               (and (eq menu-bar-last-search-type 'string) search-ring)
               (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))
   :help "Repeat last search forward"))
(when (< emacs-major-version 21)
  (put 'nonincremental-repeat-search-foreward 'menu-enable
       '(or (not (boundp 'menu-bar-last-search-type))
         (and (eq menu-bar-last-search-type 'string) search-ring)
         (and (eq menu-bar-last-search-type 'regexp) regexp-search-ring))))

(define-key menu-bar-search-menu [search-forward]
  (menu-item-any-version "String Forward..." nonincremental-search-forward
                         :help "Search forward for a string"))


;;; SEARCH TAGS submenu.

(define-key menu-bar-search-tags-menu [set-tags-name]
  (menu-item-any-version "Set Tags File Name..." visit-tags-table
                         :help "Tell Tags commands which tag table file to use"))

(define-key menu-bar-search-tags-menu [apropos-tags]
  (menu-item-any-version "Tags Apropos..." tags-apropos :help "Find tags matching a regexp"))

(define-key menu-bar-search-tags-menu [separator-tags-misc] '("--"))
;----------------------

(define-key menu-bar-search-tags-menu [tags-continue]
  (menu-item-any-version "Continue Tags Search/Replace" tags-loop-continue
                         :help "Continue last tags search or replace operation"))
(define-key menu-bar-search-tags-menu [tags-search]
  (menu-item-any-version "Search Tagged Files..." tags-search
                         :help "Search for a regexp in all tagged files"))
(define-key menu-bar-search-tags-menu [find-tag-regexp]
  (menu-item-any-version "Find Tag Regexp..." find-tag-regexp
                         :help "Find tag that matches a regular expression"))

(define-key menu-bar-search-tags-menu [separator-tags-regexp] '("--"))
;----------------------

(defun menu-bar-next-tag-other-frame ()
  "Find the next definition of the tag already specified."
  (interactive)
  (find-tag-other-frame nil t))

(define-key menu-bar-search-tags-menu [next-tag-other-frame]
  (menu-item-any-version "Find Next Tag" menu-bar-next-tag-other-frame
                         :enable (and (boundp 'tags-location-ring)
                                      (not (ring-empty-p tags-location-ring)))
                         :help "Find next tag name"))
(when (< emacs-major-version 21)
  (put 'menu-bar-next-tag-other-frame 'menu-enable '(and (boundp 'tags-location-ring)
                                                     (not (ring-empty-p tags-location-ring)))))

(define-key menu-bar-search-tags-menu [find-tag-other-frame]
  (menu-item-any-version "Find Tag..." find-tag-other-frame
                         :help "Find tag whose name matches input string"))

;; REPLACE submenu
(define-key menu-bar-search-replace-menu [replace-regexp]
  (menu-item-any-version "       Regexp..." replace-regexp
                         :enable (not buffer-read-only)
                         :help "Replace things after cursor that match regexp"))
(when (< emacs-major-version 21) (put 'replace-regexp 'menu-enable '(not buffer-read-only)))

(define-key menu-bar-search-replace-menu [replace-string]
  (menu-item-any-version "Global String..." replace-string
                         :enable (not buffer-read-only)
                         :help "Replace string, with no confirmation"))
;;--------------------
(define-key menu-bar-search-replace-menu [separator-search-replace-global] '("--"))
(define-key menu-bar-search-replace-menu [tags-query-replace]
  (menu-item-any-version
   (substitute-command-keys
    "            Tags... (again: \\[tags-loop-continue])")
   tags-query-replace
   :help "Replace a regexp in tagged files, with confirmation"))
(define-key menu-bar-search-replace-menu [map-query-replace-regexp]
  (menu-item-any-version "            Map..." map-query-replace-regexp
                         :enable (not buffer-read-only)
                         :help "Replace regexp matches with various strings, in rotation."))
(define-key menu-bar-search-replace-menu [query-replace-regexp]
  (menu-item-any-version
   "      Regexp..." query-replace-regexp
   :enable (not buffer-read-only)
   :help "Replace regular expression interactively, ask about each occurrence"))
(define-key menu-bar-search-replace-menu [query-replace]
  (if (fboundp 'query-replace-w-options) ; Defined in `replace+.el'.
      (menu-item-any-version
       "Query String" query-replace-w-option
       :enable (not buffer-read-only)
       :help "Replace string interactively, ask about each occurrence")
    (menu-item-any-version
     "Query String" query-replace
     :enable (not buffer-read-only)
     :help "Replace string interactively, ask about each occurrence")))

(when (< emacs-major-version 21)
  (put 'replace-regexp 'menu-enable '(not buffer-read-only))
  (put 'replace-string 'menu-enable '(not buffer-read-only))
  (put 'map-query-replace-regexp 'menu-enable '(not buffer-read-only))
  (put 'query-replace-regexp 'menu-enable '(not buffer-read-only))
  (if (fboundp 'query-replace-w-options)
      (put 'query-replace-w-options 'menu-enable '(not buffer-read-only))
    (put 'query-replace 'menu-enable '(not buffer-read-only))))
;; Done in menu-bar.el:
;; (put 'query-replace 'menu-enable '(not buffer-read-only))
;; (put 'query-replace-regexp 'menu-enable '(not buffer-read-only))


;;; HELP menu.

;;; General help
(define-key menu-bar-help-menu [separator-genl-help] '("--"))
(when (fboundp 'save-*Help*-buffer)
  (define-key menu-bar-help-menu [save-*Help*-buffer]
    (menu-item-any-version "Save *Help* Buffer" save-*Help*-buffer ; In `help+.el'.
                           :help "Rename *Help* buffer as new buffer *Help*<N>, N=2,3....")))
(when (fboundp 'show-*Help*-buffer)
  (define-key menu-bar-help-menu [show-*Help*-buffer]
    (menu-item-any-version "Show *Help* Buffer" show-*Help*-buffer ; In `frame-cmds.el'
                           :help "Raise a frame showing buffer *Help*")))
(define-key menu-bar-help-menu [help-for-help]
  (menu-item-any-version "Help on Help..." help-for-help :help "Emacs main help command"))

;;; Remove some default bindings
(define-key menu-bar-help-menu [finder-by-keyword] nil)
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(when (>= emacs-major-version 21)
  (define-key menu-bar-help-menu [emacs-tutorial-language-specific] nil)
  (define-key menu-bar-help-menu [emacs-problems] nil)
  (define-key menu-bar-help-menu [sep1] nil)
  (define-key menu-bar-help-menu [emacs-manual] nil))
(define-key menu-bar-help-menu [sep2] nil)
(define-key menu-bar-help-menu [emacs-faq] nil)
(define-key menu-bar-help-menu [emacs-news] nil)



;;; Whoops!? submenu
(defvar menu-bar-whereami-menu (make-sparse-keymap "Whoops!?"))
(define-key menu-bar-help-menu [whereami]
  (cons "Whoops!?" menu-bar-whereami-menu))
(define-key menu-bar-whereami-menu [view-lossage]
  (menu-item-any-version "What did I do !?" view-lossage
                         :help "Display last 100 input keystrokes"))
(define-key menu-bar-whereami-menu [top-level]
  (menu-item-any-version "Back to Top Level" top-level
                         :help "Exit all recursive editing levels"))
(define-key menu-bar-whereami-menu [keyboard-quit]
  (menu-item-any-version "Cancel Current Action" keyboard-quit
   :help "Quit any operation in progress"))

;;; Apropos submenu
(defvar menu-bar-apropos-menu (make-sparse-keymap "Apropos"))
(define-key-after menu-bar-help-menu [apropos]
  (cons "Apropos" menu-bar-apropos-menu) 'separator-genl-help)
(define-key menu-bar-apropos-menu [apropos-doc] ; Defined in `apropos.el'.
  (menu-item-any-version
   "Symbol Descriptions..." apropos-documentation
   :help "Find functions and variables whose doc string matches a regexp"))
(define-key menu-bar-apropos-menu [apropos-tags]
  (menu-item-any-version "Tags..." tags-apropos :help "Find tags matching a regexp"))
(define-key menu-bar-apropos-menu [apropos-doc] ; Defined in `apropos.el'.
  (menu-item-any-version
   "Symbol Descriptions..." apropos-documentation
   :help "Find functions and variables whose doc string matches a regexp"))
(define-key menu-bar-apropos-menu [apropos-symbols] ; Defined in `apropos.el'
  (menu-item-any-version "Symbols..." apropos
                         :help "Find symbols whose name matches a regexp"))
(define-key menu-bar-apropos-menu [apropos-value]
  (menu-item-any-version "Variable Values..." apropos-value
                         :help "Find variables whose values match a regexp"))
(define-key menu-bar-apropos-menu [apropos-variables]
  (menu-item-any-version "All Variables..." apropos-variable
                         :help "Find variables whose name matches a regexp"))
(when (fboundp 'apropos-user-options)
  (define-key menu-bar-apropos-menu [apropos-user-options]
    (menu-item-any-version
     "User Options..." apropos-user-options
     :help "Find user options (variables you can change) whose name matches a regexp")))
(define-key menu-bar-apropos-menu [apropos-command]
  (menu-item-any-version "Commands..." apropos-command
                         :help "Find commands whose name matches a regexp"))

;;; Describe submenu
(define-key-after menu-bar-help-menu [describe]
  (cons "Describe" menu-bar-describe-menu) 'apropos)
(when (fboundp 'help-on-click/key)
  (define-key menu-bar-describe-menu [help-on-click]
    (menu-item-any-version
     "This..." help-on-click/key        ; Defined in `help+.el'.
     :help "Give help on a key/menu sequence or object clicked with the mouse")))
(define-key-after menu-bar-describe-menu [describe-mode]
  (menu-item-any-version "Buffer Modes" describe-mode
                         :help "Describe this buffer's major and minor modes")
  'help-on-click)
;; Remove this one for Emacs 21:
(define-key menu-bar-describe-menu [describe-key-1] nil)
(define-key-after menu-bar-describe-menu [describe-key]
  (menu-item-any-version
   "Key..." describe-key
   :help "Display documentation of command bound to a key or menu item")
  'describe-mode)
(define-key-after menu-bar-describe-menu [describe-function]
  (menu-item-any-version "Function..." describe-function
                         :help "Display documentation of command or other function")
  'describe-key)
(define-key-after menu-bar-describe-menu [describe-variable]
  (menu-item-any-version "Variable..." describe-variable
                         :help "Display documentation of user option or other variable")
  'describe-function)
(define-key-after menu-bar-describe-menu [list-keybindings]
  (menu-item-any-version "All Key Bindings" describe-bindings
                         :help "List all current keybindings, with brief descriptions")
  'describe-variable)
(when (fboundp 'describe-menubar)
  (define-key-after menu-bar-describe-menu [describe-menubar]
    (menu-item-any-version "Menu Bar" describe-menubar
                           :help "Explain the menu-bar, in general terms")
    'list-keybindings))
(define-key-after menu-bar-describe-menu [describe-syntax]
  (menu-item-any-version "Major Mode Syntax" describe-syntax
                         :help "Describe the syntax specifications in the syntax table")
  'describe-menubar)


;;; Manuals submenu.

;; REPLACES ORIGINAL defined in `menu-bar.el'.
;; Remove some default bindings.
;; Name changes.
(defconst menu-bar-manuals-menu (make-sparse-keymap "Learn More"))
(define-key-after menu-bar-help-menu [manuals]
  (cons "Learn More" menu-bar-manuals-menu) 'describe)
(when (>= emacs-major-version 21)
  (define-key menu-bar-manuals-menu [order-emacs-manuals]
    (menu-item-any-version "Ordering Manuals" view-order-manuals
                           :help "How to order manuals from the Free Software Foundation")))
(define-key-after menu-bar-help-menu [separator-manuals] '("--") 'manuals)
(define-key menu-bar-manuals-menu [man]
  (menu-item-any-version
   "Unix Man Page..." manual-entry
   :help "Unix man-page documentation for external commands and libraries"))
(define-key menu-bar-manuals-menu [info]
  (menu-item-any-version "All Manuals (`Info')" Info-directory
                         :help "Read any of the installed manuals"))
(define-key menu-bar-manuals-menu [last-info]
  (menu-item-any-version "Last Accessed Manual (`Info')" info
                         :help "Open Info, the doc browser, at the last doc place visited"))
(define-key menu-bar-manuals-menu [emacs-faq] nil)
(define-key menu-bar-manuals-menu [emacs-news] nil)
(define-key menu-bar-manuals-menu [key] nil)
(define-key menu-bar-manuals-menu [command] nil)


;;; Emacs Lisp submenu of Manuals submenu.
(defvar menu-bar-emacs-lisp-manual-menu (make-sparse-keymap "Emacs Lisp"))
(define-key menu-bar-manuals-menu [emacs-lisp-manual]
  (cons "Emacs Lisp" menu-bar-emacs-lisp-manual-menu))

;; Is there a direct way to get to Lisp NEWS in Emacs 21?
(when (< emacs-major-version 21)
  (define-key menu-bar-emacs-lisp-manual-menu [emacs-Lisp-News]
    (menu-item-any-version "Change History" view-emacs-lisp-news
                           :help "Display information on recent changes to Emacs Lisp")))
(define-key menu-bar-emacs-lisp-manual-menu [finder-by-keyword]
  (menu-item-any-version "Locate Libraries by Keyword" finder-by-keyword
                         :help "Find Emacs Lisp packages matching a keyword"))
(define-key menu-bar-emacs-lisp-manual-menu [locate-library] ; Defined in `help.el'.
  (menu-item-any-version "Locate Library..." locate-library
                         :help "Show the full path name of an Emacs library"))
(define-key menu-bar-emacs-lisp-manual-menu [emacs-lisp-manual-separator] '("--"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (define-key menu-bar-emacs-lisp-manual-menu [elisp-index-search]
    (menu-item-any-version "Look Up Subject..." elisp-index-search
                           :help "Search Emacs Lisp manual for a subject")))
(when (fboundp 'menu-bar-read-lispref)	; Defined in `info+.el'.
  (define-key menu-bar-emacs-lisp-manual-menu [menu-bar-read-lispref]
    (menu-item-any-version "Manual (`Info')" menu-bar-read-lispref
                           :help "Read the Emacs Lisp Reference manual"))
  (define-key menu-bar-emacs-lisp-manual-menu [info-elintro]
    (menu-item-any-version "Intro" menu-bar-read-lispintro
                           :help "Read an introduction to Emacs Lisp programming")))


;;; Emacs submenu of Manuals submenu.
(defvar menu-bar-emacs-manual-menu (make-sparse-keymap "Emacs"))
(define-key menu-bar-manuals-menu [emacs-manual]
  (cons "Emacs" menu-bar-emacs-manual-menu))
(when (>= emacs-major-version 21)
  (define-key menu-bar-emacs-manual-menu [emacs-problems]
    '(menu-item "Emacs Known Problems" view-emacs-problems)))
(define-key menu-bar-emacs-manual-menu [emacs-faq]
  (menu-item-any-version "FAQ" view-emacs-FAQ
                         :help "Read frequently asked questions about Emacs (with answers)"))
(define-key menu-bar-emacs-manual-menu [emacs-news]
  (menu-item-any-version "Change History (News)" view-emacs-news
                         :help "New features of this Emacs version"))
(define-key menu-bar-emacs-manual-menu [emacs-manual-separator] '("--"))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(when (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (define-key menu-bar-emacs-manual-menu [emacs-glossary]
    (menu-item-any-version "Emacs Terminology" search-emacs-glossary
                           :help "Display the Glossary section of the Emacs manual"))
  (define-key menu-bar-emacs-manual-menu [emacs-index-search]
    (menu-item-any-version "Look Up in Index..." emacs-index-search
                           :help "Search Emacs manual for a subject")))
(define-key menu-bar-emacs-manual-menu [key]
  (menu-item-any-version "Find Key in Manual" Info-goto-emacs-key-command-node
                         :help "Display manual section that describes a key"))
(define-key menu-bar-emacs-manual-menu [command]
  (menu-item-any-version "Find Command in Manual" Info-goto-emacs-command-node
                         :help "Display Emacs manual section that describes a command"))
(when (fboundp 'info-emacs-manual)
  (define-key menu-bar-emacs-manual-menu [info-emacs-manual]
    (menu-item-any-version "Manual (`Info')" info-emacs-manual
                           :help "Read the Emacs manual")))
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
    (define-key menu-bar-emacs-manual-menu [emacs-tutorial-language-specific]
      '(menu-item "Tutorial (choose language)..."
                  help-with-tutorial-spec-language
                  :help "Learn how to use Emacs (choose a language)"))
  (define-key menu-bar-emacs-manual-menu [emacs-tutorial]
    (menu-item-any-version "Tutorial" help-with-tutorial
                           :help "Learn Emacs with a hands-on tutorial")))


;;; OPTIONS menu.

(defmacro menu-bar-make-toggle-any-version (name variable doc message help &rest body)
  "Return a valid `menu-bar-make-toggle' call in Emacs 20 or later.
NAME is the name of the toggle command to define.
VARIABLE is the variable to set.
DOC is the menu-item name.
MESSAGE is the toggle message, minus status.
HELP is :help string.
BODY is the function body to use.  If present, it is responsible for
setting the variable and displaying a status message (not MESSAGE)."
  (if (< emacs-major-version 21)
      `(menu-bar-make-toggle ,name ,variable ,doc ,message ,@body)
    `(menu-bar-make-toggle ,name ,variable ,doc ,message ,help ,@body)))

(when (or (boundp 'doremi-push-frame-config-for-cmds-flag) (boundp 'inhibit-fit-frame-flag)
          (boundp 'autofit-frames-flag) (boundp 'thumbify-instead-of-iconify-flag))
  (define-key menu-bar-options-menu [frames-separator] '("--")))
(when (boundp 'doremi-push-frame-config-for-cmds-flag)
  (define-key menu-bar-options-menu [doremi-push-frame-config]
    (menu-bar-make-toggle-any-version menu-bar-doremi-push-frame-config
                          doremi-push-frame-config-for-cmds-flag
                          "Save Frame Configs (DoReMi)"
                          "Saving frame configurations is %s for DoReMi commands"
                          "Saving of frame configurations by DoReMi commands")))
(when (boundp 'inhibit-fit-frame-flag)
  (define-key menu-bar-options-menu [inhibit-fit-frame]
    (menu-bar-make-toggle-any-version menu-bar-inhibit-fit-frame inhibit-fit-frame-flag
                          "Inhibit Frame Fitting"
                          "Inhibit frame fitting is %s (overrides automatic frame fitting)"
                          "Inhibit frame fitting")))
(when (boundp 'autofit-frames-flag)
  (define-key menu-bar-options-menu [autofit-frames]
    (menu-bar-make-toggle-any-version menu-bar-autofit-frames autofit-frames-flag
                          "Fit Frames Automatically"
                          "Automatic fitting of one-window frames is %s"
                          "Automatic fitting of one-window frames")))
(when (boundp 'thumbify-instead-of-iconify-flag)
  (define-key menu-bar-options-menu [thumbify-frames]
    (menu-bar-make-toggle-any-version menu-bar-thumbify-frames thumbify-instead-of-iconify-flag
                          "Thumbify, Don't Iconify, Frames"
                          "Thumbifying instead of iconifying frames is %s"
                          "Thumbifying instead of iconifying frames")))

(define-key menu-bar-options-menu [all-options-separator] '("--"))
(define-key menu-bar-options-menu [edit-options]
  (menu-item-any-version "Show, Edit All Options" edit-options
                         :help "Edit a list of Emacs user option (variable) values"))

(when (boundp 'replace-w-completion-flag)
  (define-key-after menu-bar-options-menu [replace-w-completion-flag]
    (menu-bar-make-toggle-any-version menu-bar-toggle-replace-w-completion
                                      replace-w-completion-flag
                          "Completion for Query Replace"
                          "Using completion with query replace is %s"
                          "Using completion with query replace")
    'case-fold-search))

;; Can't seem to byte-compile this - try to debug later.
;; (when (and (< emacs-major-version 21) (featurep 'icomplete)) ; Not needed/available for 21?
;;   (define-key-after menu-bar-options-menu [icomplete-mode]
;;     (menu-bar-make-toggle toggle-icomplete-mode icomplete-mode
;;                           "Command Completion Clues" "Completion Clues %s")
;;     'all-options-separator))



;; REPLACES ORIGINAL in `menu-bar.el'.
;; Updated for added items.
;; For this to work, however, compilation needs to be with Emacs >= 21,
;; to use the right definition of `menu-bar-make-toggle'.
;;
(cond ((= emacs-major-version 21)
       (defun menu-bar-options-save ()
         "Save current values of Options menu items using Custom."
         (interactive)
         (dolist (elt '(debug-on-quit debug-on-error auto-compression-mode
                        case-fold-search truncate-lines show-paren-mode
                        transient-mark-mode global-font-lock-mode
                        current-language-environment default-input-method
                        ;; D. ADAMS: Added these options.
                        doremi-push-frame-config-for-cmds-flag
                        inhibit-fit-frame-flag autofit-frames-flag
                        thumbify-instead-of-iconify-flag replace-w-completion-flag))
           (if (default-value elt)
               (customize-save-variable elt (default-value elt))))
         (if (memq 'turn-on-auto-fill text-mode-hook)
             (customize-save-variable 'text-mode-hook
                                      (default-value 'text-mode-hook)))
         (if (featurep 'saveplace)
             (customize-save-variable 'save-place (default-value 'save-place)))
         (if (featurep 'uniquify)
             (customize-save-variable 'uniquify-buffer-name-style
                                      (default-value 'uniquify-buffer-name-style)))))
      ((> emacs-major-version 21)
       (defun menu-bar-options-save ()
         "Save current values of Options menu items using Custom."
         (interactive)
         (let ((need-save nil))
           ;; These are set with menu-bar-make-mm-toggle, which does not
           ;; put on a customized-value property.
           (dolist (elt '(line-number-mode column-number-mode size-indication-mode
                          cua-mode show-paren-mode transient-mark-mode
                          blink-cursor-mode display-time-mode display-battery-mode))
             (and (customize-mark-to-save elt)
                  (setq need-save t)))
           ;; These are set with `customize-set-variable'.
           (dolist (elt '(scroll-bar-mode
                          debug-on-quit debug-on-error
                          tooltip-mode menu-bar-mode tool-bar-mode
                          save-place uniquify-buffer-name-style fringe-mode
                          indicate-empty-lines indicate-buffer-boundaries
                          case-fold-search
                          current-language-environment default-input-method
                          ;; D. ADAMS: Added these options.
                          doremi-push-frame-config-for-cmds-flag
                          inhibit-fit-frame-flag autofit-frames-flag
                          thumbify-instead-of-iconify-flag replace-w-completion-flag
                          ;; Saving `text-mode-hook' is somewhat questionable,
                          ;; as we might get more than we bargain for, if
                          ;; other code may has added hooks as well.
                          ;; Nonetheless, not saving it would like be confuse
                          ;; more often.
                          ;; -- Per Abrahamsen <abraham@dina.kvl.dk> 2002-02-11.
                          text-mode-hook))
             (and (get elt 'customized-value)
                  (customize-mark-to-save elt)
                  (setq need-save t)))
           ;; Save if we changed anything.
           (when need-save
             (custom-save-all))))))



;;;@@@Emacs19 (autoload 'show-calendar "calendar+"
;;;@@@Emacs19   "Show *Calendar* buffer, generating it if not already present." t)
;;;@@@Emacs19 (autoload 'calendar "calendar+"
;;;@@@Emacs19   "Display a 3-month calendar in another window." t)
;;;@@@Emacs19  ;; Autoloaded from `calendar+.el': calendar, show-calendar

;;; ---------------------------

;;;@@@Emacs19 (defvar menu-bar-help-menu (make-sparse-keymap "Help")) ; Wipe out original.
;;;@@@Emacs19 (define-key global-map [menu-bar help-menu] (cons "?" menu-bar-help-menu))


;;; TOOLS menu.
;;;@@@Emacs19 ;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar tools separator-print])
;;;@@@Emacs19 (global-unset-key [menu-bar tools ps-print-region])
;;;@@@Emacs19 (global-unset-key [menu-bar tools ps-print-buffer])
;;;@@@Emacs19 (global-unset-key [menu-bar tools print-region])
;;;@@@Emacs19 (global-unset-key [menu-bar tools print-buffer])
;;;@@@Emacs19
;;;@@@Emacs19 (defvar menu-bar-print-menu (make-sparse-keymap "Print"))
;;;@@@Emacs19 (define-key menu-bar-tools-menu [print] (cons "Print" menu-bar-print-menu))
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [separator-print] '("--") 'print)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [separator-cal] '("--") 'rmail)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [show-calendar]
;;;@@@Emacs19                 '("Show Calendar" . show-calendar) 'separator-cal)
;;;@@@Emacs19 (define-key-after menu-bar-tools-menu [calendar]
;;;@@@Emacs19                 '("Calendar and Reminders" . calendar) 'calendar)

;;;@@@Emacs19 ;; PRINT submenu of TOOLS
;;;@@@Emacs19 ;; The `*-declp-*' commands are defined in `misc-cmds.el'.
;;;@@@Emacs19 (define-key menu-bar-print-menu [ps-print-region]
;;;@@@Emacs19   '("Region as Postscript" . ps-print-region-with-faces))
;;;@@@Emacs19 ;(put 'declp-region-w-switches 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [declp-region-w-switches]
;;;@@@Emacs19 ;  '("Region with Switches..." . declp-region-w-switches))
;;;@@@Emacs19 ;(put 'pr-declp-region 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-paged-region]
;;;@@@Emacs19 ;  '("Paged Region..." . pr-declp-region))
;;;@@@Emacs19 ;(put 'declp-region 'menu-enable 'mark-active)
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-region]
;;;@@@Emacs19 ;  '("Region..." . declp-region))
;;;@@@Emacs19 ;;--------------------
;;;@@@Emacs19 (define-key menu-bar-print-menu [separator-print-buffer] '("--"))
;;;@@@Emacs19 (define-key menu-bar-print-menu [ps-print-buffer]
;;;@@@Emacs19   '("Buffer as Postscript" . ps-print-buffer-with-faces))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [declp-buffer-w-switches]
;;;@@@Emacs19 ;  '("Buffer with Switches..." . declp-buffer-w-switches))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-paged-buffer]
;;;@@@Emacs19 ;  '("Paged Buffer..." . pr-declp-buffer))
;;;@@@Emacs19 ;(define-key menu-bar-print-menu [print-buffer]
;;;@@@Emacs19 ;  '("Buffer..." . declp-buffer))

;;;@@@Emacs19 ;;; VERSION CONTROL submenu of TOOLS
;;;@@@Emacs19 (when (boundp 'vc-menu-map)
;;;@@@Emacs19 ;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar tools vc separator1])
;;;@@@Emacs19 (global-unset-key [menu-bar tools vc separator2])
;;;@@@Emacs19 (define-key vc-menu-map [vc-status-here] '("Files Here" . vc-status-here)))
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-directory] '("Files Below" . vc-directory)
;;;@@@Emacs19                   'vc-status-here)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-dir-separator1] '("--") 'vc-directory)
;;;@@@Emacs19 (define-key-after vc-menu-map [ediff-revision]
;;;@@@Emacs19                   '("Compare with Version..." . vc-ediff) 'vc-dir-separator1)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-diff]
;;;@@@Emacs19              '("Compare Last Version using Diff" . vc-diff) 'ediff-revision)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-version-other-window]
;;;@@@Emacs19   '("Show Other Version..." . vc-version-other-window) 'vc-diff)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-dir-separator2] '("--")
;;;@@@Emacs19                   'vc-version-other-window)
;;;@@@Emacs19 (define-key-after vc-menu-map [vc-rename-file]
;;;@@@Emacs19                   '("Rename File..." . vc-rename-file)
;;;@@@Emacs19   'vc-register)
;;;@@@Emacs19 (define-key vc-menu-map [vc-check-out] '("Check In/Out" . vc-toggle-read-only))
;;;@@@Emacs19
;;;@@@Emacs19 (put 'vc-diff 'menu-enable
;;;@@@Emacs19      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-ediff 'menu-enable
;;;@@@Emacs19                '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-version-other-window 'menu-enable
;;;@@@Emacs19                  '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-toggle-read-only 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs19 (put 'vc-insert-headers 'menu-enable '(or vc-mode vc-dired-mode))
;;;@@@Emacs19 ;;; vc-dired-mode is OK (e.g. Unregistered).
;;;@@@Emacs19 (put 'vc-register 'menu-enable '(not vc-mode))
;;;@@@Emacs19 (put 'vc-rename-file 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-revert-buffer 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-cancel-version 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-print-log 'menu-enable
;;;@@@Emacs19                      '(or vc-mode vc-dired-mode (eq 'dired-mode major-mode)))
;;;@@@Emacs19 (put 'vc-update-change-log 'menu-enable
;;;@@@Emacs19  '(or (eq (vc-buffer-backend) 'RCS) vc-dired-mode (eq 'dired-mode major-mode)))


;;; EDIT menu
;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar edit separator-edit])

;;;@@@Emacs19 (define-key menu-bar-edit-menu [undo] '("Undo" . advertised-undo))
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [separator-edit-undo] '("--") 'undo)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [cut]
;;;@@@Emacs19                   '("Cut" . kill-region) 'separator-edit-undo)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [copy]
;;;@@@Emacs19                   '("Copy" . menu-bar-kill-ring-save) 'cut)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [paste] '("Paste" . yank) 'copy)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [select-paste]
;;;@@@Emacs19                   '("Select and Paste" . yank-menu)
;;;@@@Emacs19   'paste)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [clear]
;;;@@@Emacs19                   '("Clear" . delete-region) 'select-paste)


;;;@@@Emacs19 (defalias 'menu-bar-edit-fill-menu (symbol-value 'menu-bar-edit-fill-menu))

;;;@@@Emacs19 (when (fboundp 'start-process)
;;;@@@Emacs19 (define-key-after menu-bar-edit-menu [spell]
;;;@@@Emacs19                   '("Spell" . ispell-menu-map) 'sort))


;; Remove some default bindings.
;;;@@@Emacs19 (global-unset-key [menu-bar help emacs-news])
;;;@@@Emacs19 (global-unset-key [menu-bar help emacs-faq])

;;;@@@Emacs19 (define-key menu-bar-help-menu [help-on-click]
;;;@@@Emacs19   '("What's This?..." . help-on-click/key))       ; Defined in `help+.el'.
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [help-for-help]
;;;@@@Emacs19   '("Help on Help..." . help-for-help) 'help-on-click)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-menubar]
;;;@@@Emacs19   '("Describe Menu Bar" . describe-menubar) 'help-for-help)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-help-click] '("--")
;;;@@@Emacs19   'view-lossage)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-mode]
;;;@@@Emacs19   '("Describe Mode" . describe-mode) 'separator-help-click)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-syntax]
;;;@@@Emacs19   '("Describe Mode Syntax" . describe-syntax) 'describe-mode)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-variable]
;;;@@@Emacs19   '("Describe Variable..." . describe-variable) 'describe-syntax)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [edit-options]
;;;@@@Emacs19   '("Show/Set Variables" . edit-options) 'describe-variable)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-function]
;;;@@@Emacs19   '("Describe Function..." . describe-function) 'edit-options)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [where-is]
;;;@@@Emacs19   '("Where is Command..." . where-is) 'describe-function)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [Info-goto-emacs-command-node]
;;;@@@Emacs19   '("`Info' on Command..." . Info-goto-emacs-command-node) 'where-is)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [describe-key]
;;;@@@Emacs19   '("Describe Key/Menu..." . describe-key) 'Info-goto-emacs-command-node)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [Info-goto-emacs-key-command-node]
;;;@@@Emacs19   '("`Info' on Key/Menu..." . Info-goto-emacs-key-command-node) 'describe-key)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [list-keybindings]
;;;@@@Emacs19   '("Show Key/Menu Bindings" . describe-bindings)
;;;@@@Emacs19   'Info-goto-emacs-key-command-node)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-manuals] '("--")
;;;@@@Emacs19   'apropos-doc%%%%%%%%%%%%%)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [info] '("`Info' (online manuals)" . info)
;;;@@@Emacs19   'separator-manuals)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [man]
;;;@@@Emacs19   '("Unix Manual..." . manual-entry) 'info)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-emacs] '("--") 'man)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-tutorial]
;;;@@@Emacs19   '("Emacs Tutorial" . help-with-tutorial) 'separator-emacs)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-faq]
;;;@@@Emacs19   '("Emacs FAQ" . view-emacs-FAQ) 'emacs-tutorial)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-news]
;;;@@@Emacs19   '("Emacs Changes" . view-emacs-news) 'emacs-faq)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-lispdoc] '("--")
;;;@@@Emacs19   'emacs-news)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [finder-by-keyword]
;;;@@@Emacs19   '("Lisp Libraries by Keyword" . finder-by-keyword) ; Defined in `finder.el'.
;;;@@@Emacs19   'separator-lispdoc)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [locate-library] ; Defined in `help.el'.
;;;@@@Emacs19   '("Locate Lisp Library..." . locate-library) 'finder-by-keyword)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-Lisp-News]
;;;@@@Emacs19   '("Emacs Lisp Changes" . view-emacs-lisp-news) ; Defined in `help.el'.
;;;@@@Emacs19   'locate-library)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-about] '("--")
;;;@@@Emacs19   'emacs-Lisp-News)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [emacs-version]
;;;@@@Emacs19   '("Show Version" . emacs-version) 'separator-about)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [report-emacs-bug]
;;;@@@Emacs19   '("Send Bug Report..." . report-emacs-bug) 'emacs-version)
;;--------------------
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [separator-help-buffer] '("--")
;;;@@@Emacs19   'report-emacs-bug)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [save-*Help*-buffer]
;;;@@@Emacs19   '("Save *Help* Buffer" . save-*Help*-buffer) 'separator-help-buffer)
;;;@@@Emacs19 (define-key-after menu-bar-help-menu [show-*Help*-buffer]
;;;@@@Emacs19   '("Show *Help* Buffer" . show-*Help*-buffer) 'save-*Help*-buffer)

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'menu-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; menu-bar+.el ends here

