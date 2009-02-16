;;; isearch+.el --- Extensions to `isearch.el'.
;;
;; Filename: isearch+.el
;; Description: Extensions to `isearch.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Fri Dec 15 10:44:14 1995
;; Version: 21.0
;; Last-Updated: Sun Jul 30 10:44:33 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 240
;; URL: http://www.emacswiki.org/cgi-bin/wiki/isearch+.el
;; Keywords: help, matching, internal, local
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `isearch.el'.
;;
;;  Commands defined here:
;;
;;    `isearchp-toggle-set-region', `set-region-around-search-target'.
;;
;;  Non-interactive functions defined here:
;;
;;    `isearchp-set-region'.
;;
;;  User options defined here:
;;
;;    `isearchp-set-region-flag'.
;;
;;
;;  ***** NOTE: The following function defined in `isearch.el' has
;;              been REDEFINED HERE:
;;
;;  `isearch-mode-help' - Ends isearch.  Lists bindings.
;;
;;
;;  The following bindings are made here for incremental search mode
;;  (`C-s' prefix):
;;
;;    `C-SPC'      `isearchp-toggle-set-region'
;;    `C-c'        `isearch-toggle-case-fold'
;;    `C-h'        `isearch-mode-help'
;;    `C-j'        `isearch-edit-string'
;;    `C-t'        `isearch-toggle-regexp'
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `isearch.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "isearch" '(require 'isearch+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/07/30 dadams
;;     Added: set-region-around-search-target.
;; 2006/07/29 dadams
;;     Added: isearchp-toggle-set-region, isearchp-set-region(-flag).  Thx to Andreas Roehler.
;; 2006/01/24 dadams
;;     On MS Windows, bind isearch-complete* to C-tab.
;; 1999/03/17 dadams
;;     Updated to corrspond to Emacs 34.1 version.
;; 1996/04/24 dadams
;;     Added redefinition of isearch-search.  Require cl.el.
;; 1995/12/28 dadams
;;     Changed isearch-edit-string binding.
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

 ;; Cannot do (require 'isearch), because `isearch.el' does no `provide'.
 ;; Don't want to do a (load-library "isearch") either, because it wouldn't
 ;; allow doing (eval-after-load "isearch" '(progn (require 'isearch+)))

;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom isearchp-set-region-flag nil
  "Non-nil means set region around search target.
You can toggle this with `isearchp-toggle-set-region', bound to
`C-SPC' during isearch."
  :type 'boolean :group 'isearch)

(add-hook 'isearch-mode-end-hook 'isearchp-set-region)
(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(control ? )] 'isearchp-toggle-set-region)
            (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
            (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
            (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
            (define-key isearch-mode-map "\C-j" 'isearch-edit-string)
            (when (eq system-type 'windows-nt) ; Windows uses Meta-TAB for other
              (define-key isearch-mode-map [M-C-tab] 'isearch-complete)
              (define-key minibuffer-local-isearch-map [M-C-tab]
                'isearch-complete-edit))))

(defun isearchp-set-region ()
  "Set the region around the search target."
  (when isearchp-set-region-flag (push-mark isearch-other-end t 'activate)))

(defun isearchp-toggle-set-region ()
  "Toggle `isearchp-set-region-flag'."
  (interactive)
  (setq isearchp-set-region-flag (not isearchp-set-region-flag))
  (if isearchp-set-region-flag
      (message "Setting region around search target is now ON")
    (message "Setting region around search target is now OFF")))

(defun set-region-around-search-target ()
  "Set the region around the last search or query-replace target."
  (interactive)
  (case last-command
    ((isearch-forward isearch-backward isearch-forward-regexp isearch-backward-regexp)
     (push-mark isearch-other-end t 'activate))
    (t (push-mark (match-beginning 0) t 'activate)))
  (setq deactivate-mark nil))



;; An alternative to binding `isearch-edit-string' (but less flexible):
;; (setq search-exit-option 'edit) ; M- = edit search string, not exit.


;; REPLACES ORIGINAL in `isearch.el':
;; 1. Ends isearch: does `isearch-done' and `isearch-clean-overlays'
;;    instead of `isearch-update'.
;; 2. Lists isearch bindings too.
;;;###autoload
(defun isearch-mode-help ()
  "Display information on interactive search in buffer *Help*."
  (interactive)
  (describe-function 'isearch-forward)
  (isearch-done)
  (isearch-clean-overlays)
  (save-excursion
    (set-buffer "*Help*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert (substitute-command-keys "

Bindings in Isearch minor mode:
------------------------------

\\{isearch-mode-map}")))))


;;;(require 'cl) ;; when, unless, cadr

;;;;; REPLACES ORIGINAL in `isearch.el':
;;;;; 1. Prevent null `isearch-string' from giving wrong-type-arg error.
;;;;;    This fixes a bug: C-M-s M-p C-s with no previous regexp search.
;;;;; 2. The general `error' handler shows the whole error message to
;;;;;    user (in `isearch-invalid-regexp').  The original version showed
;;;;;    just (cadr lossage).
;;;;;;###autoload
;;;(defun isearch-search ()
;;;  ;; Do the search with the current search string.
;;;  (isearch-message nil t)
;;;  (when (and (eq isearch-case-fold-search t) search-upper-case)
;;;    (setq isearch-case-fold-search
;;;          (isearch-no-upper-case-p isearch-string isearch-regexp)))
;;;  (condition-case lossage
;;;      (let ((inhibit-quit nil)
;;;	    (case-fold-search isearch-case-fold-search))
;;;	(when isearch-regexp (setq isearch-invalid-regexp nil))
;;;	(setq isearch-within-brackets nil)
;;;        ;; Prevent a null isearch-string from giving a wrong-type-arg error.
;;;        (setq isearch-string (or isearch-string ""))
;;;	(setq isearch-success
;;;	      (funcall (cond (isearch-word (if isearch-forward
;;;                                               'word-search-forward
;;;                                             'word-search-backward))
;;;                             (isearch-regexp (if isearch-forward
;;;                                                 're-search-forward
;;;                                               're-search-backward))
;;;                             (t (if isearch-forward 'search-forward
;;;                                  'search-backward)))
;;;                       isearch-string nil t))
;;;	(setq isearch-just-started nil)
;;;	(when isearch-success (setq isearch-other-end (if isearch-forward
;;;                                                          (match-beginning 0)
;;;                                                        (match-end 0)))))
;;;    (quit (isearch-unread ?\C-g) (setq isearch-success nil))
;;;    (invalid-regexp
;;;     (setq isearch-invalid-regexp (cadr lossage))
;;;     (setq isearch-within-brackets (string-match "\\`Unmatched \\["
;;;						 isearch-invalid-regexp))
;;;     (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid "
;;;                         isearch-invalid-regexp)
;;;       (setq isearch-invalid-regexp "incomplete input")))
;;;    ;; Stack overflow in regexp search.  (original comment)
;;;    ;; The original version set `isearch-invalid-regexp' to: (cadr lossage).
;;;    ;; But this handler catches all errors, not just regexp stack overflow,
;;;    ;; so it may be best to show the whole error message to the user (in
;;;    ;; `isearch-invalid-regexp').
;;;    (error (setq isearch-invalid-regexp (format "%s" lossage))))
;;;  (unless isearch-success
;;;    ;; Ding if failed this time after succeeding last time.
;;;    (when (nth 3 (car isearch-cmds)) (ding))
;;;    (goto-char (nth 2 (car isearch-cmds)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'isearch+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch+.el ends here
