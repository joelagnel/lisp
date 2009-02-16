;;; icicles-mcmd.el --- Minibuffer commands for Icicles
;;
;; Filename: icicles-mcmd.el
;; Description: Minibuffer commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Jan 14 10:46:38 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 6942
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mcmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `hexrgb', `icicles-fn',
;;   `icicles-opt', `icicles-var', `pp', `pp+', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  commands to be used mainly in the minibuffer or buffer
;;  *Completions* (and a few non-interactive functions used in those
;;  commands).  For top-level commands, see `icicles-cmd.el'.  For
;;  Icicles documentation, see `icicles.el' .
;;
;;  Commands defined here:
;;
;;    `icicle-abort-minibuffer-input',
;;    `icicle-add/update-saved-completion-set',
;;    `icicle-all-candidates-action', `icicle-apropos-complete',
;;    `icicle-apropos-complete-and-exit',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-candidate-action',
;;    `icicle-candidate-read-fn-invoke',
;;    `icicle-candidate-set-complement',
;;    `icicle-candidate-set-define',
;;    `icicle-candidate-set-difference',
;;    `icicle-candidate-set-intersection',
;;    `icicle-candidate-set-retrieve',
;;    `icicle-candidate-set-retrieve-from-cache-file',
;;    `icicle-candidate-set-retrieve-from-variable',
;;    `icicle-candidate-set-save',
;;    `icicle-candidate-set-save-to-cache-file',
;;    `icicle-candidate-set-save-to-variable',
;;    `icicle-candidate-set-swap', `icicle-candidate-set-truncate',
;;    `icicle-candidate-set-union', `icicle-completion-help',
;;    `icicle-Completions-mouse-3-menu',
;;    `icicle-delete-backward-char', `icicle-delete-char',
;;    `icicle-delete-windows-on', `icicle-describe-file',
;;    `icicle-digit-argument', `icicle-dispatch-C-^',
;;    `icicle-dispatch-C-.', `icicle-erase-minibuffer',
;;    `icicle-erase-minibuffer-or-history-element',
;;    `icicle-exit-minibuffer', `icicle-help-on-candidate',
;;    `icicle-help-on-next-apropos-candidate',
;;    `icicle-help-on-previous-apropos-candidate',
;;    `icicle-help-on-next-prefix-candidate',
;;    `icicle-help-on-previous-prefix-candidate', `icicle-history',
;;    `icicle-insert-completion', `icicle-insert-string-at-point',
;;    `icicle-insert-string-from-variable', `icicle-isearch-complete',
;;    `icicle-keep-only-past-inputs', `icicle-kill-line',
;;    `icicle-kill-paragraph', `icicle-kill-region',
;;    `icicle-kill-region-wimpy', `icicle-kill-sentence',
;;    `icicle-kill-sexp', `icicle-kill-word',
;;    `icicle-minibuffer-complete-and-exit',
;;    `icicle-mouse-candidate-action',
;;    `icicle-mouse-candidate-read-fn-invoke',
;;    `icicle-mouse-choose-completion',
;;    `icicle-mouse-help-on-candidate',
;;    `icicle-move-to-next-completion',
;;    `icicle-move-to-previous-completion',
;;    `icicle-narrow-candidates', `icicle-negative-argument',
;;    `icicle-next-apropos-candidate',
;;    `icicle-next-apropos-candidate-action',
;;    `icicle-next-candidate-per-mode', `icicle-next-line',
;;    `icicle-next-prefix-candidate',
;;    `icicle-next-prefix-candidate-action',
;;    `icicle-pp-eval-expression', `icicle-prefix-complete',
;;    `icicle-prefix-complete-no-display',
;;    `icicle-prefix-word-complete',
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-candidate-per-mode', `icicle-previous-line',
;;    `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-remove-Completions-window',
;;    `icicle-retrieve-last-input', `icicle-scroll-Completions',
;;    `icicle-self-insert', `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions',
;;    `icicle-switch-to/from-minibuffer',
;;    `icicle-toggle-~-for-home-dir',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-case-sensitivity',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-search-cleanup',
;;    `icicle-toggle-sorting', `icicle-toggle-transforming',
;;    `icicle-transpose-chars', `icicle-transpose-sexps',
;;    `icicle-transpose-words', `icicle-universal-argument',
;;    `icicle-universal-argument-minus',
;;    `icicle-universal-argument-more',
;;    `icicle-universal-argument-other-key', `icicle-yank',
;;    `icicle-yank-pop', `old-exit-minibuffer',
;;    `old-minibuffer-complete-and-exit', `old-switch-to-completions',
;;    `toggle-icicle-~-for-home-dir',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-case-sensitivity',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-search-cleanup',
;;    `toggle-icicle-sorting', `toggle-icicle-transforming'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-apply-to-saved-candidate', `icicle-apropos-complete-1',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-help-on-candidate-symbol', `icicle-insert-input',
;;    `icicle-insert-thing', `icicle-isearch-resume',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-prefix-complete-1', `icicle-raise-Completions-frame',
;;    `icicle-retrieve-candidates-from-set', `icicle-signum',
;;    `icicle-transform-multi-completion',
;;    `icicle-transform-sole-candidate'.
;;
;;  Internal variables defined here:
;;
;;    `overriding-map-is-bound', `saved-overriding-map'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;
;;
;;  ***** NOTE: The following function defined in `mouse.el' has
;;              been REDEFINED HERE:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 20007/01/14 dadams
;;      Added: icicle-transform-multi-completion, icicle-transform-sole-candidate.
;;      icicle-(apropos|prefix)-complete-1: Use icicle-transform-sole-candidate.  Thx to Rubikitch.
;; 2007/01/13 dadams
;;     Added: icicle-describe-file, icicle-help-on-candidate-symbol.
;;     icicle-help-on-candidate:
;;       If existing symbol, describe it.  Else if buffer or file, describe it.
;;       Otherwise, convert string to symbol and describe it.  Use icicle-help-on-candidate-symbol.  
;; 2007/01/10 dadams
;;     icicle-switch-to/from-minibuffer: Error message if minibuffer is not active.
;; 2007/01/06 dadams
;;     icicle-(apropos|prefix)-complete-1:
;;       expand-file-name -> icicle-abbreviate-or-expand-file-name.
;;     Added: icicle-toggle-~-for-home-dir.
;;     icicle-prefix-complete-1: Set icicle-default-directory only if also icicle-file-name-input-p
;; 2007/01/01 dadams
;;     icicle-add/update-saved-completion-set: Use icicle-assoc-delete-all, not delete of assoc.
;;     Runtime, not compile-time, require of icicles-var.el, icicles-opt.el.
;; 2006/12/29 dadams
;;     icicle-insert-string-at-point:
;;       Treat nil return of alternative text-grabbing function.
;;       Echo the text-grabbing function when icicle-default-thing-insertion = alternatives.
;;     icicle-ensure-overriding-map-is-bound: Bug fix: Separate treatment for diff Emacs versions.
;; 2006/12/25 dadams
;;     icicle-keep-only-past-inputs:
;;       Added optional recent-first arg: Use icicle-most-recent-first-p as sort function.
;;       Update candidates list if repeat.  Do not scroll *Completions*; update it unconditionally.
;;     Added: icicle-candidate-set-truncate.
;;     Uncommented describe-mode code, since RMS fixed Emacs bug that caused infinite recursion.
;; 2006/12/24 dadams
;;     Added: icicle-Completions-mouse-3-menu.
;; 2006/12/23 dadams
;;     icicle-narrow-candidates: Bug fix: Treat file-name completion with read-file-name.
;;     icicle-help-on-candidate: Call non-nil icicle-candidate-help-fn on candidate.
;; 2006/12/18 dadams
;;     icicle-apply-to-saved-candidate: Remove print arg and use current-prefix-arg instead.
;;     icicle-ensure-overriding-map-is-bound: Protect overriding-map-is-bound with boundp.
;;     Bug fix for Emacs 21: protect help-xref with get type button-category-symbol.
;; 2006/12/17 dadams
;;     Added: icicle(-mouse)-candidate-read-fn-invoke, icicle-apply-to-saved-candidate.
;; 2006/12/10 dadams
;;     Created from minibuffer and *Completions* commands in icicles-cmd.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (and (< emacs-major-version 20) (require 'cl))) ;; when, unless

(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-universal-argument-map,
  ;; icicle-completion-candidates, icicle-completion-help-string,
  ;; icicle-current-completion-candidate-overlay, icicle-current-completion-mode,
  ;; icicle-current-input, icicle-current-raw-input, icicle-default-directory,
  ;; icicle-default-thing-insertion-flipped-p, icicle-icompleting-p, icicle-ignored-extensions,
  ;; icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-insert-string-at-pt-end, `icicle-insert-string-at-pt-start,
  ;; icicle-last-completion-candidate, icicle-last-completion-command, icicle-last-input,
  ;; icicle-last-sort-function, icicle-last-transform-function, icicle-menu-items-alist,
  ;; icicle-nb-of-other-cycle-candidates, icicle-pre-minibuffer-buffer,
  ;; icicle-saved-candidates-variables-obarray, icicle-saved-completion-candidates,
  ;; icicle-saved-ignored-extensions, icicle-successive-grab-count, icicle-thing-at-pt-fns-pointer,
  ;; icicle-universal-argument-map, icicle-variable-history
(require 'icicles-opt)
  ;; icicle-alternative-sort-function, icicle-Completions-frame-at-right-flag, 
  ;; icicle-cycling-respects-completion-mode-flag, icicle-default-thing-insertion,
  ;; icicle-expand-input-to-common-match-flag, icicle-ignore-space-prefix-flag,
  ;; icicle-incremental-completion-flag, icicle-input-string, icicle-key-descriptions-use-<>-flag,
  ;; icicle-regexp-quote-flag, icicle-reminder-prompt-flag, icicle-saved-completion-sets, 
  ;; icicle-search-cleanup-flag, icicle-search-highlight-all-current-flag, icicle-sort-function,
  ;; icicle-TAB-shows-candidates-flag, icicle-thing-at-point-functions, icicle-transform-function

(require 'icicles-fn) ;; icicle-assoc-delete-all

(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression

;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(when (< emacs-major-version 22)
  (defvar overriding-map-is-bound)
  (defvar saved-overriding-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands -----------------------------------------------


;;; Redefined standard commands.............................


;;; REPLACE ORIGINAL `exit-minibuffer' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
(or (fboundp 'old-exit-minibuffer)
(fset 'old-exit-minibuffer (symbol-function 'exit-minibuffer)))

;;;###autoload
(defun icicle-exit-minibuffer ()        ; Bound to `C-m' (`RET') and `\n' in minibuffer.
  "Terminate this minibuffer argument.  Removes *Completions* window."
  (interactive)
  (icicle-remove-Completions-window)
  (old-exit-minibuffer))


;;; REPLACE ORIGINAL `minibuffer-complete-and-exit' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
(or (fboundp 'old-minibuffer-complete-and-exit)
(fset 'old-minibuffer-complete-and-exit (symbol-function 'minibuffer-complete-and-exit)))

;;;###autoload
(defun icicle-minibuffer-complete-and-exit ()
                                   ;; Bound to `C-m', `\n' in `minibuffer-local-must-match-map'.
  "If the minibuffer contents is a valid completion, then exit.
Otherwise try to complete it.  If completion leads to a valid completion,
a repetition of this command will exit.
Removes *Completions* window."
  (interactive)
  (save-excursion (icicle-remove-Completions-window))
  (old-minibuffer-complete-and-exit))


;;; REPLACE ORIGINAL `mouse-choose-completion' in `mouse.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Return the number of the completion.
;;;
(or (fboundp 'old-mouse-choose-completion)
(fset 'old-mouse-choose-completion (symbol-function 'mouse-choose-completion)))

;;;###autoload
(defun icicle-mouse-choose-completion (event) ; Bound to `mouse-2' in `completion-list-mode-map'.
  "Click a completion candidate in buffer `*Completions*', to choose it.
Returns the number of the candidate - 0 for first, 1 for second, ..."
  (interactive "e")
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let* ((buffer (window-buffer))
         (orig-buffer buffer)
         choice base-size)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (unless beg (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face) (point-max))
                choice (buffer-substring beg end)))))
    (if (eq orig-buffer (get-buffer "*Completions*"))
        (icicle-remove-Completions-window)
      (save-selected-window (icicle-remove-Completions-window)))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (posn-point (event-start event))))
    (choose-completion-string choice buffer base-size)))

(defun icicle-nb-of-candidate-in-Completions (position)
  "Return number of completion candidate at POSITION in *Completions*.
POSITION is a buffer position."
  (let ((compl-buf (get-buffer "*Completions*")))
    (unless compl-buf (error "No *Completions* buffer"))
    (save-window-excursion
      (set-buffer compl-buf)
      (goto-char position)
      ;; If in a completion, move to its start, and set POSITION there.
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
        (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change
                      (point) 'mouse-face nil (icicle-start-of-candidates-in-Completions)))))
      (setq position (point))
      ;; Binary search.
      (let ((cand-nb (/ (length icicle-completion-candidates) 2))
            (last-nb 0))
        (goto-char (point-min))
        (icicle-move-to-next-completion cand-nb t)
        (while (/= (point) position)
          (let ((delta (max 1 (/ (abs (- cand-nb last-nb)) 2))))
            (cond ((< (point) position)                 
                   (icicle-move-to-next-completion delta t)
                   (setq cand-nb (+ cand-nb delta)))
                  (t
                   (icicle-move-to-next-completion (- delta) t)
                   (setq cand-nb (- cand-nb delta))))
            (setq last-nb cand-nb)))
        (set-buffer-modified-p nil)
        (1- cand-nb)))))


;;; REPLACE ORIGINAL `switch-to-completions' defined in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-switch-to-completions)
(fset 'old-switch-to-completions (symbol-function 'switch-to-completions)))

;;;###autoload
(defun icicle-switch-to-completions ()  ; Bound to `insert' in minibuffer.
  "Select the completion list window, *Completions*."
  (interactive)
  ;; Make sure we have a completions window.
  (or (get-buffer-window "*Completions*") (minibuffer-completion-help))
  (let ((window (get-buffer-window "*Completions*" 0))) ; Added 0 arg.
    (when window
      (select-window window)
      (goto-char (icicle-start-of-candidates-in-Completions)))))

;;; The branch that deletes a history element is based on Juri Linkov's
;;; `delete-history-element', proposed for Emacs 22 but rejected by RMS.
;;;###autoload
(defun icicle-erase-minibuffer-or-history-element () ; Bound to `M-k' in minibuffer.
  "`icicle-erase-minibuffer' or, if using history, delete history element."
  (interactive)
  (if (not (memq last-command '(previous-history-element next-history-element
                                icicle-erase-minibuffer-or-history-element
                                previous-matching-history-element next-matching-history-element)))
      (icicle-erase-minibuffer)
    (let* ((curr-pos (1- minibuffer-history-position))
           (current (nth curr-pos
                         (symbol-value minibuffer-history-variable))))
      (cond ((= minibuffer-history-position 1)
             (set minibuffer-history-variable (cdr (symbol-value minibuffer-history-variable))))
            ((> minibuffer-history-position 1)
             (setcdr (nthcdr (- minibuffer-history-position 2)
                             (symbol-value minibuffer-history-variable))
                     (nthcdr minibuffer-history-position
                             (symbol-value minibuffer-history-variable)))))
      (condition-case nil
          (cond ((memq last-command '(next-history-element next-matching-history-element))
                 (next-history-element 1)
                 (setq this-command 'next-history-element))
                ((memq last-command '(previous-history-element previous-matching-history-element))
                 (next-history-element 1)
                 (previous-history-element 1)
                 (setq this-command 'previous-history-element)))
        (error (condition-case nil
                   (cond ((memq last-command '(next-history-element next-matching-history-element))
                          (previous-history-element 1)
                          (setq this-command 'previous-history-element))
                         ((memq last-command
                                '(previous-history-element previous-matching-history-element))
                          (next-history-element 1)
                          (setq this-command 'next-history-element)))
                 (error nil))))
      (when (wholenump curr-pos)
        (icicle-msg-maybe-in-minibuffer "Deleted `%s'" current)))))



 
;;; Icicles commands........................................

;;; Minibuffer editing commands  . . . . . . . . . . . . . .
;;;
;;; All except `icicle-erase-minibuffer' are bound in the minibuffer to whatever the same
;;; command without `icicle-' is bound to globally.

;;;###autoload
(defun icicle-backward-delete-char-untabify (n &optional killflag)
  "`backward-delete-char-untabify' + update *Completions* with matches.
See description of `backward-delete-char-untabify'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'backward-delete-char-untabify n killflag))

;;;###autoload
(defun icicle-delete-backward-char (n &optional killflag) ; Bound to `DEL' in minibuffer.
  "`delete-backward-char' and update *Completions* with input matches.
See description of `delete-backward-char'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'delete-backward-char n killflag))

;;;###autoload
(defun icicle-delete-char (n &optional killflag) ; Bound to `C-d' in minibuffer.
  "`delete-char' and update *Completions* with input matches.
See description of `delete-char'."
  (interactive "*p\nP")
  (icicle-call-then-update-Completions #'delete-char n killflag))

;;;###autoload
(defun icicle-backward-kill-word (arg)  ; Bound to `M-DEL' (`M-backspace') in minibuffer.
  "`backward-kill-word' and update *Completions* with input matches.
See description of `backward-kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-word arg))

;;;###autoload
(defun icicle-kill-word (arg)           ; Bound to `M-d' in minibuffer.
  "`kill-word' and update *Completions* with regexp input matches.
See description of `kill-word'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-word arg))

;;;###autoload
(defun icicle-backward-kill-sexp (arg)  ; Bound to `C-M-backspace' in minibuffer.
  "`backward-kill-sexp' and update *Completions* with input matches.
See description of `backward-kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sexp arg))

;;;###autoload
(defun icicle-kill-sexp (arg)           ; Bound to `C-M-delete' and `C-M-k' in minibuffer.
  "`kill-sexp' and update *Completions* with regexp input matches.
See description of `kill-sexp'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sexp arg))

;;;###autoload
(defun icicle-backward-kill-sentence (arg) ; Bound to `C-x DEL' in minibuffer.
  "`backward-kill-sentence' and update *Completions* with input matches.
See description of `backward-kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-sentence arg))

;;;###autoload
(defun icicle-kill-sentence (arg)
  "`kill-sentence' and update *Completions* with regexp input matches.
See description of `kill-sentence'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-sentence arg))

;;;###autoload
(defun icicle-backward-kill-paragraph (arg) ; Bound to `C-backspace' in minibuffer.
  "`backward-kill-paragraph' and update *Completions* with input matches.
See description of `backward-kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'backward-kill-paragraph arg))

;;;###autoload
(defun icicle-kill-paragraph (arg)      ; Bound to `C-delete' in minibuffer.
  "`kill-paragraph' and update *Completions* with regexp input matches.
See description of `kill-paragraph'."
  (interactive "p")
  (icicle-call-then-update-Completions #'kill-paragraph arg))

;;;###autoload
(defun icicle-kill-line (arg)           ; Bound to `C-k' and `delete' in minibuffer.
  "`kill-line' and update *Completions* with regexp input matches.
See description of `kill-line'."
  (interactive "P")
  (icicle-call-then-update-Completions #'kill-line arg))

;;;###autoload
(defun icicle-kill-region (beg end)     ; Bound to `C-w' and `S-delete' in minibuffer.
;; Don't bother with Emacs 22 optional 3rd arg.
  "`kill-region' and update *Completions* with regexp input matches.
See description of `kill-region'."
  (interactive "r")
  (icicle-call-then-update-Completions #'kill-region beg end))

;;;###autoload
(when (fboundp 'kill-region-wimpy)
  (defun icicle-kill-region-wimpy (beg end) ; Bound to `C-w' and `S-delete' in minibuffer.
    "`kill-region-wimpy' and update *Completions* with input matches.
See description of `kill-region-wimpy'."
    (interactive "r")
    (icicle-call-then-update-Completions #'kill-region-wimpy beg end)))

;;;###autoload
(defun icicle-transpose-chars (arg)     ; Bound to `C-t' in minibuffer.
  "`transpose-chars' and update *Completions* with regexp input matches.
See description of `transpose-chars'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'transpose-chars arg))

;;;###autoload
(defun icicle-transpose-words (arg)     ; Bound to `M-t' in minibuffer.
  "`transpose-words' and update *Completions* with regexp input matches.
See description of `transpose-words'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-words arg))

;;;###autoload
(defun icicle-transpose-sexps (arg)    ; Bound to `C-M-t' in minibuffer.
  "`transpose-sexps' and update *Completions* with regexp input matches.
See description of `transpose-sexps'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'transpose-sexps arg))

;;;###autoload
(defun icicle-yank (arg)                ; Bound to `C-y' and `S-insert' in minibuffer.
  "`yank' and update *Completions* with regexp input matches.
See description of `yank'."
  (interactive "*P")
  (icicle-call-then-update-Completions #'yank arg))

;;;###autoload
(defun icicle-yank-pop (arg)            ; Bound to `M-y' and `M-insert' in minibuffer.
  "`yank-pop' and update *Completions* with regexp input matches.
See description of `yank-pop'."
  (interactive "*p")
  (icicle-call-then-update-Completions #'yank-pop arg))

;;;###autoload
(defun icicle-self-insert (n) ;; Bound in minibuf to stuff bound globally to `self-insert-command'.
  "`self-insert' and update *Completions* with regexp input matches.
See description of `self-insert'."
  (interactive "p")
  (icicle-call-then-update-Completions #'self-insert-command n))

;; Make delete-selection mode recognize self-insertion, so it replaces region text.
(put 'icicle-self-insert 'delete-selection t)

;;;###autoload
(defun icicle-insert-a-space ()
  "Insert a space.
For convenience in the minibuffer - does the same thing as `C-q SPC'.
To use this, bind it to some key sequence in keymaps
`minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', and
`minibuffer-local-must-match-map'."
  (interactive) (insert ?\ ))

;;;###autoload
(defun icicle-erase-minibuffer ()       ; Bound to `M-S-backspace', `M-S-delete' in minibuffer.
  "Delete all user input in the minibuffer."
  (interactive)
  (icicle-call-then-update-Completions #'icicle-clear-minibuffer))



 
;;; Other commands to be used mainly in the minibuffer . . . . . . .

;; $$ Probably need to do something to work around problem of Windows
;; selecting the new frame, when `pop-up-frames' is non-nil.  Need to
;; redirect focus back to the frame with the minibuffer.  Leave it as
;; is, for now, in hopes Emacs will eventually fix this.
;;
;;;###autoload
(defun icicle-completion-help ()        ; Bound to `C-?' in minibuffer.
  "Describe minibuffer bindings for completion."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ icicle-completion-help-string))
  ;; Don't bother to do this for Emacs 21.3.  Its `help-insert-xref-button' signature is different.
  (when (and (featurep 'help-mode) (fboundp 'help-insert-xref-button)) ; Defined in `help-mode.el'.
    (save-excursion
      (with-current-buffer (get-buffer "*Help*")
        (let ((buffer-read-only nil))
          (goto-char (point-min))
          (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
          (insert "                   ")
          (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
          (insert "\n\n\n")
          (goto-char (point-max))
          (insert "\n")
          (help-insert-xref-button "[Icicles Help on the Web]" 'icicle-help-button)
          (insert "                   ")
          (help-insert-xref-button "[Icicles Options & Faces]" 'icicle-customize-button)
          (insert "\n\n")
          (goto-char (point-min))))))
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

(when (and (fboundp 'define-button-type) (get 'help-xref 'button-category-symbol)) ; In `button.el'
  (define-button-type 'icicle-help-button
      :supertype 'help-xref
      'help-function #'(lambda () (browse-url "http://www.emacswiki.org/cgi-bin/wiki/Icicles"))
      'help-echo
      (purecopy
       "mouse-2, RET: Icicles documentation on the Emacs Wiki (requires Internet access)"))
  (define-button-type 'icicle-customize-button
      :supertype 'help-xref
      'help-function #'(lambda () (customize-group-other-window 'icicles))
      'help-echo (purecopy "mouse-2, RET: Customize/Browse Icicles Options & Faces")))

;; This is just the macro expansion of the following:
;; `(def-completion-wrapper icicle-abort-minibuffer-input :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.
(put 'icicle-abort-minibuffer-input 'completion-function 'use-completion-minibuffer-separator)

;;;###autoload
(defun icicle-abort-minibuffer-input () ; Bound to `C-g' in minibuffer, `C-g', `q' in *Completions*
  "Abort minibuffer input.
Remove \"*Completions*\" window, if any, before aborting minibuffer
input via `abort-recursive-edit'.
If the minibuffer is not active, then just kill buffer *Completions*."
  (interactive)
  (if (not (active-minibuffer-window))
      (when (get-buffer "*Completions*") (kill-buffer (get-buffer "*Completions*")))
    (icicle-remove-Completions-window)
    (abort-recursive-edit)))

(defun icicle-ensure-overriding-map-is-bound ()
  "Set `overriding-terminal-local-map' to `icicle-universal-argument-map'."
  (if (not (boundp 'overriding-map-is-bound)) ; Emacs 20, 21.
      (setq overriding-terminal-local-map icicle-universal-argument-map)
    (unless overriding-map-is-bound     ; Emacs 22
      (setq saved-overriding-map overriding-terminal-local-map)
      (setq overriding-terminal-local-map icicle-universal-argument-map)
      (setq overriding-map-is-bound t))))

;;;###autoload
(defun icicle-digit-argument (arg) ; Bound to `C-<0-9>', `M-<0-9>', `C-M-<0-9>' in minibuffer.
  "`digit-argument', but also echo the prefix."
  (interactive "P")
  (let* ((char (if (integerp last-command-char)
		   last-command-char
		 (get last-command-char 'ascii-character)))
	 (digit (- (logand char ?\177) ?0)))
    (cond ((integerp arg)
	   (setq prefix-arg (+ (* arg 10)
			       (if (< arg 0) (- digit) digit))))
	  ((eq arg '-)
	   ;; Treat -0 as just -, so that -01 will work.
	   (setq prefix-arg (if (zerop digit) '- (- digit))))
	  (t
	   (setq prefix-arg digit))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-negative-argument (arg) ; Bound to `M--', `C-M--' in minibuffer.
  "`negative-argument', but also echo the prefix."
  (interactive "P")
  (cond ((integerp arg) (setq prefix-arg (- arg)))
	((eq arg '-) (setq prefix-arg nil))
	(t (setq prefix-arg '-)))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument ()    ; Bound to `C-u' in minibuffer.
  "`universal-argument', but also echo the prefix."
  (interactive)
  (setq prefix-arg (list 4))
  (setq universal-argument-num-events (length (this-command-keys)))
  (icicle-ensure-overriding-map-is-bound)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-more (arg)
  "`universal-argument-more', but also echo the prefix."
  (interactive "P")
  (universal-argument-more arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-other-key (arg)
  "`universal-argument-other-key', but also echo the prefix."
  (interactive "P")
  (universal-argument-other-key arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-universal-argument-minus (arg)
  "`universal-argument-minus', but also echo the prefix."
  (interactive "P")
  (universal-argument-minus arg)
  (icicle-msg-maybe-in-minibuffer "prefix %S" prefix-arg))

;;;###autoload
(defun icicle-apropos-complete-and-exit () ; Bound to `S-RET' in `minibuffer-local-must-match-map'.
  "If the minibuffer contents is a valid apropos completion, then exit.
Otherwise try to complete it.  If completion leads to a valid
completion, then exit.
This is to `minibuffer-complete-and-exit' as `icicle-apropos-complete'
is to `minibuffer-complete'.  That is, it is the regexp-match version."
  (interactive)
  (let* ((icicle-apropos-complete-and-exit-p t) ; Suppress "[Sole apropos completion]" msg & wait.
         (candidates (icicle-apropos-complete)))
    (when (and candidates (null (cdr candidates))) ; Single candidate.
      (old-exit-minibuffer))))

;;;###autoload
(defun icicle-retrieve-last-input ()    ; Bound to `C-l' in minibuffer.
  "Put the last real input into the minibuffer.
Use this to replace a completion candidate inserted during cycling.
If `icicle-expand-input-to-common-match-flag' is non-nil, then using this
once restores the longest common match string, and using it twice in
succession restores your original regexp.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-retrieve-last-input]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-clear-minibuffer)
  (if (and icicle-expand-input-to-common-match-flag
           (memq last-command '(icicle-retrieve-last-input handle-switch-frame)))
      (insert icicle-current-raw-input)
    (insert icicle-current-input))
  (when (interactive-p) (setq icicle-last-completion-command nil))
  (let ((input (if (and icicle-expand-input-to-common-match-flag
                        (memq last-command (list this-command 'handle-switch-frame)))
                   icicle-current-raw-input
                 icicle-current-input)))
    (icicle-highlight-initial-whitespace input) ; Highlight initial whitespace (e.g. user typo).
    (icicle-place-cursor input))
  (deactivate-mark))

;; $$ No longer used.  It was originally used in `icicle-retrieve-last-input'.
(defun icicle-insert-input (input)
  "Insert INPUT.  Prepend the directory if appropriate."
  (insert (if (and (icicle-file-name-input-p) insert-default-directory)
              (icicle-expand-file-name input (file-name-directory input))
            input)))

;;;###autoload
(defun icicle-insert-string-at-point (&optional arg) ; Bound to `M-.' in minibuffer.
  "Insert text at the cursor into the minibuffer.
Each time this command is called, some text at or near the cursor is
inserted into the minibuffer.  One of two things happens, depending on
the value of option `icicle-default-thing-insertion' and whether or
not you use `C-u'.

`icicle-thing-at-point-functions' is a cons of two parts - call them
ALTERNATIVES and FORWARD-THING.

If ALTERNATIVES is not nil and one of the following is true:
 - FORWARD-THING is nil
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have not used `C-u' (without #) in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have used `C-u' (without #) in this series of `M-.'
then the next function in ALTERNATIVES is used to retrieve the text to
be inserted.

If FORWARD-THING is not nil and one of the following is true:
 - ALTERNATIVES is nil
 - the value of `icicle-default-thing-insertion' is `more-of-the-same'
   and you have not used `C-u' in this series of `M-.'
 - the value of `icicle-default-thing-insertion' is `alternatives' and
   you have used `C-u' in this series of `M-.'
then function FORWARD-THING is used to retrieve the text to be
inserted.

If `C-u' is used with a numeric argument (not just plain `C-u'), then
function FORWARD-THING is used to retrieve the text to be inserted,
and the argument determines the number of things to grab.  It also
determines the direction of thing-grabbing: A negative argument grabs
text to the left of the cursor; a positive argument grabs text to the
right.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-at-point]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (consp icicle-thing-at-point-functions) ; Option should always be a cons cell.
    (unless (eq last-command this-command) (setq icicle-default-thing-insertion-flipped-p nil))
    (let ((alt-fns (car icicle-thing-at-point-functions))
          (fwd-thing-fn (cdr icicle-thing-at-point-functions))
          (flipped (or icicle-default-thing-insertion-flipped-p ; Already flipped.
                       (setq icicle-default-thing-insertion-flipped-p
                             (if (eq 'alternatives icicle-default-thing-insertion)
                                 arg    ; Either `C-u' or `C-u 3' flips it for `alternatives'.
                               (consp arg)))))) ; Only `C-u' flips it for `more-of-the-same'.
      (cond
        ;; Use alternative text-grabbing functions successively.
        ((and alt-fns (or (if (eq 'alternatives icicle-default-thing-insertion)
                              (not flipped) ; Normal behavior for `alternatives'.
                            flipped)    ; Flipped behavior for `more-of-the-same'.
                          (not fwd-thing-fn))) ; No alternative.
         (setq icicle-successive-grab-count 1) ; In this mode, reset other mode's accumulator.
         (setq icicle-thing-at-pt-fns-pointer
               (if (eq last-command this-command) ; If repeated, get next text-grabbing function.
                   (mod (1+ icicle-thing-at-pt-fns-pointer) (length alt-fns))
                 0))
         (let ((thing "")
               (alt-fn (nth icicle-thing-at-pt-fns-pointer alt-fns)))
           (save-excursion (set-buffer (cadr (buffer-list))) (setq thing (funcall alt-fn)))
           (setq thing (or thing "nil"))
           (icicle-insert-thing thing)
           (icicle-msg-maybe-in-minibuffer (format "`%s'" alt-fn))))

        ;; Use same text-grabbing function successively.
        ((and fwd-thing-fn (or (if (eq 'alternatives icicle-default-thing-insertion)
                                   flipped ; Flipped behavior for `alternatives'.
                                 (not flipped)) ; Normal behavior for `more-of-the-same'.
                               (not alt-fns))) ; No alternative.
         (if (and arg (atom arg))

             ;; Explicit numeric arg.  If it doesn't change direction, then increment
             ;; existing count.  Otherwise, set count absolutely.
             (if (eq last-command this-command)
                 (if (= (icicle-signum icicle-successive-grab-count) ; Repeated `M-.'.
                        (icicle-signum (prefix-numeric-value arg)))
                     (setq icicle-successive-grab-count ; Same direction - increment count.
                           (* (icicle-signum icicle-successive-grab-count)
                              (+ (abs icicle-successive-grab-count)
                                 (abs (prefix-numeric-value arg)))))
                   (setq icicle-successive-grab-count (prefix-numeric-value arg))) ; New dir - set.
               (setq icicle-successive-grab-count (prefix-numeric-value arg))) ; First `M-.' - set.

           ;; No explicit numeric arg.
           ;; If first `M-.' or plain `C-u', set count. Otherwise, increment count.
           (if (eq last-command this-command)
               (setq icicle-successive-grab-count ; Repeated `M-.'.
                     (if (consp arg)
                         ;; We're here from plain `C-u' with `alternatives' - use 1, not 4.
                         (if (wholenump icicle-successive-grab-count) 1 -1)
                       (if (wholenump icicle-successive-grab-count) ; Increment count.
                           (+ icicle-successive-grab-count (abs (prefix-numeric-value arg)))
                         (- icicle-successive-grab-count (abs (prefix-numeric-value arg))))))
             (setq icicle-successive-grab-count 1))) ; First `M-.' - reset count.
         (let ((things ""))
           (save-excursion
             (set-buffer (cadr (buffer-list)))
             (setq things (buffer-substring-no-properties
                           (point)
                           (save-excursion (funcall fwd-thing-fn icicle-successive-grab-count)
                                           (point)))))
           (icicle-insert-thing things)))))))

(defun icicle-signum (num)
  "Return 1 if NUM is positive, -1 if negative, 0 if zero."
  (cond ((< num 0) -1) ((> num 0) 1) (t 0)))

(defun icicle-insert-thing (text)
  "Insert TEXT in the minibuffer.
TEXT replaces the last text that was inserted, if this command repeats
the last."
  (when (and (stringp text) (not (string= "" text)))
    (remove-text-properties 0 (length text) '(face nil) text)
    (when (eq last-command this-command)
      (delete-region icicle-insert-string-at-pt-start icicle-insert-string-at-pt-end))
    (setq icicle-insert-string-at-pt-start (point))
    (insert text)
    (setq icicle-insert-string-at-pt-end (point))))

;;;###autoload
(defun icicle-insert-string-from-variable (askp) ; Bound to `C-=' in the minibuffer.
  "Insert text into the minibuffer from a variable.
By default, the variable is user option `icicle-input-string'.
To insert from a different variable, use a prefix argument; you are
then prompted for the variable to use.  You can use command
`icicle-save-string-to-variable' to save a string to a variable.
Typically, you store a regexp or part of a regexp in the variable.
This command is bound in the minibuffer to `C-=', by default.
This is especially useful when used with command `icicle-search'.

Some regexps that you might want to assign to variables:

 \"[A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\"          ; Email address
 \"\\\\([0-9]+\\\.[0-9]+\\\.[0-9]+\\\.[0-9]+\\\\)\"     ; IP address
 \"[0-9]\\\\\\\={4\\\\}-[0-9]\\\\\\\={2\\\\}-[0-9]\\\\\\\={2\\\\}\"   ; Date: 2006-04-14, Time:
 \"^[ \\\=\\t]*[0-9]?[0-9]\\\\([:.]?[0-9][0-9]\\\\)?\\\\(am\\\\|pm\\\\|AM\\\\|PM\\\\)?\"
 \"`\\\\(\\\\sw\\\\sw+\\\\)'\"                        ; Words inside `_'
 \"\\\\*.*\\\\*\"                                 ; Special buffer name: *_*

Standard Emacs Lisp libraries are full of regexps that you can assign
to variables for use with `C-='.
 See `align.el' for regexps for programming languages.
 See `url-dav.el' for regexps matching iso8601 dates.
 See `rmail.el', `sendmail.el', and `mh-show.el' for regexps matching
 mail-header fields.

Imenu regexps occurring as parts of different values of
`imenu-generic-expression' for different buffer types can be used as
variable values for `C-='.  They all work fine with `icicle-search',
turning it into a browser or navigator for the given mode.

See, for example, `generic-x.el' and `lisp-mode.el'.  Here is a regexp
for Javascript function definitions from `generic-x.el':

 \"^function\\\\s-+\\\\([A-Za-z0-9_]+\\\\)\"

And `lisp-imenu-generic-expression' (in `lisp-mode.el') provides
regexps for Lisp function, variable, and type definitions.  Here is
the variable-definition regexp:

 \"^\\\\s-*(\\\\(def\\\\(c\\\\(onst\\\\(ant\\\\)?\\\\|ustom\\\\)\\\\|ine-symbol-macro\\\\|
 parameter\\\\|var\\\\)\\\\)\\\\s-+\\\\(\\\\(\\\\sw\\\\|\\\\s_\\\\)+\\\\)\"

Command `icicle-imenu' exploits this to automatically let you browse
definitions.  It is a specialization of `icicle-search' for Imenu.

For more useful regexps, grep for `font-lock-keywords' in Emacs `lisp'
directory and subdirs.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (if askp
      (let* ((enable-recursive-minibuffers t)
             (var (intern
                   (completing-read
                    "Insert text from variable: "
                    (mapcar
                     #'list
                     (mapcar 'symbol-name
                             '(adaptive-fill-first-line-regexp adaptive-fill-regexp
                               add-log-current-defun-header-regexp ange-ftp-gateway-prompt-pattern
                               allout-bullets-string allout-line-boundary-regexp allout-regexp
                               comment-start-skip comment-end comint-prompt-regexp
                               ffap-url-regexp find-face-regexp find-function-regexp
                               find-variable-regexp imenu-example--function-name-regexp-c
                               org-plain-time-of-day-regexp outline-heading-end-regexp
                               outline-line-boundary-regexp outline-plain-bullets-string
                               outline-regexp page-delimiter paragraph-separate paragraph-start
                               rmail-mime-charset-pattern sentence-end shell-prompt-pattern
                               telnet-prompt-pattern temp-file-name-pattern
                               thing-at-point-url-regexp)))
                    (lambda (cand) (boundp (intern (car cand))))
                    nil nil 'icicle-variable-history)))
             ;; Make sure we use the buffer-local value of the variable, if there is one.
             (text (with-current-buffer (cadr (buffer-list)) (symbol-value var))))
        (icicle-insert-thing text))
    (icicle-insert-thing icicle-input-string)))

;;;###autoload
(defun icicle-insert-key-description (toggle-angle-brackets-p) ; Bound to `M-q' in minibuffer.
  "Read key and insert its description.
For example, if the key read is ^F, then \"C-f\" is inserted.

`icicle-key-descriptions-use-<>-flag' determines whether angle
brackets (`<', `>') are used for named keys, such as function
keys, but a prefix argument reverses the meaning of
`icicle-key-descriptions-use-<>-flag'."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((enable-recursive-minibuffers t)
         (key (progn (minibuffer-message " [Quoting key]") (read-event))))
    (insert (single-key-description key (if toggle-angle-brackets-p
                                            icicle-key-descriptions-use-<>-flag
                                          (not icicle-key-descriptions-use-<>-flag))))))
                                      
;;;###autoload
(defun icicle-pp-eval-expression ()     ; Bound to `M-:' in minibuffer.
  "Evaluate an expression and pretty-print its value.
This just calls `pp-eval-expression' from a recursive minibuffer."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil))
    (call-interactively 'pp-eval-expression))
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

;;;###autoload
(defun icicle-next-candidate-per-mode (&optional nth)
                                        ; Bound to `icicle-modal-cycle-down-key' in minibuffer.
  "Replace input by NTH next completion candidate.
Default value of NTH is 1, meaning use the next candidate.
Negative NTH means use a previous, not subsequent, candidate.

Uses the next prefix or apropos completion command, depending on
`icicle-current-completion-mode'.  If that is nil and
`icicle-cycling-respects-completion-mode-flag' is non-nil, use the
next history element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-candidate-per-mode]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (cond ((eq icicle-current-completion-mode 'prefix)
         (setq this-command 'icicle-next-prefix-candidate)
         (icicle-next-prefix-candidate nth))
        ((eq icicle-current-completion-mode 'apropos)
         (setq this-command 'icicle-next-apropos-candidate)
         (icicle-next-apropos-candidate nth))
        ((and (eq icicle-current-completion-mode nil) icicle-cycling-respects-completion-mode-flag)
         (next-history-element (or nth 1)))))

;;;###autoload
(defun icicle-previous-candidate-per-mode (&optional nth)
                                        ; Bound to `icicle-modal-cycle-up-key' in minibuffer.
  "Replace input by NTH previous completion candidate.
Default value of NTH is 1, meaning use the previous candidate.
Negative NTH means use a subsequent, not previous, candidate.

Uses the previous prefix or apropos completion command, depending on
`icicle-current-completion-mode'. If that is nil and
`icicle-cycling-respects-completion-mode-flag' is non-nil, use the
previous history element instead.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-candidate-per-mode]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-next-candidate-per-mode (- (or nth 1))))

(put 'icicle-previous-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-previous-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-previous-prefix-candidate (&optional nth) ; Bound to `C-p', `up' in minibuffer.
  "Replace input by NTH previous prefix completion for an input.
Default value of NTH is 1, meaning use the previous prefix completion.
Negative NTH means use a subsequent, not previous, prefix completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq nth (or nth 1))
  (icicle-next-prefix-candidate (- nth)))

(put 'icicle-next-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-next-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-next-prefix-candidate (&optional nth) ; Bound to `down', `C-n' in minibuffer.
  "Replace input by NTH next prefix completion for an input.
Default value of NTH is 1, meaning use the next prefix completion.
Negative NTH means use a previous, not subsequent, prefix completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'prefix)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-prefix-candidates
                               'icicle-prefix-candidates)))

(put 'icicle-previous-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-previous-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-previous-apropos-candidate (&optional nth) ; Bound to `prior', `M-v' in minibuffer.
  "Replace input by NTH previous apropos completion for an input.
Default value of NTH is 1, meaning use the previous apropos completion.
Negative NTH means use a subsequent, not previous, apropos completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq nth (or nth 1))
  (icicle-next-apropos-candidate (- nth)))

(put 'icicle-next-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-next-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-next-apropos-candidate (&optional nth) ; Bound to `next', `C-v' in minibuffer.
  "Replace input by NTH next apropos completion for an input.
Default value of NTH is 1, meaning use the next apropos completion.
Negative NTH means use a previous, not subsequent, apropos completion.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'apropos)
  (icicle-next-candidate nth (if (icicle-file-name-input-p)
                                 'icicle-file-name-apropos-candidates
                               'icicle-apropos-candidates)
                         'regexp-p))

(put 'icicle-previous-prefix-candidate-action 'icicle-cycling-command t)
(put 'icicle-previous-prefix-candidate-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-previous-prefix-candidate-action (&optional nth) ; Bound to `C-up', `M-{' in minibuf.
  "`icicle-candidate-action', then `icicle-previous-prefix-candidate'.
Optional argument NTH is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-prefix-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-candidate-action)))
  (icicle-previous-prefix-candidate nth))

(put 'icicle-next-prefix-candidate-action 'icicle-cycling-command t)
(put 'icicle-next-prefix-candidate-action 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-next-prefix-candidate-action (&optional nth) ; Bound to `C-down', `M-}' in minibuf.
  "`icicle-candidate-action', then `icicle-next-prefix-candidate'.
Optional argument NTH is as for `icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-prefix-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'prefix)
  (save-excursion (save-selected-window (icicle-candidate-action)))
  (icicle-next-prefix-candidate nth))

(put 'icicle-previous-apropos-candidate-action 'icicle-cycling-command t)
(put 'icicle-previous-apropos-candidate-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-previous-apropos-candidate-action (&optional nth) ; Bound to `C-prior', `C-x >'.
  "`icicle-candidate-action', then `icicle-previous-apropos-candidate'.
Optional argument NTH is as for `icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-previous-apropos-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-candidate-action)))
  (icicle-previous-apropos-candidate nth))

(put 'icicle-next-apropos-candidate-action 'icicle-cycling-command t)
(put 'icicle-next-apropos-candidate-action 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-next-apropos-candidate-action (&optional nth) ; Bound to `C-next', `C-x <' in minibuf
  "`icicle-candidate-action', then `icicle-next-apropos-candidate'.
Optional argument NTH is as for `icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-next-apropos-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-completion-mode 'apropos)
  (save-excursion (save-selected-window (icicle-candidate-action)))
  (icicle-next-apropos-candidate nth))

(put 'icicle-help-on-previous-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-previous-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-help-on-previous-prefix-candidate (&optional nth) ; Bound to `C-M-up' in minibuf.
  "`icicle-help-on-candidate', then `icicle-previous-prefix-candidate'.
Optional argument NTH is as for `icicle-previous-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-help-on-candidate)))
  (icicle-previous-prefix-candidate nth))

(put 'icicle-help-on-next-prefix-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-next-prefix-candidate 'icicle-prefix-cycling-command t)
;;;###autoload
(defun icicle-help-on-next-prefix-candidate (&optional nth) ; Bound to `C-M-down' in minibuf.
  "`icicle-help-on-candidate', then `icicle-next-prefix-candidate'.
Optional argument NTH is as for `icicle-next-prefix-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-prefix-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-help-on-candidate)))
  (icicle-next-prefix-candidate nth))

(put 'icicle-help-on-previous-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-previous-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-help-on-previous-apropos-candidate (&optional nth) ; Bound to `C-M-prior' in minibuf.
  "`icicle-help-on-candidate', then `icicle-previous-apropos-candidate'.
Optional argument NTH is as for `icicle-previous-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-previous-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-help-on-candidate)))
  (icicle-previous-apropos-candidate nth))

(put 'icicle-help-on-next-apropos-candidate 'icicle-cycling-command t)
(put 'icicle-help-on-next-apropos-candidate 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-help-on-next-apropos-candidate (&optional nth) ; Bound to `C-M-next' in minibuf.
  "`icicle-help-on-candidate', then `icicle-next-apropos-candidate'.
Optional argument NTH is as for `icicle-next-apropos-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (save-excursion (save-selected-window (icicle-help-on-candidate)))
  (icicle-next-apropos-candidate nth))

(put 'icicle-prefix-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-complete ()        ; Bound to `TAB' in minibuffer.
  "Complete the minibuffer contents as far as possible, as a prefix.
If no characters can be completed, display the possible completions.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1))

(put 'icicle-prefix-complete-no-display 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-complete-no-display () ; Bound to `C-M-TAB' in minibuffer.
  "Like `icicle-prefix-complete', but without displaying *Completions*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-prefix-complete-1 'no-display))

;;;###autoload
(defun icicle-prefix-complete-1 (&optional no-display-p)
  "Helper function for `icicle-prefix-complete(-no-display)'.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
*Completions*.  Returns the list of completion candidates."
  (setq icicle-current-completion-mode 'prefix)
  (setq icicle-current-input
        (if (and icicle-last-input (symbolp last-command)
                 (get last-command 'icicle-cycling-command))
            icicle-last-input           ; $$ Previously didn't allow the -action's.
          (icicle-minibuffer-contents-from-minibuffer)))
  (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
               (string= icicle-current-input icicle-last-input)
               (eq last-command 'icicle-prefix-complete))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-prefix-candidates icicle-current-input)
            (icicle-prefix-candidates icicle-current-input))))
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (save-selected-window (icicle-remove-Completions-window))
         (minibuffer-message "  [No prefix completions]"))
        ((null (cdr icicle-completion-candidates)) ;Single candidate. Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-icompleting-p
           (icicle-clear-minibuffer)
           (insert (setq icicle-last-completion-candidate
                         (if (and (icicle-file-name-input-p) insert-default-directory)
                             (icicle-abbreviate-or-expand-file-name
                              (car icicle-completion-candidates)
                              (icicle-file-name-directory-w-default icicle-current-input))
                           (car icicle-completion-candidates))))
           (when (icicle-file-directory-p (icicle-abbreviate-or-expand-file-name
                                           icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate))))
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-transform-sole-candidate)
         (icicle-highlight-complete-input)
         (if icicle-icompleting-p
             (minibuffer-message (format "  [One prefix completion: %s]"
                                         (car icicle-completion-candidates)))
           (minibuffer-message "  [Sole prefix completion]")))
        (t                              ; Multiple candidates.
         (if icicle-icompleting-p
             (icicle-display-candidates-in-Completions nil no-display-p)
           (icicle-clear-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (deactivate-mark)
           (icicle-highlight-initial-whitespace icicle-current-input)
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate)))
           (when (member icicle-current-input icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (cond ((get-buffer-window "*Completions*" 0)
                  (if (and (eq icicle-last-completion-command 'icicle-prefix-complete)
                           (memq last-command '(icicle-prefix-complete handle-switch-frame)))
                      ;; Second `TAB' in a row.  Scroll window around.
                      (icicle-scroll-Completions)
                    ;; Did something else (e.g. changed input).  Update the display.
                    (icicle-display-candidates-in-Completions nil no-display-p)))
                 ;; No window yet.  If 2nd TAB or no chars can be completed, show window.
                 (t
                  (cond (icicle-TAB-shows-candidates-flag
                         (icicle-display-candidates-in-Completions nil no-display-p))
                        ((and (eq icicle-last-completion-command 'icicle-prefix-complete)
                              (memq last-command '(icicle-prefix-complete handle-switch-frame))
                              completion-auto-help)
                         (icicle-display-candidates-in-Completions nil no-display-p))
                        ((member icicle-current-input icicle-completion-candidates)
                         (minibuffer-message "  [Complete, but not unique]"))
                        ((and (string= icicle-current-raw-input icicle-current-input)
                              completion-auto-help)
                         (icicle-display-candidates-in-Completions nil no-display-p))))))))
  (setq icicle-last-completion-command 'icicle-prefix-complete)
  icicle-completion-candidates)

(put 'icicle-prefix-word-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-word-complete ()   ; Bound to `M-SPC' in minibuffer.
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-word-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input
        (if (and (symbolp last-command) (get last-command 'icicle-cycling-command))
            icicle-last-input           ; $$ Previously didn't allow the -action's.
          (icicle-minibuffer-contents-from-minibuffer)))
  (let ((return-value (minibuffer-complete-word)))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-prefix-candidates icicle-current-input)
            (icicle-prefix-candidates icicle-current-input)))
    (when (get-buffer-window "*Completions*" 0)
      (icicle-display-candidates-in-Completions))
    (setq icicle-last-completion-command 'icicle-prefix-word-complete)
    return-value))

(put 'icicle-apropos-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete ()       ; Bound to `S-TAB' in minibuffer.
  "Complete the minibuffer contents as far as possible.
This uses \"apropos completion\", defined as follows:
A completion contains the minibuffer input somewhere, as a substring.
Display a list of possible completions in buffer *Completions*.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names that match the current
input, taken as a regular expression, where appropriateness is
determined by the context (command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(put 'icicle-apropos-complete-no-display 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete-no-display () ; Bound to `S-C-M-TAB' in minibuffer.
  "Like `icicle-apropos-complete', but without displaying *Completions*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1 'no-display)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(defun icicle-apropos-complete-1 (&optional no-display-p)
  "Helper function for `icicle-apropos-complete(-no-display)'.
This does everything, except deal with regexp-match errors.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
*Completions*.  Returns the list of completion candidates."
  (setq icicle-current-completion-mode 'apropos)
  (setq icicle-current-input
        (if (and icicle-last-input (symbolp last-command)
                 (get last-command 'icicle-cycling-command))
            icicle-last-input
          (icicle-minibuffer-contents-from-minibuffer)))
  (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
               (string= icicle-current-input icicle-last-input)
               (eq last-command 'icicle-apropos-complete))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-apropos-candidates icicle-current-input)
            (icicle-apropos-candidates icicle-current-input))))
  ;; If input matches an empty directory, then use that directory as the sole completion.
  (when (and (icicle-file-name-input-p) (null icicle-completion-candidates)
             (string-match "/$" icicle-current-input))
    (setq icicle-completion-candidates '("")))
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (save-selected-window (icicle-remove-Completions-window))
         (minibuffer-message "  [No apropos completion]"))
        ((null (cdr icicle-completion-candidates)) ; Single candidate. Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-icompleting-p
           (icicle-clear-minibuffer)
           (insert (setq icicle-last-completion-candidate
                         (if (and (icicle-file-name-input-p) insert-default-directory)
                             (icicle-abbreviate-or-expand-file-name
                              (car icicle-completion-candidates)
                              (icicle-file-name-directory-w-default icicle-current-input))
                           (car icicle-completion-candidates))))
           (when (icicle-file-directory-p (icicle-abbreviate-or-expand-file-name
                                           icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate))))
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-transform-sole-candidate)
         (unless (boundp 'icicle-apropos-complete-and-exit-p)
           (icicle-highlight-complete-input)
           (if icicle-icompleting-p
               (minibuffer-message (format "  [One apropos completion: %s]"
                                           (car icicle-completion-candidates)))
             (minibuffer-message "  [Sole apropos completion]"))))
        (t                              ; Multiple candidates.
         (if icicle-icompleting-p
             (icicle-display-candidates-in-Completions nil no-display-p)
           (icicle-clear-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (deactivate-mark)
           (icicle-highlight-initial-whitespace icicle-current-input)
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate)))
           (when (member icicle-current-input icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (if (get-buffer-window "*Completions*" 0)
               (if (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                        (memq last-command '(icicle-apropos-complete handle-switch-frame)))
                   ;; Second `S-TAB' in a row.  Scroll window around.
                   (icicle-scroll-Completions)
                 ;; Did something else (e.g. changed input).  Update the display.
                 (icicle-display-candidates-in-Completions nil no-display-p))
             ;; No window yet.  Show window.
             (icicle-display-candidates-in-Completions nil no-display-p)))))
  (setq icicle-last-completion-command 'icicle-apropos-complete)
  icicle-completion-candidates)

(defun icicle-transform-sole-candidate ()
  "Transform matching candidate according to `icicle-list-use-nth-parts'."
  (when (and icicle-list-use-nth-parts icicle-list-join-string)
    (let ((newcand (icicle-transform-multi-completion (car icicle-completion-candidates))))
      (icicle-clear-minibuffer)
      (insert newcand)
      (setq icicle-completion-candidates (list newcand)))))

(defun icicle-transform-multi-completion (candidate)
  "Transform CANDIDATE according to `icicle-list-use-nth-parts'.
If CANDIDATE is not a multi-completion, do nothing.
Return the possibly transformed candidate."
  (if (and icicle-list-use-nth-parts icicle-list-join-string)
      (let* ((parts (split-string candidate icicle-list-join-string))
             (maxpart (length parts))
             (indexes icicle-list-use-nth-parts)
             (cand "")
             (firstp t)
             partnum)
        (while indexes
          (setq partnum (car indexes))
          (when (> partnum maxpart) (setq partnum maxpart))
          (unless firstp (setq cand (concat cand icicle-list-nth-parts-join-string)))
          (setq firstp nil)
          (setq cand (concat cand (nth (1- partnum) parts)))
          (setq indexes (cdr indexes)))
        cand)
    candidate))

;;;###autoload
(defun icicle-switch-to-Completions-buf () ; Bound to `insert' in minibuffer.
  "Select the completion list window.
The cursor is placed on the first occurrence of the current minibuffer
content.  You can use \\<completion-list-mode-map>\
`\\[icicle-insert-completion]' to get back to the minibuffer.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-switch-to-Completions-buf]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
  (let ((window (get-buffer-window "*Completions*" t))
        (search-fn 'search-forward))
    (unless window                      ; Make sure we have a completions window.
      (icicle-apropos-complete)
      (setq window (get-buffer-window "*Completions*" t)
            search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
    (when window
      (select-window window)
      (let ((case-fold-search completion-ignore-case))
        (goto-char (icicle-start-of-candidates-in-Completions))
        (when (icicle-file-name-input-p)
          (setq icicle-current-input (icicle-file-name-nondirectory icicle-current-input)))
        (when (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                   ;; $$ Previously allowed the -action's.
                   (not (and (symbolp last-command) (get last-command 'icicle-cycling-command))))
          (setq search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
        (while (and (not (eobp))
                    (save-restriction
                      (narrow-to-region (point) (next-single-property-change (point) 'mouse-face
                                                                             nil (point-max)))
                      (not (funcall search-fn icicle-current-input nil 'leave-at-end)))))
        (unless (eobp)
          (goto-char (match-beginning 0))
          (let ((prop (get-text-property (1- (point)) 'mouse-face)))
            ;; If in a completion, move to the start of it.
            (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
              (goto-char (previous-single-property-change (point) 'mouse-face nil (point-min)))))
          (icicle-place-overlay
           (point) (next-single-property-change (point) 'mouse-face nil (point-max))
           'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
           (current-buffer)))))))

;;;###autoload
(defun icicle-insert-completion ()   ; Bound to `insert' in *Completions*.
  "Select the active minibuffer window.  Insert current completion.
The current candidate in *Completions* (under the cursor) is inserted
into the minibuffer as the current input.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-switch-to-Completions-buf]'
to switch to the *Completions* window.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-insert-completion]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (when (active-minibuffer-window)
    (let ((completion (icicle-current-completion-in-Completions)))
      (select-window (active-minibuffer-window))
      (goto-char (icicle-minibuffer-prompt-end))
      (icicle-clear-minibuffer)
      (insert completion))))

(defun icicle-current-completion-in-Completions ()
  "The completion candidate under the cursor in buffer *Completions*.
The name is returned as a string."
  ;; This code comes from `choose-completion'.
  (let ((buffer completion-reference-buffer)
        (base-size completion-base-size)
        beg end completion)
    (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
      (setq end (point) beg (1+ (point))))
    (when (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
      (setq end (1- (point)) beg (point)))
    (when (null beg) (error "No completion here"))
    (setq beg (or (previous-single-property-change beg 'mouse-face) (point-min))
          end (or (next-single-property-change end 'mouse-face) (point-max)))
    (buffer-substring beg end)))

;;;###autoload
(defun icicle-switch-to/from-minibuffer () ; Bound to `pause' in Icicle mode.
  "Switch to minibuffer or previous buffer, in other window.
If current buffer is the minibuffer, then switch to the buffer that
was previously current.  Otherwise, switch to the minibuffer."
  (interactive)
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))  
  (if (eq (selected-window) (active-minibuffer-window))
      (switch-to-buffer-other-window icicle-pre-minibuffer-buffer)
    (select-window (active-minibuffer-window))))


;; Replaces `previous-completion' (defined in `simple.el').
;;;###autoload
(defun icicle-move-to-previous-completion (n) ; Bound to `left', `S-TAB' in *Completions*.
  "Move to the previous item in the completion list.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-previous-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (icicle-move-to-next-completion (- n)))


;; Replaces `next-completion' (defined in `simple.el').
;; This is the same code, except:
;; 1. This highlights the current candidate.
;; 2. This wraps around from first to last and last to first.
;;;###autoload
(defun icicle-move-to-next-completion (n &optional no-minibuffer-follow-p)
                                        ; Bound to `right', `TAB' in *Completions*.
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward).
Optional second argument, if non-nil, means do not copy the completion
back to the minibuffer.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-next-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (let ((beg (icicle-start-of-candidates-in-Completions))
        (end (point-max)))
    (while (and (> n 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'mouse-face)
        (goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'mouse-face)
        (goto-char (or (next-single-property-change (point) 'mouse-face)
                       beg)))           ; Wrap back to first candidate.
      (setq n (1- n)))
    (while (and (< n 0) (>= (count-lines 1 (point)) 3))
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
        ;; If in a completion, move to the start of it.
        (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change (point) 'mouse-face nil beg))))
      ;; Move to end of the previous completion.
      (unless (or (< (count-lines 1 (point)) 3)
                  (get-text-property (1- (point)) 'mouse-face))
        (goto-char (or (previous-single-property-change (point) 'mouse-face)
                       end)))           ; Wrap back to last candidate.
      ;; Move to the start of that one.
      (goto-char (previous-single-property-change (point) 'mouse-face nil beg))
      (setq n (1+ n)))
    (icicle-place-overlay
     (point) (next-single-property-change (point) 'mouse-face nil end)
     'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
     (current-buffer)))
  (unless no-minibuffer-follow-p
    (save-excursion (save-window-excursion (icicle-insert-completion)))))

;;;###autoload
(defun icicle-previous-line ()          ; Bound to `up' *Completions*.
  "Move up a line, in *Completions* buffer.  Wrap around first to last.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-previous-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((bolp-at-start (bolp)))
    (if (> (count-lines 1 (point)) (if bolp-at-start 3 4))
        (icicle-move-to-previous-completion 2)
      (goto-char (point-max))
      (icicle-move-to-previous-completion 1)
      (if bolp-at-start
          (while (not (bolp)) (icicle-move-to-previous-completion 1))
        (while (bolp) (icicle-move-to-previous-completion 1))))))

;;;###autoload
(defun icicle-next-line ()              ; Bound to `down' in *Completions*.
  "Move down a line, in *Completions* buffer.  Wrap around last to first.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-next-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((num-lines (- (count-lines (point-min) (point-max)) 1))
        (bolp-at-start (bolp)))
    (cond ((< (count-lines 1 (point)) (if bolp-at-start num-lines (1+ num-lines)))
           (icicle-move-to-next-completion 2)
           (when (and (bolp) (not bolp-at-start)) (icicle-move-to-next-completion 1)))
          (t
           (goto-char (point-min))
           (icicle-move-to-next-completion 1)
           (if bolp-at-start
               (while (not (bolp))
                 (icicle-move-to-next-completion 1))
             (while (bolp) (icicle-move-to-next-completion 1)))))))

;;;###autoload
(defun icicle-all-candidates-action ()  ; Bound to `C-!' in minibuffer.
  "Take action on all completion candidates.
Apply `icicle-candidate-action-fn' to each completion candidate that
matches the current input (a regular expression), successively.
The candidates that were not successfully acted upon are listed in
buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (unless icicle-candidate-action-fn (error "No action.  `icicle-candidate-action-fn' is nil."))
  (let ((candidates icicle-completion-candidates)
        (failures nil))
    (while candidates
      (let ((error-msg (condition-case act-on-each
                           (funcall icicle-candidate-action-fn (car candidates))
                         (error (error-message-string act-on-each)))))
        (when error-msg
          (setq failures (cons (cons (car candidates) error-msg) failures)))
        (setq candidates (cdr candidates))))
    (when failures
      (with-output-to-temp-buffer "*Help*"
        (princ "Action failures:")(terpri)(terpri)
        (mapcar (lambda (entry)
                  (princ (car entry)) (princ ":") (terpri) (princ "  ")
                  (princ (cdr entry)) (terpri))
                failures))))
  (icicle-abort-minibuffer-input))

;;;###autoload
(defun icicle-candidate-action ()       ; Bound to `C-RET' and `C-o' in minibuffer.
  "Take action on the current minibuffer-completion candidate.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the current candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-current-input)
    (setq last-command 'icicle-candidate-action)
    (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                 'icicle-prefix-candidates
                               'icicle-apropos-candidates)
                           (eq icicle-current-completion-mode 'apropos)))
  (if (not icicle-candidate-action-fn)
      (icicle-help-on-candidate)
    (funcall icicle-candidate-action-fn icicle-last-completion-candidate)
    (icicle-raise-Completions-frame)))

;;;###autoload
(defun icicle-mouse-candidate-action (event) ; Bound to `C-down-mouse-2' in *Completions*.
  "Take action on the minibuffer-completion candidate clicked by mouse.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (if (not icicle-candidate-action-fn)
        (icicle-help-on-candidate)
      (funcall icicle-candidate-action-fn icicle-last-completion-candidate)
      (icicle-raise-Completions-frame posn-col posn-row))))

;;;###autoload
(defun icicle-mouse-help-on-candidate (event) ; Bound to `C-M-mouse-2' in minibuffer.
  "Display help on the minibuffer-completion candidate clicked by mouse."
  (interactive "e")
  (let ((icicle-candidate-action-fn nil)) (icicle-mouse-candidate-action event)))

;;;###autoload
(defun icicle-help-on-candidate ()      ; Bound to `C-M-RET', `C-help', and `C-f1' in minibuffer.
                                        ; Bound to `C-M-RET' in *Completions.
  "Display help on the current minibuffer-completion candidate.
The help displayed depends on the type of candidate, as follows:

 menu item - the corresponding command is described using
             `describe-function' (available only if `icicles-menu.el'
             is loaded)
 command or other function - described using `describe-function'
 user option or other variable - described using `describe-variable'
 face - described using `describe-face'
 property list - described using `apropos-describe-plist'
 buffer name - modes described using `describe-mode' (Emacs > 20)
 file name - file properties described

In the minibuffer, you can also use `\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]', `\\[icicle-help-on-previous-apropos-candidate]',
`\\[icicle-help-on-next-prefix-candidate]', and \
`\\[icicle-help-on-previous-prefix-candidate]', to display help on the candidate and then
move to the next or previous candidate.  See, for example,
`icicle-help-on-next-apropos-candidate'.
\
You can use this command only from the minibuffer or *Completions*
\(`\\[icicle-help-on-candidate]')."
  (interactive)                         ; Interactively, just describes itself.
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((frame-with-focus (selected-frame))
        cand-symb)
    (if (eq (current-buffer) (get-buffer "*Completions*"))
        (setq cand-symb (intern-soft (icicle-current-completion-in-Completions)))

      ;; If no last candidate, then reset to first candidate matching input.
      (unless (stringp icicle-last-completion-candidate)
        (setq icicle-last-completion-candidate icicle-current-input)
        (setq last-command 'icicle-help-on-candidate)
        (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                     'icicle-prefix-candidates
                                   'icicle-apropos-candidates)
                               (eq icicle-current-completion-mode 'apropos)))
      (setq cand-symb (intern-soft icicle-last-completion-candidate)))

    ;; If this is a call to `icicle-execute-menu-command' (defined in `icicles-menu.el'), then 
    ;; use command associated with a menu item.  `icicle-menu-items-alist' is set in
    ;; `icicles-menu.el'.  If non-nil, then `icicle-execute-menu-command' is being called.
    (when (consp icicle-menu-items-alist)
      (setq cand-symb (cdr (assoc icicle-last-completion-candidate icicle-menu-items-alist)))
      (unless (symbolp cand-symb) (setq cand-symb nil))) ; Menu item with lambda definition.

    ;; If this is a key-completion candidate, then get the true command from the candidate.
    (when (boundp 'icicle-completing-keys-p)
      (string-match "\\(.+\\)  =  \\(.+\\)" icicle-last-completion-candidate)
      (setq cand-symb (intern-soft (substring icicle-last-completion-candidate
                                              (match-beginning 2) (match-end 2)))))

    ;; Provide the help appropriate for the given type of candidate.
    (if cand-symb
        (icicle-help-on-candidate-symbol cand-symb)
      ;; Describe buffer's mode or a file's properties.  Otherwise, create a symbol and try again.
      (cond ((and (bufferp (get-buffer icicle-last-completion-candidate))
                  (condition-case nil           ; Emacs 21+ `describe-mode' takes arg; not Emacs 20
                      (save-selected-window     ; Work around Emacs 22 bug: can't do from minibuffer
                        (when (eq (minibuffer-window) (selected-window))
                          (select-window (next-window nil 'no-mini t)))
                        (describe-mode (get-buffer icicle-last-completion-candidate)))
                    (wrong-number-of-arguments nil)))) ; Don't report Emacs 20 error.
            ((file-exists-p icicle-last-completion-candidate)
             (icicle-describe-file icicle-last-completion-candidate))
            (t
             (setq cand-symb (intern icicle-last-completion-candidate)) ; Hard intern.
             (icicle-help-on-candidate-symbol cand-symb))))
    (icicle-raise-Completions-frame)
    ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
    ;; because the *Help* frame takes the focus away from the minibuffer frame.
    ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
    (let* ((help-window (get-buffer-window "*Help*" t))
           (help-frame (and help-window (window-frame help-window))))
      (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
  (message nil))                        ; Let minibuffer contents show immmediately.

(defun icicle-help-on-candidate-symbol (symb)
  "Helper function for `icicle-help-on-candidate'.  The arg is a symbol."
  (cond (icicle-candidate-help-fn (funcall icicle-candidate-help-fn (symbol-name symb)))
        ((functionp symb) (describe-function symb))
        ((boundp symb) (describe-variable symb))
        ((facep symb) (describe-face symb))
        ((symbol-plist symb) (apropos-describe-plist symb))
        (t
         (setq symb (symbol-name symb)) ; Convert symbol to string, and try some more.
         (cond ((and (bufferp (get-buffer symb))
                     (condition-case nil            ; Emacs 21+ `describe-mode' takes arg; not 20
                         (save-selected-window      ; Work around Emacs 22 bug: can't from minibuffer
                           (when (eq (minibuffer-window) (selected-window))
                             (select-window (next-window nil 'no-mini t))
                             (describe-mode (get-buffer symb))))
                       (wrong-number-of-arguments nil)))) ; Don't report Emacs 20 error.
               ((file-exists-p symb) (icicle-describe-file symb))
               (t (icicle-msg-maybe-in-minibuffer "No help"))))))

;; This is the same as `describe-file' in `misc-cmds.el', but we avoid requiring that library.
;;;###autoload
(if (and (not (fboundp 'icicle-describe-file)) (fboundp 'describe-file))
    (fset 'icicle-describe-file (symbol-function 'describe-file))
  (defun icicle-describe-file (filename)
    "Describe the file named FILENAME."
    (interactive "FDescribe file: ")
    (help-setup-xref (list #'icicle-describe-file filename) (interactive-p))
    (let ((attrs (file-attributes filename)))
      (if (null attrs)
          (icicle-msg-maybe-in-minibuffer (format "Cannot open file `%s'" filename))
        (let ((type            (nth 0 attrs))
              (numlinks        (nth 1 attrs))
              (uid             (nth 2 attrs))
              (gid             (nth 3 attrs))
              (last-access     (nth 4 attrs))
              (last-mod        (nth 5 attrs))
              (last-status-chg (nth 6 attrs))
              (size            (nth 7 attrs))
              (permissions     (nth 8 attrs))
              ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
              (inode           (nth 10 attrs))
              (device          (nth 11 attrs)))
          (save-excursion
            (with-output-to-temp-buffer "*Help*"
              (princ (format "Properties of `%s':\n\n" filename))
              (princ (format "Type:                       %s\n"
                             (cond ((eq t type) "Directory")
                                   ((stringp type) (format "Symbolic link to `%s'" type))
                                   (t "Normal file"))))
              (princ (format "Permissions:                %s\n" permissions))
              (unless (eq t type) (princ (format "Size in bytes:              %g\n" size)))
              (princ (format "Time of last access:        %s\n" last-access))
              (princ (format "Time of last modification:  %s\n" last-mod))
              (princ (format "Time of last status change: %s\n" last-status-chg))
              (princ (format "Number of links:            %d\n" numlinks))
              (princ (format "User ID (UID):              %s\n" uid))
              (princ (format "Group ID (GID):             %s\n" gid))
              (princ (format "Inode:                      %s\n" inode))
              (princ (format "Device number:              %s\n" device))
              (princ "\n\n")
              (print-help-return-message)
              (with-current-buffer standard-output (buffer-string))))))))) ; Return displayed text.

;;;###autoload
(defun icicle-candidate-read-fn-invoke () ; Bound to `M-RET' in minibuffer.
  "Read function name.  Invoke function on current completion candidate.
Set `icicle-candidate-action-fn' to the interned name.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-read-fn-invoke]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-current-input)
    (setq last-command 'icicle-candidate-action)
    (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                 'icicle-prefix-candidates
                               'icicle-apropos-candidates)
                           (eq icicle-current-completion-mode 'apropos)))
  (let ((enable-recursive-minibuffers t)
        (icicle-saved-completion-candidate icicle-last-completion-candidate)
        (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
    (icicle-apply-to-saved-candidate
     (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                      obarray 'functionp))))

;;;###autoload
(defun icicle-mouse-candidate-read-fn-invoke (event) ; Bound to `M-mouse-2' in *Completions*.
  "Read function name.  Invoke function on candidate clicked by mouse."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    ;; (read-event)                        ; Swallow mouse up event. $$ Not needed if bound to up.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (let ((enable-recursive-minibuffers t)
          (icicle-saved-completion-candidate icicle-last-completion-candidate)
          (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
      (icicle-apply-to-saved-candidate
       (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                        obarray 'functionp)))))

(defun icicle-apply-to-saved-candidate (function)
  "Apply FUNCTION to `icicle-saved-completion-candidate'.
If `current-prefix-arg' is non-nil, then pretty-print the result using
`pp-eval-expression'."
  (let ((icicle-candidate-action-fn (car (read-from-string function))))
    (unless (functionp icicle-candidate-action-fn)
      (error "Not a function: `%S'" icicle-candidate-action-fn))
    (condition-case icicle-candidate-read-fn-invoke
        (if current-prefix-arg
            (pp-eval-expression '(funcall icicle-candidate-action-fn
                                  icicle-saved-completion-candidate))
        (funcall icicle-candidate-action-fn icicle-saved-completion-candidate))
      (error (icicle-msg-maybe-in-minibuffer
              (format "ERROR invoking `%S' on `%s'"
                      icicle-candidate-action-fn icicle-saved-completion-candidate))))
    (select-frame-set-input-focus (window-frame (minibuffer-window)))
    (icicle-raise-Completions-frame)))

(defun icicle-raise-Completions-frame (&optional mouse-col mouse-row)
  "Raise *Completions* frame, if displayed.
This helps keep *Completions* on top.

If `icicle-Completions-frame-at-right-flag' is non-nil and
*Completions* is in its own frame, then move that frame to the right,
out of the way.

Non-nil optional args MOUSE-COL and MOUSE-ROW move the mouse pointer
to column MOUSE-COL and row MOUSE-ROW.  Do this because
`icicle-candidate-action-fn' can call `select-frame-set-input-focus',
which can position mouse pointer on a standalone minibuffer frame."
  ;; Raise *Completions* frame, if displayed.  This helps keep *Completions* on top.
  (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
    (when compl-win
      (save-window-excursion
        (select-window compl-win)
        ;; Move frame to the right, out of the way.
        (when (and (one-window-p t) icicle-Completions-frame-at-right-flag)
          (modify-frame-parameters
           (selected-frame)             ; Hard-code 7 here - what does it depend on?
           `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7))))))
        (raise-frame)
        (when (and (integerp mouse-col) (integerp mouse-row))
          (set-mouse-position (selected-frame) mouse-col mouse-row))))))

;;;###autoload
(defun icicle-Completions-mouse-3-menu (event)
  "Pop-up menu on `mouse-3' for the current candidate in *Completions*."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        candidate base-size menu-choice)
    ;; (read-event)                        ; Swallow mouse up event. $$ Not needed if bound to up.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg       (previous-single-property-change beg 'mouse-face)
                end       (or (next-single-property-change end 'mouse-face)(point-max))
                candidate (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions
                               (posn-point (event-start event)))
          icicle-last-completion-candidate candidate)
    (setq menu-choice
          (x-popup-menu
           event
           (list
            "Completion Candidate"
            (list
             "$$ NOT USED $$"
             '("Help About  (`C-M-RET')" . icicle-help-on-candidate)
             '("Act On  (`C-RET', `C-mouse-2')" . icicle-candidate-action)
             '("Apply a Function...  (`M-RET', `M-mouse-2')" . icicle-candidate-read-fn-invoke)
             '("Insert in Minibuffer  (`insert')" .
               (lambda ()
                 (interactive)
                 (select-window (active-minibuffer-window))
                 (goto-char (icicle-minibuffer-prompt-end))
                 (icicle-clear-minibuffer)
                 (insert icicle-last-completion-candidate)))
             '("--")
             '("Match Also...  (`M-*')" . icicle-narrow-candidates)
             '("Only Previously Entered  (`M-pause')" . icicle-keep-only-past-inputs)
             '("--")
             '("Complement All  (`C-~')" . icicle-candidate-set-complement)
             '("Save All  (`C-M->')" . icicle-candidate-set-save)
             '("Save All to Variable...  (`C-M-})' " . icicle-candidate-set-save-to-variable)
             '("Save All to Cache File...  (`C-})" . icicle-candidate-set-save-to-cache-file)
             '("Retrieve Saved  (`C-M-<')" . icicle-candidate-set-retrieve)
             '("Add Saved  (`C-+')" . icicle-candidate-set-union)
             '("Subtract Saved  (`C--')" . icicle-candidate-set-difference)
             '("Intersect Saved  (`C-*')" . icicle-candidate-set-intersection)
             '("Act On All - Careful!  (`C-!')" . icicle-all-candidates-action)
             '("--")
             '("Toggle Case Sensitivity  (`C-A')" . icicle-toggle-case-sensitivity)
             '("Toggle Sorting  (`C-,')" . icicle-toggle-sorting)
             '("Toggle Alternative Sorting  (`M-,')" . icicle-toggle-alternative-sorting)
             '("Toggle Duplicate Removal  (`C-$')" . icicle-toggle-transforming)
             '("Toggle Angle Brackets  (`C-<')" . icicle-toggle-angle-brackets)
             '("Toggle Ignored File Extensions  (`C-.')" . icicle-toggle-ignored-extensions)
             '("Toggle Ignoring Space Prefix  (`C-^')" . icicle-toggle-ignored-space-prefix)
             '("Toggle Incremental Completion  (`C-#')" . icicle-toggle-incremental-completion)
             '("Toggle Escaping Special Regexp Chars  (`C-`')" . icicle-toggle-regexp-quote)
             '("--")
             '("Restore Input  (`C-l')" . icicle-retrieve-last-input)
             '("Scroll  (repeated `TAB' or `S-TAB')" . icicle-scroll-Completions)
             '("One-Off Eval...  (`M-:')" . icicle-pp-eval-expression)
             '("Insert `icicle-input-string'  (`C-=')" . icicle-insert-string-from-variable)
             '("--")
             '("Icicles Help  (`C-?')" . icicle-completion-help)))))
    (and menu-choice (call-interactively menu-choice))))

;;;###autoload
(defun icicle-narrow-candidates ()     ; Bound to `M-*' in minibuffer.
  "Narrow the set of completion candidates using another input regexp.
This, in effect, performs a set intersection operation on 1) the set
of candidates in effect before the operation and 2) the set of
candidates that match the current input.  You can repeatedly use this
command to continue intersecting candidate sets, progressively
narrowing the set of matches.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates]')."
  ;; We handle `no-catch' errors here because `old-completing-read' and
  ;; `old-read-file-file-name' can still be called in Icicle mode by, for instance, an
  ;; `interactive' spec (e.g. (interactive "bBuffer: ")).  In that case, we throw to a
  ;; non-existant catch.  After doing that, we just insert the result, to pass it to the
  ;; next-higher recursive minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil)) ; Inhibit reminder.
    (cond ((null icicle-completion-candidates)
           (error
            (substitute-command-keys
             "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
          ((null (cdr icicle-completion-candidates))
           (minibuffer-message "  [Sole completion]")
           (condition-case i-narrow-candidates
               (throw 'icicle-read-top (car icicle-completion-candidates))
             (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                       (icicle-retrieve-last-input)
                       icicle-current-input)
             (error (message (error-message-string i-narrow-candidates)))))
          (t
           (let* ((current-candidates icicle-completion-candidates)
                  (result
                   (if (icicle-file-name-input-p)
                       (read-file-name "Match also (regexp): "
                                       (icicle-file-name-directory-w-default icicle-current-input)
                                       nil nil nil
                                       (lambda (fname) (member fname current-candidates)))
                     (completing-read "Match also (regexp): "
                                      (mapcar #'list icicle-completion-candidates) nil nil nil
                                      'regexp-history))))
             ;; Normally, `icicle-narrow-candidates' is called from the minibuffer.
             ;; If not, just return the result read.
             (if (> (minibuffer-depth) 0)
                 (condition-case i-narrow-candidates
                     (throw 'icicle-read-top result)
                   (no-catch (setq icicle-current-input result)
                             (icicle-retrieve-last-input)
                             icicle-current-input)
                   (error (message (error-message-string i-narrow-candidates))))
               result))))))

;;;###autoload
(defun icicle-candidate-set-swap ()     ; Bound to `C-%' in minibuffer.
  "Swap the saved set and current sets of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-swap]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-saved-completion-candidates
        (prog1 icicle-completion-candidates
          (setq icicle-completion-candidates icicle-saved-completion-candidates)))
  (minibuffer-message "  [Saved set of candidates SWAPPED with current]"))

;;;###autoload
(defun icicle-candidate-set-define ()   ; Bound to `C-:' in minibuffer.
  "Define the set of current completion candidates by evaluating a sexp.
The Lisp sexp must evaluate to a list of strings, such as is returned
by `all-completions'.

You can use this command at top level or from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-define]')."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (evald-sexp (eval-minibuffer "Set the completion candidates to sexp (eval): ")))
    (if (or (null evald-sexp) (and (consp evald-sexp) (stringp (car evald-sexp))))
        (setq icicle-completion-candidates evald-sexp)
      (error "Sexp did not evaluate to a list of strings: %S" evald-sexp)))
  (icicle-maybe-sort-and-strip-candidates)
  (message "List of completion candidates DEFINED: %S" icicle-completion-candidates)
  (when (> (minibuffer-depth) 0)
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list icicle-completion-candidates))
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-candidate-set-difference () ; Bound to `C--' in minibuffer.
  "Take the set difference between the current and saved candidates.
The new set of candidates is the set of candidates prior to executing
this command minus the saved set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-difference]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-set-1 'icicle-set-difference "  [saved set of candidates SUBTRACTED]"))

;;;###autoload
(defun icicle-candidate-set-union ()    ; Bound to `C-+' in minibuffer.
  "Take the set union between the current and saved candidates.
The new set of candidates is the union of the saved set of candidates
and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-union]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-set-1 'icicle-set-union "  [saved set of candidates ADDED]"))

;;;###autoload
(defun icicle-candidate-set-intersection () ; Bound to `C-*' in minibuffer.
  "Take the set intersection between the current and saved candidates.
The new set of candidates is the intersection of the saved set of
candidates and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-intersection]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-set-1 'icicle-set-intersection
                          "  [INTERSECTION of saved and current sets of candidates]"))

;;;###autoload
(defun icicle-candidate-set-complement () ; Bound to `C-~' in minibuffer.
  "Complement the set of current completion candidates.
The new set of candidates is the set of `all-completions' minus the
set of candidates prior to executing this command - that is, all
possible completions of the appropriate type, except for those that
are in the current set of completions.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-complement]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-completion-candidates
        (icicle-set-difference
         (all-completions "" minibuffer-completion-table minibuffer-completion-predicate
                          icicle-ignore-space-prefix-flag)
         icicle-completion-candidates))
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list icicle-completion-candidates))
  (minibuffer-message "  [Set of candidates COMPLEMENTED]")
  (icicle-narrow-candidates))

(defun icicle-candidate-set-truncate (n) ; Bound to `M-$' in minibuffer.
  "Trim the set of current completion candidates at the end.
The first N candidates are kept.  N is read."
  ;; Ugly hack: `icicle-saved-completion-candidates-internal'.  No way to bind a variable
  ;; in `interactive' and have the binding be active in the function body.
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (setq icicle-saved-completion-candidates-internal icicle-completion-candidates)
           (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (read-number "Number of candidates to keep: ")))))
  (setq icicle-completion-candidates icicle-saved-completion-candidates-internal)
  (setcdr (nthcdr (1- n) icicle-completion-candidates) nil)
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list icicle-completion-candidates))
  (message (format "  [Set of candidates TRUNCATED to %d]" n))
  (icicle-narrow-candidates))
      
(defun icicle-retrieve-candidates-from-set (set-name)
  "Retrieve the saved set of completion candidates named SET-NAME.
The candidates are retrieved to `icicle-saved-completion-candidates'.
Return the name of the cache file for set SET-NAME."
  (let ((file-name (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless file-name (error "Set `%s' not found in `icicle-saved-completion-sets'.  \
Use `icicle-add/update-saved-completion-set'" set-name))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          (candidates nil))
      (message "Retrieving saved candidates from `%s'..." file-name)
      (unwind-protect
           (when (listp (setq candidates (read list-buf)))
             (message "Set `%s' read from file `%s'" set-name file-name))
        (kill-buffer list-buf))
      (unless candidates (error "No completion candidates in file `%s'" file-name))
      (setq icicle-saved-completion-candidates candidates))
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-retrieve (&optional arg) ; Bound to `C-M-<' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]', `\\[icicle-candidate-set-save-to-variable]', or \
`\\[icicle-candidate-set-save-to-cache-file]'.
With a plain prefix arg `C-u', retrieve candidates from a cache file.
With a numeric prefix arg N, retrieve candidates from a variable.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((name nil)
        (variablep (and arg (atom arg))))
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if variablep
              (setq icicle-completion-candidates
                    (symbol-value (setq name (intern
                                              (completing-read
                                               "Retrieve candidates from variable: "
                                               icicle-saved-candidates-variables-obarray
                                               nil nil nil 'icicle-variable-history)))))
            (let ((set-name (completing-read "Retrieve completion candidates from set: "
                                             icicle-saved-completion-sets nil nil nil
                                             'icicle-completion-set-history
                                             (caar icicle-saved-completion-sets))))
              (setq name (icicle-retrieve-candidates-from-set set-name)))
            (setq icicle-completion-candidates icicle-saved-completion-candidates)))
      (setq icicle-completion-candidates icicle-saved-completion-candidates))
    (message (substitute-command-keys
              (format "%s (`\\<minibuffer-local-completion-map>\\[icicle-apropos-complete]' or \
`\\[icicle-prefix-complete]' to display)"
                      (if name
                          (format "Saved candidates RESTORED from %s `%s'"
                                  (if variablep "variable" "cache file") name)
                        "Saved candidates RESTORED"))))
    (sit-for 2)
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-variable () ; Bound to `C-M-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-variable]' (or `\\[icicle-candidate-set-save]' with a numeric
prefix arg).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]')."
  (interactive)
  (icicle-candidate-set-retrieve 99))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-cache-file () ; Bound to `C-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]' or `C-u \\[icicle-candidate-set-save]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]')."
  (interactive)
  (icicle-candidate-set-retrieve '(1)))

;;;###autoload
(defun icicle-candidate-set-save (&optional arg) ; Bound to `C-M->' in minibuffer.
  "Save the set of current completion candidates, for later recall.
Saves candidates in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a numeric prefix arg (`C-u N'), save candidates in a variable.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((candidates icicle-completion-candidates))
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if (consp arg)
              (let ((file-name (icicle-add/update-saved-completion-set))) ; Write to cache also.
                (setq icicle-saved-completion-candidates candidates)
                (with-temp-message (format "Writing completion candidates to cache file `%s'..."
                                           file-name)
                  (with-temp-file file-name
                    (prin1 icicle-saved-completion-candidates (current-buffer))))
                (minibuffer-message (format "  [Current candidates SAVED to cache file `%s']"
                                            file-name)))
            (let ((var-name (completing-read "Save candidates in variable: "
                                             icicle-saved-candidates-variables-obarray
                                             nil nil nil 'icicle-variable-history)))
              (intern var-name icicle-saved-candidates-variables-obarray) ; For retrieval.
              (set (intern var-name) candidates))))
      (setq icicle-saved-completion-candidates candidates))
    (minibuffer-message "  [Current candidates SAVED]")))

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-retrieve-candidates-from-set' and `icicle-candidate-set-save'.
;;
;; We don't define this using `icicle-define-add-to-alist-command', because we want to
;; return the cache-file name.
;;;###autoload
(defun icicle-add/update-saved-completion-set ()
  "Add or update an entry in `icicle-saved-completion-sets'.
You are prompted for the name of a set of completion candidates and
its cache file.  List `icicle-saved-completion-sets' is updated to
have an entry with these set and file names.
Returns the cache-file name."
  (interactive)
  (let* ((set-name (completing-read "Saved completion set: " icicle-saved-completion-sets
                                    nil nil nil 'icicle-completion-set-history
                                    (caar icicle-saved-completion-sets)))
         (file-name ""))
    (while (not (icicle-file-writable-p file-name))
      (setq file-name (expand-file-name
                       (read-file-name "Cache file for the set: " default-directory nil nil
                                       (concat "icicles-"
                                               (icicle-delete-whitespace-from-string set-name)
                                               ".cache")))))
    (setq icicle-saved-completion-sets  ; Remove any old definition of this set.
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (push (cons set-name file-name) icicle-saved-completion-sets) ; Add new set definition.
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Added set to `icicle-saved-completion-sets': `%s'" set-name)
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-save-to-variable () ; Bound to `C-M-}' in minibuffer.
  "Save the set of current completion candidates in a variable you choose.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]' (or `\\[icicle-candidate-set-retrieve]'
with a numeric prefix arg).
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-variable]')."
  (interactive)
  (icicle-candidate-set-save 99))

;;;###autoload
(defun icicle-candidate-set-save-to-cache-file () ; Bound to `C-}' in minibuffer.
  "Save the set of current completion candidates persistently in a file.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]' or `C-u \\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]')."
  (interactive)
  (icicle-candidate-set-save '(1)))

;;;###autoload
(defun icicle-keep-only-past-inputs (&optional recent-first) ; Bound to`M-pause' in minibuffer.
  "Reduce completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping only
those that have been used before.

With a prefix arg, the previous inputs are sorted chronologically,
most recent first.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((icicle-sort-function
         (if recent-first 'icicle-most-recent-first-p icicle-sort-function)))
    (when (or recent-first (eq icicle-last-completion-command 'icicle-keep-only-past-inputs))
      (icicle-update-completions 'no-display))
    (if (null icicle-completion-candidates)
        (minibuffer-message "  [No completion candidates to filter]")
      (when (and (symbolp minibuffer-history-variable)
                 (consp (symbol-value minibuffer-history-variable)))
        (setq icicle-completion-candidates
              (icicle-delete-if-not
               (lambda (candidate)
                 (when (icicle-file-name-input-p)
                   (setq candidate (expand-file-name candidate
                                                     (file-name-directory icicle-last-input))))
                 (member candidate (symbol-value minibuffer-history-variable)))
               icicle-completion-candidates))
        (cond ((null icicle-completion-candidates)
               (save-selected-window (icicle-remove-Completions-window))
               (minibuffer-message "  [None of the completions have been used before]"))
              (t
               (setq icicle-current-input
                     (if (and (symbolp last-command) (get last-command 'icicle-cycling-command))
                         icicle-last-input ; $$ Previously didn't allow the -action's.
                       (icicle-minibuffer-contents-from-minibuffer)))
               (icicle-retrieve-last-input)
               (cond ((null icicle-completion-candidates)
                      (setq icicle-nb-of-other-cycle-candidates 0)
                      (save-selected-window (icicle-remove-Completions-window))
                      (minibuffer-message "  [No matching history element]"))
                     ((null (cdr icicle-completion-candidates)) ; Single cand. Update minibuffer.
                      (setq icicle-nb-of-other-cycle-candidates 0)
                      (icicle-clear-minibuffer)
                      (insert (setq icicle-last-completion-candidate
                                    (if (and (icicle-file-name-input-p) insert-default-directory)
                                        (expand-file-name (car icicle-completion-candidates)
                                                          (icicle-file-name-directory-w-default
                                                           (car icicle-completion-candidates)))
                                      (car icicle-completion-candidates))))
                      (save-selected-window (icicle-remove-Completions-window))
                      (icicle-highlight-complete-input)
                      (minibuffer-message (format "  [One matching history element]")))
                     (t
                      (when (member icicle-current-input icicle-completion-candidates)
                        (icicle-highlight-complete-input))
                      (icicle-display-candidates-in-Completions)
                      (save-window-excursion
                        (select-window (active-minibuffer-window))
                        (minibuffer-message
                         (concat "  [Filtered to (matching) historical candidates"
                                 (and recent-first ", most recent first")
                                 "]")))))
               (setq icicle-last-completion-command 'icicle-keep-only-past-inputs)))))
    icicle-completion-candidates))

;;;###autoload
(defun icicle-scroll-Completions ()     ; Actioned by > 1 `TAB' or `S-TAB' in minubuffer.
  "Scroll the *Completions* window."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window "*Completions*" 0))
    (if (not (= (window-end) (point-max)))
        (scroll-up nil)
      (unless (= (window-start) (point-min))
        (goto-char (icicle-start-of-candidates-in-Completions))))))

;;;###autoload
(defun icicle-history ()                ; Bound to `M-h' in minibuffer.
  "Access the appropriate history list using completion or cycling.
The current minibuffer input is interpreted as a regexp and matched
against items in the history list in use for the current command.

Note:

If the required input is a file or directory name, then the entire
minibuffer input is what is matched against the history list.  The
reason for this is that file names in the history list are usually
absolute.  This is unlike the case for normal file-name completion,
which assumes the default directory.

Keep this in mind for apropos (regexp) completion; it means that to
match a file-name using a substring you must, in the minibuffer,
either not specify a directory or explicitly use \".*\" before the
file-name substring.

For example, `/foo/bar/lph' will not apropos-match the previously
input file name `/foo/bar/alphabet-soup.el'; you should use either
`/foo/bar/.*lph' or `lph' (no directory).

This also represents a difference in behavior compared to the similar
command `icicle-keep-only-past-inputs' (\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs] in the minibuffer).
That command simply filters the current set of completion candidates,
which in the case of file-name completion is a set of relative file
names.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-history]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (icicle-file-name-input-p) (setq minibuffer-completion-predicate nil))
  (when (arrayp minibuffer-completion-table)
    (setq minibuffer-completion-predicate
          `(lambda (elt) (funcall ',minibuffer-completion-predicate (intern (car elt))))))
  (when (and (symbolp minibuffer-history-variable)
             (consp (symbol-value minibuffer-history-variable)))
    (setq minibuffer-completion-table
          (mapcar #'list (icicle-remove-duplicates (symbol-value minibuffer-history-variable)))))
  (let ((last-completion-cmd (or icicle-last-completion-command 'icicle-apropos-complete)))
    (setq icicle-last-completion-command "") ; Force redisplay of *Completions* even if displayed.
    (setq icicle-current-input
          (if (and (symbolp last-command) (get last-command 'icicle-cycling-command))
              icicle-last-input         ; $$ Previously didn't allow the -action's.
            (icicle-minibuffer-contents-from-minibuffer)))
    (icicle-retrieve-last-input)
    (funcall last-completion-cmd)))

;; This is not actually a minibuffer command, since `isearch' technically uses the echo area.
;;;###autoload
(defun icicle-isearch-complete ()       ; Bound to `S-TAB' in `isearch-mode-map'.
  "Complete the search string using candidates from the search ring."
  (interactive)
  (isearch-done 'nopush)
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (completion (completing-read "Complete search string: "
                                      (mapcar #'list (icicle-remove-duplicates ring))
                                      nil nil isearch-string
                                      (if isearch-regexp 'regexp-search-ring 'search-ring))))
    (setq isearch-string completion)
    (icicle-isearch-resume isearch-string isearch-regexp isearch-word isearch-forward
                           (mapconcat 'isearch-text-char-description isearch-string "")
                           nil)))

(defun icicle-isearch-resume (search regexp word forward message case-fold)
  "Resume an incremental search.
SEARCH is the string or regexp searched for.
REGEXP non-nil means the resumed search was a regexp search.
WORD non-nil means resume a word search.
FORWARD non-nil means resume a forward search.
MESSAGE is the echo-area message recorded for the search resumed.
CASE-FOLD non-nil means the search was case-insensitive."
  (isearch-mode forward regexp nil nil word)
  (setq isearch-string search
	isearch-message message
	isearch-case-fold-search case-fold)
  (isearch-search-and-update))


(defalias 'toggle-icicle-~-for-home-dir 'icicle-toggle-~-for-home-dir)

;;;###autoload
(defun icicle-toggle-~-for-home-dir ()  ; Bound to `M-~' in the minibuffer.
  "Toggle the value of option `icicle-use-~-for-home-dir-flag'."
  (interactive)
  (setq icicle-use-~-for-home-dir-flag (not icicle-use-~-for-home-dir-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-use-~-for-home-dir-flag
                                      "Using `~' for home directory is now ON"
                                    "Using `~' for home directory is now OFF")))


(defalias 'toggle-icicle-alternative-sorting 'icicle-toggle-alternative-sorting)

;;;###autoload
(defun icicle-toggle-alternative-sorting () ; Bound to `M-,' in the minibuffer.
  "Toggle alternative sorting of minibuffer completion candidates.
This swaps `icicle-alternative-sort-function' and `icicle-sort-function'."
  (interactive)
  (let ((alt-sort-fn icicle-alternative-sort-function))
    (setq icicle-alternative-sort-function (or icicle-sort-function icicle-last-sort-function)
          icicle-sort-function (or alt-sort-fn icicle-last-sort-function))
    (icicle-update-completions)
    (icicle-msg-maybe-in-minibuffer "Alternative sorting toggled")))


(defalias 'toggle-icicle-sorting 'icicle-toggle-sorting)

;;;###autoload
(defun icicle-toggle-sorting ()         ; Bound to `C-,' in the minibuffer.
  "Toggle sorting of minibuffer completion candidates.
When sorting is active, comparison is done by `icicle-sort-function'."
  (interactive)
  (if icicle-sort-function
      (setq icicle-last-sort-function icicle-sort-function ; Save it, for restoring.
            icicle-sort-function      nil)
    (setq icicle-sort-function icicle-last-sort-function)) ; Restore it.
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-sort-function
                                      "Completion-candidate sorting is now ON"
                                    "Completion-candidate sorting is now OFF")))

(defalias 'toggle-icicle-angle-brackets 'icicle-toggle-angle-brackets)

;;;###autoload
(defun icicle-toggle-angle-brackets () ; Bound to `C-<' in the minibuffer.
  "Toggle `icicle-key-descriptions-use-<>-flag'."
  (interactive)
  (setq icicle-key-descriptions-use-<>-flag (not icicle-key-descriptions-use-<>-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-key-descriptions-use-<>-flag
                                      "Displaying <...> in key descriptions is now ON"
                                    "Displaying <...> in key descriptions is now OFF")))

(defalias 'toggle-icicle-transforming 'icicle-toggle-transforming)

;;;###autoload
(defun icicle-toggle-transforming ()    ; Bound to `C-$' in the minibuffer.
  "Toggle transforming of minibuffer completion candidates.
When transforming is active, it is done by `icicle-transform-function'.

By default, transformation, if active, simply removes duplicate
candidates.  Icicles commands already \"do the right thing\" when it
comes to duplicate removal, so you might never need this command."
  (interactive)
  (if icicle-transform-function
      (setq icicle-last-transform-function icicle-transform-function ; Save it, for restoring.
            icicle-transform-function      nil)
    (setq icicle-transform-function icicle-last-transform-function)) ; Restore it.
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-transform-function
                                      "Completion-candidate transformation is now ON"
                                    "Completion-candidate transformation is now OFF")))


(defalias 'toggle-icicle-incremental-completion 'icicle-toggle-incremental-completion)

;;;###autoload
(defun icicle-toggle-incremental-completion () ; Bound to `C-#' in the minibuffer.
  "Toggle the value of option `icicle-incremental-completion-flag'.
If the current value is t or `always', then it is set to nil.
If the current value is nil, then it is set to t.
This command never sets the value to non-nil and non-t."
  (interactive)
  (setq icicle-incremental-completion-flag (not icicle-incremental-completion-flag))
  (setq icicle-incremental-completion-p icicle-incremental-completion-flag)
  (icicle-msg-maybe-in-minibuffer (if icicle-incremental-completion-flag
                                      "Incremental completion is now ON"
                                    "Incremental completion is now OFF")))


(defalias 'toggle-icicle-ignored-space-prefix 'icicle-toggle-ignored-space-prefix)

;;;###autoload
(defun icicle-toggle-ignored-space-prefix () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-ignore-space-prefix-flag'.
Note: If the current command binds `icicle-ignore-space-prefix-flag'
locally, then it is the local, not the global, value that is changed.
For example, `icicle-buffer' binds it to the value of
`icicle-buffer-ignore-space-prefix-flag'.  If that is non-nil, then
\\<minibuffer-local-completion-map>`\\[icicle-dispatch-C-^]' toggles \
`icicle-ignore-space-prefix-flag' to nil only for the
duration of `icicle-buffer'."
  (interactive)
  (setq icicle-ignore-space-prefix-flag (not icicle-ignore-space-prefix-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-ignore-space-prefix-flag
                                      "Ignoring space prefix is now ON"
                                    "Ignoring space prefix is now OFF")))

(defalias 'toggle-icicle-highlight-all-current 'icicle-toggle-highlight-all-current)

;;;###autoload
(defun icicle-toggle-highlight-all-current () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-search-highlight-all-current-flag'."
  (interactive)
  (setq icicle-search-highlight-all-current-flag (not icicle-search-highlight-all-current-flag))
  (icicle-erase-minibuffer)
  (icicle-retrieve-last-input)
  (icicle-msg-maybe-in-minibuffer
   (if icicle-search-highlight-all-current-flag
       "Highlighting current input match in each main search hit is now ON"
     "Highlighting current input match in each main search hit is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-^ ()           ; Bound to `C-^' in the minibuffer.
  "Do the right thing for `C-^'
When Icicles searching, call `icicle-toggle-highlight-all-current'.
Otherwise, call `icicle-toggle-ignored-space-prefix'."
  (interactive)
  (if (eq icicle-candidate-action-fn 'icicle-search-action)
      (icicle-toggle-highlight-all-current)
    (icicle-toggle-ignored-space-prefix)))


(defalias 'toggle-icicle-ignored-extensions 'icicle-toggle-ignored-extensions)

;;;###autoload
(defun icicle-toggle-ignored-extensions () ; Bound to `C-.' in minibuffer during file-name input.
  "Toggle respect of `completion-ignored-extensions'."
  (interactive)
  (if (consp completion-ignored-extensions)
      (setq icicle-saved-ignored-extensions  completion-ignored-extensions ; Save it.
            completion-ignored-extensions    nil
            icicle-ignored-extensions-regexp nil)
    (setq completion-ignored-extensions icicle-saved-ignored-extensions) ; Restore it.
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")))
  ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
  ;; `completion-ignored-extensions' changes.
  (setq icicle-ignored-extensions completion-ignored-extensions)
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if completion-ignored-extensions
                                      "Ignoring selected file extensions is now ON"
                                    "Ignoring selected file extensions is now OFF")))

(defalias 'toggle-icicle-search-cleanup 'icicle-toggle-search-cleanup)

;;;###autoload
(defun icicle-toggle-search-cleanup () ; Bound to `C-.' in minibuffer, except for file-name input.
  "Toggle removal of `icicle-search' highlighting after a search.
This toggles option `icicle-search-cleanup-flag'."
  (interactive)
  (setq icicle-search-cleanup-flag (not icicle-search-cleanup-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-search-cleanup-flag
                                      "Removal of Icicles search highlighting is now ON"
                                    "Removal of Icicles search highlighting is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-. ()           ; Bound to `C-.' in the minibuffer.
  "Do the right thing for `C-.'
When completing a file name, call `icicle-toggle-ignored-extensions'.
Otherwise, call `icicle-toggle-search-cleanup'."
  (interactive)
  (if (icicle-file-name-input-p)
      (icicle-toggle-ignored-extensions)
    (icicle-toggle-search-cleanup)))

(defalias 'toggle-icicle-regexp-quote 'icicle-toggle-regexp-quote)

;;;###autoload
(defun icicle-toggle-regexp-quote ()    ; Bound to `C-`' in the minibuffer.
  "Toggle escaping of regexp special chars (`icicle-regexp-quote-flag')."
  (interactive)
  (setq icicle-regexp-quote-flag (not icicle-regexp-quote-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-regexp-quote-flag
                                      "Escaping of regexp special characters is now ON"
                                    "Escaping of regexp special characters is now OFF")))

(defalias 'toggle-icicle-case-sensitivity 'icicle-toggle-case-sensitivity)

;;;###autoload
(defun icicle-toggle-case-sensitivity () ; Bound to `S-C-a' in the minibuffer, that is, `C-A'.
  "Toggle case sensitivity.
This toggles both `completion-ignore-case' and `case-fold-search'.
More precisely, it toggles `case-fold-search', and then it sets
`completion-ignore-case' to the value of `case-fold-search'.

Note: Some commands bind one or both of these variables, so toggling
them during command execution will not necessarily toggle the global
values of both variables."
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (setq completion-ignore-case case-fold-search)
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if case-fold-search
                                      "Case-sensitive comparison is now OFF"
                                    "Case-sensitive comparison is now ON")))

;;;###autoload
(defun icicle-remove-Completions-window () ; Bound to `C-x 0' in the minibuffer.
  "Remove the *Completions* window."
  (interactive) (icicle-delete-windows-on "*Completions*")) ; Defined in `icicles-cmd.el'.

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-remove-Completions-window'.
;;;###autoload
(defun icicle-delete-windows-on (buffer) ; From `remove-windows-on' in `frame-cmds.el'.
  "Delete all windows showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; Avoid error message "Attempt to delete minibuffer or sole ordinary window".
    (let ((frames (icicle-frames-on buffer t)))
      (unless (and frames (null (cdr frames)) ; One frame shows buffer.
                   (cdr (assoc 'minibuffer (frame-parameters (car frames)))) ; Has a minibuffer.
                   (save-window-excursion
                     (select-frame (car frames))
                     (one-window-p t 'selected-frame))) ; Only one window.
        (dolist (fr frames)
          (delete-window (get-buffer-window buffer t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mcmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mcmd.el ends here
