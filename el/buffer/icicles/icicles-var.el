;;; icicles-var.el --- Internal variables for Icicles
;;
;; Filename: icicles-var.el
;; Description: Internal variables for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:23:26 2006
;; Version: 22.0
;; Last-Updated: Fri Nov 24 22:08:56 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 286
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-var.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `hexrgb', `icicles-opt', `thingatpt', `thingatpt+',
;;   `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  internal variables (not to be modified by users.  See `icicles.el'
;;  for documentation.
;;
;;  Internal variables defined here:
;;
;;    `frame-name-history', `icicle-bookmark-history',
;;    `icicle-buffer-config-history', `icicle-candidate-action-fn',
;;    `icicle-candidate-entry-fn', `icicle-candidate-nb',
;;    `icicle-candidates-alist', `icicle-cmd-calling-for-completion',
;;    `icicle-color-history', `icicle-color-theme-history',
;;    `icicle-common-match-string', `icicle-complete-input-overlay',
;;    `icicle-completion-candidates' `icicle-completion-help-string',
;;    `icicle-completion-set-history',
;;    `icicle-current-completion-candidate-overlay',
;;    `icicle-current-completion-mode', `icicle-current-input',
;;    `icicle-current-raw-input', `icicle-default-directory',
;;    `icicle-default-thing-insertion-flipped-p',
;;    `icicle-dictionary-history', `icicle-extra-candidates',
;;    `icicle-font-history', `icicle-function-history',
;;    `icicle-icompleting-p', `icicle-ignored-extensions',
;;    `icicle-ignored-extensions-regexp',
;;    `icicle-incremental-completion-p', `icicle-initial-value',
;;    `icicle-insert-string-at-pt-end',
;;    `icicle-insert-string-at-pt-start', `icicle-kill-history',
;;    `icicle-kmacro-alist', `icicle-kmacro-history',
;;    `icicle-last-completion-candidate',
;;    `icicle-last-completion-command', `icicle-last-input',
;;    `icicle-last-sort-function', `icicle-last-transform-function',
;;    `icicle-menu-items-alist', `icicle-must-match-regexp',
;;    `icicle-must-not-match-regexp', `icicle-must-pass-predicate',
;;    `icicle-nb-of-other-cycle-candidates',
;;    `icicle-pre-minibuffer-buffer', `icicle-post-command-hook',
;;    `icicle-pre-command-hook', `icicle-prompt',
;;    `icicle-prompt-suffix', `icicle-re-no-dot',
;;    `icicle-saved-candidates-variables-obarray',
;;    `icicle-saved-completion-candidates',
;;    `icicle-saved-ignored-extensions',
;;    `icicle-saved-kmacro-ring-max',
;;    `icicle-saved-regexp-search-ring-max',
;;    `icicle-saved-region-background',
;;    `icicle-saved-search-ring-max', `icicle-search-command',
;;    `icicle-search-current-overlay', `icicle-search-final-choice',
;;    `icicle-search-history', `icicle-search-overlays',
;;    `icicle-search-refined-overlays',
;;    `icicle-successive-grab-count',
;;    `icicle-thing-at-pt-fns-pointer',
;;    `icicle-universal-argument-map', `icicle-variable-history'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/11/24 dadams
;;     Added: icicle-universal-argument-map, icicle-kmacro-alist, icicle-saved-kmacro-ring-max,
;;            icicle-kmacro-history.
;; 2006/11/18 dadams
;;     Added: frame-name-history, icicle-bookmark-history, icicle-buffer-config-history,
;;            icicle-color-history, icicle-color-theme-history, icicle-completion-set-history,
;;            icicle-dictionary-history, icicle-font-history, icicle-function-history,
;;            icicle-kill-history, icicle-search-history, icicle-variable-history,
;; 2006/11/09 dadams
;;     icicle-search-refined-overlays: Updated doc string: icicle-search-highlight-threshold.
;; 2006/10/14 dadams
;;     Moved conditional eval-when-compile to top level.
;; 2006/09/24 dadams
;;     icicle-last-transform-function: Corrected default value.
;; 2006/09/12 dadams
;;     Added: icicle-pre-minibuffer-buffer.
;; 2006/08/20 dadams
;;     icicle-current-completion-mode: Updated doc string.
;; 2006/08/04 dadams
;;     Removed icicle-apropos-completing-p (not used).
;; 2006/07/23 dadams
;;     Added: icicle-last-transform-function.
;; 2006/07/22 dadams
;;     Added: icicle-search-command, icicle-search-final-choice.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-current-completion-type.
;;     Renamed: icicle-current-completion-type to icicle-current-completion-mode.
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;; 2006/06/18 dadams
;;     Added: icicle-apropos-completing-p.
;; 2006/04/30 dadams
;;     Added: icicle-candidate-entry-fn.
;;     Renamed: icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/14 dadams
;;     Renamed icicle-search-refined-overlay to icicle-search-refined-overlays.
;;     Added: icicle-search-candidates.
;; 2006/04/07 dadams
;;     Added: icicle-search-overlays.
;;     Renamed icicle-search-overlay to icicle-search-current-overlay.
;; 2006/03/27 dadams
;;     Added: icicle-search-refined-overlay.
;; 2006/03/26 dadams
;;     Added: icicle-search-overlay.
;; 2006/03/25 dadams
;;     Added: icicle-saved-candidates-variables-obarray.
;; 2006/03/20 dadams
;;     Added: icicle-common-match-string, icicle-current-regexp-input.
;; 2006/03/14 dadams
;;     Removed: icicle-icicle-completing-p.
;; 2006/03/13 dadams
;;     Added: icicle-re-no-dot.
;; 2006/03/05 dadams
;;     Moved to icicles-mode.el: icicle-mode-map.
;; 2006/03/04 dadams
;;     Moved options stuff to Options menu, when available.
;;     Moved apropos stuff to Apropos menu, when available.
;;     Moved describe stuff to Describe menu, when available.
;; 2006/03/03 dadams
;;     Added to Icicles menu: icicle-complete-thesaurus-entry, icicle-apropos*,
;;       option-setting cmds, buffer-config cmds icicle-(var|fun)doc.
;;     Require apropos-fn+var.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile                               ;; for Emacs < 21: push
 (when (< emacs-major-version 21) (require 'cl)));; for Emacs < 20: when, unless

(require 'apropos-fn+var nil t) ;; (no error if not found): apropos-command, apropos-function,
                                ;; apropos-option, apropos-variable
(require 'icicles-opt) ;; icicle-sort-function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Internal variables (alphabetical) ----------------------

;; These two are defined here so they won't raise an error in `font-lock-add-keywords'.
(defvar font-lock-function-name-face 'font-lock-function-name-face ; Defined in `font-lock.el'.
  "Face name to use for function names.")

(defvar font-lock-keyword-face 'font-lock-keyword-face ; Defined in `font-lock.el'.
  "Face name to use for keywords.")

(defvar frame-name-history nil "History for frame names.") ; Defined in Emacs 22 `frame.el'.

(defvar icicle-bookmark-history nil "History for bookmark names.")

(defvar icicle-buffer-config-history nil "History for buffer configuration names.")

(defvar icicle-candidate-action-fn nil
  "Function to be applied to current completion candidate.
For `icicle-all-candidates-action' to be able to report successes,
this should return non-nil for \"success\" and nil for \"failure\".")

(defvar icicle-candidate-entry-fn nil
  "Function to apply to selected entries in `icicle-candidates-alist'.")

(defvar icicle-candidate-nb nil
  "Current completion candidate number, or nil if not cycling candidates.
Numbering starts at zero.")

(defvar icicle-candidates-alist nil
  "Alist of candidate entries.
The car (key) of each entry is treated as a completion candidate.
The cdr is some other data to be used when the candidate is chosen.")

(defvar icicle-cmd-calling-for-completion 'ignore
  "Last command causing display of list of possible completions.")

(defvar icicle-color-history nil "History for color names.")

(defvar icicle-color-theme-history nil "History for color-theme names.")

(defvar icicle-common-match-string nil
  "Longest common match among all completion candidates.
Nil means no such common match is available.")

(defvar icicle-complete-input-overlay nil
  "Overlay used to highlight minibuffer input when it is complete.")

(defvar icicle-completion-candidates nil "Current list of completion candidates.")

(defvar icicle-completion-help-string ""
  "Description of minibuffer bindings.")

(defvar icicle-completion-set-history nil "History for completion-set names.")

(defvar icicle-current-completion-candidate-overlay nil
  "Overlay used to highlight current completion candidate.")

(defvar icicle-current-completion-mode nil
  "Symbol `prefix' or `apropos', specifying the current completion mode.")

(defvar icicle-current-input "" "Current minibuffer input.")

(defvar icicle-current-raw-input "" "Current minibuffer raw (unexpanded) input.
This can be different from `icicle-current-input' only when
`icicle-expand-input-to-common-match-flag' is non-nil.")

(defvar icicle-default-directory default-directory
  "Local copy of `default-directory'.
Set whenever minibuffer is entered or input is completed.")

(defvar icicle-default-thing-insertion-flipped-p nil
  "Non-nil means a previous `M-.' in this succession was used with `C-u'.
This means that the meaning of `icicle-default-thing-insertion' has
been reversed.")

(defvar icicle-dictionary-history nil "History for dictionary entries.")

(defvar icicle-extra-candidates nil
  "A list of extra completion candidates (strings).")

(defvar icicle-font-history nil "History for font names.")

(defvar icicle-function-history nil "History for function names.")

(defvar icicle-icompleting-p nil
  "Internal flag: non-nil when editing text in minibuffer.
This is really non-nil when inside simple character-editing commands
such as `icicle-self-insert' and `icicle-delete-backward-char'.")

(defvar icicle-ignored-extensions completion-ignored-extensions
  "Copy of `completion-ignored-extensions', serving as a control flag.
When `completion-ignored-extensions' changes, we remake
`icicle-ignored-extensions-regexp'.")

(defvar icicle-ignored-extensions-regexp
  (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|")
          "\\)\\'")
  "Regular expression matching ignored file extensions.
If this is nil, then no file extensions are ignored.
The ignored file extensions come from `completion-ignored-extensions'.")

(defvar icicle-incremental-completion-p nil
  "Takes the place of `icicle-incremental-completion-flag' during input.
The program updates this to `always' from `t' after *Completions* has
been displayed.")

(defvar icicle-initial-value ""
  "Initial value used in minibuffer completion.
Any function that reads from the minibuffer and accepts a default
value or initial value should, before reading, put that value in
`icicle-initial-value'.  For example, `completing-read' does that.")

(defvar icicle-insert-string-at-pt-end 0
  "Position of end of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-insert-string-at-pt-start 0
  "Position of start of text `icicle-insert-string-at-point' inserted.")

(defvar icicle-kill-history nil "History of kill-ring entries.")

(when (boundp 'kmacro-ring)             ; Emacs 22
  (defvar icicle-kmacro-alist nil
    "Alist with elements (CANDIDATE-NAME . RING-ITEM).
CANDIDATE-NAME is a synthetic macro name: \"macro #\" followed by a
unique number 1, 2, 3....

RING-ITEM is an item in `kmacro-ring' or `(kmacro-ring-head)'.")
  (defvar icicle-kmacro-history nil "History for keyboard-macro names."))

(defvar icicle-last-completion-candidate ""
  "Last completion candidate used in minibuffer completion.")

;; This is used to be able to ignore `handle-switch-frame'.
(defvar icicle-last-completion-command nil "Last completion command used.")

(defvar icicle-last-input "" "Last minibuffer input typed (not from cycling).")

(defvar icicle-last-sort-function (or icicle-sort-function 'string-lessp)
  "Local copy of `icicle-sort-function', so we can restore it.")

(defvar icicle-last-transform-function (or icicle-transform-function 'icicle-remove-duplicates)
  "Local copy of `icicle-transform-function', so we can restore it.")

(defvar icicle-menu-items-alist nil)    ; Defined in `icicles-menu.el'.

(defvar icicle-must-match-regexp nil
  "Nil or a regexp that completion candidates must match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that match it (and match the user input).
See also `icicle-must-not-match-regexp'.")

(defvar icicle-must-not-match-regexp nil
  "Nil or a regexp that completion candidates must not match.
If nil, then this does nothing.  If a regexp (string), then show only
candidates that do not match it.
See also `icicle-must-match-regexp'.")

(defvar icicle-must-pass-predicate nil
  "Nil or a predicate that completion candidates must satisfy.
If nil, then this does nothing.  Otherwise, this is a function of one
argument, a candidate, and only candidates that satisfy the predicate
are displayed.")

(defvar icicle-nb-of-other-cycle-candidates 0
  "Number of other candidates available for cycling.
This is for use by other libraries, in particular, `icomplete+.el'.")

;; Inspired from `icomplete-post-command-hook'.
(defvar icicle-post-command-hook nil
  "Functions added to `post-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

;; Inspired from `icomplete-pre-command-hook'.  There is none, by default.
(defvar icicle-pre-command-hook nil
  "Functions added to `pre-command-hook' when in Icicle mode.
Use command `icy-mode' (aka `icicle-mode') to set this up properly.")

(defvar icicle-pre-minibuffer-buffer nil
  "Buffer that was current before the minibuffer became active.")

(defvar icicle-prompt ""
  "Prompt used for completion.  See also `icicle-prompt-suffix'.")

(defvar icicle-prompt-suffix ""
  "String to append to the input-completion prompt, if there is room.
Intended to remind you how to obtain help on input completion.")

(defvar icicle-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"
  "Regexp that matches anything except `.' and `..'.")

(defvar icicle-saved-candidates-variables-obarray (make-vector 100 0)
  "Obarray of variables you have saved sets of completion candidates in.
Used for completion in `icicle-candidate-set-retrieve-from-variable'.")

(defvar icicle-saved-completion-candidates nil
  "Completion candidates user saved using `icicle-candidate-set-save'.")

(defvar icicle-saved-ignored-extensions nil
  "Local copy of `icicle-ignored-extensions', so we can restore it.")

(when (boundp 'kmacro-ring)             ; Emacs 22
  (defvar icicle-saved-kmacro-ring-max kmacro-ring-max
    "Saved value of `kmacro-ring-max', so it can be restored."))

(defvar icicle-saved-regexp-search-ring-max regexp-search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-saved-region-background nil
  "Background of `region' face.  Saved so it can be restored.")

(defvar icicle-saved-search-ring-max search-ring-max
  "Saved value of `search-ring-max', so it can be restored.")

(defvar icicle-search-command 'icicle-search
  "Command to use for Icicles searches.
You can set a buffer-local value of this variable, to use a specific
search command in a particular mode.")

(defvar icicle-search-current-overlay nil
  "Overlay used to highlight current match of `icicle-search' regexp arg.")

(defvar icicle-search-final-choice nil
  "Final user input from `icicle-search'.
This might or might not be one of the possible search candidates.")

(defvar icicle-search-history nil "History for `icicle-search' final choices.")

(defvar icicle-search-overlays nil
  "Overlays used to highlight match of `icicle-search' regexp argument.")

(defvar icicle-search-refined-overlays nil
  "Overlay(s) used to highlight match of current input for `icicle-search'.
If `icicle-search-highlight-threshold' is less than one, then this is
a single overlay (or nil).  Otherwise, this is a list of overlays.")

(defvar icicle-successive-grab-count 0
  "Number of text things to be grabbed by next `\\<minibuffer-local-map>\
\\[icicle-insert-string-at-point]'.")

(defvar icicle-thing-at-pt-fns-pointer 0
  "Current index into the car of `icicle-thing-at-point-functions'.
This points to the current function in the list.")

(defvar icicle-universal-argument-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'icicle-universal-argument-other-key)
    (define-key map (vector meta-prefix-char t) 'icicle-universal-argument-other-key)
    (define-key map [switch-frame] nil)
    (define-key map [?\C-u] 'icicle-universal-argument-more)
    (define-key map [?-] 'icicle-universal-argument-minus)
    (define-key map [?0] 'icicle-digit-argument)
    (define-key map [?1] 'icicle-digit-argument)
    (define-key map [?2] 'icicle-digit-argument)
    (define-key map [?3] 'icicle-digit-argument)
    (define-key map [?4] 'icicle-digit-argument)
    (define-key map [?5] 'icicle-digit-argument)
    (define-key map [?6] 'icicle-digit-argument)
    (define-key map [?7] 'icicle-digit-argument)
    (define-key map [?8] 'icicle-digit-argument)
    (define-key map [?9] 'icicle-digit-argument)
    (define-key map [kp-0] 'icicle-digit-argument)
    (define-key map [kp-1] 'icicle-digit-argument)
    (define-key map [kp-2] 'icicle-digit-argument)
    (define-key map [kp-3] 'icicle-digit-argument)
    (define-key map [kp-4] 'icicle-digit-argument)
    (define-key map [kp-5] 'icicle-digit-argument)
    (define-key map [kp-6] 'icicle-digit-argument)
    (define-key map [kp-7] 'icicle-digit-argument)
    (define-key map [kp-8] 'icicle-digit-argument)
    (define-key map [kp-9] 'icicle-digit-argument)
    (define-key map [kp-subtract] 'icicle-universal-argument-minus)
    map)
  "Keymap used while processing `C-u' during Icicles completion.")

(defvar icicle-variable-history nil "History for variable names.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-var)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-var.el ends here
