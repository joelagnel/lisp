;;; icicles-cmd.el --- Commands for Icicles
;;
;; Filename: icicles-cmd.el
;; Description: Commands for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:04 2006
;; Version: 22.0
;; Last-Updated: Sun Nov 26 21:18:55 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 6135
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-cmd.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `custom', `dired',
;;   `dired+', `dired-aux', `dired-x', `easymenu', `ediff-diff',
;;   `ediff-help', `ediff-init', `ediff-merg', `ediff-mult',
;;   `ediff-util', `ediff-wind', `fit-frame', `frame-cmds',
;;   `frame-fns', `info', `info+', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `pp', `pp+', `strings', `thingatpt',
;;   `thingatpt+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  commands (and a few non-interactive functions used in commands).
;;  See `icicles.el' for documentation.
;;
;;  Commands defined here -
;;
;;   Commands to be used mainly at top level:
;;
;;    `icicle-add-buffer-candidate', `icicle-add-buffer-config',
;;    `icicle-add-region', `icicle-add/update-saved-completion-set',
;;    `icicle-apropos', `icicle-apropos-command',
;;    `icicle-apropos-function', `icicle-apropos-option',
;;    `icicle-apropos-variable', `icicle-apropos-zippy',
;;    `icicle-bookmark', `icicle-buffer', `icicle-buffer-config',
;;    `icicle-buffer-list', `icicle-buffer-other-window',
;;    `icicle-clear-option', `icicle-color-theme',
;;    `icicle-comint-command', `icicle-comint-search',
;;    `icicle-compilation-search', `icicle-complete-keys',
;;    `icicle-complete-thesaurus-entry',
;;    `icicle-customize-icicles-group', `icicle-dabbrev-completion',
;;    `icicle-delete-file', `icicle-delete-window',
;;    `icicle-delete-windows', `icicle-delete-windows-on',
;;    `icicle-dired-saved-file-candidates',
;;    `icicle-dired-saved-file-candidates-other-window', `icicle-doc',
;;    `icicle-execute-extended-command',
;;    `icicle-execute-named-keyboard-macro', `icicle-find-file',
;;    `icicle-find-file-other-window', `icicle-font',
;;    `icicle-frame-bg', `icicle-frame-fg', `icicle-fundoc',
;;    `icicle-generic-S-tab', `icicle-imenu', `icicle-Info-goto-node',
;;    `icicle-Info-goto-node-cmd', `icicle-Info-index',
;;    `icicle-Info-index-20', `icicle-Info-index-cmd',
;;    `icicle-insert-char', `icicle-insert-kill',
;;    `icicle-insert-thesaurus-entry', `icicle-kill-buffer',
;;    `icicle-kmacro', `icicle-lisp-complete-symbol',
;;    `icicle-locate-file', `icicle-locate-file-other-window',
;;    `icicle-map' `icicle-mode', `icy-mode', `icicle-occur',
;;    `icicle-other-window-or-frame', `icicle-plist'
;;    `icicle-read-kbd-macro', `icicle-recent-file',
;;    `icicle-recent-file-other-window',
;;    `icicle-remove-buffer-candidate', `icicle-remove-buffer-config',
;;    `icicle-remove-region', `icicle-remove-saved-completion-set',
;;    `icicle-repeat-complex-command', `icicle-reset-option-to-nil',
;;    `icicle-save-string-to-variable', `icicle-search',
;;    `icicle-search-generic', `icicle-search-highlight-cleanup',
;;    `icicle-search-region', `icicle-select-frame',
;;    `icicle-select-region', `icicle-select-window',
;;    `icicle-set-option-to-t', `icicle-switch-to/from-minibuffer',
;;    `icicle-toggle-alternative-sorting',
;;    `icicle-toggle-angle-brackets',
;;    `icicle-toggle-case-sensitivity',
;;    `icicle-toggle-highlight-all-current',
;;    `icicle-toggle-ignored-extensions',
;;    `icicle-toggle-ignored-space-prefix',
;;    `icicle-toggle-incremental-completion',
;;    `icicle-toggle-regexp-quote', `icicle-toggle-search-cleanup',
;;    `icicle-toggle-sorting', `icicle-toggle-transforming',
;;    `icicle-vardoc', `icicle-yank-insert',
;;    `toggle-icicle-alternative-sorting',
;;    `toggle-icicle-angle-brackets',
;;    `toggle-icicle-case-sensitivity',
;;    `toggle-icicle-ignored-extensions',
;;    `toggle-icicle-ignored-space-prefix',
;;    `toggle-icicle-incremental-completion',
;;    `toggle-icicle-regexp-quote', `toggle-icicle-search-cleanup',
;;    `toggle-icicle-sorting', `toggle-icicle-transforming'.
;;
;;   Commands to be used mainly in the minibuffer or *Completions*:
;;
;;    `icicle-abort-minibuffer-input', `icicle-apropos-complete',
;;    `icicle-apropos-complete-and-exit',
;;    `icicle-apropos-complete-no-display',
;;    `icicle-backward-delete-char-untabify',
;;    `icicle-backward-kill-paragraph',
;;    `icicle-backward-kill-sentence', `icicle-backward-kill-sexp',
;;    `icicle-backward-kill-word', `icicle-candidate-action',
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
;;    `icicle-candidate-set-swap', `icicle-candidate-set-union',
;;    `icicle-choose-completion-string', `icicle-completion-help',
;;    `icicle-customize-apropos', `icicle-customize-apropos-faces',
;;    `icicle-customize-apropos-groups',
;;    `icicle-customize-apropos-options',
;;    `icicle-delete-backward-char', `icicle-delete-char',
;;    `icicle-digit-argument', `icicle-dispatch-C-.',
;;    `icicle-erase-minibuffer',
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
;;    `icicle-mouse-choose-completion',
;;    `icicle-mouse-help-on-candidate'
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
;;    `icicle-previous-apropos-candidate',
;;    `icicle-previous-apropos-candidate-action',
;;    `icicle-previous-candidate-per-mode', `icicle-previous-line',
;;    `icicle-previous-prefix-candidate',
;;    `icicle-previous-prefix-candidate-action',
;;    `icicle-retrieve-candidates-from-set',
;;    `icicle-retrieve-last-input', `icicle-scroll-Completions',
;;    `icicle-self-insert', `icicle-send-bug-report',
;;    `icicle-switch-to-Completions-buf',
;;    `icicle-switch-to-completions',
;;    `icicle-switch-to/from-minibuffer',
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
;;    `icicle-yank-pop', `old-completing-read',
;;    `old-choose-completion-string', `old-completion-setup-function',
;;    `old-exit-minibuffer', `old-minibuffer-complete-and-exit',
;;    `old-read-file-name', `old-switch-to-completions',
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
;;    `custom-variable-p', `icicle-add-key+cmd',
;;    `icicle-apropos-complete-1', `icicle-binary-option-p',
;;    `icicle-comint-get-final-choice',
;;    `icicle-comint-get-minibuffer-input',
;;    `icicle-comint-send-input', `icicle-complete-keys-1',
;;    `icicle-complete-keys-action',
;;    `icicle-current-completion-in-Completions',
;;    `icicle-delete-file-or-directory', `icicle-edmacro-parse-keys',
;;    `icicle-ensure-overriding-map-is-bound',
;;    `icicle-execute-extended-command-1', `icicle-filter-alist',
;;    `icicle-find-file-other-window-w-wildcards',
;;    `icicle-find-file-w-wildcards', `icicle-imenu-in-buffer-p',
;;    `icicle-Info-goto-node-action', `icicle-Info-index-action',
;;    `icicle-insert-for-yank', `icicle-insert-input',
;;    `icicle-insert-thing', `icicle-isearch-resume',
;;    `icicle-keys+cmds-w-prefix', `icicle-kill-a-buffer',
;;    `icicle-kmacro-action', `icicle-map-action',
;;    `icicle-nb-of-candidate-in-Completions',
;;    `icicle-non-whitespace-string-p', `icicle-prefix-complete-1',
;;    `icicle-raise-Completions-frame',
;;    `icicle-read-from-minibuf-nil-default',
;;    `icicle-read-single-key-description', `icicle-search-action',
;;    `icicle-search-highlight-all-input-matches',
;;    `icicle-search-regexp-scan', `icicle-search-region-beg-end',
;;    `icicle-signum', `icicle-this-command-keys-prefix'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-complete-keys-alist'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `exit-minibuffer'              - Remove *Completion* window.
;;  `minibuffer-complete-and-exit' - Remove *Completion* window.
;;
;;
;;  ***** NOTE: The following functions defined in `dabbrev.el' have
;;              been REDEFINED HERE:
;;
;;  `dabbrev-completion' - Use Icicles completion when you repeat
;;                         (`C-M-/').
;;
;;
;;  ***** NOTE: The following functions defined in `lisp.el' have
;;              been REDEFINED in Icicles:
;;
;;  `lisp-complete-symbol' - Selects *Completions* window even if on
;;                           another frame.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-choose-completion' - Return the number of the completion.
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;  `switch-to-completions' - Always selects *Completions* window.
;;
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;
;;  ***** NOTE: The following functions defined in `cus-edit.el' have
;;              been REDEFINED HERE:
;;
;;  `customize-apropos', `customize-apropos-faces',
;;  `customize-apropos-groups', `customize-apropos-options' -
;;     Use `completing-read' to read the regexp.
;;
;;
;;  Key bindings made by Icicles: See "Key Bindings" in `icicles.el'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/11/26 dadams
;;    icicle-search-action: Bug fix: Use icicle-candidate-nb, not assoc, to get cand+mrker.
;;    icicle-*-complete-1: Bug fix: Don't set icicle-current-input to icicle-last-input if nil.
;;    Renamed: icicle-search-region to icicle-search-region-beg-end.
;;    Added: icicle-(add|remove|select|search)-region.
;;    icicle-search: Use icicle-regions for numeric prefix arg.  Updated doc string.
;;    Added: icicle-Info-index-20 - thx to Tamas Patrovics.  Use it in icicle-Info-index.
;; 2006/11/25 dadams
;;    icicle-search: After final selection, select orig-window and give its frame input focus.
;; 2006/11/24 dadams
;;    Added: icicle-ensure-overriding-map-is-bound, icicle-universal-argument,
;;           icicle-universal-argument-more, icicle-negative-argument, icicle-digit-argument,
;;           icicle-universal-argument-other-key, icicle-universal-argument-minus,
;;           icicle-kmacro(-action).
;;    icicle-dabbrev-completion: Don't stop at common root, and use lax completion.
;;    Replaced icicle-select-window-or-frame by icicle-other-window-or-frame, which respects C-u 0.
;; 2006/11/23 dadams
;;    icicle-prefix-complete-1: Respect icicle-TAB-shows-candidates-flag.
;;    icicle-execute-extended-command-1: Treat named keyboard macros too.
;;    Added: icicle-execute-named-keyboard-macro.
;; 2006/11/18 dadams icicle-add/update-saved-completion-set, icicle-apropos*, icicle-bookmark,
;;    icicle-buffer-config, icicle-candidate-set-retrieve, icicle-candidate-set-save,
;;    icicle-color-theme, icicle-comint-command, icicle-complete-thesaurus-entry,
;;    icicle-customize-apropos*, icicle-delete-windows, icicle-font, icicle-frame-bg,
;;    icicle-frame-fg, icicle-insert-kill, icicle-insert-string-from-variable,
;;    icicle-insert-thesaurus-entry, icicle-locate-file*, icicle-map, icicle-narrow-candidates,
;;    icicle-remove-buffer-config, icicle-remove-saved-completion-set, icicle-reset-option-to-nil,
;;    icicle-save-string-to-variable, icicle-search, icicle-select-window, icicle-set-option-to-t,
;;    icicle-toggle-option:
;;      Use a specific history variable.
;; 2006/11/17 dadams
;;    Added: icicle-select-(frame|window), icicle-select-window-or-frame.
;; 2006/11/10 dadams
;;    icicle-mouse-candidate-action: read-event to swallow mouse up event.
;;    icicle-map-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;                       Unwind-protect to reselect minibuffer frame.
;;                       Don't bind case-fold-search.
;;    icicle-map: enable-recursive-minibuffers.
;; 2006/11/09 dadams
;;    icicle-nb-of-candidate-in-Completions: Redefined using binary search, for better performance.
;;    icicle-toggle-ignored-space-prefix: Update doc string to use icicle-dispatch-C-^.
;;    icicle-search:
;;      Bind icicle-update-input-hook unconditionally, after icicle-search-regexp-scan.
;;    icicle-search-regexp-scan: Highlight up to icicle-search-highlight-threshold.
;;    icicle-search-highlight-all-input-matches:
;;      Only update input and highlight if icicle-search-highlight-all-current-flag.
;;    icicle-search-action: Don't use icicle-filter-alist - find string in icicle-candidates-alist.
;;    icicle-search-highlight-cleanup: Bind inhibit-quit to t.
;; 2006/11/07 dadams
;;    Added: icicle-toggle-highlight-all-current.
;; 2006/11/06 dadams
;;    icicle-search-action:
;;      Highlight icicle-current-input, not icicle-current-raw-input (not updated).
;;    Renamed icicle-search-highlight-all-flag to icicle-search-highlight-threshold.
;; 2006/11/05 dadams
;;    Added: icicle-search-regexp-scan.
;;    icicle-search:
;;      Added buffers arg.  Updated doc string.
;;      Use icicle-search-regexp-scan:  Scan each buffer in buffers.
;;                                      Add marker, not position, to icicle-candidates-alist.
;;      Go to candidate in its buffer.
;;      Added progress message.
;;    icicle-search-action: Pop to buffer of candidate (marker) and raise frame.
;;    icicle-occur: Added buffers arg.  Updated doc string.  Call icicle-search-highlight-cleanup.
;;    icicle-search-highlight-all-input-matches: set-buffer for each ov in dolist (minor opt.).
;;    icicle-search-highlight-cleanup: Added progress messages.  Minor optimization.
;; 2006/10/22 dadams
;;    icicle-complete-keys-action:
;;      Set last-nonmenu-event to non-mouse info, to ignore *Completions* click.
;;    icicle-complete-keys-1: Don't use a default value for completing-read.
;; 2006/10/21 dadams
;;    Added: icicle-insert-char.
;;    icicle-add-key+cmd: Respect icicle-complete-keys-self-insert-flag.
;; 2006/10/20 dadams
;;    icicle-map, icicle-delete-window: Corrected doc string.
;; 2006/10/16 dadams
;;    icicle-add-key+cmd: Protect :enable's eval with condition-case.
;;    icicle-complete-keys-1: No longer use icicle-extra-candidates.
;;                            Use default value of .. for completing-read (except at top level).
;;    icicle-complete-keys-action: Correct no-match case: must match whole and part.
;;    icicle-keys+cmds-w-prefix: Add .. to icicle-complete-keys-alist unless at top level.
;; 2006/10/15 dadams
;;    icicle-complete-keys:
;;      Bind icicle-complete-keys-action, not icicle-complete-keys-help, to icicle-*-action-fn.
;;      Bind orig-buff, orig-window, and icicle-completing-keys-p, for use elsewhere.
;;    Added: icicle-complete-keys-action.  Initial definition from code in icicle-complete-keys.
;;    icicle-complete-keys-action:
;;      Use orig-buff and orig-window; restore to originally selected window.
;;      Error if candidate doesn't match template xxx  =  yyy.
;;    icicle-complete-keys-1:
;;      Call icicle-complete-keys-action on chosen candidate.
;;    icicle-help-on-candidate: Treat key completion also.
;;    Added from cus-edit+.el: custom-variable-p.
;;    Moved to icicles-mode.el: icicle-select-minibuffer-contents, next-history-element.
;;    Moved here from icicles-mode.el: icicle-generic-S-tab.
;;    icicle-generic-S-tab (bug fix): Do not set last-command to icicle-apropos-complete.
;;    Added: eval-when-compile's.
;; 2006/10/13 dadams
;;    icicle-add-key+cmd:
;;      Add actual key to icicle-complete-keys-alist.  Thx to Stefan Monnier.
;;      Don't filter out index (Imenu) keymap.
;;      Treat :enable condition.
;;    icicle-complete-keys-1:
;;      Use actual key recorded in icicle-complete-keys-alist. Don't convert to key description.
;;      Treat digit-argument and negative-argument.
;;    icicle-complete-keys-alist: Updated doc string for new structure.
;; 2006/10/08 dadams
;;    Added: icicle-add-key+cmd, icicle-read-single-key-description.
;;    Added: icicle-complete-keys-alist.
;;           Use in icicle-complete-keys-1, icicle-keys+cmds-w-prefix, icicle-add-key+cmd.
;;    icicle-add-key+cmd: Update binding, depending on its type (menu item etc.).
;;      Push (cons candidate binding), not just candidate, onto icicle-complete-keys-alist.
;;    icicle-complete-keys-1:
;;      Use binding, not just command name.  Call it and put it in (this|last)-command.
;;      Flipped (corrected) use of icicle-key-descriptions-use-<>-flag.
;;      Use icicle-read-single-key-description.
;;    icicle-prefix-keys-first-p, icicle-complete-keys-1, icicle-complete-keys-help,
;;      icicle-keys+cmds-w-prefix: Don't use icicle-list-*-string.
;; 2006/10/05 dadams
;;    icicle-complete-keys-1: Remove icicle-special-candidate property from all candidates.
;;    icicle-keys+cmds-w-prefix:
;;      Intern candidate and, if local binding, put icicle-special-candidate property on it.
;;      Use single string for candidate (don't use multi-completion).
;; 2006/10/03 dadams
;;     icicle-complete-keys-1: Treat "..".
;;     icicle-complete-keys: Updated doc string accordingly.
;;     icicle-prefix-keys-first-p: ".." is less than all other strings.  Don't hard-code "= ".
;;     icicle-keys+cmds-w-prefix:
;;       Filtered out shadowed bindings, icicle-generic-S-tab, and icicle-complete-keys.
;;       Use only map-keymap & lookup-key, not accessible-keymaps, current-active-maps, map-keymap.
;; 2006/10/01 dadams
;;     icicle-complete-keys: Bind sort functions, to put prefix keys first, by default.
;;                           Set last-command, before recursing.
;;     Replaced icicle-alternative-sort with icicle-toggle-alternative-sorting (new).
;;     icicle-(apropos|prefix)-complete-1:
;;       Ensure icicle-(current|last)-input are strings, before compare.
;;     icicle-keys+cmds-w-prefix: Tolerate empty local and global maps.
;; 2006/09/30 dadams
;;     Added: icicle-read-kbd-macro, icicle-edmacro-parse-keys, icicle-toggle-angle-brackets.
;;     icicle-complete-keys-1, icicle-dabbrev-completion:
;;       key-description -> icicle-key-description, with icicle-key-descriptions-use-<>-flag.
;;     icicle-complete-keys-1:
;;       read-kbd-macro -> icicle-read-kbd-macro, with icicle-key-descriptions-use-<>-flag.
;;       Got rid of extra space in prompt before colon, when no prefix.
;;     icicle-keys+cmds-w-prefix: Use single-key-description with icicle-*-use-<>-flag.
;;     icicle-insert-key-description:
;;       Change arg to a toggle, and use icicle-key-descriptions-use-<>-flag.
;;     Bind icicle-candidate-set-retrieve, icicle-candidate-set-save to C-M-<, C-M->, not C-<, C->.
;;     icicle-dired-saved-file-candidates*:
;;       Changed doc strings and messages to use dynamic binding of icicle-candidate-set-save.
;; 2006/09/24 dadams
;;     Added: icicle-complete-keys-help.
;;     icicle-complete-keys:
;;       Bind icicle-*-action-fn to icicle-complete-keys-help.  Mention help keys in docstring.
;;     icicle-complete-keys-1:
;;       Set last-command to command, so completion doesn't think candidate was last-command.
;;     icicle-keys+cmds-w-prefix: Provide placeholder for future insertion of generic characters.
;; 2006/09/23 dadams
;;     icicle-complete-keys-1:
;;       Error if there are no keys for the prefix.
;;       Error, not self-insert-command, for key read-kbd-macro can't convert. Use condition-case.
;;       Report error if calling cmd fails.
;;       Use vconcat for recursive call.
;;       Read cmd, don't intern it - it might be a lambda or byte-compiled function.
;;       Remove duplicates.
;;       Provide KEYS arg to call-interactively, for error reporting. 
;;       No longer bind icicle-must-not-match-regexp to "^Character set .*=  self-insert-command"
;;     icicle-keys+cmds-w-prefix:
;;       Treat also local keymap and current minor maps.
;;       Do nothing if keys+maps is nil.
;;       Only map-keymap if the target is a keymap.
;;       Use keymapp, not functionp, as the binding test.
;;       Only add binding if it is a command or keymap.
;;       Only add self-insert-command binding if the key is char-valid-p.
;;       Use format %S, not %s for a command binding.
;;     icicle-insert-key-description: Added no-angle-brackets-p arg.
;; 2006/09/22 dadams
;;     icicle-complete-keys-1: Filter out keys described "Character set ...= self-insert-command".
;; 2006/09/20 dadams
;;     icicle-complete-keys-1: Treat self-insert-command specially.
;; 2006/09/17 dadams
;;     Added: icicle-complete-keys(-1), icicle-*-keys-prefix, icicle-keys+cmds-w-prefix, 
;;     icicle-doc: Removed one \n from each candidate.
;; 2006/09/12 dadams
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;;     Added: icicle-switch-to/from-minibuffer.
;;     icicle-completion-help: Keep focus in the minibuffer after displaying help.
;; 2006/09/02 dadams
;;     icicle-help-on-(next|previous)-(apropos|prefix)-candidate,
;;       icicle-(next|previous)-(apropos|prefix)-candidate-action:
;;       Use save-selected-window, not save-window-excursion.
;;     icicle-find-file*: In Dired, ignore errors picking up current-line's file name.
;;     icicle-mouse-choose-completion: Error if minibuffer is not active.
;; 2006/08/27 dadams
;;     icicle-abort-minibuffer-input: If minibuffer is not active, just kill buffer *Completions*.
;;     icicle-execute-extended-command-1, icicle-insert-thesaurus-entry, icicle-search-action:
;;       Ensure orig-window is live before using it.
;; 2006/08/23 dadams
;;     Added: icicle-delete-window(s).
;;     Added soft require of frame-cmds.el.
;; 2006/08/22 dadams
;;     icicle-execute-extended-command-1: Bind this-command, don't assign it (fixes C-next).
;;     icicle-help-on-candidate: If no last candidate, then reset to first matching candidate.
;;     icicle-*-*-candidate-action, icicle-help-on-*-*-candidate: save-window-excursion. (good?)
;; 2006/08/20 dadams
;;     icicle-find-file*: Use diredp-find-a-file* in Dired mode (Emacs 22 or later).
;;     Bug fix: icicle-candidate-action: Use icicle-*-candidates, not icicle-next-*-candidate.
;;              icicle-next-*-candidate(-action): Set icicle-current-completion-mode.
;; 2006/08/18 dadams
;;     Added: icicle-Info-goto-node(-(action|cmd)).
;;     icicle-candidate-action: If no icicle-last-completion-candidate, use the first candidate.
;; 2006/08/15 dadams
;;     Added: icicle-help-on-*-*-candidate,icicle-mouse-help-on-candidate.
;;     No longer put icicle-candidate-action-command property on symbols (not used).
;;     Added: icicle-raise-Completions-frame.
;;     icicle*-candidate-action, icicle-help-on-candidate: Use icicle-raise-Completions-frame.
;;     icicle-help-on-candidate: Can use it from *Completions* too now.
;;                               Use icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     Added: icicle-Info-index(-(action|cmd)).
;; 2006/08/04 dadams
;;     icicle-*-complete-1, icicle-prefix-word-complete, icicle-keep-only-past-inputs:
;;       Set icicle-last-completion-command to the explicit command, not this-command.
;;     icicle-history: Call icicle-last-completion-command, not icicle-apropos-complete.
;;     icicle-apropos-complete-1, icicle-narrow-candidates:
;;       Removed binding of icicle-apropos-completing-p (not used).
;;     Added: icicle-plist, icicle-remove-Completions-window, icicle-pp-eval-expression.
;;     Added soft require of pp+.el.
;;     icicle-exit-minibuffericicle-minibuffer-complete-and-exit, icicle-mouse-choose-completion,
;;     icicle-abort-minibuffer-input, icicle-(apropos|prefix)-complete-1,
;;     icicle-keep-only-past-inputs, icicle-insert-thesaurus-entry-cand-fn:
;;       Use icicle-remove-Completions-window.
;;     icicle-doc: Treat doc of faces also.
;;     icicle-non-whitespace-string-p: Added doc string.
;; 2006/08/03 dadams
;;     Added:
;;       icicle-comint-command, icicle-insert-kill, icicle-insert-for-yank,icicle-yank-insert.
;;     Bound icicle-comint-command to C-c TAB in comint-mode.
;;     icicle-search, icicle-comint-search: Cleaned up doc string.
;; 2006/08/02 dadams
;;     icicle-comint-search: Mention *-prompt-pattern.  Thx to Kevin Rodgers.
;;     icicle-insert-string-from-variable: Added some more variables to the completing-read alist.
;; 2006/07/29 dadams
;;     Added: icicle-dispatch-C-., toggle-icicle-search-cleanup, icicle-toggle-search-cleanup.
;; 2006/07/23 dadams
;;     Added: icicle-toggle-transforming.
;;     icicle-comint-search: Bind icicle-transform-function to icicle-remove-duplicates.
;; 2006/07/22 dadams
;;     Added: icicle-comint-search, icicle-comint-send-input, icicle-comint-get-minibuffer-input,
;;            icicle-comint-get-final-choice, icicle-search-generic.
;;     icicle-search: Added require-match arg for non-interactive calls.
;;                    Run the hooks if no match and no match required, and if we didn't cycle.
;;                    Return final choice as value (not used yet).
;;     icicle-insert-string-from-variable: Use buffer-local value of variable, if there is one.
;;     icicle-insert-string-from-variable:
;;       Make sure we use the buffer-local value of the variable, if there is one
;;       Added comint-prompt-regexp to regexp list.
;;     Added mode hooks for icicle-compilation-search and icicle-comint-send-input.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added: icicle-(next|previous)-context-candidate, icicle-scroll-completions.
;;       icicle-switch-to-completions, icicle-switch-to-Completions-buf,
;;         icicle-move-to-next-completion, icicle-map-action, icicle-search-action:
;;           Use icicle-start-of-completions.
;;       icicle-(apropos|prefix)-complete-1:
;;         Set icicle-current-completion-type vs use icicle-arrows-respect-completion-type-flag.
;;         Use icicle-scroll-completions.
;;       icicle-current-completion-in-Completions: Use point-min if no previous prop change.
;;       icicle-keep-only-past-inputs: Use icicle-scroll-completions.
;;     Renamed icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;             icicle-current-completion-type to icicle-current-completion-mode,
;;             icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;             icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-(next|previous)-context-candidate: Use icicle-barf-if-outside-minibuffer.
;;     icicle-scroll-Completions: Changed with-selected-window to Emacs 20 equivalent.
;; 2006/07/18 dadams
;;     icicle-search: Bind completion-ignore-case to case-fold-search.
;;     icicle-search-highlight-all-input-matches, icicle-search-action:
;;       Put search inside condition-case, for bad regexp.
;;     Added: icicle-toggle-case-sensitivity, toggle-icicle-case-sensitivity.
;; 2006/07/10 dadams
;;     Added: icicle-search-region.  Use in search functions.  Thx to Le Wang.
;; 2006/07/08 dadams
;;     icicle-search-highlight-all-input-matches: Bug fix - Use *-current-*, not *-current-raw-*.
;;     icicle-execute-extended-command-1:
;;       First try a string candidate as arg, then read it to convert it to symbol or number.
;;       Reset focus back to the minibuffer, in action function.
;; 2006/07/07 dadams
;;     Added: icicle-alternative-sort.
;;     icicle-imenu: Show *Completions* initially for submenu choice (only).
;;     icicle-execute-extended-command: Echo prefix arg in prompt.  Thx: *.dhcp.mdsn.wi.charter.com
;; 2006/07/06 dadams
;;     Added (eval-when-compile (require 'icicles-mac)).
;; 2006/07/05 dadams
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;;     icicle-prefix-complete-1: Don't set icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-prefix-complete-1: No longer calculate common prefix and set current input to it.
;;     Added plist entries to categorize commands:
;;       icicle-(cycling|completing|candidate-action)-command.
;;     icicle-(apropos|prefix)-complete-1, icicle-prefix-word-complete,
;;     icicle-switch-to-Completions-buf, icicle-keep-only-past-inputs, icicle-history:
;;       Use icicle-cycling-command property.
;;     icicle-apropos-complete-1: Removed regexp-p arg in call to icicle-save-or-restore-input.
;; 2006/07/03 dadams
;;     icicle-(apropos|prefix)-complete-1: deactivate mark after inserting current input.
;; 2006/06/18 dadams
;;     icicle-apropos-complete-1, icicle-narrow-candidates: Bind icicle-apropos-completing-p.
;; 2006/06/09 dadams
;;     Bug fixes: Picked up matching subdir as default dir, even if there other files match. 
;;                  Thx to Andrey Zhdanov.
;;                Empty directory not treated as a match.
;;     icicle-(apropos|prefix)-complete-1:
;;       If input matches an empty directory, then use that directory as the sole completion.
;;       Do not expand file-name input before call icicle-file-name-(apropos|prefix)-candidates.
;;     icicle-retrieve-last-input: Use insert, not icicle-insert-input (no longer used).
;;                                 (Input backslashes reverted to slashes.)
;; 2006/06/08 dadams
;;     Bug fix: Could not complete after cycling file names.  Thx to Andrey Zhdanov.
;;     icicle-insert-input: Use icicle-expand-file-name.
;;     icicle-prefix-complete-1:
;;       Expand file-name input before call icicle-file-name-prefix-candidates.
;;       Expand icicle-last-completion-candidate if it is a directory name.
;; 2006/05/30 dadams
;;     icicle-erase-minibuffer-or-history-element: Fix for consecutive deletions.
;; 2006/05/26 dadams
;;     Added: icicle-erase-minibuffer-or-history-element.
;; 2006/05/19 dadams
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;;     icicle-narrow-candidates: Bind icicle-reminder-prompt-flag to nil, not t.
;; 2006/05/16 dadams
;;     Added: icicle-kill(-a)-buffer.
;; 2006/05/15 dadams
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-candidate-set-complement: Put back icicle-ignore-space-prefix-flag.
;;     icicle-buffer(-other-window): Bind icicle-buffer-ignore-space-prefix-flag.
;;     Added: icicle-toggle-ignored-space-prefix, toggle-icicle-ignored-space-prefix.
;; 2006/05/13 dadams
;;     icicle-occur: Make icicle-search-main-regexp-others unnoticeable instead of
;;                   setting icicle-search-highlight-all-flag to nil.
;;     icicle-candidate-set-complement: Use nil, not icicle-completion-nospace-flag.
;;     Renamed: icicle-search-imenu to icicle-imenu,
;;              icicle-search-imenu-in-buffer-p to icicle-imenu-in-buffer-p.
;; 2006/05/12 dadams
;;     icicle-search-imenu: Remove unmatched submenus.  Error if no imenu for the buffer.
;;     Added: icicle-search-imenu-in-buffer-p.
;;     icicle-insert-string-at-point: Use icicle-barf-if-outside-minibuffer.
;;     Moved to icicles-fn.el: icicle-barf-if-outside-*.
;;     Moved some commands to minibuffer-cmds section from top-level cmds section.
;; 2006/05/09 dadams
;;     Added: icicle-customize-icicles-group, icicle-send-bug-report, icicle-customize-button.
;; 2006/04/30 dadams
;;     Added: icicle-map, icicle-map-action.
;;     icicle-filter-alist: Corrected and simplified.
;;     icicle-search: Corrected cand-nb adjustment when cycle with action fns.
;;     Renamed: icicle-search-action-fn to icicle-search-action,
;;              icicle-search-candidates to icicle-candidates-alist.
;; 2006/04/28 dadams
;;     icicle-retrieve-last-input, icicle-(apropos|prefix)-complete-1:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/25 dadams
;;     icicle-completion-help: Emacs 21.3's help-insert-xref-button signature is different.
;; 2006/04/16 dadams
;;     Added: icicle-search-imenu.
;;     icicle-search: Bug fixes:
;;       Treat completion without cycling: error or singleton go-to.
;;       Only subtract one from candidate number for C- cycling, not regular cycling.
;; 2006/04/14 dadams
;;     icicle-search:
;;       Bug fix: Position was off by one.
;;       Highlight input match inside each main regexp match (or not).
;;         Bind icicle-update-input-hook and icicle-incremental-completion-flag.
;;       Extract code to define icicle-search-action-fn.
;;       Use icicle-search-candidates instead of local variable search-candidates.
;;       Respect icicle-search-cleanup-flag.
;;     Added: icicle-search-highlight-*, icicle-search-action-fn,
;;            icicle-(insert|save)-text-(from|to)-variable.
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/09 dadams
;;     icicle-(apropos|prefix)-complete-1: Deal with icicle-arrows-respect-completion-type-flag.
;;     Moved here from icicles-fn.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/07 dadams
;;     icicle-search: Highlight all occurrences at once (like isearch highlighting, but not lazy).
;;                    Error if no match for initial regexp.
;;     icicle-occur: Bind icicle-search-highlight-all-flag to nil, so don't highlight each line.
;; 2006/04/02 dadms
;;     Added: icicle-toggle-regexp-quote, icicle-find-file*-w-wildcards.
;;     icicle-find-file*: Use icicle-find-file*-w-wildcards.
;; 2006/03/31 dadams
;;     icicle-search: Wrap action function with unwind-protect to select minibuffer frame.
;;                    Use completion-ignore-case when highlighting search hits.
;;                    Protect delete-overlay with overlayp.
;;                    Turn off region highlighting (so can see highlighting done here).
;;                    Removed sit-for-period argument.
;;     icicle-candidate-set-save: Use prin1 instead of pp.
;; 2006/03/27 dadams
;;     Added: icicle-occur.
;;     icicle-search: Highlight also match of current regexp, inside that of regexp arg.
;;                    Use new faces icicle-search-*-regexp.
;;     icicle-search, icicle-switch-to-Completions-buf, icicle-move-to-next-completion:
;;       Use new, generic icicle-place-overlay.
;;     Removed icicle-place-search-overlay.
;; 2006/03/26 dadams
;;     icicle-search: Use icicle-search-overlay.  Ensure don't match twice at same position.
;;                    Added regexp arg.  Use 0 as sit-for default.
;;     Added: icicle-place-search-overlay.
;; 2006/03/25 dadams
;;     icicle-prefix-complete: Minor bug fix: Don't save try-completion if not a string.
;;     icicle-candidate-set-(save|retrieve): Allow use of a variable to save/retrieve.
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-variable, icicle-*-complete-no-display,
;;            icicle-prefix-complete-1.
;;     icicle-apropos-complete-1: Added no-display-p optional arg.
;;     Use no-display-p arg in calls to icicle-display-candidates-in-Completions.
;;     icicle-candidate-set-(retrieve-from|save-to)-cache-file: Pass a consp, not t.
;;     icicle-candidate-set-retrieve: Don't display *Completions*.
;; 2006/03/24 dadams
;;     Added icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-candidate-set-define: Rewrote.  Can also use at top level.
;;       Error if wrong result type.  Sort result.  Use display-completion-list and
;;       icicle-narrow-candidates (unless at top level).
;;     icicle-narrow-candidates: Can call from top-level (but not interactively).
;;     icicle-candidate-set-complement: Use icicle-maybe-sort-and-strip-candidates.
;;     Mention in doc strings of minibuffer and *Completions* functions: where, key.
;; 2006/03/22 dadams
;;     icicle-find-file*: Use default-directory as default, so opens directory on empty input.
;;     icicle-prefix-complete:
;;       Save icicle-current-regexp-input.
;;       Set icicle-current-input to common prefix.  Use it everywhere here.
;;     Calls to icicle-display-candidates-in-Completions: no root arg now.
;; 2006/03/21 dadams
;;     icicle-insert-input: Bug fix: Use directory of input, not default-directory.
;;                                   Append a slash if input itself is a directory.
;; 2006/03/20 dadams
;;     icicle-retrieve-last-input: Insert icicle-current-regexp-input if repeat C-l.
;;     Added: icicle-insert-input.
;; 2006/03/19 dadams
;;     icicle-apropos-complete-1: Call icicle-save-or-restore-input with non-nil regexp-p arg.
;; 2006/03/17 dadams
;;     Added: icicle-add/update-saved-completion-set, icicle-remove-saved-completion-set,
;;            icicle-retrieve-candidates-from-set.
;;     Removed: icicle-cache-file.
;;     icicle-candidate-set-retrieve: Read candidates set and use its cache file.
;;                                    Enable recursive minibuffers.
;;     icicle-candidate-set-save: Read candidates set and cache-file names.
;;                                Use icicle-add/update-saved-completion-set.
;;     icicle-barf-if-outside-minibuffer: Move interactive test to calling functions.
;;     icicle-files-within: Moved to icicle-fn.el.
;; 2006/03/16 dadams
;;     Added: icicle*-saved-completion-set.
;; 2006/03/14 dadams
;;     icicle-narrow-candidates: Handle no-catch error.  Don't use icicle-completing-p.
;;     icicle-candidate-set-complement:
;;       Do what we do in icicle-candidate-set-retrieve: call icicle-narrow-candidates.
;;     icicle-candidate-set-(retrieve|complement): Msg when display.
;;     icicle-(apropos|prefix)-complete-1:
;;       Removed test for last-command = icicle-candidate-set-complement.
;; 2006/03/13 dadams
;;     Added: icicle-candidate-set-(retrieve-from|save-to)-cache-file.
;;     icicle-candidate-set-(retrieve|save): C-u uses cache file.
;; 2006/03/12 dadams
;;     Added: icicle-dired-saved-file-candidates(-other-window), icicle-locate-file*,
;;            icicle-files-within.
;; 2006/03/11 dadams
;;     icicle-find-file*, icicle-delete-file*:
;;       Reverted to simple form (moved directory control to icicles-mac.el).
;;     icicle-keep-only-past-inputs: Expand file name relative to directory of last input.
;; 2006/03/10 dadams
;;     icicle-find-file*, icicle-delete-file*: Expand file name relative to dir of last input.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;; 2006/03/09 dadams
;;     icicle-barf-if-outside-*: Removed argument - use this-command instead.
;; 2006/03/08 dadams
;;     icicle-bookmark: Use default value, not init value, for completing-read.
;; 2006/03/07 dadams
;;     icicle-doc: Save table in minibuffer-completion-table, so can access via C-RET too.
;;     icicle-insert-thesaurus-entry, icicle-*doc:
;;       Removed binding of icicle-incremental-completion-flag to nil.
;;     Added: icicle-barf-if-outside-(minibuffer|Completions).  Use in appropriate commands.
;;     Added: icicle-non-whitespace-string-p.  Use in icicle-*doc.
;; 2006/03/06 dadams
;;     Update doc strings of *-thesaurus*.
;; 2006/03/05 dadams
;;     Added: icicle-toggle-incremental-completion, toggle-icicle-incremental-completion.
;; 2006/03/03 dadams
;;     icicle-*doc: Clarified doc strings.  Updated prompts.
;;     Added: icicle-help-button.  Use in icicle-completion-help.
;; 2006/03/02 dadams
;;     icicle-insert-thesaurus-entry, icicle-complete-thesaurus-entry:
;;       Use synonyms-ensure-synonyms-read-from-cache.  Clarified doc strings.
;;     icicle-complete-thesaurus-entry: Error if no word at point.  Corrected looking-at regexp.
;; 2006/03/01 dadams
;;     Added: icicle-insert-thesaurus-entry, icicle-insert-thesaurus-entry-cand-fn,
;;            icicle-complete-thesaurus-entry.
;;     icicle-(previous|next)-(apropos|prefix)-candidate-action: Wrap in save-excursion.
;;     Use icicle-clear-minibuffer instead of icicle-erase-minibuffer non-interactively.
;;     icicle-erase-minibuffer: Use icicle-call-then-update-Completions.
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

(eval-when-compile (require 'cl)) ;; loop
                                  ;; plus, for Emacs < 21: dolist, pop, push
                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (when (>= emacs-major-version 22) (require 'edmacro))) ;; edmacro-subseq
(eval-when-compile (when (>= emacs-major-version 21) (require 'recentf))) ;; recentf-mode

(eval-when-compile (require 'dabbrev))
  ;; dabbrev-case-fold-search, dabbrev-upcase-means-case-search, dabbrev--last-obarray,
  ;; dabbrev--last-completion-buffer, dabbrev--last-abbreviation, dabbrev--check-other-buffers,
  ;; dabbrev-case-replace, dabbrev--reset-global-variables, dabbrev--abbrev-at-point,
  ;; dabbrev--minibuffer-origin, dabbrev--find-all-expansions, dabbrev--substitute-expansion
(eval-when-compile (require 'cus-edit))
  ;; custom-buffer-create, custom-buffer-order-groups, custom-sort-items
(eval-when-compile (require 'bookmark))
  ;; bookmark-all-names, bookmark-buffer-name, bookmark-current-bookmark
(eval-when-compile (require 'comint))
  ;; comint-prompt-regexp, comint-input-ring, comint-mode-map, comint-check-proc,
  ;; comint-copy-old-input, comint-send-input
(eval-when-compile (require 'imenu)) ;; imenu-syntax-alist
(eval-when-compile (require 'compile)) ;; compilation-find-buffer
(eval-when-compile (require 'info)) ;; Info-goto-node

(eval-when-compile (require 'synonyms nil t)) ;; (no error if not found):
  ;; synonyms-ensure-synonyms-read-from-cache, synonyms-obarray
(eval-when-compile (require 'misc-cmds nil t)) ;; (no error if not found):
  ;; kill-buffer-and-its-windows

(eval-when-compile (require 'icicles-mac)) ;; icicle-define-command, icicle-define-file-command
(eval-when-compile (require 'icicles-mode)) ;; icicle-completing-p
(eval-when-compile (require 'icicles-var)) ;; icicle-universal-argument-map

(require 'misc-fns nil t)   ;; (no error if not found): another-buffer
(require 'apropos-fn+var nil t) ;; (no error if not found): apropos-command, apropos-function,
                                ;; apropos-option, apropos-variable
(require 'pp+ nil t) ;; (no error if not found): pp-eval-expression
(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-find-a-file, diredp-find-a-file-other-window
(require 'frame-cmds nil t) ;; (no error if not found): delete-windows-on

;; Byte-compiling this file, you will likely get some byte-compiler warning messages.
;; These are probably benign - ignore them.  Icicles is designed to work with multiple
;; versions of Emacs, and that fact provokes compiler warnings.  If you get byte-compiler
;; errors (not warnings), then please report a bug, using `M-x icicle-send-bug-report'.

;;; Some defvars to quiet byte-compiler a bit:

(defvar recentf-list)
(defvar yow-file)
(defvar yow-load-message)
(defvar yow-after-load-message)
(defvar cookie-cache)
(when (< emacs-major-version 22)
  (defvar saved-overriding-map)
  (defvar overriding-map-is-bound))
(defvar icicle-track-pt) ;; Defined in icicle-insert-thesaurus-entry

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands -----------------------------------------------


;;; Redefined standard commands.............................





;;; REPLACE ORIGINAL `dabbrev-completion' defined in `dabbrev.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-dabbrev-completion)
(fset 'old-dabbrev-completion (symbol-function 'dabbrev-completion)))

;;;###autoload
(defun icicle-dabbrev-completion (&optional arg) ; Bound to `C-M-/' globally.
  "Completion on current word.
Like \\[dabbrev-expand], but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by
`dabbrev-friend-buffer-function', to find the completions.

If the prefix argument is 16 (which comes from `C-u C-u'), then it
searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already."
  (interactive "*P")
  (unless (featurep 'dabbrev)
    (unless (require 'dabbrev nil t) (error "Library `dabbrev' not found"))
    (icicle-mode 1))                    ; Redefine `dabbrev-completion' to Icicles version.
  (dabbrev--reset-global-variables)
  (let* ((dabbrev-check-other-buffers (and arg t))
         (dabbrev-check-all-buffers (and arg (= (prefix-numeric-value arg) 16)))
         (abbrev (dabbrev--abbrev-at-point))
         (ignore-case-p (and (if (eq dabbrev-case-fold-search 'case-fold-search)
                                 case-fold-search
                               dabbrev-case-fold-search)
                             (or (not dabbrev-upcase-means-case-search)
                                 (string= abbrev (downcase abbrev)))))
         (my-obarray dabbrev--last-obarray)
         init)
    ;; If new abbreviation to expand, then expand it.
    (save-excursion
      (unless (and (null arg)
                   my-obarray
                   (or (eq dabbrev--last-completion-buffer (current-buffer))
                       (and (window-minibuffer-p (selected-window))
                            (eq dabbrev--last-completion-buffer
                                (dabbrev--minibuffer-origin))))
                   dabbrev--last-abbreviation
                   (>= (length abbrev) (length dabbrev--last-abbreviation))
                   (string= dabbrev--last-abbreviation
                            (substring abbrev 0 (length dabbrev--last-abbreviation)))
                   (setq init (try-completion abbrev my-obarray)))
        (setq dabbrev--last-abbreviation abbrev)
        (let ((completion-list (dabbrev--find-all-expansions abbrev ignore-case-p))
              (completion-ignore-case ignore-case-p))
          ;; Make an obarray with all expansions
          (setq my-obarray (make-vector (length completion-list) 0))
          (unless (> (length my-obarray) 0)
            (error "No dynamic expansion for \"%s\" found%s" abbrev
                   (if dabbrev--check-other-buffers "" " in this-buffer")))
          (dolist (string completion-list)
            (cond ((or (not ignore-case-p) (not dabbrev-case-replace))
                   (intern string my-obarray))
                  ((string= abbrev (upcase abbrev))
                   (intern (upcase string) my-obarray))
                  ((string= (substring abbrev 0 1) (upcase (substring abbrev 0 1)))
                   (intern (capitalize string) my-obarray))
                  (t (intern (downcase string) my-obarray))))
          (setq dabbrev--last-obarray my-obarray)
          (setq dabbrev--last-completion-buffer (current-buffer))
          ;; Find the longest common string.
          (setq init (try-completion abbrev my-obarray)))))
    ;; Let the user choose between the expansions
    (unless (stringp init) (setq init abbrev))
    (cond
      ((and (not (string-equal init ""))
            (not (string-equal (downcase init) (downcase abbrev)))
            (<= (length (all-completions init my-obarray)) 1))
       (message "Completed (no other completions)")
       (if (< emacs-major-version 21)
           (dabbrev--substitute-expansion nil abbrev init)
         (dabbrev--substitute-expansion nil abbrev init nil))
       (when (window-minibuffer-p (selected-window)) (message nil)))
;;$$       ;; Complete text only up through the common root. NOT USED.
;;       ((and icicle-dabbrev-stop-at-common-root-p
;;             (not (string-equal init ""))
;;             (not (string-equal (downcase init) (downcase abbrev))))
;;        (message "Use `%s' again to complete further"
;;                 (icicle-key-description (this-command-keys)
;;                                         (not icicle-key-descriptions-use-<>-flag)))
;;        (if (< emacs-major-version 21)
;;            (dabbrev--substitute-expansion nil abbrev init)
;;          (dabbrev--substitute-expansion nil abbrev init nil))
;;        (when (window-minibuffer-p (selected-window)) (message nil))) ; $$ NEEDED?
      (t
       ;; String is a common root already.  Use Icicles completion.
       (message "Making completion list...")
       (search-backward abbrev)
       (replace-match "")
       (condition-case nil
           (let* ((icicle-show-Completions-initially-flag t)
                  (icicle-incremental-completion-p 'display)
                  (minibuffer-completion-table my-obarray)
                  (choice (completing-read "Complete: " my-obarray nil nil init nil init)))
             (when choice (insert choice)))
         (quit (insert abbrev)))))))


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


;;; REPLACE ORIGINAL `lisp-complete-symbol' defined in `lisp.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Selects *Completions* window even if on another frame.
;;;
(or (fboundp 'old-lisp-complete-symbol)
(fset 'old-lisp-complete-symbol (symbol-function 'lisp-complete-symbol)))

;;;###autoload
(defun icicle-lisp-complete-symbol ()   ; Bound to `ESC TAB' globally.
  "Complete the Lisp symbol preceding point against known Lisp symbols.
If no characters can be completed, display a list of possible completions.
Repeating the command at that point scrolls the list.

The context determines which symbols are considered.
If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table emacs-lisp-mode-syntax-table)
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (buffer-substring beg end))
	 (predicate
	  (if (eq (char-after (1- beg)) ?\()
	      'fboundp
	    (function (lambda (sym)
			(or (boundp sym) (fboundp sym)
			    (symbol-plist sym))))))
         (enable-recursive-minibuffers (active-minibuffer-window))
         (completion (completing-read "Complete Lisp symbol: " obarray predicate t pattern nil)))
    (delete-region beg end)
    (insert completion)))


;;; REPLACE ORIGINAL `minibuffer-complete-and-exit' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Removes *Completion* window.
;;;
(or (fboundp 'old-minibuffer-complete-and-exit)
(fset 'old-minibuffer-complete-and-exit (symbol-function 'minibuffer-complete-and-exit)))

;; Bound to `C-m' and `\n' in `minibuffer-local-must-match-map'.
;;;###autoload
(defun icicle-minibuffer-complete-and-exit ()
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



;;; REPLACE ORIGINAL `customize-apropos' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos)
(fset 'old-customize-apropos (symbol-function 'customize-apropos)))

;;;###autoload
(defun icicle-customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which
  are not user-settable, as well as faces and groups.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize (regexp): " obarray
                            (lambda (symbol)
                              (or (get symbol 'custom-group)
                                  (custom-facep symbol)
                                  (and (boundp symbol)
                                       (or (get symbol 'saved-value)
                                           (custom-variable-p symbol)
                                           (if (null pref-arg)
                                               (user-variable-p symbol)
                                             (get symbol 'variable-documentation))))))
                            nil nil 'regexp-history)
           pref-arg)))
  (let ((found nil))
    (mapatoms (lambda (symbol)
                (when (string-match regexp (symbol-name symbol))
                  (when (and (not (memq all '(faces options))) ; groups or t
                             (get symbol 'custom-group))
                    (push (list symbol 'custom-group) found))
                  (when (and (not (memq all '(options groups))) ; faces or t
                             (custom-facep symbol))
                    (push (list symbol 'custom-face) found))
                  (when (and (not (memq all '(groups faces))) ; options or t
                             (boundp symbol)
                             (or (get symbol 'saved-value)
                                 (custom-variable-p symbol)
                                 (if (memq all '(nil options))
                                     (user-variable-p symbol)
                                   (get symbol 'variable-documentation))))
                    (push (list symbol 'custom-variable) found)))))
    (if (not found)
        (error "No matches")
      (custom-buffer-create (custom-sort-items found t custom-buffer-order-groups)
                            "*Customize Apropos*"))))

;; Define this for Emacs 20 and 21
(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Return non-nil if VARIABLE is a custom variable."
    (or (get variable 'standard-value) (get variable 'custom-autoload))))



;;; REPLACE ORIGINAL `customize-apropos-faces' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-faces)
(fset 'old-customize-apropos-faces (symbol-function 'customize-apropos-faces)))

;;;###autoload
(defun icicle-customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize faces (regexp): " obarray 'custom-facep nil nil
                          'regexp-history)))
  (customize-apropos regexp 'faces))



;;; REPLACE ORIGINAL `customize-apropos-groups' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-groups)
(fset 'old-customize-apropos-groups (symbol-function 'customize-apropos-groups)))

;;;###autoload
(defun icicle-customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP.
Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (list (completing-read "Customize groups (regexp): " obarray
                          (lambda (symbol) (get symbol 'custom-group)) nil nil 'regexp-history)))
  (customize-apropos regexp 'groups))



;;; REPLACE ORIGINAL `customize-apropos-options' defined in `cus-edit.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the regexp.
;;;
(or (fboundp 'old-customize-apropos-options)
(fset 'old-customize-apropos-options (symbol-function 'customize-apropos-options)))

;;;###autoload
(defun icicle-customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable.

Use `S-TAB', [next], and [prior], to match regexp input - this lets
you see what items will be available in the customize buffer."
  (interactive
   (let ((pref-arg current-prefix-arg))
     (list (completing-read "Customize options (regexp): " obarray
                            (lambda (symbol)
                              (and (boundp symbol)
                                   (or (get symbol 'saved-value)
                                       (custom-variable-p symbol)
                                       (if (null pref-arg)
                                           (user-variable-p symbol)
                                         (get symbol 'variable-documentation)))))
                            nil nil 'regexp-history)
           pref-arg)))
  (customize-apropos regexp (or arg 'options)))



;;; REPLACE ORIGINAL `repeat-complex-command' defined in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Uses `completing-read' to read the command to repeat, letting you use `S-TAB' and
;;; `TAB' to see the history list and `C-,' to toggle sorting that display.
;;;
(or (fboundp 'old-repeat-complex-command)
(fset 'old-repeat-complex-command (symbol-function 'repeat-complex-command)))

;;;###autoload
(defun icicle-repeat-complex-command (arg)
  "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one which used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous command
it is added to the front of the command history.
You can use the minibuffer history commands \\<minibuffer-local-map>\\[next-history-element] and \
\\[previous-history-element]
to get different commands to edit and resubmit.

Use `S-TAB', [next], and [prior], to match regexp input - this gives
you the functionality of `repeat-matching-complex-command'."
  (interactive "p")
  (let ((elt (nth (1- arg) command-history))
        (icicle-sort-function nil)
        newcmd)
    (if elt
        (progn
          (setq newcmd
                (let ((print-level nil)
                      (minibuffer-history-position arg)
                      (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                  (unwind-protect
                       (read (completing-read
                              "Redo: " (mapcar (lambda (entry) (list (prin1-to-string entry)))
                                               command-history)
                              nil nil (prin1-to-string elt) (cons 'command-history arg)
                              (prin1-to-string elt)))
                    ;; If command was added to command-history as a string, get rid of that.
                    ;; We want only evaluable expressions there.
                    (if (stringp (car command-history))
                        (setq command-history (cdr command-history))))))
          ;; If command to be redone does not match front of history, add it to the history.
          (or (equal newcmd (car command-history))
              (setq command-history (cons newcmd command-history)))
          (eval newcmd))
      (if command-history
          (error "Argument %d is beyond length of command history" arg)
        (error "There are no previous complex commands to repeat")))))



 
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

(when (fboundp 'define-button-type)     ; Defined in `button.el'.
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

;; This is just the macro expansion of the following:
;; `(def-completion-wrapper icicle-abort-minibuffer-input :minibuffer-separator)'.
;; Taken from the definition of `def-completion-wrapper' in `completion.el'.
(put 'icicle-abort-minibuffer-input 'completion-function 'use-completion-minibuffer-separator)

(defun icicle-ensure-overriding-map-is-bound ()
  "Check `overriding-terminal-local-map' is `icicle-universal-argument-map'."
  (unless overriding-map-is-bound
    (setq saved-overriding-map overriding-terminal-local-map)
    (setq overriding-terminal-local-map icicle-universal-argument-map)
    (setq overriding-map-is-bound t)))

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
(defun icicle-apropos-complete-and-exit () ; Bound to `S-RET' in minibuffer for must-match.
  "If the minibuffer contents is a valid apropos completion, then exit.
Otherwise try to complete it.  If completion leads to a valid
completion, then exit.
This is to `minibuffer-complete-and-exit' as `icicle-apropos-complete'
is to `minibuffer-complete'.  That is, it is the regexp-match version."
  ;; Bound to `S-RET' in `minibuffer-local-must-match-map'.
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

;; $$ No longer used.
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
         (let ((thing ""))
           (save-excursion
             (set-buffer (cadr (buffer-list)))
             (setq thing (funcall (nth icicle-thing-at-pt-fns-pointer alt-fns))))
           (icicle-insert-thing thing)))

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
            icicle-last-input ; $$ Previously didn't allow the -action's.
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
           (insert
            (setq icicle-last-completion-candidate
                  (if (and (icicle-file-name-input-p) insert-default-directory)
                      (expand-file-name (car icicle-completion-candidates)
                                        (icicle-file-name-directory-w-default
                                         icicle-current-input))
                    (car icicle-completion-candidates))))
           (when (icicle-file-directory-p (expand-file-name icicle-last-completion-candidate))
             (setq icicle-default-directory (expand-file-name icicle-last-completion-candidate))))
         (save-selected-window (icicle-remove-Completions-window))
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
           (when (icicle-file-directory-p icicle-last-completion-candidate)
             (setq icicle-default-directory (expand-file-name icicle-last-completion-candidate)))
           (when (member icicle-current-input icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (cond ((get-buffer-window "*Completions*" 0)
                  (if (and (eq icicle-last-completion-command 'icicle-prefix-complete)
                           (memq last-command '(icicle-prefix-complete handle-switch-frame)))
                      ;; Second TAB in a row.  Scroll window around.
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
           (insert
            (setq icicle-last-completion-candidate
                  (if (and (icicle-file-name-input-p) insert-default-directory)
                      (expand-file-name
                       (car icicle-completion-candidates)
                       (icicle-file-name-directory-w-default icicle-current-input))
                    (car icicle-completion-candidates))))
           (when (icicle-file-directory-p (expand-file-name icicle-last-completion-candidate))
             (setq icicle-default-directory (expand-file-name icicle-last-completion-candidate))))
         (save-selected-window (icicle-remove-Completions-window))
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
             (setq icicle-default-directory (expand-file-name icicle-last-completion-candidate)))
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
(defun icicle-switch-to/from-minibuffer () ; Bound to `pause' in minibuffer.
  "Switch to minibuffer or previous buffer, in other window.
If current buffer is the minibuffer, then switch to the buffer that
was previously current.  Otherwise, switch to the minibuffer."
  (interactive)
  (when (active-minibuffer-window)
    (if (eq (selected-window) (active-minibuffer-window))
        (switch-to-buffer-other-window icicle-pre-minibuffer-buffer)
      (select-window (active-minibuffer-window)))))


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
  (let ((icicle-candidate-action-fn nil))
    (icicle-mouse-candidate-action event)))

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
 buffer name - modes described using `describe-mode' (Emacs > 20).

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
    (cond (cand-symb
           (cond ((functionp cand-symb) (describe-function cand-symb))
                 ((boundp cand-symb) (describe-variable cand-symb))
                 ((facep cand-symb) (describe-face cand-symb))
                 ((symbol-plist cand-symb) (apropos-describe-plist cand-symb))
                 (t (icicle-msg-maybe-in-minibuffer "No help"))))
          (t                            ; Not a symbol - treat string itself.
           (cond ((and (bufferp (get-buffer icicle-last-completion-candidate))
                       (condition-case nil ; Emacs 21+ `describe-mode' takes arg; not Emacs 20
                           (describe-mode icicle-last-completion-candidate)
                         (wrong-number-of-arguments nil))))
                 (t (icicle-msg-maybe-in-minibuffer "No help")))))
    (icicle-raise-Completions-frame)
    ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
    ;; because the *Help* frame takes the focus away from the minibuffer frame.
    ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
    (let* ((help-window (get-buffer-window "*Help*" t))
           (help-frame (and help-window (window-frame help-window))))
      (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
  (message nil))                        ; Let minibuffer contents show immmediately.

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
(defun icicle-narrow-candidates ()      ; Bound to `M-*' in minibuffer.
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
           (let ((result (completing-read "Match also (regexp): "
                                  (mapcar #'list icicle-completion-candidates) nil nil nil
                                  'regexp-history)))
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
  "Define the set of current completion candidates by evaluating a sexpr.
The sexpr must evaluate to a list of strings, such as is returned by
`all-completions'.

You can use this command at top level or from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-define]')."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (evald-sexpr (eval-minibuffer "Set the completion candidates to sexpr (eval): ")))
    (if (or (null evald-sexpr) (and (consp evald-sexpr) (stringp (car evald-sexpr))))
        (setq icicle-completion-candidates evald-sexpr)
      (error "Sexpr did not evaluate to a list of strings: %S" evald-sexpr)))
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
(defun icicle-add/update-saved-completion-set ()
  "Add or update an entry in `icicle-saved-completion-sets'.
You are prompted for the name of a set of completion candidates and
its cache file.  List `icicle-saved-completion-sets' is updated to
have an entry with these set and file names.
Returns the cache-file name."
  (interactive)
  (let* ((set-name (completing-read "Name of completion-candidates set: "
                                    icicle-saved-completion-sets nil nil nil
                                    'icicle-completion-set-history
                                    (caar icicle-saved-completion-sets)))
         (file-name ""))
    (while (not (icicle-file-writable-p file-name))
      (setq file-name (expand-file-name
                       (read-file-name "Cache file for the set: " default-directory nil nil
                                       (concat "icicles-"
                                               (icicle-delete-whitespace-from-string set-name)
                                               ".cache")))))
    (setq icicle-saved-completion-sets  ; Remove any old definition of this set.
          (delete (assoc set-name icicle-saved-completion-sets) icicle-saved-completion-sets))
    (push (cons set-name file-name) icicle-saved-completion-sets) ; Add new set definition.
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Set `%s' was added/updated in `icicle-saved-completion-sets'" set-name)
    file-name))                         ; Return cache-file name.

;;;###autoload
(icicle-define-command icicle-remove-saved-completion-set ; Command name
                       "Remove an entry from `icicle-saved-completion-sets'.
This does not remove the associated cache file." ; Doc string
                       (lambda (set-name) ; Action function
                         (setq icicle-saved-completion-sets
                               (delete (assoc set-name icicle-saved-completion-sets)
                                       icicle-saved-completion-sets))
                         (customize-save-variable 'icicle-saved-completion-sets
                                                  icicle-saved-completion-sets)
                         (message "Candidate set `%s' removed" set-name))
                       "Remove set of completion candidates named: " ; `completing-read' args
                       icicle-saved-completion-sets nil t nil 'icicle-completion-set-history
                       (caar icicle-saved-completion-sets))

;;;###autoload
(defun icicle-dired-saved-file-candidates (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (kill-buffer existing-dired-buffer))
    (dired (cons default-directory icicle-saved-completion-candidates))))

;;;###autoload
(defun icicle-dired-saved-file-candidates-other-window (prompt-for-dir-p)
  "Open Dired on the set of completion candidates saved with \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]'.
Open in another window.
With a prefix argument, you are prompted for the directory."
  (interactive "P")
  (unless icicle-saved-completion-candidates
    (error "No saved completion candidates.  Use \\<minibuffer-local-completion-map>\
`\\[icicle-candidate-set-save]' to save candidates"))
  (let* ((default-directory (if prompt-for-dir-p
                                (read-file-name "Directory: " nil default-directory nil)
                              default-directory))
         (existing-dired-buffer (get-buffer (file-name-nondirectory
                                             (directory-file-name default-directory)))))
    (unless (file-exists-p (car icicle-saved-completion-candidates))
      (error "Bad directory? No file `%s' in `%s'"
             (car icicle-saved-completion-candidates) default-directory))
    (when (and existing-dired-buffer
               (y-or-n-p (format "Replace existing Dired buffer `%s'? "
                                 (buffer-name existing-dired-buffer))))
      (if (fboundp 'kill-buffer-and-its-windows)
          (kill-buffer-and-its-windows existing-dired-buffer) ; Defined in `misc-cmds.el'
        (kill-buffer existing-dired-buffer)))
    (dired-other-window (cons default-directory icicle-saved-completion-candidates))))

;;;###autoload
(defun icicle-keep-only-past-inputs ()  ; Bound to`M-pause' in minibuffer.
  "Reduce completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping only
those that have been used before.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
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
                   ((null (cdr icicle-completion-candidates)) ; Single candidate. Update minibuffer
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
                    (if (get-buffer-window "*Completions*" 0)
                        (if (and (eq icicle-last-completion-command 'icicle-keep-only-past-inputs)
                                 (memq last-command
                                       '(icicle-keep-only-past-inputs handle-switch-frame)))
                            ;; Second `S-TAB' in a row.  Scroll window around.
                            (icicle-scroll-Completions)
                          ;; Did something else (e.g. changed input).  Update the display.
                          (icicle-display-candidates-in-Completions))
                      ;; No window yet.  Show window.
                      (icicle-display-candidates-in-Completions))
                    (save-window-excursion
                      (select-window (active-minibuffer-window))
                      (minibuffer-message "  [Filtered to (matching) historical candidates]"))))
             (setq icicle-last-completion-command 'icicle-keep-only-past-inputs))))
    icicle-completion-candidates))

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

(defun icicle-remove-Completions-window () ; Bound to `C-x 0' in the minibuffer.
  "Remove the *Completions* window."
  (interactive) (icicle-delete-windows-on "*Completions*"))




 
;;; Commands to be used mainly at top level  . . . . . . . .


;;; Icicles multi-commands .   .   .   .   .   .   .   .   .

(icicle-define-command icicle-execute-extended-command ; Bound to `M-x' in Icicle mode.
                       "Read command name, then read its arguments and call it.
This is `execute-extended-command', turned into a multi-command." ; Doc string
                       icicle-execute-extended-command-1 ; Function to perform the action
                       (format "Execute command%s: " ; `completing-read' args
                               (if current-prefix-arg
                                   (format " (prefix %d)"
                                           (prefix-numeric-value current-prefix-arg))
                                 ""))
                       obarray 'commandp t nil 'extended-command-history nil nil
                       ((last-cmd last-command))) ; Bindings: save the last command.

(defun icicle-execute-extended-command-1 (cmd-name)
  "Candidate action function for `icicle-execute-extended-command'."
  (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars.
  (when (window-live-p orig-window) (select-window orig-window))

  ;; Rebind `icicle-candidate-action-fn' to a function that calls the
  ;; candidate CMD-NAME on a single argument that it reads.  This is
  ;; used only if CMD-NAME is a command that, itself, reads an input
  ;; argument with completion.  When that is the case, you can use
  ;; completion on that input, and if you do that, you can use `C-RET'
  ;; to use command CMD-NAME as a multi-command.  In other words, this
  ;; binding allows for two levels of multi-commands.
  (let* ((cmd (intern cmd-name))
         (icicle-candidate-action-fn
          (lambda (arg) 
            (condition-case nil
                (funcall cmd arg)       ; Try to use string candidate `arg'.
              (wrong-type-argument      ; If that didn't work, use a symbol or number candidate.
               (funcall cmd (car (read-from-string arg)))))
            (select-frame-set-input-focus (window-frame (minibuffer-window))))))
    (setq last-command last-cmd)        ; Restore last real command. `last-cmd' is free here.
    (let ((this-command cmd)            ; Establish this command.
          (fn (symbol-function cmd))
          (count (prefix-numeric-value current-prefix-arg)))
      (if (not (arrayp fn))
          (call-interactively cmd 'record-it)
        (execute-kbd-macro fn count)
        (when (> count 1) (message "(%d times)" count))))))

(icicle-define-command icicle-execute-named-keyboard-macro ; Bound to `C-x M-e' in Icicle mode.
                       "Read the name of a keyboard macro, then execute it."
                       icicle-execute-extended-command-1 ; Function to perform the action
                       (format "Execute keyboard macro%s: " ; `completing-read' args
                               (if current-prefix-arg
                                   (format " (prefix %d)"
                                           (prefix-numeric-value current-prefix-arg))
                                 ""))
                       obarray (lambda (fn) (and (commandp fn) (arrayp (symbol-function fn))))
                       t nil 'icicle-kmacro-history nil nil
                       ((last-cmd last-command))) ; Bindings: save the last command.

(when (boundp 'kmacro-ring)             ; Bound to `f5' in Icicle mode (Emacs 22).
  (icicle-define-command icicle-kmacro
                         "Execute a keyboard macro according to its position in `kmacro-ring'.
Macros in the keyboard macro ring are given names \"macro #1\",
\"macro #2\", and so on, for use as completion candidates."
                         icicle-kmacro-action ; Function to perform the action
                         (format "Execute keyboard macro%s: " ; `completing-read' args
                                 (if current-prefix-arg
                                     (format " (prefix %d)"
                                             (prefix-numeric-value current-prefix-arg))
                                   ""))
                         (let ((count 0))
                           (setq icicle-kmacro-alist
                                 (mapcar (lambda (x)
                                           (cons (format "macro #%d" (setq count (1+ count))) x))
                                         (nreverse (cons (kmacro-ring-head) kmacro-ring)))))
                         nil t nil 'icicle-kmacro-history nil nil)

  (defun icicle-kmacro-action (cand)
    "Action function for `icicle-kmacro'."
    (when (get-buffer orig-buff) (set-buffer orig-buff)) ; `orig-buff', `orig-window' are free vars
    (when (window-live-p orig-window) (select-window orig-window))
    (let ((count (prefix-numeric-value current-prefix-arg)))
      (execute-kbd-macro (cadr (assoc cand icicle-kmacro-alist)) count
                         #'kmacro-loop-setup-function)
      (when (> count 1) (message "(%d times)" count)))))

(icicle-define-command icicle-set-option-to-t ; Command name
                       "Set option to t.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
                       (lambda (opt)    ; Function to perform the action
                         (set (intern opt) t) (message "`%s' is now t" opt))
                       "Set option to t: " obarray ; `completing-read' args
                       (cond ((and current-prefix-arg
                                   (wholenump (prefix-numeric-value current-prefix-arg)))
                              'user-variable-p)
                             (current-prefix-arg 'boundp)
                             (t 'icicle-binary-option-p))
                       'must-confirm nil 'icicle-variable-history)

(defalias 'icicle-clear-option 'icicle-reset-option-to-nil)

(icicle-define-command icicle-reset-option-to-nil ; Command name
                       "Set option to nil.  This makes sense for binary and list options.
By default, the set of completion candidates is limited to user
options.  Note: it is *not* limited to binary and list options.
With a prefix arg, all variables are candidates." ; Doc string
                       (lambda (opt)    ; Function to perform the action
                         (set (intern opt) nil) (message "`%s' is now nil" opt))
                       "Clear option (set it to nil): " obarray ; `completing-read' args
                       (if current-prefix-arg 'boundp 'user-variable-p) t nil
                       'icicle-variable-history)

(icicle-define-command icicle-toggle-option ; Command name
                       "Toggle option's value.  This makes sense for binary (toggle) options.
By default, completion candidates are limited to user options that
have `boolean' custom types.  However, there are many \"binary\" options
that allow other non-nil values than t.

You can use a prefix argument to change the set of completion
candidates, as follows:

 - With a non-negative prefix arg, all user options are candidates.
 - With a negative prefix arg, all variables are candidates." ; Doc string
                       (lambda (opt)    ; Function to perform the action
                         (let ((sym (intern opt)))
                           (set sym (not (eval sym))) (message "`%s' is now %s" opt (eval sym))))
                       "Toggle value of option: " obarray ; `completing-read' args
                       (cond ((and current-prefix-arg
                                   (wholenump (prefix-numeric-value current-prefix-arg)))
                              'user-variable-p)
                             (current-prefix-arg 'boundp)
                             (t 'icicle-binary-option-p))
                       'must-confirm nil 'icicle-variable-history)

(defun icicle-binary-option-p (symbol)
  "Non-nil if SYMBOl is a user option that has custom-type `boolean'."
  (eq (get symbol 'custom-type) 'boolean))

(icicle-define-command icicle-bookmark  ; Command name
                       "Jump to a bookmark." ; Doc string
                       bookmark-jump    ; Function to perform the action
                       "Bookmark: " (mapcar #'list (bookmark-all-names)) ; `completing-read' args
                       nil t nil 'icicle-bookmark-history
                       (or (and (boundp 'bookmark-current-bookmark)
                                bookmark-current-bookmark)
                           (bookmark-buffer-name)))

(defun icicle-other-window-or-frame (arg) ; Bound to `C-x o' in Icicle mode.
  "`other-window'/`other-frame', or, with `C-u 0', multi-command version.
With non-zero prefix argument, this is `other-window', or `other-frame'
if `one-window-p'.

With a zero prefix argument, this is `icicle-select-window', or
`icicle-select-frame' if `one-window-p'."
  (interactive "p")
  (if (zerop arg)
      (if (one-window-p) (icicle-select-frame) (icicle-select-window))
    (if (one-window-p) (other-frame arg) (other-window arg))))

(icicle-define-command icicle-select-frame ; Bound to `C-x 5 o' in Icicle mode.
                       "Select frame by name and raise it." ; Doc string
                       select-frame-by-name ; Function to perform the action
                       "Select frame: " ; `completing-read' args
                       (make-frame-names-alist) nil t nil 'frame-name-history
                       (cdr (assq 'name (frame-parameters (next-frame (selected-frame))))) t)

(icicle-define-command icicle-select-window ; Command name
                       "Select window by its buffer name." ; Doc string
                       (lambda (buf) (select-window (get-buffer-window buf))) ; Action function
                       "Select window: " ; `completing-read' args
                       (let ((bufs (buffer-list (selected-frame)))
                             (cand-bufs nil))
                         (dolist (buf bufs)
                           (when (get-buffer-window buf)
                             (push (list (buffer-name buf)) cand-bufs)))
                         cand-bufs)
                       nil t nil 'buffer-name-history
                       (buffer-name (window-buffer (other-window 1)))
                       t)

(icicle-define-command icicle-delete-windows ; Command name
                       "Delete windows showing a buffer, anywhere." ; Doc string
                       delete-windows-on ; Function to perform the action
                       "Delete windows on buffer: "
                       (let ((all-bufs (buffer-list))
                             (cand-bufs nil))
                         (dolist (buf all-bufs)
                           (when (get-buffer-window buf t)
                             (push (list (buffer-name buf)) cand-bufs)))
                         cand-bufs)
                       nil t nil 'buffer-name-history (buffer-name (current-buffer)) t)

(defun icicle-delete-window (bufferp)   ; Bound to `C-x 0' in Icicle mode.
  "`delete-window' or prompt for buffer and delete all its windows.
With no prefix arg, delete the selected window.
With a prefix arg, prompt for a buffer and delete all windows, on any
  frame, that show that buffer.

With a prefix arg, this is an Icicles multi-command - see
`icicle-mode'.  Input-candidate completion and cycling are available.
While cycling, these keys act on the current candidate:

`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next prefix-completion candidate
`C-prior' - Act, then move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`C-!'     - Act on *all* candidates, successively (careful!)

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit."
  (interactive "P")
  (if bufferp (icicle-delete-windows) (delete-window)))

(icicle-define-command icicle-kill-buffer ; Bound to `C-x k' in Icicle mode.
                       "Kill a buffer." ; Doc string
                       icicle-kill-a-buffer ; Function to perform the action
                       "Kill buffer: "  ; `completing-read' args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil t nil 'buffer-name-history
                       (buffer-name (current-buffer))
                       nil
                       ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
                        (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
                        (icicle-must-pass-predicate icicle-buffer-predicate)
                        (icicle-extra-candidates icicle-buffer-extras)
                        (icicle-sort-function icicle-buffer-sort)
                        (icicle-require-match-flag icicle-buffer-require-match-flag)
                        (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

(defun icicle-kill-a-buffer (buf)
  "Kill buffer BUF."
  (setq buf (get-buffer buf))
  (if buf
      (condition-case err
          (if (not (buffer-live-p buf))
              (message "Buffer already deleted: `%s'" buf)
            (if (fboundp 'kill-buffer-and-its-windows)
                (kill-buffer-and-its-windows buf) ; Defined in `misc-cmds.el'.
              (kill-buffer buf))
            ;; Update the set of completions, then update *Completions*.
            (setq minibuffer-completion-table (mapcar (lambda (buf) (list (buffer-name buf)))
                                                      (buffer-list)))
            (icicle-update-completions))
        (error nil))
    (message "No such live buffer: `%s'" buf)))

(icicle-define-command icicle-buffer ; Bound to `C-x b' in Icicle mode.
                       "Switch to a different buffer.
These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'." ; Doc string
                       switch-to-buffer ; Function to perform the action
                       "Switch to buffer: " ; `completing-read' args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history
                       (buffer-name (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer))))
                       nil
                       ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
                        (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
                        (icicle-must-pass-predicate icicle-buffer-predicate)
                        (icicle-extra-candidates icicle-buffer-extras)
                        (icicle-sort-function icicle-buffer-sort)
                        (icicle-require-match-flag icicle-buffer-require-match-flag)
                        (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

(icicle-define-command icicle-buffer-other-window ; Bound to `C-x 4 b' in Icicle mode.
                       "Switch to a different buffer in another window.
These options, when non-nil, control candidate matching and filtering:

 `icicle-buffer-ignore-space-prefix-flag' - Ignore space-prefix names
 `icicle-buffer-extras'             - Extra buffers to display
 `icicle-buffer-match-regexp'       - Regexp that buffers must match
 `icicle-buffer-no-match-regexp'    - Regexp buffers must not match
 `icicle-buffer-predicate'          - Predicate buffer must satisfy
 `icicle-buffer-sort'               - Sort function for candidates

For example, to show only buffers that are associated with files, set
`icicle-buffer-predicate' to (lambda (buf) (buffer-file-name buf)).

Option `icicle-buffer-require-match-flag' can be used to override
option `icicle-require-match-flag'.

See also command `icicle-buffer-config'" ; Doc string
                       switch-to-buffer-other-window ; Function to perform the action
                       "Switch to buffer: " ; `completing-read' args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history
                       (buffer-name (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer))))
                       nil
                       ((icicle-must-match-regexp icicle-buffer-match-regexp) ; Additional bindings
                        (icicle-must-not-match-regexp icicle-buffer-no-match-regexp)
                        (icicle-must-pass-predicate icicle-buffer-predicate)
                        (icicle-extra-candidates icicle-buffer-extras)
                        (icicle-sort-function icicle-buffer-sort)
                        (icicle-require-match-flag icicle-buffer-require-match-flag)
                        (icicle-ignore-space-prefix-flag icicle-buffer-ignore-space-prefix-flag)))

(icicle-define-command icicle-add-buffer-candidate ; Command name
                       "Add buffer as an always-show completion candidate.
This just adds the buffer to `icicle-buffer-extras'." ; Doc string
                       (lambda (buf)
                         (add-to-list 'icicle-buffer-extras buf) ; Action function
                         (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
                         (message "Buffer `%s' added to always-show buffers" buf))
                       "Buffer candidate to show always: " ; `completing-read' args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history
                       (buffer-name (if (fboundp 'another-buffer)
                                        (another-buffer nil t)
                                      (other-buffer (current-buffer)))))

(icicle-define-command icicle-remove-buffer-candidate ; Command name
                       "Remove buffer as an always-show completion candidate.
This just removes the buffer from `icicle-buffer-extras'." ; Doc string
                       (lambda (buf)    ; Action function
                         (setq icicle-buffer-extras (delete buf icicle-buffer-extras))
                         (customize-save-variable 'icicle-buffer-extras icicle-buffer-extras)
                         (message "Buffer `%s' removed from always-show buffers" buf))
                       "Remove buffer from always-show list: " ; `completing-read' args
                       (mapcar #'list icicle-buffer-extras)
                       nil t nil 'buffer-name-history
                       (car icicle-buffer-extras))

(icicle-define-command icicle-buffer-config ; Command name
                       "Choose a configuration of user options for `icicle-buffer'.
See user option `icicle-buffer-configs'.  See also commands
`icicle-add-buffer-config' and `icicle-remove-buffer-config'." ; Doc string
                       (lambda (config-name) ; Function to perform the action
                         (let ((config (assoc config-name icicle-buffer-configs)))
                           (setq icicle-buffer-match-regexp (elt config 1))
                           (setq icicle-buffer-no-match-regexp (elt config 2))
                           (setq icicle-buffer-predicate (elt config 3))
                           (setq icicle-buffer-extras (elt config 4))
                           (setq icicle-buffer-sort (elt config 5))))
                       "Configuration: " icicle-buffer-configs nil t nil ; `completing-read' args
                       'icicle-buffer-config-history)

(defun icicle-add-buffer-config ()
  "Add buffer configuration to `icicle-buffer-configs'.
You are prompted for the buffer configuration components.
For the list of extra buffers to always display, you can choose them
using `C-mouse-2', `C-RET', and so on, just as you would make any
Icicles multiple choice."
  (interactive)
  (let ((name (read-from-minibuffer "Add buffer configuration.  Name: "))
        (match-regexp (icicle-read-from-minibuf-nil-default
                       "Regexp to match: " nil nil nil nil icicle-buffer-match-regexp))
        (nomatch-regexp (icicle-read-from-minibuf-nil-default
                         "Regexp not to match: " nil nil nil nil icicle-buffer-no-match-regexp))
        (pred (icicle-read-from-minibuf-nil-default "Predicate to satify: " nil nil nil nil
                                                    icicle-buffer-predicate))
        (extras (progn (message "Choose extra buffers to show...") (sit-for 1)
                       (icicle-buffer-list)))
        (sort-fn (icicle-read-from-minibuf-nil-default "Sort function: " nil nil t nil
                                                       (symbol-name icicle-buffer-sort))))
    (add-to-list 'icicle-buffer-configs
                 (list name match-regexp nomatch-regexp pred extras sort-fn))
    (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
    (message "Buffer configuration `%s' added" (caar icicle-buffer-configs))))

(defun icicle-read-from-minibuf-nil-default (prompt &optional initial-contents keymap read hist
                                             default-value inherit-input-method)
  "Like `read-from-minibuffer', but return nil for empty input.
Args are as for `read-from-minibuffer'.
If nothing is input, then nil is returned."
  (let ((input (read-from-minibuffer prompt initial-contents keymap nil hist default-value
                                     inherit-input-method)))
    (if (string= "" input)
        nil
      (if read
          (car (read-from-string input))
        input))))

(icicle-define-command icicle-buffer-list ; Command name
                       "Choose a list of buffer names.
The list of names (strings) is returned." ; Doc string
                       (lambda (name) (push name buf-names)) ; Function to perform the action
                       "Choose buffer (`RET' when done): " ; `completing-read' args
                       (mapcar (lambda (buf) (list (buffer-name buf))) (buffer-list))
                       nil nil nil 'buffer-name-history nil nil
                       ((buf-names nil)) ; Additional bindings
                       nil nil          ; First code, undo code
                       (delete "" buf-names)) ; Last code

(icicle-define-command icicle-remove-buffer-config ; Command name
                       "Remove buffer configuration from `icicle-buffer-configs'." ; Doc string
                       (lambda (config-name) ; Action function
                         (setq icicle-buffer-configs
                               (delete (assoc config-name icicle-buffer-configs)
                                       icicle-buffer-configs))
                         (customize-save-variable 'icicle-buffer-configs icicle-buffer-configs)
                         (message "Buffer configuration `%s' removed" config-name))
                       "Remove buffer configuration: " ; `completing-read' args
                       (mapcar (lambda (config) (list (car config))) icicle-buffer-configs)
                       nil t nil 'icicle-buffer-config-history (caar icicle-buffer-configs))

(icicle-define-command icicle-color-theme ; Command name
                       "Change color theme. ; Doc string
To use this command, you must have loaded library `color-theme.el',
available from http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme." ; Doc string
                       (lambda (theme) (funcall (intern theme))) ; Action - just call the theme.
                       "Theme: " icicle-color-themes nil t nil ; `completing-read' args
                       'icicle-color-theme-history)

(icicle-define-command icicle-insert-kill "Insert previously killed text from the `kill-ring'.
This is like `yank', but it does not rotate the `kill-ring'."
                       icicle-insert-for-yank
                       "Insert: " (mapcar #'list kill-ring) nil t nil 'icicle-kill-history
                       (car kill-ring) nil
                       ((icicle-transform-function 'icicle-remove-duplicates))) ; Bindings

(defun icicle-insert-for-yank (string)
  "`insert-for-yank', if defined; else, `insert' with `read-only' removed."
  (if (fboundp 'insert-for-yank)        ; Defined in `subr.el' (not required).
      (insert-for-yank string)
    (let ((opoint (point)))
      (insert string)
      (let ((inhibit-read-only t)) (remove-text-properties opoint (point) '(read-only nil))))))

(defun icicle-yank-insert (&optional arg) ;  Bound to `C-y' (or whatever `yank' was bound to).
  "`icicle-insert-kill', if prefix arg < 0; otherwise, `yank'."
  (interactive "*P")
  (if (wholenump (prefix-numeric-value arg)) (yank arg) (icicle-insert-kill)))

(icicle-define-file-command icicle-delete-file ; Command name
                            "Delete a file or directory." ; Doc string
                            icicle-delete-file-or-directory ; Function to perform the action
                            "Delete file or directory: " ; `read-file-name' args
                            default-directory nil t)

(defun icicle-delete-file-or-directory (file)
  "Delete file (or directory) FILE."
  (condition-case i-delete-file
      (if (eq t (car (file-attributes file)))
          (delete-directory file)
        (delete-file file))
    (error (message (error-message-string i-delete-file))
           (error (error-message-string i-delete-file)))))

(icicle-define-file-command icicle-find-file ; Bound to `C-x C-f' in Icicle mode.
                            "Visit a file or directory." ; Doc string
                            icicle-find-file-w-wildcards ; Function to perform the action
                            "File or directory: " nil ; `read-file-name' args
                            (if (and (eq major-mode 'dired-mode)
                                     (fboundp 'diredp-find-a-file)) ; Defined in `dired+.el'.
                                (condition-case nil ; E.g. error because not on file line (ignore)
                                    (abbreviate-file-name (dired-get-file-for-visit))
                                  (error default-directory))
                              default-directory))

(defun icicle-find-file-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file))
      (diredp-find-a-file filename t)
    (find-file filename t)))

(icicle-define-file-command icicle-find-file-other-window ; Bound to `C-x 4 f' in Icicle mode.
                            "Visit a file or directory in another window." ; Doc string
                            icicle-find-file-other-window-w-wildcards ; Action function
                            "File or directory: " nil ; `read-file-name' args
                            (if (and (eq major-mode 'dired-mode)
                                     (fboundp 'diredp-find-a-file-other-window)) ; In `dired+.el'.
                                (condition-case nil ; E.g. error because not on file line (ignore)
                                    (abbreviate-file-name (dired-get-file-for-visit))
                                  (error default-directory))
                              default-directory))

(defun icicle-find-file-other-window-w-wildcards (filename)
  "Find file FILENAME, where the name possibly includes shell wildcards."
  (if (and (eq major-mode 'dired-mode) (fboundp 'diredp-find-a-file-other-window))
      (diredp-find-a-file-other-window filename t)
    (find-file-other-window filename t)))

(icicle-define-command icicle-recent-file ; Command name
                       "Open a recently used file." ; Doc string
                       find-file        ; Function to perform the action
                       "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
                       nil t (car recentf-list) 'file-name-history (car recentf-list) nil
                       nil              ; Additional bindings
                       (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
                              (when (fboundp 'recentf-mode) (recentf-mode 99))
                              (unless (consp recentf-list) (error "No recently accessed files"))))

(icicle-define-command icicle-recent-file-other-window ; Command name
                       "Open a recently used file in another window." ; Doc string
                       find-file-other-window ; Function to perform the action
                       "Recent file: " (mapcar #'list recentf-list) ; `completing-read' args
                       nil t (car recentf-list) 'file-name-history (car recentf-list) nil
                       nil              ; Additional bindings
                       (progn (unless (boundp 'recentf-list) (require 'recentf)) ;  First code
                              (when (fboundp 'recentf-mode) (recentf-mode 99))
                              (unless (consp recentf-list) (error "No recently accessed files"))))

(icicle-define-command icicle-locate-file
                       "Visit a file within a directory or its subdirectories.
With a prefix argument, you are prompted for the directory.
Otherwise, the current directory is used.

The absolute names of all files within the directory and all of its
subdirectories are targets for completion.  Regexp input is matched
against all parts of the absolute name, not just the file-name part.

You can use this to find all files within your file system that match
a regexp, but be aware that gathering and matching the file names will
take some time.

Remember that you can save the set of files matching your input using \
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]' or \
`\\[icicle-candidate-set-save-to-cache-file]'.
You can then retrieve quickly them later using `\\[icicle-candidate-set-retrieve]' or \
`\\[icicle-candidate-set-retrieve-from-cache-file]'. "
                       find-file
                       "File: " (mapcar #'list (icicle-files-within ; `completing-read' args
                                                (directory-files dir 'full icicle-re-no-dot) nil))
                       nil t nil 'file-name-history nil nil
                       ((dir (if current-prefix-arg ; Additional bindings
                                 (read-file-name "Locate under which directory: "
                                                 nil default-directory nil)
                               default-directory)))
                       (message "Gathering files within `%s' (this could take a while)..." dir))

(icicle-define-command icicle-locate-file-other-window
                       "Like `icicle-locate-file' but the file is visited in another window."
                       find-file-other-window
                       "File: " (mapcar #'list (icicle-files-within ; `completing-read' args
                                                (directory-files dir 'full icicle-re-no-dot) nil))
                       nil t nil 'file-name-history nil nil
                       ((dir (if current-prefix-arg ; Additional bindings
                                 (read-file-name "Locate under which directory: "
                                                 nil default-directory nil)
                               default-directory)))
                       (message "Gathering files within `%s' (this could take a while)..." dir))

(icicle-define-command icicle-font      ; Command name
                       "Change font of current frame." ; Doc string
                       (lambda (font)   ; Function to perform the action
                         (modify-frame-parameters orig-frame (list (cons 'font font))))
                       "Font: " (mapcar #'list (x-list-fonts "*")) ; `completing-read' args
                       nil t nil 'icicle-font-history nil nil
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-font (frame-parameter nil 'font)))
                       nil              ; Additional code at beginning
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons 'font orig-font)))
                       nil)             ; Additional code at end

(icicle-define-command icicle-frame-bg  ; Command name
                       "Change background of current frame." ; Doc string
                       (lambda (color)  ; Function to perform the action
                         (modify-frame-parameters orig-frame
                                                  (list (cons 'background-color color))))
                       "Background color:: " (mapcar #'list (x-defined-colors))
                       nil t nil 'icicle-color-history nil nil ; `completing-read' args
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-bg (frame-parameter nil 'background-color)))
                       nil              ; First code
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons 'background-color orig-bg)))
                       nil)             ; Last code

(icicle-define-command icicle-frame-fg  ; Command name
                       "Change foreground of current frame." ; Doc string
                       (lambda (color)  ; Function to perform the action
                         (modify-frame-parameters orig-frame
                                                  (list (cons 'foreground-color color))))
                       "Foreground color:: " (mapcar #'list (x-defined-colors))
                       nil t nil 'icicle-color-history nil nil ; `completing-read' args
                       ((orig-frame (selected-frame)) ; Additional bindings
                        (orig-bg (frame-parameter nil 'foreground-color)))
                       nil              ; First code
                       (modify-frame-parameters orig-frame ; Undo code
                                                (list (cons 'foreground-color orig-bg)))
                       nil)             ; Last code

;; Bind this, not `icicle-Info-index', to `i' in Info mode,
;; so plain `Info-index' will be used when not also in Icicle mode.
(defun icicle-Info-index-cmd ()
  "If in Icicle mode, run `icicle-Info-index'; else, run `Info-index'.
Note: In Emacs versions prior to version 22, this runs `Info-index'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-index 'Info-index)))

(defun icicle-Info-index ()
  "Like `Info-index', but you can use Icicles keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-index-action))
    (call-interactively (if (> emacs-major-version 21) 'Info-index 'icicle-Info-index-20))))

;; Thx to Tamas Patrovics for this.
(defun icicle-Info-index-20 ()
  "Like `Info-index', but you can use completion for the index topic."
  (interactive)
  (Info-index "")
  (let ((pattern "\\* +\\([^:]*\\):.")
        (candidates nil))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t) (push (list (match-string 1)) candidates))
    (Info-index (completing-read "Index topic: " candidates nil t))))

(defun icicle-Info-index-action (topic)
  "Completion action function for `icicle-Info-index'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf)               ; `info-buf', `info-window' defined in `icicle-Info-index'.
    (select-window info-window)
    (Info-index topic)
    (select-window minibuf-win)))

;; Bind this, not `icicle-Info-goto-node', to `g' in Info mode,
;; so plain `Info-goto-node' will be used when not also in Icicle mode.
(defun icicle-Info-goto-node-cmd ()
  "In Icicle mode, run `icicle-Info-goto-node'; else, `Info-goto-node'."
  (interactive)
  (call-interactively (if icicle-mode 'icicle-Info-goto-node 'Info-goto-node)))

(defun icicle-Info-goto-node ()
  "Like `Info-goto-node', but you can use keys `C-RET', `C-up' etc."
  (interactive)
  (let ((info-buf (current-buffer))
        (info-window (selected-window))
        (icicle-candidate-action-fn 'icicle-Info-goto-node-action))
    (call-interactively 'Info-goto-node)))

(defun icicle-Info-goto-node-action (node)
  "Completion action function for `icicle-Info-goto-node'."
  (let ((minibuf-win (selected-window)))
    (set-buffer info-buf) ; `info-buf', `info-window' defined in `icicle-Info-goto-node'.
    (select-window info-window)
    (Info-goto-node node)
    (select-window minibuf-win)))

(icicle-define-command icicle-insert-thesaurus-entry ; Command name
                       "Insert an entry from a thesaurus.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
                       icicle-insert-thesaurus-entry-cand-fn ; Function to perform the action
                       "Thesaurus entry to match: " synonyms-obarray ; `completing-read' args
                       nil nil nil 'icicle-dictionary-history nil nil
                       ((icicle-track-pt (point))) ; Additional bindings
                       (progn           ; First code
                         (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
                           (error "You must first load library `synonyms.el'"))
                         (synonyms-ensure-synonyms-read-from-cache))
                       (when (window-live-p orig-window)
                         (select-frame-set-input-focus (window-frame orig-window))
                         (goto-char icicle-track-pt)) ; Undo code
                       (when (window-live-p orig-window)
                         (select-frame-set-input-focus (window-frame orig-window))
                         (goto-char icicle-track-pt))) ; Last code

(defun icicle-insert-thesaurus-entry-cand-fn (string)
  "Action function for `icicle-insert-thesaurus-entry'.
Insert STRING, followed by a space, at position TRACK-PT of buffer
ORIG-BUFF."
  (set-buffer orig-buff)                ; `orig-buff' is a free variable here.
  (goto-char icicle-track-pt)
  (insert string " ")
  (setq icicle-track-pt (point))
  (save-excursion (set-buffer (window-buffer (minibuffer-window))) (icicle-clear-minibuffer))
  (save-selected-window (icicle-remove-Completions-window)))

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

(defun icicle-complete-thesaurus-entry (word) ; Bound to `C-c /' in Icicle mode.
  "Complete WORD to an entry from a thesaurus.
The default value of WORD is the word at the cursor.
Library `synonyms.el' is needed for this.  If you have never used
command `synonyms' before, then the first use of
`icicle-insert-thesaurus-entry' will take a while, because it will
build a cache file of synonyms that are used for completion.  See
`synonyms.el'."
  (interactive (list (word-at-point)))
  (unless word (error "No word at point to complete"))
  (unless (or (boundp 'synonyms-obarray) (require 'synonyms nil t))
    (error "You must first load library `synonyms.el'"))
  (synonyms-ensure-synonyms-read-from-cache)
  (when (and (looking-at "\\b") (not (looking-at "\\s-"))) (forward-word 1))
  (delete-region (progn (forward-word -1) (point)) (progn (forward-word 1) (point)))
  (insert (completing-read "Thesaurus entry to match: " synonyms-obarray
                           nil nil word 'icicle-dictionary-history word))
  (unless (looking-at "\\s-") (insert " ")))

(icicle-define-command icicle-plist     ; Command name
                       "Choose description of a symbol and property list.
Each candidate for completion is a symbol name plus its property list
\(as a string).  They are separated by `icicle-list-join-string'
\(^G^J, by default).  You can match an input regexp against the symbol
name or the property list or both.  Use `C-q C-g C-q C-j' to input the
default separator.

Remember that you can use `[^^G]' to match any character except ^G,
which includes newline. Remember also that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion."
                       (lambda (entry)
                         (with-output-to-temp-buffer "*Help*" (princ entry)))
                       "SYMB `C-q C-g C-q C-j' PLIST (`RET' when done): " ; `completing-read' args
                       (let ((result nil)) ; TABLE arg is an alist with items ((symb plist-string))
                         (mapatoms
                          (lambda (symb) ; That is, each completion candidate is a list of strings.
                            (let ((plist (symbol-plist symb)))
                              (when plist
                                (push (list (list (symbol-name symb) (format "%s" plist)))
                                      result)))))
                         result)
                       nil nil nil nil nil nil
                       nil              ; Additional bindings
                       (message "Gathering property lists...")) ; First code

(icicle-define-command icicle-vardoc    ; Command name
                       "Choose a variable description.
Each candidate for completion is a variable name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the variable name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*list\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all variables whose
names contain \"dired\" and whose documentation contains \"list\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
                       (lambda (entry)  ; Action function
                         (with-output-to-temp-buffer "*Help*" (princ entry)))
                       "VAR `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
                       (let ((result nil)) ; TABLE arg is an alist whose items are ((symb doc)).
                         (mapatoms
                          (lambda (symb) ; That is, each completion candidate is a list of strings.
                            (when (boundp symb)
                              (let ((doc (documentation-property symb 'variable-documentation)))
                                (when (icicle-non-whitespace-string-p doc)
                                  (push (list (list (symbol-name symb) doc)) result))))))
                         result)
                       nil nil nil nil nil nil
                       nil              ; Additional bindings
                       (message "Gathering variable descriptions...")) ; First code

(icicle-define-command icicle-fundoc    ; Command name
                       "Choose a function description.
Each candidate for completion is a function name plus its
documentation.  They are separated by `icicle-list-join-string' (^G^J,
by default).  You can match an input regexp against the function name
or the documentation or both.  Use `C-q C-g C-q C-j' to input the
default separator.

For example, use input

\"dired.*^G
\[^^G]*file\"

with \\<minibuffer-local-completion-map>`\\[icicle-apropos-complete]' to match all functions whose
names contain \"dired\" and whose documentation contains \"file\".
Here, `[^^G]' matches any character except ^G, which includes newline.
If you use `.*' here, instead, then only the first lines of doc
strings are searched.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
                       (lambda (entry)  ; Action function
                         (with-output-to-temp-buffer "*Help*" (princ entry)))
                       "FUNC `C-q C-g C-q C-j' DOC (`RET' when done): " ; `completing-read' args
                       (let ((result nil)) ; TABLE arg is an alist whose items are ((symb doc)).
                         (mapatoms
                          (lambda (symb) ; That is, each completion candidate is a list of strings.
                            (when (fboundp symb)
                              (let ((doc (documentation symb)))
                                (when (icicle-non-whitespace-string-p doc)
                                  (push (list (list (symbol-name symb) doc)) result))))))
                         result)
                       nil nil nil nil nil nil
                       nil              ; Additional bindings
                       (message "Gathering function descriptions...")) ; First code

(icicle-define-command icicle-doc       ; Command name
                       "Choose documentation for a symbol.
Each candidate for completion is the description of a function,
variable, or face.  Displays the documentation and returns the
symbol.

Remember that you can use `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-incremental-completion] to toggle incremental completion." ; Doc string
                       (lambda (entry)  ; Action function: display the doc.
                         (let ((symb (cdr (assoc entry minibuffer-completion-table))))
                           (when (and symb (boundp symb)) (describe-variable symb))
                           (when (fboundp symb) (describe-function symb))
                           (when (facep symb) (describe-face symb))
                           symb))       ; Return the symbol.
                       "Find doc with regexp: " ; `completing-read' args
                       (let ((result nil)
                             doc)
                         (mapatoms
                          (lambda (symb) ; TABLE arg is an alist whose items are (doc . symb).
                            (when (fboundp symb) ; That is, the completions are the doc strings.
                              (setq doc (concat (documentation symb) "\n\n"))
                              (when (icicle-non-whitespace-string-p doc)
                                (push (cons doc symb) result)))
                            (when (boundp symb)
                              (setq doc (concat
                                         (documentation-property symb 'variable-documentation)
                                         "\n\n"))
                              (when (icicle-non-whitespace-string-p doc)
                                (push (cons doc symb) result)))))
                         ;; `icicle-candidate-action-fn' is used in the main body of command
                         ;;`icicle-doc' and is also bound to `C-RET'.  We need to refer to the
                         ;; TABLE arg to `completing-read' within the body of the function.
                         ;; So, we cheat and pre-assign `minibuffer-completion-table' to it here.
                         (setq minibuffer-completion-table result))
                       nil nil nil nil nil nil
                       nil              ; Additional bindings
                       (message "Gathering documentation...")) ; First code

(defun icicle-non-whitespace-string-p (string)
  "Returns non-nil if STRING contains a non-whitespace character.
The `standard-syntax-table' definition of whitespace is used."
  (interactive "s")
  (let ((orig-syntable (syntax-table)))
    (unwind-protect
       (progn
         (set-syntax-table (standard-syntax-table))
         (and (stringp string) (> (length string) 0) (string-match "\\S-" string)))
      (set-syntax-table orig-syntable))))

;;;###autoload
(defun icicle-apropos (apropos-regexp &optional do-all)
  "Like `apropos', but lets you see the list of matches (with `S-TAB')."
  (interactive (list (completing-read "Apropos symbol (regexp or words): " obarray
                                      nil nil nil 'regexp-history)
                     current-prefix-arg))
  (apropos apropos-regexp do-all))

;;;###autoload
(cond
  ;; Use my versions of the `apropos*' commands, defined in `apropos-fn+var.el'.
  ;; Note that unlike my versions of `apropos-option' and `apropos-command', the `icicle-'
  ;; versions here do not respect `apropos-do-all': they always work with options and commands.
  ((fboundp 'apropos-option)
   (defun icicle-apropos-variable (pattern)
     "Show variables that match PATTERN.
This includes variables that are not user options.
You can see the list of matches with `S-TAB'.
See `apropos-variable' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos variable (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray
             #'(lambda (symbol) (and (boundp symbol) (get symbol 'variable-documentation)))
             nil nil 'regexp-history)))
     (apropos-variable pattern))

   (defun icicle-apropos-option (pattern)
     "Show user options (variables) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-option' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos user option (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'user-variable-p nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-option pattern)))

   (defun icicle-apropos-function (pattern)
     "Show functions that match PATTERN.
This includes functions that are not commands.
You can see the list of matches with `S-TAB'.
See `apropos-function' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos function (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'functionp nil nil 'regexp-history)))
     (apropos-function pattern))

   (defun icicle-apropos-command (pattern)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.
See `apropos-command' for a description of PATTERN."
     (interactive
      (list (completing-read
             (concat "Apropos command (regexp" (and (>= emacs-major-version 22) " or words")
                     "): ") obarray 'commandp nil nil 'regexp-history)))
     (let ((apropos-do-all nil))
       (apropos-command pattern))))

  ;; My versions are not available.  Use the vanilla Emacs versions of the `apropos...' commands.
  (t
   (defun icicle-apropos-variable (pattern &optional do-all)
     "Show variables that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-variable' for a description of PATTERN.

With optional prefix DO-ALL or if `apropos-do-all' is non-nil, also
show normal variables."
     (interactive
      (list (progn
              (unless (or (boundp 'apropos-do-all) (require 'apropos nil t))
                (error "Library `apropos' not found"))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "variable" "user option")
                       " (regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if (or current-prefix-arg apropos-do-all)
                           #'(lambda (symbol) (and (boundp symbol)
                                                   (get symbol 'variable-documentation)))
                         'user-variable-p)
               nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-variable pattern do-all))

   (defun icicle-apropos-command (pattern &optional do-all var-predicate)
     "Show commands (interactively callable functions) that match PATTERN.
You can see the list of matches with `S-TAB'.

See `apropos-command' for a description of PATTERN.

With \\[universal-argument] prefix, or if `apropos-do-all' is non-nil,
also show noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE."
     (interactive
      (list (progn
              (unless (boundp 'apropos-do-all)
                (unless (require 'apropos nil t) (error "Library `apropos' not found")))
              (completing-read
               (concat "Apropos " (if (or current-prefix-arg apropos-do-all)
                                      "command or function" "command")
                       "(regexp" (and (>= emacs-major-version 22) " or words") "): ")
               obarray (if current-prefix-arg 'functionp 'commandp) nil nil 'regexp-history))
            current-prefix-arg))
     (apropos-command pattern do-all var-predicate))))

;;;###autoload
(defun icicle-apropos-zippy (regexp)
  "Show all Zippy quotes matching the regular-expression input.
Returns the list of matches."
  (interactive (progn (unless (boundp 'yow-file)
                        (unless (require 'yow nil t) (error "Library `yow' not found")))
                      (cookie yow-file yow-load-message yow-after-load-message)
                      (let* ((case-fold-search t)
                             (cookie-table-symbol (intern yow-file cookie-cache))
                             (string-table (symbol-value cookie-table-symbol))
                             (table (nreverse (mapcar #'list string-table))))
                        (list (completing-read "Apropos Zippy (regexp): " table
                                               nil nil nil 'regexp-history)))))
  (let ((matches (apropos-zippy icicle-current-input)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Zippy Apropos*"
        (while matches
          (princ (car matches))
          (setq matches (cdr matches))
          (and matches (princ "\n\n")))))
    matches))                           ; Return matching Zippyisms.

;;;###autoload
(defun icicle-map (alist fn)
  "Selectively apply a function to items in an alist.
FN is a function.  ALIST is an alist - interactively, it is a variable
whose value is an alist.

You are prompted for both arguments.  Completion is available.
The completion list for ALIST is a set of variables whose value is a
cons.  With no prefix arg, the names of these variables must end with
\"alist\".  With a prefix argument, the first car of each variable
value must itself be a cons.

Examples: If ALIST is `auto-mode-alist' and FN is `cdr', then the
completion candidates are the keys of the alist and the result of
applying FN to an alist entry is simply the value of that key.  If you
choose, for example, candidate \"\\.el\\'\", then the result is
`emacs-lisp-mode'.  In this case, the function performs simple lookup.

If, instead, FN were (lambda (x) (describe-function (cdr x))), then
the result of choosing \"\\.el\\'\" would be to display the help for
function `emacs-lisp-mode'.

During completion you can use these keys also.  Each displays the
value of applying FN to the current completion candidate.
\\<minibuffer-local-completion-map>\
`C-RET'   - Act on current completion candidate only
`C-next'  - Act, then move to next prefix-completion candidate
`C-prior' - Act, then move to previous prefix-completion candidate
`next'    - Move to next apropos-completion candidate
`prior'   - Move to previous apropos-completion candidate
`\\[icicle-all-candidates-action]'     - Act on *all* candidates, successively

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'.

`icicle-map' overrides `icicle-ignore-space-prefix-flag', binding it
to nil so that candidates with initial spaces can be matched."
  (interactive
   (list (symbol-value
          (intern (completing-read
                   "Alist (variable): " obarray
                   `(lambda (symb)
                     (and
                      (boundp symb) (consp (symbol-value symb))
                      ,(if current-prefix-arg
                           '(consp (car (symbol-value symb)))
                           '(string-match "alist$" (symbol-name symb)))))
                   t nil 'icicle-variable-history)))
         (read (completing-read "Function: " obarray 'functionp
                                nil nil 'icicle-function-history))))
  (setq icicle-candidates-alist         ; Make keys of ALIST be strings.  Save in global variable.
        (mapcar (lambda (key+val) (cons (format "%s" (car key+val)) (cdr key+val))) alist))
  (setq icicle-candidate-entry-fn fn)   ; Save in global variable.
  (let ((icicle-candidate-action-fn 'icicle-map-action)
        (icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-ignore-space-prefix-flag nil)
        (enable-recursive-minibuffers t))
    (condition-case failure
        (let ((cand-nb 0)
              candidate-entries result)
          (completing-read "Choose an occurrence: " icicle-candidates-alist nil t)
          (setq candidate-entries (icicle-filter-alist icicle-completion-candidates
                                                       icicle-candidates-alist))
          (cond ((not (wholenump icicle-candidate-nb)) ; Didn't cycle to choose the candidate.
                 (when (cdr candidate-entries) ; Multiple entries with the same key
                   (error (substitute-command-keys "Ambiguous choice. Try again.")))
                 ;; Do nothing here if only one entry with the chosen key.
                 )
                (t
                 (setq cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                 ;; If cycling with action functions, add or subtract one from candidate #.
                 (when (memq last-command '(icicle-next-apropos-candidate-action
                                            icicle-next-prefix-candidate-action))
                   (setq cand-nb (1- cand-nb))
                   (when (< cand-nb 0) (setq cand-nb 0)))
                 (when (memq last-command '(icicle-previous-apropos-candidate-action
                                            icicle-previous-prefix-candidate-action))
                   (setq cand-nb (1+ cand-nb))
                   (when (> cand-nb (length icicle-candidates-alist))
                     (setq cand-nb (length icicle-candidates-alist))))))
          ;; Note: If didn't cycle to choose candidate then `candidate-entries' is singleton.
          (setq result (funcall icicle-candidate-entry-fn (elt candidate-entries cand-nb)))
          (message "Key: %s,  Result: %s" (car (elt candidate-entries cand-nb)) result)
          result)
      (error (error (error-message-string failure))))))

(defun icicle-map-action (string)
  "Completion action function for `icicle-map'."
  (unwind-protect
       (condition-case icicle-map-action
           (let (curr-cand-string)
             ;; Highlight the current candidate in *Completions*.
             (let ((compl-win (get-buffer-window "*Completions*" t))
                   curr-cand-pos)
               (when compl-win
                 (save-window-excursion
                   (select-window compl-win)
                   (goto-char (icicle-start-of-candidates-in-Completions))
                   (icicle-move-to-next-completion icicle-candidate-nb t)
                   (set-buffer-modified-p nil)
                   (setq curr-cand-pos (point))
                   (setq curr-cand-string (icicle-current-completion-in-Completions)))
                 (set-window-point compl-win curr-cand-pos)))
             ;; Apply function to candidate entry and display it.
             (if icicle-candidate-nb
                 (let ((key+result (assoc curr-cand-string icicle-candidates-alist)))
                   (message "Key: %s,  Result: %s" (car key+result)
                            (funcall icicle-candidate-entry-fn key+result)))
               (error "No such occurrence"))
             t)                         ; Return non-nil for success.
         (error nil)))                  ; Return nil for failure.
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

;;;###autoload
(defun icicle-add-region (start end)
  "Add current region to the list of Icicles regions, `icicle-regions'."
  (interactive "r")
  (when (> start end) (setq end (prog1 start (setq start end))))
  (add-to-list 'icicle-regions
               (list (buffer-substring-no-properties
                      start
                      (+ start (min icicle-regions-name-length-max (- end start))))
                     (buffer-name)
                     start
                     end)))

;;;###autoload
(icicle-define-command icicle-remove-region
                       "Remove a region from the list of Icicles regions, `icicle-regions'.
Use `icicle-add-region' to define the list of regions."
                       (lambda (reg-name)    ; Action function
                         (let ((reg (assoc reg-name icicle-regions)))
                           (setq icicle-regions (delete reg icicle-regions))))
                       "Remove region: " icicle-regions ; `completing-read' args
                       nil t nil nil (caar icicle-regions))

;;;###autoload
(icicle-define-command icicle-select-region
                       "Choose a region from the list of Icicles regions, and activate it.
You can use `icicle-add-region' to define the list of regions,
`icicle-regions'."
                       (lambda (reg-name) ; Action function
                         (let* ((reg (assoc reg-name icicle-regions))
                                (buf (cadr reg)))
                           (set-buffer buf)
                           (goto-char (caddr reg))
                           (push-mark (cadddr reg) 'nomsg 'activate)))
                       "Select region: " icicle-regions ; `completing-read' args
                       nil t nil nil (caar icicle-regions))

;;;###autoload
(icicle-define-command icicle-search-region
                       "Search a region from the list of Icicles regions, `icicle-regions'.
You can use `icicle-add-region' to define the list of regions."
                       (lambda (reg-name) ; Action function
                         (let* ((reg (assoc reg-name icicle-regions))
                                (buf (cadr reg)))
                           (cond ((get-buffer buf)
                                  (set-buffer buf)
                                  (let ((icicle-show-Completions-initially-flag t))
                                    (icicle-search (caddr reg) (cadddr reg) regexp t))
                                  (save-excursion
                                    (set-buffer (window-buffer (minibuffer-window)))
                                    (icicle-erase-minibuffer)))
                                 (t
                                  (message "Skipping regions in `%s' - no such buffer" buf)
                                  (sit-for 2)))))
                       "Search region: " icicle-regions ; `completing-read' args
                       nil t nil nil (caar icicle-regions) nil
                       ((enable-recursive-minibuffers t)
                        (icicle-sort-function nil)
                        (regexp (read-from-minibuffer "Search for (regexp): " ; Bindings
                                                      nil nil nil 'regexp-history))))

;;;###autoload
(defun icicle-search-generic ()         ; Bound to `C-x `'.
  "Run `icicle-search-command'."
  (interactive)
  (call-interactively icicle-search-command))

;;;###autoload
(defun icicle-search (beg end regexp require-match &optional where) ; Bound to `C-c `'.
  "Search for regexp matches, with completion and completion cycling.
You are prompted for a regexp, which you enter using `RET'.  Matches
are available as completion candidates.  You can then use apropos
completion to filter the candidates using a different regexp.

By default, search only the current buffer.  Search the active region,
or, if there is none, search the entire buffer.

With a prefix argument, you can search multiple buffers or regions, as
follows:

- With a simple prefix arg (`C-u'), search multiple buffers
completely.  You are prompted for the buffers to search - all of each
buffer is searched.

- With a numeric prefix arg, search all of the regions in
`icicles-regions'.  Those regions can be in any buffers.  If a region
is in a buffer that does not exist, it is skipped.  You can always
re-create the buffer (e.g. visit the file), and try again.

Note: To individually search selected regions in `icicle-regions', use
multi-command `icicle-search-region'.

The use of completion for this command is a bit special.  It is not
unusual in this context to have multiple completion candidates that
are identical - only the positions of their occurrences in the search
buffer(s) differ.  In that case, you cannot choose one by completing
it in the minibuffer, because the destination would be ambiguous.
That is, simply completing your input and entering the completion with
`RET' will not take you to its occurrence in the search buffer unless
it is unique.

Instead, cycle among the candidates and choose one: cycling tracks the
search-buffer position.  To move to each occurrence in the search
buffer as you cycle among candidates, use `C-next', `C-prior',
`C-down', and `C-up'.

To cycle among some candidates without moving to their occurrences in
the search buffer, use `next', `prior', `down', and `up'.  Then use
`C-RET' or `RET' to move to the current-candidate occurrence.

Note that `RET' after cycling with `next' takes you to the current
candidate, but it does not do so after cycling with `C-next'.  This
apparent inconsistency is for convenience.  The `RET' is in fact
ignored, as stated above.  It is the cycling that establishes the
target position, and `icicle-search' simply takes you there.  After
cycling, the minibuffer content corresponds to the current candidate
when you use `next', but for `C-next' it corresponds to the following
candidate.  You do not usually want to go to the following candidate,
so the `RET' is ignored.

After you accept a completion candidate, and go to it, the hooks in
variable `icicle-search-hook' are run.

In the search buffer, `icicle-search':

 Highlights the current match for the initial regexp using face
 `icicle-search-main-regexp-current'.

 Highlights the match for your current input (e.g. a different regexp
 from the initial one) using face `icicle-search-current-input'.

 Highlights the first `icicle-search-highlight-threshold' matches for
 your initial regexp, using face `icicle-search-main-regexp-others'.

If user option `icicle-search-cleanup-flag' is non-nil (the default),
then all search highlighting is removed from the search buffer when
you are finished searching.  If it is nil, then you can remove this
highlighting later using command `icicle-search-highlight-cleanup'.
You can toggle `icicle-search-cleanup-flag' at any time using `\\<minibuffer-local-completion-map>\
\\[icicle-toggle-search-cleanup]' in the minibuffer.

You can use `\\[icicle-insert-string-from-variable]' (command \
`icicle-insert-string-from-variable') to
insert text (e.g. a regexp) from a variable into the minibuffer.
For example, you can search for ends of sentences by using \
`C-u \\[icicle-insert-string-from-variable]'
and choosing variable `sentence-end' as the variable.  And you can use
`\\[icicle-save-string-to-variable]' to save a string to a variable
for later use by `\\[icicle-insert-string-from-variable]'.

When employed with useful regexps, `C-=' can turn `icicle-search' into
a general navigator or browser of code, mail messages, and many other
types of buffer.  Imenu regexps work fine, for example - command
`icicle-imenu' simply uses `icicle-search' this way.  See
`icicle-insert-string-from-variable' for more tips on inserting
regexps from variables.

`icicle-search' overrides `icicle-ignore-space-prefix-flag', binding
it to nil so that candidates with initial spaces can be matched.

`icicle-search' sets `icicle-search-final-choice' to the final user
choice, which might not be one of the search candidates, if
REQUIRE-MATCH is nil.

When called from a program, these are the `icicle-search' arguments:

BEG is the beginning of the region to search; END is the end.
REGEXP is the regexp that determines the set of initial candidates.
REQUIRE-MATCH is passed to `completing-read'.

Optional arg WHERE is a list of buffer names or a list of regions of
the same form as `icicle-regions'."

  (interactive `(,@(icicle-search-region-beg-end)
                 ,(read-from-minibuffer "Search for (regexp): " nil nil nil 'regexp-history)
                 t
                 ,(cond ((consp current-prefix-arg) (mapcar #'get-buffer (icicle-buffer-list)))
                        (current-prefix-arg icicle-regions)
                        (t nil))))
                         
  (setq regexp (or regexp
                   (read-from-minibuffer "Search for (regexp): " nil nil nil 'regexp-history)))
  (let ((icicle-incremental-completion-flag 'always)
        (icicle-sort-function nil)
        (icicle-ignore-space-prefix-flag nil)
        (completion-ignore-case case-fold-search)
        (orig-point (point))
        (orig-window (selected-window))
        (mark-active nil))              ; So region highlighting doesn't hide highlighting here.
    (setq icicle-candidates-alist nil)
    ;; Build `icicle-candidates-alist'. Highlight up to `icicle-search-highlight-threshold' matches
    (message "Finding matches...")
    (cond ((and (consp where) (stringp (car where))) ; List of buffer names - search buffers.
           (dolist (buf where) (icicle-search-regexp-scan buf regexp)))
          ((consp where)                ; Search all regions in `icicle-regions'.
           (dolist (reg where)
             (if (bufferp (get-buffer (cadr reg)))
                 (icicle-search-regexp-scan
                  (get-buffer (cadr reg)) regexp (caddr reg) (cadddr reg))
               (message "Skipping regions in `%s' - no such buffer" (cadr reg)) (sit-for 2))))
          (t                            ; Search this buffer only.
           (icicle-search-regexp-scan (current-buffer) regexp beg end)))
    (setq icicle-candidates-alist (nreverse icicle-candidates-alist))
    (unless icicle-candidates-alist (error "No match for regexp `%s'" regexp))
    (let ((icicle-candidate-action-fn 'icicle-search-action)
          (icicle-update-input-hook (list 'icicle-search-highlight-all-input-matches)))
      (setq icicle-search-final-choice nil)
      (unwind-protect
           (condition-case failure
               (let (candidate-entries)
                 (setq icicle-search-final-choice (completing-read
                                                   "Choose an occurrence: "
                                                   icicle-candidates-alist nil require-match
                                                   nil 'icicle-search-history)
                       candidate-entries (icicle-filter-alist icicle-completion-candidates
                                                              icicle-candidates-alist))
                 (cond
                   ;; No match required, and not a match - just run the hook.
                   ((and (not require-match) (null icicle-completion-candidates))
                    (run-hooks 'icicle-search-hook))
                   ;; Didn't cycle - completed.
                   ((not (wholenump icicle-candidate-nb))
                    (if (cdr candidate-entries)
                        (error (substitute-command-keys "Ambiguous choice. Try again using \
\\<minibuffer-local-completion-map>\\[icicle-next-apropos-candidate-action] to browse."))
                      (goto-char (cdr (car candidate-entries))) ; Go to sole completion.
                      (run-hooks 'icicle-search-hook)))
                   ;; Cycled.
                   (t
                    (let ((cand-nb (mod icicle-candidate-nb (length icicle-candidates-alist)))
                          marker buf)
                      ;; If cycling with action functions, add or subtract one from candidate #.
                      (when (memq last-command '(icicle-next-apropos-candidate-action
                                                 icicle-next-prefix-candidate-action))
                        (setq cand-nb (1- cand-nb))
                        (when (< cand-nb 0) (setq cand-nb 0)))
                      (when (memq last-command '(icicle-previous-apropos-candidate-action
                                                 icicle-previous-prefix-candidate-action))
                        (setq cand-nb (1+ cand-nb))
                        (when (> cand-nb (length icicle-candidates-alist))
                          (setq cand-nb (length icicle-candidates-alist))))
                      (setq marker (cdr (elt candidate-entries cand-nb)))
                      (unless marker (error "No such occurrence"))
                      (setq buf (marker-buffer marker))
                      (unless (bufferp buf) (error "No such buffer: %s" buf))
                      (set-buffer buf)
                      (goto-char (marker-position marker))
                      (run-hooks 'icicle-search-hook)))))
             (quit (goto-char orig-point))
             (error (goto-char orig-point) (error (error-message-string failure))))
        (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
        (when (window-live-p orig-window)
          (select-window orig-window)
          (select-frame-set-input-focus (selected-frame))))
      icicle-search-final-choice)))

(defun icicle-search-region-beg-end ()
    "Return the start and end of the search region as a list.
If the region is not active or empty, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

(defun icicle-search-regexp-scan (buffer regexp &optional beg end)
  "Scan BUFFER for REGEXP, pushing hits onto `icicle-candidates-alist'.
Highlight matches in face `icicle-search-main-regexp-others'.
If BEG and END are non-nil, scan only between positions BEG and END."
  (when (bufferp buffer)
    (let ((last-beg nil))
      (set-buffer buffer)
      (unless (and beg end) (setq beg (point-min) end (point-max)))
      (condition-case icicle-search-regexp-scan
          (save-excursion
            (goto-char beg)
            (while (and beg (< beg end))
              (while (and (setq beg (re-search-forward regexp end t)) (eq last-beg beg))
                (forward-char) (setq beg (1+ beg))) ; Matched again, same place.  Advance 1 char.
              (when beg              ; Add (strg . pos) to candidates.
                (push (cons (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                            (copy-marker beg))
                      icicle-candidates-alist)
                ;; Highlight candidate in buffer.
                (when (<= (length icicle-candidates-alist) icicle-search-highlight-threshold)
                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                    (push ov icicle-search-overlays)
                    (overlay-put ov 'priority 200) ; > ediff's 100+, but < isearch overlays
                    (overlay-put ov 'face 'icicle-search-main-regexp-others))))
              (setq last-beg beg)))
        (quit (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup)))
        (error (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
               (error (error-message-string icicle-search-regexp-scan)))))))

(defun icicle-search-highlight-all-input-matches (&optional input)
  "Highlight, inside each search regexp match, what current input matches."
  (save-excursion
    ;; Update by deleting (if it exists) and then creating.
    ;; If a single overlay exists, it means that the user just changed
    ;; `icicle-search-highlight-threshold' to non-zero.
    ;; Otherwise, it's nil or a list of overlays.
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays))))
  (when icicle-search-highlight-all-current-flag
    (setq input (or input icicle-current-input))
    (unless (or (string= "" input) (null icicle-search-overlays))
      (save-excursion
        (dolist (ov icicle-search-overlays)
          (set-buffer (overlay-buffer ov))
          (save-restriction             ; Search within the current search regexp match.
            (narrow-to-region (overlay-start ov) (overlay-end ov))
            (goto-char (point-min))
            (when (condition-case nil
                      (re-search-forward input nil t)
                    (error nil))
              (setq ov (make-overlay (match-beginning 0) (match-end 0)))
              (push ov icicle-search-refined-overlays)
              (overlay-put ov 'priority 204)
              (overlay-put ov 'face 'icicle-search-current-input))))))))

(defun icicle-search-action (string)
  "Completion action function for command `icicle-search'.
1. Highlight the current candidate in *Completions*.
2. Move to the regexp match in the original buffer and highlight it.
3. If `icicle-search-highlight-threshold' is zero, highlight what the
   current input matches, inside the match of the initial regexp."
  (unwind-protect
       (condition-case icicle-search-action
           (let (curr-cand-string)
             ;; Highlight the current candidate in *Completions*.
             (let ((compl-win (get-buffer-window "*Completions*" t))
                   curr-cand-pos)
               (when compl-win
                 (save-window-excursion
                   (select-window compl-win)
                   (let ((completion-ignore-case case-fold-search))
                     (goto-char (icicle-start-of-candidates-in-Completions))
                     (icicle-move-to-next-completion icicle-candidate-nb t)
                     (set-buffer-modified-p nil)
                     (setq curr-cand-pos (point))
                     (setq curr-cand-string (icicle-current-completion-in-Completions))))
                 (set-window-point compl-win curr-cand-pos)))
             ;; Move cursor to the match in the original buffer and highlight it.
             (let* ((cand+mrker (elt icicle-candidates-alist icicle-candidate-nb))
                    (candidate (car-safe cand+mrker))
                    (marker (cdr-safe cand+mrker)))
               (unless marker (error "No such occurrence"))
               (save-selected-window
                 (when (window-live-p orig-window) (select-window orig-window))
                 (let ((completion-ignore-case case-fold-search)
                       buf)
                   (setq buf (marker-buffer marker))
                   (unless (bufferp buf) (error "No such buffer: %s" buf))
                   (pop-to-buffer buf)
                   (raise-frame)
                   (goto-char (marker-position marker))
                   (icicle-place-overlay (- marker (length candidate)) marker
                                         'icicle-search-current-overlay
                                         'icicle-search-main-regexp-current
                                         (current-buffer))
                   (overlay-put icicle-search-current-overlay 'priority 202)
                   (unless (or (> 0 icicle-search-highlight-threshold)
                               (string= "" icicle-current-raw-input))
                     ;; Highlight, inside search regexp match, what current input matches.
                     (save-excursion
                       (save-restriction ; Search within the current search regexp match.
                         (narrow-to-region (- marker (length candidate)) marker)
                         (goto-char (point-min))
                         ;; Use `icicle-current-input' here, so get only the matched part.
                         (when (re-search-forward icicle-current-input nil t)
                           (icicle-place-overlay
                            (match-beginning 0) (point) 'icicle-search-refined-overlays
                            'icicle-search-current-input (current-buffer))
                           (overlay-put icicle-search-refined-overlays 'priority 204)))))
                   (run-hooks 'icicle-search-hook)))
               t))                      ; Return non-nil for success.
         (error nil))                   ; Return nil for failure.
    (select-frame-set-input-focus (window-frame (minibuffer-window)))))

(defun icicle-filter-alist (filter-keys alist)
  "Filter ALIST, keeping items whose cars match FILTER-KEYS, in order.
The original ALIST is not altered; a copy is filtered and returned."
  (icicle-delete-if-not (lambda (item) (member (car item) filter-keys)) alist))

(defun icicle-search-highlight-cleanup ()
  "Remove all highlighting from the last use of `icicle-search'."
  (interactive)
  (let ((inhibit-quit t))
    (message "Removing search highlighting...")
    (while icicle-search-overlays
      (delete-overlay (car icicle-search-overlays))
      (setq icicle-search-overlays (cdr icicle-search-overlays)))
    (when (overlayp icicle-search-current-overlay)
      (delete-overlay icicle-search-current-overlay))
    (when (overlayp icicle-search-refined-overlays)
      (delete-overlay icicle-search-refined-overlays)
      (setq icicle-search-refined-overlays nil))
    (while icicle-search-refined-overlays
      (delete-overlay (car icicle-search-refined-overlays))
      (setq icicle-search-refined-overlays (cdr icicle-search-refined-overlays)))
    (message "Removing search highlighting...done")))

;;;###autoload
(defun icicle-occur (beg end &optional buffers) ; Bound to `C-c ''.
  "`icicle-search' with a regexp of \".*\".  An `occur' with icompletion.
Type a regexp to match within each line of the buffer.  Use `S-TAB' to
show matching lines.  Use `C-RET' or `C-mouse-2' to go to the line of
the current candidate.  Use `C-next', `C-prior', `C-down', or`C-up' to
cycle among the matching lines.

By default, search only the current buffer.  Search the active region,
or, if none, the entire buffer.  With a prefix argument, you are
prompted for the buffers to search.  You can choose buffers using
completion (`C-RET' and so on).

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match."
  (interactive `(,@(icicle-search-region-beg-end)
                 ,(and current-prefix-arg (mapcar #'get-buffer (icicle-buffer-list)))))
  (let ((fg (face-foreground 'icicle-search-main-regexp-others))
        (bg (face-background 'icicle-search-main-regexp-others)))
    (unwind-protect
         (progn
           (set-face-foreground 'icicle-search-main-regexp-others nil)
           (set-face-background 'icicle-search-main-regexp-others nil)
           (icicle-search beg end ".*" t buffers))
      (when icicle-search-cleanup-flag (icicle-search-highlight-cleanup))
      (set-face-foreground 'icicle-search-main-regexp-others fg)
      (set-face-background 'icicle-search-main-regexp-others bg))))

;;;###autoload
(defun icicle-comint-search (beg end)   ; Bound to `C-x `' in `comint-mode'.
  "Use `icicle-search' to pick up a previous input for reuse.
Use this in a `comint-mode' buffer, such as *shell* or
*inferior-lisp*.  This searches your interactive history in the buffer
for a match to your current input, which you can change dynamically.
When you choose a previous input, it is copied to the current prompt,
for reuse.  If the region is active, then only it is searched;
otherwise, the entire buffer is searched.

Use `C-RET' or `C-mouse-2' to choose a previous input for reuse.  Use
`C-next', `C-prior', `C-down', or `C-up' to cycle among your previous
inputs.

As for other Icicles search commands, your current input narrows the
set of possible candidates.  See `icicle-search' for more
information.

You can use `M-*' to further narrow the match candidates, typing
additional regexps to match.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

Being a search command, `icicle-comint-search' cannot give you access
to previous commands that are not visible in the current buffer.  See
also \\<comint-mode-map>\\[icicle-comint-command] for another way to
reuse commands, including those from previous sessions."
  (interactive (icicle-search-region-beg-end))
  ;; Is there a better test we can use, to make sure the current mode inherits from `comint-mode'?
  (unless (where-is-internal 'comint-send-input (keymap-parent (current-local-map)))
    (error "Current mode must be derived from comint mode"))
  (let ((orig-search-hook icicle-search-hook)
        (icicle-transform-function 'icicle-remove-duplicates))
    (add-hook 'icicle-search-hook 'icicle-comint-send-input)
    (icicle-search beg end (concat comint-prompt-regexp "\\S-.*") nil) ; Match not required (edit).
    (remove-hook 'icicle-search-hook 'icicle-comint-send-input))
  (goto-char (point-max)))

(defun icicle-comint-send-input ()
  "Grab current completion input and use that for comint input."
  (unless (comint-check-proc (current-buffer))
    (error "No live process associated with this buffer"))
  (let ((comint-get-old-input
         (if (minibuffer-window-active-p (minibuffer-window))
             'icicle-comint-get-minibuffer-input ; Use minibuffer input (e.g. for action fn).
           'icicle-comint-get-final-choice))) ; Use final choice.
    (comint-copy-old-input))
  (comint-send-input))

(defun icicle-comint-get-minibuffer-input ()
  "Return the minibuffer input, beyond the prompt."
  (let* ((cand (icicle-minibuffer-contents))
         (input-start (and (string-match comint-prompt-regexp cand) (match-end 0))))
    (if input-start (substring cand input-start) cand)))

(defun icicle-comint-get-final-choice ()
  "Return the final choice, beyond the prompt."
  (let ((input-start (and (string-match comint-prompt-regexp icicle-search-final-choice)
                          (match-end 0))))
    (if input-start
        (substring icicle-search-final-choice input-start)
      icicle-search-final-choice)))

;;;###autoload
(icicle-define-command icicle-comint-command ; Bound to `C-c TAB' in `comint-mode'.
                       "Retrieve a previously used command.
Use this in a `comint-mode' buffer such as *shell* or *inferior-lisp*.

Note, depending on your shell, you might want to customize variables
such as the following:

`shell-prompt-pattern',`telnet-prompt-pattern'.

See also \\<comint-mode-map>\\[icicle-comint-search] for another way to reuse commands."
                       insert
                       "Choose a previous command: " ; `completing-read' args
                       (mapcar #'list (cddr comint-input-ring)) nil nil nil 'shell-command-history
                       (aref (cddr comint-input-ring) 0) nil
                       ((icicle-transform-function 'icicle-remove-duplicates))) ; Bindings

(add-hook 'comint-mode-hook
          (lambda ()
            (set (make-local-variable 'icicle-search-command) 'icicle-comint-search)
            (define-key comint-mode-map "\C-c\C-i" 'icicle-comint-command)
            (define-key comint-mode-map [(control ?c) tab] 'icicle-comint-command)))

;;;###autoload
(defun icicle-compilation-search (beg end) ; Bound to `C-c `' in `compilation(-minor)-mode'.
  "Like `icicle-search', but shows the matching compilation-buffer
hit.  Use this in a compilation buffer, such as `*grep*', searching
for a regexp as with `icicle-search'.  Use `C-RET' or `C-mouse-2' to
show the target-buffer hit corresponding to the current completion
candidate.  Use `C-next', `C-prior', `C-down', or `C-up' to cycle
among the target-buffer hits.

As for `icicle-search', you can further narrow the match candidates by
typing a second regexp to search for among the first matches.  See
`icicle-search' for more information.

Altogether, using this with `grep' gives you two or three levels of
regexp searching: 1) the `grep' regexp, 2) the major `icicle-search'
regexp, and optionally 3) the refining `icicle-search' regexp."
  (interactive (icicle-search-region-beg-end))
  (unless (condition-case nil
              (eq (current-buffer) (compilation-find-buffer))
            (error nil))
    (error "Current buffer must be a compilation buffer"))
  (let ((orig-search-hook icicle-search-hook))
    (add-hook 'icicle-search-hook 'compile-goto-error)
    (icicle-search beg end nil t)
    (remove-hook 'icicle-search-hook 'compile-goto-error)))

(add-hook 'compilation-minor-mode-hook
          (lambda () (set (make-local-variable 'icicle-search-command)
                          'icicle-compilation-search)))
(add-hook 'compilation-mode-hook
          (lambda () (set (make-local-variable 'icicle-search-command)
                          'icicle-compilation-search)))

;;;###autoload
(defun icicle-imenu (beg end)
  "Go to an Imenu entry using `icicle-search'.
See `icicle-search' for usage notes."
  (interactive (icicle-search-region-beg-end))
  (unless imenu-generic-expression (error "No Imenu for this buffer"))
  (let ((case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    (dolist (syn slist)                 ; Modify the syntax table used while matching regexps.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table) ; Single character.
        (mapc (lambda (c) (modify-syntax-entry c (cdr syn) table)) (car syn)))) ; String.
    (unwind-protect
         (save-match-data
           (set-syntax-table table)
           (let* ((menus (icicle-delete-if-not
                          #'icicle-imenu-in-buffer-p ; Only use menus that match the buffer.
                          (mapcar (lambda (menu) ; Name an unlabeled menu `Others'.
                                    (if (stringp (car menu)) menu (cons "Others" (cdr menu))))
                                  imenu-generic-expression)))
                  (submenu (let ((icicle-show-Completions-initially-flag t))
                             (completing-read "Choose: " menus nil t)))
                  (regexp (cadr (assoc submenu menus))))
             (unless (stringp regexp) (error "No match"))
             (icicle-search beg end regexp t)))
      (set-syntax-table old-table))))

(defun icicle-imenu-in-buffer-p (menu)
  "Return non-nil if the regexp in MENU has a match in the buffer."
  (save-excursion (goto-char (point-min)) (re-search-forward (cadr menu) nil t)))

;;;###autoload
(defun icicle-save-string-to-variable (askp)
  "Save a string (text) to a variable.
By default, the variable is user option `icicle-input-string'.
To save to a different variable, use a prefix argument; you are then
prompted for the variable to use.  You can use `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]' to insert a string from a
variable.  Typically, you store a regexp or part of a regexp in the
variable."
  (interactive "P")
  (let* ((enable-recursive-minibuffers t)
         (var (if askp
                  (intern (completing-read
                           "Variable: " obarray 'boundp nil nil
                           'icicle-variable-history (symbol-name 'icicle-input-string)))
                'icicle-input-string))
         (text (read-string (format "Text to save in `%s': " var))))
    (set var text)))

;;;###autoload
(defun icicle-customize-icicles-group ()
  "Customize Icicles options and faces.  View their documentation."
  (interactive)
  (customize-group-other-window 'icicles))

;;;###autoload
(defun icicle-send-bug-report ()
  "Send a bug report about an Icicles problem."
  (interactive)
  (browse-url (concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions.")))

;;;###autoload
(defun icicle-scroll-Completions ()
  "Scroll the *Completions* window."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window "*Completions*" 0))
    (if (not (= (window-end) (point-max)))
        (scroll-up nil)
      (unless (= (window-start) (point-min))
        (goto-char (icicle-start-of-candidates-in-Completions))))))

;;;###autoload
(defun icicle-next-candidate-per-mode (&optional nth)
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


;;; Other top-level Icicles commands   .   .   .   .   .   .

;;;###autoload
(when (fboundp 'map-keymap)             ; Emacs 22.

  (defvar icicle-complete-keys-alist nil "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a symbol naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
 KEY is the actual key sequence
 BINDING is the actual binding of KEY.")

  (defun icicle-generic-S-tab ()
    "Treat `S-TAB' in minibuffer, *Completions*, and elsewhere.
Bound to `S-TAB' by default, but whatever it is bound to, does this:
 - Call `icicle-apropos-complete' if in minibuffer during completion.
 - Call `icicle-move-to-previous-completion' if in *Completions*.
 - Call `icicle-complete-keys', otherwise.

If you do not use the default minibuffer completion binding of
`S-TAB' for `icicle-apropos-complete', then you can simply bind
\[S-tab] to nil in `icicle-mode-map' in `icicle-mode-hook'."
    (interactive)
    (cond ((icicle-completing-p)
           (setq this-command 'icicle-apropos-complete)
           (icicle-apropos-complete))   ; Defined in `icicles-cmd.el'
          ((eq (get-buffer "*Completions*") (current-buffer))
           (setq this-command 'icicle-move-to-previous-completion)
           (icicle-move-to-previous-completion (prefix-numeric-value current-prefix-arg)))
          (t
           (setq this-command 'icicle-complete-keys)
           (icicle-complete-keys))))

  ;; This is a quick-and-dirty definition, not an efficient one.
  ;; It gathers all key bindings and then throws most of them away!  Good enough.
  (defun icicle-insert-char ()
    "Insert a character, using key completion.
Keys bound to `self-insert-command' are completion candidates."
    (interactive)
    (barf-if-buffer-read-only)
    (let ((icicle-complete-keys-self-insert-flag t)
          (icicle-must-match-regexp "^.+  =  self-insert-command"))
      (icicle-complete-keys)))

  (defun icicle-complete-keys () ; Bound to prefix keys followed by `S-TAB' (unless defined).
    "Complete a key sequence for the currently invoked prefix key.
Input-candidate completion and cycling are available.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x = ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and complete candidates there.  For example, if you are
currently completing prefix key `C-x 5', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `icicle-complete-keys-self-insert-flag' is non-nil, then
keys bound to `self-insert-command' are included as possible
completion candidates; otherwise (the default), they are not.  Command
`icicle-insert-char' works like `icicle-complete-keys', but in
includes only keys bound to `self-insert-command' - use it to insert a
character that is difficult or impossible to type with your keyboard.

While cycling, these keys describe the command of the current
candidate:

`C-RET'   - Describe command of current completion candidate only
`C-down'  - Describe; move to next prefix-completion candidate
`C-up'    - Describe; move to previous prefix-completion candidate
`C-next'  - Describe; move to next apropos-completion candidate
`C-prior' - Describe; move to previous apropos-completion candidate
`C-!'     - Describe *all* candidates, successively - use the [back]
            button in buffer *Help* to visit the descriptions

Use `RET' or `S-RET' to finally choose a candidate, or `C-g' to quit.
This is an Icicles command - see `icicle-mode'."
    (interactive)
    (let* ((icicle-transform-function 'icicle-remove-duplicates)
           (icicle-show-Completions-initially-flag t)
           (icicle-candidate-action-fn 'icicle-complete-keys-action)
           (icicle-alternative-sort-function (or icicle-sort-function
                                                 icicle-alternative-sort-function))
           (icicle-sort-function 'icicle-prefix-keys-first-p)
           (enable-recursive-minibuffers t)
           (orig-buff (current-buffer)) ; Used as free var in `icicle-complete-keys-action'.
           (orig-window (selected-window)) ; Used as free var in `icicle-complete-keys-action'.
           (icicle-completing-keys-p t)) ; Provide a condition to test key completion. 
      (icicle-complete-keys-1 (icicle-this-command-keys-prefix))))

  (defun icicle-prefix-keys-first-p (s1 s2)
    "S1 < S2 if S1 is a prefix key and S2 is not or S1 sorts < S2 normally.
Returns non-nil if S1 is a prefix key and either S2 is not or S1 < S2
in the normal sort order (`icicle-sort-function').  S1 and S2 must be
strings.  For this function, a prefix key is represented by a string
that ends in \"...\".

When used as a comparison function for completion candidates, this
makes prefix keys that match your input available first (at the top of
buffer *Completions*).  Candidates are effectively in two groups, each
of which is sorted alphabetically separately: prefix keys, followed by
non-prefix keys.

The special key representation \"..\" is less than all other keys,
including prefix keys."
    (let* ((prefix-string "  =  \\.\\.\\.$")
           (parent-string "..")
           (s1-prefix-p (string-match prefix-string s1))
           (s2-prefix-p (string-match prefix-string s2)))
      (and (not (string= parent-string s2))
           (or (string= parent-string s1)
               (and (not s1-prefix-p) (not s2-prefix-p) (string-lessp s1 s2))
               (and s1-prefix-p (not s2-prefix-p))
               (and s1-prefix-p s2-prefix-p (string-lessp s1 s2))))))

  (defun icicle-this-command-keys-prefix ()
    "Return the prefix of the currently invoked key sequence."
    (let ((this-key (this-command-keys))) (substring this-key 0 (1- (length this-key)))))

  (defun icicle-complete-keys-1 (prefix) ; PREFIX is a free var in `icicle-complete-keys-action'.
    "Complete a key sequence for prefix key PREFIX (a vector)."
    (let ((orig-extra-candidates icicle-extra-candidates)) ; Free in `icicle-complete-keys-action'.
      (unwind-protect
           (progn
             (icicle-keys+cmds-w-prefix prefix)
             (unless icicle-complete-keys-alist (error "No keys for prefix `%s'" prefix))
             (let* ((this-cmd-keys ; For error report - e.g. mouse cmd.
                     (this-command-keys-vector)) ; Free var in `icicle-complete-keys-action'.
                    (prefix-description
                     (icicle-key-description prefix (not icicle-key-descriptions-use-<>-flag)))
                    (candidate (completing-read
                                (concat "Complete keys" (and (not (string= "" prefix-description))
                                                             (concat " " prefix-description))
                                        ": ")
                                icicle-complete-keys-alist nil t nil nil
                                ;;$$ (if (equal [] prefix) nil "\\.\\.")
                                )))
               (icicle-complete-keys-action candidate)))
        (mapc (lambda (cand) (put (car cand) 'icicle-special-candidate nil))
              icicle-complete-keys-alist))))

  (defun icicle-complete-keys-action (candidate)
    "Completion action function for `icicle-complete-keys'."
    (let* ((key+binding (cdr-safe (assq (intern candidate) icicle-complete-keys-alist)))
           (key (car-safe key+binding))
           (binding (cdr-safe key+binding))
           (cmd-name nil)
           (action-window (selected-window)))
      (unwind-protect
           (progn
             ;; `orig-buff' and `orig-window' are free here.  Defined in `icicle-complete-keys'.
             (set-buffer orig-buff)
             (select-window orig-window)
             (if (string= ".." candidate)
                 (setq cmd-name "..")
               (unless (and (string-match "\\(.+\\)  =  \\(.+\\)" candidate) (match-beginning 2))
                 (error "No match"))
               (setq cmd-name (substring candidate (match-beginning 2) (match-end 2))))      
             ;; `prefix', `orig-extra-candidates', and `this-cmd-keys' are free vars here.
             ;; They are defined in `icicle-complete-keys-1'.
             (cond ((string= ".." cmd-name) ; Go back up to parent prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat (nbutlast (append prefix nil)))))
                   ((and key (string= "..." cmd-name)) ; Go down to prefix.
                    (setq last-command 'icicle-complete-keys)
                    (icicle-complete-keys-1 (vconcat prefix key)))
                   (t
                    (setq this-command binding last-command binding)
                    (setq icicle-extra-candidates orig-extra-candidates) ; Restore it.
                    (when (eq 'self-insert-command binding)
                      (unless key (error "Cannot insert `%s'" key))
                      (setq last-command-char (aref key 0)))
                    (when (eq 'digit-argument binding)
                      (setq last-command-char (aref key 0))
                      (icicle-msg-maybe-in-minibuffer "Numeric argument"))
                    (when (eq 'negative-argument binding)
                      (icicle-msg-maybe-in-minibuffer "Negative argument"))
                    (setq last-nonmenu-event 1) ; So *Completions* mouse-click info is ignored.
                    (condition-case try-command
                        (call-interactively binding nil this-cmd-keys)
                      (error (error (error-message-string try-command)))))))
        (select-window action-window))))

  (defun icicle-keys+cmds-w-prefix (prefix)
    "Fill `icicle-complete-keys-alist' for prefix key PREFIX (a vector)."
    (let ((prefix-map nil))
      (setq icicle-complete-keys-alist nil)
      (dolist (map (current-active-maps t))
        (setq prefix-map (lookup-key map prefix))
        ;; NOTE: `icicle-add-key+cmd' Uses `prefix' and `map' as free vars.
        (when (keymapp prefix-map) (map-keymap #'icicle-add-key+cmd prefix-map)))
      (unless (equal [] prefix) (push (list '\.\.) icicle-complete-keys-alist))
      icicle-complete-keys-alist))

  (defun icicle-add-key+cmd (event binding)
    "Add completion for EVENT and BINDING to `icicle-complete-keys-alist'."
    (cond
      ;; (menu-item ITEM-STRING): non-selectable item - skip it.
      ((and (eq 'menu-item (car-safe binding))
            (null (cdr-safe (cdr-safe binding))))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (ITEM-STRING): non-selectable item - skip it.
      ((and (stringp (car-safe binding)) (null (cdr-safe binding)))
       (setq binding nil))          ; So `keymapp' test, below, fails.
      
      ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
      ((eq 'menu-item (car-safe binding))
       (let ((enable-condition (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
         (if (or (not enable-condition)
                 (condition-case nil ; Don't enable if we can't check the condition.
                     (eval (cadr enable-condition))
                   (error nil)))
             (setq binding (car-safe (cdr-safe (cdr-safe binding))))
           (setq binding nil))))
      
      ;; (ITEM-STRING . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
      ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
      ((stringp (car-safe binding))
       (setq binding (cdr binding))
       ;; Skip HELP-STRING
       (when (stringp (car-safe binding)) (setq binding (cdr binding)))
       ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
       (when (and (consp binding) (consp (car binding))) (setq binding (cdr binding)))))
    
    ;; Follow indirections to ultimate symbol naming a command.
    (while (and (symbolp binding) (fboundp binding) (keymapp (symbol-function binding)))
      (setq binding (symbol-function binding)))

    ;; NOTE: `prefix' and `map' are free here.  They are bound in `icicle-keys+cmds-w-prefix'.
    (cond ((and (or (keymapp binding)
                    (and (commandp binding)
                         (equal binding (key-binding (vconcat prefix (vector event))))
                         (not (eq binding 'icicle-generic-S-tab))
                         (not (eq binding 'icicle-complete-keys))))
                (or (not (eq binding 'self-insert-command)) ; Command, keymap.
                    (and icicle-complete-keys-self-insert-flag ; Insert normal char.
                         (char-valid-p event))))
           (let ((candidate
                  (intern
                   (concat
                    (single-key-description event (not icicle-key-descriptions-use-<>-flag))
                    "  =  "
                    (if (keymapp binding) "..." (prin1-to-string binding))))))
             (push (cons candidate (cons (vector event) binding)) icicle-complete-keys-alist)
             (when (eq map (current-local-map)) (put candidate 'icicle-special-candidate t))))
          ((and (integerp event) (generic-char-p event) ; Insert generic char.
                (eq 'self-insert-command  binding))
           (ignore))))                  ; Placeholder for future use.

  ;; $$ No longer used.  Was used in `icicle-complete-keys-1'.
  (defun icicle-read-single-key-description (string need-vector &optional no-angles)
    "If STRING contains a space, then the vector containing the symbol named STRING.
Otherwise, call `icicle-read-kbd-macro'.
Other args are as for `icicle-read-kbd-macro'."
    (cond ((and no-angles (string-match " " string)) (vector (intern string)))
          ((string-match "^<\\([^>]* [^>]*\\)>" string)
           (vector (intern (substring string (match-beginning 1) (match-end 1)))))
          (t (icicle-read-kbd-macro string need-vector no-angles))))

  ;; $$ No longer used.  Was used as `icicle-candidate-action-fn' in `icicle-complete-keys'.
  (defun icicle-complete-keys-help (candidate)
    "Describe the command associated with the current completion candidate."
    (interactive)              ; Interactively, just describes itself.
    (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
    (string-match "\\(.+\\)  =  \\(.+\\)" candidate)
    (let ((frame-with-focus (selected-frame))
          (cmd (intern-soft (substring candidate (match-beginning 2) (match-end 2)))))
      (if (not (functionp cmd))
          (icicle-msg-maybe-in-minibuffer "No help")
        (describe-function cmd))
      (icicle-raise-Completions-frame)
      ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
      ;; because the *Help* frame takes the focus away from the minibuffer frame.
      ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
      (let* ((help-window (get-buffer-window "*Help*" t))
             (help-frame (and help-window (window-frame help-window))))
        (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
    (message nil))        ; Let minibuffer contents show immmediately.

  (defun icicle-read-kbd-macro (start &optional end no-angles)
    "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always.

Optional argument NO-ANGLES non-nil means to expect key
descriptions not to use angle brackets (<...>).  For example:
 
 (icicle-read-kbd-macro \"<mode-line>\" t)   returns [mode-line]
 (icicle-read-kbd-macro  \"mode-line\"  t t) returns [mode-line]"
    (interactive "r")
    (if (stringp start)
        (icicle-edmacro-parse-keys start end no-angles)
      (setq last-kbd-macro
            (icicle-edmacro-parse-keys (buffer-substring start end) nil no-angles))))

  (defun icicle-edmacro-parse-keys (string &optional need-vector no-angles)
    "Same as `edmacro-parse-keys', but with added NO-ANGLES argument.
NO-ANGLES is the same as for `icicle-read-kbd-macro'."
    (let ((case-fold-search nil)
          (pos 0)
          (res []))
      (while (and (< pos (length string))
                  (string-match "[^ \t\n\f]+" string pos))
        (let ((word (substring string (match-beginning 0) (match-end 0)))
              (key nil)
              (times 1))
          (setq pos (match-end 0))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" string pos)))
                ((and (string-match (if no-angles
                                        "^\\(\\([ACHMsS]-\\)*\\)\\(..+\\)$"
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$")
                                    word)
                      (or (not no-angles)
                          (save-match-data (not (string-match "^\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word (concat (substring word (match-beginning 1)
                                                      (match-end 1))
                                           (substring word (match-beginning 3)
                                                      (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (incf bits (cdr (assq (aref word 0)
                                           '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                             (?H . ?\H-\^@) (?M . ?\M-\^@)
                                             (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (incf prefix 2)
                     (callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (incf bits ?\C-\^@)
                     (incf prefix)
                     (callf substring word 1))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (loop for ch across word
                        for n = 0 then (+ (* n 8) ch -48)
                        finally do (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (loop for x across word collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\^@)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (loop repeat times do (callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res (edmacro-subseq res 2 -2)))
      (if (and (not need-vector)
               (loop for ch across res
                  always (and (char-valid-p ch)
                              (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                (and (>= ch2 0) (<= ch2 127))))))
          (concat (loop for ch across res
                     collect (if (= (logand ch ?\M-\^@) 0)
                                 ch (+ ch 128))))
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-cmd.el ends here
