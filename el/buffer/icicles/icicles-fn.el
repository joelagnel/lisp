;;; icicles-fn.el --- Non-interactive functions for Icicles
;;
;; Filename: icicles-fn.el
;; Description: Non-interactive functions for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:25:53 2006
;; Version: 22.0
;; Last-Updated: Fri Nov 10 16:39:32 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 2606
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-fn.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
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
;;  This is a helper library for library `icicles.el'.  It defines
;;  non-interactive functions.  See `icicles.el' for documentation.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-apropos-candidates',
;;    `icicle-barf-if-outside-Completions',
;;    `icicle-barf-if-outside-Completions-and-minibuffer',
;;    `icicle-barf-if-outside-minibuffer', `icicle-bind-isearch-keys',
;;    `icicle-call-then-update-Completions', `icicle-candidate-set-1',
;;    `icicle-choose-completion-string', `icicle-clear-minibuffer',
;;    `icicle-completing-read', `icicle-completion-setup-function',
;;    `icicle-control-reminder-prompt', `icicle-delete-if',
;;    `icicle-delete-if-not', `icicle-delete-whitespace-from-string',
;;    `icicle-display-Completions',
;;    `icicle-display-candidates-in-Completions',
;;    `icicle-expand-file-name', `icicle-file-directory-p',
;;    `icicle-file-name-apropos-candidates',
;;    `icicle-file-name-directory-w-default',
;;    `icicle-file-name-input-p', `icicle-file-name-nondirectory',
;;    `icicle-file-name-prefix-candidates', `icicle-file-readable-p',
;;    `icicle-file-writable-p', `icicle-files-within',
;;    `icicle-filter-wo-input', `icicle-fix-default-directory',
;;    `icicle-frames-on', `icicle-highlight-complete-input',
;;    `icicle-highlight-initial-whitespace',
;;    `icicle-increment-cand-nb+signal-end',
;;    `icicle-insert-Completions-help-string',
;;    `icicle-key-description', `icicle-longest-common-match',
;;    `icicle-maybe-sort-and-strip-candidates',
;;    `icicle-minibuffer-contents',
;;    `icicle-minibuffer-contents-from-minibuffer',
;;    `icicle-minibuffer-prompt-end',
;;    `icicle-msg-maybe-in-minibuffer', `icicle-next-candidate',
;;    `icicle-place-cursor', `icicle-place-overlay',
;;    `icicle-prefix-candidates', `icicle-read-file-name',
;;    `icicle-read-from-minibuffer', `icicle-read-string',
;;    `icicle-recompute-candidates',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remove-dots',
;;    `icicle-remove-duplicates', `icicle-remove-property',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-save-or-restore-input',
;;    `icicle-scroll-or-update-Completions', `icicle-set-difference',
;;    `icicle-set-intersection', `icicle-set-union',
;;    `icicle-sort-and-strip-ignored',
;;    `icicle-sort-case-insensitively', `icicle-sort-dirs-last',
;;    `icicle-start-of-candidates-in-Completions',
;;    `icicle-unsorted-apropos-candidates',
;;    `icicle-unsorted-file-name-apropos-candidates',
;;    `icicle-unsorted-file-name-prefix-candidates',
;;    `icicle-unsorted-prefix-candidates',
;;    `icicle-update-completions'.
;;
;;
;;  ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `completing-read'              - (See below and doc string.)
;;  `read-file-name'               - (See below and doc string.)
;;  `read-from-minibuffer'         - (See below and doc string.)
;;  `read-string'                  - (See below and doc string.)
;;
;;
;;  ***** NOTE: The following functions defined in `simple.el' have
;;              been REDEFINED HERE:
;;
;;  `choose-completion-string' -
;;     Don't exit minibuffer after `lisp-complete-symbol' completion.
;;  `completion-setup-function' - 1. Put faces on inserted string(s).
;;                                2. Help on help.
;;  `repeat-complex-command' - Use `completing-read' to read command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/11/10 dadams
;;     icicle-completing-read, icicle-read-file-name: Prefix prompt by + if a multi-command.
;; 2006/10/15 dadams
;;     icicle-save-or-restore-input: Change test from cmd is same as last to input is same as last.
;;     icicle-rebind-completion-maps: When turn off, bind C-M-mouse-2 and C-down-mouse-2 to nil.
;;     icicle-display-candidates-in-Completions: Accumulate (merge) highlight faces.
;;     Moved to icicles-mode.el:
;;       icicle-bind-isearch-keys, icicle-rebind-completion-maps,
;;       icicle-(redefine|restore)-standard-(commands|options),
;;       icicle-(redefine|restore)-std-completion-fns), icicle-(re|un)map,
;;       icicle-(bind|restore)-completion-keys, icicle-minibuffer-setup,
;;       icicle-cancel-*Help*-redirection, icicle-activate-mark,
;;       icicle-run-icicle-(pre|post)-command-hook, icicle-set-calling-cmd,
;;       icicle-undo-std-completion-faces, icicle-update-ignored-extensions-regexp,
;;       icicle-completing-p, icicle-restore-region-face.
;;     Removed eval-when-compile of *-face, *-var, *-mac, *-cmd.
;;     Removed some defvars for quieting byte compiler.
;; 2006/10/05 dadams
;;     icicle-display-candidates-in-Completions: Highlight candidates that are special.
;; 2006/10/03 dadams
;;     icicle-display-candidates-in-Completions:
;;       Removed predicate filtering, as the predicate doesn't necessarily apply to the candidate.
;;       This has been in the code forever, so commented it out, in case it is needed somehow ;-).
;; 2006/10/01 dadams
;;     icicle-alternative-sort -> icicle-toggle-alternative-sorting.
;;     icicle-update-completions: Treat icicle-prefix-word-complete case too.
;; 2006/09/30 dadams
;;     Added: icicle-key-description.
;;     icicle-(bind|restore)-completion-keys:
;;       Bind icicle-candidate-set-(save|retrieve) to C-M-(<|>), not C-(<|>).
;;       Bind icicle-toggle-angle-brackets to C-<.
;;       No longer remap help-command to icicle-completion-help.
;;       Bind icicle-completion-help to C-?.
;;       Rename [menu-bar minibuf C-h] to [menu-bar minibuf completion-help].
;;     icicle-completing-p: Bug fix: Use where-is-internal, not minibuffer-completion-table.
;; 2006/09/22 dadams
;;     icicle-minibuffer-setup:
;;       Apropos-complete, don't prefix-complete, when icicle-show-Completions-initially-flag.
;; 2006/09/17 dadams
;;     icicle-completing-p: Ensure minibuffer is active too.
;; 2006/09/16 dadams
;;     Bound icicle-insert-key-description to M-q.
;;     icicle-completing-read:
;;       Use icicle-list-join-string only to join parts of candidate (alist key).
;;       Append icicle-list-end-string instead.
;;     icicle-msg-maybe-in-minibuffer: Fixed doc string (active -> inactive).
;; 2006/09/12 dadams
;;     icicle-minibuffer-setup: Set icicle-pre-minibuffer-buffer.
;;     Renamed icicle-switch-to-minibuffer to icicle-insert-completion.
;; 2006/09/03 dadams
;;     Renamed icicle-show-Completions-help to icicle-show-Completions-help-flag.
;; 2006/08/27 dadams
;;     Bind Quit in Minibuf menu to icicle-abort-minibuffer-input.
;; 2006/08/22 dadams
;;     icicle-save-or-restore-input:
;;       If icicle-last-completion-candidate is nil, don't try to restore.
;; 2006/08/18 dadams
;;     icicle-minibuffer-setup: Reset icicle-last-completion-candidate to nil.
;;     icicle-rebind-completion-maps: Added icicle-Info-goto-node to icicle-completion-help-string.
;; 2006/08/15 dadams
;;     icicle-(bind|restore)-completion-keys:
;;       Bind icicle-help-on-(previous|next)-(apropos|prefix)-candidate.
;;       Reorder bindings.  Bind C-mouse-2 to 'ignore, not nil.
;;     icicle-rebind-completion-maps: Bind icicle-help-on-candidates in completion-list-mode-map.
;;     Added: icicle-barf-if-outside-Completions-and-minibuffer.
;; 2006/08/13 dadams
;;     icicle-completing-read, icicle-read-file-name: Use icicle-completing*-prompt-prefix.
;; 2006/08/04 dadams
;;     icicle-call-then-update-Completions:
;;       Call icicle-last-completion-command, not just prefix or apropos (so get prefix-word too).
;;     icicle-completing-read, icicle-read-file-name, icicle-next-candidate,
;;     icicle-recompute-candidates, icicle-call-then-update-Completions:
;;       Use icicle-remove-Completions-window.
;;     icicle-(bind|restore)-completion-keys: Bound icicle-pp-eval-expression to M-:.
;; 2006/08/03 dadams
;;     icicle-completion-setup-function: Removed useless highlighting code at end (Emacs 20).
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string.
;; 2006/07/30 dadams
;;     icicle-call-then-update-Completions: save match-data.
;; 2006/07/29 dadams
;;     icicle-activate-mark: Do it only if icicle-completing-p.  Thx to Le Wang.
;;     icicle-rebind-completion-maps:
;;       Updated to use icicle-dispatch-C-..
;;       Added icicle-toggle-search-cleanup to icicle-completion-help-string.
;;     icicle-bind-completion-keys:
;;       Use icicle-dispatch-C-. instead of icicle-toggle-ignored-extensions.
;; 2006/07/28 dadams
;;     icicle-longest-common-match: Treat special case of input such as "$" or "\\>$".
;; 2006/07/24 dadams
;;     icicle-call-then-update-Completions: Deactivate mark at the end.  Thx to Le Wang.
;; 2006/07/23 dadams
;;     Added: icicle-transform-candidates.
;;     icicle-rebind-completion-maps, icicle-(bind|restore)-completion-keys:
;;       Added icicle-toggle-transforming.
;;     icicle-unsorted(-file-name)-(apropos|prefix)-candidates: Use icicle-transform-candidates.
;; 2006/07/20 dadams
;;     Renamed icicle-arrows-respect-* to icicle-cycling-respects-completion-mode-flag.
;; 2006/07/19 dadams
;;     Applied patch from Damien Elmes <emacs@repose.cx>:
;;       Added icicle-insert-help-string, icicle-start-of-completions (factored from existing).
;;       icicle-completion-setup-function: Use icicle-insert-help-string.
;;       icicle-display-candidates-in-Completions:
;;         Use icicle-start-of-completions, and adjust loop accordingly.
;;       icicle-minibuffer-setup:
;;         Reset icicle-current-completion-type.
;;         Bind (up|down) to icicle-*-context-candidate, not (previous|next)-history-element.
;;       icicle-next-candidate: Use icicle-start-of-completions.
;;       icicle-scroll-or-update-Completions: Use icicle-scroll-completions.
;;     Renamed: icicle-start-of-completions to icicle-start-of-candidates-in-Completions,
;;              icicle-insert-help-string to icicle-insert-Completions-help-string,
;;              icicle-current-completion-type to icicle-current-completion-mode,
;;              icicle-*-context-candidate to icicle-(next|previous)-candidate-per-mode,
;;              icicle-scroll-completions to icicle-scroll-Completions.
;;     icicle-minibuffer-setup:
;;       Replaced icicle-display-Completions with icicle-prefix-complete, to get initial highlight.
;; 2006/07/18 dadams
;;     icicle-call-then-update-Completions:
;;       Delete *Completions* window, depending on icicle-Completions-display-min-input-chars.
;;         Thx to Damien Elmes.
;;     icicle-rebind-completion-maps: Add icicle-toggle-case-sensitivity to help list.
;;     icicle-bind-completion-keys: Bind icicle-toggle-case-sensitivity to S-C-a (i.e. C-A).
;; 2006/07/17 dadams
;;     icicle-call-then-update-Completions: sit-for delay if no candidates.  Thx to Damien Elmes.
;; 2006/07/09 dadams
;;     icicle-save-or-restore-input:
;;       Put back test that current input differs from last cycling candidate (user has edited it).
;;     icicle-next-candidate: Removed filtering with predicate (vestigial cruft).
;; 2006/07/08 dadams
;;     icicle-save-or-restore-input: Bug fix - Restore if currently cycling, not if not completing.
;; 2006/07/07 dadams
;;     icicle-display-candidates-in-Completions: Fixed test for historical candidate.
;;     Bound icicle-alternative-sort to M-,.  Updated icicle-completion-help-string.
;; 2006/07/05 dadams
;;     icicle-save-or-restore-input:
;;       For restoring: 1) No longer test if current input = *-last-completion-candidate.
;;                      2) No longer test if current input = icicle-initial-value.
;;       No longer save icicle-current-input as icicle-last-completion-candidate.
;;       Simplified the code.
;;     icicle-call-then-update-Completions: Do not set this-command or last-command.
;;     Renamed: icicle-current-regexp-input to icicle-current-raw-input.
;; 2006/07/04 dadams
;;     icicle-unsorted(-file-name)-prefix-candidates: Update icicle-common-match-string.
;;     icicle-unsorted-file-name-prefix-candidates:
;;       If prefix matches an empty directory, then use that directory as the sole completion.
;;     icicle-next-candidate: Use icicle-*-cycling-command properties.
;;                            Removed regexp-p argument in calls to icicle-save-or-restore-input.
;;     icicle-save-or-restore-input:
;;       Update icicle-common-match-string and icicle-current-regexp-input even if not regexp-p.
;;       Removed optional regexp-p argument.
;;       Do not update icicle-last-completion-candidate.
;;       Use icicle-*-*ing-command properties.
;;     icicle-recompute-candidates: Use icicle-*-cycling-command properties.
;; 2006/07/03 dadams
;;     Bug fixes -
;;       icicle-next-candidate:
;;         Don't reset icicle-common-match-string if this is an apropos cycling command
;;           and last command was an apropos command (cycling or completing).
;;         Do icicle-save-or-restore-input a second time, after recompute candidates,
;;           to pick up the common match.
;;         Always pass icicle-current-input to icicle-place-cursor.
;;       icicle-save-or-restore-input:
;;         Don't do anything if last command was a cycling command.
;;         Don't save input as regexp for C-l if this command is a cycling command,
;;           unless it is the first or it follows a completion command.
;; 2006/07/02 dadams
;;     icicle-place-cursor: position point & mark at least past prompt.  Thx to Peter Povinec.
;; 2006/06/09 dadams
;;     icicle(-file-name)-(apropos|prefix)-candidates: Reset icicle-candidate-nb to nil.
;;     icicle-recompute-candidates: Don't reset icicle-candidate-nb to nil.
;;     icicle-place-cursor: Prevent error on search-forward.
;; 2006/06/08 dadams
;;     icicle-save-or-restore-input: Do not restore if current command is completion.
;;     Added: icicle-expand-file-name.
;;     icicle-next-candidate: Don't pass NTH arg to icicle-display-candidates-in-Completions.
;; 2006/06/06 dadams
;;     icicle-control-reminder-prompt: Protect with condition-case, since it's on kill-emacs-hook.
;; 2006/06/01 dadams
;;     icicle-read-from-minibuffer: Emacs 22 removed the keep-all arg it had added.
;; 2006/05/31 dadams
;;     icicle-barf-if-outside*: Simplified.
;; 2006/05/30 dadams
;;     Bind icicle-erase-minibuffer-or-history to M-k also in non-completion minibuffer maps.
;; 2006/05/26 dadams
;;     Bind icicle-erase-minibuffer-or-history to M-k.
;;     Do not remap (or unmap) kill-sentence (it is on M-k in global map).
;; 2006/05/19 dadams
;;     Added: icicle-control-reminder-prompt.
;;     icicle-reminder-prompt-flag, icicle-read-file-name: Treat new values of icicle-reminder*.
;;     Renamed icicle-inhibit-reminder* to icicle-reminder*.
;; 2006/05/16 dadams
;;     Bug fix:
;;       icicle-recompute-candidates: Add new saved-last-input arg (replaces icicle-last-input).
;;       icicle-next-candidate: Pass saved old last input to icicle-recompute-candidates.
;; 2006/05/15 dadams
;;     Reverted change: icicle-unsorted(-file-name)-apropos-candidates, icicle-display-Completions:
;;       Use icicle-completion-nospace-flag, not nil.
;;     Renamed: icicle-completion-nospace-flag to icicle-ignore-space-prefix-flag.
;;     icicle-toggle-incremental-completion: C-#, icicle-toggle-ignored-space-prefix: C-^.
;; 2006/05/13 dadams
;;     icicle-unsorted(-file-name)-apropos-candidates, icicle-display-Completions:
;;       Use nil, not icicle-completion-nospace-flag.
;; 2006/05/12 dadams
;;     icicle-completion-help-string: Added faces and commands. Cleanup.
;;     Moved from icicles-cmd.el: icicle-barf-if-outside-*.
;; 2006/05/09 dadams
;;     icicle-display-*: Only issue Displaying... message when more candidates than threshold.
;; 2006/05/01 dadams
;;     icicle-save-or-restore-input: No-restore test is non-nil, not non-"", icicle-last-input.
;;     icicle-minibuffer-setup: Reset icicle-last-input to nil, not "".
;;     icicle-next-candidate: Highlight initial whitespace before underline root.
;; 2006/04/28 dadams
;;     icicle-save-or-restore-input:
;;       Restore empty input if it is not a file name.
;;       Don't expand empty common-match-string file-name input (it would lose trailing /).
;;     Added: icicle-highlight-initial-whitespace.
;;     icicle-next-candidate, icicle-call-then-update-Completions:
;;       Use icicle-highlight-initial-whitespace.
;; 2006/04/14 dadams
;;     icicle-call-then-update-Completions: Call icicle-update-input-hook.
;;     Bound icicle-insert-string-from-variable to C-=.  Added to icicle-completion-help-string.
;; 2006/04/09 dadams
;;     icicle-bind-completion-keys, icicle-minibuffer-setup:
;;       Deal with icicle-arrows-respect-completion-type-flag.
;;     icicle-display-candidates-in-Completions:
;;       Bug fix: regexp-quote common match when highlighting it.
;;     icicle-clear-minibuffer: Remove interactive spec.
;;     Moved to icicles-cmd.el: icicle-customize-apropos*, icicle-repeat-complex-command.
;; 2006/04/02 dadams
;;     Bound icicle-toggle-regexp-quote.
;; 2006/03/31 dadams
;;     icicle-next-candidate:
;;       Apply icicle-place-cursor to icicle-current-regexp-input if regexp-p.
;;     icicle-save-or-restore-input:
;;       Don't set icicle-current-regexp-input if this is a next-candidate action.
;; 2006/03/27 dadams
;;     icicle-place-overlay: Made generic: added args overlay, face, buffer, properties.
;; 2006/03/25 dadams
;;     icicle-call-then-update-Completions: Corrected use of icicle-incremental-completion*.
;; 2006/03/24 dadams
;;     Renamed icicle-expand-input-to-common-match to icicle-longest-common-match.  Rewrote it.
;;     icicle-call-then-update-Completions:
;;       Use icicle-incremental-completion-delay and -threshold.
;;     Mapped icicle-delete-char.
;; 2006/03/23 dadams
;;     icicle-expand-input-to-common-match:
;;       Return the longest common match.  Don't set icicle-common-match-string here.
;;     icicle-unsorted-*apropos-candidates: Set icicle-common-match-string here explicitly.
;;     Added: icicle-maybe-sort-and-strip-candidates.  Use in icicle-candidate-set-1.
;; 2006/03/22 dadams
;;     icicle-display-candidates-in-Completions:
;;       Removed root arg (always use icicle-current-input).
;;       Always highlight normal match part.
;;       Highlight common-match part if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input:
;;       Update regexp even if not icicle-expand-input-to-common-match-flag.
;;     icicle-recompute-candidates: If no candidates, then delete *Completions* window.
;;     icicle-next-candidate: Set default-directory only if icicle-file-name-input-p.
;;     Applied renamings of icicle-match-* faces.
;; 2006/03/21 dadams
;;     icicle-expand-input-to-common-match:
;;       Bug fixes:
;;         If no overlap between first and second candidates, then no common match.
;;         If no match with another candidate, then no common match.
;;         Input must match computed common match.
;;         When checking others, check only the added (pre|suf)fix, and reduce those as needed.
;;     icicle-save-or-restore-input:
;;       Bug fixes:
;;         When icicle-expand-input-to-common-match-flag, expand using directory from the
;;           input, not the default-directory.  Thx to cacher3.ericsson.net for report.
;;         Do test for case-only difference only when case-fold-search.
;;         If input is a directory (with slash), then use it as is.
;;         Save icicle-current-regexp-input if no icicle-common-match-string too.
;;     icicle-display-candidates-in-Completions: Use icicle-common-match-highlight-Completions.
;; 2006/03/20 dadams
;;     icicle-save-or-restore-input: Set icicle-current-regexp-input too.
;;                                   Corrected letter-case test.
;; 2006/03/19 dadams
;;     Added: icicle-expand-input-to-common-match.
;;     icicle-unsorted*-apropos-candidates:
;;       Set icicle-common-match-string if icicle-expand-input-to-common-match-flag.
;;     icicle-save-or-restore-input:
;;       Added regexp-p arg.  Update input to icicle-common-match-string if appropriate.
;;     icicle-next-candidate: Reset icicle-common-match-string.
;; 2006/03/17 dadams
;;     icicle-file-(read|writ)able-p: Put non-empty string condition first.
;;     Added: icicle-delete-whitespace-from-string.
;;     icicle-files-within: Moved here from icicle-cmd.el.
;; 2006/03/14 dadams
;;     Removed: icicle-reset-icicle-completing-p.
;;     icicle-completing-read, icicle-read-file-name: Removed icicle-icicle-completing-p.
;;     icicle-display-*: Added Displaying... message.
;; 2006/03/13 dadams
;;     Added: icicle-file-(read|writ)able-p.  Bound them to C-{ and C-} in minibuffer.
;;     icicle-rebind-completion-maps, icicle-bind-completion-keys: Added the new commands.
;;     icicle-recompute-candidates: Forgot icicle-keep-only-past-inputs in other branch.
;; 2006/03/10 dadams
;;     icicle-save-or-restore-input: Bug fix (thx to Toby Cubitt) - Not relative to default dir.
;;       Use directory-file-name, so don't include /.
;;       Use file-name-nondirectory, not file-relative-name if not cycling into subdirs.
;;     Renamed icicle-minibuffer-contents to icicle-minibuffer-contents-from-minibuffer.
;;     Added new icicle-minibuffer-contents, which can be called outside minibuffer.
;; 2006/03/08 dadams
;;     icicle-place-overlay: Use new face, icicle-current-candidate-highlight.
;; 2006/03/05 dadams
;;     Bound icicle-toggle-incremental-completion to C-^ in minibuffer.
;;     Updated icicle-completion-help-string with C-^ binding.
;;     icicle-display-candidates-in-Completions:
;;       Allow for on-the-fly changes to icicle-incremental-completion-flag.
;; 2006/03/01 dadams
;;     Added: icicle-clear-minibuffer.  Use in icicle-next-candidate.
;; 2006/02/27 dadams
;;     icicle-call-then-update-Completions: Set last-command to fn arg.
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

(eval-when-compile (require 'cl)) ;; case
                                  ;; plus, for Emacs < 21: dolist, push, pop
                                  ;; plus, for Emacs < 20: when, unless

(eval-when-compile (require 'icicles-opt)) ;; icicle-init-value-flag

;; Byte-compiling this file, you will likely get some error or warning
;; messages. All of the following are benign.  They are due to
;; differences between different versions of Emacs.
;;
;; Compiling in Emacs 22:
;;
;; Warning: `directory-sep-char' is an obsolete variable (as of Emacs 21.1); do not use it.
;; Warning: `make-local-hook' is an obsolete function (as of Emacs 21.1); not necessary any more.
;;
;; Compiling in Emacs 20:
;;
;; The following functions are not known to be defined:
;;     minibufferp, minibuffer-prompt-end, field-string, minibuffer-contents,
;;     display-mouse-p, propertize, dabbrev--reset-global-variables,
;;     dabbrev--abbrev-at-point, dabbrev--minibuffer-origin,
;;     dabbrev--find-all-expansions, dabbrev--substitute-expansion,
;;     face-spec-reset-face, set-face-attribute,
;;     minibuffer-contents-no-properties


;;; Defvars to quiet byte-compiler

(defvar directory-sep-char)
(defvar partial-completion-mode)
(defvar completion-root-regexp)
(defvar minibuffer-prompt-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Noninteractive Functions -------------------------------


;;; Redefined standard functions............................


;;; REPLACE ORIGINAL `choose-completion-string' in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Don't exit minibuffer if this is just a `lisp-complete-symbol' completion.
;;; Free variable `completion-reference-buffer' is defined in `simple.el'.
;;;
(or (fboundp 'old-choose-completion-string)
(fset 'old-choose-completion-string (symbol-function 'choose-completion-string)))

;;;###autoload
(when (< emacs-major-version 22)
  (defun icicle-choose-completion-string (choice &optional buffer base-size)
    "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name and CHOICE is a directory
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
    (let ((buffer (or buffer completion-reference-buffer))) ; In `simple.el'.
      ;; If BUFFER is a minibuffer, barf unless it's currently active.
      (when (and (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name buffer))
                 (or (not (active-minibuffer-window))
                     (not (equal buffer (window-buffer (active-minibuffer-window))))))
        (error "Minibuffer is not active for completion"))
      ;; Insert the completion into the buffer where completion was requested.
      (set-buffer buffer)
      (if base-size
          (delete-region (+ base-size (point-min)) (point))
        (choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
        (set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice,
      ;; unless this was a `lisp-complete-symbol' completion.
      (and (not completion-no-auto-exit)
           (equal buffer (window-buffer (minibuffer-window)))
           minibuffer-completion-table
           (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
           ;; If this is reading a file name, and the file name chosen
           ;; is a directory, don't exit the minibuffer.
           (if (and (eq minibuffer-completion-table 'read-file-name-internal)
                    (file-directory-p (buffer-string)))
               (select-window (active-minibuffer-window))
             (exit-minibuffer))))))

;;;###autoload
(when (>= emacs-major-version 22)
  (defun icicle-choose-completion-string (choice &optional buffer base-size)
    "Switch to BUFFER and insert the completion choice CHOICE.
BASE-SIZE, if non-nil, says how many characters of BUFFER's text
to keep.  If it is nil, we call `choose-completion-delete-max-match'
to decide what to delete.
If BUFFER is the minibuffer, then exit the minibuffer, unless one of
the following is true:
   - it is reading a file name and CHOICE is a directory
   - `completion-no-auto-exit' is non-nil
   - this is just a `lisp-complete-symbol' completion."
    (let* ((buffer (or buffer completion-reference-buffer)) ; In `simple.el'.
           (mini-p (minibufferp buffer)))
      ;; If BUFFER is a minibuffer, barf unless it's the currently
      ;; active minibuffer.
    (if (and mini-p
             (or (not (active-minibuffer-window))
                 (not (equal buffer (window-buffer (active-minibuffer-window))))))
        (error "Minibuffer is not active for completion")
      ;; Set buffer so buffer-local choose-completion-string-functions works.
      (set-buffer buffer)
      (unless (run-hook-with-args-until-success 'choose-completion-string-functions
                                                choice buffer mini-p base-size)
      ;; Insert the completion into the buffer where completion was requested.
      (if base-size
          (delete-region (+ base-size (if mini-p (minibuffer-prompt-end) (point-min))) (point))
        (choose-completion-delete-max-match choice))
      (insert choice)
      (remove-text-properties (- (point) (length choice)) (point) '(mouse-face nil))
      ;; Update point in the window that BUFFER is showing in.
      (let ((window (get-buffer-window buffer t)))
        (set-window-point window (point)))
      ;; If completing for the minibuffer, exit it with this choice,
      ;; unless this was a `lisp-complete-symbol' completion.
      (and (not completion-no-auto-exit)
           (equal buffer (window-buffer (minibuffer-window)))
           minibuffer-completion-table
           (not (eq 'lisp-complete-symbol icicle-cmd-calling-for-completion))
           ;; If this is reading a file name, and the file name chosen
           ;; is a directory, don't exit the minibuffer.
           (if (and (eq minibuffer-completion-table 'read-file-name-internal)
                    (file-directory-p (field-string (point-max))))
                 (let ((mini (active-minibuffer-window)))
                   (select-window mini)
                   (when minibuffer-auto-raise (raise-frame (window-frame mini))))
             (exit-minibuffer))))))))



;;; REPLACE ORIGINAL `completion-setup-function' in `simple.el',
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; 1. Put faces on inserted strings.  2. Help on help.
;;;
(or (fboundp 'old-completion-setup-function)
(fset 'old-completion-setup-function (symbol-function 'completion-setup-function)))

;;;###autoload
(when (< emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf (current-buffer)))
        (set-buffer standard-output)
        (completion-list-mode)
        (make-local-variable 'completion-reference-buffer)
        (setq completion-reference-buffer mainbuf)
        (if (eq minibuffer-completion-table 'read-file-name-internal)
            ;; For file name completion,
            ;; use the number of chars before the start of the
            ;; last file name component.
            (setq completion-base-size (save-excursion
                                         (set-buffer mainbuf)
                                         (goto-char (point-max))
                                         (skip-chars-backward (format "^%c" directory-sep-char))
                                         (- (point) (point-min))))
          ;; Otherwise, in minibuffer, the whole input is being completed.
          (save-match-data
            (if (string-match "\\` \\*Minibuf-[0-9]+\\*\\'" (buffer-name mainbuf))
                (setq completion-base-size 0))))
        (icicle-insert-Completions-help-string)))))

;;;###autoload
(when (>= emacs-major-version 22)
  (defun icicle-completion-setup-function ()
    "Set up for completion.  This goes in `completion-setup-hook'
so it is called after completion-list buffer text is written."
    (save-excursion
      (let* ((mainbuf (current-buffer))
             (mbuf-contents (minibuffer-contents)))
        ;; When reading a file name in the minibuffer,
        ;; set default-directory in the minibuffer
        ;; so it will get copied into the completion list buffer.
        (if minibuffer-completing-file-name
            (with-current-buffer mainbuf
              (setq default-directory (file-name-directory mbuf-contents))))
        ;; If partial-completion-mode is on, point might not be after the
        ;; last character in the minibuffer.
        ;; FIXME: This still doesn't work if the text to be completed
        ;; starts with a `-'.
        (when (and partial-completion-mode (not (eobp)))
          (setq mbuf-contents
                (substring mbuf-contents 0 (- (point) (point-max)))))
        (with-current-buffer standard-output
          (completion-list-mode)
          (make-local-variable 'completion-reference-buffer)
          (setq completion-reference-buffer mainbuf)
          (if minibuffer-completing-file-name
              ;; For file name completion,
              ;; use the number of chars before the start of the
              ;; last file name component.
              (setq completion-base-size
                    (with-current-buffer mainbuf
                      (save-excursion
                        (goto-char (point-max))
                        (skip-chars-backward completion-root-regexp)
                        (- (point) (minibuffer-prompt-end)))))
            ;; Otherwise, in minibuffer, the whole input is being completed.
            (if (minibufferp mainbuf) (setq completion-base-size 0)))
          ;; Put faces on first uncommon characters and common parts.
          (when completion-base-size
            (let* ((common-string-length
                    (- (length mbuf-contents) completion-base-size))
                   (element-start (next-single-property-change (point-min) 'mouse-face))
                   (element-common-end
                    (and element-start (+ (or element-start nil) common-string-length)))
                   (maxp (point-max)))
              (while (and element-start (< element-common-end maxp))
                (when (and (get-char-property element-start 'mouse-face)
                           (get-char-property element-common-end 'mouse-face))
                  (put-text-property element-start element-common-end
                                     'font-lock-face 'completions-common-part)
                  (put-text-property element-common-end (1+ element-common-end)
                                     'font-lock-face 'completions-first-difference))
                (setq element-start (next-single-property-change element-start 'mouse-face))
                (if element-start
                    (setq element-common-end  (+ element-start common-string-length))))))
          (icicle-insert-Completions-help-string))))))

(defun icicle-insert-Completions-help-string ()
  "Add or remove help in *Completions*.
This is controlled by `icicle-show-Completions-help-flag'.  If that
option is nil, remove help; else, add it."
  (if icicle-show-Completions-help-flag
      (let ((instruction2 (or (and icicle-mode
                                   (substitute-command-keys
                                    (concat "(\\<minibuffer-local-completion-map>"
                                            "\\[icicle-completion-help]: help) ")))
                              ""))
            instruction1)
        (cond ((< emacs-major-version 22)
               (setq instruction1 (if window-system ; We have a mouse.
                                      (substitute-command-keys
                                       "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] on a completion to select it.  ")
                                    (substitute-command-keys ; No mouse.
                                     "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  "))))
              ((>= emacs-major-version 22)
               (setq instruction1 (if (display-mouse-p) ; We have a mouse.
                                      (substitute-command-keys
                                       "Click \\<completion-list-mode-map>\
\\[mouse-choose-completion] or type \\[choose-completion] on a completion to select it.  ")
                                    (substitute-command-keys ; No mouse.
                                     "In this buffer, type \\<completion-list-mode-map>\
\\[choose-completion] to select the completion near point.  ")))))
        (goto-char (point-min))
        (put-text-property 0 (length instruction1) 'face 'icicle-Completions-instruction-1
                           instruction1)
        (put-text-property 0 (length instruction2) 'face 'icicle-Completions-instruction-2
                           instruction2)
        (insert instruction1 instruction2 "\n\n"))

    ;; Not showing help.  Remove standard Emacs help string.
    (goto-char (point-min))
    (re-search-forward "Possible completions are:\n")
    (delete-region (point-min) (point))))


;;; REPLACE ORIGINAL `completing-read' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Allows for completions that are lists of strings.
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-completing-read)
(fset 'old-completing-read (symbol-function 'completing-read)))

;;;###autoload
(defun icicle-completing-read
    (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer, with completion and cycling of completions.
Type `\\[exit-minibuffer]' to end your input.

Prefix completion via \\<minibuffer-local-completion-map>\
`\\[icicle-prefix-word-complete]' (word) and `\\[icicle-prefix-complete]' (full).
Apropos (regexp) completion via `\\[icicle-apropos-complete]'.

Prefix cycling of candidate completions via `\\[icicle-previous-prefix-candidate]' and \
`\\[icicle-next-prefix-candidate]'.
Apropos cycling of candidate completions via `\\[icicle-previous-apropos-candidate]' and \
`\\[icicle-next-apropos-candidate]'.

Cycling of past minibuffer inputs via `\\[previous-history-element]' and \
`\\[next-history-element]'.
Searching through input history via `\\[previous-matching-history-element]' \
and `\\[next-matching-history-element]'.

Case is ignored if `completion-ignore-case' is non-nil.  For file-name
  completion, `read-file-name-completion-ignore-case' is used instead.
For file-name completion, cycling into subdirectories is determined by
  `icicle-cycle-into-subdirs-flag'.
Position of the cursor (point) and the mark during completion cycling
  is determined by `icicle-point-position-in-candidate' and
  `icicle-mark-position-in-candidate', respectively.
Highlighting of the matched part of completion candidates during
  cycling is determined by `icicle-match-highlight-minibuffer',
  `icicle-match-highlight-Completions', and
  `icicle-common-match-highlight-Completions'.

Use `\\[icicle-completion-help]' during completion for more information on completion and key
bindings in Icicle mode.

Args: PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST:

PROMPT is a string to prompt with; normally ends in a colon and space.

TABLE is an alist whose elements' cars are strings, or an obarray.

PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details on
completion, TABLE, PREDICATE.

If REQUIRE-MATCH is non-nil, you are not allowed to exit unless the
input is (or completes to) an element of TABLE or is null.  If it is
also not `t', `\\[exit-minibuffer]' doesn't exit if it effects non-null
completion.  If the input is null, `completing-read' returns an empty
string, regardless of the value of REQUIRE-MATCH.

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
If it is (STRING . POSITION), the initial input is STRING, but point
is placed POSITION characters into the string.

HIST, if non-nil, specifies a history list, and optionally the initial
position in the list.  It can be a symbol, which is the history list
variable to use, or it can be a cons cell (HISTVAR . HISTPOS).  In
that case, HISTVAR is the history list variable to use, and HISTPOS is
the initial position (the position in the list which INITIAL-INPUT
corresponds to).  Positions are counted starting from 1 at the
beginning of the list.

DEF, if non-nil, is the default value.

Non-nil `icicle-init-value-flag' means that when DEF is non-nil and
INITIAL-INPUT is nil or \"\", DEF is inserted in the minibuffer as the
INITIAL-INPUT.  The particular non-nil value determines whether or not
the value is preselected and, if preselected, where the cursor is left
\(at the beginning or end of the value).

If INHERIT-INPUT-METHOD is non-nil, the minibuffer inherits the
current input method and the setting of `enable-multibyte-characters'.

Completion ignores case when`completion-ignore-case' is non-nil."
  (unless initial-input (setq initial-input ""))
  (if (consp initial-input)
      (setq icicle-initial-value (car initial-input))
    (setq initial-input        (format "%s" initial-input) ; Convert symbol to string
          icicle-initial-value (or initial-input "")))
  (setq icicle-nb-of-other-cycle-candidates 0)

  ;; Maybe use DEF for INITIAL-INPUT also.
  (when (and icicle-init-value-flag def (stringp initial-input) (string= "" initial-input))
    (setq initial-input def))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))
  (let* ((minibuffer-completion-table table)
         (completing-prompt-prefix-symb (if require-match
                                       'icicle-completing-mustmatch-prompt-prefix
                                     'icicle-completing-prompt-prefix))
         (completing-prompt-prefix (symbol-value completing-prompt-prefix-symb))
         result)

    ;; Extension: candidate is a list of strings.  Used for multi-completion.
    (when (and (consp table) (consp (car table)) (consp (caar table)))
      (setq minibuffer-completion-table
            (setq table
                  (mapcar
                   (lambda (entry)
                     (cons (concat (mapconcat #'identity (car entry) icicle-list-join-string)
                                   icicle-list-end-string)
                           (cdr entry)))
                   table))))

    ;; Add `completing-prompt-prefix' (Emacs 21+).
    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)
           (setq result (old-completing-read icicle-prompt table predicate require-match
                                             initial-input hist def inherit-input-method)))
          ((or (null icicle-reminder-prompt-flag)
               (and (wholenump icicle-reminder-prompt-flag) (zerop icicle-reminder-prompt-flag))
               (> (length icicle-initial-value) ; No room to add suffix.
                  (- (window-width (minibuffer-window)) (length prompt))))
           (setq icicle-prompt
                 (if (fboundp 'propertize) ; Emacs 21+ only
                     (concat (and icicle-candidate-action-fn "+")
                             (propertize completing-prompt-prefix
                                         'face completing-prompt-prefix-symb)
                             (and (not (string= "" completing-prompt-prefix)) " ")
                             (propertize prompt 'face 'minibuffer-prompt))
                   (concat (and icicle-candidate-action-fn "+ ") prompt)))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (setq result (catch 'icicle-read-top
                            (old-completing-read icicle-prompt table predicate require-match
                                                 initial-input hist def inherit-input-method)))))
          (t                            ; Append suffix to prompt.
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (and icicle-candidate-action-fn "+")
                             (propertize completing-prompt-prefix
                                         'face completing-prompt-prefix-symb)
                             (and (not (string= "" completing-prompt-prefix)) " ")
                             (propertize prompt 'face 'minibuffer-prompt)
                             (propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
                             "  ")
                   (concat (and icicle-candidate-action-fn "+ ") prompt icicle-prompt-suffix "  ")))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (setq result
                   (catch 'icicle-read-top
                     (old-completing-read icicle-prompt table predicate require-match
                                          initial-input hist def inherit-input-method))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))


;;; REPLACE ORIGINAL `read-file-name' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Appends `icicle-prompt-suffix' if resulting prompt is not too long.
;;; Removes *Completions* window.
;;;
(or (fboundp 'old-read-file-name)
(fset 'old-read-file-name (symbol-function 'read-file-name)))

;;;###autoload
(defun icicle-read-file-name (prompt &optional dir default-filename
                              require-match initial-input predicate)
  "Read file name, prompting with prompt and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default the name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  but if INITIAL-INPUT is specified, that combined with DIR is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg REQUIRE-MATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg INITIAL-INPUT specifies text to start with.
If optional sixth arg PREDICATE is non-nil, possible completions and
 the resulting file name must satisfy `(funcall predicate NAME)'.
 This argument is only available starting with Emacs 21.
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

Non-nil `icicle-init-value-flag' means that when DEFAULT-FILENAME is
non-nil and INITIAL-INPUT is nil or \"\", DEFAULT-FILENAME is inserted
in the minibuffer as the INITIAL-INPUT.  The particular non-nil value
determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

If option `icicle-require-match-flag' is non-nil, it overrides the
value of REQUIRE-MATCH.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

Removes *Completions* window when done.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (setq icicle-initial-value                  (or initial-input "")
        icicle-nb-of-other-cycle-candidates   0)
  (icicle-fix-default-directory)        ; Make sure there are no backslashes in it.

  ;; Maybe use DEFAULT-FILENAME for INITIAL-INPUT also, after removing the directory part.
  ;; Note that if DEFAULT-FILENAME is null, then we let INITIAL-INPUT remain null too.
  (when (and icicle-init-value-flag default-filename (string= "" icicle-initial-value))
    (setq initial-input (file-name-nondirectory default-filename)))

  ;; Override REQUIRE-MATCH as needed.
  (setq require-match (case icicle-require-match-flag
                        ((nil) require-match)
                        (no-match-required nil)
                        (partial-match-ok t)
                        (full-match-required 'full-match-required)))
  (let* ((completing-prompt-prefix-symb (if require-match
                                      'icicle-completing-mustmatch-prompt-prefix
                                    'icicle-completing-prompt-prefix))
         (completing-prompt-prefix (symbol-value completing-prompt-prefix-symb))
         result)
    ;; Append suffix if prompt is not too long.
    ;; Use face on suffix if (boundp 'minibuffer-prompt-properties).
    (cond ((not icicle-mode)
           (setq icicle-prompt prompt)
           (condition-case nil          ; If Emacs 22+, use predicate arg.
               (setq result (old-read-file-name icicle-prompt dir default-filename
                                                require-match initial-input predicate))
             (wrong-number-of-arguments
              (setq result (old-read-file-name icicle-prompt dir default-filename
                                               require-match initial-input)))))
          ((or (null icicle-reminder-prompt-flag)
               (and (wholenump icicle-reminder-prompt-flag) (zerop icicle-reminder-prompt-flag))
               (> (length initial-input) ; No room to add suffix.
                  (- (window-width (minibuffer-window)) (length prompt))))
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (and icicle-candidate-action-fn "+")
                             (propertize completing-prompt-prefix
                                         'face completing-prompt-prefix-symb)
                             (and (not (string= "" completing-prompt-prefix)) " ")
                             (propertize prompt 'face 'minibuffer-prompt))
                   (concat (and icicle-candidate-action-fn "+ ") prompt)))
           (let ((minibuffer-prompt-properties
                  (and (boundp 'minibuffer-prompt-properties) ; Emacs 21+ only
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (condition-case nil          ; If Emacs 22+, use predicate arg.
                 (setq result
                       (catch 'icicle-read-top
                         (old-read-file-name icicle-prompt dir default-filename
                                             require-match initial-input predicate)))
               (wrong-number-of-arguments
                (setq result
                      (catch 'icicle-read-top
                        (old-read-file-name icicle-prompt dir default-filename
                                            require-match initial-input)))))))
          (t                            ; Append suffix to prompt.
           (setq icicle-prompt
                 (if (fboundp 'propertize)
                     (concat (and icicle-candidate-action-fn "+")
                             (propertize completing-prompt-prefix
                                         'face completing-prompt-prefix-symb)
                             (and (not (string= "" completing-prompt-prefix)) " ")
                             (propertize prompt 'face 'minibuffer-prompt)
                             (propertize icicle-prompt-suffix 'face 'icicle-prompt-suffix)
                             "  ")
                   (concat (and icicle-candidate-action-fn "+ ")
                           prompt icicle-prompt-suffix "  ")))
           (let ((minibuffer-prompt-properties ; If Emacs 22+, use pred and suffix face.
                  (and (boundp 'minibuffer-prompt-properties)
                       (icicle-remove-property 'face minibuffer-prompt-properties))))
             (condition-case nil
                 (setq result
                       (catch 'icicle-read-top
                         (old-read-file-name icicle-prompt dir default-filename
                                             require-match initial-input predicate)))
               (wrong-number-of-arguments
                (setq result
                      (catch 'icicle-read-top
                        (old-read-file-name icicle-prompt dir default-filename
                                            require-match initial-input))))))))
    ;; HACK.  Without this, when REQUIRE-MATCH is non-nil, *Completions* window
    ;; does not disappear.
    (when require-match (icicle-remove-Completions-window))
    result))

(defun icicle-fix-default-directory ()
  "Convert backslashes in `default-directory' to slashes."
;; This is a hack.  If you do `C-x 4 f' from a standalone minibuffer
;; frame, `default-directory' on MS Windows has this form:
;; `C:\some-dir/'.  There is a backslash character in the string.  This
;; is not a problem for standard Emacs, but it is a problem for Icicles,
;; because we interpret backslashes using regexp syntax - they are not
;; file separators for Icicles.  So, we call `substitute-in-file-name' to
;; change all backslashes in `default-directory' to slashes.  This
;; shouldn't hurt, because `default-directory' is an absolute directory
;; name - it doesn't contain environment variables.  For example, we
;; convert `C:\some-dir/' to `c:/some-directory/'."
  (setq default-directory (substitute-in-file-name default-directory)))


(defun icicle-remove-property (prop plist)
  "Remove property PROP from property-list PLIST, non-destructively.
Returns the modified copy of PLIST."
  (let ((cpy plist)
        (result nil))
    (while cpy
      (unless (eq prop (car cpy)) (setq result `(,(cadr cpy) ,(car cpy) ,@result)))
      (setq cpy (cddr cpy)))
    (nreverse result)))



;;; REPLACE ORIGINAL `read-from-minibuffer' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-from-minibuffer)
(fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;;;###autoload
(defun icicle-read-from-minibuffer (prompt &optional initial-contents keymap read hist
                                    default-value inherit-input-method keep-all)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an alternative to
  DEFAULT-VALUE.  Vanilla Emacs considers it to be obsolete, but
  Icicles does not.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eighth arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.  This is available starting with Emacs 22.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

Non-nil `icicle-init-value-flag' means that when DEFAULT-VALUE is
non-nil and INITIAL-CONTENTS is nil or \"\", DEFAULT-VALUE is inserted
in the minibuffer as the INITIAL-CONTENTS.  The particular non-nil
value determines whether or not the value is preselected and, if
preselected, where the cursor is left \(at the beginning or end of the
value).

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is (STRING . POSITION), the initial input
is STRING, but point is placed at _one-indexed_ position POSITION in
the minibuffer.  Any integer value less than or equal to one puts
point at the beginning of the string.  *Note* that this behavior
differs from the way such arguments are used in `completing-read' and
some related functions, which use zero-indexing for POSITION."
  (unless initial-contents (setq initial-contents ""))
  ;; Maybe use DEFAULT-VALUE for INITIAL-CONTENTS also.
  (when (and icicle-init-value-flag default-value (stringp initial-contents)
             (string= "" initial-contents))
    (setq initial-contents default-value))
  (old-read-from-minibuffer prompt initial-contents keymap read hist default-value
                            inherit-input-method))



;;; REPLACE ORIGINAL `read-string' (built-in function),
;;; saving it for restoration when you toggle `icicle-mode'.
;;;
;;; Respect `icicle-init-value-flag'.
;;;
(or (fboundp 'old-read-string)
(fset 'old-read-string (symbol-function 'read-string)))

;;;###autoload
(defun icicle-read-string (prompt &optional initial-input history
                           default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
  Vanilla Emacs considers it to be obsolete, but Icicles does not.  It
  behaves as in `read-from-minibuffer'.  See the documentation string
  of `read-from-minibuffer' for details.
The third arg HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of enable-multibyte-characters."
  (let ((value (read-from-minibuffer prompt initial-input nil nil
                                     history default-value inherit-input-method)))
    (if (and default-value (equal value "")) default-value value)))



 
;;; Icicle functions - completion display (not cycling).....

(defun icicle-display-candidates-in-Completions (&optional reverse-p no-display-p)
  "Refresh the current set of completion candidates in *Completions*.
REVERSE-P non-nil means display the candidates in reverse order.
NO-DISPLAY-P means do not display the candidates; just recompute them."
;;$$$   ;; Pred is special if `minibuffer-completion-table' is a function.
;;   (when (and (not (functionp minibuffer-completion-table))
;;              (functionp minibuffer-completion-predicate))
;;     (setq icicle-completion-candidates
;;           (icicle-delete-if-not
;;            (lambda (cand)
;;              (funcall minibuffer-completion-predicate
;;                       (if (arrayp minibuffer-completion-table) (intern cand) (list cand))))
;;            icicle-completion-candidates)))
  (case icicle-incremental-completion-flag
    ((t always) (setq icicle-incremental-completion-p 'always))
    ((nil) (setq icicle-incremental-completion-p nil)))
  (unless no-display-p
    (when (> (length icicle-completion-candidates) icicle-incremental-completion-threshold)
      (message "Displaying completion candidates..."))
    (with-output-to-temp-buffer "*Completions*"
      ;; `condition-case' shouldn't be needed, but it prevents an "End of buffer"
      ;; message from `display-completion-list' on Emacs 22.
      (condition-case nil
          (display-completion-list (if reverse-p
                                       (reverse icicle-completion-candidates)
                                     icicle-completion-candidates))
        (error nil)))
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer "*Completions*"))
        (let ((buffer-read-only nil)
              (eob (point-max))
              (case-fold-search completion-ignore-case)
              (dir (and (icicle-file-name-input-p) (file-name-directory icicle-last-input)))
              (hist (and (symbolp minibuffer-history-variable)
                         (symbol-value minibuffer-history-variable))))
          (goto-char (icicle-start-of-candidates-in-Completions))
          (while (not (eobp))
            (let* ((beg (point))
                   (end (next-single-property-change beg 'mouse-face nil eob))
                   (next (next-single-property-change end 'mouse-face nil eob))
                   (faces nil))

              ;; Highlight candidate (`*-historical-candidate') if it has been used previously.
              ;; Highlight it specially (`icicle-special-candidate') if it is a special candidate.
              (let ((candidate (icicle-current-completion-in-Completions)))
                (when dir (setq candidate (expand-file-name candidate dir)))
                (when (or (and icicle-special-candidate-regexp
                               (string-match icicle-special-candidate-regexp candidate))
                          (get (intern candidate) 'icicle-special-candidate))
                  (setq faces (cons 'icicle-special-candidate faces))
                  (put-text-property beg end 'face faces))
                (when (and (consp hist) (member candidate hist))
                  (put-text-property beg end 'face (cons 'icicle-historical-candidate faces))))

              ;; Highlight, inside the candidate, the longest common match.
              (when (and icicle-expand-input-to-common-match-flag
                         (not (string= "" icicle-current-input)))
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end) ; Search within the completion candidate.
                    (when (re-search-forward
                           (regexp-quote (if (icicle-file-name-input-p)
                                             (icicle-file-name-nondirectory icicle-current-input)
                                           icicle-current-input))
                           nil t)
                      (setq faces (cons 'icicle-common-match-highlight-Completions faces))
                      (put-text-property (match-beginning 0) (point) 'face faces)))))

              ;; Highlight, inside the candidate, what the input expression matches.
              (unless (string= "" icicle-current-raw-input)
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end) ; Search within the completion candidate.
                    (when (re-search-forward (if (icicle-file-name-input-p)
                                                 (icicle-file-name-nondirectory
                                                  icicle-current-raw-input)
                                               icicle-current-raw-input)
                                             nil t)
                      (setq faces (cons 'icicle-match-highlight-Completions faces))
                      (put-text-property (match-beginning 0) (point) 'face faces)))))
              (goto-char next))))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))
    (message nil)))                     ; Clear out any "Looking for..."

(defun icicle-place-cursor (input)
  "Position point and mark with respect to the minibuffer candidate.
Positions are `icicle-point-position-in-candidate' and
`icicle-mark-position-in-candidate', respectively.
INPUT is the current user input, that is, the completion root."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (goto-char (icicle-minibuffer-prompt-end))
    (setq input-start-position (point))
    (when (and (icicle-file-name-input-p) insert-default-directory)
      (search-forward (icicle-file-name-directory-w-default input) nil t)
      (setq input-start-position (point))) ; Skip directory.
    ;; Locate completion root within current completion candidate.
    (when (or (memq icicle-point-position-in-candidate '(root-start root-end))
              (memq icicle-mark-position-in-candidate  '(root-start root-end)))
      (save-excursion
        (save-restriction
          (narrow-to-region (point) (point-max)) ; Search within the completion candidate.
          (re-search-forward (if (icicle-file-name-input-p)
                                 (icicle-file-name-nondirectory input)
                               input)
                             nil t))))
    ;; Position point.
    (case icicle-point-position-in-candidate
      (input-start (goto-char input-start-position))
      (input-end (goto-char (point-max)))
      (root-start (goto-char (max input-start-position (match-beginning 0))))
      (root-end (goto-char (max input-start-position (match-end 0)))))
    ;; Position mark.
    (unless (eq icicle-point-position-in-candidate icicle-mark-position-in-candidate)
      (push-mark (case icicle-mark-position-in-candidate
                   (input-start input-start-position)
                   (input-end (point-max))
                   (root-start (max input-start-position (match-beginning 0)))
                   (root-end (max input-start-position (match-end 0))))
                 'nomsg
                 'activate-mark))))

(defun icicle-highlight-initial-whitespace (input)
  "Highlight any initial whitespace in your input (it might be a typo).
Only if `icicle-highlight-input-initial-whitespace-flag' is non-nil.
INPUT is the current user input, that is, the completion root.
This must be called in the minibuffer."
  (when (and icicle-highlight-input-initial-whitespace-flag (not (string= "" input)))
    (let ((case-fold-search completion-ignore-case)
          input-start-position)
      (save-excursion
        (goto-char (icicle-minibuffer-prompt-end))
        (setq input-start-position (point))
        (when (and (icicle-file-name-input-p) insert-default-directory
                   (search-forward (icicle-file-name-directory-w-default input) nil t))
          (setq input-start-position (point))) ; Skip directory.
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (point-max)) ; Search within completion candidate.
            (while (and (not (eobp)) (looking-at "\\(\\s-\\|\n\\)+"))
              (put-text-property (point) (1+ (point))
                                 'face 'icicle-whitespace-highlight)
              (forward-char 1))
            ;; Remove any previous whitespace highlighting that is no longer part of prefix.
            (while (not (eobp))
              (put-text-property (point) (1+ (point)) 'face nil)
              (forward-char 1))))))))

(defun icicle-minibuffer-prompt-end ()
  "Version of `minibuffer-prompt-end' that works for Emacs 20 and later."
  (if (fboundp 'minibuffer-prompt-end) (minibuffer-prompt-end) (point-min)))



 
;;; Icicles functions - prefix completion cycling...........

(defun icicle-prefix-candidates (input)
  "List of candidate prefix completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (if icicle-sort-function
      (sort (icicle-unsorted-prefix-candidates input) icicle-sort-function)
    (icicle-unsorted-prefix-candidates input)))

(defun icicle-unsorted-prefix-candidates (input)
  "Unsorted list of prefix completions for the current partial INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common prefix over
all candidates."
  (let* ((candidates (icicle-transform-candidates
                      (all-completions input minibuffer-completion-table
                                       minibuffer-completion-predicate
                                       icicle-ignore-space-prefix-flag)))
         (filtered-candidates (append icicle-extra-candidates
                                      (icicle-delete-if-not
                                       (lambda (cand)
                                         (let ((case-fold-search completion-ignore-case))
                                           (icicle-filter-wo-input cand)))
                                       candidates))))
    (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
      (let ((common-prefix (try-completion input minibuffer-completion-table
                                           minibuffer-completion-predicate)))
        (setq icicle-common-match-string (if (eq t common-prefix) input common-prefix))))
    filtered-candidates))

(defun icicle-file-name-prefix-candidates (input)
  "List of prefix completions for partial file name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-prefix-candidates (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-prefix-candidates (input)
  "Unsorted list of prefix completions for the current file-name INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common prefix over
all candidates."
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (let* ((candidates (icicle-transform-candidates
                        (all-completions input minibuffer-completion-table
                                         (if slashed-p "/" default-directory)
                                         icicle-ignore-space-prefix-flag)))
           (filtered-candidates
            (append icicle-extra-candidates
                    (icicle-delete-if-not
                     (lambda (cand)
                       (let ((case-fold-search completion-ignore-case))
                         (if (member cand '("../" "./"))
                             (member input '(".." ".")) ; Prevent "" from matching "../"
                           (and (string-match (concat "^" (regexp-quote input)) cand)
                                (icicle-filter-wo-input cand)))))
                     candidates))))
      (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
        (let ((common-prefix (try-completion input minibuffer-completion-table
                                             (if slashed-p "/" default-directory))))
          ;; If prefix matches an empty directory, then use that directory as the sole completion.
          (when (and (stringp common-prefix) (string-match "/\\.$" common-prefix))
            (setq common-prefix (substring common-prefix 0 (- (length common-prefix) 2))))
          (setq icicle-common-match-string (if (eq t common-prefix) input common-prefix))))
      filtered-candidates)))



 
;;; Icicles functions - apropos completion cycling..........

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (if icicle-sort-function
      (sort (icicle-unsorted-apropos-candidates input) icicle-sort-function)
    (icicle-unsorted-apropos-candidates input)))

(defun icicle-unsorted-apropos-candidates (input)
  "Unsorted list of apropos completions for the current partial INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common match of input
over all candidates."
  (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
  (let* ((candidates (icicle-transform-candidates
                      (all-completions "" minibuffer-completion-table
                                       minibuffer-completion-predicate
                                       icicle-ignore-space-prefix-flag)))
         (filtered-candidates (append icicle-extra-candidates
                                      (icicle-delete-if-not
                                       (lambda (cand)
                                         (let ((case-fold-search completion-ignore-case))
                                           (and (string-match input cand)
                                                (icicle-filter-wo-input cand))))
                                       candidates))))
    (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
      (setq icicle-common-match-string (icicle-longest-common-match input filtered-candidates)))
    filtered-candidates))               ; Return candidates.

(defun icicle-file-name-apropos-candidates (input)
  "List of apropos completions for partial file-name INPUT.
INPUT is a string.
Candidates can be directories.  Each candidate is a string."
  (setq icicle-candidate-nb nil)
  (let ((default-directory (icicle-file-name-directory-w-default input)))
    (icicle-sort-and-strip-ignored
     (icicle-unsorted-file-name-apropos-candidates
      (or (icicle-file-name-nondirectory input) "")))))

(defun icicle-unsorted-file-name-apropos-candidates (input)
  "Unsorted list of apropos completions for the partial file-name INPUT.
When `icicle-expand-input-to-common-match-flag' is non-nil, this also
sets `icicle-common-match-string' to the longest common match of input
over all candidates."
  (when icicle-regexp-quote-flag (setq input (regexp-quote input)))
  (let ((slashed-p (and (> (length input) 0) (eq ?/ (aref input 0)))))
    (when slashed-p (setq input (substring input 1)))
    (let* ((candidates (icicle-transform-candidates
                        (all-completions "" minibuffer-completion-table
                                         (if slashed-p "/" default-directory)
                                         icicle-ignore-space-prefix-flag)))
           (filtered-candidates
            (append icicle-extra-candidates
                    (icicle-delete-if-not
                     (lambda (cand)
                       (let ((case-fold-search completion-ignore-case))
                         (if (member cand '("../" "./"))
                             (member input '(".." ".")) ; Prevent "" from matching "../"
                           (and (string-match input cand)
                                (icicle-filter-wo-input cand)))))
                     candidates))))
      (when (and icicle-expand-input-to-common-match-flag (consp filtered-candidates))
        (setq icicle-common-match-string (icicle-longest-common-match input filtered-candidates)))
      filtered-candidates)))            ; Return candidates.

(defun icicle-longest-common-match (input candidates)
  "Return the longest common match for INPUT among all CANDIDATES.
This assumes that INPUT matches each string in list CANDIDATES.
Return nil if there is no common match.  This actually returns
`regexp-quote' applied to the longest common match, so that special
characters in the match don't throw off regexp matching."
  (let ((case-fold-search completion-ignore-case)
        (first (car candidates)))
    (string-match input first)
    (let* ((len-first (length first))
           (beg 0)
           (end len-first)
           (orig-match-beg (match-beginning 0))
           (lcm first)                  ; "lcm" for "longest common match".
           (rest (cdr candidates)))
      (if (= orig-match-beg end)
          (setq lcm "")                 ; INPUT was, for instance, "$" or "\\>$; return "".
        ;; Compare with the rest of the candidates, reducing as needed.
        (while (and rest lcm)
          ;; Remove any prefix that doesn't match some other candidate.
          (while (and (< beg orig-match-beg)
                      (not (string-match
                            (regexp-quote (substring lcm 0 (1+ (- orig-match-beg beg))))
                            (car rest)))) ; Use 1+ so include first character of input.
            ;; Take a character off of the left.
            (setq lcm (substring lcm 1)
                  beg (1+ beg)))
          ;; Remove any suffix that doesn't match some other candidate.
          (while (and (> end 0) (not (string-match (regexp-quote lcm) (car rest))))
            ;; Take a character off of the right.
            (setq lcm (substring lcm 0 (1- (length lcm)))
                  end (1- end)))
          (unless (and (string-match (regexp-quote lcm) (car rest))
                       (string-match input lcm))
            (setq lcm nil))             ; No possible expansion
          (pop rest))
        lcm))))



 
;;; Icicles functions - common helper functions.............

;; Main cycling function - used by `icicle-next-prefix-candidate', `icicle-next-apropos-candidate'.
(defun icicle-next-candidate (nth candidates-fn &optional regexp-p)
  "Replace input by NTH next or previous completion for an input.
Default value of NTH is 1, meaning use the next completion.
Negative NTH means use a previous, not subsequent, completion.

CANDIDATES-FN is a function that returns the list of candidate
completions for its argument, the current partial input (a string).

Optional arg REGEXP-P non-nil means that CANDIDATES-FN uses regexp
matching. This is used to highlight the appropriate matching root."
  (let ((saved-last-input icicle-last-input)) ; For call to `icicle-recompute-candidates'.
    (unless (stringp icicle-last-completion-candidate)
      (setq icicle-last-completion-candidate icicle-initial-value))
    (setq nth (or nth 1))
    (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
    (unless (and (and (symbolp this-command) (get this-command 'icicle-apropos-cycling-command))
                 (or (and (symbolp last-command)
                          (get last-command 'icicle-apropos-cycling-command))
                     (memq last-command '(icicle-candidate-action icicle-apropos-complete
                                          icicle-apropos-complete-no-display))))
      (setq icicle-common-match-string nil)) ; Don't use old one in `icicle-save-or-restore-input'
    (icicle-save-or-restore-input)
    (when (and (icicle-file-name-input-p) (icicle-file-directory-p icicle-current-input))
      (setq icicle-default-directory icicle-current-input))
    (icicle-recompute-candidates nth candidates-fn saved-last-input)
    (icicle-save-or-restore-input) ; Again, based on updated `icicle-common-match-string'.
    (cond ((null icicle-completion-candidates)
           (save-selected-window (icicle-remove-Completions-window))
           (minibuffer-message "  [No completion]"))
          (t
           (icicle-clear-minibuffer)
           (let ((nb-cands (length icicle-completion-candidates))
                 (unit (if (wholenump nth) 1 -1))
                 next)
             ;; So `icomplete+' can append the number of other candidates to the minibuffer.
             (when icicle-completion-candidates
               (setq icicle-nb-of-other-cycle-candidates (1- nb-cands)))
             (icicle-increment-cand-nb+signal-end nth nb-cands)
             (setq next (elt icicle-completion-candidates icicle-candidate-nb))
             (while (null next)         ; Skip null candidates.
               (icicle-increment-cand-nb+signal-end unit nb-cands)
               (setq next (elt icicle-completion-candidates icicle-candidate-nb)))

             ;; Reset last candidate.  Need a copy, because we change its text properties.
             (setq icicle-last-completion-candidate (copy-sequence next))

             ;; Highlight any initial whitespace (probably a user typo).
             (let ((input (if regexp-p icicle-current-raw-input icicle-current-input)))
               (icicle-highlight-initial-whitespace input))

             ;; Underline the root that was completed, in the minibuffer.
             (let ((case-fold-search completion-ignore-case)
                   (inp (if (icicle-file-name-input-p)
                            (icicle-file-name-nondirectory icicle-current-input)
                          icicle-current-input))
                   indx)
               (unless regexp-p (setq inp (regexp-quote inp)))
               (setq indx (string-match inp icicle-last-completion-candidate))
               (when indx
                 (put-text-property indx (match-end 0) 'face 'icicle-match-highlight-minibuffer
                                    icicle-last-completion-candidate)))
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     icicle-last-completion-candidate)
             (icicle-place-cursor icicle-current-input)

             ;; Highlight current completion candidate, if *Completions* is displayed.
             (when (get-buffer-window "*Completions*" t)
               
               ;; Refresh *Completions*, updating it to reflect the current candidates.
               (unless (or (and (symbolp this-command)
                                (get this-command 'icicle-apropos-cycling-command)
                                (or (and (symbolp last-command)
                                         (get last-command 'icicle-apropos-cycling-command))
                                    (eq last-command 'icicle-candidate-action)))
                           (and (symbolp this-command)
                                (get this-command 'icicle-prefix-cycling-command)
                                (or (and (symbolp last-command)
                                         (get last-command 'icicle-prefix-cycling-command))
                                    (eq last-command 'icicle-candidate-action))))
                 (icicle-display-candidates-in-Completions))
               ;; Highlight current candidate in *Completions*.
               (let ((compl-win (get-buffer-window "*Completions*" t))
                     curr-candidate-pos)
                 (save-window-excursion
                   (select-window compl-win)
                   (let ((case-fold-search completion-ignore-case))
                     (goto-char (icicle-start-of-candidates-in-Completions))
                     (icicle-move-to-next-completion icicle-candidate-nb t)
                     (set-buffer-modified-p nil)
                     (setq curr-candidate-pos (point))))
                 (set-window-point compl-win curr-candidate-pos))))))))

(defun icicle-save-or-restore-input ()
  "Save the current minibuffer input, or restore the last input.
If this is a cycling command, and there is a previous input, and the
  current input differs from the last cycling candidate (so the user
  has edited it), then restore the last input.  Cycled completions
  don't count as input.
Otherwise, save the current input for use by `C-l', and then update
  the input to be the longest common match.

There are several particular cases that modulate the behavior - see
the code."
  (cond
    ;; Restore the last input, provided there is some to restore and this a cycling command.
    ((and icicle-last-input
          (symbolp this-command) (get this-command 'icicle-cycling-command)
          icicle-last-completion-candidate
          ;; Current input = last completion candidate?
          (string= (if (icicle-file-name-input-p)
                       (directory-file-name (icicle-remove-dots icicle-last-completion-candidate))
                     icicle-last-completion-candidate)
                   (if (icicle-file-name-input-p)
                       (if icicle-cycle-into-subdirs-flag
                           (icicle-file-name-nondirectory icicle-current-input)
                         (file-name-nondirectory
                          (directory-file-name (icicle-remove-dots icicle-current-input))))
                     icicle-current-input)))        
     (setq icicle-current-input icicle-last-input)) ; Return `icicle-current-input'.
    (t
     (cond
       ;; Save the current input for `C-l', then update it to the longest common match.
       ;; Don't do this if:
       ;;      the user doesn't want to use the longest common match
       ;;   or there is no common match string
       ;;   or the last command was a cycling command
       ;;   or the input has not changed (so saved regexp is not overwritten).
       ((not (or (not icicle-expand-input-to-common-match-flag)
                 (not icicle-common-match-string)
                 (and (symbolp last-command) (get last-command 'icicle-cycling-command))
                 (equal icicle-last-input icicle-current-input)))

        ;; Expand current input to longest common match, after saving it for `C-l'.
        (let ((common (if (and (icicle-file-name-input-p) insert-default-directory)
                          (if (string= "" icicle-common-match-string)
                              (file-name-directory icicle-current-input)
                            (directory-file-name
                             (expand-file-name icicle-common-match-string
                                               (file-name-directory icicle-current-input))))
                        icicle-common-match-string)))

          ;; Save current input for `C-l', then save common match as current input.
          ;; Don't do anything if we're ignoring letter case and that is the only difference
          ;; between the common match and the input (e.g. MS Windows file names).
          (unless (and case-fold-search (string= (upcase icicle-current-input)
                                                 (upcase common))
                       (not (string= icicle-current-input common)))
                  
            ;; Save input for `C-l' if this is not a cycling command.  Save it also if this is
            ;; the first cycling command, or the first one after completion.
            (unless (and (symbolp this-command)
                         (get this-command 'icicle-cycling-command)
                         (or icicle-candidate-nb ; Not the first cycling command.
                             (and (symbolp last-command)
                                  (get last-command 'icicle-completing-command))))
              (setq icicle-current-raw-input icicle-current-input)) ; Save it for `C-l'.
                  
            ;; Save longest common match as current input, unless input is a directory.
            (unless (and (icicle-file-name-input-p) (file-directory-p icicle-current-input))
              (setq icicle-current-input common)))))

       ;; Save input for `C-l'.
       ;; Don't do this if:
       ;;      either this command or last command was a cycling command
       ;;   or this command is the same as last command.
       ((not (or (and (symbolp last-command) (get last-command 'icicle-cycling-command))
                 (and (symbolp this-command) (get this-command 'icicle-cycling-command))
                 (and (symbolp last-command) (get last-command 'icicle-completing-command))
                 (memq last-command (list this-command 'handle-switch-frame))))
        (setq icicle-current-raw-input icicle-current-input))))) ; Save it for `C-l'.
  (setq icicle-last-input icicle-current-input)) ; Return `icicle-current-input'.

(defun icicle-remove-dots (filename)
  "Strip leading string through last ../ or ./ from FILENAME."
  (let ((newname filename))
    (while
        (or (string-match "\\.\\./" newname)
            (string-match "\\./" newname)
            ;; Emacs 21+ `file-relative-name' returns ".." and "." (no slash) for "" first arg
            (string-match "^\\.\\.$" newname)
            (string-match "^\\.$" newname))
      (setq newname (substring newname (match-end 0))))
    newname))

(defun icicle-recompute-candidates (nth candidates-fn saved-last-input)
  "Recompute `icicle-completion-candidates', if needed.
If buffer *Completions* is already displayed, it is updated.
This does nothing, unless the user changed the minibuffer input or the
completion type has changed (from apropos to prefix or vice versa).
Argument NTH is passed to `icicle-display-candidates-in-Completions'.
Argument CANDIDATES-FN is a function that recomputes the candidates.
SAVED-LAST-INPUT is the last input, as in `icicle-last-input'."
  (unless (and icicle-last-completion-command
               (string= icicle-current-input saved-last-input) ; No change in user input.
               ;; No change in completion type: apropos vs prefix.
               (or (and (memq icicle-last-completion-command
                              '(icicle-apropos-complete icicle-candidate-set-complement
                                icicle-keep-only-past-inputs))
                        (or (eq this-command 'icicle-apropos-complete)
                            (and (symbolp this-command)
                                 (get this-command 'icicle-apropos-cycling-command))))
                   (and (memq icicle-last-completion-command
                              '(icicle-prefix-complete icicle-candidate-set-complement
                                icicle-keep-only-past-inputs))
                        (or (eq this-command 'icicle-prefix-complete)
                            (and (symbolp this-command)
                                 (get this-command 'icicle-prefix-cycling-command))))))
    ;; Set `icicle-last-completion-command', to record new completion type.
    (cond ((and (symbolp this-command) (get this-command 'icicle-prefix-cycling-command))
           (setq icicle-last-completion-command 'icicle-prefix-complete))
          ((and (symbolp this-command) (get this-command 'icicle-apropos-cycling-command))
           (setq icicle-last-completion-command 'icicle-apropos-complete)))

    ;; Recompute and redisplay completion candidates.  Reset candidate number.
    (setq icicle-completion-candidates (funcall candidates-fn icicle-current-input))
    (when (get-buffer-window "*Completions*" 0) ; Update *Completions* display or remove it.
      (if icicle-completion-candidates
          (icicle-display-candidates-in-Completions (not (wholenump nth)))
        (save-selected-window (icicle-remove-Completions-window))))))

(defun icicle-increment-cand-nb+signal-end (incr max)
  "Increment candidate number by INCR modulo MAX, and signal end of cycle."
  (if icicle-candidate-nb
      (setq icicle-candidate-nb (+ incr icicle-candidate-nb))
    (setq icicle-candidate-nb 0))       ; Reset.
  (setq icicle-candidate-nb (mod icicle-candidate-nb max))
  (when (and (= 0 icicle-candidate-nb)  ; Signal end of cycle.
             (eq last-command this-command))
    (let ((visible-bell t)) (ding) (setq visible-bell nil) (ding))))

(defun icicle-place-overlay (start end overlay face buffer &rest properties)
  "Put OVERLAY with FACE between START and END in BUFFER.
OVERLAY is a symbol whose value is the overlay.  If nil, the overlay
  is created.  If non-nil, it is simply moved.
PROPERTIES are additional overlay properties to add: pairs of a
property and a value."
  (if (symbol-value overlay)            ; Overlay exists, just move it.
      (move-overlay (symbol-value overlay) start end buffer)
    (set overlay (make-overlay start end buffer))
    (overlay-put (symbol-value overlay) 'face face)))

(defun icicle-sort-and-strip-ignored (candidates)
  "Remove file names with ignored extensions, and \".\".  Sort CANDIDATES.
If `icicle-sort-function' is nil, then do not sort."
  (let* ((pred1 (lambda (cand) (or (string-match icicle-ignored-extensions-regexp cand)
                                   (string= "./" cand))))
         (pred2 (lambda (cand) (string= "./" cand)))
         (new-candidates (icicle-delete-if (if icicle-ignored-extensions-regexp pred1 pred2)
                                           candidates)))
    ;; If the only candidates have ignored extensions, then use them.
    (unless new-candidates (setq new-candidates (icicle-delete-if pred2 candidates)))
    (if icicle-sort-function
        (sort new-candidates icicle-sort-function)
      new-candidates)))

(defun icicle-transform-candidates (candidates)
  "Apply `icicle-transform-function' to CANDIDATES.
If `icicle-transform-function' is nil, return CANDIDATES."
  (if icicle-transform-function
      (funcall icicle-transform-function candidates)
    candidates))

(defun icicle-file-name-directory-w-default (file)
  "Like `file-name-directory', but return `default-directory', not nil.
Does not treat backslash as a directory separator, even on MS Windows."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (or (file-name-directory escaped-file) default-directory)))

(defun icicle-file-name-nondirectory (file)
  "Like `file-name-nondirectory', but does not treat backslash specially.
That is, backslash is never treated as a directory separator."
  (let ((escaped-file (subst-char-in-string ?\\ ?\a file)))
    (subst-char-in-string ?\a ?\\ (file-name-nondirectory escaped-file))))

(defun icicle-file-name-input-p ()
  "Return non-nil if expected input is a file name.
This is used, instead of variable `minibuffer-completing-file-name',
because we sometimes complete against an explicit alist of file names,
even in the overall context of file-name input.  In that case, we do
not want to use file-name completion.  An example of this is
completing against a history list of file names, using
`icicle-history'."
  ;;
  ;; Note that some Emacs 20 code uses this as the equivalent of `minibuffer-completing-file-name':
  ;; (memq minibuffer-completion-table '(read-file-name-internal read-directory-name-internal))
  ;;
  (and (symbolp minibuffer-completion-table) (stringp minibuffer-completion-predicate)))

(defun icicle-sort-dirs-last (name1 name2)
  "Non-nil if NAME1 is a file and NAME2 is a dir, or `string-lessp'.
This can be used as the value for `icicle-sort-function'.
It is especially useful when `icicle-cycle-into-subdirs-flag' is
non-nil.  Otherwise, cycling into subdirectories is depth-first, not
breadth-first."
  (if (icicle-file-name-input-p)
      (let ((name1-dir-p (icicle-file-directory-p name1))
            (name2-dir-p (icicle-file-directory-p name2)))
        (if (or (and name1-dir-p name2-dir-p) ; Both or neither are directories.
                (not (or name1-dir-p name2-dir-p)))
            (string-lessp name1 name2)  ; Compare equals.
          name2-dir-p))                 ; Files come before directories.
    (string-lessp name1 name2)))

(defun icicle-sort-case-insensitively (string1 string2)
  "Like `string-lessp', but case is ignored, so `A' = `a' , and so on."
  (string-lessp (upcase string1) (upcase string2)))

(defun icicle-file-directory-p (file)
  "Local, faster replacement for `file-directory-p'.
This does not do all of the file-handler processing that
`file-directory-p' does, so it is not a general replacement."
  (and (stringp file) (string= file (icicle-file-name-directory-w-default file))))

(defun icicle-minibuffer-contents ()
  "Return the user minibuffer input as a string, without text-properties."
  (save-selected-window (select-window (minibuffer-window))
                        (icicle-minibuffer-contents-from-minibuffer)))

(defun icicle-minibuffer-contents-from-minibuffer ()
  "Return the user minibuffer input as a string, without text-properties.
The current buffer must be a minibuffer."
  (let ((input (if (fboundp 'minibuffer-contents-no-properties)
                   (minibuffer-contents-no-properties) ; e.g. Emacs 22
                 (buffer-substring-no-properties (point-min) (point-max))))) ; e.g. Emacs 20
    (when (and (icicle-file-name-input-p)
               (not (string= "" input))) ; Do nothing if user deleted everything in minibuffer.
      (let ((last-char ""))
        (when (string= "$" (substring input (1- (length input)) (length input)))
          (setq last-char "$"
                input (substring input 0 (1- (length input)))))
        (setq input
              (save-match-data
                (concat (subst-char-in-string ?\a ?\\
                                              (condition-case nil
                                                  (substitute-in-file-name
                                                   (subst-char-in-string ?\\ ?\a input 'in-place))
                                                (error input))
                                              'in-place)
                        last-char)))))
    input))

(defun icicle-filter-wo-input (candidate)
  "Filter completion CANDIDATE using regexps and predicate.
This filtering is in addition to matching user input."
  (and (or (not icicle-must-match-regexp)
           (string-match icicle-must-match-regexp candidate))
       (or (not icicle-must-not-match-regexp)
           (not (string-match icicle-must-not-match-regexp candidate)))
       (or (not icicle-must-pass-predicate)
           (funcall icicle-must-pass-predicate candidate))))

(defun icicle-update-completions ()
  "Update completions list.  Update display too, if already shown."
  (setq icicle-completion-candidates
        (funcall (case icicle-last-completion-command
                   ((icicle-prefix-complete icicle-prefix-word-complete)
                    (if (icicle-file-name-input-p)
                        #'icicle-file-name-prefix-candidates
                      #'icicle-prefix-candidates))
                   (t
                    (if (icicle-file-name-input-p)
                        #'icicle-file-name-apropos-candidates
                      #'icicle-apropos-candidates)))
                 icicle-current-input))
  (when (get-buffer-window "*Completions*" 0)
    (icicle-display-candidates-in-Completions)))

(defun icicle-msg-maybe-in-minibuffer (format-string &rest args)
  "Display FORMAT-STRING as a message.
If called with the minibuffer inactive, this is done using `message'.
Otherwise, it is done using `minibuffer-message'."
  (if (active-minibuffer-window)
      (minibuffer-message (apply #'format (concat "  [" format-string "]") args))
    (apply #'message format-string args)))

(defun icicle-delete-if (pred inlist)
  "A copy of list INLIST with no elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (unless (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-delete-if-not (pred inlist)
  "A copy of list INLIST with only elements that satisfy predicate PRED."
  (let ((outlist nil))
    (dolist (o inlist) (when (funcall pred o) (push o outlist)))
    (nreverse outlist)))

(defun icicle-frames-on (buffer &optional frame) ; From `frames-on' in `frame-fns.el'.
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

(defun icicle-candidate-set-1 (set-fn msg)
  "Helper function for defining Icicle set commands.
SET-FN is the function to apply to the current and saved candidates.
MESSAGE is the confirmation message to display in the minibuffer."
  (setq icicle-completion-candidates
        (funcall set-fn icicle-completion-candidates icicle-saved-completion-candidates))
  (if (null icicle-completion-candidates)
      (minibuffer-message "  [EMPTY SET]")
    (icicle-maybe-sort-and-strip-candidates)
    (icicle-scroll-or-update-Completions msg)))

(defun icicle-maybe-sort-and-strip-candidates ()
  "Sort `icicle-completion-candidates'.  Strip ignored file names too."
  (if (icicle-file-name-input-p)
      (setq icicle-completion-candidates
            (icicle-sort-and-strip-ignored icicle-completion-candidates))
    (if icicle-sort-function
        (setq icicle-completion-candidates
              (sort icicle-completion-candidates icicle-sort-function)))))

(defun icicle-scroll-or-update-Completions (msg)
  "Scroll *Completions* if this command was repeated; else update it."
  (if (get-buffer-window "*Completions*" 0)
      (if (eq last-command this-command)
          ;; User repeated the command.  Scroll window around.
          (icicle-scroll-Completions)
        ;; User did something else (e.g. changed input).  Update the display.
        (icicle-display-candidates-in-Completions)
        (minibuffer-message msg))
    ;; No window yet.  Show window.
    (icicle-display-candidates-in-Completions)
    (minibuffer-message msg)))

(defun icicle-display-Completions ()
  "Display *Completions* buffer."
  (let ((completions (all-completions "" minibuffer-completion-table
                                      minibuffer-completion-predicate
                                      icicle-ignore-space-prefix-flag)))
    (when (> (length icicle-completion-candidates) icicle-incremental-completion-threshold)
      (message "Displaying completion candidates..."))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (if icicle-sort-function (sort completions icicle-sort-function) completions)))))

;; From `cl-seq.el', function `union', without keyword treatment.
;; Same as `simple-set-union' in `misc-fns.el'.
(defun icicle-set-union (list1 list2)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) list1)
        (t
         (or (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
         (while list2
           (unless (member (car list2) list1)
               (setq list1 (cons (car list2) list1)))
           (setq list2 (cdr list2)))
         list1)))

;; From `cl-seq.el', function `intersection', without keyword treatment.
;; Same as `simple-set-intersection' in `misc-fns.el'.
(defun icicle-set-intersection (list1 list2)
  "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
  (and list1 list2
       (if (equal list1 list2)
           list1
         (let ((result nil))
           (unless (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1)))) ; Swap them.
           (while list2
             (when (member (car list2) list1)
               (setq result (cons (car list2) result)))
             (setq list2 (cdr list2)))
           result))))

;; From `cl-seq.el', function `set-difference', without keyword treatment.
;; Same as `simple-set-difference' in `misc-fns.el'.
(defun icicle-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is non-destructive; it makes a copy of the data if necessary, to
avoid corrupting the original LIST1 and LIST2."
  (if (or (null list1) (null list2)) list1    (let ((result nil))
      (while list1
        (unless (member (car list1) list2) (setq result (cons (car list1) result)))
        (setq list1 (cdr list1)))
      result)))

;; Note that initial and trailing spaces will not be noticeable.  That's OK.
(defun icicle-highlight-complete-input ()
  "Highlight minibuffer input, showing that it is a sole completion.
Overlay `icicle-complete-input-overlay' is created with `match' face,
unless it exists."
  (let ((case-fold-search completion-ignore-case)
        input-start-position)
    (save-excursion
      (goto-char (icicle-minibuffer-prompt-end))
      (setq input-start-position (point))
      (when (and (icicle-file-name-input-p) insert-default-directory)
        (search-forward (icicle-file-name-directory-w-default
                         (icicle-minibuffer-contents-from-minibuffer)))
        (setq input-start-position (point))) ; Skip directory.
      (if icicle-complete-input-overlay ; Don't recreate if exists.
          (move-overlay icicle-complete-input-overlay
                        input-start-position (point-max) (current-buffer))
        (setq icicle-complete-input-overlay (make-overlay input-start-position (point-max)))
        (overlay-put icicle-complete-input-overlay 'face 'icicle-complete-input)))))

(defun icicle-call-then-update-Completions (fn &rest args)
  "Call FN with ARGS, then update *Completions* with input matches."
  (save-match-data
    (apply fn args)
    (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
    (icicle-highlight-initial-whitespace icicle-current-input)
    (if (< (length icicle-current-input) icicle-Completions-display-min-input-chars)
        (save-selected-window (icicle-remove-Completions-window))
      (when (and icicle-incremental-completion-p
                 (or (get-buffer-window "*Completions*" 0) ; Already displayed.
                     (not (eq t icicle-incremental-completion-p))) ; Display anyway.
                 (or (and icicle-completion-candidates
                          (> icicle-incremental-completion-threshold
                             (length icicle-completion-candidates)))
                     (sit-for icicle-incremental-completion-delay))) ; Wait if many candidates.
        (let ((icicle-icompleting-p t))
          (funcall (or icicle-last-completion-command 'icicle-apropos-complete))
          (run-hooks 'icicle-update-input-hook))))
    (setq mark-active nil)))

(defun icicle-clear-minibuffer ()
  "Delete all user input in the minibuffer."
  (if (fboundp 'delete-minibuffer-contents) (delete-minibuffer-contents) (erase-buffer)))

;; Borrowed from `ps-print.el'
(defun icicle-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))

(defun icicle-file-readable-p (file)
  "Return non-nil if FILE (a string) names a readable file."
  (and (not (string= "" file)) (file-readable-p file) (not (file-directory-p file))))

(defun icicle-file-writable-p (file)
  "Return non-nil if FILE (a string) names a writable file."
  (and (not (string= "" file)) (file-writable-p file) (not (file-directory-p file))))

(defun icicle-files-within (file-list accum)
  "List of all files in FILE-LIST.
Directories in FILE-LIST are processed recursively to include their
files and the files in their subdirectories.  The list of files is
accumulated in ACCUM, which is used for recursive calls."
  (let ((res accum))
    (while file-list
      (if (file-directory-p (car file-list))
          (setq res (icicle-files-within (directory-files (car file-list) 'full icicle-re-no-dot)
                                         res))
        (setq res (cons (car file-list) res)))
      (pop file-list))
    res))

(defun icicle-delete-whitespace-from-string (string)
  "Remove whitespace from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (char)
      (while (not (eobp))
        (setq char (char-after))
        (if (memq char '(?\  ?\t ?\n)) (delete-char 1) (forward-char 1)))
      (buffer-string))))

(defun icicle-barf-if-outside-minibuffer ()
  "Raise an error if `this-command' is called outside the minibuffer."
  (unless (eq (current-buffer) (window-buffer (minibuffer-window)))
    (error "Command `%s' must be called from the minibuffer" this-command)))

(defun icicle-barf-if-outside-Completions ()
  "Raise an error if `this-command' is called outside buffer *Completions*."
  (unless (eq (current-buffer) (get-buffer "*Completions*"))
    (error "Command `%s' must be called from *Completions* buffer" this-command)))

(defun icicle-barf-if-outside-Completions-and-minibuffer ()
  "Error if `this-command' called outside *Completions* and minibuffer."
  (unless (or (eq (current-buffer) (window-buffer (minibuffer-window)))
              (eq (current-buffer) (get-buffer "*Completions*")))
    (error "Command `%s' must be called from *Completions* buffer" this-command)))

(defun icicle-control-reminder-prompt ()
  "If `icicle-reminder-prompt-flag' > 0, then decrement it and save it.
Used in `kill-emacs-hook'."
  (when (and (wholenump icicle-reminder-prompt-flag) (> icicle-reminder-prompt-flag 0))
    (condition-case nil                 ; Don't raise an error, since it's on `kill-emacs-hook.
        (customize-save-variable 'icicle-reminder-prompt-flag (1- icicle-reminder-prompt-flag))
      (error nil))))

(defun icicle-expand-file-name (input dir)
  "Expand file-name INPUT in directory DIR.
Similar to `expand-file-name', except:

 - If INPUT does not end in a slash, and DIR/INPUT is a directory,
   a trailing slash is added.

 - If INPUT ends in a slash, but DIR/INPUT is not a directory, then
   the trailing slash is removed."
  (let ((expanded-input (directory-file-name (expand-file-name input dir))))
    ;; Add trailing slash if input is a directory.
    (when (file-directory-p expanded-input)
      (setq expanded-input (file-name-as-directory expanded-input)))
    expanded-input))

(defun icicle-start-of-candidates-in-Completions ()
  "Return buffer position of the first candidate in *Completions*."
  (if icicle-show-Completions-help-flag
      (save-excursion (goto-char (point-min)) (forward-line 3) (point))
    (point-min)))

(defun icicle-key-description (keys &optional no-angles)
  "`key-description', but non-nil NO-ANGLES means use no angle brackets."
  (let ((result (key-description keys)))
    (when no-angles              ; Assume space separates angled keys.
      (setq result (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result 'fixed-case)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-fn.el ends here
