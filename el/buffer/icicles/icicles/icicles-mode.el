;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Fri Jan 12 11:28:06 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 1724
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mode.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `cl', `color-theme', `cus-face',
;;   `easymenu', `ffap', `ffap-', `hexrgb', `icicles-fn',
;;   `icicles-opt', `icicles-var', `thingatpt', `thingatpt+',
;;   `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  See `icicles.el' for documentation.
;;
;;  Commands defined here:
;;
;;    `icicle-mode', `icy-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-bind-completion-keys',
;;    `icicle-bind-isearch-keys', `icicle-cancel-Help-redirection',
;;    `icicle-completing-p', `icicle-define-icicle-maps',
;;    `icicle-minibuffer-setup', `icicle-rebind-completion-maps',
;;    `icicle-rebind-global', `icicle-rebind-non-completion-keys',
;;    `icicle-restore-non-completion-keys',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remap',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-mode-map'.
;;
;;
;;  ***** NOTE: The following function defined in `simple.el' has
;;              been REDEFINED HERE:
;;
;;  `next-history-element' (advised only) -
;;     Depending on `icicle-init-value-flag', select minibuffer
;;     contents.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2007/01/12 dadams
;;     Removed: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map.
;;              Not used in minibuffer hooks.
;;     Removed [pause] bindings from minibuffer maps.
;;     Removed remap of yank in minibuffer maps.
;;     No longer bind icicle-remove-Completions-window in minibuffer maps.
;; 2007/01/11 dadams
;;     Renamed: icicle-define-icicle-mode-map to icicle-define-icicle-maps.
;;     icicle-define-icicle-maps: Use icicle-menu-map.  Don't recreate it.
;;     Bound [pause] to icicle-switch-to/from-minibuffer in all minibuffer maps.
;; 2007/01/10 dadams
;;     Added: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map,
;;            icicle-(rebind|restore)-non-completion-keys. 
;;     Added: icicle-rebind-global: This used to be called icicle-remap.
;;     icicle-(remap|unmap): Different purpose and use now.  Redefined to use remapping when
;;        available (as was done before for self-insert-command).
;;     icicle-mode:
;;       Add, remove as minibuffer setup and exit hooks: icicle-override-maps-w-minibuffer-map,
;;                                                       icicle-restore-overriding-local-map.
;;       Call icicle-(rebind|restore)-non-completion-keys.
;;     icicle-define-icicle-mode-map:
;;       Use icicle-remap where previously used substitute-key-definition for top-level commands.
;;       Moved to icicle-(rebind|restore)-non-completion-keys:
;;         binding of Info commands in Info map and S-tab in all keymaps (to *-rebind-* only).
;;     icicle-(bind|restore)-completion-keys: Use new icicle-(remap|unmap) where possible.
;;       Use icicle-rebind-global and substitute-key-definition for keys defined in vanilla 
;;         completion maps.
;; 2007/01/06 dadams
;;     icicle-mode: Update doc and bind icicle-toggle-~-for-home-dir to M-~.
;; 2007/01/01 dadams
;;     Moved assq-delete-all to icicles-fn.el.
;;     Require at runtime, not compile-time: icicles-var.el, icicles-fn.el.
;; 2006-12-31 dadams
;;     icicle-define-icicle-mode-map: Delete icicle-mode entry from minor-mode-map-alist.
;;     icicle-mode: Unbind icicle-mode-map when the mode is turned off.
;;     Added assq-delete-all for Emacs 20.
;;     Use current-global-map function, not global-map variable.
;; 2006/12/25 dadams
;;     Bound icicle-candidate-set-truncate to M-$.
;; 2006/12/24 dadams
;;     icicle-bind-completion-keys: transpose-yank(-pop) -> yank(-pop): typo.
;;     Bound mouse-3 to icicle-Completions-mouse-3-menu in completion-list-mode-map.
;; 2006/12/22 dadams
;;     Bound icicle-exchange-point-and-mark.
;;     :group 'icicles -> :group 'Icicles-Miscellaneous.
;; 2006/12/17 dadams
;;     Bound icicle(-mouse)-candidate-read-fn-invoke.
;; 2006/12/16 dadams
;;     icicle-define-icicle-mode-map: Protect icicle-kmacro with fboundp.
;; 2006/12/12 dadams
;;     Added icicle-customize-icicles-group, icicle-kill-buffer, icicle-delete-windows to I. menu.
;;     Added + to multi-command menu items.
;; 2006/12/11 dadams
;;     Added icicle-customize-apropos* and icicle-Info-* to menu-bar menus.
;; 2006/12/10 dadams
;;     Updated user options list in icicle-completion-help-string.
;;     Updated list of icicle-opt stuff used here.
;; 2006/12/06
;;     icicle-select-minibuffer-contents:
;;       Use icicle-minibuffer-prompt-end, not point-min.  Thx to Erik Postma.
;; 2006/11/26 dadams
;;     Added icicle-regions stuff.
;; 2006/11/24 dadams
;;     icicle-redefine-standard-options: Treat icicle-kmacro-ring-max.
;;     Bind icicle-kmacro to f5
;;     Replaced icicle-select-window-or-frame by icicle-other-window-or-frame.
;;     Removed binding of icicle-select-frame.
;;     Do not require mb-depth+.el for Emacs 21 (do it only for Emacs 22).
;; 2006/11/23 dadams
;;     Bound icicle-execute-named-keyboard-macro to C-x M-e.
;; 2006/11/18 dadams
;;     Soft require mb-depth+.el instead of minibuf-depth.el.
;; 2006/11/17 dadams
;;     Bind icicle-select-window-or-frame to whatever other-window(-or-frame) is bound to.
;;     Bind icicle-select-frame to whatever other-frame is bound to.
;; 2006/11/09 dadams
;;     Bind icicle-dispatch-C-^, not icicle-toggle-ignored-space-prefix, to C-^.
;;     icicle-rebind-completion-maps: Updated doc string for icicle-dispatch-C-^.
;; 2006/11/05 dadams
;;     Bound icicle-occur to C-c '.  Added it to menu-bar menus.
;; 2006/10/18 dadams
;;     icy-mode: Invoke icicle-define-icicle-mode-map unconditionally, not just first time.
;; 2006/10/16 dadams
;;     icicle-define-icicle-mode-map: Try to avoid binding S-TAB to menu maps.
;; 2006/10/15 dadams
;;     icicle-define-icicle-mode-map: Simplified and corrected binding of S-TAB for key completion.
;;                                    Use a separate map for the menu bar.
;;     Moved here from icicles-fn.el:
;;       icicle-bind-isearch-keys, icicle-rebind-completion-maps,
;;       icicle-(redefine|restore)-standard-(commands|options),
;;       icicle-(redefine|restore)-std-completion-fns, icicle-(re|un)map,
;;       icicle-(bind|restore)-completion-keys, icicle-minibuffer-setup,
;;       icicle-cancel-*Help*-redirection, icicle-activate-mark,
;;       icicle-run-icicle-(pre|post)-command-hook, icicle-set-calling-cmd,
;;       icicle-undo-std-completion-faces icicle-update-ignored-extensions-regexp,
;;       icicle-completing-p, icicle-restore-region-face.
;;     Renamed: icicle-cancel-*Help*-redirection to icicle-cancel-Help-redirection.
;;     Moved here from icicles-cmd.el: icicle-select-minibuffer-contents, next-history-element.
;;     Moved to icicles-cmd.el: icicle-generic-S-tab.
;;     Require icicles-opt.el.
;;     Added eval-when-compile's and defvars to quite byte compiler.
;; 2006/09/23 dadams
;;     icicle-define-icicle-mode-map: Corrected binding of icicle-yank-insert.
;; 2006/09/22 dadams
;;     icicle-minibuffer-setup: Set this-command and last-command, for scrolling *Completions*.
;; 2006/09/18 dadams
;;     icicle-mode: Picked up all global prefixes for S-TAB.
;; 2006/09/17 dadams
;;     Added: icicle-generic-S-tab.  Bound to S-TAB.
;;     icicle-mode:
;;       Bound icicle-complete-keys to prefix keys followed by S-TAB.
;;       Added run-hooks for Emacs 22 version.
;; 2006/09/12 dadams
;;     Bound icicle-switch-to/from-minibuffer to [pause].
;; 2006/08/27 dadams
;;     Bound icicle-abort-minibuffer-input to what abort-recursive-edit is normally bound to.
;;       And add it to Icicle menu.
;; 2006/08/23 dadams
;;     Bound icicle-delete-window to what delete-window and delete-windows-for are normally
;;       bound to.
;;     Put use of Info-mode-map inside an eval-after-load.
;; 2006/08/18 dadams
;;     Added icicle-Info-goto-node-cmd to icicle-mode doc string.
;;       Substitute it for Info-goto-node binding.
;; 2006/08/13 dadams
;;     Added icicle-Info-index-cmd to icicle-mode doc string.
;;       Substitute it for Info-index binding.
;; 2006/08/04 dadams
;;     Added icicle-plist to menus.
;;     icicle-doc treats faces too now.
;; 2006/08/03 dadams
;;     Bound icicle-insert-yank to what yank is normally bound to.
;;     icicle-mode: Updated doc string.
;; 2006/07/29 dadams
;;     icy-mode, icicle-define-icicle-mode-map: Added missing toggle commands.
;; 2006/07/22 dadams
;;     Changed binding of C-c C-s for icicle-search to C-c ` for icicle-search-generic.
;;     Removed: add-hooks for icicle-compilation-search - see icicles-cmd.el.
;; 2006/06/08 dadams
;;     Converted global bindings in icicles-keys.el to icicle-mode-map bindings here.
;;     Added f10 binding for icicle-execute-menu-command.
;; 2006/05/19 dadams
;;     icicle-mode: (add-hook 'kill-emacs-hook 'icicle-control-reminder-prompt).
;; 2006/05/18 dadams
;;     Change :init-value to nil, per new Emacs convention.
;; 2006/05/13 dadams
;;     icicle-mode: Updated doc string.
;; 2006/05/10 dadams
;;     icicle-define-icicle-mode-map: Added menu item Send Bug Report.
;; 2006/04/03 dadams
;;     icicle-define-icicle-mode-map: Added icicle-toggle-(regexp-quote|incremental-completion).
;; 2006/03/16 dadams
;;     icicle-mode: Turn on minibuffer-indicate-depth-mode (Emacs 22 only).
;;     Added soft require of minibuf-depth.el for Emacs 22.
;; 2006/03/14 dadams
;;     Do not use icicle-reset-icicle-completing-p as minibuffer-exit-hook.
;; 2006/03/07 dadams
;;     Corrected menu items for icicle-doc (no name regexp input, just doc regexp).
;; 2006/03/05 dadams
;;     Moved here from icicle-opt.el: icicle-mode, icicle-mode-hook.
;;     Moved here from icicle-fn.el: icicle-mode-map.
;;     Added: icicle-define-icicle-mode-map.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'icicles-opt)
  ;; icicle-bind-top-level-commands-flag, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-change-region-background-flag, icicle-cycling-respects-completion-mode-flag,
  ;; icicle-incremental-completion-flag, icicle-init-value-flag (next-history-element defadvice),
  ;; icicle-kmacro-ring-max, icicle-minibuffer-setup-hook, icicle-modal-cycle-down-key,
  ;; icicle-modal-cycle-up-key, icicle-redefine-standard-commands-flag,
  ;; icicle-regexp-search-ring-max, icicle-region-background, icicle-search-ring-max,
  ;; icicle-show-Completions-initially-flag, icicle-touche-pas-aux-menus-flag,
  ;; icicle-word-completion-key
(require 'icicles-fn) ;; assq-delete-all
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-cmd-calling-for-completion,
  ;; icicle-completion-candidates, icicle-completion-help-string, icicle-current-completion-mode,
  ;; icicle-default-directory, icicle-ignored-extensions, icicle-ignored-extensions-regexp,
  ;; icicle-incremental-completion-p, icicle-initial-value, icicle-last-completion-candidate,
  ;; icicle-last-completion-command, icicle-last-input, icicle-pre-minibuffer-buffer,
  ;; icicle-prompt, icicle-prompt-suffix, icicle-saved-completion-candidates,
  ;; icicle-saved-kmacro-ring-max, icicle-saved-regexp-search-ring-max,
  ;; icicle-saved-region-background, icicle-saved-search-ring-max, icicle-search-current-overlay,
  ;; icicle-search-overlays, icicle-search-refined-overlays 

;; This requirement is real, but leads to recursion.
;; You should, in any case, just load everything by loading `icicles.el'.
;; (require 'icicles-cmd) ;; icicle-add-buffer-candidate, icicle-add-buffer-config, 

(when (>= emacs-major-version 22) (require 'mb-depth+ nil t)) ; Emacs 22

(eval-when-compile
 (when (< emacs-major-version 21) (require 'cl))) ;; push, dolist
                                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (require 'menu-bar+ nil t)) ;; (no error if not found): menu-bar-frames-menu

;; `icicle-apropos-complete' is used here.  It is defined in `icicles-cmd.el'.
;; `icicle-file-name-input-p' is used here.  It is defined in `icicles-fn.el'.

;;; Defvars to quiet byte-compiler:
(defvar minibuffer-local-filename-completion-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; User Options (alphabetical)

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Icicles-Miscellaneous :require 'icicles))

;;;###autoload
(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)


;;; Internal variables (alphabetical) ----------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-rebind-completion-maps' for minibuffer bindings.")
 


;;; Icicle mode command ------------------------------------

(defalias 'icy-mode 'icicle-mode)

;; Main command.  Inspired from `icomplete-mode'.
;;;###autoload
(if (fboundp 'define-minor-mode)
    ;; Emacs 21+ ------------
    (eval '(define-minor-mode icicle-mode
            "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-regions'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-clear-option'                  - Set binary option(s) to nil
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-mode'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'                 - Remove from `icicle-regions'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-region'                 - Search multiple regions
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-vardoc'                        - Show variable description(s)
`icicle-yank-insert'                   - `yank' using completion

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
            :global t :group 'Icicles-Miscellaneous :lighter " Icy" :init-value nil
            (cond (icicle-mode
                   (icicle-define-icicle-maps)
                   (icicle-rebind-non-completion-keys)
                   (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
                   ;; The pre- and post-command hooks are local to the minibuffer,
                   ;; So they are added in `icicle-minibuffer-setup', not here.
                   ;; Nevertheless, they are removed here when Icicle mode is exited.
                   (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
                   (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   (icicle-undo-std-completion-faces)
                   (icicle-redefine-std-completion-fns)
                   (icicle-redefine-standard-commands)
                   (icicle-redefine-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode 99)))
                  (t
                   (makunbound 'icicle-mode-map)
                   (icicle-restore-non-completion-keys)
                   (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                   (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil)
                   (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil)
                   (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                   (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   ;; $$ Should restore standard completion faces here.
                   (icicle-restore-std-completion-fns)
                   (icicle-restore-standard-commands)
                   (icicle-restore-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode -99))))
            (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
            (icicle-rebind-completion-maps icicle-mode)
            (run-hooks 'icicle-mode-hook)
            (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF"))))

  ;; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-regions'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-clear-option'                  - Set binary option(s) to nil
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-mode'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'                 - Remove from `icicle-regions'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-region'                 - Search multiple regions
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-vardoc'                        - Show variable description(s)
`icicle-yank-insert'                   - `yank' using completion

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
    (interactive "P")
    (setq icicle-mode (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-rebind-completion-maps icicle-mode)
    (cond (icicle-mode
           (icicle-define-icicle-maps)
           (icicle-rebind-non-completion-keys)
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
           (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-commands)
           (icicle-redefine-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (makunbound 'icicle-mode-map)
           (icicle-restore-non-completion-keys)
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil)
           (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-commands)
           (icicle-restore-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF"))))
  (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define Icicles menu-bar menu.  Create it only once.  Sacrifice latest bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map (make-sparse-keymap "Icicles"))
    (define-key icicle-menu-map [icicle-mode] '("Turn Off Icicle Mode" . icicle-mode))
    (define-key icicle-menu-map [icicle-abort]
      '("Cancel Minibuffer" . icicle-abort-minibuffer-input))
    (put 'icicle-abort 'menu-enable '(and icicle-mode (active-minibuffer-window)))
    (define-key icicle-menu-map [icicle-report-bug] '("Send Bug Report" . icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '("Customize Icicles" . icicle-customize-icicles-group))
    (define-key icicle-menu-map [icicle-help] '("Help" . icicle-completion-help))
    (define-key icicle-menu-map [icicle-separator-last] '("--"))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Defined in `menu-bar+.el'.
           (define-key menu-bar-frames-menu [icicle-separator-frame] '("--"))
           (define-key menu-bar-frames-menu [icicle-font] '("[Icy] + Change Font" . icicle-font))
           (define-key menu-bar-frames-menu [icicle-frame-fg]
             '("[Icy] + Change Foreground..." . icicle-frame-fg))
           (define-key menu-bar-frames-menu [icicle-frame-bg]
             '("[Icy] + Change Background..." . icicle-frame-bg)))
          (t
           (define-key icicle-menu-map [icicle-font] '("+ Change Font of Frame..." . icicle-font))
           (define-key icicle-menu-map [icicle-frame-fg]
             '("+ Change Foreground of Frame..." . icicle-frame-fg))
           (define-key icicle-menu-map [icicle-frame-bg]
             '("+ Change Background of Frame..." . icicle-frame-bg))
           (define-key icicle-menu-map [icicle-separator-frame] '("--"))))
    (put 'icicle-font 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-frame-bg 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-frame-fg 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-describe-menu)) ; Use Describe menu, if available.
           (define-key menu-bar-describe-menu [icicle-separator-doc] '("--"))
           (define-key menu-bar-describe-menu [icicle-plist]
             '("[Icy] + Symbol with Property List..." . icicle-plist))
           (define-key menu-bar-describe-menu [icicle-doc]
             '("[Icy] + Doc of Fun, Var, or Face..." . icicle-doc))
           (define-key menu-bar-describe-menu [icicle-fundoc]
             '("[Icy] + Function with Name, Doc..." . icicle-fundoc))
           (define-key menu-bar-describe-menu [icicle-vardoc]
             '("[Icy] + Variable with Name, Doc..." . icicle-vardoc)))
          (t
           (define-key icicle-menu-map [icicle-plist]
             '("+ Symbol with Property List..." . icicle-plist))
           (define-key icicle-menu-map [icicle-doc]
             '("+ Doc of Fun, Var, or Face..." . icicle-doc))
           (define-key icicle-menu-map [icicle-fundoc]
             '("+ Describe Function with Name, Doc..." . icicle-fundoc))
           (define-key icicle-menu-map [icicle-vardoc]
             '("+ Describe Variable with Name, Doc..." . icicle-vardoc))
           (define-key icicle-menu-map [icicle-separator-doc] '("--"))))

    (define-key icicle-menu-map [icicle-map] '("+ Apply Function to Alist Items..." . icicle-map))
    (define-key icicle-menu-map [icicle-save-string-to-variable]
      '("Save String to Variable..." . icicle-save-string-to-variable))
    (define-key icicle-menu-map [icicle-color-theme]
      '("+ Choose Color Theme..." . icicle-color-theme))
    (put 'icicle-color-theme 'menu-enable
         '(and icicle-mode (not (window-minibuffer-p (frame-selected-window
                                                      menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-remove-saved-completion-set]
      '("+ Remove a Saved Completion Set..." . icicle-remove-saved-completion-set))
    (put 'icicle-remove-saved-completion-set 'menu-enable
         '(and icicle-mode icicle-saved-completion-sets))
    (define-key icicle-menu-map [icicle-add/update-saved-completion-set]
      '("Add/Update a Saved Completion Set..." . icicle-add/update-saved-completion-set))
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-menu-map [icicle-kmacro]
        '("+ Execute Nth Keyboard Macro..." . icicle-kmacro))
      (put 'icicle-kmacro 'menu-enable'(and icicle-mode (or (kmacro-ring-head) kmacro-ring))))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '("+ Execute Named Keyboard Macro..." . icicle-execute-named-keyboard-macro))
    (define-key icicle-menu-map [icicle-imenu] '("+ Imenu..." . icicle-imenu))
    (define-key icicle-menu-map [icicle-goto-global-marker]
      '("+ Go To Global Mark..." . icicle-goto-global-marker))
    (define-key icicle-menu-map [icicle-goto-marker] '("+ Go To Mark..." . icicle-goto-marker))
    (put 'icicle-goto-global-marker 'menu-enable
         '(and icicle-mode (consp (icicle-markers global-mark-ring))))
    (put 'icicle-goto-marker 'menu-enable '(and icicle-mode (consp (icicle-markers mark-ring))))
    (define-key icicle-menu-map [icicle-separator-misc] '("--"))

    (define-key icicle-menu-map [icicle-remove-region]
      '("+ Remove a Region from List..." . icicle-remove-region))
    (define-key icicle-menu-map [icicle-add-region]
      '("Add Current Region to List" . icicle-add-region))
    (define-key icicle-menu-map [icicle-search-region]
      '("+ Search a Region..." . icicle-search-region))
    (define-key icicle-menu-map [icicle-select-region]
      '("+ Choose a Region..." . icicle-select-region))
    (define-key icicle-menu-map [icicle-separator-region] '("--"))
    (put 'icicle-remove-region 'menu-enable '(and icicle-mode icicle-regions))
    (put 'icicle-add-region 'menu-enable '(and icicle-mode mark-active))
    (put 'icicle-search-region 'menu-enable '(and icicle-mode icicle-regions))
    (put 'icicle-select-region 'menu-enable '(and icicle-mode icicle-regions))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-apropos-menu)) ; Use Apropos menu, if available.
           (define-key menu-bar-apropos-menu [icicle-separator-apropos] '("--"))
           (define-key menu-bar-apropos-menu [icicle-apropos-zippy]
             '("[Icy] Zippy..." . icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key menu-bar-apropos-menu [icicle-apropos]
                    '("[Icy] Symbols..." . icicle-apropos))
                  (define-key menu-bar-apropos-menu [icicle-apropos-function]
                    '("[Icy] Functions..." . icicle-apropos-function))
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '("[Icy] Variables..." . icicle-apropos-variable))
                  (define-key menu-bar-apropos-menu [icicle-apropos-option]
                    '("[Icy] Options..." . icicle-apropos-option))
                  (define-key menu-bar-apropos-menu [icicle-apropos-command]
                    '("[Icy] Commands..." . icicle-apropos-command)))
                 (t
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '("[Icy] Variables..." . icicle-apropos-variable))))
           (define-key menu-bar-apropos-menu [icicle-apropos-command]
             '("[Icy] Commands..." . icicle-apropos-command)))
          (t
           (define-key icicle-menu-map [icicle-apropos-zippy]
             '("Apropos Zippy..." . icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-menu-map [icicle-apropos]
                    '("Apropos..." . icicle-apropos))
                  (define-key icicle-menu-map [icicle-apropos-function]
                    '("Apropos Functions..." . icicle-apropos-function))
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '("Apropos Variables..." . icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-option]
                    '("Apropos Options..." . icicle-apropos-option))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '("Apropos Commands..." . icicle-apropos-command)))
                 (t
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '("Apropos Variables..." . icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '("Apropos Commands..." . icicle-apropos-command))))
           (define-key icicle-menu-map [icicle-separator-apropos] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-options-menu)) ; Use Options menu, if available.
           (define-key menu-bar-options-menu [icicle-separator-toggle] '("--"))
           (define-key menu-bar-options-menu [icicle-set-option-to-t]
             '("[Icy] + Turn On Any Option..." . icicle-set-option-to-t))
           (define-key menu-bar-options-menu [icicle-reset-option-to-nil]
             '("[Icy] + Turn Off Any Option..." . icicle-reset-option-to-nil))
           (define-key menu-bar-options-menu [icicle-toggle-option]
             '("[Icy] + Toggle Any Option..." . icicle-toggle-option))
           (define-key menu-bar-options-menu [icicle-toggle-transforming]
             '("[Icy] Toggle Duplicate Removal" . icicle-toggle-transforming))
           (define-key menu-bar-options-menu [icicle-toggle-sorting]
             '("[Icy] Toggle Completion Sorting" . icicle-toggle-sorting))
           (define-key menu-bar-options-menu [icicle-toggle-search-cleanup]
             '("[Icy] Toggle Removal of Search Highlighting" . icicle-toggle-search-cleanup))
           (define-key menu-bar-options-menu [icicle-toggle-regexp-quote]
             '("[Icy] Toggle Escaping Special Chars" . icicle-toggle-regexp-quote))
           (define-key menu-bar-options-menu [icicle-toggle-incremental-completion]
             '("[Icy] Toggle Incremental Completion" . icicle-incremental-completion))
           (define-key menu-bar-options-menu [icicle-toggle-ignore]
             '("[Icy] Toggle Ignoring Space Prefix" . icicle-toggle-ignored-space-prefix))
           (define-key menu-bar-options-menu [icicle-toggle-ignore]
             '("[Icy] Toggle Ignored File Extensions" . icicle-toggle-ignored-extensions)))
          (t
           (define-key icicle-menu-map [icicle-set-option-to-t]
             '("+ Turn On Any Option..." . icicle-set-option-to-t))
           (define-key icicle-menu-map [icicle-reset-option-to-nil]
             '("+ Turn Off Any Option..." . icicle-reset-option-to-nil))
           (define-key icicle-menu-map [icicle-toggle-option]
             '("+ Toggle Any Option..." . icicle-toggle-option))
           (define-key icicle-menu-map [icicle-toggle-transforming]
             '("Toggle Duplicate Removal" . icicle-toggle-transforming))
           (define-key icicle-menu-map [icicle-toggle-sorting]
             '("Toggle Completion Sorting" . icicle-toggle-sorting))
           (define-key icicle-menu-map [icicle-toggle-search-cleanup]
             '("Toggle Removal of Search Highlighting" . icicle-toggle-search-cleanup))
           (define-key icicle-menu-map [icicle-toggle-regexp-quote]
             '("Toggle Escaping Special Chars" . icicle-toggle-regexp-quote))
           (define-key icicle-menu-map [icicle-toggle-incremental-completion]
             '("Toggle Incremental Completion" . icicle-incremental-completion))
           (define-key icicle-menu-map [icicle-toggle-ignore]
             '("Toggle Ignoring Space Prefix" . icicle-toggle-ignored-space-prefix))
           (define-key icicle-menu-map [icicle-toggle-ignore]
             '("Toggle Ignored File Extensions" . icicle-toggle-ignored-extensions))
           (define-key icicle-menu-map [icicle-separator-toggle] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-custom-menu)) ; Use Customize menu, if available.
           (define-key menu-bar-custom-menu [icicle-customize-separator] '("--"))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-groups]
             '("[Icy] Groups Matching Regexp..." . icicle-customize-apropos-groups))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-faces]
             '("[Icy] Faces Matching Regexp..." . icicle-customize-apropos-faces))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-options]
             '("[Icy] Options Matching Regexp..." . icicle-customize-apropos-options))
           (define-key menu-bar-custom-menu [icicle-customize-apropos]
             '("[Icy] Settings Matching Regexp..." . icicle-customize-apropos)))
          (t
           (define-key icicle-menu-map [icicle-customize-separator] '("--"))
           (define-key icicle-menu-map [icicle-customize-apropos-groups]
             '("Groups Matching Regexp..." . icicle-customize-apropos-groups))
           (define-key icicle-menu-map [icicle-customize-apropos-faces]
             '("Faces Matching Regexp..." . icicle-customize-apropos-faces))
           (define-key icicle-menu-map [icicle-customize-apropos-options]
             '("Options Matching Regexp..." . icicle-customize-apropos-options))
           (define-key icicle-menu-map [icicle-customize-apropos]
             '("Settings Matching Regexp..." . icicle-customize-apropos))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'Info-mode-menu)) ; Use Info menu, if available.
           (define-key Info-mode-menu [icicle-Info-separator] '("--"))
           (define-key Info-mode-menu [icicle-Info-goto-node]
             '("[Icy] + Go to Node..." . icicle-Info-goto-node))
           (define-key Info-mode-menu [icicle-Info-index]
             '("[Icy] + Look Up in Index..." . icicle-Info-index)))
          (t
           (define-key icicle-menu-map [icicle-Info-separator] '("--"))
           (define-key icicle-menu-map [icicle-Info-goto-node]
             '("+ Go to Node..." . icicle-Info-goto-node))
           (define-key icicle-menu-map [icicle-Info-index]
             '("+ Look Up in Index..." . icicle-Info-index))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-edit-menu)) ; Use Edit menu, if available.
           (define-key menu-bar-edit-menu [icicle-edit-separator] '("--"))
           (define-key menu-bar-edit-menu [icicle-complete-thesaurus-entry]
             '("[Icy] Complete with Thesaurus..." . icicle-complete-thesaurus-entry))
           (define-key menu-bar-edit-menu [icicle-insert-thesaurus-entry]
             '("[Icy] + Insert Thesaurus Entry..." . icicle-insert-thesaurus-entry))
           (define-key menu-bar-edit-menu [icicle-insert-kill]
             '("[Icy] + Paste Copied Text..." . icicle-insert-kill)))
          (t
           (define-key icicle-menu-map [icicle-edit-separator] '("--"))
           (define-key icicle-menu-map [icicle-complete-thesaurus-entry]
             '("Complete with Thesaurus..." . icicle-complete-thesaurus-entry))
           (define-key icicle-menu-map [icicle-insert-thesaurus-entry]
             '("+ Insert Thesaurus Entry..." . icicle-insert-thesaurus-entry))
           (define-key icicle-menu-map [icicle-insert-kill]
             '("+ Paste Copied Text..." . icicle-insert-kill))))
    (put 'icicle-complete-thesaurus-entry 'menu-enable
         '(and icicle-mode (not buffer-read-only) (boundp 'synonyms-obarray)))
    (put 'icicle-insert-thesaurus-entry 'menu-enable
         '(and icicle-mode (not buffer-read-only) (boundp 'synonyms-obarray)))
    (put 'icicle-insert-kill 'menu-enable '(and icicle-mode (not buffer-read-only)))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-file-menu)) ; Use File menu, if available.
           (define-key menu-bar-file-menu [icicle-file-separator] '("--"))
           (define-key menu-bar-file-menu [icicle-kill-buffer]
             '("[Icy] + Kill Buffer..." . icicle-kill-buffer))
           (define-key menu-bar-file-menu [icicle-delete-file]
             '("[Icy] + Delete File..." . icicle-delete-file))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key menu-bar-file-menu [icicle-recent-file-other-window]
               '("[Icy] + Open Recent File (Other Window)..." . icicle-recent-file-other-window))
             (define-key menu-bar-file-menu [icicle-recent-file]
               '("[Icy] + Open Recent File..." . icicle-recent-file)))
           (define-key menu-bar-file-menu [icicle-dired-saved-file-candidates-other-window]
             '("[Icy] Dired Saved Candidates (Other Window)..." .
               icicle-dired-saved-file-candidates-other-window))
           (define-key menu-bar-file-menu [icicle-dired-saved-file-candidates]
             '("[Icy] Dired Saved Candidates..." .
               icicle-dired-saved-file-candidates))
           (define-key menu-bar-file-menu [icicle-locate-file]
             '("[Icy] + Open File under Directory..." . icicle-locate-file))
           (define-key menu-bar-file-menu [icicle-locate-file-other-window]
             '("[Icy] + Open File under Directory (Other Window)..." .
               icicle-locate-file-other-window))
           (define-key menu-bar-file-menu [icicle-find-file-other-window]
             '("[Icy] + Open File or Directory (Other Window)..." . icicle-find-file-other-window))
           (define-key menu-bar-file-menu [icicle-find-file]
             '("[Icy] + Open File or Directory..." . icicle-find-file)))
          (t
           (define-key icicle-menu-map [icicle-kill-buffer]
             '("+ Kill Buffer..." . icicle-kill-buffer))
           (define-key icicle-menu-map [icicle-delete-file]
             '("+ Delete File..." . icicle-delete-file))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key icicle-menu-map [icicle-recent-file-other-window]
               '("+ Open Recent File (Other Window)..." . icicle-recent-file-other-window))
             (define-key icicle-menu-map [icicle-recent-file]
               '("+ Open Recent File..." . icicle-recent-file)))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates-other-window]
             '("[Icy] Dired Saved Candidates (Other Window)..." .
               icicle-dired-saved-file-candidates-other-window))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates]
             '("[Icy] Dired Saved Candidates..." .
               icicle-dired-saved-file-candidates))
           (define-key icicle-menu-map [icicle-locate-file-other-window]
             '("+ Open File under Directory (Other Window)..." . icicle-locate-file-other-window))
           (define-key icicle-menu-map [icicle-locate-file]
             '("+ Open File under Directory..." . icicle-locate-file))
           (define-key icicle-menu-map [icicle-find-file-other-window]
             '("+ Open File or Directory (Other Window)..." . icicle-find-file-other-window))
           (define-key icicle-menu-map [icicle-find-file]
             '("+ Open File or Directory ..." . icicle-find-file))))
    (put 'icicle-delete-file 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-find-file 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-find-file-other-window 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (when (condition-case nil (require 'recentf) (error nil))
      (put 'icicle-recent-file 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
      (put 'icicle-recent-file-other-window 'menu-enable
           '(and icicle-mode
             (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
    (put 'icicle-dired-saved-file-candidates 'menu-enable
         '(and icicle-mode icicle-saved-completion-candidates
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-dired-saved-file-candidates-other-window 'menu-enable
         '(and icicle-mode icicle-saved-completion-candidates
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-buffer-list] '("+ Buffer List..." . icicle-buffer-list))
    (define-key icicle-menu-map [icicle-remove-buffer-config]
      '("+ Remove Buffer Configuration..." . icicle-remove-buffer-config))
    (put 'icicle-remove-buffer-config 'menu-enable '(and icicle-mode icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-add-buffer-config]
      '("New Buffer Configuration..." . icicle-add-buffer-config))
    (define-key icicle-menu-map [icicle-buffer-config]
      '("+ Choose Buffer Configuration..." . icicle-buffer-config))
    (put 'icicle-buffer-config 'menu-enable '(and icicle-mode icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-remove-buffer-candidate]
      '("+ Don't Always Include Buffer..." . icicle-remove-buffer-candidate))
    (put 'icicle-remove-buffer-candidate 'menu-enable '(and icicle-mode icicle-buffer-extras))
    (define-key icicle-menu-map [icicle-add-buffer-candidate]
      '("+ Always Include Buffer..." . icicle-add-buffer-candidate))
    (define-key icicle-menu-map [icicle-kill-buffer] '("+ Kill Buffer..." . icicle-kill-buffer))
    (define-key icicle-menu-map [icicle-delete-windows]
      '("+ Delete Windows on Buffer..." . icicle-delete-windows))
    (define-key icicle-menu-map [icicle-buffer-other-window]
      '("+ Switch to Buffer (Other Window)..." . icicle-buffer-other-window))
    (put 'icicle-buffer-other-window 'menu-enable
         '(and icicle-mode (not (window-minibuffer-p (frame-selected-window
                                                      menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-buffer] '("+ Switch to Buffer..." . icicle-buffer))
    (put 'icicle-buffer 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
           (require 'bookmark)          ; `bookmark-buffer-name' is not autoloaded.
           (define-key menu-bar-bookmark-map [icicle-bookmark]
             '("[Icy] + Jump to Bookmark Using Icicles..." . icicle-bookmark)))
          (t
           (define-key icicle-menu-map [icicle-bookmark]
             '("+ Jump To Bookmark..." . icicle-bookmark))
           (define-key icicle-menu-map [icicle-separator-bookmark-buffer] '("--"))))
    (put 'icicle-bookmark 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-menu)) ; Use Search menu, if available.
           (define-key menu-bar-search-menu [icicle-separator-search] '("--"))
           (define-key menu-bar-search-menu [icicle-search-highlight-cleanup]
             '("[Icy] Remove Search Highlighting..." . icicle-search-highlight-cleanup))
           (define-key menu-bar-search-menu [icicle-compilation-search]
             '("[Icy] + Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
           (define-key menu-bar-search-menu [icicle-search]
             '("[Icy] + Search (Regexp)..." . icicle-search))
           (define-key menu-bar-search-menu [icicle-occur]
             '("[Icy] + Occur (Regexp)..." . icicle-occur)))
          (t
           (define-key icicle-menu-map [icicle-search-highlight-cleanup]
             '("Remove Search Highlighting..." . icicle-search-highlight-cleanup))
           (define-key icicle-menu-map [icicle-compilation-search]
             '("+ Search Compilation/Grep Hits (Regexp)..." . icicle-compilation-search))
           (define-key icicle-menu-map [icicle-search] '("+ Search (Regexp)..." . icicle-search))
           (define-key icicle-menu-map [icicle-occur] '("+ Occur (Regexp)..." . icicle-occur))))
    (put 'icicle-search-highlight-cleanup 'menu-enable
         '(and icicle-mode (or icicle-search-overlays
                            (overlayp icicle-search-current-overlay)
                            (overlayp icicle-search-refined-overlays)
                            icicle-search-refined-overlays)))
    (put 'icicle-compilation-search 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
           (condition-case nil (eq (current-buffer) (compilation-find-buffer)) (error nil))))
    (put 'icicle-search 'menu-enable
         '(and icicle-mode
           (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (put 'icicle-occur 'menu-enable
         '(and icicle-mode (not (window-minibuffer-p (frame-selected-window
                                                      menu-updating-frame))))))

  ;; Install Icicles menu-bar menu.
  (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" icicle-menu-map))

  ;; Optional `icicle-mode-map' bindings - governed by `icicle-bind-top-level-commands-flag'.
  (when icicle-bind-top-level-commands-flag
    (define-key icicle-mode-map [pause]    'icicle-switch-to/from-minibuffer)
    (define-key icicle-mode-map "\C-c`"    'icicle-search-generic)
    (define-key icicle-mode-map "\C-c'"    'icicle-occur)
    (define-key icicle-mode-map "\C-c/"    'icicle-complete-thesaurus-entry)
    (define-key icicle-mode-map "\C-x\M-e" 'icicle-execute-named-keyboard-macro)
    (when (fboundp 'icicle-kmacro) (define-key icicle-mode-map [f5] 'icicle-kmacro)) ; Emacs 22
      
    ;; Remap some top-level commands.
    (icicle-remap 'abort-recursive-edit                   'icicle-abort-minibuffer-input
                  icicle-mode-map (current-global-map)) ; `C-g'
    (icicle-remap 'execute-extended-command               'icicle-execute-extended-command
                  icicle-mode-map (current-global-map)) ; `M-x'
    (icicle-remap 'switch-to-buffer                       'icicle-buffer
                  icicle-mode-map (current-global-map)) ; `C-x b'
    (icicle-remap 'switch-to-buffer-other-window          'icicle-buffer-other-window
                  icicle-mode-map (current-global-map)) ; `C-x 4 b'
    (icicle-remap 'find-file                              'icicle-find-file
                  icicle-mode-map (current-global-map)) ; `C-x C-f'
    (icicle-remap 'find-file-other-window                 'icicle-find-file-other-window
                  icicle-mode-map (current-global-map)) ; `C-x 4 f'
    (icicle-remap 'kill-buffer                            'icicle-kill-buffer
                  icicle-mode-map (current-global-map)) ; `C-x k'
    (icicle-remap 'kill-buffer-and-its-windows            'icicle-kill-buffer
                  icicle-mode-map (current-global-map)) ; `C-x k'
    (icicle-remap 'delete-window                          'icicle-delete-window
                  icicle-mode-map (current-global-map)) ; `C-x 0'
    (icicle-remap 'delete-windows-for                     'icicle-delete-window
                  icicle-mode-map (current-global-map)) ; `C-x 0'
    (icicle-remap 'other-window-or-frame                  'icicle-other-window-or-frame
                  icicle-mode-map (current-global-map)) ; `C-x o'
    (icicle-remap 'other-window                           'icicle-other-window-or-frame
                  icicle-mode-map (current-global-map)) ; `C-x o'
    (icicle-remap 'exchange-point-and-mark                'icicle-exchange-point-and-mark
                  icicle-mode-map (current-global-map)) ; `C-x C-x'
    (icicle-remap 'yank                                   'icicle-yank-insert
                  icicle-mode-map (current-global-map)) ; `C-y'
            
    ;; This is for Icicles Menu, not Icicles, but it's convenient to do this here.
    (when (fboundp 'icicle-execute-menu-command) ; Defined in `icicles-menu.el'.
      (define-key icicle-mode-map [?\e ?\M-x] 'icicle-execute-menu-command) ; `ESC M-x'
      (define-key icicle-mode-map [?\M-`] 'icicle-execute-menu-command) ; `M-`'
      (define-key icicle-mode-map [f10] 'icicle-execute-menu-command))) ; Replaces `tmm-menu'.

  ;; Bind `S-TAB' in `icicle-mode-map', for generic `S-TAB'.  Emacs 22.
  (when (fboundp 'map-keymap) (define-key icicle-mode-map [S-tab] 'icicle-generic-S-tab)) ; `S-TAB'

  ;; Install or update `icicle-mode-map'.
  (if icicle-minor-mode-map-entry
      (setcdr icicle-minor-mode-map-entry icicle-mode-map)
    (setq icicle-minor-mode-map-entry (cons 'icicle-mode icicle-mode-map))
    (add-to-list 'minor-mode-map-alist icicle-minor-mode-map-entry)))

(defun icicle-rebind-non-completion-keys ()
  "Rebind some keys in maps other than minibuffer maps and `icicle-mode-map'"
  ;; Replace some standard Info bindings.
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map) ; `g'
          (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map))) ; `i'
    (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map)
    (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map))

  ;; Bind `S-TAB' in all maps, for key completion.  This is NOT undone when you leave Icicle mode.
  (when (fboundp 'map-keymap)           ; Emacs 22.
    (dolist (key+map (accessible-keymaps (current-global-map)))
      (when (and (keymapp (cdr key+map))
                 (not (stringp (car-safe (last (cdr key+map)))))) ; Try to exclude menu maps.
        (define-key (cdr key+map) [S-tab] 'icicle-complete-keys)))))

(defun icicle-restore-non-completion-keys ()
  "Restore some bindings changed by `icicle-rebind-non-completion-keys'.
`S-TAB' is still left bound in all maps."
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
          (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)))
    (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
    (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)))

 


;;; Other Icicles functions that define Icicle mode---------

(defun icicle-rebind-completion-maps (turn-on-p)
  "Rebind minibuffer completion maps to be able to cycle completions.
Also, update the bindings in the minibuffer-completion help variables.

This is called by `icicle-mode'.  When in Icicle mode, all keys that
are globally bound to `next-line' are rebound in the minibuffer to
`icicle-next-prefix-candidate', for minibuffer completion purposes.
Similarly for other keys."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'icicle-abort-minibuffer-input
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'icicle-abort-minibuffer-input)))
     (define-key minibuffer-local-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-map [(control ?=)]  'icicle-insert-string-from-variable)
     ;; Replaces `kill-sentence':
     (define-key minibuffer-local-map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element)
     (define-key minibuffer-local-map [(meta ?:)]     'icicle-pp-eval-expression)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     (define-key minibuffer-local-ns-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-ns-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-ns-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-ns-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-ns-map [(control ?=)]  'icicle-insert-string-from-variable)
     (define-key minibuffer-local-ns-map [(meta ?k)] ; Replaces `kill-sentence':
       'icicle-erase-minibuffer-or-history-element)
     (define-key minibuffer-local-ns-map [(meta ?:)]     'icicle-pp-eval-expression)

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]  'icicle-abort-minibuffer-input)
     (define-key minibuffer-local-isearch-map [M-S-backspace] 'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [M-S-delete]    'icicle-erase-minibuffer)
     (define-key minibuffer-local-isearch-map [(meta ?.)]     'icicle-insert-string-at-point)
     (define-key minibuffer-local-isearch-map [(control ?=)]  'icicle-insert-string-from-variable)
     (define-key minibuffer-local-isearch-map [(meta ?k)] ; Replaces `kill-sentence':
       'icicle-erase-minibuffer-or-history-element)
     (define-key minibuffer-local-isearch-map [(meta ?:)]     'icicle-pp-eval-expression)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-filename-completion-map': file-name completion map (Emacs 22).
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-bind-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-bind-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [S-return] 'icicle-apropos-complete-and-exit)

     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on [insert].  Do not allow normal input.
     (define-key completion-list-mode-map [(control ?g)]   'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map "q"              'icicle-abort-minibuffer-input)
     (define-key completion-list-mode-map [insert]         'icicle-insert-completion)
     (define-key completion-list-mode-map [down]           'icicle-next-line)
     (define-key completion-list-mode-map [up]             'icicle-previous-line)
     (define-key completion-list-mode-map [S-iso-lefttab]  'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [S-tab]          'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [left]           'icicle-move-to-previous-completion)
     (define-key completion-list-mode-map [(control ?i)]   'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [tab]            'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [right]          'icicle-move-to-next-completion)
     (define-key completion-list-mode-map [C-down-mouse-2] 'icicle-mouse-candidate-action)
     (define-key completion-list-mode-map [C-mouse-2]      'ignore)
     (define-key completion-list-mode-map [C-M-down-mouse-2] 'icicle-mouse-help-on-candidate)
     (define-key completion-list-mode-map [M-mouse-2]      'icicle-mouse-candidate-read-fn-invoke)
     (define-key completion-list-mode-map [C-M-mouse-2]    'ignore)
     (define-key completion-list-mode-map [C-M-return]     'icicle-help-on-candidate)
     (define-key completion-list-mode-map [mouse-3]        'icicle-Completions-mouse-3-menu)
     ;; (suppress-keymap completion-list-mode-map) ; Inhibit character self-insertion.
     )


    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (if (> emacs-major-version 21)
         (define-key minibuffer-local-map [menu-bar minibuf quit]
           (list 'menu-item "Quit" 'keyboard-escape-quit
                 :help "Abort input and exit minibuffer"))
       (define-key minibuffer-local-map [menu-bar minibuf quit]
         (cons "Quit" 'keyboard-escape-quit)))
     (define-key minibuffer-local-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-map [M-S-backspace] nil)
     (define-key minibuffer-local-map [M-S-delete]    nil)
     (define-key minibuffer-local-map [(meta ?.)]     nil)
     (define-key minibuffer-local-map [(control ?=)]  nil)
     (define-key minibuffer-local-map [(meta ?k)]     nil)
     (define-key minibuffer-local-map [(meta ?:)]     nil)

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     (define-key minibuffer-local-ns-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-ns-map [M-S-backspace] nil)
     (define-key minibuffer-local-ns-map [M-S-delete]    nil)
     (define-key minibuffer-local-ns-map [(meta ?.)]     nil)
     (define-key minibuffer-local-ns-map [(control ?=)]  nil)
     (define-key minibuffer-local-ns-map [(meta ?k)]     nil)
     (define-key minibuffer-local-ns-map [(meta ?:)]     nil)


     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     (define-key minibuffer-local-isearch-map [(control ?g)]  'abort-recursive-edit)
     (define-key minibuffer-local-isearch-map [M-S-backspace] nil)
     (define-key minibuffer-local-isearch-map [M-S-delete]    nil)
     (define-key minibuffer-local-isearch-map [(meta ?.)]     nil)
     (define-key minibuffer-local-isearch-map [(control ?=)]  nil)
     (define-key minibuffer-local-isearch-map [(meta ?k)]     nil)
     (define-key minibuffer-local-isearch-map [(meta ?:)]     nil)

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-filename-completion-map': file-name completion map.
     (when (boundp 'minibuffer-local-filename-completion-map)
       (icicle-restore-completion-keys minibuffer-local-filename-completion-map))

     ;; `minibuffer-local-must-match-map': must-match map.
     (icicle-restore-completion-keys minibuffer-local-must-match-map)
     (define-key minibuffer-local-must-match-map [S-return] nil)

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (define-key completion-list-mode-map [(control ?g)]    nil)
     (define-key completion-list-mode-map "q"               nil)
     (define-key completion-list-mode-map [insert]          nil)
     (define-key completion-list-mode-map [down]            nil)
     (define-key completion-list-mode-map [up]              nil)
     (define-key completion-list-mode-map [left]            'previous-completion)
     (define-key completion-list-mode-map [right]           'next-completion)
     (define-key completion-list-mode-map [S-iso-lefttab]   nil)
     (define-key completion-list-mode-map [S-tab]           nil)
     (define-key completion-list-mode-map [tab]             nil)
     (define-key completion-list-mode-map [(control ?i)]    nil)
     (define-key completion-list-mode-map [C-down-mouse-2]  nil)
     (define-key completion-list-mode-map [C-mouse-2]       nil)
     (define-key completion-list-mode-map [M-mouse-2]       nil)
     (define-key completion-list-mode-map [C-M-down-mouse-2] nil)
     (define-key completion-list-mode-map [C-M-mouse-2]     nil)
     (define-key completion-list-mode-map [C-M-return]      nil)
     (define-key completion-list-mode-map [mouse-3]         nil)))

  ;; Update the bindings within the help string.
  (setq icicle-completion-help-string
        (substitute-command-keys
         "\\<minibuffer-local-completion-map>                     \
Icicles Minibuffer Completion
                     -----------------------------

Minibuffer input can be completed in several ways.
These are the main Icicles actions and their minibuffer key bindings:

 * Display this help.                        \\[icicle-completion-help]
     For help on individual completion candidates, see \"Show help on
     completion candidates\", below.

 * Manipulate your input.  You can modify it, before committing it.
     Erase (clear) your input                M-S-backspace
     Yank text at cursor into minibuffer     \\[icicle-insert-string-at-point]
     Retrieve your last real input           \\[icicle-retrieve-last-input] (repeat)
     Insert text (regexp) from a variable    \\[icicle-insert-string-from-variable]
     Abandon input (from minibuffer)         \\[icicle-abort-minibuffer-input]
     Abandon input (from anywhere)           \\[abort-recursive-edit]
     Send input to Emacs                     RET
       Complete partial input, then send     \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>

 * Complete the current input in the minibuffer.
     Apropos (regexp) completion             \\[icicle-apropos-complete]
       Without displaying candidates         \\[icicle-prefix-complete-no-display]
       Match another regexp (chaining)       \\[icicle-narrow-candidates]
     Prefix completion
       As much as possible                   \\[icicle-prefix-complete]
         Without displaying candidates       \\[icicle-prefix-complete-no-display]
       A word at a time                      \\[icicle-prefix-word-complete]
     Complete and send (when match required) \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>
     Complete search string with search ring \\[icicle-apropos-complete]

 * Display/navigate completions for current input (in *Completions*).
     Show completion candidates
       Prefix completion                     \\[icicle-prefix-complete] (repeat)
       Apropos completion                    \\[icicle-apropos-complete]
     Move between minibuffer and list        \\<completion-list-mode-map>\
\\[icicle-insert-completion]
     Cycle among completion candidates       right, left, \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
       Within a *Completions* column         down, up
     Choose a completion candidate           \\[choose-completion], \
\\[mouse-choose-completion]\\<minibuffer-local-completion-map>

 * Cycle among input candidates.
     Prefix-completion candidates            down, up
     Apropos-completion candidates           next, prior
     Minibuffer history items                \\[next-history-element], \
\\[previous-history-element]

 * Choose a previous input from the minibuffer history.
     Apropos-complete against history items  \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     Sort history items first (default sort) \\[icicle-toggle-alternative-sorting]
     Cycle among minibuffer history items    \\[next-history-element], \
\\[previous-history-element]
     Search among minibuffer history items   \
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Toggle Icicles options on the fly.
     Case-sensitivity                        \\[icicle-toggle-case-sensitivity]
     Incremental completion                  \\[icicle-toggle-incremental-completion]
     Sorting completion candidates           \\[icicle-toggle-sorting]
     Alternative sorting                     \\[icicle-toggle-alternative-sorting]
     Removal of duplicate candidates         \\[icicle-toggle-transforming]
     Removal of `icicle-search' highlighting \\[icicle-dispatch-C-.]
     Ignoring certain file extensions        \\[icicle-dispatch-C-.]
     `icicle-search' all-current highlights  \\[icicle-dispatch-C-^]
     Ignoring space prefix                   \\[icicle-dispatch-C-^]
     Using `~' for your home directory       \\[icicle-toggle-~-for-home-dir]
     Escaping regexp special chars           \\[icicle-toggle-regexp-quote]

 * Show help on completion candidates.
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous prefix-match candidate   C-M-down, C-M-up
     Next, previous apropos-match candidate  C-M-next, C-M-prior

 * Multi-commands: Act on completion candidates.
     Current candidate                       C-RET, C-o, C-mouse-2
     Next, previous prefix-match candidate   C-down, C-up
     Next, previous apropos-match candidate  C-next, C-prior
     All candidates at once                  \\[icicle-all-candidates-action]
     Object-action: apply a fn to candidate  M-RET

 * Perform set operations on candidate sets.
     Set complement                          \\[icicle-candidate-set-complement]
     Set difference                          \\[icicle-candidate-set-difference]
     Set union                               \\[icicle-candidate-set-union]
     Set intersection                        \\[icicle-candidate-set-intersection]
     Set intersection using another regexp   \\[icicle-narrow-candidates]
     Save current set                        \\[icicle-candidate-set-save]
     Retrieve saved set                      \\[icicle-candidate-set-retrieve]
     Save current set to cache file          \\[icicle-candidate-set-save-to-cache-file]
     Retrieve saved set from cache file      \\[icicle-candidate-set-retrieve-from-cache-file]
     Save current set to variable            \\[icicle-candidate-set-save-to-variable]
     Retrieve saved set from variable        \\[icicle-candidate-set-retrieve-from-variable]
     Swap current and saved sets             \\[icicle-candidate-set-swap]
     Define current set by evaluating sexp   \\[icicle-candidate-set-define]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]

Remember: You can always input any character that is bound to a
          command (e.g. \\[icicle-prefix-complete]) \
by preceding it with \\<global-map>\\[quoted-insert].

Though it has no direct connection with completion, you can use \
`\\<minibuffer-local-completion-map>\\[icicle-pp-eval-expression]'
in the minibuffer at any time to evaluate an Emacs-Lisp expression.
This calls `pp-eval-expression', which displays the result in the
echo area or a pop-up buffer, *Pp Eval Output*.

----------------------------------------------------------------------

Customize Icicles: `M-x icicle-customize-icicles-group'.
Summary of customizable options and faces (alphabetical order).

Some of the binary options can be toggled; their toggle keys are
mentioned here.

* `completion-ignore-case', `read-file-name-completion-ignore-case'
                                         - Case sensitivity? (`C-A')
* `completion-ignored-extensions'        - Ignored filenames (`C-.')
* `icicle-alternative-sort-function'     - `M-,' alternative to
                                           `icicle-sort-function'
* `icicle-bind-top-level-commands-flag'  - Bind Icicles commands?
* `icicle-buffer-*'                      - `icicle-buffer' options
* `icicle-change-region-background-flag' - Change region color?
* `icicle-color-themes'                  - For `icicle-color-theme'
* `icicle-complete-keys-self-insert-flag'- `S-TAB' for self-insert?
* `icicle-completing*-prompt-prefix'     - Completing prompt indicator
* `icicle-Completions-display-min-input-chars' - Remove *Completions*
                                           if fewer chars input
* `icicle-Completions-frame-at-right-flag'- *Completions* at right?
* `icicle-cycle-into-subdirs-flag'       - Explore subdirectories?
* `icicle-cycling-respects-completion-mode-flag' - Completion mode
                                           affects cycling mode?
* `icicle-default-thing-insertion'       - Control behavior of \
\\<minibuffer-local-completion-map>\\[icicle-insert-string-at-point]
* `icicle-expand-input-to-common-match-flag'- Maximally expand input?
* `icicle-highlight-input-initial-whitespace-flag'
                                         - Highlight input whitespace?
* `icicle-ignore-space-prefix-flag'      - See initial space? (`C-^')
* `icicle-incremental-completion-delay'  - Before update *Completions*
* `icicle-incremental-completion-flag'   - Icompletion? (`C-#')
* `icicle-incremental-completion-threshold'- # of candidates for delay
* `icicle-init-value-flag'               - Use default as init value?
* `icicle-input-string'                  - String inserted by `C-='
* `icicle-key-descriptions-use-<>-flag'  - Key shown as \"<>\"? (`C-<')
* `icicle-kmacro-ring-max'               - Icicles `kmacro-ring-max'
* `icicle-list-join-string'              - Multi-completion join
* `icicle-mark-position-in-candidate'    - Mark position in cycling
* `icicle-minibuffer-setup-hook'         - Functions run after setup
* `icicle-modal-cycle-down-key'          - Down key for modal cycling
* `icicle-modal-cycle-up-key'            - Up key for modal cycling
* `icicle-point-position-in-candidate'   - Cursor position in cycling
* `icicle-redefine-standard-commands-flag'- Redefine std commands?
* `icicle-regexp-quote-flag'             - Escape chars? (`C-`')
* `icicle-regexp-search-ring-max'        - `regexp-search-ring-max'
* `icicle-region-background'             - Background for region
* `icicle-regions'                       - List of regions
* `icicle-regions-name-length-max'       - # chars to name a region
* `icicle-reminder-prompt-flag'          - Show reminder in prompt?
* `icicle-require-match-flag'            - Override REQUIRE-MATCH?
* `icicle-saved-completion-sets'         - Completion sets for `\\[icicle-candidate-set-retrieve]'
* `icicle-search-cleanup-flag'           - Remove search highlighting?
                                           (`C-.')
* `icicle-search-highlight-all-current-flag'- In each hit (`C-^')
* `icicle-search-highlight-threshold'    - # hits to highlight at once
* `icicle-search-hook'                   - Functions run by `C-c `'
* `icicle-search-ring-max'               - Icicles `search-ring-max'
* `icicle-show-Completions-help-flag'    - Show *Completions* help?
* `icicle-show-Completions-initially-flag'- Show *Completions* first?
* `icicle-sort-function'                 - Sort candidates (`C-,')
* `icicle-special-candidate-regexp'      - For highlighted candidates
* `icicle-TAB-shows-candidates-flag'     - 1st `TAB' shows candidates?
* `icicle-thing-at-point-functions'      - Functions to yank things
* `icicle-touche-pas-aux-menus-flag'     - Add to standard menus?
* `icicle-transform-function'            - Remove duplicates (`C-$')
* `icicle-update-input-hook'             - Fns run when input changes
* `icicle-word-completion-key'           - Key for word completion

Faces that highlight input in minibuffer.

* `icicle-complete-input'               - Input when it is complete
* `icicle-match-highlight-minibuffer'   - Matched part of input
* `icicle-whitespace-highlight'         - Initial whitespace in input

Faces that highlight candidates in buffer *Completions*.

* `icicle-common-match-highlight-Completions' - Max common substring
* `icicle-current-candidate-highlight'  - Highlight cycle candidate
* `icicle-historical-candidate'         - Highlight candidates used
* `icicle-match-highlight-Completions'  - Matched part of input

Faces that highlight for command `icicle-search'.

* `icicle-search-current-input'         - What input matches
* `icicle-search-main-regexp-current'   - Current match of 1st regexp
* `icicle-search-main-regexp-others'    - Other matches of 1st regexp

----------------------------------------------------------------------

Some top-level Icicles commands (alphabetical order, with exceptions).
Bind them to keys you like.  See recommended bindings in `icicles.el'.
Multi-commands are indicated by `+': They act any number of times.
You can tell a multi-command when you execute it by the fact that the
input prompt is prefixed by `+'.

+ `icicle-add-buffer-candidate'        - To always-candidate buffer
+ `icicle-remove-buffer-candidate'     -   From same
  `icicle-add-buffer-config'           - To `icicle-buffer-configs'
+ `icicle-remove-buffer-config'        -   From same
  `icicle-add-region'                  - To `icicle-regions'
+ `icicle-remove-region'               -   From same
  `icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
+ `icicle-remove-saved-completion-set' -   From same
  `icicle-apropos'                     - `apropos', but shows matches
  `icicle-apropos-command'             - Enhanced `apropos-command'
  `icicle-apropos-variable'            - Enhanced `apropos-variable'
  `icicle-apropos-zippy'               - Show matching Zippy quotes
+ `icicle-bookmark'                    - Jump to bookmark
+ `icicle-buffer'(`-other-window')     - Switch to buffer (`C-x b')
+ `icicle-buffer-config'               - Pick `icicle-buffer' options
+ `icicle-buffer-list'                 - Choose a list of buffer names
+ `icicle-clear-option'                - Set binary option to nil
+ `icicle-color-theme'                 - Change color theme
+ `icicle-comint-command'              - Reuse command (`C-c TAB')
+ `icicle-comint-search'               - Reuse command (`C-c `')
+ `icicle-compilation-search'          - Show hits (`C-c `')
+ `icicle-complete-keys'               - Complete keys (`S-TAB')
  `icicle-complete-thesaurus-entry'    - Complete word (`C-/')
  `icicle-completion-help'             - Display this help
  `icicle-customize-icicles-group'     - Customize options and faces
+ `icicle-delete-file'                 - Delete file/directory
+ `icicle-delete-windows'              - Delete windows (`C-u C-x 0')
+ `icicle-doc'                         - Show doc for fn, var, or face
+ `icicle-execute-extended-command'    - Execute Emacs command (`M-x')
+ `icicle-execute-named-keyboard-macro' - Execute named keyboard macro
+ `icicle-find-file'(`-other-window')  - Visit file/dir (`C-x C-f')
+ `icicle-font'                        - Change font of frame
+ `icicle-frame-bg'                    - Change background of frame
+ `icicle-frame-fg'                    - Change foreground of frame
+ `icicle-fundoc'                      - Show function description
+ `icicle-goto-global-marker'          - Go to a global marker
+ `icicle-goto-marker'                 - Go to a marker in this buffer
+ `icicle-imenu'                       - Navigate among Imenu entries
+ `icicle-Info-goto-node'              - Multi-cmd `Info-goto-node' 
+ `icicle-Info-index'                  - Multi-command `Info-index'
+ `icicle-insert-kill'                 - Like `yank', without rotating
+ `icicle-insert-thesaurus-entry'      - Insert thesaurus entry
+ `icicle-kill-buffer'                 - Kill buffer (`C-x k')
+ `icicle-kmacro'                      - Execute keyboard macro (`f5')
+ `icicle-locate-file'(`-other-window') - Visit file in a directory
+ `icicle-map'                         - Apply function to alist items
  `icy-mode' or `icicle-mode'          - Toggle Icicle mode
+ `icicle-occur'                       - Enhanced `occur' (`C-c '')
+ `icicle-other-window-or-frame'       - Other window/frame (`C-x o')
+ `icicle-plist'                       - Show symbols, property lists
+ `icicle-recent-file'(`-other-window') - Open recently used file
+ `icicle-reset-option-to-nil'         - Set binary option to nil
  `icicle-save-string-to-variable'     - Save text for use with \
`\\[icicle-insert-string-from-variable]'
+ `icicle-search'                      - Search (`C-c `')
+ `icicle-search-region'               - Search multiple regions
+ `icicle-select-frame'                - Select a frame by name
+ `icicle-select-region'               - Select a region
+ `icicle-select-window'               - Select window by buffer name
  `icicle-send-bug-report'             - Send Icicles bug report
+ `icicle-set-option-to-t'             - Set binary option to t
  `icicle-toggle-~-for-home-dir'       - Toggle using `~' for $HOME
  `icicle-toggle-case-sensitivity'     - Toggle case sensitivity
  `icicle-toggle-ignored-extensions'   - Toggle ignored files
  `icicle-toggle-ignored-space-prefix' - Toggle ignoring space
  `icicle-toggle-incremental-completion' - Toggle icompletion
+ `icicle-toggle-option'               - Toggle binary user option
  `icicle-toggle-regexp-quote'         - Toggle regexp escaping
  `icicle-toggle-search-cleanup'       - Toggle highlight removal
  `icicle-toggle-sorting'              - Toggle sorting
  `icicle-toggle-transforming'         - Toggle duplicate removal
+ `icicle-vardoc'                      - Show variable description
  `icicle-yank-insert'                 - `yank' + completion (`C-y')

----------------------------------------------------------------------

Send an Icicles bug report: `\\[icicle-send-bug-report]'.

----------------------------------------------------------------------

These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}---------------------------------------\
-------------------------------
"))

  (setq icicle-prompt-suffix
        (substitute-command-keys
         " (\\<minibuffer-local-completion-map>\\[icicle-apropos-complete], \
\\[icicle-prefix-complete]: list, \\[icicle-completion-help]: help) "))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-completion-help]' in minibuffer for help."))))

(defun icicle-remap (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

(defun icicle-unmap (command map current)
  "In MAP, unbind any keys that are bound to COMMAND.
If command remapping is available, remap COMMAND to nil in MAP,
unbinding it.
Otherwise, bind COMMAND to whatever CURRENT is bound to in MAP."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap command) nil)
    (substitute-key-definition current command map)))

(defun icicle-rebind-global (old new map)
  "Bind command NEW in MAP to all keys currently bound globally to OLD."
  (substitute-key-definition old new map (current-global-map)))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf separator-last] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf action-all]
        (list 'menu-item "Act On All Completions - Careful!" 'icicle-all-candidates-action
              :enable '(and icicle-mode icicle-candidate-action-fn)
              :help "Apply the command action to *each* of the possible completions"))
    (define-key map [menu-bar minibuf action-all]
      (cons "Act On All Completions - Careful!" 'icicle-all-candidates-action))
    (put 'icicle-all-candidates-action 'menu-enable
         '(and icicle-mode icicle-candidate-action-fn)))
  (define-key map [menu-bar minibuf separator-actions] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-define]
        (list 'menu-item "Define Completions Set by Lisp Sexp" 'icicle-candidate-set-define
              :help "Define the set of current completion candidates by evaluating a sexp"))
    (define-key map [menu-bar minibuf set-define]
      (cons "Define Completions Set by Lisp Sexp" 'icicle-candidate-set-define)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-swap]
        (list 'menu-item "Swap Saved and Current Completions Sets" 'icicle-candidate-set-swap
              :help "Swap the saved and current sets of completion candidates"))
    (define-key map [menu-bar minibuf set-swap]
      (cons "Swap Saved and Current Completions Sets" 'icicle-candidate-set-swap)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve-from-variable]
        (list 'menu-item "Retrieve Saved Completions Set From Variable"
              'icicle-candidate-set-retrieve-from-variable
              :help "Retrieve saved completion candidates from variable, making them current"))
    (define-key map [menu-bar minibuf set-retrieve-from-variable]
      (cons "Retrieve Saved Completions Set From Variable"
            'icicle-candidate-set-retrieve-from-variable)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save-to-variable]
        (list 'menu-item "Save Completions Set To Variable"
              'icicle-candidate-set-save-to-variable
              :help "Save current completion candidates to a variable, for later recall"))
    (define-key map [menu-bar minibuf set-save-to-variable]
      (cons "Save Completions Set To Variable" 'icicle-candidate-set-save-to-variable)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
        (list 'menu-item "Retrieve Saved Completions Set From Cache File"
              'icicle-candidate-set-retrieve-from-cache-file
              :help "Retrieve saved completion candidates from cache file, making them current"))
    (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
      (cons "Retrieve Saved Completions Set From Cache File"
            'icicle-candidate-set-retrieve-from-cache-file)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save-to-cache-file]
        (list 'menu-item "Save Completions Set To Cache File"
              'icicle-candidate-set-save-to-cache-file
              :help "Save current completion candidates to your cache file, for later recall"))
    (define-key map [menu-bar minibuf set-save-to-cache-file]
      (cons "Save Completions Set To Cache File" 'icicle-candidate-set-save-to-cache-file)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-retrieve]
        (list 'menu-item "Retrieve Saved Completions Set" 'icicle-candidate-set-retrieve
              :help "Retrieve the saved set of completion candidates, making it current"))
    (define-key map [menu-bar minibuf set-retrieve]
      (cons "Retrieve Saved Completions Set" 'icicle-candidate-set-retrieve)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-save]
        (list 'menu-item "Save Completions Set" 'icicle-candidate-set-save
              :help "Save the set of current completion candidates, for later recall"))
    (define-key map [menu-bar minibuf set-save]
      (cons "Save Completions Set" 'icicle-candidate-set-save)))
  (define-key map [menu-bar minibuf separator-set2] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-intersection]
        (list 'menu-item "Intersect Saved Completions Set" 'icicle-candidate-set-intersection
              :help "Set intersection between the current and saved candidates"))
    (define-key map [menu-bar minibuf set-intersection]
      (cons "Intersect Saved Completions Set" 'icicle-candidate-set-intersection)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-union]
        (list 'menu-item "Add Saved Completions Set" 'icicle-candidate-set-union
              :help "Set difference between the current and saved completion candidates"))
    (define-key map [menu-bar minibuf set-union]
      (cons "Add Saved Completions Set" 'icicle-candidate-set-union)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-difference]
        (list 'menu-item "Subtract Saved Completions Set" 'icicle-candidate-set-difference
              :help "Set difference between the current and saved completion candidates"))
    (define-key map [menu-bar minibuf set-difference]
      (cons "Subtract Saved Completions Set" 'icicle-candidate-set-difference)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf set-complement]
        (list 'menu-item "Complement Completions Set" 'icicle-candidate-set-complement
              :help "Complement the set of current completion candidates"))
    (define-key map [menu-bar minibuf set-complement]
      (cons "Complement Completions Set" 'icicle-candidate-set-complement)))
  (define-key map [menu-bar minibuf separator-set1] '("--"))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf word-complete]
        (list 'menu-item "Word-Complete" 'icicle-prefix-word-complete
              :help "Complete at most one word of prefix"))
    (define-key map [menu-bar minibuf word-complete]
      (cons "Word-Complete" 'icicle-prefix-word-complete)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf prefix-complete]
        (list 'menu-item "Prefix-Complete" 'icicle-prefix-complete
              :help "Complete prefix as far as possible"))
    (define-key map [menu-bar minibuf prefix-complete]
      (cons "Prefix-Complete" 'icicle-prefix-complete)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf apropos-complete]
        (list 'menu-item "Apropos-Complete" 'icicle-apropos-complete
              :help "Complete regular expression as far as possible and list completions"))
    (define-key map [menu-bar minibuf apropos-complete]
      (cons "Apropos-Complete" 'icicle-apropos-complete)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf quit]
        (list 'menu-item "Quit" 'icicle-abort-minibuffer-input
              :help "Cancel minibuffer input or recursive edit"))
    (define-key map [menu-bar minibuf quit] (cons "Quit" 'icicle-abort-minibuffer-input)))
  (if (> emacs-major-version 21)
      (define-key-after map [menu-bar minibuf completion-help]
        (list 'menu-item "Help" 'icicle-completion-help
              :help "Display help on minibuffer completion") [menu-bar minibuf ?\?])
    ;; Emacs 20: Cannot use `define-key-after' with multi-event key.
    (define-key map [menu-bar minibuf completion-help] (cons "Help" 'icicle-completion-help)))

  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument ; `C-u'
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument ; `M--'
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument ; `C-9'
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char ; `C-d', `deletechar'
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word ; `M-DEL'
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word ; `M-d'
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp ; `C-M-backspace'
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp ; `C-M-k', `C-M-delete'
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence ; `C-x DEL'
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph ; `C-backspace'
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph ; `C-delete'
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line ; `C-k', `delete', `deleteline'
                map (current-global-map))
  (icicle-remap 'kill-region                   'icicle-kill-region ; `C-w', `S-delete'
                map (current-global-map))
  (icicle-remap 'kill-region-wimpy             'icicle-kill-region-wimpy ; `C-w', `S-delete'
                map (current-global-map))
  (icicle-remap 'transpose-chars               'icicle-transpose-chars ; `C-t'
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words ; `M-t'
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps ; `C-M-t'
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop ; `M-y', `M-insert'
                map (current-global-map))

  ;; Rebind some global bindings for the completion maps.
  (unless icicle-cycling-respects-completion-mode-flag
    (icicle-rebind-global 'previous-line  'icicle-previous-prefix-candidate map) ; `down', `C-n'
    (icicle-rebind-global 'next-line      'icicle-next-prefix-candidate map)) ; `up', `C-p'
  (icicle-rebind-global 'scroll-down      'icicle-previous-apropos-candidate map) ; `prior', `M-v'
  (icicle-rebind-global 'scroll-up        'icicle-next-apropos-candidate map) ; `next', `C-v'
  (icicle-rebind-global 'backward-paragraph
                        'icicle-previous-prefix-candidate-action map) ; `C-up', `M-{'
  (icicle-rebind-global 'forward-paragraph
                        'icicle-next-prefix-candidate-action map) ; `C-down', `M-}'
  (icicle-rebind-global 'scroll-right
                        'icicle-previous-apropos-candidate-action map) ; `C-prior', `C-x >'
  (icicle-rebind-global 'scroll-left
                        'icicle-next-apropos-candidate-action map) ; `C-next', `C-x <'

  ;; Bind some additional keys.
  (define-key map icicle-word-completion-key 'icicle-prefix-word-complete)
  (define-key map [(control meta up)]        'icicle-help-on-previous-prefix-candidate) ; `C-M-up'
  (define-key map [(control meta down)]      'icicle-help-on-next-prefix-candidate) ; `C-M-down'
  (define-key map [(control meta prior)]  'icicle-help-on-previous-apropos-candidate) ; `C-M-prior'
  (define-key map [(control meta next)]      'icicle-help-on-next-apropos-candidate) ; `C-M-next'
  (define-key map [(control help)]           'icicle-help-on-candidate) ; `C-help'
  (define-key map [(control f1)]             'icicle-help-on-candidate) ; `C-f1'
  (define-key map [(control meta return)]    'icicle-help-on-candidate) ; `C-M-RET'
  (define-key map [(meta return)]            'icicle-candidate-read-fn-invoke) ; `M-RET'
  (define-key map [(control return)]         'icicle-candidate-action) ; `C-RET'
  (define-key map [(control ?o)]             'icicle-candidate-action) ; `C-o'
  (define-key map [(control ?!)]             'icicle-all-candidates-action) ; `C-!'
  (define-key map [S-iso-lefttab]            'icicle-apropos-complete) ; `S-TAB'
  (define-key map [S-tab]                    'icicle-apropos-complete) ; `S-TAB'
  (define-key map [S-M-C-iso-lefttab]        'icicle-apropos-complete-no-display) ; `S-M-C-TAB'
  (define-key map [S-M-C-tab]                'icicle-apropos-complete-no-display) ; `S-M-C-TAB'
  (define-key map [(control ?i)]             'icicle-prefix-complete) ; `TAB'
  (define-key map [tab]                      'icicle-prefix-complete) ; `TAB'
  (define-key map [(control meta ?/)]        'icicle-prefix-complete) ; `C-M-/', for `dabbrev.el'.
  (define-key map [(control meta tab)]       'icicle-prefix-complete-no-display) ; `C-M-TAB'
  (define-key map [(meta ?h)]                'icicle-history) ; `M-h'
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs) ; `M-pause'
  (define-key map [insert]                   'icicle-switch-to-Completions-buf) ; `insert'
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert) ; `?'
  (define-key map " "                        'icicle-self-insert) ; " "
  (define-key map [M-S-backspace]            'icicle-erase-minibuffer) ; `M-S-backspace'
  (define-key map [M-S-delete]               'icicle-erase-minibuffer) ; `M-S-delete'
  ;; Replaces `kill-sentence':
  (define-key map [(meta ?k)]                'icicle-erase-minibuffer-or-history-element) ; `M-k'
  (define-key map [(meta ?q)]                'icicle-insert-key-description) ; `M-q'
  (define-key map [(control ?g)]             'icicle-abort-minibuffer-input) ; `C-g'
  (define-key map [(control ?l)]             'icicle-retrieve-last-input) ; `C-l'
  (define-key map [(meta ?$)]                'icicle-candidate-set-truncate) ; `M-$'
  (define-key map [(control ?~)]             'icicle-candidate-set-complement) ; `C-~'
  (define-key map [(control ?-)]             'icicle-candidate-set-difference) ; `C--'
  (define-key map [(control ?+)]             'icicle-candidate-set-union) ; `C-+'
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection) ; `C-*'
  (define-key map [(control meta ?>)]        'icicle-candidate-set-save) ; `C-M->'
  (define-key map [(control meta ?<)]        'icicle-candidate-set-retrieve) ; `C-M-<'
  (define-key map [(control meta ?})]        'icicle-candidate-set-save-to-variable) ; `C-M-}'
  (define-key map [(control meta ?{)]       'icicle-candidate-set-retrieve-from-variable) ; `C-M-{'
  (define-key map [(control ?})]             'icicle-candidate-set-save-to-cache-file) ; `C-}'
  (define-key map [(control ?{)]            'icicle-candidate-set-retrieve-from-cache-file) ; `C-{'
  (define-key map [(control ?%)]             'icicle-candidate-set-swap) ; `C-%'
  (define-key map [(control ?:)]             'icicle-candidate-set-define) ; `C-:'
  (define-key map [(control ?=)]             'icicle-insert-string-from-variable) ; `C-='
  (define-key map [(control ?,)]             'icicle-toggle-sorting) ; `C-,'
  (define-key map [(control ?<)]             'icicle-toggle-angle-brackets) ; `C-<'
  (define-key map [(control ?$)]             'icicle-toggle-transforming) ; `C-$'
  (define-key map [(control ??)]             'icicle-completion-help) ; `C-?'
  (define-key map [(control ?.)]             'icicle-dispatch-C-.) ; `C-.'
  (define-key map [(control ?#)]             'icicle-toggle-incremental-completion) ; `C-#'
  (define-key map [(control ?^)]             'icicle-dispatch-C-^) ; `C-^'
  (define-key map [(control ?`)]             'icicle-toggle-regexp-quote) ; `C-`'
  (define-key map [(shift control ?a)]       'icicle-toggle-case-sensitivity) ; `S-C-a' (`C-A')
  (define-key map [(meta ?~)]                'icicle-toggle-~-for-home-dir) ; `M-~'
  (define-key map [(meta ?,)]                'icicle-toggle-alternative-sorting) ; `M-,'
  (define-key map [(meta ?.)]                'icicle-insert-string-at-point) ; `M-.'
  (define-key map [(meta ?*)]                'icicle-narrow-candidates) ; `M-*'
  (define-key map [(meta ?:)]                'icicle-pp-eval-expression)) ; `M-:'

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  (define-key map [menu-bar minibuf separator-last]    nil)
  (define-key map [menu-bar minibuf action-all]        nil)
  (define-key map [menu-bar minibuf separator-actions] nil)
  (define-key map [menu-bar minibuf set-define]        nil)
  (define-key map [menu-bar minibuf set-swap]          nil)
  (define-key map [menu-bar minibuf set-retrieve]      nil)
  (define-key map [menu-bar minibuf set-save]          nil)
  (define-key map [menu-bar minibuf separator-set2]    nil)
  (define-key map [menu-bar minibuf set-intersection]  nil)
  (define-key map [menu-bar minibuf set-union]         nil)
  (define-key map [menu-bar minibuf set-difference]    nil)
  (define-key map [menu-bar minibuf set-complement]    nil)
  (define-key map [menu-bar minibuf separator-set1]    nil)
  (define-key map [menu-bar minibuf word-complete]     nil)
  (define-key map [menu-bar minibuf prefix-complete]   nil)
  (define-key map [menu-bar minibuf apropos-complete]  nil)
  (define-key map [menu-bar minibuf quit]              nil)
  (define-key map [menu-bar minibuf completion-help]   nil)
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf ?\?]
        (list 'menu-item "List Completions" 'minibuffer-completion-help
              :help "Display all possible completions"))
    (define-key map [menu-bar minibuf ?\?]
      (cons "List Completions" 'minibuffer-completion-help)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf space]
        (list 'menu-item "Complete Word" 'minibuffer-complete-word
              :help "Complete at most one word"))
    (define-key map [menu-bar minibuf space]
      (cons "Complete Word" 'minibuffer-complete-word)))
  (if (> emacs-major-version 21)
      (define-key map [menu-bar minibuf tab]
        (list 'menu-item "Complete" 'minibuffer-complete :help "Complete as far as possible"))
    (define-key map [menu-bar minibuf tab] (cons "Complete" 'minibuffer-complete)))

  ;; Unmap commands that were bound for completion.
  (icicle-unmap 'self-insert-command           map 'icicle-self-insert)
  (icicle-unmap 'universal-argument            map 'icicle-universal-argument)
  (icicle-unmap 'negative-argument             map 'icicle-negative-argument)
  (icicle-unmap 'digit-argument                map 'icicle-digit-argument)
  (icicle-unmap 'backward-delete-char-untabify map 'icicle-backward-delete-char-untabify)
  (icicle-unmap 'delete-backward-char          map 'icicle-delete-backward-char)
  (icicle-unmap 'delete-char                   map 'icicle-delete-char)
  (icicle-unmap 'backward-kill-word            map 'icicle-backward-kill-word)
  (icicle-unmap 'kill-word                     map 'icicle-kill-word)
  (icicle-unmap 'backward-kill-sexp            map 'icicle-backward-kill-sexp)
  (icicle-unmap 'kill-sexp                     map 'icicle-kill-sexp)
  (icicle-unmap 'backward-kill-sentence        map 'icicle-backward-kill-sentence)
  (icicle-unmap 'backward-kill-paragraph       map 'icicle-backward-kill-paragraph)
  (icicle-unmap 'kill-paragraph                map 'icicle-kill-paragraph)
  (icicle-unmap 'kill-line                     map 'icicle-kill-line)
  (icicle-unmap 'kill-region                   map 'icicle-kill-region)
  (icicle-unmap 'kill-region-wimpy             map 'icicle-kill-region-wimpy)
  (icicle-unmap 'transpose-chars               map 'icicle-transpose-chars)
  (icicle-unmap 'transpose-words               map 'icicle-transpose-words)
  (icicle-unmap 'transpose-sexps               map 'icicle-transpose-sexps)
  (icicle-unmap 'yank-pop                      map 'icicle-yank-pop)

  ;; Restore commands that were bound for completion.
  (substitute-key-definition 'icicle-previous-prefix-candidate         'previous-line      map)
  (substitute-key-definition 'icicle-next-prefix-candidate             'next-line          map)
  (substitute-key-definition 'icicle-previous-apropos-candidate        'scroll-down        map)
  (substitute-key-definition 'icicle-next-apropos-candidate            'scroll-up          map)
  (substitute-key-definition 'icicle-previous-prefix-candidate-action  'backward-paragraph map)
  (substitute-key-definition 'icicle-next-prefix-candidate-action      'forward-paragraph  map)
  (substitute-key-definition 'icicle-previous-apropos-candidate-action 'scroll-right       map)
  (substitute-key-definition 'icicle-next-apropos-candidate-action     'scroll-left        map)

  ;; Restore additional bindings.
  (define-key map icicle-word-completion-key nil) ; Do first, so can be rebound, as needed.
  (define-key map [(control meta up)]        nil)
  (define-key map [(control meta down)]      nil)
  (define-key map [(control meta prior)]     nil)
  (define-key map [(control meta next)]      nil)
  (define-key map [(control help)]           nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control meta return)]    nil)
  (define-key map [(meta return)]            nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?o)]             nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [S-iso-lefttab]            nil)
  (define-key map [S-tab]                    nil)
  (define-key map [S-M-C-iso-lefttab]        nil)
  (define-key map [S-M-C-tab]                nil)
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map [(control meta ?/)]        nil)
  (define-key map [(control meta tab)]       nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [insert]                   nil)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map " "                        'minibuffer-complete-word)
  (define-key map [M-S-backspace]            nil)
  (define-key map [M-S-delete]               nil)
  (define-key map [(meta ?k)]                nil)
  (define-key map [(meta ?q)]                nil)
  (define-key map [(control ?g)]             'abort-recursive-edit)
  (define-key map [(control ?l)]             nil)
  (define-key map [(meta ?$)]                nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control meta ?>)]        nil)
  (define-key map [(control meta ?<)]        nil)
  (define-key map [(control meta ?})]        nil)
  (define-key map [(control meta ?{)]        nil)
  (define-key map [(control ?})]             nil)
  (define-key map [(control ?{)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control ?=)]             nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(control ?$)]             nil)
  (define-key map [(control ??)]             nil)
  (define-key map [(control ?.)]             nil)
  (define-key map [(control ?#)]             nil)
  (define-key map [(control ?^)]             nil)
  (define-key map [(control ?`)]             nil)
  (define-key map [(shift control ?a)]       nil)
  (define-key map [(meta ?~)]                nil)
  (define-key map [(meta ?,)]                nil)
  (define-key map [(meta ?.)]                nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map [(meta ?:)]                nil)
  (define-key map [(meta ?n)]                'next-history-element)
  (define-key map [down]                     'next-history-element)
  (define-key map [next]                     'next-history-element)
  (define-key map [(meta ?p)]                'previous-history-element)
  (define-key map [up]                       'previous-history-element)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [(meta ?v)]                'switch-to-completions))

;; Inspired from `icomplete-minibuffer-setup'.
;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
;;
(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icicle-mode (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
    ;; The pre- and post-command hooks are local to the
    ;; minibuffer, so they are added here, not in `icicle-mode'.
    ;; They are removed in `icicle-mode' when mode is exited.
    (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil t)
    (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil t)
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background (face-background 'region)))
    (when icicle-change-region-background-flag
      (set-face-background 'region icicle-region-background))
    ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
    ;; or `completing-read'.  Reset other stuff too.
    (setq icicle-prompt                         ""
          icicle-default-directory              default-directory
          icicle-incremental-completion-p       icicle-incremental-completion-flag
          icicle-pre-minibuffer-buffer          (cadr (buffer-list))
          icicle-initial-value                  nil
          icicle-last-completion-command        nil
          icicle-last-completion-candidate      nil
          icicle-last-input                     nil
          icicle-completion-candidates          nil
          icicle-candidate-nb                   nil
          icicle-current-completion-mode        nil)
    (when icicle-cycling-respects-completion-mode-flag
      (dolist (map (if (boundp 'minibuffer-local-filename-completion-map)
                       (list minibuffer-local-completion-map
                             minibuffer-local-filename-completion-map
                             minibuffer-local-must-match-map)
                     (list minibuffer-local-completion-map minibuffer-local-must-match-map)))
        (define-key map icicle-modal-cycle-up-key   'icicle-previous-candidate-per-mode)
        (define-key map icicle-modal-cycle-down-key 'icicle-next-candidate-per-mode)))
    (icicle-update-ignored-extensions-regexp)
    (when (memq icicle-init-value-flag '(preselect-start preselect-end))
      (icicle-select-minibuffer-contents))
    (when (and icicle-show-Completions-initially-flag (icicle-completing-p))
      ;;(icicle-display-Completions)) ;; $$ Removed 2006/07/19, to get initial highlighting.

      ;; Apropos completion, by default.
      (icicle-apropos-complete))         ; Defined in `icicles-cmd.el'.
    (run-hooks 'icicle-minibuffer-setup-hook)))

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (let ((min (icicle-minibuffer-prompt-end)))
    (set-mark (if (eq 'preselect-start icicle-init-value-flag) (point-max) min))
    (goto-char (if (eq 'preselect-start icicle-init-value-flag) min (point-max)))))

(defadvice next-history-element (after icicle-select-minibuffer-contents activate)
  "Select minibuffer contents and leave point at its beginning."
  (when (and icicle-mode (memq icicle-init-value-flag '(preselect-start preselect-end)))
    (icicle-select-minibuffer-contents)
    (setq deactivate-mark nil)))

(defun icicle-cancel-Help-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window (get-buffer-window "*Help*" t))
         (help-frame (and help-window (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p) ; Defined in `icicles-fn.el'.
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'"))
    ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
    ;; `completion-ignored-extensions' changes.
    (setq icicle-ignored-extensions completion-ignored-extensions)))

(defun icicle-completing-p ()
  "Non-nil if reading minibuffer input with completion."
  (and (active-minibuffer-window) (where-is-internal 'icicle-candidate-action nil 'first-only)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when icicle-change-region-background-flag
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (icicle-completing-p)
             (window-minibuffer-p (selected-window))
             (not executing-kbd-macro))
    (setq deactivate-mark nil)))

(defun icicle-redefine-standard-commands ()
  "Replace certain standard Emacs commands with Icicles versions."
  (when (and (fboundp 'icicle-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'icicle-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'icicle-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'icicle-repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'icicle-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'icicle-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'icicle-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'icicle-customize-apropos-options))
    (defalias 'read-from-minibuffer         (symbol-function 'icicle-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'icicle-read-string))))

(defun icicle-restore-standard-commands ()
  "Restore standard Emacs commands replaced in Icicle mode."
  (when (and (fboundp 'old-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'old-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'old-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'old-repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'old-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'old-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'old-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'old-customize-apropos-options))
    (defalias 'read-from-minibuffer         (symbol-function 'old-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'old-read-string))))

(defun icicle-redefine-std-completion-fns ()
  "Replace standard completion functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'icicle-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'icicle-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'icicle-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'icicle-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'icicle-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'icicle-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'icicle-completing-read))
    (defalias 'read-file-name               (symbol-function 'icicle-read-file-name))))

(defun icicle-restore-std-completion-fns ()
  "Restore standard completion functions replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'exit-minibuffer              (symbol-function 'old-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'old-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'old-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'old-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'old-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'old-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'old-completing-read))
    (defalias 'read-file-name               (symbol-function 'old-read-file-name))))

(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max        search-ring-max ; Save it.
          search-ring-max                     icicle-search-ring-max)
    (setq icicle-saved-regexp-search-ring-max regexp-search-ring-max ; Save it.
          regexp-search-ring-max              icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max)
    (setq icicle-saved-kmacro-ring-max        kmacro-ring-max ; Save it.
          kmacro-ring-max                     icicle-kmacro-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max        icicle-saved-search-ring-max)
    (setq regexp-search-ring-max icicle-saved-regexp-search-ring-max)))

;; This is used only in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))

(defun icicle-bind-isearch-keys ()
  "Bind `S-TAB' in Isearch maps.  Use in `isearch-mode-hook'."
  (define-key isearch-mode-map [S-tab] 'icicle-isearch-complete)
  (define-key minibuffer-local-isearch-map [S-tab] 'isearch-complete-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here
