;;; help+.el --- Extensions to `help.el'.
;;
;; Filename: help+.el
;; Description: Extensions to `help.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2006, Drew Adams, all rights reserved.
;; Created: Tue Mar 16 14:18:11 1999
;; Version: 20.0
;; Last-Updated: Tue Jul 11 15:49:52 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 1560
;; URL: http://www.emacswiki.org/cgi-bin/wiki/help+.el
;; Keywords: help
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-cmds', `frame-fns', `info',
;;   `info+', `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help.el'.
;;
;;  Commands defined here:
;;
;;    `help-on-click/key', `mouse-help-on-click',
;;    `mouse-help-on-mode-line-click', `pop-to-help-toggle',
;;    `view-emacs-lisp-news', `save-*Help*-buffer'.
;;
;;  Non-interactive functions defined here:
;;
;;    `help-on-click/key-lookup', `remove-help-window'.
;;
;;  Internal variables defined here:
;;
;;    `help-origin-buffer'.
;;
;;
;;  ***** NOTE: The following functions defined in `help.el' have
;;              been REDEFINED HERE:
;;
;;  `describe-function', `describe-key', `describe-mode',
;;  `describe-project', `describe-variable', `help-with-tutorial',
;;  `locate-library', `view-emacs-FAQ', `view-emacs-news', `where-is'.
;;
;;
;;  ***** NOTE: The doc string for `help-for-help' has been
;;              REDEFINED HERE
;;              (see `make-help-screen help-for-help')
;;
;;  The following bindings are made here:
;;
;;    `C-h RET'        `help-on-click/key'
;;    `C-h o'          `edit-options'
;;    `C-h u'          `manual-entry'
;;    `C-h C-a'        `apropos'
;;    `C-h M-a'        `apropos-documentation'
;;    `C-h M-C-a'      `tags-apropos'
;;    `C-h C-l'        `locate-library'
;;    `C-h C-n'        `view-emacs-lisp-news'
;;    `C-h C-s'        `save-*Help*-buffer'
;;    [mouse-1]        `mouse-help-on-click' (non-mode-line)
;;    [mouse-1]        `mouse-help-on-mode-line-click' (mode-line)
;;
;;  Suggested additional binding:
;;
;;   (global-set-key [f1] 'help-on-click/key)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/07/11 dadams
;;     Added: help-origin-buffer, pop-to-help-toggle.  Bound latter to C-h C-o globally
;; 2005/10/31 dadams
;;     Use nil as init-value arg in calls to completing-read, everywhere.
;; 2001/01/10 dadams
;;     Protected remove-windows-on via fboundp.
;; 1999/08/12 dadams
;;     Added doc strings for help-on-click/key(-lookup) and updated header.
;; 1999/04/09 dadams
;;     help-on-click/key: Treat mouse click on symbol via apropos.
;; 1999/04/08 dadams
;;     help-on-click/key: 1. Treat mouse menus.
;;                        2. Corrected: flush extra mode-line mouse events.
;; 1999/04/08 dadams
;;     help-on-click/key: Bound temp-buffer-show-function so use other win.
;; 1999/04/08 dadams
;;     1. Added binding: help-on-click/key.
;;     2. Added: make-help-screen help-for-help.
;;     3. help-on-click/key-lookup: show-*Help*-buffer.
;;     4. help-on-click/key: Prompt.  Corrected: event->key, show-*Help*-buffer.
;; 1999/04/07 dadams
;;     1. Added: (replacement) describe-key.
;;     2. Added: help-on-click/key-lookup, help-on-click/key.
;; 1999/04/06 dadams
;;     Added binding for save-*Help*-buffer.
;; 1999/04/06 dadams
;;     1. Added some key bindings: o, u, C-l, C-a, M-a, C-M-a.
;;     2. Added: save-*Help*-buffer.
;; 1999/03/31 dadams
;;     Protected symbol-nearest-point with fboundp.
;; 1999/03/17 dadams
;;     1. Added: remove-help-window, help-with-tutorial, describe-project,
;;        view-emacs-FAQ, view-emacs-news, describe-function, describe-variable,
;;        where-is, locate-library, view-emacs-lisp-news.
;;     2. help-iso-prefix: Treat unbound iso-transl-char-map error.  Removed
;;        highlighting.
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

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; cadr when, unless

(require 'info nil t) ;; (no error if not found):
                      ;; Info-exit, Info-goto-node, Info-goto-emacs-key-command-node
(require 'info+ nil t) ;; (no error if not found):
                       ;; Info-goto-emacs-key-command-node (returns found-p)
(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point
(require 'frame-cmds nil t) ;; (no error if not found): remove-windows-on
(require 'frame-fns nil t) ;; (no error if not found): 1-window-frames-on

;; Get macro `make-help-screen' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile
  (require 'help-macro nil t) ;; (no error if not found) make-help-screen
  (require 'help-macro+ nil t)) ;; (no error if not found): make-help-screen


;;; You will get this message:
;;; While compiling describe-key:
;;;   ** describe-function-1 called with 2 arguments, but requires 3
;;; While compiling describe-function:
;;;   ** describe-function-1 called with 2 arguments, but requires 3

;;;;;;;;;;;;;;;;;;;;

(defvar help-origin-buffer nil "Buffer that we left, to go to *Help*.")

;;;###autoload
(define-key help-map [?\C-m] 'help-on-click/key) ; RET
;;;###autoload
(define-key help-map [?\C-n] 'view-emacs-lisp-news)
;;;###autoload
(define-key help-map "o" 'edit-options) ; in `options.el'
;;;###autoload
(define-key help-map "u" 'manual-entry) ; in `man.el'
;;;###autoload
(define-key help-map "\C-l" 'locate-library)
;;;###autoload
(define-key help-map "\C-o" 'pop-to-help-toggle)
;;;###autoload
(define-key help-map "\C-s" 'save-*Help*-buffer)
;;;###autoload
(define-key help-map "\C-a" 'apropos)
;;;###autoload
(define-key help-map "\M-a" 'apropos-documentation)
;;;###autoload
(define-key help-map "\M-\C-a" 'tags-apropos)
;;;###autoload
(define-key help-map [down-mouse-1] 'mouse-help-on-click)
;;;###autoload
(define-key help-map [mode-line down-mouse-1] 'mouse-help-on-mode-line-click)


(defsubst remove-help-window ()
  "If called from `help-for-help', remove display of help window."
  (when (and (eq 'help-for-help this-command)
             (fboundp 'remove-windows-on)) ; Defined in `frame-cmds.el'.
    (remove-windows-on "*Help*")))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages added.
;;;###autoload
(defun help-with-tutorial (&optional arg)
  "Select the Emacs learn-by-doing tutorial.
If there is a tutorial version written in the language
of the selected language environment, that version is used.
If there's no tutorial in that language, `TUTORIAL' is selected.
With prefix ARG, you are asked to choose which language."
  (interactive "P")
  (message "Looking for Emacs Tutorial file...")
  (let ((lang (if arg
		  (read-language-name 'tutorial "Language: " "English")
		(if (get-language-info current-language-environment 'tutorial)
		    current-language-environment
		  "English")))
	file filename)
    (setq filename (get-language-info lang 'tutorial))
    (setq file (expand-file-name (concat "~/" filename)))
    (delete-other-windows)
    (if (get-file-buffer file)
        (switch-to-buffer-other-window (get-file-buffer file))
      (switch-to-buffer-other-window (create-file-buffer file))
      (setq buffer-file-name file)
      (setq default-directory (expand-file-name "~/"))
      (setq buffer-auto-save-file-name nil)
      (insert-file-contents (expand-file-name filename data-directory))
      (goto-char (point-min))
      (search-forward "\n<<")
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (let ((n (- (window-height (selected-window))
                  (count-lines (point-min) (point))
                  6)))
        (if (< n 12)
            (newline n)
          ;; Some people get confused by the large gap.
          (newline (/ n 2))
          (insert "[Middle of page left blank for didactic purposes.  "
                  "Text continues below]")
          (newline (- n (/ n 2)))))
      (goto-char (point-min))
      (set-buffer-modified-p nil)))
  (remove-help-window)
  (message "Looking for Emacs Tutorial file...done"))


;; REPLACES ORIGINAL in `help.el':
;; Return nil if KEY is undefined; else return t.
(defun describe-key (key)
  "Describe the command that a keyboard/menu/mouse sequence invokes.
Argument KEY is a string.
Return nil if KEY is undefined; else return t."
  (interactive "kDescribe command bound to keyboard/menu/mouse sequence: ")
;;;@@@Emacs19  ;; If this key seq ends with a down event, discard the
;;;@@@Emacs19  ;; following click or drag event.  Otherwise that would
;;;@@@Emacs19  ;; erase the message.
;;;@@@Emacs19  (let ((type (aref key (1- (length key)))))
;;;@@@Emacs19    (when (listp type) (setq type (car type)))
;;;@@@Emacs19    (when (and (symbolp type) (memq 'down (event-modifiers type)))
;;;@@@Emacs19      (read-event)))
  (save-excursion
    (let ((modifiers (event-modifiers (aref key 0)))
	  window position)
      ;; For a mouse button event, go to the button it applies to
      ;; to get the right key bindings.  And go to the right place
      ;; in case the keymap depends on where you clicked.
      (if (or (memq 'click modifiers) (memq 'down modifiers)
	      (memq 'drag modifiers))
	  (setq window (posn-window (event-start (aref key 0)))
		position (posn-point (event-start (aref key 0)))))
      (if (windowp window)
	  (progn
	    (set-buffer (window-buffer window))
	    (goto-char position)))
      (let ((defn (key-binding key)))
        (cond ((or (null defn) (integerp defn))
               (message "`%s' is undefined." (key-description key))
               nil)                     ; Return nil: undefined.
              (t
               (with-output-to-temp-buffer "*Help*"
                 (princ (key-description key))
                 (if (windowp window)
                     (princ " at that spot"))
                 (princ " runs the command ")
                 (prin1 defn)
                 (princ "\n   which is ")
                 (if (>= emacs-major-version 20)
                     (describe-function-1 defn nil (interactive-p))
                   (describe-function-1 defn nil))
;;;@@@Emacs19                  (save-excursion
;;;@@@Emacs19                    (set-buffer standard-output)
;;;@@@Emacs19                    (help-mode))
                 (print-help-return-message))
               t))))))                  ; Return t: defined.


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages.
;;;###autoload
(defun describe-project ()
  "Display information on the GNU project."
  (interactive)
  (message "Looking for file describing GNU project...")
  (find-file-read-only-other-window (expand-file-name "GNU" data-directory))
  (remove-help-window)
  (message "Looking for file describing GNU project...done"))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Messages added.
;; 4. Turns off `auto-save-mode'.
;;;###autoload
(defun view-emacs-news (&optional arg)
  "Display information on recent changes to Emacs.
With numeric prefix ARG, display correspondingly older changes."
  (interactive "P")
  (message "Looking for Emacs Changes file...")
  (let* ((arg (if arg (prefix-numeric-value arg) 0)))
    (find-file-read-only-other-window
     (expand-file-name (concat (make-string arg ?O) "NEWS")
		       data-directory)))
    (auto-save-mode nil)                  ; Turn it off.
  (remove-help-window)
  (message "Looking for Emacs Changes file...done"))


;; REPLACES ORIGINAL in `help.el':
;; 1. Uses other window.
;; 2. Calls `remove-help-window'.
;; 3. Turns off `auto-save-mode'.
;;;###autoload
(defun view-emacs-FAQ ()
  "Display Frequently Asked Questions about Emacs (with answers)."
  (interactive)
  (message "Looking for Emacs FAQ file...")
  (find-file-read-only-other-window (expand-file-name "FAQ" data-directory))
  (auto-save-mode nil)                  ; Turn it off.
  (remove-help-window)
  (message "Looking for Emacs FAQ file...done"))


;; REPLACES ORIGINAL in `help.el':
;; Updated key bindings.
(make-help-screen help-for-help
  "RET [abcCfFhiIklLmnopstuvw] C-[acdfiklnpsw] M-a C-M-a (? for more help):"
  "This is the Emacs `help-command', accessible via `%THIS-KEY%'.
Type a Help option (below) now, for help on a particular topic.
Use \\<help-map>`\\[scroll-up]' or `\\[scroll-down]' to scroll this text.  \
Type `\\[help-quit]' to exit Help.
\(A \"command\" is any function that you can execute via `M-x'.)

COMMONLY USED
-------------
\\[help-on-click/key] `help-on-click/key': Help on any key sequence or anything you
    click with the mouse.
\\[describe-mode]   `describe-mode': Describes the current major and minor \
modes.
\\[describe-bindings]   `describe-bindings': Shows all current bindings \
\(mode-local and
    global): keyboard, menu bar, and mouse sequences.
\\[apropos-command]   `apropos-command': Gives commands that contain a \
given string.
    (see `\\[apropos]' and `\\[apropos-documentation]', below)
\\[describe-key-briefly]   `describe-key-briefly': Identifies the command \
bound to a given
    keyboard/menu/mouse sequence.  (see `\\[describe-key]', below)
\\[describe-key]   `describe-key': Describes the command that is bound to \
a given
    keyboard/menu/mouse sequence.
\\[Info-goto-emacs-key-command-node] `Info-goto-emacs-key-command-node': \
Shows the Info doc for the
    command bound to a given keyboard/menu/mouse sequence. (see `\\[info]')
\\[describe-function]   `describe-function': Shows the doc on an Emacs \
function.
\\[Info-goto-emacs-command-node] `Info-goto-emacs-command-node'.  Takes you \
to the Info doc node
    for a given command.  (see `\\[info]')
\\[info]   `info': Enters the Info hypertext documentation browser.
\\[view-lossage]   `view-lossage': Shows what you just did (last 100 keys/menus).

LEARNING EMACS
--------------
\\[help-with-tutorial]   `help-with-tutorial': Starts up a tutorial for \
learning Emacs.
\\[view-emacs-FAQ]   `view-emacs-FAQ': Explains Frequently Asked Emacs \
Questions.

MORE ADVANCED HELP
------------------
\\[edit-options]   `edit-options': Edit user options (vars), with values \
and doc.
\\[save-*Help*-buffer] `save-*Help*-buffer': Rename *Help* buffer as \
buffer *Help*<N>.
\\[describe-variable]   `describe-variable': Gives a variable's value & \
documentation.
\\[where-is]   `where-is': Identifies keyboard/menu/mouse sequences that \
invoke
    a given command.
\\[apropos] `apropos': Gives functions and variables containing a
    given string.  (see `\\[command-apropos]' and `\\[apropos-documentation]')
\\[apropos-documentation] `apropos-documentation': Gives fns and vars whose \
doc contains a
    given string. (see `\\[command-apropos]' and `\\[apropos]')
\\[tags-apropos] `tags-apropos'.  Shows the tags matched by a given string.
\\[describe-syntax]   `describe-syntax': Explains the current syntax table.
\\[manual-entry]   `manual-entry': Gives the Unix Manual entry for a given \
topic.
\\[finder-by-keyword]   `finder-by-keyword':  Finds Lisp libraries matching \
a topic.
\\[locate-library] `locate-library': Gives the path name of an Emacs Lisp \
library.
\\[view-emacs-news]   `view-emacs-news': Describes the latest Emacs changes.
\\[view-emacs-lisp-news] `view-emacs-lisp-news'; Describes latest Emacs \
Lisp changes.
\\[describe-copying] `describe-copying': Show GNU Emacs General Public License.
\\[describe-distribution] `describe-distribution': Show Emacs ordering information.
\\[describe-project] `describe-project': Show information about the GNU project.
\\[describe-no-warranty] `describe-no-warranty': Show information on absence of warranty.

INTERNATIONAL
-------------
\\[describe-coding-system]   `describe-coding-system': Describes a specific coding system
    (if you type its name) or the coding systems currently in use
    (if you type just RET).
\\[describe-input-method]   `describe-input-method' Describes a specific input method
    (if you type its name) or the current input method (if you type just RET).
\\[info-lookup-symbol]  `info-lookup-symbol' Displays the definition of a symbol
    as found in the manual for the language this buffer is written in.
\\[describe-language-environment]   `describe-language-environment' Describes a
    specific language environment (if you type its name)
    or the current language environment (if you type just RET).
h   Display the HELLO file which illustrates various scripts.
"
  help-map)


(or (fboundp 'old-describe-mode)
    (fset 'old-describe-mode (symbol-function 'describe-mode)))

;; REPLACES ORIGINAL in `help.el':
;; 1. Provides message telling how to change pages in *Help* buffer.
;; 2. Doc string also explains this.
;;    Really, the text at the beginning of *Help* should explain this - TO BE DONE.
;;;###autoload
(defun describe-mode ()
  "Display documentation of current major mode and minor modes.
Each mode (minor or major) is displayed on a different \"page\" in the
*Help* buffer (the pages are separated by `^L' characters).
You can change pages with `\\[forward-page]' and `\\[backward-page]'.

Note: For a minor mode to be described correctly here, the mode's
indicator variable (listed in `minor-mode-alist') must also be a
function whose documentation describes the minor mode."
  (interactive)
  (let ((font-lock-verbose nil));; This should inhibit msgs, but doesn't!
    (old-describe-mode)
    (message (substitute-command-keys
              "You can use `\\[forward-page]' and `\\[backward-page]' \
in *Help* buffer to change pages."))))


;; REPLACES ORIGINAL in `help.el':
;; Preferred candidate is `symbol-nearest-point'.
;;;###autoload
(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol).
FUNCTION names an Emacs Lisp function, possibly a user command.
Default candidate is: preferably the `symbol-nearest-point', or else
the innermost function call surrounding point
\(`function-called-at-point').
Return the description that was displayed, as a string."
  (interactive
   (let ((fn (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                 (function-called-at-point)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe function: " obarray 'fboundp t
                                    nil nil (and fn (symbol-name fn)) t)))))
  (unless (fboundp function)
    (error "Not a defined Emacs function: `%s'" function))
  (with-output-to-temp-buffer "*Help*"
    (prin1 function)
    ;; Use " is " instead of a colon so that
    ;; it is easier to get out the function name using forward-sexp.
    (princ " is ")
    (if (>= emacs-major-version 20)
        (describe-function-1 function nil (interactive-p))
      (describe-function-1 function nil))
    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
;;;@@@Emacs19       (help-mode)
      ;; Return the text we displayed.
      (buffer-string))))


;; REPLACES ORIGINAL in `help.el':
;; Preferred candidate is `symbol-nearest-point'.
;; Uses `substitute-command-keys' on doc string.
;;;###autoload
(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string."
  (interactive
   (let ((symb (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                   (and (symbolp (variable-at-point)))))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe variable: " obarray 'boundp t
                                    nil nil (and symb (symbol-name symb)) t)))))
  (unless (boundp variable)
    (error "Not a defined Emacs variable: `%s'" variable))
  (let (valvoid)
    (with-output-to-temp-buffer "*Help*"
      (prin1 variable)
      (if (not (boundp variable))
          (progn
            (princ " is void")
            (terpri)
            (setq valvoid t))
        (princ "'s value is ")
        (terpri)
        (pp (symbol-value variable))
        (terpri))
      (if (local-variable-p variable)
          (progn
            (princ (format "Local in buffer %s; " (buffer-name)))
            (if (not (default-boundp variable))
                (princ "globally void")
              (princ "global value is ")
              (terpri)
              (pp (default-value variable)))
            (terpri)))
      (terpri)
      (save-current-buffer
       (set-buffer standard-output)
       (if (> (count-lines (point-min) (point-max)) 10)
           (progn
             (goto-char (point-min))
             (if valvoid
                 (forward-line 1)
               (forward-sexp 1)
               (delete-region (point) (progn (end-of-line) (point)))
               (insert "'s value is shown below.\n\n")
               (save-excursion
                 (insert "\n\nValue:"))))))
      (princ "Documentation:")
      (terpri)
      (let ((doc (documentation-property variable 'variable-documentation)))
        (if (and doc (not (string= "" doc)))
            (princ (substitute-command-keys doc))
          (princ "Not documented as a variable.")))
      (when (>= emacs-major-version 20)
        (help-setup-xref (list #'describe-variable variable) (interactive-p))

        ;; Make a link to customize if this variable can be customized.
        ;; Note, it is not reliable to test only for a custom-type property
        ;; because those are only present after the var's definition
        ;; has been loaded.
        (if (or (get variable 'custom-type) ; after defcustom
                (get variable 'custom-loads) ; from loaddefs.el
                (get variable 'standard-value)) ; from cus-start.el
            (let ((customize-label "customize"))
              (terpri)
              (terpri)
              (princ (concat "You can " customize-label " this variable."))
              (with-current-buffer "*Help*"
                (save-excursion
                  (re-search-backward
                   (concat "\\(" customize-label "\\)") nil t)
                  (help-xref-button 1 #'(lambda (v)
                                          (customize-variable v)) variable)
                  ))))
        ;; Make a hyperlink to the library if appropriate.  (Don't
        ;; change the format of the buffer's initial line in case
        ;; anything expects the current format.)
        (when (string< "20.5" emacs-version)
          (let ((file-name (symbol-file variable)))
            (when file-name
              (princ "\n\nDefined in `")
              (princ file-name)
              (princ "'.")
              (with-current-buffer "*Help*"
                (save-excursion
                  (re-search-backward "`\\([^`']+\\)'" nil t)
                  (help-xref-button 1 (lambda (arg)
                                        (let ((location
                                               (find-variable-noselect arg)))
                                          (pop-to-buffer (car location))
                                          (goto-char (cdr location))))
                                    variable)))))))

      (print-help-return-message)
      (save-excursion
        (set-buffer standard-output)
;;;@@@Emacs19         (help-mode)
        ;; Return the text we displayed.
        (buffer-string)))))


;; REPLACES ORIGINAL in `help.el':
;; 1. Preferred candidate is `symbol-nearest-point'.
;; 2. Must be a command, not just a function.
;; 3. Calls `remove-help-window'.
;;;###autoload
(defun where-is (definition &optional insert)
  "Give keyboard/menu/mouse sequences that invoke specified command.
Argument DEFINITION is a command definition, usually a symbol with a
function definition.  Default candidate is: preferably the
`symbol-nearest-point', or else the innermost function call
surrounding point (`function-called-at-point').
Non-nil prefix arg INSERT means insert the message in the buffer."
  (interactive
   (let ((fn (or (and (fboundp 'symbol-nearest-point)(symbol-nearest-point))
                 (function-called-at-point)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Where is command: " obarray 'commandp t
                                    nil nil (and fn (symbol-name fn)) t)))))
  (remove-help-window)
  (let* ((keys (where-is-internal definition overriding-local-map nil nil))
         (keys1 (mapconcat 'key-description keys ", "))
	 (standard-output (if insert (current-buffer) t)))
    (if insert
	(if (> (length keys1) 0)
	    (princ (format "%s (%s)" keys1 definition))
	  (princ (format "M-x %s RET" definition)))
      (if (> (length keys1) 0)
	  (princ (format "`%s' is on `%s'" definition keys1))
	(princ (format "`%s' is not on any key" definition)))))
  nil)


;; REPLACES ORIGINAL in `help.el':
;; Calls `remove-help-window'.
;;;###autoload
(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the full path name of Emacs library LIBRARY.
This command searches the directories in your `load-path' like
`M-x load-library' to find the file that would be loaded by
`M-x load-library RET LIBRARY RET'.

Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc'
or `.el' to the specified name LIBRARY (like calling `load' instead of
`load-library').

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normaly returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (read-string "Locate library: ")
		     nil nil
		     t))
  (let (result)
    (catch 'answer
      (mapcar
       (lambda (dir)
         (mapcar (lambda (suf)
                   (let ((try (expand-file-name
                               (concat library suf)
                               dir)))
                     (when (and (file-readable-p try)
                                (null (file-directory-p try)))
                       (setq result try)
                       (message "Library is file `%s'." try)
                       (throw 'answer try))))

                 (if nosuffix
                     '("")
                   '(".elc" ".el" "")
;;; load doesn't handle this yet.
;;;	    (let ((basic '(".elc" ".el" ""))
;;;		  (compressed '(".Z" ".gz" "")))
;;;	      ;; If autocompression mode is on,
;;;	      ;; consider all combinations of library suffixes
;;;	      ;; and compression suffixes.
;;;	      (if (rassq 'jka-compr-handler file-name-handler-alist)
;;;		  (apply 'nconc
;;;			 (mapcar (lambda (compelt)
;;;				   (mapcar (lambda (baselt)
;;;					     (concat baselt compelt))
;;;					   basic))
;;;				 compressed))
;;;		basic))
                   )))
       (or path load-path)))
    (and interactive-call
	 (if result
	     (message "Library is file `%s'" result)
	   (message "No library `%s' in search path." library)))
    (remove-help-window)
    result))

;; Is there a direct way to get to the Lisp news in Emacs 21?
(when (< emacs-major-version 21)
  (defun view-emacs-lisp-news ()
    "Display information on recent changes to Emacs Lisp."
    (interactive)
    (message "Looking for Emacs Lisp Changes file...")
    (find-file-read-only-other-window (expand-file-name "LNEWS" data-directory))
    (auto-save-mode nil)                  ; Turn it off.
    (remove-help-window)
    (message "Looking for Emacs Lisp Changes file...done")))


;;;@@@Emacs19 (defvar iso-transl-char-map)		; To quiet byte compiler

;;;@@@Emacs19 ;;;###autoload
;;;@@@Emacs19 (defun help-iso-prefix ()
;;;@@@Emacs19   "Displays commands bound to ISO (pseudo-)prefix key sequences.
;;;@@@Emacs19 That is, displays a list of the alternatives that can follow the
;;;@@@Emacs19 ISO prefix key.
;;;@@@Emacs19
;;;@@@Emacs19 This is the same behavior that you get for the \"C-x\" prefix when you
;;;@@@Emacs19 type `C-x C-h'.  Whatever the ISO prefix key is now, it is treated the
;;;@@@Emacs19 same way by this command.  Normally, then, `help-iso-prefix' should be
;;;@@@Emacs19 bound to `KEY C-h', where KEY is the ISO prefix key."
;;;@@@Emacs19   (interactive)
;;;@@@Emacs19   (unless (boundp 'iso-transl-char-map)
;;;@@@Emacs19     (error "No ISO prefix key defined"))
;;;@@@Emacs19   (let* ((key (this-command-keys))
;;;@@@Emacs19          (prefix (make-vector (1- (length key)) nil))
;;;@@@Emacs19          (prefix-len (length prefix))
;;;@@@Emacs19          (i 0))
;;;@@@Emacs19     (while (< i prefix-len) (aset prefix i (aref key i)) (incf i))
;;;@@@Emacs19     (setq prefix (key-description prefix)) ; Reuse vars prefix & prefix-len.
;;;@@@Emacs19     (setq prefix-len (- 15 (length prefix)))
;;;@@@Emacs19     (with-output-to-temp-buffer "*Help*"
;;;@@@Emacs19       (princ "Global Bindings Starting With ")
;;;@@@Emacs19       (princ prefix) (princ ":") (terpri)
;;;@@@Emacs19       (princ "key             binding") (terpri)
;;;@@@Emacs19       (princ "---             -------") (terpri) (terpri)
;;;@@@Emacs19       (dolist (seq iso-transl-char-map)
;;;@@@Emacs19         (setq i 0) (princ prefix) (princ " ") (princ (car seq)) ; Reuse var i.
;;;@@@Emacs19         (while (< i (- prefix-len (length (car seq)))) (princ " ") (incf i))
;;;@@@Emacs19         (princ (key-description (cdr seq))) (terpri))))
;;;@@@Emacs19   (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))) ; In `frame-cmds.el'.


;;;###autoload
(defun save-*Help*-buffer ()
  "Rename *Help* buffer as new buffer *Help*<N>, N=2,3...."
  (interactive)
  (let ((notifying-user-of-mode nil)    ; No msg on mode (in `misc-fns.el').
        (saved-help (buffer-name (generate-new-buffer "*Help*"))))
    (save-excursion
      (set-buffer "*Help*")
      (copy-to-buffer saved-help (point-min) (point-max))
      (when (interactive-p)
        (message "Saved contents of *Help* buffer to buffer %s."
                 saved-help)))))


;;;###autoload
(defun help-on-click/key-lookup (key &optional pp-key where)
  "Look up information on KEY via `describe-key' and `info'.
Optional args PP-KEY and WHERE are strings naming KEY and its type.
Their defaults are KEY's `key-description' and \"Key sequence\".
Function `Info-goto-emacs-key-command-node' is used to look up KEY."
  (sit-for 0 200) ;; HACK to fix bug if click on scroll bar in `help-on-click/key'.
  (setq where (or where "Key sequence "))
  (setq pp-key (or pp-key (key-description key)))
  (let* ((described-p (describe-key key))
         ;; The version of `Info-goto-emacs-key-command-node' defined in `info+.el' will return
         ;; non-nil if Info doc is found.  The standard version defined `info.el' will not.
         (documented-p (Info-goto-emacs-key-command-node key))) ; NIL if only have std version.
    (when (and (not documented-p)(get-buffer-window "*info*" 'visible)) (Info-exit))
    (cond ((and described-p documented-p)
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in *Help* buffer; doc in *info* buffer."
                    where pp-key))
          (described-p
           (when (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
           (message "%s`%s': summary in *Help* buffer." where pp-key))
          (documented-p
           (message "%s`%s': doc in *info* buffer." where pp-key))
          (t
           (message "%s`%s' is undefined." where pp-key)))))


;;;###autoload
(defun help-on-click/key (key)
  "Give help on a key/menu sequence or object clicked with the mouse.
The object can be any part of an Emacs window or a name appearing in a
buffer.  You can do any of the following:

    type a key sequence (e.g. `C-M-s')
    choose a menu item (e.g. [menu-bar files open-file])
    click on a scroll bar
    click on the mode line
    click in the minibuffer
    click on an Emacs-related name in a buffer: apropos is called
    click anywhere else in a buffer: its modes are described

Help is generally provided using `describe-key' and the Emacs online
manual (via `Info-goto-emacs-key-command-node').  If no entry is found
in the index of the Emacs manual, then the manual is searched from the
beginning for literal occurrences of KEY.

For example, the KEY `C-g' is not in the index (for some reason), so
the manual is searched.  (Once an occurrence is found, you can
repeatedly type `s' in *Info* to search for additional occurrences.)

If you click on a name in a buffer, then `apropos-documentation' and
`apropos' are used to find information on the name.  These functions
are not used when you do something besides click on a name.

If you click elsewhere in a buffer other than the minibuffer, then
`describe-mode' is used to describe the buffer's current mode(s)."
  (interactive "kClick mouse on something or type a key sequence.")
  (let ((temp-buffer-show-function 'switch-to-buffer-other-window)
        (font-lock-verbose nil)
        (global-font-lock-mode nil))
    ;; DEBUG (message "KEY: `%s'" key)(sit-for 4) ; DEBUG
    (cond ((stringp key)
           (help-on-click/key-lookup key))
          (t                            ; Vector.
           (let ((type (aref key 0)))
             (cond ((or (symbolp type)(integerp type))
                    (cond ((eq 'mode-line type) ; Click on the mode line.
                           (Info-goto-node "(emacs)Mode Line")
                           (message "Mode line: decribed in *info* buffer."))
                          (t            ; Normal key sequence.
                           (help-on-click/key-lookup key))))
                   ((eq 'menu-bar (car type))

                    (help-on-click/key-lookup key (aref key (1- (length key))) "Menu item "))
                   ((not (eq 'down (car (event-modifiers (car type))))) ; e.g. mouse menus
                    (help-on-click/key-lookup key))
                   (t                   ; Mouse click.
                    (setq key type)
                    (cond ((window-minibuffer-p ; Click in minibuffer.
                            (posn-window (event-start key)))
                           (Info-goto-node "(emacs)Minibuffer")
                           (message "Minibuffer: decribed in *info* buffer."))
                          (t
                           (let ((symb (save-excursion (mouse-set-point key)
                                                       (symbol-at-point)))
                                 (apropos-do-all t)
                                 (found-doc nil)
                                 (found nil)
                                 (symb-regexp nil))
                             (cond (symb
                                    (message "Looking for info apropos `%s'..." symb)
                                    (when (get-buffer "*Apropos Doc*")
                                      (kill-buffer (get-buffer "*Apropos Doc*")))
                                    (setq found-doc
                                          (apropos-documentation
                                           (setq symb-regexp
                                                 (regexp-quote
                                                  (setq symb (format "%s" symb))))))
                                    (when found-doc
                                      (save-excursion
                                        (set-buffer (get-buffer "*Apropos*"))
                                        (rename-buffer "*Apropos Doc*"))
                                      (when (fboundp '1-window-frames-on) ; In `frame-fns.el'.
                                        (let ((frames (1-window-frames-on "*Apropos Doc*")))
                                          (while frames
                                            (save-window-excursion
                                              (select-frame (car frames))
                                              (rename-frame nil "*Apropos Doc*")
                                              (pop frames))))))
                                    (setq found (apropos symb-regexp))
                                    ;; Remove empty stuff.
                                    (setq found
                                          (and (consp found) (or (cdr found) (cadr found))))
                                    ;; Remove *Apropos* window that was displayed needlessly.
                                    (unless (or found (not (fboundp 'remove-windows-on)))
                                      (remove-windows-on "*Apropos*"))
                                    (cond
                                     ((and found-doc found)
                                      (message
                                       "See *Apropos* and *Apropos Doc* buffers."
                                       symb))
                                     (found
                                      (message
                                       "See information on `%s' in the *Apropos* buffer."
                                       symb))
                                     (found-doc
                                      (message
                                       "See information on `%s' in the *Apropos Doc* buffer."
                                       symb))
                                     (t
                                      (message
                                       "No information found regarding `%s'."
                                       symb))))
                                   (t   ; User clicked in buffer, but not on a symbol.
                                    (let ((bufname (buffer-name (current-buffer))))
                                      (describe-mode)
                                      (when
                                          (fboundp 'show-*Help*-buffer) (show-*Help*-buffer))
                                      (message
                                       "Mode(s) of buffer `%s' are described in *Help* buffer."
                                       bufname))))))))))))))

;;;###autoload
(defun mouse-help-on-click (event)
  "Give help on an object clicked with the mouse."
  (interactive "e")
  (help-on-click/key (vector event)))

;;;###autoload
(defun mouse-help-on-mode-line-click (event)
  "Give help on the mode line."
  (interactive "e")
  (help-on-click/key (vector 'mode-line event)))

;;;###autoload
(defun pop-to-help-toggle ()
  "Pop to buffer *Help* or back to the buffer that sent you to *Help*."
  (interactive)
  (let ((orig-buf (and (buffer-live-p help-origin-buffer)
                       (get-buffer help-origin-buffer)))
        (w32-grab-focus-on-raise   t)
        (win32-grab-focus-on-raise t))   ; Older name.
    (if (string-match "*Help*" (buffer-name))
        (cond ((not orig-buf)
               (error "No buffer to return to"))
              ((string-match "Minibuf" (buffer-name orig-buf)) ; No `minibufferp' in Emacs 20.
               (select-frame-set-input-focus
                (window-frame (select-window (minibuffer-window)))))
              (t
               (pop-to-buffer orig-buf)))
      (setq help-origin-buffer (current-buffer))
      (pop-to-buffer "*Help*"))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help+.el ends here
