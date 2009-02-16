;;; giguile.el -- tweaks for Guile editing and inferior processes

;; Copyright (C) 2002 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <nwv@neilvandyke.org>
;; Created:  04-Jan-2001
;; Version:  0.2
;; Keywords: scheme, lisp, guile, inferior process, cmuscheme, ilisp
;; X-URL:    http://www.neilvandyke.org/giguile/
;; X-CVS:    $Id: giguile.el,v 1.110 2002/01/10 04:50:34 nwv Exp $ UTC

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; About:
;;
;;   `giguile' is currently a very small package that tweaks the standard Emacs
;;   packages `scheme' (by Bill Rozas and Dave Love) and `cmuscheme' (by
;;   Olin Shivers) for use with Guile Scheme and inferior Guile processes.
;;   `giguile' interposes itself with little disruption to the other two
;;   packages, and they can continue to be used as before for other Scheme
;;   dialects.  The author's thinking is that the combination of these three
;;   packages might make for a starting point that is easier to tailor to Guile
;;   in the long-term than the venerable and big-boned ILISP package would be.
;;   
;;   Discussion of various other Emacs tools for editing Guile Scheme code and
;;   and interacting with inferior Guile processes can be found in EmacsWiki:
;;   http://www.emacswiki.org/cgi-bin/wiki.pl?GuileIde
;;
;;   The name `giguile' comes from the fact that Emacs idiom suggests that a
;;   package supporting an inferior Guile process be called `iguile', but
;;   claiming such a grand name for such a modest and derivative package seemed
;;   presumptuous.  So to name something considered inferior to "inferior," we
;;   will say "grossly inferior," and thus, `giguile'.

;; Requirements:
;;
;;   `giguile' is developed under Emacs 21.1 on a GNU/Linux system.  Please
;;   email the author feedback on compatibility problems with other Emacs
;;   versions and variants and OS platforms.
;;
;;   `giguile' should work with most or all versions of Guile, but it may start
;;   using features of the 1.6 release branch that is currently under
;;   development in Guile CVS (`http://savannah.gnu.org/cvs/?group_id=39').
;;   Email the author if you have special requests.

;; Installation:
;;
;;   Put the `giguile.el' file in one of your Emacs Lisp directories, and add
;;   the following line to your `~/.emacs':
;;
;;       (require 'giguile)
;;
;;   Advanced users may wish to load `giguile' on demand, such as with an
;;   `eval-after-load' on the `scheme' package.
;;
;;   After restarting Emacs, customize `giguile' package by selecting the
;;   "Giguile Options" submenu of the "Scheme Mode" menu, by executing `M-x
;;   giguile-customize RET', or by editing your `.emacs' file.

;; Author's To-Do List:
;;
;;   * When evaluating code from within a `scheme-mode' buffer, look for module
;;     forms to determine namespace context.
;;
;;   * In repl buffer, provide indicator of current namespace, and some kind of
;;     UI for changing.
;;
;;   * Do more for running multiple inferior Scheme processes at once, making
;;     sure we always handle buffers named other than `*scheme*' properly in
;;     window management, etc.
;;
;;   * Gradually do things that ILISP does for CL, in a Guile-optimized way.

;;; Change Log:

;; [Version 0.2, 09-Jan-2002, nwv@neilvandyke.org]
;; * Added fancy options menu (partly to figure out how to do dynamic menus in
;    Emacs for later, more useful, things).
;; * Added optional switch-to-scheme window management behaviors.
;; * Gratuitous renaming of various identifiers.

;; [Version 0.1, 06-Jan-2002, nwv@neilvandyke.org]
;; * First release.

;;; Code:

(defconst giguile-version "0.2")

(require 'advice)
(require 'custom)

(or (fboundp 'customize-save-variable)
    (autoload 'customize-save-variable "cus-edit"))

(eval-when-compile
  (defvar scheme-buffer))

;; Customization Variables:

(defgroup giguile nil
  "Tweaks for Guile editing and inferior processes."
  :group  'scheme
  :prefix "giguile-"
  :link   '(url-link "http://www.neilvandyke.org/giguile/"))

(defcustom giguile-autoload-p t
  "Defer loading the `scheme' and `cmuscheme' packages until necessary.
This causes the `scheme' and `cmuscheme' packages to be autoloaded on demand,
rather than requiring them to be loaded when the `giguile' package is.  Must be
set before loading `giguile'."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-modify-automodes-p t
  "Modify `auto-mode-alist' for Guile.
This will ensure that the `.scm' filename extension, which is conventional for
Guile Scheme code is set to `scheme-mode' in `auto-mode-alist'.    Must be set
before loading `giguile'."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-programs
  '("guile" "guile1.4" "guile1.5" "guile1.7" "scheme")
  "List of Scheme interpreter programs that can be used with `run-scheme'.
These names will be accessible via completion when `run-scheme' prompts for
which program to run."
  :group 'giguile
  :type  '(repeat string))

(defcustom giguile-default-program "guile"
  "Default Scheme interpreter program to use with `run-scheme'."
  :group 'giguile
  :type  'string)

(defcustom giguile-run-scheme-always-prompts-p t
  "`run-scheme' should always prompt for which program to run.
If nil, `run-scheme' will always use `giguile-default-program' when invoked
interactively without a prefix argument; this is closest to the behavior of the
`cmuscheme' package."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-run-scheme-prompt-defaults-to-last-p t
  "`run-scheme' prompt should default to the last program run."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-switch-to-scheme-method 'other-window
  "Method to use for choosing a window and frame for the process buffer.
One of three symbols:
`other-window' will split display in a different window in the current frame,
splitting the current window if necessary.
`own-frame' will display the process buffer in its own frame.
`cmuscheme' will use the normal behavior of the `cmuscheme' package."
  :group  'giguile
  :type   '(choice (const :tag "Other Window"       other-window)
                   (const :tag "Own Frame"          own-frame)
                   (const :tag "Cmuscheme Behavior" cmuscheme)))

(defcustom giguile-warp-pointer-to-frame-p t
  "Warp mouse pointer to frame with Scheme process buffer.
When `giguile-switch-to-scheme-method' is `own-frame', `switch-to-scheme' will
warp the mouse pointer to the frame displaying the Scheme process buffer."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-set-indent-properties-p t
  "Set Guile-specific `scheme-indent-function' properties.
Setting this to non-nil is helpful for Guile Scheme code and probably benign
for non-Guile Scheme code.  Must be set before loading `giguile'."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-create-option-menu-p t
  "Create a Giguile option submenu of the Scheme mode menu."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-options-persist-p t
  "Option menu settings and programs persist using the `custom' facility.
Note that the value of this option itself cannot be set persistently via the
option menu -- you must use the `customize' interface or set it manually in an
Emacs startup file.  This is by design, to avoid the risk of users accidentally
disabling their ability to set persistent options via the option menu."
  :group 'giguile
  :type  'boolean)

(defcustom giguile-remember-new-programs-p t
  "Programs are added to `giguile-programs' automatically."
  :group 'gigule
  :type  'boolean)

;; Non-Option Global Variables:

(defvar giguile-run-scheme-prompt-history '())

;; Package Dependencies:

(if giguile-autoload-p
    (progn (autoload 'run-scheme       "cmuscheme" nil t)
           (autoload 'scheme-mode      "scheme"    nil t)
           (autoload 'switch-to-scheme "cmuscheme" nil t))
  (require 'scheme)
  (require 'cmuscheme))

;; Functions:

(defun giguile-customize ()
  "Customize options for the `giguile' package."
  (interactive)
  (customize-group 'giguile))

(defun giguile-eval-after-load-scheme ()
  (if giguile-set-indent-properties-p
      (mapcar (function (lambda (n)
                          (put (car n) 'scheme-indent-function (cdr n))))
              '((catch . 1)
                (while . 1))))
  (if giguile-create-option-menu-p
      (giguile-optionmenu-define)))

(defun giguile-force-frame-switch-to-window (win)
  (let ((frame (window-frame win)))
    (unless (eq frame (selected-frame))
      (and window-system
           giguile-warp-pointer-to-frame-p
           (set-mouse-position frame 0 0))
      (select-frame frame))
    (select-window win)))

(defun giguile-forget-program (program)
  (setq giguile-programs (delete program giguile-programs))
  (giguile-option-set 'giguile-programs giguile-programs t)
  (message "Forgot program %S." program))

(defun giguile-sort-string-list-copy (lst)
  (sort (copy-sequence lst) 'string<))

(defun giguile-option-set (sym value &optional silently)
  ;; Clean up the value based on the option name.
  (cond ((eq sym 'giguile-programs)
         (setq value (giguile-sort-string-list-copy value))))
  ;; Set local binding and default binding.
  (set sym value)
  (when (local-variable-p sym)
    (message "Warning: %s is buffer-local, which is unsupported." sym)
    (set-default sym value))
  ;; Save option in `custom' facility if that feature is enabled.
  (and giguile-options-persist-p (customize-save-variable sym value))
  ;; Optionally display feedback in echo area.
  (or silently
      (message "Set %s%s to: %S"
               sym
               (if giguile-options-persist-p "" " (non-persistently)")
               value)))

(defun giguile-option-toggle (sym &optional silently)
  (giguile-option-set sym (not (symbol-value sym)) t)
  (or silently
      (message "Set %s%s %s."
               sym
               (if giguile-options-persist-p "" " (non-persistently)")
               (if (symbol-value sym) "ON" "OFF"))))

(defun giguile-optionmenu-define ()
  (let ((scheme-menu (lookup-key scheme-mode-map [menu-bar scheme]))
        (om          (make-sparse-keymap "Giguile Options"))
        (som         (make-sparse-keymap "Startup Options"))
        (stsm        (make-sparse-keymap "Switch-to-Scheme Method")))
    
    ;; Add to `scheme-mode' menu.
    (define-key-after scheme-menu [giguile-separator] '("--"))
    (define-key-after scheme-menu [giguile-options]
      (cons "Giguile Options" om))
    
    ;; Startup Options Menu.
    (define-key-after som [note-1] '("Changes below will not take effect"))
    (define-key-after som [note-2] '("until the next Emacs session"))
    (define-key-after som [separator] '("--"))
    (define-key-after som [toggle-awp]
      '(menu-item "Autoload When Possible" 
                  (lambda () (interactive)
                    (giguile-option-toggle 'giguile-autoload-p))
                  :button (:toggle . giguile-autoload-p)))
    (define-key-after som [toggle-mama]
      '(menu-item "Modify 'auto-mode-alist'" 
                  (lambda () (interactive)
                    (giguile-option-toggle 'giguile-modify-automodes-p))
                  :button (:toggle . giguile-modify-automodes-p)))
    (define-key-after som [toggle-sip]
      '(menu-item "Set Indent Properties"
                  (lambda () (interactive)
                    (giguile-option-toggle 'giguile-set-indent-properties-p))
                  :button (:toggle . giguile-set-indent-properties-p)))
    (define-key-after som [toggle-com]
      '(menu-item "Create Option Menu"
                  (lambda () (interactive)
                    (if (or (not giguile-create-option-menu-p)
                            (y-or-n-p (concat "Are you sure you want to "
                                              "disable the option menu? ")))
                        (giguile-option-toggle 'giguile-create-option-menu-p)))
                  :button (:toggle . giguile-create-option-menu-p)))

    ;; Switch-to-Scheme Method Menu.
    (define-key-after stsm [otherwindow]
      '(menu-item "Other Window" 
                  (lambda () (interactive)
                    (giguile-option-set 'giguile-switch-to-scheme-method
                                        'other-window))
                  :button (:radio . (eq giguile-switch-to-scheme-method
                                        'other-window))))
    (define-key-after stsm [ownframe]
      '(menu-item "Own Frame" 
                  (lambda () (interactive)
                    (giguile-option-set 'giguile-switch-to-scheme-method
                                        'own-frame))
                  :button (:radio . (eq giguile-switch-to-scheme-method
                                        'own-frame))))
    (define-key-after stsm [cmuscheme]
      '(menu-item "Cmuscheme Behavior" 
                  (lambda () (interactive)
                    (giguile-option-set 'giguile-switch-to-scheme-method
                                        'cmuscheme))
                  :button (:radio . (or (not giguile-switch-to-scheme-method)
                                        (eq giguile-switch-to-scheme-method
                                            'cmuscheme)))))
    (define-key-after stsm [toggle-wptf]
      '(menu-item "Warp Pointer to Frame" 
                  (lambda () (interactive)
                    (giguile-option-toggle 'giguile-warp-pointer-to-frame-p))
                  :button (:toggle . giguile-warp-pointer-to-frame-p)
                  :enable (eq giguile-switch-to-scheme-method 'own-frame)))
    
    ;; Options Menu.
    (when giguile-options-persist-p
      ;; If this variable is nil at the time we create the menu, then that
      ;; suggests the user went to some trouble to disable Giguile's use of the
      ;; `custom' facility for making options persistent, so we respect the
      ;; user's presumed wish and do not show these menu items at all.
      (define-key-after om [toggle-omsp]
        '(menu-item "Options Persist Across Sessions"
                    (lambda () (interactive)
                      (giguile-option-toggle 'giguile-options-persist-p))
                    :button (:toggle . giguile-options-persist-p)))
      (define-key-after om [startup-options]
        `(menu-item "Startup Options" ,som
                    :enable giguile-options-persist-p))
      (define-key-after om [separator-1] '("--")))
    (define-key-after om [toggle-apfp]
      '(menu-item "Always Prompt for Program"
                  (lambda () (interactive)
                    (giguile-option-toggle
                     'giguile-run-scheme-always-prompts-p))
                  :button (:toggle . giguile-run-scheme-always-prompts-p)))
    (define-key-after om [toggle-ppdtl]
      '(menu-item "Program Prompt Defaults to Last" 
                  (lambda () (interactive)
                    (giguile-option-toggle
                     'giguile-run-scheme-prompt-defaults-to-last-p))
                  :button
                  (:toggle . giguile-run-scheme-prompt-defaults-to-last-p)))
    (define-key-after om [toggle-auto-add]
      '(menu-item "Remember New Programs"
                     (lambda nil
                       (interactive)
                       (giguile-option-toggle
                        'giguile-remember-new-programs-p))
                     :button
                     (:toggle . giguile-set-indent-properties-p)))
    (define-key-after om [separator-2] '("--"))
    (define-key-after om [default-program]
      '(menu-item "Default Program" nil
                  :filter giguile-optionmenu-filter-defaultprogram))
    (define-key-after om [stsm]
      `(menu-item "Switch-to-Scheme Method" ,stsm))))

(defun giguile-optionmenu-filter-defaultprogram (real-binding)
  `(keymap "Default Program"
           ,@(giguile-optionmenu-items-setdefaultprogram)
           (separator-1 "--")
           (other menu-item "Other Program..."
                  (lambda () (interactive)
                    (let* ((minibuffer-allow-text-properties nil)
                           (program (giguile-without-side-whitespace
                                     (read-string "Other Default Program: "))))
                      (if (string= program "")
                          (message "Default program unchanged.")
                        (giguile-remember-program-maybe program)
                        (giguile-option-set 'giguile-default-program
                                            program))))
                  (nil))
           (separator-2 "--")
           (forget menu-item "Forget Program"
                   (keymap "Forget Program"
                           ,@(giguile-optionmenu-items-forgetprogram)))))

(defun giguile-optionmenu-items-forgetprogram ()
  (let ((programs giguile-programs)
        (count    0))
    (mapcar
     (function
      (lambda (program)
        (let* ((sym (intern (format "forget-%d" (setq count (1+ count))))))
          `(,sym
            menu-item
            ,(format "Forget  %s" program)
            (lambda () (interactive) (giguile-forget-program ,program))
            (nil)))))
     programs)))

(defun giguile-optionmenu-items-setdefaultprogram ()
  (let* ((programs      giguile-programs)
         (add-default-p (and giguile-default-program
                             (not (member giguile-default-program programs))))
         (count         0))
    (setq programs (giguile-sort-string-list-copy programs))
    (and add-default-p
         (setq programs (cons giguile-default-program programs)))
    (mapcar
     (function
      (lambda (program)
        (let* ((sym (intern (format "set-%d" (setq count (1+ count)))))
               (selected-p (and giguile-default-program
                                (equal program giguile-default-program))))
          `(,sym
            menu-item
            ,(format "%s%s"
                     program
                     (if (and add-default-p
                              (equal program giguile-default-program))
                         " (temporary)"
                       ""))
            (lambda () (interactive)
              (giguile-option-set 'giguile-default-program ,program))
            (nil)
            :button
            (:radio . ,selected-p)))))
     programs)))

(defun giguile-remember-program-maybe (program)
  (when (and giguile-remember-new-programs-p
             (not (member program giguile-programs)))
    (giguile-option-set 'giguile-programs (cons program giguile-programs) t)
    (message "Remembering program %S." program)))

(defun giguile-run-scheme-prompt ()
  (let* ((last    (car giguile-run-scheme-prompt-history))
         (default (or (and giguile-run-scheme-prompt-defaults-to-last-p
                           last)
                      giguile-default-program
                      scheme-program-name
                      last
                      "guile"))
         (program (let ((minibuffer-allow-text-properties nil))
                    (completing-read
                     (concat "Run Scheme"
                             (if default
                                 (format " (default %S)" default)
                               "")
                             ": ")
                     (giguile-run-scheme-prompt-completion-collection)
                     nil nil nil
                     'giguile-run-scheme-prompt-history
                     default))))
    (giguile-remember-program-maybe program)
    program))

(defun giguile-run-scheme-prompt-completion-collection ()
  (let ((program-list giguile-programs))
    (mapcar (function (lambda (program)
                        (and program
                             (not (member program program-list))
                             (setq program-list (cons program program-list)))))
            (list giguile-default-program
                  scheme-program-name))
    (mapcar (function (lambda (program) (cons program nil)))
            program-list)))

(defun giguile-without-side-whitespace (str)
  ;; Copied from `padr-str-trim-ws' by author.
  (save-match-data
    (if (string-match "^[ \t\n\r]+" str)
        (setq str (substring str (match-end 0))))
    (if (string-match "[ \t\n\r]+$" str)
        (setq str (substring str 0 (match-beginning 0))))
    str))

;; Advice:

(defadvice run-scheme (around giguile-ad-run first nil activate)
  "Adds prompting for which Scheme interpreter program to run."
  (interactive (list (if (or giguile-run-scheme-always-prompts-p
                              current-prefix-arg)
                         (giguile-run-scheme-prompt)
                       giguile-default-program)))
  ;; Since `run-scheme' calls `pop-to-buffer' rather than `switch-to-scheme',
  ;; our options for Scheme process window management, such as putting the
  ;; process buffer window in its own frame, do not take effect when the
  ;; process buffer is displayed by `run-scheme'.  So, unless we are using the
  ;; `cmuscheme' window management behavior, we attempt to undo whatever window
  ;; changes and buffer changes `run-scheme' makes, then just call
  ;; `switch-to-scheme'.  \(This code will be revisited once we decide how to
  ;; handle multiple Schemes, if not before then.\)
  (let ((buf (current-buffer))
        (wg  (current-window-configuration)))
    ad-do-it
    (unless (or (not giguile-switch-to-scheme-method)
                (eq giguile-switch-to-scheme-method 'cmuscheme))
      (set-window-configuration wg)
      (set-buffer buf)
      (switch-to-scheme t))))
      
(defadvice switch-to-scheme (before giguile-ad-switch last nil activate)
  "Adds support for the `giguile-switch-to-scheme-method' option."
  ;; This can be done as before-advice since the `pop-to-buffer' that
  ;; `switch-to-scheme' is using appears to always be a no-op when the target
  ;; buffer is already the current buffer.
  (require 'cmuscheme)
  ;; The `eval' below is to avoid problems with the byte-compiler and advising.
  ;; It doesn't seem to like: (and (boundp 'SYM) SYM)
  (let ((repl-buf (eval '(and (boundp 'scheme-buffer)
                              scheme-buffer
                              (get-buffer scheme-buffer)))))
    (cond ((not repl-buf)
           (error (concat "No process current buffer."
                          " Set `scheme-buffer' or execute `run-scheme'")))

          ((or (not giguile-switch-to-scheme-method)
               (eq giguile-switch-to-scheme-method 'cmuscheme))
           nil)

          ((eq (current-buffer) repl-buf) nil)
          
          ((eq giguile-switch-to-scheme-method 'other-window)
           (switch-to-buffer-other-window repl-buf))

          ;; The following code may be revived if anyone reports problems with
          ;; the use of `special-display-popup-frame'.
          ;;       
          ;; ((eq giguile-switch-to-scheme-method 'own-frame)
          ;;  (let ((pop-up-frames                t)
          ;;        (same-window-buffer-names     nil)
          ;;        (same-window-regexps          nil)
          ;;        (special-display-buffer-names nil)
          ;;        (special-display-regexps      nil))
          ;;    (switch-to-buffer (pop-to-buffer repl-buf))))
          
          ((eq giguile-switch-to-scheme-method 'own-frame)
           (giguile-force-frame-switch-to-window
            (special-display-popup-frame repl-buf)))

          (t (error "Invalid giguile-switch-to-scheme-method: %S"
                    giguile-switch-to-scheme-method)))))
          
;; Initialization:

(if giguile-modify-automodes-p
    (setq auto-mode-alist (cons (cons "\\.scm\\'" 'scheme-mode)
                                auto-mode-alist)))

(eval-after-load "scheme" '(giguile-eval-after-load-scheme))

(provide 'giguile)

;;; giguile.el ends here
