;;; mcomplete.el --- minibuffer completion with prefix and substring matching

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 1.5 $
;; Keywords: local, help, abbrev

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This program borrows the ideas and the design of the following
;; programs:
;;
;;  icomplete.el --- minibuffer completion incremental feedback
;;  Copyright (C) 1992, 1993, 1994, 1997 Free Software Foundation, Inc.
;;  Author: Ken Manheimer <klm@python.org>
;;
;;  iswitchb.el --- switch between buffers using substrings
;;  Copyright (C) 1996, 1997  Free Software Foundation, Inc.
;;  Author: Stephen Eglen <stephen@anc.ed.ac.uk>
;;
;; All of these are distributed under GPL.
;; Thanks to the authors for writing these excellent programs.


;;; Commentary:

;; This package enhances Emacs's minibuffer completion mechanism.
;;
;; In short, this is icomplete.el + iswitchb.el +/- something.
;;
;; Features:
;;  * supports 2 completion methods (prefix and substring matching).
;;    Prefix matching is the Emacs's default completion method.
;;    Substring matching is a completion method where all the
;;    completion commands work in terms of a substring of the
;;    all possible completions.
;;    e.g.  "buffer" matches "backup-buffer", "buffer-name",
;;                                   ^^^^^^    ^^^^^^
;;          "exit-minibuffer", ...
;;                    ^^^^^^
;;
;;    You can cycle through the completion methods by `C-n' and
;;    `C-p' in the minibuffer.
;;
;;  * displays possible completion candidates in the minibuffer.
;;    e.g. When you enter `M-x apr', the minibuffer looks like
;;         the following:
;;         M-x apr[opos]{,-command,-documentation,-value,-zippy}
;;
;;  * `RET' in the minibuffer picks the first candidate displayed.
;;    e.g. `M-x apr RET' selects `apropos' command (and execute it).
;;
;;    You can cycle through the candidates by `C-s' and `C-r'.
;;
;;    When you want to give the exact string you entered, use `M-RET'
;;    or `ESC RET'.
;;
;;  * `C-c' in the minibuffer toggles case significance in completion.
;;
;;  * supports faces (highlights the display).
;;
;;  * supports customization per user command.


;;; Requirements:

;; Tested with GNU Emacs 20.7.2, GNU Emacs 21.0.91.1 and XEmacs 21.1.9.


;;; Compatibility:

;; complete.el
;;   tab, space, ... is used for mcomplete-mode.
;;   M-tab, M-SPACE, ... might work.
;;
;; completing-help.el (>= version 3.3)
;;   With this packege, short descriptions on completions is displayed
;;   by pressing `?'.
;;
;;   You can get completing-help.el at
;;   http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el
;;
;; icomplete.el
;;   icomplete-mode gets turned off when mcomplete-mode is activated.
;;
;; ido.el (http://hjem.get2net.dk/storm/emacs/)
;;   Both work. ido commands take precedence.
;;
;; iswitchb.el
;;   Both work. iswitchb commands take precedence.
;;


;;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/mcomplete.el <RET>'
;;    to speed up the execution of this package.
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'mcomplete-mode "mcomplete"
;;      "Toggle minibuffer completion with prefix and substring matching."
;;      t nil)
;;    (autoload 'turn-on-mcomplete-mode "mcomplete"
;;      "Turn on minibuffer completion with prefix and substring matching."
;;      t nil)
;;    (autoload 'turn-off-mcomplete-mode "mcomplete"
;;      "Turn off minibuffer completion with prefix and substring matching."
;;      t nil)
;;
;;    If you want to activate this package as you start Emacs,
;;    add the following line, too:
;;    (turn-on-mcomplete-mode)
;;
;; 4: Restart Emacs or enter `M-x load-library <RET> mcomplete'.


;;; Activation:

;; * Enter `M-x turn-on-mcomplete-mode' to activate this package.
;; * Enter `M-x turn-off-mcomplete-mode' to deactivate this package.


;;; Customization:

;; * Enter `M-x customize-group <RET> mcomplete' to customize this
;;   package.
;;   You might need to enter `M-x load-library <RET> mcomplete' in
;;   advance.
;;
;; * To control the mcomplete-mode's behavior for a specific command,
;;   write something like the following in your .emacs file.
;;   (put 'YOUR-COMMAND
;;        'mcomplete-mode
;;        '(:mode on
;;          ;; `on' means turning on `mcomplete-mode' temporarily for
;;          ;; your-command irrespective of the current state of
;;          ;; mcomplete-mode.
;;          ;; `off' means the reverse of the above.
;;          ;; `nil' or omitting the pair of :mode and its value means
;;          ;; respecting the current state of `mcomplete-mode'.
;;
;;          :method-set
;;          (mcomplete-substr-method   ; 1st matching method for your-command
;;           mcomplete-prefix-method)  ; 2nd matching method for your-command
;;          ;; `nil' or omitting the pair of :method-set and its value means
;;          ;; using the value of `mcomplete-default-method-set'
;;
;;          :exhibit-start-chars 2
;;          ;; Number of input characters to start to exhibit completion
;;          ;; information.
;;          ;; `nil' or omitting the pair of :exhibit-start-chars and its value
;;          ;; means using the value of `mcomplete-default-exhibit-start-chars'
;;
;;          :ignore-case on
;;          ;; `on' means case is not significant in completion.
;;          ;; `off' means case is significant in completion.
;;          ;; `nil' or omitting the pair of :ignore-case and its value means
;;          ;; using the value of `completion-ignore-case'.
;;         ))
;;
;;  Here's code for bookmark-jump a la iswitch-buffer.
;;  (put 'bookmark-jump
;;       'mcomplete-mode
;;       '(:method-set (mcomplete-substr-method mcomplete-prefix-method)
;;         :exhibit-start-chars 0))


;;; Distribution:

;; You can find the latest version of this package at:
;; http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el


;;; How this package works:

;; On loading this package,
;; `mcomplete-minibuffer-setup' is registered in `minibuffer-setup-hook'.
;;
;;
;; Some command tries to begin a minibuffer session
;;    |
;;    v
;; (run-hooks 'minibuffer-setup-hook) -+
;;    |                                |
;;    |                                v
;;    |           `mcomplete-minibuffer-setup'
;;    |              (when (mcomplete-p)
;;    |                 (run-hooks 'mcomplete-minibuffer-setup-hook))
;;    |                                |
;;    |                                v
;;    |           `mcomplete-setup-command-hooks'
;;    |              * registers `mcomplete-pre-command-hook' in
;;    |                the buffer local `pre-command-hook'.
;;    |              * registers `mcomplete-post-command-hook' in
;;    |                the buffer local `post-command-hook'.
;;    v
;; The minibuffer session begins
;;    |
;;    v
;; Some key is pressed in the minibuffer
;;    |
;;    v
;; (run-hooks 'pre-command-hook) -+
;;    |                           |
;;    |                           v
;;    |           (run-hooks 'mcomplete-pre-command-hook)
;;    |                           |
;;    |                           v
;;    |           `mcomplete-pre-command-tidy'
;;    |               * clears information display in the minibuffer.
;;    v
;; The command for the key is executed
;;    |
;;    v
;; (run-hooks 'post-command-hook) -+
;;                                 |
;;                                 v
;;                 (run-hooks 'mcomplete-post-command-hook)
;;                                 |
;;                                 v
;;                 `mcomplete-post-command-exhibit'
;;                    * displays information in the minibuffer.


;;; TODO:

;; * more doc.
;;
;; * M-x apr
;;     >> M-x apr[opos]{,-command,-documentation,-value,-zippy}
;;     C-s C-s
;;     >> M-x apr[opos]{-documentation,-value,-zippy,,-command}
;;     TAB
;;     >> M-x apropos{,-command,-documentation,-value,-zippy}
;;     should be
;;     >> M-x apropos{-documentation,-value,-zippy,,-command}
;;
;; * XEmacs only.
;;     M-x apropos C-a C-@ C-e
;;     >> M-x apropos-!-{,-command,-documentation,-value,-zippy}
;;        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;                         highlighted
;;     should be
;;     >> M-x apropos-!-{,-command,-documentation,-value,-zippy}
;;        ^^^^^^^^^^^
;;        highlighted
;;
;; * Allow more aggressive customization per command, per method.
;;   (put 'your-command
;;        'mcomplete-mode
;;        '(:method-set (mcomplete-substr-method mcomplete-prefix-method)
;;          :ignore-case on
;;          :substr-method (:ignore-case off :exhibit off)
;;          :prefix-method (:ignore-case on  :exhibit on)
;;         ))
;;
;; * Add more matching methods.
;;       + partial matching method a la complete.el.
;;       + regexp matching method
;;


;;; Change Log:

;; Version 1.5 (21 Aug 2004)
;;  * Apply a patch from J.D. Smith which changes the way completion candidates
;;    are sorted in subtring match method to combine the best of both
;;    prefix and substring completion methods. By this change, completion
;;    candidates which match the prefix are sorted to show up *before*
;;    candidates which match in the middle.
;;    So in substring match method, when you enter `M-x set-v', you get
;;    M-x set-v<set-variable, set-visited-file-name customize-set-value,...>
;;    Instead of
;;    M-x set-v<customize-set-value, set-variable, set-visited-file-name,...>
;;    Any comments are welcome about this change.

;; Version 0.20 (18 Dec 2003)
;;  * To get along with howm-1.1, apply patch from HIRAOKA Kazuyuki <hira@ics.saitama-u.ac.jp>
;;  * Correct doc error in the last line of Activation section
;;    (`M-x turn-on-mcomplete-mode' should be `M-x turn-off-mcomplete-mode'),
;;    which Stephen Eglen reported.

;; Version 0.18 (28 Oct 2001)
;;  * Use `minibuffer-prompt-end' if available for Emacs 21
;;  * Avoid "FSF Emacs" and use "GNU Emacs" since that seems more appropriate.

;;  * 
;;


;;; Code:
(eval-when-compile
  (defvar obarray)
  (defvar deactivate-mark)
  (defvar buffer-undo-list)
  (defvar quit-flag)
  (defvar unread-command-events)
  (defvar this-command)
  (defvar last-command)
  (defvar executing-kbd-macro)
  (defvar minibuffer-local-completion-map)
  (defvar minibuffer-local-must-match-map)
  (defvar minibuffer-completion-table)
  (defvar minibuffer-completion-predicate)
  (defvar minibuffer-completion-confirm)
  (defvar minibuffer-scroll-window)
  (defvar completion-auto-help)
  (defvar completion-ignore-case)
  (defvar minibuffer-setup-hook)
  (defvar pre-command-hook)
  (defvar post-command-hook))


;;; Customization group
(defgroup mcomplete nil
  "This package enhances Emacs's minibuffer completion mechanism.

In short, this is icomplete.el + iswitchb.el +/- something.

Features:
 * supports 2 completion methods.
   Prefix matching is the Emacs's default completion method.
   Substring matching is a completion method where all the
   completion commands work in terms of a substring of the
   all possible completions.
   e.g.  \"buffer\" matches \"backup-buffer\", \"buffer-name\",
         \"exit-minibuffer\", ...

   You can toggle the completion methods by `C-n' and `C-p' in
   the minibuffer.

 * displays possible completion candidates in the minibuffer.
   e.g. When you enter `M-x apr', the minibuffer looks like
        the following:
        M-x apr[opos]{,-command,-documentation,-value,-zippy}

 * `RET' in the minibuffer picks the first candidate displayed.
   e.g. `M-x apr RET' selects `apropos' command (and execute it).

   You can cycle through the candidates by `C-s' and `C-r'.

   When you want to give the exact string you entered to the command,
   use `M-RET' or `ESC RET'.

 * supports faces (highlights the display)."
  :group 'minibuffer)

(defcustom mcomplete-mode nil
  "Toggle minibuffer completion with prefix and substring matching.
Setting this variable directly does not take effect;
use either \\[customize] or the commands `mcomplete-mode',
`turn-on-mcomplete-mode', and `turn-off-mcomplete-mode'."
  :set        #'(lambda (symbol value) (mcomplete-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type       'boolean
  :group      'mcomplete
  :require    'mcomplete)


;;; Mode switch
(eval-when-compile
  (defvar icomplete-mode)
  (autoload 'icomplete-mode "icomplete"))


;;;###autoload
(defun mcomplete-mode (&optional arg)
  "Toggle minibuffer completion with prefix and substring matching.
With ARG, turn the mode on if ARG is positive, off otherwise."
  (interactive "P")
  (setq mcomplete-mode (if (null arg)
                           (not mcomplete-mode)
                         (> (prefix-numeric-value arg) 0)))
  (when mcomplete-mode
    (when (and (boundp 'icomplete-mode) icomplete-mode)
      (icomplete-mode -1)))
  (when (interactive-p)
    (message "mcomplete-mode %s" (if mcomplete-mode "enabled" "disabled"))))

;;;###autoload
(defun turn-on-mcomplete-mode ()
  "Turn on minibuffer completion with prefix and substring matching."
  (interactive)
  (mcomplete-mode 1))

;;;###autoload
(defun turn-off-mcomplete-mode ()
  "Turn off minibuffer completion with prefix and substring matching."
  (interactive)
  (mcomplete-mode -1))


;;; Utilities
(defun mcomplete-rotate-list (list count)
  "Rotate LIST COUNT times destructively.
\(mcomplete-rotate-list (list 1 2 3) 1) => (2 3 1).
\(mcomplete-rotate-list (list 1 2 3) -1) => (3 1 2)."
  (when list
    (let* ((len (length list))
           (count (mod count len))
           new-top new-last)
      (if (zerop count)
          list
        (setq new-last (nthcdr (1- count) list)
              new-top  (cdr new-last))
        (setcdr (last new-top) list)
        (setcdr new-last nil)
        new-top))))


;;; Completion methods
(defvar mcomplete-method-default-plist) ; defined later

(defcustom mcomplete-default-method-set
  '(mcomplete-prefix-method mcomplete-substr-method)
  "List of completion methods.  The first method is applied first."
  :type  '(repeat function)
  :group 'mcomplete)

(defvar mcomplete-current-method-set
  nil
  "List of completion method symbols for the current minibuffer.
Users can activate these methods during a minibuffer session.")
(make-variable-buffer-local 'mcomplete-current-method-set)

(defun mcomplete-current-method ()
  "Return the completion method symbol for the current minibuffer."
  (car mcomplete-current-method-set))

(defun mcomplete-get (property &optional method)
  "Return PROPERTY of completion METHOD (default current method)."
  (unless method (setq method (mcomplete-current-method)))
  (if (memq property (symbol-value method))
      (plist-get (symbol-value method) property)
    (plist-get mcomplete-method-default-plist property)))


(defvar mcomplete-display-current-method-name-sec 0.5
  "Number of seconds to display the current method name when it's changed.")

(defun mcomplete-display-current-method-name ()
  "Display the current method name in the minibuffer."
  (unless (input-pending-p)
    (let ((name (mcomplete-get :name))
          (sec  mcomplete-display-current-method-name-sec))
      (unless (stringp name)
        (error "mcomplete: name not defined for method %s"
               (mcomplete-current-method)))
      (mcomplete-message (format " [%s]" name) sec))))


;;; minibuffer utilities
(defun mcomplete-prompt-end ()
  (if (fboundp 'minibuffer-prompt-end)
      (minibuffer-prompt-end)
      (point-min)))

(defsubst mcomplete-minibuffer-string ()
  (buffer-substring (mcomplete-prompt-end) (point-max)))

(defsubst mcomplete-clear-minibuffer ()
  (delete-region (mcomplete-prompt-end) (point-max)))


(defun mcomplete-message (str &optional sec)
  "Display STR at the end of the minibuffer for SEC (default 2) seconds.
The minibuffer must be the current buffer.
Stop displaying when the next input event arrives.
Work almost the same as `minibuffer-message'."
  (unless sec (setq sec 2))
  (let ((buffer-undo-list t)            ; prevent undo recording
        (pt-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (message nil)
      (goto-char (point-max))
      (insert str))
    (sit-for sec)
    (delete-region pt-max (point-max))
    (when quit-flag
      (let ((quit-char (if (fboundp 'current-input-mode)
                           (nth 3 (current-input-mode))
                         ?\^G))
            (char-to-event (if (fboundp 'character-to-event)
                               'character-to-event
                             'identity)))
        (cond
         ;; check new features first
         ((boundp 'unread-command-events)
          (setq unread-command-events (list (funcall char-to-event quit-char))
                quit-flag nil))
         ;;;((boundp 'unread-command-event)
         ;;; (setq unread-command-event (funcall char-to-event quit-char)
         ;;;       quit-flag nil))
         ;;;
         ;;;((boundp 'unread-command-char)
         ;;; (setq unread-command-char quit-char
         ;;;       quit-flag nil))
         (t
          (error "mcomplete-message: %S is not bound."
                 'unread-command-events)))))))

;;; Minibuffer setup hook
(defun mcomplete-p ()
  "Return non-nil if `mcomplete-mode' is applicable to the current minibuffer."
  (and (symbolp this-command)
       (let ((mode (plist-get (get this-command 'mcomplete-mode) :mode)))
         (cond ((eq mode 'on)  t)
               ((eq mode 'off) nil)
               ((eq mode 'nil) mcomplete-mode)))
       (window-minibuffer-p (selected-window))
       (not executing-kbd-macro)
       (not (functionp minibuffer-completion-table))
       minibuffer-completion-table))

(defvar mcomplete-minibuffer-setup-hook nil
  "`minibuffer-setup-hook' hook for `mcomplete-mode'.")

(defun mcomplete-minibuffer-setup ()
  "Run `mcomplete-minibuffer-setup-hook' if `mcomplete-mode' is active."
  (when (mcomplete-p)
    (run-hooks 'mcomplete-minibuffer-setup-hook)
    (when (featurep 'xemacs)
      ;; needed when mcomplete-exhibit-start-chars is 0
      (run-hooks (mcomplete-get :post-command-hook)))))

(add-hook 'minibuffer-setup-hook 'mcomplete-minibuffer-setup)


;;; *Completions* buffer
(defadvice choose-completion-delete-max-match
  ;; The original function is defined in GNU Emacs's simple.el,
  ;; and XEmacs's list-mode.el.
  ;; Actually, this advise is not necessary for GNU Emacs.
  ;; This function is called when a user press RET or clicks a mouse button
  ;; in the "*Completions*" buffer.
  (around mcomplete last activate compile preactivate)
  "Delete appropriate piece of input string in the current minibuffer."
  (if (mcomplete-p)
      (delete-region (mcomplete-prompt-end) (point-max))
    ad-do-it))


;;; Key maps
(defcustom mcomplete-permissive-completion-map-alist
  '(("\t"   . MComplete-complete)
    (" "    . MComplete-complete-word)
    ("?"    . MComplete-completion-help)
    ("\r"   . MComplete-exit-minibuffer)
    ("\e\r" . exit-minibuffer)
    ("\n"   . MComplete-exit-minibuffer)
    ("\e\n" . exit-minibuffer)
    ("\C-c" . MComplete-toggle-ignore-case)
    ("\C-n" . MComplete-next-method)
    ("\C-p" . MComplete-previous-method)
    ("\C-s" . MComplete-next-candidate)
    ("\C-r" . MComplete-previous-candidate))
  "Alist of key bindings to override `minibuffer-local-completion-map'.
These bindings are used when an exact match is NOT required."
  :type  '(repeat
           (cons (choice string
                         (restricted-sexp :match-alternatives (vectorp)))
                 ;; the vector designator can be more elaborate (using
                 ;; :inline) but I found they were harder to input in
                 ;; the customization buffer.
                 function))
  :group 'mcomplete)

(defcustom mcomplete-must-match-completion-map-alist
  '(("\t"   . MComplete-complete)
    (" "    . MComplete-complete-word)
    ("?"    . MComplete-completion-help)
    ("\r"   . MComplete-complete-and-exit)
    ("\n"   . MComplete-complete-and-exit)
    ("\C-c" . MComplete-toggle-ignore-case)
    ("\C-n" . MComplete-next-method)
    ("\C-p" . MComplete-previous-method)
    ("\C-s" . MComplete-next-candidate)
    ("\C-r" . MComplete-previous-candidate))
  "Alist of key bindings to override `minibuffer-local-must-match-map'.
These bindings are used when an exact match is required."
  :type  '(repeat
           (cons (choice string
                         (restricted-sexp :match-alternatives (vectorp)))
                 function))
  :group 'mcomplete)

(defun mcomplete-setup-local-keymap ()
  "Setup local keymap for `mcomplete-mode'."
  (let* ((old-map (current-local-map))
         (new-map (copy-keymap old-map))
         (alist   (if (eq (lookup-key old-map "\n")
                          (lookup-key minibuffer-local-completion-map "\n"))
                      ;; For GNU Emacs, we can simply get away with (eq old-map
                      ;; minibuffer-local-completion-map), but XEmacs makes
                      ;; a copy and defines a help-key binding in it.
                      mcomplete-permissive-completion-map-alist
                    mcomplete-must-match-completion-map-alist)))
    (use-local-map new-map)
    (mapcar #'(lambda (assoc) (define-key new-map (car assoc) (cdr assoc)))
            alist)))
  
(add-hook 'mcomplete-minibuffer-setup-hook 'mcomplete-setup-local-keymap)


;;; Pre/Post-Command hooks
(defun mcomplete-setup-command-hooks ()
  "Setup `pre-command-hook' and `post-command-hook' for `mcomplete-mode'."
  ;; setup PRE-COMMAND-HOOK
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook
            #'(lambda () (run-hooks (mcomplete-get :pre-command-hook)))
            nil                         ; nil means prepend
            t)                          ; t means a local hook

  ;; setup POST-COMMAND-HOOK
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook
            #'(lambda () (run-hooks (mcomplete-get :post-command-hook)))
            nil                         ; nil means prepend
            t))                         ; t means a local hook

(add-hook 'mcomplete-minibuffer-setup-hook 'mcomplete-setup-command-hooks)


(defvar mcomplete-input ""
  "Minibuffer contents without completion information.")
(make-variable-buffer-local 'mcomplete-input)

(defvar mcomplete-pre-command-hook nil
  "`pre-command-hook' for `mcomplete-mode'.")

(defun mcomplete-pre-command-tidy ()
  "Delete completion information in the minibuffer."
  (let ((buffer-undo-list t)      ; prevent undo recording
        (deactivate-mark nil))   ; protect the original value
    (delete-region (+ (mcomplete-prompt-end) (length mcomplete-input))
                   (point-max))
    (remove-text-properties (mcomplete-prompt-end) (point-max) '(face nil))
    ;(set-buffer-modified-p nil)         ; clear modified flag
    ))
(add-hook 'mcomplete-pre-command-hook 'mcomplete-pre-command-tidy)

(defvar mcomplete-post-command-hook nil
  "`post-command-hook' for `mcomplete-mode'.")

(defcustom mcomplete-default-exhibit-start-chars 1
  "Number of input characters to start to exhibit completion information."
  :type  'integer
  :group 'mcomplete)

(defvar mcomplete-exhibit-start-chars nil
  "Current number of input characters to start to exhibit information.")

(defun mcomplete-post-command-exhibit ()
  "Exhibit completion information."
  (setq mcomplete-input (mcomplete-minibuffer-string))
  (when (and (not (input-pending-p))
             (>= (length (mcomplete-minibuffer-string))
                 mcomplete-exhibit-start-chars))
    (catch 'input-pending
      (save-match-data
        (save-excursion
          (let* ((buffer-undo-list t)   ; prevent undo recording
                 (deactivate-mark nil)  ; protect the original value
                 (str (mcomplete-minibuffer-string)))
            (goto-char (point-max))
                   
            (funcall (mcomplete-get :exhibit)
                     str
                     (mcomplete-all-completions str "abort on input")
                     (mcomplete-try-completion  str "abort on input"))
            nil))))))

(add-hook 'mcomplete-post-command-hook 'mcomplete-post-command-exhibit)


;;; Method enter/leave hooks
(defvar mcomplete-method-enter-hook nil
  "Hook run when entering a completion method.")
(add-hook 'mcomplete-method-enter-hook 'mcomplete-display-current-method-name)

(defvar mcomplete-method-leave-hook nil
  "Hook run when leaving a completion method.")



;;; completion core functions
(defvar mcomplete-ignore-case nil
  "Non-nil means case is not considered significant in completion.")
(make-variable-buffer-local 'mcomplete-ignore-case)


(defun mcomplete-exact-match-p (str table &optional pred)
  "Return t if an exact match for STR, satisfying PRED, exists in TABLE."
  (let ((ignore-case mcomplete-ignore-case))
    (cond
     ((listp table)                     ; alist or nil
      (let ((assoc (funcall (if ignore-case 'assoc-ignore-case 'assoc)
                            str table)))
        (and assoc (or (null pred) (funcall pred assoc)))))

     ((vectorp table)                   ; obarray
      (if (and (or (string= str "nil")
                   (and ignore-case (string= (downcase str) "nil")))
               (eq table obarray))
          t                             ; We catch the `nil' here
        (let* ((lowercase-str (downcase str))
               (symbol (if (not ignore-case)
                           (intern-soft str table)
                         (catch 'found
                           (mapatoms
                            #'(lambda (s)
                                (when (string= lowercase-str
                                               (downcase (symbol-name s)))
                                  (throw 'found s)))
                            table)))))
          (and symbol (or (null pred) (funcall pred symbol))))))
     
     (t                                 ; programmed completion
      (funcall table str pred 'lambda)))))


(defun mcomplete-predicate-with-input-check (arg)
  "Throw signal if input is pending, or call `minibuffer-completion-predicate'."
  (cond
   ((input-pending-p)
    (throw 'input-pending t))
   (minibuffer-completion-predicate
    (funcall minibuffer-completion-predicate arg))
   (t t)))


;; caching facility
(defun mcomplete-with-cache (func cache-var method str abort-on-input)
  "Call FUNC chaching the result."
  (let ((cache (symbol-value cache-var))
        (table minibuffer-completion-table)
        (pred  minibuffer-completion-predicate)
        result)
    (if (and (eq    method (plist-get cache :method))
             (equal str    (plist-get cache :str))
             (eq    table  (plist-get cache :table))
             (eq    pred   (plist-get cache :pred))
             (eq    mcomplete-ignore-case (plist-get cache :ignore-case)))
        (plist-get cache :result)
      (setq result (funcall func str abort-on-input))
      (set cache-var (list :method method :str str :table table :pred pred
                           :ignore-case mcomplete-ignore-case
                           :result result))
      result)))


;; all-completions with cache
(defvar mcomplete-all-completions-cache
  '(:str nil :table nil :pred nil :method nil :result nil)
  "Cache for `mcomplete-all-completions'.")
(make-variable-buffer-local 'mcomplete-all-completions-cache)

(defun mcomplete-all-completions (str &optional abort-on-input)
  "Call the current method's `all-completions'."
  (mcomplete-with-cache (mcomplete-get :all-completions)
                        'mcomplete-all-completions-cache
                        (mcomplete-current-method)
                        str abort-on-input))

;; try-completion
(defvar mcomplete-try-completion-cache
  '(:str nil :table nil :pred nil :method nil :result nil)
  "Cache for `mcomplete-try-completion'.")

(defun mcomplete-try-completion (str &optional abort-on-input)
  "Call the current method's `try-completion'."
  (mcomplete-with-cache (mcomplete-get :try-completion)
                        'mcomplete-try-completion-cache
                        (mcomplete-current-method)
                        str abort-on-input))


;;; default completion commands
(defvar mcomplete-last-exact-completion
  nil
  "Private variable to hold a state of `mcomplete-do-completion'.")

(add-hook 'mcomplete-minibuffer-setup-hook
          #'(lambda () (setq mcomplete-last-exact-completion nil)))

(defun mcomplete-do-completion ()
  "Perform completion in the minibuffer."
  (let* ((str        (mcomplete-minibuffer-string))
         (completion (mcomplete-try-completion str))
         (status     '(t))
         (last       mcomplete-last-exact-completion))
    (setq mcomplete-last-exact-completion nil)

    (cond
     ((null completion)
      (mcomplete-message " [No match]")
      nil)                              ; nil: no possible completion
     ((eq completion t) t)              ; t:   was already an exact and
                                        ;      unique completion
     (t
      (when (> (length completion) (length str)) ; completed
        (add-to-list 'status 'completed)
        (mcomplete-clear-minibuffer)
        (insert completion))
      (when (mcomplete-exact-match-p str
                                     minibuffer-completion-table
                                     minibuffer-completion-predicate)
        (add-to-list 'status 'exact)
        (unless (memq 'completed status)
          (setq mcomplete-last-exact-completion str)
          (when (equal last str)
            (mcomplete-completion-help))))
      (when (equal status '(t))
        (if completion-auto-help
            (mcomplete-completion-help)
          (mcomplete-message " [Next char not unique]")))
      status))))


(defun mcomplete-complete ()
  "Complete the minibuffer contents as far as possible."
  (unless (eq last-command this-command)
    (setq minibuffer-scroll-window nil))
  (let* ((help-win-exists (and (windowp       minibuffer-scroll-window)
                               (window-buffer minibuffer-scroll-window)
                               (buffer-name   (window-buffer
                                               minibuffer-scroll-window))))
         status)
    (if help-win-exists
        (with-current-buffer (window-buffer minibuffer-scroll-window)
          (if (pos-visible-in-window-p (point-max) minibuffer-scroll-window)
              (set-window-start minibuffer-scroll-window (point-min))
            (scroll-other-window)))
      (setq status (mcomplete-do-completion))
      (cond
       ((null status) nil)
       ((eq status t)
        (mcomplete-message " [Sole completion]"))
       ((and (not (memq 'completed status)) (memq 'exact status))
        (mcomplete-message " [Complete, but not unique]")))
      status)))


(defun mcomplete-complete-and-exit ()
  "Completes the minibuffer contents, and exit."
  (if (or (string= (mcomplete-minibuffer-string) "")
          (mcomplete-exact-match-p (mcomplete-minibuffer-string)
                                   minibuffer-completion-table
                                   minibuffer-completion-predicate))
      (throw 'exit nil)
    (let ((status (mcomplete-do-completion)))
      (cond
       ((eq status t) (throw 'exit nil))
       ((memq 'exact status)
        (if (and (memq 'completed status) minibuffer-completion-confirm)
            (mcomplete-message " [Confirm]")
          (throw 'exit nil)))))))


(defun mcomplete-insert-1st-candidate ()
  (let* ((str (mcomplete-minibuffer-string))
         (first (unless (< (length (mcomplete-minibuffer-string))
                           mcomplete-exhibit-start-chars)
                    (car (mcomplete-all-completions str)))))
    (when first
      (mcomplete-clear-minibuffer)
      (insert first)
      t)))

(defun mcomplete-pick-1st-candidate-and-exit ()
  "Pick the first completion candidate, and exit (for strict completion)."
  (if (or (mcomplete-insert-1st-candidate)
          (string= (mcomplete-minibuffer-string) ""))
      (throw 'exit nil)
    (mcomplete-do-completion)))

(defun mcomplete-exit-minibuffer ()
  "Pick the first completion candidate, and exit (for permissive completion)."
  (mcomplete-insert-1st-candidate)
  (exit-minibuffer))

(defvar mcomplete-complete-word-high-priority-strings
  '(" "  "-")
  "The default value should make `mcomplete-complete-word' act like `minibuffer-complete-word'.")


(defun mcomplete-complete-word ()
  "Complete the minibuffer contents by at most a single word."
  (let* ((str        (mcomplete-minibuffer-string))
         (completion (mcomplete-try-completion str))
         (suffix     (when (stringp completion)
                       (string-match (regexp-quote str) completion)
                       (substring completion (match-end 0)))))
    (cond
     ((null completion)
      (mcomplete-message " [No match]") nil)
     ((eq completion t)
      (mcomplete-message " [Sole completion]") nil)
     ((string= suffix "")
      (let ((strings mcomplete-complete-word-high-priority-strings))
        (unless (catch 'inserted
                  (while strings
                    (when (mcomplete-try-completion (concat str (car strings)))
                      (goto-char (point-max))
                      (insert (car strings))
                      (throw 'inserted t))
                    (setq strings (cdr strings))))
          (if completion-auto-help
              (mcomplete-completion-help)
            (mcomplete-message " [Next char not unique]")))))
     ((string-match "\\`\\sw*\\Sw?" suffix)
      (goto-char (point-max))
      (insert (match-string 0 suffix))
      t)
     (t (error "mcomplete-complete-word: logical error")))))
  ;; Completion behavior of GNU Emacs's `minibuffer-complete-word'
  ;;
  ;; (completing-read "test: " '(("bm-emacs_something.el")))
  ;; <SPACE>
  ;; bm-<SPACE>
  ;; bm-emacs_<SPACE>
  ;; bm-emacs_something.<SPACE>
  ;; bm-emacs_something.el
  ;;
  ;; (completing-read "test: " '(("space wins") ("space_wins") ("space-wins") ("spacewinds")))
  ;; <SPACE>
  ;; "space<SPACE>"
  ;; "space <SPACE>"
  ;; "space wins"
  ;;
  ;; (completing-read "test: " '(("hyphen-wins") ("hyphen_wins")))
  ;; <SPACE>
  ;; hyphen<SPACE>
  ;; hyphen-<SPACE>
  ;; hyphen-wins
  ;;
  ;; (completing-read "test: " '(("can't_decide") ("can'tdecide")))
  ;; <SPACE>
  ;; can'<SPACE>
  ;; can't<SPACE>
  ;; [open help window]
  ;;


(defun mcomplete-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (message "Making completion list...")
  (let ((completions (mcomplete-all-completions
                      (mcomplete-minibuffer-string))))
    (message nil)
    (cond
     ((null completions)
      (ding)
      (mcomplete-message " [No completions]"))
     (t
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list completions))))))


;;; Minibuffer commands
(defun MComplete-complete ()
  "Complete the minibuffer contents as far as possible."
  (interactive)
  (funcall (mcomplete-get :complete)))

(defun MComplete-complete-word ()
  "Complete the minibuffer contents by at most a single word."
  (interactive)
  (funcall (mcomplete-get :complete-word)))

(defun MComplete-complete-and-exit ()
  "If the minibuffer contents is a valid completion then exit.
Otherwise try to complete it."
  (interactive)
  (funcall (mcomplete-get :complete-and-exit)))

(defun MComplete-exit-minibuffer ()
  "Exit the minibuffer."
  (interactive)
  (funcall (mcomplete-get :exit-minibuffer)))


(defun MComplete-toggle-ignore-case ()
  "Toggle case significance in completion."
  (interactive)
  (setq mcomplete-ignore-case (not mcomplete-ignore-case))
  (mcomplete-message (format "[Case is %ssignificant]"
                             (if mcomplete-ignore-case
                                 "NOT "
                               ""))
                     0.5))
                                    

(defun MComplete-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (funcall (mcomplete-get :completion-help)))

(defun mcomplete-rotate-current-method-set (count-or-method)
  (run-hooks (mcomplete-get :method-leave-hook))
  (let ((count (if (integerp count-or-method)
                   count-or-method
                 (catch 'count
                   (let ((list  mcomplete-current-method-set)
                         (count 0))
                     (while list
                       (when (eq (car list) count-or-method)
                         (throw 'count count))
                       (setq count (1+ count)
                             list  (cdr list)))
                     (error "Method symbol %S not found" count-or-method))))))
    (setq mcomplete-current-method-set
          (mcomplete-rotate-list mcomplete-current-method-set count)))
  (run-hooks (mcomplete-get :method-enter-hook)))

(defun MComplete-next-method (&optional arg)
  "Change the completion method to the next one."
  (interactive "p")
  (unless arg (setq arg 1))
  (mcomplete-rotate-current-method-set arg))

(defun MComplete-previous-method (&optional arg)
  "Change the completion method to the previous one."
  (interactive "p")
  (unless arg (setq arg 1))
  (mcomplete-rotate-current-method-set (- arg)))

(defun MComplete-next-candidate (&optional arg)
  "Change the default completion candidate to the next one."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((plist mcomplete-all-completions-cache))
    (plist-put plist
               :result
               (mcomplete-rotate-list (plist-get plist :result) arg))))

(defun MComplete-previous-candidate (&optional arg)
  "Change the default completion candidate to the previous one."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((plist mcomplete-all-completions-cache))
    (plist-put plist
               :result
               (mcomplete-rotate-list (plist-get plist :result) (- arg)))))


;; ------------------------------------------------------------------
;;; Per command customization
;; ------------------------------------------------------------------
(defun mcomplete-get-command-default-plist ()
  (cons :method-set (list mcomplete-default-method-set)))

(defun mcomplete-setup-command-env ()
  (let ((plist (append (get this-command 'mcomplete-mode)
                       (mcomplete-get-command-default-plist))))
    (setq mcomplete-current-method-set
          (apply 'list (plist-get plist :method-set)))
    (setq mcomplete-exhibit-start-chars
          (or (plist-get plist :exhibit-start-chars)
              mcomplete-default-exhibit-start-chars))
    (setq mcomplete-ignore-case
          (let ((ignore (plist-get plist :ignore-case)))
            (cond ((eq ignore 'on)  t)
                  ((eq ignore 'off) nil)
                  ((eq ignore 'nil) completion-ignore-case))))))

(add-hook 'mcomplete-minibuffer-setup-hook 'mcomplete-setup-command-env)

(mapcar #'(lambda (f) (put f 'mcomplete-mode '(:mode off)))
        '(iswitchb-buffer iswitchb-buffer-other-window
          iswitchb-display-buffer iswitchb-buffer-other-frame
          
          ido-find-file ido-find-alternate-file ido-write-file
          ido-insert-file ido-switch-buffer ido-insert-buffer
          ido-kill-buffer ido-find-file-other-window
          ido-find-file-other-window ido-switch-buffer-other-window
          ido-display-buffer ido-find-file-other-frame
          ido-switch-buffer-other-frame))


;; ------------------------------------------------------------------
;;; default plist for all methods
;; ------------------------------------------------------------------
(defvar mcomplete-method-default-plist
  '(;; Name
    :name                 nil

    ;; Core functions
    :all-completions      nil
    :try-completion       nil

    ;; Method enter/leave hooks
    :method-enter-hook    mcomplete-method-enter-hook
    :method-leave-hook    mcomplete-method-leave-hook

    ;; Minibuffer commands
    :complete             mcomplete-complete
    :complete-word        mcomplete-complete-word
    :completion-help      mcomplete-completion-help
    :complete-and-exit    mcomplete-pick-1st-candidate-and-exit
    :exit-minibuffer      mcomplete-exit-minibuffer

    ;; Pre/Post command hooks and functions
    :pre-command-hook     mcomplete-pre-command-hook
    :post-command-hook    mcomplete-post-command-hook)
    "Default property list for completion methods of `mcomplete-mode'.")


;; ------------------------------------------------------------------
;;; prefix match method
;; ------------------------------------------------------------------

(defvar mcomplete-prefix-method
  '(:name    "Prefix match"
    ;; Core functions
    :try-completion  mcomplete-prefix-method-try-completion
    :all-completions mcomplete-prefix-method-all-completions
    ;; Candidates exhibition
    :exhibit mcomplete-prefix-method-exhibit)
  "Property list for prefix matching completion method.")

(defun mcomplete-prefix-method-try-completion (str abort-on-input)
  "`try-completion' for prefix matching method."
  (let ((completion-ignore-case mcomplete-ignore-case))
    (try-completion str
                    minibuffer-completion-table
                    (if (and minibuffer-completion-predicate abort-on-input)
                        'mcomplete-predicate-with-input-check
                      minibuffer-completion-predicate))))


(defun mcomplete-prefix-method-all-completions (str abort-on-input)
  "`all-completions' for prefix matching method."
  (let ((completion-ignore-case mcomplete-ignore-case))
    (sort
     (all-completions str
                      minibuffer-completion-table
                      (if (and minibuffer-completion-predicate abort-on-input)
                          'mcomplete-predicate-with-input-check
                        minibuffer-completion-predicate))
     'string<)))


(defface mcomplete-prefix-method-fixed-part-face
  '((t (:bold t :foreground "Aquamarine")))
  "Face to highlight the fixed part of input for prefix matching method."
  :group      'mcomplete)
                     
(defface mcomplete-prefix-method-alternative-part-face
  '((t (:foreground "Aquamarine")))
  "Face to highlight the alternative part of input for prefix matching method."
  :group      'mcomplete)

(defun mcomplete-prefix-method-exhibit (str all try)
  "Exhibit prefix matching completion information in the minibuffer."
  (let* ((f-face 'mcomplete-prefix-method-fixed-part-face)
         (a-face 'mcomplete-prefix-method-alternative-part-face))
    (unless (null try)
      (put-text-property (mcomplete-prompt-end) (point-max) 'face f-face))
    (cond
     ((null try) (insert " [No match]"))
     ((eq try t) (insert " [Sole completion]"))
     (t
      (let* ((fixed (substring try (length str)))
             (tail1 (substring (car all) (length try)))
             (rest  (cdr all))
             tail)
        (unless (string= fixed "")
          (put-text-property 0 (length fixed) 'face f-face fixed)
          (insert (concat "[" fixed "]")))
        (when (or rest (not (string= tail1 "")))
          (insert "{")
          (put-text-property 0 (length tail1) 'face a-face tail1)
          (insert tail1)
          (while (and rest
                      (< (+ (point-max)
                            (length (setq tail (substring (car rest)
                                                          (length try))))
                            4)
                         (window-width)))
            (insert "," tail)
            (setq rest (cdr rest)))
          (insert (if rest ",..}" "}"))))))))


;; ------------------------------------------------------------------
;;; substring match method
;; ------------------------------------------------------------------

(defvar mcomplete-substr-method
  '(:name                 "Substring match"

    ;; Core functions
    :try-completion       mcomplete-substr-method-try-completion
    :all-completions      mcomplete-substr-method-all-completions

    ;; Candidates exhibition
    :exhibit              mcomplete-substr-method-exhibit)
  "Property list for substring matching completion method.")



(defun mcomplete-substr-method-all-completions (str abort-on-input)
  "`all-completions' for substring match method of `mcomplete-mode'."
  (let ((table minibuffer-completion-table)
        (pred  (if abort-on-input
                   'mcomplete-predicate-with-input-check
                 minibuffer-completion-predicate)))
    (let ((case-fold-search mcomplete-ignore-case)
          (regexp (regexp-quote str))
	  (len (length str))
          list)
      (cond
       ((listp table)                   ; alist or nil
        (let ((rest table))
          (while rest
            (when (and (string-match regexp (caar rest))
                       (or (null pred) (funcall pred (car rest))))
              (setq list (cons (caar rest) list)))
            (setq rest (cdr rest)))))

       ((vectorp table)                 ; obarray
        (mapatoms
         #'(lambda (s)
             (when (and (string-match regexp (symbol-name s))
                        (or (null pred) (funcall pred s)))
               (setq list (cons (copy-sequence (symbol-name s)) list))))
         table))

       (t
        (error "Invalid TABLE")))
      
      (sort list (lambda (a b)
		   (let ((a-prefix (eq (compare-strings a 0 len str 0 len) t))
			 (b-prefix (eq (compare-strings b 0 len str 0 len) t)))
		     (cond 
		      ;; Sort prefix matches first
		      ((and a-prefix (not b-prefix)) t)
		      ((and b-prefix (not a-prefix)) nil)
		      (t (string< a b)))))))))


(defun mcomplete-substr-method-try-completion (str abort-on-input)
  "`try-completion' for substring match method of `mcomplete-mode'."
  (let* ((completions (mcomplete-all-completions str abort-on-input)))
    (cond
     ((null completions)                ; 0 candidate
      nil)

     ((null (cdr completions))          ; 1 candidate
      (if (string= str (car completions))
          t
        (car completions)))

     (t                                 ; multiple candidates
      (let* ((regexp (regexp-quote str))
             (tails-alist (mapcar #'(lambda (item)
                                      (string-match regexp item)
                                      (list (substring item (match-end 0))))
                                  completions)))
        (concat str (try-completion "" tails-alist)))))))


(defface mcomplete-substr-method-fixed-part-face
  '((t (:bold t :foreground "SpringGreen")))
  "Face to highlight the fixed part of input for substring matching method."
  :group      'mcomplete)
                     
(defface mcomplete-substr-method-alternative-part-face
  '((t (:foreground "SpringGreen")))
  "Face to highlight alternative parts of input for substring matching method."
  :group      'mcomplete)


(defun mcomplete-substr-method-exhibit (str all try)
  "Exhibit substring matching completion information in the minibuffer."
  (let* ((f-face 'mcomplete-substr-method-fixed-part-face)
         (a-face 'mcomplete-substr-method-alternative-part-face))
    (unless (null try)
      (put-text-property (mcomplete-prompt-end) (point-max) 'face f-face))
    (cond
     ((null try) (insert " [No match]"))
     ((eq try t) (insert " [Sole completion]"))
     (t
      (let* ((fixed (progn
                      (string-match (regexp-quote str) try)
                      (substring try (match-end 0))))
             (alt1 (copy-sequence (car all)))
             (rest (cdr all)))
        (unless (string= fixed "")
          (put-text-property 0 (length fixed) 'face f-face fixed)
          (insert (concat "[" fixed "]")))
        (insert "<")
        (string-match (regexp-quote try) alt1)
        (put-text-property 0 (match-beginning 0) 'face a-face alt1)
        (put-text-property (match-end 0) (length alt1) 'face a-face alt1)
        (put-text-property (match-beginning 0) (match-end 0) 'face f-face alt1)
        (insert alt1)
        (while (and rest (< (+ (point-max) (length (car rest)) 4)
                            (window-width)))
          (insert "," (car rest))
          (setq rest (cdr rest)))
        (insert (if rest ",..>" ">")))))))


;;; completing-help.el support
(if (featurep 'completing-help)
    (add-to-list 'completing-help-commands 'MComplete-completion-help)
  (add-hook 'completing-help-load-hook
            #'(lambda () (add-to-list 'completing-help-commands
                                      'MComplete-completion-help))))

(defvar mcomplete-load-hook nil
  "Hook to run at the end of loading mcomplete.")

(provide 'mcomplete)
(run-hooks 'mcomplete-load-hook)

;;; mcomplete.el ends here
