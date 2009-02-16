;;; tmenu.el --- a text based interface to the menubar

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Filename:      tmenu.el
;; Author:        Yuji Minejima <ggb01164@nifty.ne.jp>
;; Maintainer:    Yuji Minejima <ggb01164@nifty.ne.jp>
;; Keywords:      menu, convenience
;; Description:   a text based interface to the menubar
;; Compatibility: GNU Emacs 20.7.2, XEmacs21.1.12
;; URL:           http://homepage1.nifty.com/bmonkey/emacs/elisp/tmenu.el
;; $Revision: 0.23 $

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

;;; Commentary:

;; This package provides a text based interface to the menubar.
;; In order to test this as you see this file, proceed as follows:
;;   1: Load this package:  M-x eval-buffer
;;   2: Browse the menubar: M-x tmenu-menubar
;;   3: You see something like the following:
;;      -------------------------------------------------------------
;;      Click mouse-2 on a completion to select it.
;;      In this buffer, type RET to select the completion near point.
;;      
;;      Possible completions are:
;;      b > Buffers                        f > Files
;;      t > Tools                          e > Edit
;;      s > Search                         m > Mule
;;      E > Emacs-Lisp                     h > Help
;;      -------------------------------------------------------------
;;   4: Press the first character (e.g. b for Buffers, E for Emacs-Lisp)
;;      to select the menu item.
;;      Enter C-g to exit the menu without selecting any item.
;;


;;; What's the differences between tmenu.el and tmm.el

;;  I mainly wrote this package to lean the differences in the menu APIs
;;  between GNU Emacs and XEmacs, so I'm afraid there's no earth-shattering
;;  features that tmenu.el can be proud of.
;;
;;  tmm.el
;;   * a standard package bundled with GNU Emacs.
;;   * is lightweight.
;;   * 
;;
;;  tmenu.el
;;   * runs on GNU Emacs and XEmacs.
;;   * allow the user to move around the menu hierarchy.
;;   * displays radio/toggle buttons.
;;   * 


;;; Requirements:

;; Tested with FSF Emacs 20.7.2 and XEmacs 21.1.12.


;;; Install:

;; 1: Put this file in one of the directories listed in `load-path'.
;;    You can see the contents of `load-path' by entering
;;    `M-x customize-option <RET> load-path'.
;;
;; 2: Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/tmenu.el <RET>'
;;    to speed up the execution of this package.
;;    The resulting byte-compiled file tmenu.elc is specific to
;;    the Emacsen (GNU Emacs and XEmacs).
;;
;; 3: Put the following lines in your .emacs file.
;;
;;    (autoload 'tmenu-menubar "tmenu"
;;              "Text based interface to the menubar."
;;              t nil)
;;    (global-set-key [f10] 'tmenu-menubar)
;;
;; 4: Restart Emacs or enter `M-x load-library <RET> tmenu'.
;; 5: You can invoke tmenu-menubar by pressing F10 function key.


;;; Popup menus:

;; There are two functions for popup menus:
;; 
;; - Function: tmenu-menu-read-from-minibuffer popup-menu-spec
;;   This function returns the selected tmenu-item object.
;;   You can access various fields of the returned object by tmenu-item-FOO
;;   functions. See `;;; tmenu-item core functions' sections.
;;
;; - Function: tmenu-menu-execute popup-menu-spec
;;   This function executes or evals the selected item's real-binding.
;;
;; The argument POPUP-MENU-SPEC can be any of the popup menu forms
;; supported by the currently running Emacsen.
;; In addition, you can use XEmacs style (or easymenu style) popup menu
;; format for GNU Emacs.
;;
;; e.g. The following code should work on GNU Emacs and XEmacs.
;;      (tmenu-menu-execute
;;       '("My Popup menu"
;;         ["%_Find file at point"        find-file-at-point]
;;         ["Find %_function at point"    find-function-at-point]
;;         ["Find %_variable at point"    find-variable-at-point]
;;         ["Brows %_url at point" browse-url]))
;;
;;      *note* "%_Find ..." specifies `F' as an accelerator.
;;             ugh... but easymenu doesn't support this on GNU Emacs,
;;             so this doesn't work for the graphical popup menu.


;;; Implementation notes:

;; Bogus BNF
;; TMENU-ITEM: (tmenu-item NAME REAL-BINDING (KEY . VALUE) ...)
;;
;; REAL-BINDING: COMMAND | TMENU | TMENU-LAMBDA
;;
;; TMENU: (tmenu NAME TMENU-ITEM ...)
;;
;; TMENU-LAMBDA: (tmenu-lambda . FORM)


;;; References:

;; Standard packages concerning menus
;;   GNU Emacs
;;   |XEmacs
;;   ||
;;   XO big-menubar.el --- an alternate menubar
;;   OO easymenu.el --- support the easymenu interface for defining a menu.
;;   OX lmenu.el --- emulate Lucid's menubar support
;;   OX menu-bar.el --- define a default menu bar.
;;   XO menubar.el --- Menubar support for XEmacs
;;   XO menubar-items.el --- Menubar and popup-menu content for XEmacs.
;;   OX tmm.el --- text mode access to menu-bar (FSF Emacs only)
;;   OX x-menu.el --- menu support for X
;;   XO x-popup-menu.el --- Mimic x-popup-menu in FSF Emacs
;;
;; "(elisp)Pop-Up Menus"
;; "(elisp)Menu Keymaps"
;; "(lispref)Menus"


;;; Bugs:

;; * GNU Emacs
;;   tmenu-merge-keymaps and tmenu-merge-simple-popup-panes don't merge
;;   overlapping items correctly.
;; * GNU Emacs
;;   @ in an item string is just ignored.
;; * All the commands that use (interactive "e") (e.g. bookmark-menu-FOO)
;;   don't work.
;; * The current menu display uses Emacs's completion facilities.
;;   This is somewhat misleading.


;;; Change Log:

;; Version 0.23
;;  * Fixed a bug where tmenu-merge-keymaps distroys some keymap entries
;;    when the local keymap trys to undefine some of the global-map.

;; Version 0.22 (06 Dec 2000):
;;  * First public release


;;; Code:


;;; Declarations
(eval-when-compile
  (defvar last-command)
  (defvar last-command-char)
  (defvar last-command-event)
  (defvar overriding-local-map)
  (defvar minibuffer-completion-table)
  (defvar minibuffer-scroll-window)
  (defvar minibuffer-setup-hook)
  (defvar menu-bar-update-hook)
  (defvar menu-bar-final-items)         ; GNU Emacs only
  (defvar current-menubar)              ; XEmacs only
  (defvar menubar-configuration)        ; XEmacs only
  (defvar menubar-show-keybindings)     ; XEmacs only
  (defvar activate-menubar-hook)        ; XEmacs only
)


;;; Customization
(defgroup tmenu nil
  "This package provides a text based interface to the menubar."
  :group 'menu)

(defcustom tmenu-menubar-prompt
  "Menu bar"
  "Prompt string for the menu bar."
  :type 'string
  :group 'tmenu)

(defcustom tmenu-show-keybindings
  (if (boundp 'menubar-show-keybindings)
      menubar-show-keybindings
    t)
  "If true, the key sequences bound to the menu items are displayed."
  :type 'boolean
  :group 'tmenu)

(defcustom tmenu-parent-accelerator ?.
  "Accelerator character for a parent menu."
  :type 'character
  :group 'tmenu)

(defcustom tmenu-accelerators
  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?a ?A ?b ?B ?c ?C ?d ?D ?e ?E ?f ?F ?g ?G
    ?h ?H ?i ?I ?j ?J ?k ?K ?l ?L ?m ?M ?o ?O ?p ?P ?q ?Q ?r ?R ?s ?S ?t ?T
    ?u ?U ?v ?V ?w ?W ?x ?X ?y ?Y ?z ?Z)
  "Accelerator characters for normal menu items."
  :type '(repeat character)
  :group 'tmenu)


;;; Utilities
(defmacro tmenu-char-p (c)
  "Return non-nil when C is a character."
  (if (fboundp 'characterp)
      `(characterp ,c)
    `(integerp ,c)))

(defun tmenu-plist-to-alist (plist)
  "Return an alist equivalent to PLIST."
  (let (alist)
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist)
            plist (cddr plist)))
    alist))


;;; Eval
(defun tmenu-eval (form)
  "Evaluate elisp FORM and return the result or nil when any error occurs.

Since XEmacs complained that the value of `last-buffer-undo-list' is void
when evaluating :active property for some menu item, I had to ignore any
errors."
  (condition-case nil
      (eval form)
    (error nil)))


;;; tmenu-lambda functions
(defsubst tmenu-lambda-create (form)
  "Return a tmenu-lambda object containing FORM."
  (cons 'tmenu-lambda form))

(defsubst tmenu-lambda-p (obj)
  "Return non-nil if OBJ is a tmenu-lambda object or nil otherwise."
  (and (consp obj) (eq (car obj) 'tmenu-lambda)))

(defsubst tmenu-lambda-eval (tmenu-lambda)
  "Evaluate the form contained in TMENU-LAMBDA object and return the result."
  (tmenu-eval (cdr tmenu-lambda)))


;;; tmenu-item core functions
(defun tmenu-item-create (name real-binding &optional alist)
  "Return a tmenu-item object having NAME, REAL-BINDING and ALIST."
  (let ((item (nconc (list 'tmenu-item name real-binding) (append alist nil))))
    (when (and tmenu-show-keybindings
               (null (cdr (assoc :tmenu-key-binding-data alist))))
      (tmenu-item-put item
                      :tmenu-key-binding-data
                      (tmenu-item-compute-key-binding-data item)))
    (when (and (tmenup real-binding)
               (not (assoc :tmenu-completion-table alist)))
      (tmenu-item-put item
                      :tmenu-completion-table
                      (tmenu-item-compute-completion-table item)))
    item))

(defsubst tmenu-item-p (obj)
  "Return non-nil if OBJ is a tmenu-item object or nil otherwise."
  (and (consp obj) (eq (car obj) 'tmenu-item)))

(defsubst tmenu-item-name (item)
  "Return the name of tmenu-item ITEM."
  (nth 1 item))

(defsubst tmenu-item-real-binding (item)
  "Return the real-binding of tmenu-item ITEM."
  (nth 2 item))

(defsubst tmenu-item-set-real-binding (item real-binding)
  (setcar (nthcdr 2 item) real-binding))

(defsubst tmenu-item-alist (item)
  "Return the alist of tmenu-item ITEM."
  (nthcdr 3 item))

(defun tmenu-item-put (item property value)
  "Put PROPERTY and VALUE pair to ITEM's alist."
  (let* ((before-alist (nthcdr 2 item))
         (pair (assoc property (cdr before-alist))))
    (if pair
        (setcdr pair value)
      (setcdr before-alist (cons (cons property value) (cdr before-alist))))))

(defsubst tmenu-item-has (item property)
  "Return non-nil if ITEM has PROPERTY in its alist or nil otherwise."
  (assoc property (tmenu-item-alist item)))

(defsubst tmenu-item-get (item property)
  "Return the value for PROPERTY in ITEM's alist."
  (cdr (assoc property (tmenu-item-alist item))))


;;; tmenu-item peripheral functions
(defun tmenu-item (menu)
  "Return tmenu-item object equivalent to local Emacsen's MENU.
If MENU is t, the current menubar is used."
  (unless (or (consp menu) (eq menu t))
    (error "Non-menu object is given to tmenu-item: %S" menu))
  (cond
   ((eq menu t)
    ;; menubar
    (if (featurep 'xemacs)
        (tmenu-XEmacs-menu-to-tmenu-item (tmenu-compute-XEmacs-menubar))
      (tmenu-keymap-to-tmenu-item (tmenu-compute-menubar-keymap))))
   ((eq (car menu) 'keymap)
    ;; GNU Emacs sparse keymap
    (tmenu-keymap-to-tmenu-item menu))
   ((and (consp (car menu)) (eq (car menu) 'keymap))
    ;; list of GNU Emacs sparse keymaps
    (tmenu-keymap-to-tmenu-item (tmenu-merge-keymaps menu)))
   ((consp (cadr menu))
    ;; GNU Emacs simple popup menu
    (tmenu-simple-popup-to-tmenu-item menu))
   (t
    ;; XEmacs menu
    (tmenu-XEmacs-menu-to-tmenu-item menu))))
  
(defsubst tmenu-item-visible-p (item)
  "Return non-nil if ITEM is visible in the menu or nil otherwise."
  (or (not (tmenu-item-has item :visible))
      (tmenu-eval (tmenu-item-get item :visible))))

(defsubst tmenu-item-enable-p (item)
  "Return non-nil if ITEM is enabled in the menu or nil otherwise."
  (or (not (tmenu-item-has item :enable))
      (tmenu-eval (tmenu-item-get item :enable))))

(defun tmenu-item-selectable-p (item)
  "Return ITEM if it's selectable in the menu or nil otherwise."
  (and (tmenu-item-real-binding item) ; check for a separator str
       (tmenu-item-visible-p item)
       (tmenu-item-enable-p item)
       item))

(defun tmenu-item-compute-key-binding-data (item)
  "Return key-binding-data for ITEM.
key-binding-data is one of the following forms:
  ([KEY-SEQ] . \"  (KEY-SEQ-STR)\") | (nil) | nil
See \"(elisp)Simple Menu Items\"."
  (let* ((command (tmenu-item-real-binding item))
         seq)
    (when (and (functionp command) (commandp command)
               (setq seq (where-is-internal command overriding-local-map t)))
      (cons seq (concat "  (" (key-description seq) ")")))))


(defsubst tmenu-item-contains-menu-p (item)
  "Return non-nil if ITEM contains a menu or nil otherwise."
  (tmenup (tmenu-item-real-binding item)))

(defsubst tmenu-item-contains-lambda-p (item)
  "Return non-nil if ITEM contains a tmenu lambda object or nil otherwise."
  (tmenu-lambda-p (tmenu-item-real-binding item)))

(defun tmenu-item-compute-completion-table-key (item)
  "Return completion table key for ITEM."
  (let ((button (tmenu-item-get item :button))
        (keyseq (when tmenu-show-keybindings
                  (cdr (tmenu-item-get item :tmenu-key-binding-data))))
        table-key)
    (setq table-key
          (concat
           (char-to-string (tmenu-item-get item :tmenu-accelerator))
           (cond
            ((equal (tmenu-item-get item :tmenu-accelerator)
                    tmenu-parent-accelerator)
             " < ")
            ((or (tmenu-item-contains-menu-p item)
                 (tmenu-item-contains-lambda-p item))
             " > ")
            (t " = "))
           (when button
             (cond
              ((eq (car button) :toggle)
               (format "[%s] " (if (tmenu-eval (cdr button))
                                   "*" " ")))
              ((eq (car button) :radio)
               (format "(%s) " (if (tmenu-eval (cdr button))
                                   "*" " ")))
              (t nil)))
           (tmenu-eval (tmenu-item-name item))
           (when (tmenu-item-get item :suffix)
             (concat " " (tmenu-eval (tmenu-item-get item :suffix))))))
    (when (stringp keyseq)
      ;; a halfhearted attempt to align keyseq
      (setq table-key (format "%-22s%s" table-key keyseq)))
    table-key))

(defun tmenu-item-select-submenu-item (item submenu-item)
  "Return SUBMENU-ITEM contained in ITEM after doing a behind-the-scene job."
  (when (tmenu-lambda-p (tmenu-item-real-binding submenu-item))
    (tmenu-item-set-real-binding submenu-item
                                 (tmenu-lambda-eval
                                  (tmenu-item-real-binding submenu-item))))
  (when (and (tmenu-item-contains-menu-p submenu-item)
             (not (tmenu-item-get submenu-item :tmenu-completion-table)))
    (tmenu-item-put submenu-item
                    :tmenu-completion-table
                    (tmenu-item-compute-completion-table submenu-item item)))
  submenu-item)

(defun tmenu-item-compute-completion-table (item &optional parent)
  "Return a completion table for ITEM which is contained in PARENT."
  (let* ((menu (tmenu-item-real-binding item))
         (item-list (tmenu-items menu))
         (table (when parent
                  (setq parent (tmenu-item-create
                                (tmenu-item-name parent)
                                (tmenu-item-real-binding parent)
                                (mapcar #'(lambda (pair)
                                            (cons (car pair)
                                                  (cdr pair)))
                                        (tmenu-item-alist parent))))
                  (tmenu-item-put parent :tmenu-accelerator
                                  tmenu-parent-accelerator)
                  (list (cons (tmenu-item-compute-completion-table-key parent)
                              parent)))))
    (while item-list
      (setq table (cons (cons (tmenu-item-compute-completion-table-key
                               (car item-list))
                              (car item-list))
                        table)
            item-list (cdr item-list)))
    (nreverse table)))



;;; tmenu
(defun tmenu-create (name item-list)
  "Return a tmenu object having NAME and ITEM-LIST."
  (let ((menu (nconc (list 'tmenu name)
                     (delq nil (mapcar 'tmenu-item-selectable-p item-list)))))
    (tmenu-compute-and-set-accelerators menu)
    menu))

(defun tmenup (obj)
  "Return non-nil when OBJ is a tmenu object or nil otherwise."
  (and (consp obj) (eq (car obj) 'tmenu)))

(defun tmenu-name (menu)
  "Return the name of MENU."
  (nth 1 menu))

(defun tmenu-items (menu)
  "Return the items of MENU."
  (apply 'list (nthcdr 2 menu)))

(defun tmenu-compute-and-set-accelerators (menu)
  "Compute and set accelerators of the items of MENU."
  (unless (> (length tmenu-accelerators) 0)
    (error "tmenu-accelerators must contain at least one character."))
  (let* ((chars (append tmenu-accelerators nil))
         (regex (concat "[\\" (mapconcat 'char-to-string chars "\\") "]"))
         (fallback-char (nth (1- (length tmenu-accelerators))
                             tmenu-accelerators))
         (rest (cddr menu))
         c name item)
    (while rest
      (setq item (car rest)
            rest (cdr rest)
            c (tmenu-item-get item :accelerator)
            name (tmenu-eval (tmenu-item-name item)))
      (unless (and (tmenu-char-p c) (member c chars))
        (let ((from 0))
          (setq c nil)
          (while (string-match regex name from)
            (setq c (downcase (string-to-char (match-string 0 name))))
            (unless (member c chars)
              (setq c (upcase c)))
            (setq from (if (member c chars)
                           (length name)
                         (prog1 (match-end 0) (setq c nil)))))
          (unless c
            (setq c (if chars (car chars) fallback-char)))))
      (tmenu-item-put item :tmenu-accelerator c)
      (setq chars (delete c chars)))
    menu))
      



;;; Minibuffer functions
(defun tmenu-show-menu ()
  "Show the current menu contents in the *Completions* buffer."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list (mapcar #'car minibuffer-completion-table))))

(defvar tmenu-keyseq-list-bound-to-scroll-menu-window
  '([(?\?)] [(tab)] [(?\ )] [(escape) (tab)] [(escape) (?\ )] [(meta tab)]
     [(space)])
  "List of keysequences to be bound to `tmenu-scroll-menu-window' command.")
  

(defun tmenu-setup-local-map ()
  "Setup the local key map."
  (let ((map (copy-keymap (current-local-map)))
        (keys (cons tmenu-parent-accelerator tmenu-accelerators)))
    (while keys
      (define-key map (vector (car keys)) 'Tmenu-accelerator-command)
      (setq keys (cdr keys)))
    (mapcar #'(lambda (keyseq)
                (define-key map keyseq 'Tmenu-scroll-menu-window))
            tmenu-keyseq-list-bound-to-scroll-menu-window)
    (use-local-map map)))

(defmacro tmenu-minibuffer-prompt-end ()
  "Absorb the difference in GNU Emacs21 and the other Emacsen."
  (if (fboundp 'minibuffer-prompt-end)
      '(minibuffer-prompt-end)
    '(point-min)))

(defun Tmenu-scroll-menu-window ()
  "Scroll the *Completions* buffer displaying the menu contents."
  (interactive)
  (let* ((win (when (window-live-p minibuffer-scroll-window)
                minibuffer-scroll-window))
         (buf (when (and win (bufferp (window-buffer win)))
                (window-buffer win))))
    (if win
        (with-current-buffer buf
          (if (pos-visible-in-window-p (point-max) win)
              (set-window-start win (point-min))
            (scroll-other-window)))
      (tmenu-show-menu))))

(defun Tmenu-accelerator-command ()
  "Seletct the menu item matching the last-command-char or ding otherwise."
  (interactive)
  (let ((c last-command-char))
                ; In minibuffer
    (mapcar
     #'(lambda (pair)
         (when (equal c (tmenu-item-get (cdr pair) :tmenu-accelerator))
           (delete-region (tmenu-minibuffer-prompt-end) (point-max))
           (insert (car pair))
           (exit-minibuffer)))
     minibuffer-completion-table)

    ;; not found
    (ding)))


;;; Interface
(defun tmenu-menu-read-from-minibuffer-1 (item)
  "Return the user selected menu item contained in ITEM."
  (let* ((name (tmenu-item-name item))
         (table (tmenu-item-get item :tmenu-completion-table))
         (minibuffer-setup-hook (append minibuffer-setup-hook ()))
         ;; protect some variables for later tmenu-eval calls
         (last-command last-command)
         (last-command-event last-command-event))
    (add-hook 'minibuffer-setup-hook 'tmenu-show-menu)
    (add-hook 'minibuffer-setup-hook 'tmenu-setup-local-map t)
    (cdr (assoc (completing-read (concat name ": ") table) table))))

(defun tmenu-menu-read-from-minibuffer (menu)
  "Return the user selected menu item contained in MENU (in)directly."
  (let* ((item (tmenu-item menu))
         dad)
    (while (tmenu-item-contains-menu-p item)
      (setq dad item
            item (tmenu-item-select-submenu-item
                  dad (tmenu-menu-read-from-minibuffer-1 dad))))
    item))

(defun tmenu-menu-execute (menu)
  "Ask the user to select an item in MENU, then invoke the selected item."
  (let* ((item (tmenu-menu-read-from-minibuffer menu))
         binding)
    (when item
      (setq binding (tmenu-item-real-binding item))
      (when (tmenu-item-get item :tmenu-keymap-key)
        (setq last-command-event (tmenu-item-get item :tmenu-keymap-key)))
      (cond
       ((and (functionp binding) (commandp binding))
        (call-interactively binding))
       (t
        ;; XEmacs menubar->edit->spell->help needs this
        (eval binding))))))


;;;###autoload
(defun tmenu-menubar ()
  "Text based interface to the menubar."
  (interactive)
  (tmenu-menu-execute t))             ; t for menubar


(defmacro tmenu-defun-substitute-for-tmm ()
  "Substitute `tmenu-menubar' command for `tmm-menubar' command in tmm.el.
As of this writing (GNU Emacs 20.7.2, XEmacs 21.1.12), tmm.el is bundled
only with GNU Emacs.  But this should do XEmacs users no harm."
  (when (fboundp 'tmm-menubar)
    '(defun Tmenu-substitute-for-tmm ()
       (interactive)
       (substitute-key-definition 'tmm-menubar 'tmenu-menubar global-map))))


;;; GNU Emacs menus

;; GNU Emacs popup menus
(defun tmenu-strip-atmark-from-name (name)
  (if (and (stringp name) (>= (length name) 1) (equal (aref name 0) ?\@))
      (substring name 1)
    name))

(defun tmenu-simple-popup-to-tmenu-item (menu)
  "Return tmenu-item object equivalent to GNU Emacs simple popup MENU.
MENU looks like as follows:
\(TITLE (PANE-TITLE (LINE . ITEM) (LINE . ITEM) ...)
       \(PANE-TITLE (LINE . ITEM) (LINE . ITEM) ...)
       ...)"
  (let* ((name (car menu))
         (pane (tmenu-merge-simple-popup-panes (cdr menu))))
    (tmenu-item-create name (tmenu-simple-popup-pane-to-tmenu pane))))


(defun tmenu-merge-simple-popup-panes (pane-list)
  "Return a popup menu pane which has all the items of panes in PANE-LIST."
  (let* (name
         (pane (delq nil (mapcar #'(lambda (item)
                                     (cond
                                      ((and (stringp item) (null name))
                                       (setq name item)
                                       nil)
                                      ((consp item)
                                       item)
                                      (t nil)))
                                 (apply 'append pane-list)))))
    (unless name
      (setq name "Popup menu"))
    (cons name pane)))

(defun tmenu-simple-popup-pane-to-tmenu (pane)
  "Return a tmenu object equivalent to GNU Emacs simple popup menu item PANE."
  (tmenu-create (car pane)
                (mapcar #'tmenu-simple-popup-item-to-tmenu-item (cdr pane))))

(defun tmenu-simple-popup-item-to-tmenu-item (item)
  "Return a tmenu-item object equivalent to GNU Emacs simple popup menu ITEM."
  (tmenu-item-create (car item) (cdr item)))


;; GNU Emacs menu keymaps
(defun tmenu-compute-menubar-keymap (&optional fake-function-keyseq)
  "Return a single menu keymap denoting GNU Emacs's menu bar."
  (let ((key (or fake-function-keyseq [menu-bar])))
    (run-hooks 'menu-bar-update-hook)
    (nconc (tmenu-merge-keymaps (delq nil (list (global-key-binding key)
                                                (local-key-binding key)
                                                (minor-mode-key-binding key)))
                                (when (equal key [menu-bar])
                                  menu-bar-final-items))
           (list tmenu-menubar-prompt))))

(defun tmenu-merge-keymaps (map-list &optional final-items)
  "Return a keymap obtained by merging keymaps in MAP-LIST.
For FINAL-ITEMS, see `menu-bar-final-items'."
  (let (map entry alist final-alist name key binding assoc)
    (while map-list
      (setq map (car map-list)
            map-list (cdr map-list))
      (while map
        (setq entry (car map)
              map   (cdr map))
        (cond
         ((eq entry 'keymap))           ; ignore
         ((stringp entry)
          (when (null name) (setq name entry)))
         ((consp entry)
          (setq key (car entry)
                binding (cdr entry)
                assoc (or (assoc key alist) (assoc key final-alist)))
          (cond
           ((and assoc (eq binding 'undefined))
            (setcdr assoc nil))         ; suppress global-map binding
           (assoc
            ;; The right thing to do is to merge (cdr assoc) and `binding',
            ;; then do (setcdr assoc new-binding).
            ;; But currently the old binding takes precedence, and the
            ;; new one is simply ignored.
            )
           (t
            (if (memq key final-items)
                (setq final-alist (cons (cons key binding) final-alist))
              (setq alist (cons (cons key binding) alist))))))
         (t))))
    (nconc (list 'keymap name) (nreverse alist) (nreverse final-alist))))


(defun tmenu-keymap-to-tmenu-item (keymap &optional key submenu-handler)
  "Return tmenu-item object equivalent to GNU Emacs sparce KEYMAP.
SUBMENU-HANDLER is a function taking one argument which is a menu keymap
and returning a real-binding object suitable for tmenu-item.
Functions suitable as SUBMENU-HANDLER are as follows:
  `tmenu-keymap-to-tmenu-lambda' dilays the conversion of submenus using
   tmenu-lambda object.
  `tmenu-keymap-to-tmenu' recursively converts all submenus to tmenus all
   at once."
  (let ((tmenu (tmenu-keymap-to-tmenu keymap submenu-handler))
        (alist (when key (list (cons :tmenu-keymap-key key)))))
    (tmenu-item-create (tmenu-name tmenu) tmenu alist)))

(defun tmenu-keymap-to-tmenu-lambda (keymap)
  "Return tmenu-lambda object containing KEYMAP.
The resulting tmenu-lambda object yields tmenu object equivalent to KEYMAP,
when given as an argument to `tmenu-lambda-eval' function."
  (tmenu-lambda-create
   `(tmenu-keymap-to-tmenu ',keymap 'tmenu-keymap-to-tmenu-lambda)))

(defun tmenu-keymap-to-tmenu (keymap &optional submenu-handler)
  "Return a tmenu object equivalent to KEYMAP."
  (while (symbolp keymap)
    ;; find the actual keymap. see SYMBOL section in "(elisp)Key Lookup"
    (setq keymap (symbol-function keymap)))
  (unless (keymapp keymap)
    (error "Non keymap object given to tmenu-keymap-to-tmenu: %S" keymap))
  (let (menu-name entry item-list item)
    (setq keymap (cdr keymap))          ; skip the first 'keymap symbol
    (while keymap
      (setq entry (car keymap)
            keymap (cdr keymap))
      (cond                             ; see "(elisp)Format of Keymaps"
       ((stringp entry)                 ; found menu name
        (setq menu-name entry))
       ((consp entry)                   ; found (KEY . MENU-ITEM)
        ;; see "(elisp)Defining Menus"
        (setq item (tmenu-keymap-entry-to-tmenu-item entry submenu-handler)
              item-list (cons item item-list)))
       (t)))
    (tmenu-create menu-name (nreverse item-list))))


(defun tmenu-keymap-entry-to-tmenu-item (keymap-entry &optional
                                                      submenu-handler)
  ""
  (let* ((key     (car keymap-entry))
         (binding (cdr keymap-entry))
         item)
    (cond
     ((stringp binding)
      (setq item (tmenu-item-create binding nil)))
     ((consp binding)
      (setq item (funcall (if (eq (car binding) 'menu-item)
                              'tmenu-extended-item-to-tmenu-item
                            'tmenu-simple-item-to-tmenu-item)
                          binding submenu-handler))
      (tmenu-item-put item :tmenu-keymap-key key))
     (t))
    item))


(defun tmenu-simple-item-to-tmenu-item (item &optional submenu-handler)
  "Retern a tmenu item equivalent to simple menu ITEM.
See \"(elisp)Simple Menu Items\".

Simple menu item format is:
 \(ITEM-STRING HELP-STRING KEY-BINDING-DATA . REAL-BINDING)
where HELP-STRING and KEY-BINDING-DATA are optional.
e.g. (\"Open File...\" ([24 6] . \"  (C-x C-f)\") . find-file)"
  (unless submenu-handler (setq submenu-handler 'tmenu-keymap-to-tmenu-lambda))
  (let* ((item-str (prog1 (tmenu-strip-atmark-from-name (car item))
                     (setq item (cdr item))))
         ;; (ITEM-STRING -!- HELP-STRING KEY-BINDING-DATA . REAL-BINDING)
         (help-str (when (and (consp item) (stringp (car item)))
                     (prog1 (car item) (setq item (cdr item)))))
         ;; (ITEM-STRING HELP-STRING -!- KEY-BINDING-DATA . REAL-BINDING)
         (key-binding-data (when (and (consp item)
                                      (not (or (functionp item)
                                               ;; (commandp '((nil)))
                                               ;; throws error (bug?)
                                               (keymapp item))))
                             (prog1 (car item) (setq item (cdr item)))))
         ;; (ITEM-STRING HELP-STRING KEY-BINDING-DATA . -!- REAL-BINDING)
         (real-binding item)
         (alist (list (cons :tmenu-key-binding-data key-binding-data))))
    (when (keymapp real-binding)
      (setq real-binding (funcall submenu-handler real-binding)))
    (when help-str
      (setq alist (cons (cons :help help-str) alist)))
    (when (and (symbolp real-binding) (get real-binding 'menu-enable))
      (setq alist
            (cons (cons :enable (get real-binding 'menu-enable)) alist)))
    (tmenu-item-create item-str real-binding alist)))



(defun tmenu-extended-item-to-tmenu-item (item &optional submenu-handler)
  "Retern a tmenu item equivalent to an extended menu ITEM.
See \"(elisp)Extended Menu Items\".

Extended menu item format is:
 (menu-item ITEM-NAME)
 or
 (menu-item ITEM-NAME REAL-BINDING KEY-BINDING-DATA . ITEM-PROPERTY-LIST)
where KEY-BINDING-DATA is optional."
  (unless submenu-handler (setq submenu-handler 'tmenu-keymap-to-tmenu-lambda))
  (unless (and (consp item) (eq 'menu-item (car item)))
    (error "Malformed extended menu item given to tmenu"))
  (let* ((item-name (prog1 (tmenu-strip-atmark-from-name (cadr item))
                      (setq item (cddr item))))
         ;; (menu-item ITEM-NAME -!- REAL-BINDING KEY-BINDING-DATA KEY VAL..)
         (real-binding (prog1 (car item) (setq item (cdr item))))
         ;; (menu-item ITEM-NAME REAL-BINDING -!- KEY-BINDING-DATA KEY VAL..)
         (key-binding-data (when (consp (car item))
                             (prog1 (car item) (setq item (cdr item)))))
         (plist (append item (list :tmenu-key-binding-data key-binding-data))))
    ;; (menu-item ITEM-NAME REAL-BINDING KEY-BINDING-DATA -!- KEY VAL..)
    (when (functionp (plist-get plist :filter))
      (setq real-binding (funcall (plist-get plist :filter) real-binding)))
    (when (keymapp real-binding)        ; submenu?
      (setq real-binding (funcall submenu-handler real-binding)))
    (tmenu-item-create item-name real-binding
                       (tmenu-plist-to-alist plist))))


;;; XEmacs menus ---------------------------------------------------------
(defun tmenu-compute-XEmacs-menubar ()
  "Return a menu denoting XEmacs's menu bar."
  (run-hooks 'activate-menubar-hook)
  (cons tmenu-menubar-prompt current-menubar))


(defun tmenu-XEmacs-menu-to-tmenu-item (menu &optional item-list-handler)
  "Return a tmenu-item object equivalent to an XEmacs MENU.

XEmacs menu format is:
 (MENU-NAME-STR KEY VAL ... ITEM ...)
where KEY and VAL pairs are optional.

ITEM-LIST-HANDLER is a function taking two arguments and returning
a real-binding object suitable for tmenu-item.
ITEM-LIST-HANDLER's first argument is a name string and the second argument
is a list of items."
  (unless item-list-handler
    (setq item-list-handler 'tmenu-XEmacs-item-list-to-tmenu))
  (tmenu-XEmacs-menu-to-tmenu-item-1 menu item-list-handler))

(defun tmenu-XEmacs-menu-to-tmenu-item-1 (menu &optional item-list-handler)
  (unless (and (consp menu) (stringp (car menu)))
    (error "Non XEmacs menu given to tmenu: %S" menu))
  (unless item-list-handler
    (setq item-list-handler 'tmenu-XEmacs-item-list-to-tmenu-lambda))
  (let* ((menu-name (prog1 (car menu) (setq menu (cdr menu))))
         ;; (MENU-NAME-STR -!- KEY VAL ... ITEM ...)
         plist)
    (while (and (symbolp (car menu))
                (char-equal ?\: (aref (symbol-name (car menu)) 0)))
      (setq plist (cons (car menu) (cons (cadr menu) plist))
            menu (cddr menu)))

    ;; (MENU-NAME-STR KEY VAL ... -!- ITEM ...)
    (when (functionp (plist-get plist :filter)) ; get fresh menu items
      (setq menu (funcall (plist-get plist :filter) menu)))

    (tmenu-item-create menu-name
                       (funcall item-list-handler menu-name menu)
                       (tmenu-XEmacs-plist-to-tmenu-alist plist))))


;;(defun tmenu-XEmacs-menu-to-tmenu-item-recursive (menu)
;;  (tmenu-XEmacs-menu-to-tmenu-item
;;   menu
;;   #'(lambda (name list)
;;       (tmenu-XEmacs-item-list-to-tmenu
;;        name
;;        list
;;        'tmenu-XEmacs-menu-to-tmenu-item-recursive))))


(defun tmenu-XEmacs-item-list-to-tmenu-lambda (list-name item-list)
  "Return a tmenu-lambda object equivalent to XEmacs menu ITEM-LIST."
  (tmenu-lambda-create `(tmenu-XEmacs-item-list-to-tmenu ',list-name
                                                         ',item-list)))

(defun tmenu-XEmacs-item-list-to-tmenu (list-name item-list
                                                  &optional submenu-handler)
  "Return a tmenu equivalent to an XEmacs menu ITEM-LIST."
  (unless submenu-handler
    (setq submenu-handler 'tmenu-XEmacs-menu-to-tmenu-item-1))
  (tmenu-create list-name
                (delq nil
                      (mapcar #'(lambda (item)
                                  (cond
                                   ((stringp item) ; unselectable text
                                    (tmenu-item-create item nil))
                                   ((consp item) ; submenu
                                    (funcall submenu-handler item))
                                   ((vectorp item)
                                    (tmenu-XEmacs-item-to-tmenu-item item))
                                   (t nil)))
                              item-list))))


(defun tmenu-XEmacs-item-to-tmenu-item (item)
  "Return a tmenu-item equivalent to an XEmacs menu ITEM."
  (unless (and (vectorp item) (stringp (aref item 0)))
    (error "XEmacs menu item must have item-name-string: %S" item))
  (let ((name (aref item 0))
        (real-binding (aref item 1))
        (plist (cond
                ((= (length item) 3)
                 ;; [ NAME CALLBACK ACTIVE-P ]
                 (list :enable (aref item 2)))
                ((and (= (length item) 4)
                      (not (and (symbolp (aref item 2))
                                (equal ?:
                                       (aref (symbol-name (aref item 2)) 0)))))
                 ;; [ NAME CALLBACK ACTIVE-P SUFFIX]
                 (list :enable (aref item 2) :suffix (aref item 3)))
                (t
                 ;; [ NAME CALLBACK :KEYWORD VALUE :KEYWORD VALUE ... ]
                 (append (nthcdr 2 (append item nil)) nil)))))
    (when (string-match "\\(%_\\)\\(.\\)" name)
      ;; "%_File" -> "File" and :accelerator ?F
      (setq plist (plist-put plist
                             :accelerator
                             (string-to-char (match-string 2 name)))
            name  (replace-match "\\2" "FIXEDCASE" nil name)))
    (tmenu-item-create name
                       real-binding
                       (tmenu-XEmacs-plist-to-tmenu-alist plist))))

;; property list
(defun tmenu-XEmacs-plist-to-tmenu-alist (plist)
  "Return a canonicalized alist corresponding to PLIST."
  (let ((key-mapping '((:active . :enable)
                       (:included . :visible)))
        (ignore-key-list '(:selected))
        (selected (plist-get plist :selected))
        result key val)
    (while (and plist (consp (cdr plist)))
      (setq key (car plist)
            val (cadr plist)
            plist (cddr plist))
      (when (assq key key-mapping)
        (setq key (cdr (assq key key-mapping))))
      (when (eq key :style)
        (setq key :button
              val (cons (intern (concat ":" (symbol-name val))) selected)))
      (unless (memq key ignore-key-list)
        (setq result (cons (cons key val) result))))
    result))

;;; Hook
(defvar tmenu-load-hook nil
  "Hook to run after loading tmenu.")

(provide 'tmenu)
(run-hooks 'tmenu-load-hook)
;;; tmenu.el ends here
