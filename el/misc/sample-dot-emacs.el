;;; sample-dot-emacs.el
;;
;; This file is part of GNU Emacs.
;; GNU Emacs is free software.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;
;;; .emacs --- Sample Emacs init file to use with Drew Adams's elisp code.
;; 
;; Emacs Lisp Archive Entry
;; Filename: ~/.emacs
;; Description: Drew Adams' Emacs init file.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1995-2001, Drew Adams, all rights reserved.
;; Created: Tue Sep 12 15:54:33 1995
;; Version: $Id: EMACS-TO-COPY,v 1.6 2001/01/10 16:44:20 dadams Exp $
;; Last-Updated: Wed Jan 10 08:44:04 2001
;;           By: dadams
;;     Update #: 970
;; Keywords: init
;; Compatibility: GNU Emacs 20.x 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Emacs init file that uses Drew Adams' Emacs libraries.
;;
;;
;;  INSTALLATION
;; 
;;  Make a copy of this file in your home directory, modify that as
;;  needed, and rename it `.emacs'.  It becomes your initialization
;;  file, loaded automatically whenever you start Emacs.  You can edit
;;  this, in order to customize Emacs.  Detailed instructions for
;;  doing so can be found below.
;;
;;    MICROSOFT WINDOWS - Windows Explorer won't let you rename this
;;    file to `.emacs'.  Instead, you can use Emacs itself to rename
;;    it. You do this by editing the directory (via `C-x d') and
;;    renaming the file via `r' (`dired-rename-this-file') in the
;;    directory buffer.
;;
;;
;;  DREW'S LISP LIBRARY
;;
;;  The Emacs Lisp files in the directory named by the Emacs variable
;;  `drews-lisp-dir' (defined in this file) modify the default
;;  behavior of Emacs considerably.  You might not appreciate every
;;  such customization.
;;
;;  In any case, I suggest that you try getting to know it in the
;;  customized form provided, before changing things.  You'll find
;;  extensive comments in my source (Emacs Lisp) files that should
;;  help you understand how things work, and how best to change things
;;  to suit your needs.  The place to start, in order to understand my
;;  code and to change it, is the rest of the commentary in this file.
;;
;;  It may help to know that the names of my Lisp files follow the
;;  following convention.  Files that replace standard GNU Emacs Lisp
;;  files keep their names, naturally - example: `delsel.el'.  Names
;;  of files that are extensions of standard files are suffixed by `-'
;;  or `+', depending on whether they are to be loaded just before or
;;  just after the corresponding standard files.  For example, my file
;;  `compile-.el' is to be loaded before the standard file
;;  `compile.el'; my file `compile+.el' is to be loaded after
;;  `compile.el'.  The `+' case is more common than the `-' case.
;;
;;  Finally, note that Emacs generally offers more functionality on
;;  Unix platforms than on Windows.  The same is true of my
;;  customizations.  To get the most out of my Lisp library, use it
;;  with Emacs on Unix, not Windows.
;;
;;
;;  HELP WITH EMACS
;;  
;;  NOTATION - In Emacs documentation, "C-" means (press and hold) the
;;  Control key; "M-" means the Alt key (called "Meta"); "DEL" means
;;  the Backspace key; "RET" means the Return, or Enter, key; "SPC"
;;  means the space bar, "ESC" means the Escape key.
;;
;;  ABORTING - You can use `C-g' to abort any action you have
;;  started. For some actions, you may need to repeat this. If even
;;  this doesn't clear things up entirely, then try `C-]', which
;;  should do the trick.
;;
;;  HELP - For help with Emacs, use the `Help' menu, submenu
;;  `Describe'.  Item `This...'  in this menu (shortcut: `C-h RET')
;;  combines many of the other items into one.  It lets you
;;  mouse-click anything, type keys, or choose a menu item to get help
;;  on the object clicked, the keys typed, or the menu item chosen.
;;
;;  Some of the menus and key bindings used here are different from
;;  those described in the standard Emacs manual, which is available
;;  via the `Help' menu, item `Info' (shortcut: `C-h i'), `Emacs'.
;;  All other documentary help should be up-to-date and accurate.
;;
;;  COMMANDS - You can enter any command, whether bound to a key
;;  sequence or not, via `M-x'. Previously entered commands are
;;  available for editing and re-execution via `C-p' (previous) and
;;  `C-n' (next).  See also `C-x ESC ESC' for more advanced command
;;  editing.
;;
;;  INPUT - Some actions require you to input some text in the
;;  "minibuffer" (where you enter commands).  Default text (usually
;;  the text surrounding the cursor) may already be provided there,
;;  for possible modification.  You can remove this quickly, if you
;;  like, via `M-C-DEL' or `C-x DEL'.  Use `RET' to confirm and enter
;;  the input you want.
;;
;;    For example, the command `grep' (menu `Search', item 'Grep', or
;;    just `M-x grep') expects you to input text to search for, and
;;    file name patterns for files to search. By default, this command
;;    puts the text found around the cursor into the minibuffer as the
;;    text to look for. If that's not what you want, type `M-C-DEL'
;;    (or just repeated `DEL's) to remove it, then type the text and
;;    file name patterns you want and confirm with `RET'.
;;
;;
;;  BUFFER MODES
;;
;;  Each buffer has its own editing mode (key bindings etc).
;;  Information on a buffer's current mode is available via the `Help'
;;  menu, item `Describe Mode' (shortcuts: `C-h m' or `C-h RET' click
;;  buffer).  In the menu bar, all menus to the left of the separator
;;  `||' are specific to the current mode; the menus to the right
;;  (`Buffers', `Files', `Tools', `Edit', `Search', `Frames', `Help')
;;  are common to all modes.  (See also `M-x describe-menubar'.)
;;
;;
;;  UNDO
;;
;;  You can undo anything via the `Edit' menu, item `Undo'
;;  (shortcut:`C-/'). This can be repeated any number of times. Do
;;  anything else to stop undoing. Doing undo after that redoes what
;;  you undid (because it undoes the undoing!). You can tell when
;;  you've undone all unsaved changes, because the lower left of the
;;  mode line shows `--' instead of `**'. You can quickly undo all
;;  unsaved changes via `M-x revert-buffer'.
;;
;;  
;;  MOUSE
;; 
;;  You can use the mouse as usual: press MB1 and drag to define the
;;  selection, MB2 to paste.  Check the Emacs manual to learn about
;;  additional available mouse actions. In particular, a secondary
;;  selection is also available by pressing and holding `M-C' during
;;  mouse actions. For example, press and hold `M-C' while dragging
;;  the mouse to define the secondary selection; press `M-C' and click
;;  MB2 to paste. It's quite useful to have two separate selections to
;;  paste etc.
;;  
;;  
;;  SEARCH / REPLACE
;;  
;;  You can use the `Search' menu for this.  However, incremental
;;  searching is generally better to use (more convenient and more
;;  powerful).  It is available via `C-s' and `C-r'; for more
;;  information, type `C-s C-h'.
;;  
;;  
;;  EMACS JARGON
;;  
;;  Here is a translation to/from Emacs-speak:
;;
;;         common term       Emacs term
;;         -----------       ----------
;;         selection         region
;;         cut               kill
;;         paste             yank
;;         window            frame
;;
;;  In Emacs terms, a "window" is a frame pane, that is, a sub-frame.
;;  The "mode-line" is the text at the bottom border of a window. The
;;  "minibuffer" is the special buffer for entering commands; it
;;  appears either in a separate frame (the default here) or at the
;;  bottom of each frame (if you set `create-builtin-frames' to nil,
;;  below).  The (text) "cursor" is the place where you insert typed
;;  text; it is rectangular, by default.  The (mouse) "pointer" shows
;;  the mouse position; it is an arrow, by default.  The terms "point"
;;  and "mark" are discussed below.
;;
;;
;;  REGION (SELECTION)
;;
;;  The text cursor (not the mouse pointer) defines the position of
;;  the "point", which is one end of the region. The other end is the
;;  "mark" position. You can set the point by clicking MB1 or using
;;  the arrow keys. You can set the mark by dragging the point with
;;  MB1 (the mark is left behind), or via `C-SPC'. If you define the
;;  region without using the mouse, it may be invisible; you can make
;;  it visible via `C-x C-x', which also swaps the point and the mark.
;;
;;
;;  FRAME COMMANDS
;;
;;  Use the `Frames' menu to manipulate frames.
;;
;;    `Shrink-Wrap This Frame' (shortcut: `C-x C-_') resizes the
;;                 current frame to try to fit all the text of the
;;                 current window.
;;
;;    `Iconify All Frames' iconifies each frame separately.
;;
;;    `Hide Frames / Show Buffers' (shortcut: `M-C-z') hides all of
;;                 the frames except one, which is then iconified.
;;                 This is especially useful with Microsoft Windows,
;;                 because, unlike `Iconify All Frames', it places
;;                 only a single icon in the task bar.
;;
;;                 Memorize the binding of this command: `M-C-z'; you
;;                 use it again to reverse the action. After hiding
;;                 everything, when you click the single icon it opens
;;                 a single frame (which may not have a menu bar). You
;;                 then type `M-c-z' to get access to the other frames
;;                 (via the displayed Buffer List).
;;
;;                 An alternative shortcut for this command is
;;                 `[C-mouse-1]' in the minibuffer; that is, clicking
;;                 mouse-1 in the minibuffer while pressing the
;;                 CONTROL key.
;;
;;  Use the `Buffers' menu to raise individual frames (and buffers).
;;  Use the `Files' menu to delete, duplicate and split frames.
;;
;;
;;  CUSTOMIZING
;;
;;  This initialization file modifies basic Emacs in several ways.
;;  You may want to modify it further to suit your own tastes. Each of
;;  the customizations done here is explained below, together with how
;;  to inhibit it. The customizations made via libraries loaded here
;;  are explained in those library files; the starting points are
;;  library files `start.el' and `start-opt.el'.
;;
;;  You can always start Emacs with no customizations at all (not even
;;  your `.emacs' file is loaded) via the command line option `-q'.
;;
;;  You need to know the following to customize things further:
;;
;;  1. Initialization files are automatically loaded in this order:
;;     1) A site library, `site-start.el', if there is one.  You can
;;        inhibit this via command line option `-no-site-file' when
;;        starting Emacs.
;;     2) Your initialization file, `.emacs' (this file).
;;     3) A site library, `default.el', if there is one.  You can
;;        inhibit this by setting variable `inhibit-default-init' to
;;        non-nil.
;;
;;  2. The variable `load-path' determines the order of searching
;;     libraries for files to load.  If you want to load your own file
;;     in place of a file having the same name in another library,
;;     then its directory should come before the other library in the
;;     list `load-path'. If you want to make sure another library is
;;     already loaded when you load a file, then that file should
;;     require or load it as follows: (require 'library) or
;;     (load-library "library").
;;
;;  3. SETTING VARIABLES - You can use `defvar' to set a variable
;;     *before* loading a library that might also set it with
;;     `defvar': (defvar some-var some-value).  Only the first
;;     `defvar' executed assigns a value to the variable. You use
;;     `defconst' or `setq' to set a variable unconditionally: (setq
;;     some-var some-value) or (defconst some-var some-value).  Each
;;     call to one of these functions assigns a new value. Try using
;;     `defvar' first to customize things; resort to `defconst' only
;;     if `defvar' doesn't seem to have any effect (either because a
;;     previous `defvar' was done or a subsequent `setq' or `defconst'
;;     was done). Some variables have different (local) values in
;;     different buffers. To set a variable's default (global) value
;;     for all buffers, use `setq-default': (setq some-var
;;     default-value).
;;
;;  4. See the Emacs manual, section `Customization' (especially
;;     subsection `Customization') for more information, including
;;     examples: `C-h i'; choose `Emacs'; choose `Customization' in
;;     the main menu, under heading `Advanced Features'; choose node
;;    `Init File'.
;;
;;  5. On Unix, you can predefine certain aspects of the display via
;;     the file `.Xdefaults' in your home director (`~'). Here is one
;;     suggestion (add these or similar lines to `~/.Xdefaults'):
;;
;;       emacs.Background:   LightBlue
;;       emacs.Foreground:   Black
;;       emacs.cursorColor:  Red
;;       emacs.pointerColor: Red
;;     
;;       Emacs*popup.font: -*-*-*-*-*-21-*-*-*-*-*-iso8859-1
;;       Emacs*menubar.font: -*-*-*-*-*-21-*-*-*-*-*-iso8859-1
;;       Emacs*popup.background: Pink
;;       Emacs*menubar.background: Pink
;;       Emacs*menubar.buttonForeground: Blue
;;     
;;     For more information on the `.Xdefaults' file, see the Emacs
;;     manual (`C-h i'), node `Resources X'.
;;     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Code: 

;;; UNCOMMENT THIS TO DEBUG TROUBLE GETTING EMACS UP AND RUNNING.
;;; (setq debug-on-error t)

;;; UNCOMMENT THIS & REPLACE "XXX" HERE BY YOUR USER (LOGIN) NAME.
;;; This inhibits the initial startup echo area message.
;;; (setq inhibit-startup-echo-area-message "XXX")


;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT HEIGHT IS INAPPROPRIATE.
;;; Maximum height for new frames.
;;; (defvar create-frame-max-height-percent 82) ; no more than 82% of display height.
;;; (defvar create-frame-max-height 48)         ; no more than 48 characters high.

;;; UNCOMMENT AND CHANGE *ONE* OF THESE, IF DEFAULT WIDTH IS INAPPROPRIATE.
;;; Maximum width for new frames.
;;; (defvar create-frame-max-width-percent 94) ; no more than 94% of display width.
;;; (defvar create-frame-max-width 120)        ; no more than 120 characters wide.


;;; COMMENT THIS OUT IF YOU *DON'T* WANT MAXIMUM BUFFER HIGHLIGHTING.
(defconst font-lock-maximum-decoration t)

;;; COMMENT OUT IF YOU *DON'T* WANT THE GIVEN EFFECT.
(setq inhibit-startup-message t)        ; Do without obnoxious startup msgs.
(put 'eval-expression 'disabled nil)    ; Enable eval of Lisp sexps.
(put 'narrow-to-region 'disabled nil)   ; Enable region narrowing & widening.
(put 'downcase-region 'disabled nil)    ; Enable case changes of region text.
(put 'upcase-region 'disabled nil)      ;   "     "   
(put 'capitalize-region 'disabled nil)  ;   "     "  
(auto-compression-mode 1)               ; Auto decompress compressed files.
;;;@@@Emacs20 (add-hook 'help-mode-hook 'turn-on-font-lock) ; Fontify *Help* buffer.

;;; ;;; See doc (`C-h f') of function `lazy-lock-mode' for explanation of this.
;;; (setq font-lock-support-mode
;;;       '((compilation-mode) (occur-mode) (t . lazy-lock-mode)))
;;; (defvar lazy-lock-minimum-size
;;;   '((diff-mode . 100000) (compilation-mode . 100000) (occur-mode . 100000) (t . 102400)))



;;; UNCOMMENT IF YOU *DON'T* WANT SEPARATE FRAMES FOR THE MINIBUFFER AND BUFFERS
;;; *Help* AND *Completions*.
;;; (defvar create-builtin-frames nil) ; Uncomment for no built-in frames.

;;; UNCOMMENT AND CHANGE IF DEFAULT VALUES ARE INAPPROPRIATE.
;;; (NOTE: These are not used if `create-builtin-frames' is nil.)
;;; (defvar minibuffer-frame-top/bottom '(- 20)
;;;   "*Position of top (or bottom) of minibuffer frame, in pixels.
;;; May be of form `POS', `(+ POS)' or `(- POS)', where POS is a positive
;;; integer.  Forms POS and `(+ POS)' specify the position of
;;; frame top with respect to screen top.  Forms -POS and `(- POS)'
;;; specify the position of frame bottom with respect to screen bottom.
;;; In any case, POS itself counts toward the top.")

;;; (defvar minibuffer-frame-width 120
;;;   "*Width of minibuffer frame, in characters.")
;;; (defvar minibuffer-frame-height 1 "*Height of minibuffer frame, in characters.")

;;; COMMENT THIS OUT IF YOU *DON'T* WANT "SPECIAL" BUFFERS TO BE IN
;;; SEPARATE FRAMES ("Special" buffers are those, such as *grep*,
;;; whose names are within '*'s.) If `special-display-regexps' is
;;; non-nil, then special buffers are in dedicated frames (cannot
;;; dissociate the buffer and its frame).
(defconst special-display-regexps '("[ ]?[*][^*]+[*]"))

;;; UNCOMMENT ANY OF THESE, TO *INHIBIT* THE CORRESPONDING FRAME AUTO-RESIZING.
;;; (add-hook 'after-make-frame-functions   ; Inhibit auto-resizing *NEW* frames.
;;;           'shrink-frame-to-fit)
;;; (defvar fit-frame-when-pop-to-p nil)    ; Inhibit auto-resizing by `pop-to-buffer'.
;;; (defvar fit-frame-when-switch-to-p nil) ; Inhibit auto-resizing by `switch-to-buffer'.
;;; (defvar fit-frame-when-display-p nil)   ; Inhibit auto-resizing by `display-buffer'.
;;; (defvar enable-shrink-frame-to-fit nil) ; Inhibit *ALL* frame resizing, even manual.

;;; UNCOMMENT IF YOU *DON'T* WANT TO USE `remove-window'.
;;; Non-nil => Use `remove-window' in place of `delete-window'.
;;; (defvar sub-remove-window nil)

;;; UNCOMMENT IF YOU *DON'T* WANT TO USE `query-replace-w-options'.
;;; Non-nil => Use `query-replace-w-options' in place of `query-replace'.
;;; (defvar sub-query-replace-w-options nil)

;;; UNCOMMENT IF YOU *DON'T* WANT TO USE `kill-buffer-and-its-windows'.
;;; Non-nil => Use `kill-buffer-and-its-windows' in place of `kill-buffer'.
;;; (defvar sub-kill-buffer-and-its-windows nil)

;;; UNCOMMENT IF YOU *DON'T* WANT TO USE `exit-with-confirmation'.
;;; Non-nil => Use `exit-with-confirmation' in place of `save-buffers-kill-emacs'.
;;; (defvar sub-exit-with-confirmation nil)



;;; CHANGE THIS TO REFLECT THE ADDRESS OF DREW'S LISP LIBRARY AT YOUR SITE.
(defvar drews-lisp-dir "CHANGE THIS"  "Address of Drew's lisp libraries")

;;; ADD DREW'S LISP LIBRARY TO YOUR `load-path'.

(setq load-path (append (list drews-lisp-dir)
;;; PLACE YOUR OWN LISP LIBRARY HERE, IF ANY:
;;;                     (list "my-path/my-lib-dir")
                        load-path))

;;; PLACE YOUR `DEFVAR' VARIABLE SETTINGS HERE, IF ANY.


;;; COMPLETE THE ADDRESS OF GREP COMMAND, 
;;; OR JUST USE "grep -n" IF grep CAN BE FOUND VIA YOUR PATH.
(defvar grep-command "grep -n ")


;;; LOAD DREW'S LIBRARY FILES from the directory listed above.
;;; Library file `start.elc' loads lots of others.  In particular, it
;;; loads `setup-frames.elc' and `setup-keys.elc', which define
;;; default frame configurations and key bindings.  You can see
;;; (roughly) which library files have been loaded at any time via
;;; `C-h v features'.
(require 'start)

;;; *** NOTE TO MAINTAINERS OF DREW'S LISP LIBRARY -
;;; 
;;;   File `start.elc' loads file `loaddefs.el' (no need to compile
;;;   this), which, in turn, takes care of autoloading lots of files,
;;;   as needed.  File `loaddefs.el' is generated automatically, via
;;;   the Emacs `;;;###autoload' mechanism.  Do *not* modify
;;;   `loaddefs.el' by hand.
;;;
;;;   Instead, every time you change one of the Lisp source files
;;;   (i.e., every time you add or remove a `;;;###autoload'), you
;;;   need to execute `M-x update-autoloads-from-directory' from the
;;;   Lisp source directory.
;;;
;;;   So, get in the habit, when you change a Lisp source file, of
;;;   executing these two commands in the source directory:
;;;
;;;         M-x update-autoloads-from-directory
;;;         M-x byte-recompile-directory
;;;
;;; *** END OF NOTE TO MAINTAINERS OF DREW'S LISP LIBRARY


;;; The following setup assignments are done in file `start-opt.elc'.
;;; Action to change these should be taken *after* loading it.
;;; (See file `start-opt.el' for more detail.)
;;;
;;; 1. Some standard faces are redefined: highlight, modeline, region,
;;;    secondary-selection, query-replace, isearch, ediff-*-face-*.
;;; 2. Searching is made case-sensitive by default, but `C-c' while
;;;    searching (`C-s') toggles case-sensitivity.
;;;    To inhibit this, do (setq-default case-fold-search t).
;;; 3. DEL (backspace) removes the current selection, and typing replaces it.
;;;    To inhibit this, do (delete-selection-mode nil).
;;; 4. Coloring (font-locking) is the default in all buffers.
;;;    To inhibit this, do (global-font-lock-mode nil)
;;; 5. Indenting uses only spaces, not TABs.
;;;    To inhibit this, do (setq-default indent-tabs-mode t).
;;; 6. The default mode for buffers is `indented-text-mode'.
;;;    To inhibit this, do (setq default-major-mode 'fundamental-mode)
;;; 7. Text mode uses auto-fill, by default. 
;;;    To inhibit this, do (remove-hook 'text-mode-hook 'turn-on-auto-fill).
(require 'start-opt)                    ; Optional startup assignments.


;;; DIARY FOR USE WITH CALENDAR AND APPOINTMENTS:
;;; IF YOU *DON'T* WANT TO USE A DIARY, THEN UNCOMMENT THESE LINES:
;;; (setq view-diary-entries-initially nil)
;;; (setq mark-diary-entries-in-calendar nil)
;;;
;;; If you *DO* want to use a diary, then create a file named `diary'
;;; in your home directory.  Suggestion: put the following two lines
;;; (after uncommenting them) in that file, to be reminded of the
;;; moon's phases and the times of sunrise & sunset:
;;;
;;; &%%(diary-sunrise-sunset)
;;; &%%(diary-phases-of-moon)
;;;
;;; For more info on the calendar, the diary and appointments, see the
;;; Emacs manual (`C-h i', then choose `Calendar/Diary' in the menu).


;;; PLACE YOUR `SETQ', AND `DEFCONST' VARIABLE SETTINGS HERE, IF ANY.





;;; ******************************************************************
;;; IMPORTANT - DO THIS *LAST*, SO `rebind-minibuffer-completion-maps'
;;;             CAN PICK UP ALL CURRENT KEY DEFINITIONS.
;;; Enable minibuffer cycling of default inputs via arrow keys.
(require 'rebind-mbuf-maps)		; Requires library 'elect-mbuf'.

;;; A HACK FOR WINDOWS
(if (and (string-match "i386" system-configuration)
         (fboundp 'rename-frame))
    (add-hook 'window-setup-hook 'rename-frame)) ; Defined in `frame-cmds.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sample-dot-emacs.el ends here
