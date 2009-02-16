;;;; Emacs lisp code to bind function keys on a Sun4/SPARC keyboard in
;;;; X11 or OpenWindows.  Includes a pop-up help window to show the
;;;; current bindings, and a warning message when you press an unbound
;;;; function key.

;;;; Written 2/91, by Eero Simoncelli, eero@victoria.media.mit.edu

;;;; To use this code, load the file and then try pressing the "Help"
;;;; key.  You should get a pop-up buffer listing the current function
;;;; key bindings.  You can use the define-X-fn-key function in your
;;;; .emacs file to bind specific function keys (examples given below).
;;;; NOTE: To make porting easier, Sun-dependent items have the word 
;;;; "sun" in them.

;;;; Keys on the keyboard generate a "keycode" which X translates into
;;;; a "keysym".  The default mapping is usually found in the
;;;; directory /usr/lib/X11/keymaps.  It can be examined using
;;;; "xmodmap -pk", or interactively using the command "xev".  Note
;;;; that it can be many-to-one.

;;;; The keysyms are turned into strings (we call them "keymapstring"s
;;;; here) when they are fed to Emacs.  This mapping (keysyms to
;;;; keymapstrings) is built into Emacs - some of it may be found in
;;;; the file /usr/local/emacs/src/x11term.c.  To check what
;;;; keymapstring is generated on a keypress, you can execute the
;;;; emacs function "show-chars", defined below.  The keymapstrings
;;;; are interpreted by emacs as if you had typed them at the keyboard
;;;; (i.e., through the emacs keymaps).  The keymapstrings for the
;;;; function keys begin with with the sequence "ESC-[".  Note, again,
;;;; that this mapping can be many-to-one.  Furthermore, some keysyms
;;;; are mapped to keymapstrings that have very standard meanings in
;;;; Emacs (eg. "Up" is bound to Ctl-p), and so rebinding these
;;;; keysyms will change the functionality of the standard binding!

;;;; This file allows you to set the mapping between keysyms and emacs
;;;; functions, assuming the standard mapping from keysyms to
;;;; keymapstrings.  This file doesn't bind any keys, except for the
;;;; Help key, which is bound to 'describe-fn-key-bindings.  Use the
;;;; define-X-fn-key function to bind keys.

;;;; *** This should be rewritten to allow modifiers (i.e., shift,
;;;; control, meta).  Should also allow bindings in mode-specific
;;;; keymaps.  Both of these are hard to do, given the way standard
;;;; Gnu-emacs sets up the X-fn-keymap.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Example bindings on SPARC keyboards (put these in your .emacs
;;;; file): These assume standard X11 keysyms.  For a list of all
;;;; keysyms, see the definition of sun-keysym-keymapstring-alist
;;;; below.

;;   (define-X-fn-key "R7" 'beginning-of-buffer) ;Home
;;   (define-X-fn-key "R9" 'scroll-down) ;PgUp
;;   (define-X-fn-key "R11" 'recenter)
;;   (define-X-fn-key "R13" 'end-of-buffer) ;End
;;   (define-X-fn-key "R15" 'scroll-up) ;PgDn
;;
;;   (define-X-fn-key "L1" 'keyboard-quit)            ;Stop
;;   (define-X-fn-key "L2" 'electric-command-history) ;Again
;;   (define-X-fn-key "L3" 'list-buffers)	      ;Props
;;   (define-X-fn-key "L4" 'undo)		      ;Undo
;;   (define-X-fn-key "L9" 'bury-buffer)	      ;Find
;;   (define-X-fn-key "L6" 'copy-region-as-kill)      ;Copy
;;   (define-X-fn-key "L8" 'yank-pop)		      ;Paste
;;   (define-X-fn-key "L10" 'kill-region)	      ;Cut
;;
;;   (define-X-fn-key "F1" 'delete-other-windows)
;;   (define-X-fn-key "F2" 'split-window-and-switch-buffer)
;;   (define-X-fn-key "F3" 'other-window)
;;   (define-X-fn-key "F4" 'switch-to-buffer)
;;   (define-X-fn-key "F5" 'switch-to-buffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Old name, for back-compatibility
(defun  define-sun-X-fn-key (keysym def)
  (define-X-fn-key keysym def))

;;; Use this to bind function keys when using X windows.  
(defun define-X-fn-key (keysym def)
  (let ((keymapstring (keysym-to-keymapstring keysym)))
    (if (stringp keymapstring)
	(define-key X-fn-keymap keymapstring def)
	(error "Unknown key symbol %s does not appear in sun-keysym-keymapstring-alist."
	       keysym))))

;;; This will be bound to the "Help" key.
;;; *** Should list mouse bindings, too.
(defun describe-X-fn-bindings ()
  (interactive)
  (let ((alist  sun-keysym-keymapstring-alist)
	binding keysym)
    (with-output-to-temp-buffer "*Help*"
      (princ "X function key bindings:\n")
      (princ "-----------------\n")
      (while alist
	(setq keysym (car (car alist)))
	(setq binding (lookup-key X-fn-keymap (cdr (car alist))))
	(cond ((numberp binding)
	       (princ (format " %s:\t no keymap entry for this key symbol!\n" keysym)))
	      ((eq binding 'X-fn-key-unbound) nil)
	      (t (princ (format " %s:\t %s\n" keysym binding))))
	(setq alist (cdr alist))))))

;;; Print out chars typed - useful for finding out what sequences are generated
;;; by mouse, or fn keys.
;;; Randal L. Schwartz <merlyn@intelob.intel.com>
(defun show-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
	(inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout.")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
	    quit-flag nil))             ; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keysym-to-keymapstring (keysym)
  (cdr (assoc keysym sun-keysym-keymapstring-alist)))

(defun keymapstring-to-keysym (keymapstring)
  (car (rassoc keymapstring sun-keysym-keymapstring-alist)))

;;; Like assoc, but cdr of pairs in alist are tested
(defun rassoc (key alist)
  (while (and alist (not (equal (cdr (car alist)) key)))
    (setq alist (cdr alist)))
  (car alist))
    
(defun X-fn-key-unbound ()
  (interactive)
  (beep)
  (message (format "X keysym %s is not bound.  Use define-X-fn-key to bind it."
		   (keymapstring-to-keysym (substring (this-command-keys) 2)))))

;;; This captures the default correspondence between keysyms and the
;;; keymapstrings that Emacs receives, for keymapstrings beginning
;;; with a "ESC-[" prefix.  These correspond to the keyboard on a
;;; sparc, ASSUMING THE DEFAULT XMODMAP, which can be altered as
;;; described above.  This alist is used to build the
;;; X-fn-keymap when this file is loaded (see next expression).
(defvar sun-keysym-keymapstring-alist 
  '(("L1" . "192z")
    ("L2" . "193z")
    ("L3" . "194z")
    ("L4" . "195z")
    ("L5" . "196z")
    ("L6" . "197z")
    ("L7" . "198z")
    ("L8" . "199z")
    ("L9" . "200z")
    ("L10" . "201z")
    ("Help" . "-1z")

    ("F1" . "224z")
    ("F2" . "225z")
    ("F3" . "226z")
    ("F4" . "227z")
    ("F5" . "228z")
    ("F6" . "229z")
    ("F7" . "230z")
    ("F8" . "231z")
    ("F9" . "232z")
    ("F10" . "233z")
    ("F11" . "234z")
    ("F12" . "235z")

    ("R1" . "208z")
    ("R2" . "209z")
    ("R3" . "210z")
    ("R4" . "211z")
    ("R5" . "212z")
    ("R6" . "213z")
    ("R7" . "214z")
    ("R8" . "215z")			;only if xmodmap is used to set it!
    ("R9" . "216z")
    ("R10" . "217z")			;only if xmodmap is used to set it!
    ("R11" . "218z")
    ("R12" . "219z")			;only if xmodmap is used to set it!
    ("R13" . "220z")
    ("R14" . "221z")			;only if xmodmap is used to set it!
    ("R15" . "222z")

    ("Break" . "223z")
    ))

(defvar X-fn-keymap (make-sparse-keymap)
  "*Keymap for ESC-[ encoded keyboard")

;;; Go through the keysym-keymapstring-alist, adding an unbound entry
;;; to X-fn-keymap for all keys that are not already there.  This
;;; way, the user can preset some of them and they will not be blown
;;; away.  It is important to have an entry for each X keymapstring,
;;; since otherwise you get echoed in your buffer!
(let ((the-map X-fn-keymap)
      (alist sun-keysym-keymapstring-alist))
  (while alist
    (cond ((symbolp (lookup-key the-map (cdr (car alist)))) ;already has an entry
	   nil)
	  ((equal (car (car alist)) "Help")
	   (define-key the-map (cdr (car alist)) 'describe-X-fn-bindings))
	  (t (define-key the-map (cdr (car alist)) 'X-fn-key-unbound)))
    (setq alist (cdr alist)))
  (define-key esc-map "[" the-map))	;install it for prefix "\M-["

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These X keysyms are automatically converted by Emacs into something useful:
;;; X keysym    emacs keymapstring
;;; ---------   ---------------
;;  BackSpace     "C-h"              
;;  LineFeed       LFD
;;  Clear         "C-k"
;;  Escape         ESC
;;  Left          "C-b"
;;  Up            "C-p"
;;  Right         "C-f"
;;  Down          "C-n"
;;  KP_Space      " "
;;  KP_Enter       CR
;;  KP_Separator  ","
;;  KP_Add        "+"
;;  KP_Subtract   "-"

;;; And these are (unfortunately) mapped to the same string as the Help keysym
;;  Select        "-1z"
;;  Print         "-1z"
;;  Execute       "-1z"
;;  Ins           "-1z"
;;  Insert        "-1z"
;;  Undo          "-1z"
;;  Redo          "-1z"
;;  Menu          "-1z"
;;  Find          "-1z"
;;  Cancel        "-1z"
;;  Help          "-1z"
;;  Mode_switch   "-1z"

;;; These keysyms are ignored (they generate nothing):
;;  Pause
;;  Multi_key
;;  Kanji
;;  Home
;;  Prior
;;  Next
;;  End  
;;  Begin 
;;  KP_Tab 
;;  Num_lock

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
