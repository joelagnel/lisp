;;; sysdep.el --- consolidate Emacs-version dependencies in one file.

;; Copyright (c) 1995 - 1997 Ben Wing.

;; Author: Ben Wing <wing@666.com>, William Perry <wmperry@cs.indiana.edu>
;; Keywords: lisp, tools
;; Version: 0.003


;; This stuff should go.  We don't have papers for it and it's not
;; good practice to define things like this.  It tends to screw other
;; packages that do feature tests with fboundp.  Compare what Gnus
;; does.  (A lot of it's probably irrelevant anyhow, especially as
;; depending on Gnus MIME stuff means dropping support for old
;; Emacsen.)  -- fx

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs.  The idea is that we make it look like we're running
;; the latest version of XEmacs (currently 19.12) by emulating all the
;; missing functions.

;; #### This file does not currently do any advising but should.
;; Unfortunately, advice.el is a hugely big package.  Is any such
;; thing as `advice-lite' possible?

;; #### - This package is great, but its role needs to be thought out a bit
;; more.  Sysdep will not permit programs written for the old XEmacs API to
;; run on new versions of XEmacs.  Sysdep is a backward-compatibility
;; package for the latest and greatest XEmacs API.  It permits programmers
;; to use the latest XEmacs functionality and still have their programs run
;; on older versions of XEmacs...perhaps even on FSF Emacs.  It should NEVER
;; ever need to be loaded in the newest XEmacs.  It doesn't even make sense
;; to put it in the lisp/utils part of the XEmacs distribution because it's
;; real purpose is to be distributed with packages like w3 which take
;; advantage of the latest and greatest features of XEmacs but still need to
;; be run on older versions.  --Stig

;; Any packages that wish to use this file should load it using
;; `load-library'.  It will not load itself if a version of sysdep.el
;; that is at least as recent has already been loaded, but will
;; load over an older version of sysdep.el.  It will attempt to
;; not redefine functions that have already been custom-redefined,
;; but will redefine a function if the supplied definition came from
;; an older version of sysdep.el.

;; Packages such as w3 that wish to include this file with the package
;; should rename it to something unique, such as `w3-sysdep.el', and
;; load it with `load-library'.  That will ensure that no conflicts
;; arise if more than one package in the load path provides a version
;; of sysdep.el.  If multiple packages load sysdep.el, the most recent
;; version will end up loaded; as long as I'm careful not to
;; introduce bugs in previously working definitions, this should work
;; fine.

;; You may well discover deficiencies in this file as you use it.
;; The preferable way of dealing with this is to send me a patch
;; to sysdep.el; that way, the collective body of knowledge gets
;; increased.

;; IMPORTANT: leave the version string in the format X.XXX (e.g. 1.001)
;; so that string comparisons to other versions work properly.

(defconst sysdep-potential-version "0.003")

;; this macro means: define the function, but only if either it
;; wasn't bound before, or the supplied binding comes from an older
;; version of sysdep.el.  That way, user-supplied bindings don't
;; get overridden.

;; note: sysdep-defalias is often more useful than this function,
;; esp. since you can do load-time conditionalizing and can
;; optionally leave the function undefined. (e.g. frame functions
;; in v18.)

(defmacro sysdep-defun (function &rest everything-else)
  (` (cond ((and (not (fboundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this
		 sysdep-potential-version)
	    (defun (, function) (,@ everything-else))))))

(defmacro sysdep-defvar (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or 
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defvar (, function) (,@ everything-else))))))

(defmacro sysdep-defconst (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defconst (, function) (,@ everything-else))))))

;; similar for fset and defalias.  No need to quote as the argument
;; is already quoted.

(defmacro sysdep-fset (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def))
	    (put (, function) 'sysdep-defined-this t)
	    (fset (, function) (, def))))))

(defmacro sysdep-defalias (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def)
		 (or (listp (, def))
		     (and (symbolp (, def))
			  (fboundp (, def)))))
	    (put (, function) 'sysdep-defined-this t)
	    (defalias (, function) (, def))))))

(sysdep-defvar list-buffers-directory nil)
(sysdep-defvar x-library-search-path `("/usr/X11R6/lib/X11/"
				      "/usr/X11R5/lib/X11/"
				      "/usr/lib/X11R6/X11/"
				      "/usr/lib/X11R5/X11/"
				      "/usr/local/X11R6/lib/X11/"
				      "/usr/local/X11R5/lib/X11/"
				      "/usr/local/lib/X11R6/X11/"
				      "/usr/local/lib/X11R5/X11/"
				      "/usr/X11/lib/X11/"
				      "/usr/lib/X11/"
				      "/usr/local/lib/X11/"
				      "/usr/X386/lib/X11/"
				      "/usr/x386/lib/X11/"
				      "/usr/XFree86/lib/X11/"
				      "/usr/unsupported/lib/X11/"
				      "/usr/athena/lib/X11/"
				      "/usr/local/x11r5/lib/X11/"
				      "/usr/lpp/Xamples/lib/X11/"
				      "/usr/openwin/lib/X11/"
				      "/usr/openwin/share/lib/X11/"
				      ,data-directory)
  "Search path used for X11 libraries.")

;; frame-related stuff.

(sysdep-defun frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.
  If FRAME is omitted, describe the currently selected frame."
  (cdr (assq parameter (frame-parameters frame))))

(sysdep-defun event-point (event)
  (let ((posn (event-end event)))
    (if posn 
 	(posn-point posn))))

(sysdep-defalias 'face-list 'list-faces)

(sysdep-defun set-keymap-parent (keymap new-parent)
  (let ((tail keymap))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

;; Property list functions
;;
(sysdep-defun plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects."
  (let ((node (memq prop plist)))
    (if node
	(setcar (cdr node) val)
      (setq plist (cons prop (cons val plist))))
    plist))

(sysdep-defun plist-get (plist prop)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
  (while (and plist (not (eq (car plist) prop)))
    (setq plist (cdr (cdr plist))))
  (and plist (car (cdr plist))))

;; misc

(sysdep-defun buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without text properties, as a string."
  (format "%s" (buffer-substring beg end)))
  
(sysdep-defun symbol-value-in-buffer (symbol buffer &optional unbound-value)
  "Return the value of SYMBOL in BUFFER, or UNBOUND-VALUE if it is unbound."
  (save-excursion
    (set-buffer buffer)
    (if (not (boundp symbol))
	unbound-value
      (symbol-value symbol))))

(sysdep-defun insert-file-contents-literally
  (file &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((file-name-handler-alist nil)
	(find-file-hooks nil))
    (insert-file-contents file visit beg end replace)))

(sysdep-defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(sysdep-defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.
TOGGLE is a symbol which is used as the variable which toggle the minor mode,
NAME is the name that should appear in the modeline (it should be a string
beginning with a space), KEYMAP is a keymap to make active when the minor
mode is active, and AFTER is the toggling symbol used for another minor
mode.  If AFTER is non-nil, then it is used to position the new mode in the
minor-mode alists.  TOGGLE-FUN specifies an interactive function that
is called to toggle the mode on and off; this affects what appens when
button2 is pressed on the mode, and when button3 is pressed somewhere
in the list of modes.  If TOGGLE-FUN is nil and TOGGLE names an
interactive function, TOGGLE is used as the toggle function.

Example:  (add-minor-mode 'view-minor-mode \" View\" view-mode-map)"
  (if (not (assq toggle minor-mode-alist))
      (setq minor-mode-alist (cons (list toggle name) minor-mode-alist)))
  (if (and keymap (not (assq toggle minor-mode-map-alist)))
      (setq minor-mode-map-alist (cons (cons toggle keymap)
				       minor-mode-map-alist))))

(sysdep-defvar x-font-regexp-foundry-and-family
  (let ((- 		"[-?]")
	(foundry		"[^-]+")
	(family 		"[^-]+")
	)
    (concat "\\`[-?*]" foundry - "\\(" family "\\)" -)))

(sysdep-defalias 'valid-color-name-p
  (cond
   ((fboundp 'x-valid-color-name-p)	; XEmacs/Lucid
    'x-valid-color-name-p)
   ((and window-system
	 (fboundp 'color-defined-p))	; NS/Emacs 19
    'color-defined-p)
   ((and window-system
	 (fboundp 'pm-color-defined-p))
    'pm-color-defined-p)
   ((and window-system
	 (fboundp 'x-color-defined-p))	; Emacs 19
    'x-color-defined-p)
   ((fboundp 'get-color)		; Epoch
    (function (lambda (color)
		(let ((x (get-color color)))
		  (if x
		      (setq x (progn
				(free-color x)
				t)))
		  x))))
   (t 'identity)))			; All others

;; Misc.

(sysdep-defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))

(sysdep-defun display-error (error-object stream)
  "Display `error-object' on `stream' in a user-friendly way."
  (funcall (or (let ((type (car-safe error-object)))
		 (catch 'error
		   (and (consp error-object)
			(symbolp type)
			;;(stringp (get type 'error-message))
			(consp (get type 'error-conditions))
			(let ((tail (cdr error-object)))
			  (while (not (null tail))
			    (if (consp tail)
				(setq tail (cdr tail))
			      (throw 'error nil)))
			  t)
			;; (check-type condition condition)
			(get type 'error-conditions)
			;; Search class hierarchy
			(let ((tail (get type 'error-conditions)))
			  (while (not (null tail))
			    (cond ((not (and (consp tail)
					     (symbolp (car tail))))
				   (throw 'error nil))
				  ((get (car tail) 'display-error)
				   (throw 'error (get (car tail)
						      'display-error)))
				  (t
				   (setq tail (cdr tail)))))
			  ;; Default method
			  (function
			   (lambda (error-object stream)
			     (let ((type (car error-object))
				   (tail (cdr error-object))
				   (first t))
			       (if (eq type 'error)
				   (progn (princ (car tail) stream)
					  (setq tail (cdr tail)))
				 (princ (or (get type 'error-message) type)
					stream))
			       (while tail
				 (princ (if first ": " ", ") stream)
				 (prin1 (car tail) stream)
				 (setq tail (cdr tail)
				       first nil)))))))))
	       (function
		(lambda (error-object stream)
		  (princ "Peculiar error " stream)
		  (prin1 error-object stream))))
	   error-object stream))

(sysdep-defun find-face (face)
  (car-safe (memq face (face-list))))

(sysdep-defun copy-tree (tree &optional vecp)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
Contrast to copy-sequence, which copies only along the cdrs.  With second
argument VECP, this copies vectors as well as conses."
  (if (consp tree)
      (let ((p (setq tree (copy-list tree))))
	(while (consp p)
	  (if (or (consp (car p)) (and vecp (vectorp (car p))))
	      (setcar p (copy-tree (car p) vecp)))
	  (or (listp (cdr p)) (setcdr p (copy-tree (cdr p) vecp)))
	  (pop p)))
    (if (and vecp (vectorp tree))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (copy-tree (aref tree i) vecp))))))
  tree)

(sysdep-defun truncate-string-to-width (str len &optional start-column pad)
  "Truncate string STR so that string-width of STR is not greater than LEN.
If width of the truncated string is less than LEN, and if a character PAD is
defined, add padding end of it."
  (concat (if (> (length str) len) (substring str 0 len) str)
	  (if (or (null pad) (> (length str) len))
	      ""
	    (make-string (- len (length str)) pad))))

; next line added 1999/07/09 blp on advice of Thierry Emery
; <Thierry.Emery@alcatel.fr>
(sysdep-defvar :button-keymap ':button-keymap)

(provide 'w3-sysdp)
;;; sysdep.el ends here

;;;(sysdep.el) Local Variables:
;;;(sysdep.el) eval: (put 'sysdep-defun 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defalias 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defconst 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defvar 'lisp-indent-function 'defun)
;;;(sysdep.el) End:
