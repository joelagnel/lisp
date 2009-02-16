;;; From nobody Tue Jan 23 17:52:45 2001
;;; x-uunet-gateway: sjc3sosrv11.alter.net from gnu-emacs-sources to gnu.emacs.sources; Tue, 23 Jan 2001 22:45:29 GMT
;;; Date: Tue, 23 Jan 2001 20:38:07 -0200
;;; Message-ID: <200101232238.UAA04072@java.cpqd.com.br>
;;; X-Authentication-Warning: java.cpqd.com.br: vinicius set sender to vinicius@java.cpqd.com.br using -f
;;; From: vinicius@cpqd.com.br (Vinicius Jose Latorre)
;;; Subject: xray.el v1.0
;;; Newsgroups: gnu.emacs.sources
;;; Path: news.wam.umd.edu!news.umd.edu!bloom-beacon.mit.edu!howland.erols.net!newsfeed.skycache.com!Cidera!news-reader.ntrnet.net!uunet!ash.uu.net!spool1.news.uu.net!wendy-fate.uu.net!gnu-emacs-sources
;;; Sender: gnu-emacs-sources-request@mail.gnu.org
;;; Lines: 1548
;;; Xref: news.wam.umd.edu gnu.emacs.sources:9229


;;; xray.el --- Display internal object structures in a temporary buffer.

;; Copyright (C) 2001 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	help, internal, maintenance, debug
;; Time-stamp:	<2001/01/23 20:18:43 vinicius>
;; Version:	1.0
;; X-URL:	http://www.cpqd.com.br/~vinicius/emacs/

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; Sometimes you need to see the internal structures to understand a situation.
;; This package provides a way to display internal Emacs object structures in a
;; temporary buffer.
;;
;; For good performance, be sure to byte-compile xray.el, e.g.
;;
;;    M-x byte-compile-file <give the path to xray.el when prompted>
;;
;; This will generate xray.elc, which will be loaded instead of xray.el.
;;
;; xray was tested with GNU Emacs 20.6.1.
;;
;; At moment, there isn't any compatibility with XEmacs.
;;
;;
;; Usage
;; -----
;;
;; To use xray, insert in your ~/.emacs:
;;
;;        (require 'xray)
;;
;; And type, for example:
;;
;;    M-x xray-symbol RET describe-function RET
;; Or:
;;    M-: (xray-symbol 'describe-function) RET
;; Or:
;;    M-x global-set-key RET C-c x xray-symbol RET C-c x describe-function RET
;;
;; The following buffer (*Symbol X-Ray*) is shown:
;;
;; ------------------------------------------------- Begin *Symbol X-Ray*
;;
;; SYMBOL
;;
;; describe-function
;;    key bindings  : C-h f    C-h d    menu-bar help-menu describe desc\
;; ribe-function
;;    file          : help
;;    function cell : *Interactive-Compiled-Lisp-Function*
;;    value cell    : void
;;    property list cell:
;;      (event-symbol-element-mask  (describe-function 0)
;;       event-symbol-elements  (describe-function)
;;       modifier-cache  ((0 . describe-function)))
;; --------------------------------------------------- End *Symbol X-Ray*
;;
;; The entries on key bindings, file and function cell are "links" to other
;; help buffers.  The key bindings (C-h f, C-h d, etc.) points to a key
;; description (if you click on C-h f, it's the same as typing C-h k C-h f),
;; the file (help) points to the position on file help.el where
;; describe-function is defined, and the function cell points to a function
;; description (if you click, it's the same as typing C-h f describe-function).
;; So, if you click on any "link", you get more related information.
;;
;; As in a help buffer, when you follow the "links", it'll appear at end of
;; buffer a `[back]' button.  You can go back by clicking with mouse-2 the
;; `[back]' button or by typing C-c C-b on xray (or help) buffer.
;;
;;
;; Objects
;; -------
;;
;; The following objects may be shown:
;;
;;    + Symbol (`xray-symbol'):
;;	 It's displayed the symbol name cell, the symbol function cell, the
;;	 symbol value cell, the symbol property list cell and the key bindings
;;	 associated with symbol (if any) and from which file it was loaded.
;;
;;    + Position (`xray-position'):
;;	 It's displayed the frame, the window, the buffer, the word (if any)
;;	 around position, the character width, the character at position, the
;;	 charset, the text property list, the default text property list and
;;	 the overlay list.
;;
;;    + Buffer (`xray-buffer'):
;;	 It's displayed the frame, the window, the base buffer (if it's an
;;	 indirect buffer), buffer name, buffer size, minimum point, point,
;;	 maximum point, the mark, the mark active flag, file name visited (if
;;	 any), file modification time, the modified flag, the read only flag,
;;	 multibyte flag, inhibit read flag, display table, window list, buffer
;;	 list, hooks related to buffers, mark ring, overlay list and local
;;	 variables.
;;
;;    + Window (`xray-window'):
;;	 It's displayed the frame associated, the buffer associated, the
;;	 window, the height, the width, the edges, the buffer position, the
;;	 window start, the window end, the liveness flag, the dedicated flag,
;;	 the minibuffer flag, the horizontal scrolling amount, display table,
;;	 some window related variables, the hooks, the window least recently
;;	 selected, the largest window area and the window list.
;;
;;    + Frame (`xray-frame'):
;;	 It's displayed the frame, frame height, frame width, pixel frame
;;	 height, pixel frame width, pixel char height, pixel char width,
;;	 liveness flag, visibility flag, the first window on frame, the
;;	 selected window, the root window, some variables related to frame, the
;;	 frame parameters, the hooks, the frame list, the visible frame list
;;	 and display list.
;;
;;    + Marker (`xray-marker'):
;;	 It's displayed the buffer associated, the position, the insertion
;;	 type, the mark, the beginning of region, the end of region, some
;;	 variable related to marker, hooks and the mark ring.
;;
;;    + Overlay (`xray-overlay'):
;;	 It's displayed the buffer associated, the start position, the end
;;	 position, the overlay list and the property list.
;;
;;    + Screen (`xray-screen'):
;;	 It's displayed the screen capabilities, some variables and hooks
;;	 related to screen, and the display list.
;;
;;    + Faces (`xray-faces'):
;;	 It's displayed all defined faces.
;;
;;    + Hooks (`xray-hooks'):
;;	 It's displayed all standard hooks and other defined hooks.
;;
;;    + Features (`xray-features'):
;;	 It's displayed all features loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of xray options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `xray-property-alist'		Specify association between property
;;					symbol and a display function.
;;
;; `xray-property-recursive-list'	Specify property list which can be
;;					display recursively.
;;
;; `xray-maximum-depth'			Specify maximum display recursive
;;					depth.
;;
;; `xray-value-threshold'		Specify maximum value data length to
;;					display.
;;
;; `xray-buffer-name'			Specify x-ray buffer name.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq xray-property-alist '((some-prop . display-some-prop)))
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET xray-property-alist RET
;;	 '((some-prop . display-some-prop)) RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Development* group,
;;	 expand *Internal* group,
;;	 expand *Xray* group
;;	 and then customize xray options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v xray-property-alist RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x xray-customize RET
;;
;;    and then customize xray options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User variables


(defgroup xray nil
  "X-ray objects"
  :link '(emacs-library-link :tag "Source Lisp File" "xray.el")
  :prefix "xray-"
  :group 'internal
  :group 'maintenance
  :group 'debug)


(defcustom xray-property-alist
  '((widget-type . xray-widget-type)
    (custom-type . xray-custom-type))
  "*Specify association between property symbol and a display function.

Each element has the following form:

   (PROPERTY-SYMBOL . DISPLAY-FUNCTION)

Where:

PROPERTY-SYMBOL		property symbol which it's to be displayed in a special
			format.

DISPLAY-FUNCTION	function symbol that it'll be called to display
			PROPERTY-SYMBOL.  This function is invoked with 2
			arguments: PROPERTY-SYMBOL and the value associated.
			For an example, see `xray-widget-type' function."
  :type '(repeat :tag "Xray Property Alist"
		 (cons :tag ""
		       (symbol :tag "Xray Property Symbol")
		       (function :tag "Xray Property Display")))
  :group 'xray)


(defcustom xray-property-recursive-list
  '(widget-type)
  "*Specify property list which can be displayed recursively."
  :type '(repeat :tag "Xray Property Recursive"
		 (symbol :tag "Xray Property Symbol"))
  :group 'xray)


(defcustom xray-maximum-depth 100
  "*Specify maximum display recursive depth.

If you don't want to display recursively, set to 0 or a lesser integer.

Circularity is checked.  So, it's avoided a redisplay of a symbol property
already displayed."
  :type 'integer
  :group 'xray)


(defcustom xray-value-threshold 1024
  "*Specify maximum value data length to display.

If it's an integer greater than zero and the value converted to a string has a
length greater than `xray-value-threshold', display:

   \"\"...	if the value is a string
   ()...	if the value is a list
   []...	if the value is a vector
   ##...	any other value type

If it's not an integer or it's an integer less than or equal to zero, display
all data value."
  :type 'integer
  :group 'xray)


(defcustom xray-buffer-name nil
  "*Specify x-ray buffer name.

Valid values are:

   nil		This means that each object will have a buffer name.  For
		example, the buffer name `*Buffer X-Ray*' will be used for
		buffer objects, the buffer `*Symbol X-Ray*' will be used for
		symbol objects, etc.

   string	This means that all objects will use the same buffer name."
  :type '(choice :menu-tag "X-Ray Buffer Name"
		 :tag "X-Ray Buffer Name"
		 (const :tag "Different Buffer Name For Each Object" nil)
		 (string :tag "Unique Buffer Name"))
  :group 'xray)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro


(defmacro xray-excursion (buffer-name &rest body)
  `(with-output-to-temp-buffer (or xray-buffer-name ,buffer-name)
     (save-excursion
       (save-match-data
	 (set-buffer standard-output)
	 (let ((buffer-undo-list t)
	       (inhibit-read-only t)
	       (inhibit-point-motion-hooks t)
	       before-change-functions
	       after-change-functions
	       deactivate-mark
	       buffer-file-name
	       buffer-file-truename
	       inhibit-quit)
	   (erase-buffer)
	   ,@body
	   (help-mode))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun xray-customize ()
  "Customize xray group."
  (interactive)
  (customize-group 'xray))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User command


;;;###autoload
(defun xray-symbol (symbol)
  "Display SYMBOL internal cells in a temporary buffer.

That is, it's displayed the symbol name cell, the symbol function cell, the
symbol value cell and the symbol property list cell.  It's also displayed the
key bindings associated with symbol (if any) and from which file it was loaded.

See `xray-customize' for customization."
  (interactive (xray-interactive-prompt-symbol))
  (or (symbolp symbol)
      (error "It's not a symbol: %S" symbol))
  (setq help-xref-stack-item (list 'xray-symbol symbol))
  (xray-excursion
   "*Symbol X-Ray*"
   (let ((current (list symbol))
	 (depth (if (integerp xray-maximum-depth)
		    xray-maximum-depth
		  0))
	 visited)
     (insert "\nSYMBOL\n")
     (while (let (new)
	      (mapcar #'(lambda (sym)
			  (and sym (not (memq sym visited))
			       (progn
				 (xray-display-symbol sym)
				 (setq visited (cons sym visited)
				       new (append (xray-property-in-list sym)
						   new)))))
		      current)
	      (and (>= (setq depth (1- depth)) 0)
		   (setq current new)))))))


;;;###autoload
(defun xray-position (&optional position buffer)
  "Display POSITION internal cells in a temporary buffer.

If POSITION is nil, it's used (point).
If BUFFER is nil, it's used (current-buffer).

That is, it's displayed the frame, the window, the buffer, the word (if any)
around POSITION, the character width, the character at POSITION, the charset,
the text property list, the default text property list and the overlay list."
  (interactive)
  (or position
      (setq position (point)))
  (or (integer-or-marker-p position)
      (error "It's not a position: %S" position))
  (or buffer
      (setq buffer (current-buffer)))
  (or (bufferp buffer)
      (error "It's not a buffer: %S" buffer))
  (setq help-xref-stack-item (list 'xray-position position buffer))
  (xray-display-position position buffer))


;;;###autoload
(defun xray-buffer (&optional buffer)
  "Display BUFFER internal cells in a temporary buffer.

If BUFFER is nil, it's used (current-buffer).

That is, it's displayed the frame, the window, the base buffer (if it's an
indirect buffer), buffer name, buffer size, minimum point, point, maximum
point, the mark, the mark active flag, file name visited (if any), file
modification time, the modified flag, the read only flag, multibyte flag,
inhibit read flag, display table, window list, buffer list, hooks related to
buffers, mark ring, overlay list and local variables."
  (interactive)
  (or buffer
      (setq buffer (current-buffer)))
  (or (bufferp buffer)
      (error "It's not a buffer: %S" buffer))
  (setq help-xref-stack-item (list 'xray-buffer buffer))
  (xray-display-buffer buffer))


;;;###autoload
(defun xray-window (&optional window)
  "Display WINDOW internal cells in a temporary buffer.

If WINDOW is nil, it's used (selected-window).

That is, it's displayed the frame associated, the buffer associated, the
window, the height, the width, the edges, the buffer position, the window
start, the window end, the liveness flag, the dedicated flag, the minibuffer
flag, the horizontal scrolling amount, display table, some window related
variables, the hooks, the window least recently selected, the largest window
area and the window list."
  (interactive)
  (or window
      (setq window (selected-window)))
  (or (windowp window)
      (error "It's not a window: %S" window))
  (setq help-xref-stack-item (list 'xray-window window))
  (xray-display-window window))


;;;###autoload
(defun xray-frame (&optional frame)
  "Display FRAME internal cells in a temporary buffer.

If FRAME is nil, it's used (selected-frame).

That is, it's displayed the frame, frame height, frame width, pixel frame
height, pixel frame width, pixel char height, pixel char width, liveness flag,
visibility flag, the first window on frame, the selected window, the root
window, some variables related to frame, the frame parameters, the hooks, the
frame list, the visible frame list and display list."
  (interactive)
  (or frame
      (setq frame (selected-frame)))
  (or (framep frame)
      (error "It's not a frame: %S" frame))
  (setq help-xref-stack-item (list 'xray-frame frame))
  (xray-display-frame frame))


;;;###autoload
(defun xray-marker (&optional marker)
  "Display MARKER internal cells in a temporary buffer.

If MARKER is nil, it's used (mark t).

That is, it's displayed the buffer associated, the position, the insertion
type, the mark, the beginning of region, the end of region, some variable
related to marker, hooks and the mark ring."
  (interactive)
  (or marker
      (setq marker (mark-marker)))
  (cond ((markerp marker)
	 (or (marker-buffer marker)
	     (error "There is no marker in current buffer"))
	 (setq help-xref-stack-item (list 'xray-marker marker))
	 (xray-display-marker marker))
	((null marker)
	 (error "There is no marker in current buffer"))
	(t
	 (error "It's not a marker: %S" marker))
	))


;;;###autoload
(defun xray-overlay (&optional overlay)
  "Display OVERLAY internal cells in a temporary buffer.

If OVERLAY is nil, try to use the overlay on current buffer position (if any).

That is, it's displayed the buffer associated, the start position, the end
position, the overlay list and the property list."
  (interactive)
  (or overlay
      (setq overlay (car (overlays-at (point)))))
  (cond ((overlayp overlay)
	 (setq help-xref-stack-item (list 'xray-overlay overlay))
	 (xray-display-overlay overlay))
	((null overlay)
	 (error "There is no overlay at position %d" (point)))
	(t
	 (error "It's not an overlay: %S" overlay))
	))


;;;###autoload
(defun xray-screen (&optional screen)
  "Display SCREEN capabilities in a temporary buffer.

If SCREEN is nil, use the first screen given by `x-display-list'.

That's, it's displayed SCREEN capabilities, some variables and hooks related to
screen, and the display list."
  (interactive)
  (setq help-xref-stack-item (list 'xray-screen screen))
  (xray-display-screen (or screen (car (x-display-list)))))


;; list-faces-display - hacked from faces.el
;;;###autoload
(defun xray-faces ()
  "Display all defined faces."
  (interactive)
  (setq help-xref-stack-item '(xray-faces))
  (xray-excursion
   "*Face X-Ray*"
   (insert "\nFACES\n")
   (let ((faces (xray-sort (face-list))))
     (setq truncate-lines t)
     (while faces
       (let ((face (car faces))
	     (beg  (+ (point) 2)))
	 (setq faces (cdr faces))
	 (insert "\n ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz  ")
	 (put-text-property beg (- (point) 2) 'face face)
	 (xray-symbol-button face)))
     ;; If the *Face X-Ray* buffer appears in a different frame, copy all the
     ;; face definitions from FRAME, so that the display will reflect the frame
     ;; that was selected.
     (let* ((window     (get-buffer-window (get-buffer "*Face X-Ray*") t))
	    (disp-frame (if window
			    (window-frame window)
			  (car (frame-list))))
	    (frame      (selected-frame)))
       (or (eq frame disp-frame)
	   (let ((faces (face-list)))
	     (while faces
	       (copy-face (car faces) (car faces) frame disp-frame)
	       (setq faces (cdr faces)))))))))


;;;###autoload
(defun xray-hooks ()
  "Display all standard hooks and other defined hooks."
  (interactive)
  (setq help-xref-stack-item '(xray-hooks))
  (let ((standard-hooks
	 '(activate-mark-hook
	   after-change-function
	   after-change-functions
	   after-init-hook
	   after-insert-file-functions
	   after-make-frame-hook
	   after-revert-hook
	   after-save-hook
	   auto-fill-function
	   auto-save-hook
	   before-change-function
	   before-change-functions
	   before-init-hook
	   before-make-frame-hook
	   before-revert-hook
	   blink-paren-function
	   buffer-access-fontify-functions
	   c-mode-hook
	   calendar-load-hook
	   change-major-mode-hook
	   command-history-hook
	   command-line-functions
	   comment-indent-function
	   deactivate-mark-hook
	   diary-display-hook
	   diary-hook
	   dired-mode-hook
	   disabled-command-hook
	   echo-area-clear-hook
	   edit-picture-hook
	   electric-buffer-menu-mode-hook
	   electric-command-history-hook
	   electric-help-mode-hook
	   emacs-lisp-mode-hook
	   find-file-hooks
	   find-file-not-found-hooks
	   first-change-hook
	   fortran-comment-hook
	   fortran-mode-hook
	   ftp-setup-write-file-hooks
	   ftp-write-file-hook
	   indent-mim-hook
	   initial-calendar-window-hook
	   kill-buffer-hook
	   kill-buffer-query-functions
	   kill-emacs-hook
	   kill-emacs-query-functions
	   LaTeX-mode-hook
	   ledit-mode-hook
	   lisp-indent-function
	   lisp-interaction-mode-hook
	   lisp-mode-hook
	   list-diary-entries-hook
	   local-write-file-hooks
	   m2-mode-hook
	   mail-mode-hook
	   mail-setup-hook
	   mark-diary-entries-hook
	   medit-mode-hook
	   menu-bar-update-hook
	   minibuffer-setup-hook
	   minibuffer-exit-hook
	   news-mode-hook
	   news-reply-mode-hook
	   news-setup-hook
	   nongregorian-diary-listing-hook
	   nongregorian-diary-marking-hook
	   nroff-mode-hook
	   outline-mode-hook
	   plain-TeX-mode-hook
	   post-command-hook
	   pre-abbrev-expand-hook
	   pre-command-hook
	   print-diary-entries-hook
	   prolog-mode-hook
	   protect-innocence-hook
	   redisplay-end-trigger-functions
	   rmail-edit-mode-hook
	   rmail-mode-hook
	   rmail-summary-mode-hook
	   scheme-indent-hook
	   scheme-mode-hook
	   scribe-mode-hook
	   shell-mode-hook
	   shell-set-directory-error-hook
	   suspend-hook
	   suspend-resume-hook
	   temp-buffer-show-function
	   term-setup-hook
	   terminal-mode-hook
	   terminal-mode-break-hook
	   TeX-mode-hook
	   text-mode-hook
	   today-visible-calendar-hook
	   today-invisible-calendar-hook
	   vi-mode-hook
	   view-hook
	   window-configuration-change-hook
	   window-scroll-functions
	   window-setup-hook
	   window-size-change-functions
	   write-contents-hooks
	   write-file-hooks
	   write-region-annotate-functions))
	hooks)
    (mapatoms #'(lambda (sym)
		  (and (boundp sym)
		       (string-match "-hook$\\|-functions$" (symbol-name sym))
		       (not (memq sym standard-hooks))
		       (setq hooks (cons sym hooks)))))
    (xray-excursion
     "*Hook X-Ray*"
     (insert "\nHOOKS\n")
     (xray-display-hook "standard hooks" standard-hooks)
     (insert "\n")
     (xray-display-hook "other hooks" (xray-sort hooks)))))


;;;###autoload
(defun xray-features ()
  "Display all features loaded."
  (interactive)
  (setq help-xref-stack-item '(xray-features))
  (xray-excursion
   "*Features X-Ray*"
   (insert "\nFEATURES\n\n ")
   (xray-variable-button 'c-emacs-features)
   (insert "\n")
   (xray-display-list "features" (xray-sort features) #'xray-symbol-button)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


;; interactive part of describe-function - hacked from help.el
(defun xray-interactive-prompt-symbol ()
  (let ((sym (xray-symbol-at-point))
	(enable-recursive-minibuffers t)	     
	val)
    (setq val (completing-read (if sym
				   (format "X-ray symbol (default %s): " sym)
				 "X-ray symbol: ")
			       obarray 'symbolp t nil nil (symbol-name sym)))
    (list (if (equal val "")
	      sym
	    (intern val)))))


;; function-at-point - hacked from help.el
(defun xray-symbol-at-point ()
  (let ((stab (syntax-table)))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (unwind-protect
	(let ((obj
	       (or (condition-case ()
		       (save-excursion
			 (or (not (zerop (skip-syntax-backward "_w")))
			     (eq (char-syntax (following-char)) ?w)
			     (eq (char-syntax (following-char)) ?_)
			     (forward-sexp -1))
			 (skip-chars-forward "'")
			 (read (current-buffer)))
		     (error nil))
		   (condition-case ()
		       (save-excursion
			 (save-restriction
			   (narrow-to-region (max (point-min)
						  (- (point) 1000))
					     (point-max))
			   ;; Move up to surrounding paren, then after the
			   ;; open.
			   (backward-up-list 1)
			   (forward-char 1)
			   (read (current-buffer))))
		     (error nil)))))
	  (and (symbolp obj) obj))
      (set-syntax-table stab))))


(defun xray-sort (slist)
  (sort slist #'(lambda (a b)
		  (string< (symbol-name (if (listp a) (car a) a))
			   (symbol-name (if (listp b) (car b) b))))))


(defun xray-word-at-point ()
  (and (or (= (char-syntax (following-char)) ?w)
	   (= (char-syntax (following-char)) ?_))
       (save-excursion
	 (buffer-substring-no-properties
	  (progn (skip-syntax-backward "_w") (point))
	  (progn (skip-syntax-forward "_w") (point))))))


(defun xray-property-in-list (sym)
  (mapcar #'(lambda (prop)
	      (let ((value (get sym prop)))
		(and (listp value)
		     (setq value (car value)))
		(and (symbolp value)
		     value)))
	  xray-property-recursive-list))


(defun xray-display-symbol (symbol)
  (insert "\n" (symbol-name symbol))
  (xray-label-line "  key bindings" 17) (xray-binding-button symbol)
  (xray-label-line "  file" 17) (xray-file-button symbol)
  (xray-insert-line "  function cell" 17 (xray-cell-function symbol))
  (xray-xref-button "cell\\s-+: \\*\\(.+\\)\\*$" #'xray-describe-function symbol)
  (xray-label-line "  value cell" 17)
  (xray-cell-value symbol)
  (xray-xref-button ": \\*\\(.+\\)\\*  " #'xray-describe-variable symbol)
  (xray-property-list "  property list cell" 5 (symbol-plist symbol)))


(defun xray-display-position (position buffer)
  (save-excursion
    (set-buffer buffer)
    (setq position (max (min position (point-max)) (point-min)))
    (let ((properties (text-properties-at position))
	  (window     (get-buffer-window buffer))
	  word info char)
      (save-excursion
	(goto-char position)
	(setq char (following-char)
	      word (or (xray-word-at-point) "")
	      info (xray-what-cursor-position)))
      (xray-excursion
       "*Position X-Ray*"
       (insert "\nPOSITION\n")
       (xray-frame-line "frame" 12 (and window (window-frame window)))
       (xray-window-line "window" 12 window)
       (xray-buffer-line "buffer" 12 buffer)
       (xray-label-line "word" 12)
       (cond ((string= word "")
	      (insert "*No Word*"))
	     ((intern-soft word)
	      (xray-symbol-button (intern-soft word)))
	     (t
	      (insert word)))
       (xray-object-line "char width" 12 (char-width char))
       (xray-insert-line "character" 12 (cdr info))
       (xray-insert-line "position" 12 (car info))
       (xray-property-list "text property list" 4 properties)
       (xray-property-list 'default-text-properties 4
			   default-text-properties)
       (xray-overlay-list "overlay list" (overlays-at position))))))


(defun xray-display-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((list-buffer   list-buffers-directory)
	  (file-modtime  (visited-file-modtime))
	  (point         (point))
	  (point-min     (point-min))
	  (point-max     (point-max))
	  (size          (buffer-size))
	  (the-mark      (mark-marker))
	  (markers       mark-ring)
	  (marker-p      mark-active)
	  (overlays      (overlay-lists))
	  (inhibit-read  inhibit-read-only)
	  (display-table buffer-display-table)
	  (multibyte     enable-multibyte-characters)
	  (window        (get-buffer-window buffer)))
      (xray-excursion
       "*Buffer X-Ray*"
       (insert "\nBUFFER\n")
       (xray-frame-line "frame" 15 (and window (window-frame window)))
       (xray-window-line "window" 15 window)
       (xray-buffer-line "base buffer" 15 (buffer-base-buffer buffer))
       (xray-object-line "buffer" 15 buffer)
       (xray-insert-line "buffer name" 15 (buffer-name buffer))
       (xray-object-line "buffer size" 15 size)
       (xray-point-line "point min" 15 point-min buffer)
       (xray-point-line "point" 15 point buffer)
       (xray-point-line "point max" 15 point-max buffer)
       (xray-marker-line "the mark" 15 the-mark)
       (xray-object-line "mark active" 15 marker-p)
       (xray-insert-line "file name" 15 (or (buffer-file-name buffer)
					    "*No File*"))
       (xray-object-line "file modtime" 15 file-modtime)
       (xray-object-line "list buffer" 15
			 (or list-buffer "*No List Buffer*") t)
       (xray-object-line "modified flag" 15 (buffer-modified-p buffer))
       (xray-object-line "modified tick" 15 (buffer-modified-tick buffer))
       (xray-object-line "multibyte" 15 multibyte)
       (xray-object-line "inhibit read" 15 inhibit-read)
       (xray-object-line "display table" 15 display-table)
       (insert "\n")
       (xray-display-list "window list" (get-buffer-window-list buffer)
			  #'xray-window-button)
       (xray-display-list "buffer list" (buffer-list) #'xray-buffer-button)
       (xray-display-hook "hooks" '(kill-buffer-hook
				    kill-buffer-query-functions
				    buffer-offer-save))
       (xray-display-list "marker ring" markers #'xray-marker-button)
       (xray-display-list "overlay list"
			  (nconc (car overlays) (cdr overlays))
			  #'xray-overlay-button)
       (xray-display-list "local variables"
			  (xray-sort (buffer-local-variables buffer)))))))


(defun xray-display-window (window)
  (let* ((frame  (window-frame window))
	 (buffer (window-buffer window))
	 (start  (window-start window))
	 (end    (condition-case data
		     (window-end window t)
		   (error start))))
    (xray-excursion
     "*Window X-Ray*"
     (insert "\nWINDOW\n")
     (xray-frame-line "frame" 33 frame)
     (xray-buffer-line "buffer" 33 buffer)
     (xray-window-line "window" 33 window)
     (xray-object-line "height" 33 (window-height window))
     (xray-object-line "width" 33 (window-width window))
     (xray-object-line "edges" 33 (window-edges window))
     (xray-point-line "point" 33 (window-point window) buffer)
     (xray-point-line "start" 33 start buffer)
     (xray-point-line "end" 33 end buffer)
     (xray-object-line "is live" 33 (window-live-p window))
     (xray-object-line "is dedicated" 33 (window-dedicated-p window))
     (xray-object-line "is minibuffer" 33 (window-minibuffer-p window))
     (xray-object-line "leftward horizontal scrolling" 33
		       (window-hscroll window))
     (xray-object-line "display table" 33 (window-display-table window))
     (insert "\n")
     (xray-symbol-line 'pop-up-windows 33)
     (xray-symbol-line 'split-height-threshold 33)
     (xray-symbol-line 'same-window-buffer-names 33)
     (xray-symbol-line 'same-window-regexps 33 nil t)
     (xray-symbol-line 'display-buffer-function 33)
     (xray-symbol-line 'other-window-scroll-buffer 33)
     (xray-symbol-line 'scroll-margin 33)
     (xray-symbol-line 'scroll-conservatively 33)
     (xray-symbol-line 'scroll-step 33)
     (xray-symbol-line 'scroll-preserve-screen-position 33)
     (xray-symbol-line 'next-screen-context-lines 33)
     (xray-symbol-line 'window-min-height 33)
     (xray-symbol-line 'window-min-width 33)
     (insert "\n")
     (xray-display-hook "hooks" '(window-scroll-functions
				  window-size-change-functions
				  redisplay-end-trigger-functions
				  window-configuration-change-hook))
     (xray-window-line "least recently selected" 25 (get-lru-window frame))
     (xray-window-line "largest window area" 25 (get-largest-window frame))
     (insert "\n")
     (xray-display-list "window list"
			(let (windows)
			  (walk-windows #'(lambda (win)
					    (setq windows (cons win windows)))
					t t)
			  (nreverse windows))
			#'xray-window-button))))


(defun xray-display-frame (frame)
  (xray-excursion
   "*Frame X-Ray*"
   (insert "\nFRAME\n")
   (xray-frame-line "frame" 36 frame)
   (xray-object-line "frame height" 36 (frame-height frame))
   (xray-object-line "frame width" 36 (frame-width frame))
   (xray-object-line "pixel height" 36 (frame-pixel-height frame))
   (xray-object-line "pixel width" 36 (frame-pixel-width frame))
   (xray-object-line "char height" 36 (frame-char-height frame))
   (xray-object-line "char width" 36 (frame-char-width frame))
   (xray-object-line "is live" 36 (frame-live-p frame))
   (xray-object-line "is visible" 36 (frame-visible-p frame))
   (xray-window-line "first window" 36 (frame-first-window frame))
   (xray-window-line "selected window" 36 (frame-selected-window frame))
   (xray-window-line "root window" 36 (frame-root-window frame))
   (insert "\n")
   (xray-symbol-line 'default-frame-alist 36)
   (xray-symbol-line 'default-minibuffer-frame 36)
   (xray-symbol-line 'focus-follows-mouse 36)
   (xray-symbol-line 'frame-background-mode 36)
   (xray-symbol-line 'frame-creation-function 36)
   (xray-symbol-line 'frame-initial-frame-alist 36)
   (xray-symbol-line 'frame-initial-geometry-arguments 36)
   (xray-symbol-line 'frame-title-format 36)
   (xray-symbol-line 'icon-title-format 36)
   (xray-symbol-line 'initial-frame-alist 36)
   (xray-symbol-line 'minibuffer-auto-raise 36)
   (xray-symbol-line 'minibuffer-frame-alist 36)
   (xray-symbol-line 'multiple-frames 36)
   (xray-symbol-line 'pop-up-frame-alist 36)
   (xray-symbol-line 'pop-up-frame-function 36)
   (xray-symbol-line 'pop-up-frames 36)
   (xray-symbol-line 'resize-minibuffer-frame-exactly 36)
   (xray-symbol-line 'resize-minibuffer-frame-max-height 36)
   (xray-symbol-line 'selection-coding-system 36)
   (xray-symbol-line 'x-pointer-shape 36)
   (xray-symbol-line 'x-sensitive-text-pointer-shape 36)
   (insert "\n")
   (xray-display-list "parameters" (xray-sort (frame-parameters frame)))
   (xray-display-hook "hooks" '(after-make-frame-hook
				before-make-frame-hook))
   (xray-display-list "frame list" (frame-list) #'xray-frame-button)
   (xray-display-list "visible frame list" (visible-frame-list)
		      #'xray-frame-button)
   (xray-display-list "display list" (x-display-list) #'xray-screen-button)))


(defun xray-display-screen (screen)
  (xray-excursion
   "*Screen X-Ray*"
   (insert "\nSCREEN\n")
   (xray-object-line "number of screens" 32 (x-display-screens screen))
   (xray-object-line "server version" 32 (x-server-version screen))
   (xray-object-line "server vendor" 32 (x-server-vendor screen))
   (xray-object-line "screen pixel height" 32 (x-display-pixel-height screen))
   (xray-object-line "screen mm height" 32 (x-display-mm-height screen))
   (xray-object-line "screen pixel width" 32 (x-display-pixel-width screen))
   (xray-object-line "screen mm width" 32 (x-display-mm-width screen))
   (xray-object-line "backing store capability" 32
		     (x-display-backing-store screen))
   (xray-object-line "screen visual class" 32
		     (condition-case data
			 (x-display-visual-class screen)
		       (error
			'unknown)))
   (xray-object-line "has SaveUnder feature" 32 (x-display-save-under screen))
   (xray-object-line "can display shades of gray" 32
		     (x-display-grayscale-p screen))
   (xray-object-line "is color screen" 32 (x-display-color-p screen))
   (xray-object-line "number of planes" 32 (x-display-planes screen))
   (xray-object-line "number of color cells" 32 (x-display-color-cells screen))
   (insert "\n")
   (xray-symbol-line 'blink-matching-delay 32)
   (xray-symbol-line 'blink-matching-paren 32)
   (xray-symbol-line 'blink-matching-paren-distance 32)
   (xray-symbol-line 'blink-paren-function 32)
   (xray-symbol-line 'buffer-invisibility-spec 32)
   (xray-symbol-line 'cache-long-line-scans 32)
   (xray-symbol-line 'cursor-in-echo-area 32)
   (xray-symbol-line 'default-ctl-arrow 32)
   (xray-symbol-line 'default-truncate-lines 32)
   (xray-symbol-line 'defining-kbd-macro 32)
   (xray-symbol-line 'echo-keystrokes 32)
   (xray-symbol-line 'glyph-table 32)
   (xray-symbol-line 'inverse-video 32)
   (xray-symbol-line 'last-kbd-macro 32)
   (xray-symbol-line 'message-log-max 32)
   (xray-symbol-line 'mode-line-inverse-video 32)
   (xray-symbol-line 'no-redraw-on-reenter 32)
   (xray-symbol-line 'overlay-arrow-position 32)
   (xray-symbol-line 'overlay-arrow-string 32)
   (xray-symbol-line 'ring-bell-function 32)
   (xray-symbol-line 'selective-display 32)
   (xray-symbol-line 'selective-display-ellipses 32)
   (xray-symbol-line 'special-display-buffer-names 32)
   (xray-symbol-line 'special-display-frame-alist 32)
   (xray-symbol-line 'special-display-function 32)
   (xray-symbol-line 'special-display-regexps 32)
   (xray-symbol-line 'standard-display-table 32)
   (xray-symbol-line 'system-key-alist 32)
   (xray-symbol-line 'tab-width 32)
   (xray-symbol-line 'temp-buffer-show-function 32)
   (xray-symbol-line 'truncate-partial-width-windows 32)
   (xray-symbol-line 'visible-bell 32)
   (xray-symbol-line 'window-system 32)
   (insert "\n")
   (xray-display-hook "hooks" '(echo-area-clear-hook
				temp-buffer-show-hook
				window-setup-hook))
   (xray-display-list "display list" (x-display-list) #'xray-screen-button)))


(defun xray-display-marker (marker)
  (let ((buffer (marker-buffer marker)))
    (save-excursion
      (set-buffer buffer)
      (let* ((position (marker-position marker))
	     (ins-type (marker-insertion-type marker))
	     (the-mark (mark-marker))
	     (reg-beg  (and the-mark (region-beginning)))
	     (reg-end  (and the-mark (region-end)))
	     (marker-p mark-active)
	     (markers  mark-ring)
	     (max-ring mark-ring-max))
	(xray-excursion
	 "*Marker X-Ray*"
	 (insert "\nMARKER\n")
	 (xray-buffer-line "buffer" 31 buffer)
	 (xray-point-line "position" 31 position buffer)
	 (xray-insert-line "insertion type" 31 (format "%S" ins-type))
	 (xray-marker-line "the mark" 31 the-mark)
	 (xray-point-line "region beginning" 31 reg-beg buffer)
	 (xray-point-line "region end" 31 reg-end buffer)
	 (insert "\n")
	 (xray-symbol-line 'transient-mark-mode 31)
	 (xray-symbol-line 'highlight-nonselected-windows 31)
	 (xray-symbol-line 'mark-even-if-inactive 31)
	 (xray-symbol-line 'deactivate-mark 31)
	 (xray-symbol-line 'mark-active 31)
	 (xray-symbol-line 'mark-ring-max 31 max-ring)
	 (insert "\n")
	 (xray-display-hook "hooks" '(activate-mark-hook deactivate-mark-hook))
	 (insert "\n ")
	 (xray-symbol-button 'mark-ring)
	 (xray-display-list "" markers #'xray-marker-button))))))


(defun xray-display-overlay (overlay)
  (let ((buffer (overlay-buffer overlay)))
    (save-excursion
      (set-buffer buffer)
      (let ((overlays (overlay-lists)))
	(xray-excursion
	 "*Overlay X-Ray*"
	 (insert "\nOVERLAY\n")
	 (xray-buffer-line "buffer" 8 buffer)
	 (xray-point-line "start" 8 (overlay-start overlay) buffer)
	 (xray-point-line "end" 8 (overlay-end overlay) buffer)
	 (insert "\n")
	 (xray-display-list "overlay list"
			    (nconc (car overlays) (cdr overlays))
			    #'xray-overlay-button)
	 (xray-property-list "property list" 4
			     (overlay-properties overlay)))))))


(defun xray-label-line (label column)
  (insert "\n " label)
  (move-to-column column t)
  (insert ": "))


(defun xray-insert-line (label column string)
  (xray-label-line label column)
  (insert string))


(defun xray-object-line (label column object &optional string-p)
  (xray-insert-line label column
		    (if (and string-p (stringp object))
			object
		      (format "%S" object))))


(defun xray-symbol-line (symbol column &optional value-default pp)
  (insert "\n  ")
  (save-excursion
    (forward-char -1)
    (xray-symbol-button symbol))
  (move-to-column column t)
  (insert ": ")
  (let ((value (or value-default (symbol-value symbol))))
    (if pp
	(xray-pp-value value t)
      (insert (format "%S" value)))))


(defun xray-point-line (label column point buffer)
  (xray-label-line label column)
  (xray-point-button point buffer))


(defun xray-marker-line (label column marker)
  (xray-label-line label column)
  (xray-marker-button marker))


(defun xray-frame-line (label column frame)
  (xray-label-line label column)
  (xray-frame-button frame))


(defun xray-window-line (label column window)
  (xray-label-line label column)
  (xray-window-button window))


(defun xray-buffer-line (label column buffer)
  (xray-label-line label column)
  (xray-buffer-button buffer))


(defun xray-frame-button (frame)
  (xray-object-button frame #'xray-frame "Frame"))


(defun xray-screen-button (screen)
  (xray-object-button screen #'xray-screen "Screen"))


(defun xray-window-button (window)
  (xray-object-button window #'xray-window "Window"))


(defun xray-buffer-button (buffer)
  (xray-object-button buffer #'xray-buffer "Buffer"))


(defun xray-marker-button (the-mark)
  (xray-object-button the-mark #'xray-marker "Marker"))


(defun xray-overlay-button (overlay)
  (xray-object-button overlay #'xray-overlay "Overlay"))


(defun xray-object-button (object function no-object)
  (if (not object)
      (insert "*No " no-object "*")
    (insert (format "%S" object))
    (xray-xref-button "\\(#<[^>]+>\\)" function object)))


(defun xray-point-button (point buffer)
  (if (not (integer-or-marker-p point))
      (insert "*No Position*")
    (insert (number-to-string point))
    (xray-xref-button " \\([0-9]+\\)$" #'xray-position (list point buffer))))


(defun xray-symbol-button (symbol)
  (xray-xref-string-button (symbol-name symbol) symbol #'xray-symbol))


(defun xray-variable-button (variable)
  (xray-xref-string-button (symbol-name variable) variable
			   #'xray-describe-variable))


(defun xray-function-button (func)
  (xray-xref-string-button (symbol-name func) func #'xray-describe-function))


(defun xray-binding-button (symbol)
  (let ((bindings (where-is-internal symbol)))
    (if (null bindings)
	(insert "*No Binding*")
      (while (progn
	       (xray-key-button (car bindings))
	       (setq bindings (cdr bindings)))
	(insert "    ")))))


(defun xray-key-button (key)
  (xray-xref-string-button (xray-key-description key) key #'xray-describe-key))


;; part of describe-function-1 - hacked from help.el
(defun xray-file-button (symbol)
  (let ((file (symbol-file symbol)))
    (if (not file)
	(insert "*No File*")
      (xray-xref-string-button file symbol #'xray-locate-file))))


(defun xray-xref-string-button (str symbol func)
  (insert str)
  (xray-xref-button (concat "\\(" (regexp-quote str) "\\)") func symbol))


(defun xray-xref-button (regexp function symbol)
  (save-excursion
    (save-match-data
      (and (re-search-backward regexp nil t)
	   (help-xref-button 1 function symbol)))))


(defun xray-describe-function (symbol)
  (xray-setup-help)
  (describe-function symbol))


(defun xray-describe-variable (symbol)
  (xray-setup-help)
  (describe-variable symbol))


(defun xray-describe-key (key)
  (xray-setup-help)
  (describe-key key))


(defun xray-setup-help ()
  (help-setup-xref help-xref-stack-item (interactive-p)))


;; part of describe-function-1 - hacked from help.el
(defun xray-locate-file (arg)
  (let ((location (find-function-noselect arg)))
    (pop-to-buffer (car location))
    (goto-char (cdr location))))


(defun xray-key-description (key)
  (condition-case data
      (key-description key)
    (error
     (format "%s" key))))


(defun xray-property-list (label column plist)
  (insert "\n ")
  (if (symbolp label)
      (xray-symbol-button label)
    (insert label))
  (insert ":\n")
  (move-to-column column t)
  (xray-cell-plist plist))


;; describe-function - hacked from help.el
(defun xray-cell-function (symbol)
  (if (not (fboundp symbol))
      "void"
    (let ((def (symbol-function symbol)))
      (concat (if (commandp def)
		  "*Interactive-"
		"*")
	      (cond ((or (stringp def)
			 (vectorp def))
		     "Keyboard-Macro")
		    ((subrp def)
		     "Built-In-Function")
		    ((byte-code-function-p def)
		     "Compiled-Lisp-Function")
		    ((symbolp def)
		     (while (symbolp (symbol-function def))
		       (setq def (symbol-function def)))
		     (format "Alias-For `%s'" def))
		    ((eq (car-safe def) 'lambda)
		     "Lisp-Function")
		    ((eq (car-safe def) 'macro)
		     "Lisp-Macro")
		    ((eq (car-safe def) 'mocklisp)
		     "Mocklisp-Function")
		    ((eq (car-safe def) 'autoload)
		     (concat "Autoloaded-"
			     (cond ((eq (nth 4 def) 'keymap)
				    "Keymap")
				   ((nth 4 def)
				    "Lisp-Macro")
				   (t
				    "Lisp-Function")
				   )))
		    (t "")
		    )
	      "*"))))


(defun xray-cell-value (symbol)
  (if (not (boundp symbol))
      (insert "void")
    (insert (if (local-variable-p symbol)
		"*Local-"
	      "*")
	    (if (user-variable-p symbol)
		"Option"
	      "Variable")
	    "*  ")
    (xray-pp-value (symbol-value symbol) t)))


(defun xray-cell-plist (plist)
  (insert "(")
  (let ((indent (xray-current-indentation t)))
    (when plist
      (while (progn
	       (xray-plist plist)
	       (setq plist (nthcdr 2 plist)))
	(insert indent))))
  (insert ")\n"))


(defun xray-plist (plist)
  (let* ((prop  (nth 0 plist))
	 (value (nth 1 plist))
	 (fun   (cdr (assq prop xray-property-alist))))
    (if fun
	(funcall fun prop value)
      (insert (format "%S  %S" prop value)))))


(defun xray-widget-type (prop value)
  (insert (format "%S\n        (%S" prop (car value)))
  (while (setq value (cdr value))
    (insert (format "\n         %S  %S"
		    (car value) (car (setq value (cdr value))))))
  (insert ")"))


(defun xray-custom-type (prop value)
  (insert (format "%S  " prop))
  (xray-pp-value value t))


(defun xray-pp-value (value &optional no-newline)
  (let ((indent  (xray-current-indentation))
	(initial (point)))
    (pp value (current-buffer))
    (and no-newline (eq (preceding-char) ?\n)
	 (delete-char -1))
    (unless (xray-value-threshold value initial)
      (save-excursion
	(goto-char initial)
	(while (and (search-forward "\n" nil t) (not (eobp)))
	  (insert indent))))))


(defun xray-value-threshold (value &optional initial)
  (unless initial
    (setq initial (point))
    (insert (format "%S" value)))
  (and (integerp xray-value-threshold)
       (> (- (point) initial) xray-value-threshold)
       (progn
	 (delete-region initial (point))
	 (insert
	  (cond ((stringp value) "\"\"...")
		((vectorp value) "[]...")
		((listp value)   "()...")
		(t               "##...")
		))
	 t)))


(defun xray-overlay-list (title olist)
  (xray-display-list-title title)
  (let ((indent (xray-current-indentation)))
    (when olist
      (while (progn
	       (xray-overlay-button (car olist))
	       (insert "\n" indent "   ")
	       (xray-cell-plist (overlay-properties (car olist)))
	       (setq olist (cdr olist)))
	(insert indent)))
    (insert (if (= (preceding-char) ?\n)
		indent
	      "")
	    ")\n")))


(defun xray-display-list (title alist &optional func)
  (xray-display-list-title title)
  (let ((indent (xray-current-indentation t)))
    (when alist
      (if func
	  (while (progn
		   (funcall func (car alist))
		   (setq alist (cdr alist)))
	    (insert indent))
	(while (let ((value (car alist)))
		 (if (not (listp value))
		     (xray-value-threshold value)
		   (insert (format "(%S  " (car value)))
		   (setq value (cdr value))
		   (or (listp value)
		       (insert ".  "))
		   (xray-value-threshold value)
		   (insert ")"))
		 (setq alist (cdr alist)))
	  (insert indent)))))
  (insert ")\n"))


(defun xray-display-hook (title hook-list)
  (xray-display-list-title title)
  (let* ((indent      (xray-current-indentation t))
	 (hook-indent (concat indent "   ")))
    (when hook-list
      (while (let ((hook (car hook-list)))
	       (if (not (boundp hook))
		   (insert (format "%S:" hook)
			   hook-indent "void")
		 (xray-variable-button hook)
		 (insert ":" hook-indent)
		 (xray-pp-value (symbol-value hook) t))
	       (setq hook-list (cdr hook-list)))
	(insert "\n" indent))))
  (insert ")\n"))


(defun xray-display-list-title (title)
  (or (string= title "")
      (insert "\n " title))
  (insert ":\n    ("))


(defun xray-current-indentation (&optional newline)
  (concat (and newline "\n") (make-string (current-column) ?\ )))


;; what-cursor-position - hacked from simple.el
(defun xray-what-cursor-position ()
  "Returns a cons: (POSITION . CHARACTER)
Where POSITION is a string with position information and
CHARACTER is a string with character information."
  (let* ((char (following-char))
	 (beg (point-min))
	 (end (point-max))
         (pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
	 (hscroll (if (= (window-hscroll) 0)
		      ""
		    (format "  Hscroll=%d" (window-hscroll))))
	 (col (current-column))
	 (lin (if (> (buffer-size) line-number-display-limit)
		  "????"
		(number-to-string (+ (count-lines beg pos)
				     (if (zerop col) 1 0))))))
    (cons
     ;; position information part (car)
     (concat (format "%d of %d(%d%%)" pos total percent)
	     (and (or (/= beg 1) (/= end (1+ total)))
		  (format " <%d - %d>" beg end))
	     (format "  line %s  column %d%s" lin col hscroll))
     ;; character information part (cdr)
     (if (= pos end)
	 "*No Character*"
       (let* ((code buffer-file-coding-system)
	      (coding
	       (if (or (not code)
		       (eq (coding-system-type code) t))
		   default-buffer-file-coding-system
		 code))
	      (encoded
	       (and (char-valid-p char)
		    (>= char 128)
		    (encode-coding-char char coding)))
	      (encoding-msg
	       (cond ((not (char-valid-p char))
		      ", invalid")
		     (encoded
		      (format ", ext %s"
			      (if (<= (length encoded) 1)
				  ""
				(concat
				 (encoded-string-description encoded coding)
				 (and (cmpcharp char)
				      "...")))))
		     (t
		      ""))))
	 ;; we show the detailed information of CHAR.
	 (format "%s (0%o, %d, 0x%x%s) %s"
		 (if (< char 256)
		     (single-key-description char)
		   (buffer-substring pos (1+ pos)))
		 char char char
		 encoding-msg
		 (if (cmpcharp char)
		     ;; for a composite character, we show the components
		     ;; only.
		     (concat "(composed \""
			     (decompose-composite-char char)
			     "\")")
		   (split-char char))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'xray)


;;; xray.el ends here

