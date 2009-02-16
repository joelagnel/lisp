;;; ws-trim.el --- Tools and minor mode to trim whitespace on text lines

;; Copyright (C) 1997 Martin Stjernholm

;; Author: Martin Stjernholm <mast@lysator.liu.se>
;; Created: 8 Apr 1997
;; Version: 1.3
;; Keywords: wp
;; X-URL: ftp://ftp.lysator.liu.se/pub/emacs/ws-trim.el

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
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to mast@lysator.liu.se) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.

;;; Commentary:

;; This package contains tools to do various sorts of whitespace
;; trimming on buffer lines.  The main part is WS Trim mode, which is
;; a minor mode that automatically trims whitespace on text lines.
;; You can control how thorough this mode should be, e.g. whether all
;; lines or only lines you edit should be trimmed.  For further
;; reading see the document strings for the variables `ws-trim-mode'
;; and `ws-trim-level'.
;;
;; To install the package, either issue "(require 'ws-trim)" in a good
;; place (i.e. your .emacs file), or autoload some of the functions
;; below.  You can activate WS Trim mode for a specific buffer by
;; calling `turn-on-ws-trim'.
;;
;; There is also a Global WS Trim mode that automagically turns on WS
;; Trim mode in most buffers.  Put "(global-ws-trim-mode t)" in your
;; .emacs to use it.  By default a heuristic is used to determine if
;; WS Trim mode should be on or off in every buffer.  You can control
;; this through the variable `ws-trim-global-modes'.
;;
;; This package has been tested with GNU Emacs version 19.34 and 20.7.
;; It won't run in versions older than 19.23.  Versions in between are
;; currently untested.  Reported to work with XEmacs 19.14.
;;
;; Comments, suggestions and bug reports are always welcome.
;;
;; Credits:
;; * Global WS Trim mode is modeled after Global Font Lock mode in
;;   font-lock.el by "jwz, then rms, then sm <simon@gnu.ai.mit.edu>".

;;; Change log:

;; Fri Oct  5 01:04:17 CEST 2001      Martin Stjernholm <mast@lysator.liu.se>
;;	*  Check if the trimming would have any effect before doing it,
;;	   to avoid getting buffers modified unnecessarily.
;;
;; Sat May 15 19:15:24 CEST 1999      Martin Stjernholm <mast@lysator.liu.se>
;;	*  Prompt for the trim method when a prefix arg is passed to
;;	   ws-trim-line, ws-trim-region and ws-trim-buffer.
;;
;; Tue Apr  8 00:28:58 MET DST 1997   Martin Stjernholm <mast@lysator.liu.se>
;;	*  First public release.

;;; Code:

(eval-when-compile
  (require 'cl))			; Some handy macros.

;;; WS Trim tools

;;;###autoload
(defvar ws-trim-method-hook 'ws-trim-trailing
  "*The kind of trimming done by the WS Trim mode and functions.
A single or a list of functions which are run on each line that's
getting trimmed.  Supplied trim functions:

`ws-trim-trailing'        Delete trailing whitespace.
`ws-trim-leading-spaces'  Replace unnecessary leading spaces with tabs.
`ws-trim-leading-tabs'    Replace leading tabs with spaces.
`ws-trim-tabs'            Replace all tabs with spaces.

This is a perfectly normal hook run by `run-hooks' and custom
functions can of course be used.  There's no inherent restriction to
just whitespace trimming either, for that matter.  Each function
should modify the current line and leave point somewhere on it.")

;;;###autoload
(defun ws-trim-line (arg)
  "Trim whitespace on the current line.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook))
	(ws-trim-changed-region 'ignore)) ; ws-trim-after-change disabled now.
    (save-excursion
      (run-hooks 'ws-trim-method-hook))))

;;;###autoload
(defun ws-trim-region (arg)
  "Trim whitespace on each line in the region.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook)))
    (ws-trim-region-1 (mark) (point))))

;;;###autoload
(defun ws-trim-buffer (arg)
  "Trim whitespace on each line in the buffer.
Do this according to the hook `ws-trim-method-hook'.  With a prefix
argument, ask for the trim method to use instead."
  (interactive "*P")
  (ws-trim-reset-changed-region nil)
  (let ((ws-trim-method-hook (if arg (ws-trim-ask-method) ws-trim-method-hook)))
    (ws-trim-region-1 (point-min) (point-max))))

(defun ws-trim-trailing ()
  "Delete trailing whitespace on current line.
Normally used in `ws-trim-method-hook'."
  (end-of-line)
  (if (memq (preceding-char) '(?\  ?\t))
      (delete-horizontal-space)))

(defun ws-trim-leading-spaces ()
  "Replace unnecessary leading spaces with tabs on current line.
Normally used in `ws-trim-method-hook'."
  (let* ((indent-tabs-mode t)
	 (col (current-indentation))
	 (tab-col (* (/ col tab-width) tab-width)))
    (beginning-of-line)
    (skip-chars-forward "\t")
    (when (/= (current-column) tab-col)
      (delete-horizontal-space)
      (indent-to col))))

(defun ws-trim-leading-tabs ()
  "Replace leading tabs with spaces on current line.
Normally used in `ws-trim-method-hook'."
  (let ((indent-tabs-mode nil)
	(col (current-indentation)))
    (beginning-of-line)
    (skip-chars-forward " ")
    (when (/= (current-column) col)
      (delete-horizontal-space)
      (indent-to col))))

(defun ws-trim-tabs ()
  "Replace all tabs with spaces on current line.
Normally used in `ws-trim-method-hook'."
  (let ((indent-tabs-mode nil)
	col)
    (beginning-of-line)
    (while (progn
	     (skip-chars-forward "^\t\n")
	     (not (eolp)))
      (skip-chars-forward " ")
      (when (eq (following-char) ?\t)
	(skip-chars-forward " \t")
	(setq col (current-column))
	(delete-horizontal-space)
	(indent-to col)))))

(defconst ws-trim-methods '(ws-trim-trailing
			    ws-trim-leading-spaces
			    ws-trim-leading-tabs
			    ws-trim-tabs)
  "List of known trim methods.")

;;; WS Trim mode

;;;###autoload
(defvar ws-trim-mode nil
  "If non-nil, WS Trim mode is active.
This mode automatically trims whitespace on text lines.  The kind of
trimming is specified by the hook `ws-trim-method-hook'.  You can
either trim every line in the buffer or just the lines you edit
manually, see the variable `ws-trim-level' for details.  This mode
runs the hook `ws-trim-mode-hook' when activated.

Please note that there are other common functions, e.g. `indent-to',
`newline-and-indent' (often bound to LFD or RET), `fill-paragraph',
and the variable `indent-tabs-mode', that also trims whitespace in
various circumstances.  They are entirely independent of this mode.

To automatically enable WS Trim mode in any major mode, put
`turn-on-ws-trim' in the major mode's hook, e.g. in your .emacs:

  (add-hook 'emacs-lisp-mode-hook 'turn-on-ws-trim)

You can also activate WS Trim mode automagically in all modes where
it's likely to be useful by putting the following in .emacs:

  (global-ws-trim-mode t)

Exactly when WS Trim is activated are by default controlled by a
heuristic, see the function `ws-trim-mode-heuristic' for details.  You
can get more control over the process through the variable
`global-ws-trim-modes'.

This variable automatically becomes buffer local when modified.  It
should not be set directly; use the commands `ws-trim-mode' or
`turn-on-ws-trim' instead.")
(make-variable-buffer-local 'ws-trim-mode)

;;;###autoload
(defvar ws-trim-level 0
  "*How thorough automatic whitespace trimming should be in WS Trim mode.
If 3 or greater, all lines in the buffer are kept trimmed at all
times (if the buffer is modifiable).
If 2, all lines in the buffer are trimmed when the buffer is modified
for the first time.
If 1, only modified lines are trimmed.
If 0, only single modified lines are trimmed, i.e. operations that
modify more than one line doesn't cause any trimming (newline is an
exception).

The current line is never trimmed on any level, unless the buffer is
about to be written.  In that case the current line is treated as any
other line.

The default level is 0, which is very restrictive.  This is
particularly useful when you edit files which are compared with diff
\(e.g. for patches), because parts that you don't change manually are
kept unchanged.  You can also do block operations over several lines
without risking strange side effects (e.g. paste patches into mails).

This variable automatically becomes buffer local when changed.  Use
the function `set-default' to set the value it defaults to in all new
buffers.  If you want even more control it's best to put a suitable
function onto `ws-trim-mode-hook'.  Changes of `ws-trim-level' might
not take effect immediately; it's best set when the mode is
initialized.")
(make-variable-buffer-local 'ws-trim-level)

;;;###autoload
(defvar ws-trim-mode-line-string " Trim"
  "*Modeline string for WS Trim mode.
Set to nil to remove the modeline indicator for ws-trim.")

;;;###autoload
(defvar ws-trim-mode-hook nil
  "A normal hook which is run when WS Trim mode is turned on.
This hook is run by `run-hooks' and can therefore be buffer local.

Some care might be necessary when putting functions on this hook due
to the somewhat strange circumstances under which it's run.
Specifically, anything put here might indirectly be run from
`post-command-hook' or `find-file-hooks'.  Don't worry about it if you
just want to do something simple, e.g. setting some variables.")

;;;###autoload
(defun turn-on-ws-trim ()
  "Unconditionally turn on WS Trim mode.
See the variable `ws-trim-mode' for further info on this mode."
  (interactive)
  (ws-trim-mode 1))

;;;###autoload
(defun ws-trim-mode (&optional arg)
  "Toggle WS Trim mode, which automatically trims whitespace on lines.
A positive prefix argument turns the mode on, any other prefix turns
it off.

See the variable docstring for details about this mode."
  (interactive "P")
  (setq ws-trim-mode (if (null arg)
			 (not ws-trim-mode)
		       (> (prefix-numeric-value arg) 0)))
  (if ws-trim-mode
      (if (not (integerp ws-trim-level))
	  (progn
	    (error "`ws-trim-level' must be an integer")
	    (setq ws-trim-mode nil))
	(make-local-hook 'after-change-functions)
	(add-hook 'after-change-functions 'ws-trim-after-change nil t)
	(make-local-hook 'post-command-hook)
	(add-hook 'post-command-hook 'ws-trim-post-command nil t)
	(make-local-hook 'first-change-hook)
	(add-hook 'first-change-hook 'ws-trim-on-first-change nil t)
	(add-hook 'write-contents-hooks 'ws-trim-on-write)
	(run-hooks 'ws-trim-mode-hook)
	(if (or (>= ws-trim-level 3)
		(and (>= ws-trim-level 2) (buffer-modified-p)))
	    (or buffer-read-only
		(ws-trim-region-1 (point-min) (point-max)))))
    (remove-hook 'after-change-functions 'ws-trim-after-change t)
    (remove-hook 'post-command-hook 'ws-trim-post-command t)
    (remove-hook 'first-change-hook 'ws-trim-on-first-change t)
    (remove-hook 'write-contents-hooks 'ws-trim-on-write)
    (ws-trim-reset-changed-region nil))
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update)
    (set-buffer-modified-p (buffer-modified-p))))

;; Internals:

(defvar ws-trim-changed-region nil)
;; A cons of two markers marking the line region that was last
;; changed, nil if no change has occurred, `first-change' if the
;; buffer is about to be modified for the first time and
;; `ws-trim-level' >= 2, or `ignore' if a ws-trim function is changing
;; it.

(make-variable-buffer-local 'ws-trim-changed-region)

(defvar ws-trim-changed-newline nil)
;; Non-nil if there's a newline in current `ws-trim-changed-region'.
(make-variable-buffer-local 'ws-trim-changed-newline)

(when (and (= emacs-major-version 19) (<= emacs-minor-version 34))
  ;; This is a kludge to counter the way `newline' inserts newlines
  ;; under some circumstances (see the comment at the beginning of
  ;; that function in simple.el in Emacs 19.34).  Newlines aren't
  ;; always inserted near point which makes `ws-trim-after-change'
  ;; report the line two lines above point as changed.  That in turn
  ;; causes the whole changed region to be too large to be trimmed
  ;; under level 0 trimming.
  ;;
  ;; Hopefully this is only necessary up to and including Emacs 19.34.
  ;; Although the kludge tries to be as narrow as possible there's a
  ;; risk that it will incorrectly counter other situations too.
  (defvar ws-trim-newline-kludge nil)
  ;; If a marker, a "funny" newline from `newline' is suspected.  The
  ;; marker mark to the newline in question.  If t, we're sure it
  ;; can't happen now.
  (make-variable-buffer-local 'ws-trim-newline-kludge))

(defun ws-trim-ask-method ()
  (let* ((alist (mapcar (lambda (fn) (cons (symbol-name fn) fn))
			ws-trim-methods))
	 (default (or (cdr-safe (assoc (find-if (lambda (item) (assoc item alist))
						minibuffer-history)
				       alist))
		      (if (consp ws-trim-method-hook)
			  (car ws-trim-method-hook)
			ws-trim-method-hook)))
	 (val (cdr-safe (assoc (completing-read
				(format "Trim method (default %S): " default)
				alist nil t)
			       alist))))
    (or val default)))

(defun ws-trim-region-1 (from to)
  (let ((ws-trim-changed-region 'ignore)) ; ws-trim-after-change disabled now.
    (save-excursion
      (save-restriction
        (narrow-to-region from to)
        (goto-char (point-min))
        (while (not (eobp))
          (run-hooks 'ws-trim-method-hook)
          (forward-line))))))

(defun ws-trim-after-change (beg end length)
  (or (eq ws-trim-changed-region 'ignore)
      (save-excursion
	(if (eq ws-trim-changed-region 'first-change)
	    (setq ws-trim-changed-region (cons (point-min-marker) (point-max-marker)))
	  (when (boundp 'ws-trim-newline-kludge)
	    ;; This is part 1 of the kludge to counter the `newline' behavior.
	    (when (and (not (eq ws-trim-newline-kludge t))
		       (eq (1+ beg) end)
		       (not (eq end (point-max)))
		       (string-equal (buffer-substring beg (1+ end)) "\n\n"))
	      (if (null ws-trim-newline-kludge)
		  (setq ws-trim-newline-kludge (copy-marker beg)
			beg (1+ beg))
		(when (eq (1+ ws-trim-newline-kludge) (car ws-trim-changed-region))
		  (goto-char (car ws-trim-changed-region)) (forward-line -1)
		  (set-marker (car ws-trim-changed-region) (point)))
		(set-marker ws-trim-newline-kludge nil)
		(setq ws-trim-newline-kludge t))))
	  (if (consp ws-trim-changed-region)
	      (progn
		(if (< beg (car ws-trim-changed-region))
		    (set-marker (car ws-trim-changed-region)
				(progn (goto-char beg) (beginning-of-line) (point))))
		(if (> end (cdr ws-trim-changed-region))
		    (set-marker (cdr ws-trim-changed-region)
				(progn (goto-char end) (end-of-line) (point)))))
	    (setq ws-trim-changed-region
		  (cons (copy-marker (progn (goto-char beg) (beginning-of-line) (point)))
			(copy-marker (progn (goto-char end) (end-of-line) (point)))))))
	(or ws-trim-changed-newline
	    (setq ws-trim-changed-newline
		  (ws-trim-nlc (car ws-trim-changed-region)
			       (cdr ws-trim-changed-region)))))))

(defun ws-trim-post-command ()
  (if (consp ws-trim-changed-region)
      (let* ((begmark (car ws-trim-changed-region))
	     (endmark (cdr ws-trim-changed-region))
	     (beg (marker-position begmark))
	     (end (marker-position endmark)))
	(when (boundp 'ws-trim-newline-kludge)
	  ;; This is part 2 of the kludge to counter the `newline' behavior.
	  (when (markerp ws-trim-newline-kludge)
	    (save-excursion
	      (if (and (eq (1+ ws-trim-newline-kludge) beg)
		       (< beg (point))
		       (eq (ws-trim-nlc beg (point)) 1))
		  (when (null (ws-trim-nlc beg end))
		    (goto-char end) (forward-line) (end-of-line)
		    (setq end (point)))
		(when (eq (1+ ws-trim-newline-kludge) beg)
		  (goto-char beg) (forward-line -1)
		  (setq beg (point)))))
	    (set-marker ws-trim-newline-kludge nil))
	  (setq ws-trim-newline-kludge nil))
	;; This test isn't essential, but it quickly eliminates almost
	;; all cases when nothing should be done.
	(when (or ws-trim-changed-newline (< (point) beg) (> (point) end))
	  (save-excursion
	    (let* ((posmark (point-marker))
		   (pos (progn (beginning-of-line) (point)))
		   point-in-region)
	      (setq ws-trim-changed-region 'ignore) ; ws-trim-after-change disabled now.
	      (if (null ws-trim-changed-newline)
		  (if (= pos beg)
		      (setq point-in-region t)
		    ;; One line changed and point not on it.
		    (goto-char beg)
		    (run-hooks 'ws-trim-method-hook))
		(setq point-in-region (and (>= pos beg) (<= pos end)))
		(if (and point-in-region (eq (ws-trim-nlc beg end) 1))
		    ;; Two lines changed and point on one of them -
		    ;; trim the other one.  This is the newline
		    ;; exception in level 0 trimming.
		    (progn (goto-char beg)
			   (if (= pos beg) (forward-line))
			   (run-hooks 'ws-trim-method-hook))
		  (if (>= ws-trim-level 1)
		      ;; Trim changed region except current line.
		      (if (not point-in-region)
			  (ws-trim-region-1 beg end)
			(if (< beg pos) (ws-trim-region-1 beg pos))
			(goto-char posmark) (end-of-line)
			(if (< (point) endmark) (ws-trim-region-1 (point) endmark)))
		    (setq point-in-region nil))))
	      (if point-in-region
		  (setq ws-trim-changed-region
			(cons (copy-marker
			       (progn (goto-char posmark) (beginning-of-line) (point)))
			      (copy-marker
			       (progn (goto-char posmark) (end-of-line) (point)))))
		(setq ws-trim-changed-region nil))
	      (setq ws-trim-changed-newline nil)
	      (set-marker begmark nil)
	      (set-marker endmark nil)
	      (set-marker posmark nil)
	      ))))))

(defun ws-trim-on-first-change ()
  (if (>= ws-trim-level 2)
      (ws-trim-reset-changed-region 'first-change)))

(defun ws-trim-on-write ()
  (let (beg end)
    (if (consp ws-trim-changed-region)
	(setq beg (marker-position (car ws-trim-changed-region))
	      end (marker-position (cdr ws-trim-changed-region))))
    (ws-trim-reset-changed-region 'ignore) ; ws-trim-after-change disabled now.
    (if (or (>= ws-trim-level 3)
	    (and (>= ws-trim-level 2) (buffer-modified-p)))
	;; A bit defensive test - is this function ever called if the
	;; buffer is unmodified?
	(or buffer-read-only
	    (ws-trim-region-1 (point-min) (point-max)))
      (if beg
	  (if ws-trim-changed-newline
	      (if (>= ws-trim-level 1)
		  (ws-trim-region-1 beg end))
	    (save-excursion
	      (goto-char beg)
	      (run-hooks 'ws-trim-method-hook)))))
    (setq ws-trim-changed-region nil))
  nil)

(defun ws-trim-reset-changed-region (&optional newval)
  (when (consp ws-trim-changed-region)
    (set-marker (car ws-trim-changed-region) nil)
    (set-marker (cdr ws-trim-changed-region) nil))
  (setq ws-trim-changed-region newval))

(defun ws-trim-nlc (from to)
  "nil if no newlines between FROM and TO, 1 if one, t if more.
FROM <= TO is assumed."
  (save-excursion
    (save-match-data
      (goto-char from)
      (cond ((not (re-search-forward "[\n\C-m]" to t)) nil)
	    ((not (re-search-forward "[\n\C-m]" to t)) 1)
	    (t t)))))

;;; Global WS Trim mode

;; This mode is more or less a copy of global-font-lock-mode from
;; font-lock.el.  See header for credits.

;;;###autoload
(defvar global-ws-trim-mode nil
  "If non-nil, automagically turn on WS Trim mode in many major modes.
How it's done is controlled by the variable `ws-trim-global-modes'.

This variable should not be changed directly; use the command
`global-ws-trim-mode' instead.")

;;;###autoload
(defvar ws-trim-global-modes 'guess
  "*Controls which major modes should have WS Trim mode turned on.
Global WS Trim mode must first be activated, which is done by the
command `global-ws-trim-mode'.

If nil, no modes turn on WS Trim mode.
If t, all modes turn on WS Trim mode.
If `guess', then a heuristic is used to determine whether WS Trim mode
should be activated in the mode in question.  See
`ws-trim-mode-heuristic' for details.
If a list, then all modes whose `major-mode' symbol names matches some
entry in it turn on WS Trim mode.
If a list begins with `not', all modes but the ones mentioned turn on
WS Trim mode.
If a list begins with `guess', then the remaining elements must in
turn be lists as above.  All modes not specified in any of these lists
will use the heuristic.  E.g:

  (setq ws-trim-global-modes '(guess (Info-mode) (not c-mode c++-mode)))

turns on WS Trim in Info-mode (God knows why), off in C mode and
C++ mode, and uses the heuristic for all other modes.")

(defvar ws-trim-buffers nil)
;; List of buffers waiting to be processed by
;; `global-ws-trim-init-ws-trim'.

;;;###autoload
(defun global-ws-trim-mode (&optional arg)
  "Toggle Global WS Trim mode.
A positive prefix argument turns the mode on, any other prefix turns
it off.

When this mode is active, WS Trim mode is automagically turned on or
off in buffers depending on their major modes.  The behavior is
controlled by the `ws-trim-global-modes' variable."
  (interactive "P")
  (setq global-ws-trim-mode (if (null arg) (not ws-trim-mode)
			      (> (prefix-numeric-value arg) 0)))
  (if global-ws-trim-mode
      (progn
	(add-hook 'find-file-hooks 'global-ws-trim-init-ws-trim)
	(setq ws-trim-buffers (buffer-list))
	(global-ws-trim-init-ws-trim))
    (remove-hook 'find-file-hooks 'global-ws-trim-init-ws-trim)))

(defun ws-trim-mode-heuristic ()
  "Return 1 if WS Trim mode likely should be active, 0 otherwise.
This is determined by a heuristic that says \"yes\" iff the buffer is
not read only and there are some keys bound to self-insert-command.

The heuristic is based on the observation that whitespace trimming is
only useful in \"freetext modes\", i.e. modes where all text is edited
by the user.  In particular, it should NOT be active in \"constrained
modes\" where the format of the whole buffer is controlled (e.g. Dired
mode).

Free user input is normally disabled in a well-written constrained
mode by making the buffer read only and/or by disabling all self-
inserting keys (typically by using `suppress-keymap').  The heuristic
detects both these cases."
  (if (or buffer-read-only
	  (null (where-is-internal 'self-insert-command nil 'non-ascii)))
      0
    1))

;; Internals:

(defun global-ws-trim-init-ws-trim ()
  (remove-hook 'post-command-hook 'global-ws-trim-init-ws-trim)
  (while ws-trim-buffers
    (if (and (buffer-live-p (car ws-trim-buffers))
	     (not (local-variable-p 'ws-trim-mode (car ws-trim-buffers))))
	(save-excursion
	  (set-buffer (car ws-trim-buffers))
	  (ws-trim-mode
	   (cond ((eq ws-trim-global-modes t) 1)
		 ((eq ws-trim-global-modes 'guess) (ws-trim-mode-heuristic))
		 ((consp ws-trim-global-modes)
		  (catch 'done
		    (let (modes-list)
		      (if (eq (car-safe ws-trim-global-modes) 'guess)
			  (setq modes-list (cdr-safe ws-trim-global-modes))
			(setq modes-list (list ws-trim-global-modes)))
		      (while modes-list
			(if (eq (car-safe (car-safe modes-list)) 'not)
			    (if (memq major-mode (cdr-safe (car-safe modes-list)))
				(throw 'done 0))
			  (if (memq major-mode (car-safe modes-list))
			      (throw 'done 1)))
			(setq modes-list (cdr-safe modes-list))))
		    (cond ((eq (car-safe ws-trim-global-modes) 'not) 1)
			  ((eq (car-safe ws-trim-global-modes) 'guess)
			   (ws-trim-mode-heuristic))
			  (t 0))))
		 (t 0)))))
    (setq ws-trim-buffers (cdr ws-trim-buffers))))

(defun global-ws-trim-change-major-mode ()
  (when global-ws-trim-mode
    (add-to-list 'ws-trim-buffers (current-buffer))
    (add-hook 'post-command-hook 'global-ws-trim-init-ws-trim)))

;;; Installation:

;; Put the minor mode on the global minor-mode-alist.
(or (assq 'ws-trim-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(ws-trim-mode ws-trim-mode-line-string)
				 minor-mode-alist)))

(add-hook 'change-major-mode-hook 'global-ws-trim-change-major-mode)

(provide 'ws-trim)

;;; ws-trim.el ends here
