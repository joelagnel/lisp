;;; cycle-mini.el --- Cycle through completions with arrow keys
;; Copyright (C) 1994, 2002 Joseph W. Reiss

;; Author:   Joe Reiss <joe@joereiss.net>
;; Created:  26 Aug 1994
;; Version:  1.07
;; Keywords: minibuffer, completion
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; cycle-mini|Joe Reiss|joe@joereiss.net|
;; Make arrow keys cycle through completions in minibuffer.|
;; 12-Feb-2004|1.07|http://joereiss.net/misc/cycle-mini.el|

;;; Commentary:

;; This is an extension to the completing-read commands in the
;; minibuffer.  It allows you to cycle through the current list of
;; completions.  This works when changing buffers, with any command
;; which reads a function or variable name, or with a programmer
;; specified completion list.  It even works with functions which
;; read a file name!  In addition, if you have part of a name already
;; typed in, cycle-mini will use that string to narrow down the matches
;; and will only cycle through the completions which contain that
;; initial substring.

;;; Default bindings:

;; ^P,[up]	Display previous matching completion.
;; ^N,[down]	Display next matching completion.
;; TAB          Accept currently displayed completion and move cursor
;;              to end of line.  If no completion is displayed, call
;;              minibuffer-complete as usual.
;;
;; Typing any movement key will also accept the current completion.
;; Any editing key will clear the currently displayed completion
;; and will then change the remaining input as expected.

;;; Installation:

;; Byte-compile, then put this file in one of your elisp load
;; directories and add the following line to your .emacs file
;;
;; (load "cycle-mini")

;;; History:

;; Cycle-mini was originally inspired by Hans Koomen's elec-mini.el.
;; Portions of this code were also adapted from Ken Manheimer's
;; icomplete.el and Dave Gillespie's complete.el, both in the GNU
;; Emacs distribution.
;;
;; v1.07
;;  - Fixed bug caused by make-local-hook not always doing
;;    "The Right Thing (tm)" 
;; v1.06
;;  - Made compatibile with GNU Emacs 21's new handling of minibuffer
;;    prompts
;;  - Added ability to start cycling from point instead of always
;;    cycling at the end of the entire input string
;;  - If user issues an editing command while cycling, the cycled
;;    completion is cleared out
;;  - If cycle-mini-wrap is nil, a message now appears when user tries
;;    to move past the beginning or end of the completion list
;;  - Made customizable, under the "Convenience:Completion" group

;;; Known bugs: (?)

;; Not really a bug, but an ugly kludge.  In order to make file
;; completions work, cycle-mini has to make some assumptions about
;; minibuffer-completion-predicate -- namely that if it's a string and
;; minibuffer-completion-table is a symbol, then we're performing file
;; name completion.  Currently, of all the elisp code and all the
;; source distributed with GNU Emacs v19.25 and above, as well as with
;; Xemacs, only file completions use the predicate this strange way.
;; But if that use should change, or if someone else should use a
;; string for a predicate in some other way, then...
;;
;; Also note that cycle-mini has been tested with GNU Emacs 20.7 and
;; above.  Older versions of cycle-mini worked with GNU and XEmacs
;; 19+, but I haven't been able to retest recently.  I don't think it
;; should be difficult to make cycle-mini work with other variations,
;; but I just don't have access to them.  Anyone who wants to try is
;; welcome to, but please send me your modifications.

;; User modifiable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (fboundp 'defgroup)
  (defgroup cycle-mini nil
    "Cycle through completions in the minibuffer."
    :group 'completion))

(unless (fboundp 'defcustom)
  (defmacro defcustom (symbol &optional initvalue docs &rest rest)
    (list 'defvar symbol initvalue docs)))

(defcustom cycle-mini-wrap t
  "* Wrap around when we reach either end of the completion list, if non-nil.
Otherwise, stop and ring the bell."
  :type 'boolean
  :group 'cycle-mini)

(defcustom cycle-mini-cycle-after-point t
  "* Only use input text up to point to generate completions, if non-nil.
Otherwise, use the entire input string, regardless of point."
  :type 'boolean
  :group 'cycle-mini)

(defcustom cycle-mini-sort-buffers t
  "* Sort buffer names lexiographically during completion cycling, if non-nil.
Otherwise, leave buffers sorted in natural order."
  :type 'boolean
  :group 'cycle-mini)

(defvar cycle-mini-disable-bindings nil
  "* Don't set up any bindings for cycle-mini functions, if non-nil.
Otherwise, create some nice initial bindings.

MUST be set before cycle-mini loads.")


;; Internal variables.  Modified during execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle-mini-last-completion nil
  "Indicates where we are in the list of possible completions.")
(defvar cycle-mini-completion-list nil
  "List of possible completions to cycle through.")
(defvar cycle-mini-completion-type 'other
  "The type of completion we are doing.")
(defvar cycle-mini-completion-function
  (lookup-key minibuffer-local-completion-map "\t" t)
  "Function to call to perform tab completion.")

(defvar cycle-mini-ignored-extensions 'empty-cache)
(defvar cycle-mini-ignored-regexp nil)

(defconst cycle-mini-local-vars
  '(cycle-mini-last-completion cycle-mini-completion-list
    cycle-mini-completion-type cycle-mini-completion-function
    cycle-mini-ignored-extensions cycle-mini-ignored-regexp))

(defun cycle-mini-input-substring (start end))
(fset 'cycle-mini-input-substring
      (symbol-function (if (fboundp 'buffer-substring-no-properties)
			   'buffer-substring-no-properties
			 'buffer-substring)))
(defun cycle-mini-input-start ())
(fset 'cycle-mini-input-start
      (symbol-function (if (fboundp 'minibuffer-prompt-end)
			   'minibuffer-prompt-end
			 'point-min)))

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if cycle-mini-disable-bindings
    ()
  (define-key minibuffer-local-completion-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\t"
    'cycle-mini-accept-completion)

  (define-key minibuffer-local-must-match-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\t"
    'cycle-mini-accept-completion)
  )

(defun cycle-mini-no-completion ()
  "Reset cycle-mini to remember that no completion is currently displayed."
  (setq	cycle-mini-last-completion nil
	cycle-mini-completion-list nil))

(defun cycle-mini-accept-completion ()
  "Treat completed string as if it were part of the user input.
If there is no completed string, call minibuffer-complete."
  (interactive)
  (if (null cycle-mini-last-completion)	; Don't have completion displayed
      (call-interactively cycle-mini-completion-function)
    (goto-char (point-max))
    (cycle-mini-no-completion)))

(defun cycle-mini-cull-completions (input)
  "Generate list of possible completions."
  (setq cycle-mini-completion-list
	(all-completions input
			 minibuffer-completion-table
			 minibuffer-completion-predicate
			 t))

  ;; Handle completion-ignored-extensions
  (and (eq cycle-mini-completion-type 'file)
       (let ((p2 cycle-mini-completion-list) (p nil))
	 (while p2
	   (or (string-match cycle-mini-ignored-regexp (car p2))
	       (setq p (cons (car p2) p)))
	   (setq p2 (cdr p2)))
	 ;; If there are "good" names, use them
	 (and p (setq cycle-mini-completion-list p))))

  ;; Sort completion list if appropriate
  (if (or (not (eq cycle-mini-completion-type 'buffer))
	  cycle-mini-sort-buffers)
      (setq cycle-mini-completion-list
	    (sort cycle-mini-completion-list 'string<)))

  cycle-mini-completion-list)

(defun cycle-mini-brief-message (mesg)
  "Add a message to the end of the current input for just a few seconds."
  (let ((comp-end (point-max)))
    (save-excursion
      (goto-char comp-end)
      (insert " " mesg))
    (sit-for 2)
    (delete-region comp-end (point-max))))

(defun cycle-mini-next-completion (&optional incr)
  "Replace input by next possible completion."
  (interactive)
  (or incr (setq incr 1))

  (let* ((buffer-undo-list buffer-undo-list) ; don't let this be undone
	 (eoinput (if (or cycle-mini-last-completion
			  cycle-mini-cycle-after-point)
		      (point) (point-max)))
	 (input (cycle-mini-input-substring (cycle-mini-input-start) eoinput))
	 (tail  (cycle-mini-input-substring eoinput (point-max)))
	 (comps (or cycle-mini-completion-list
		    (cycle-mini-cull-completions input)))
	 (complen (length comps))
	 (filecomp (eq cycle-mini-completion-type 'file))
	 mesg taillist)

    (delete-region eoinput (point-max))

    (if (null comps)			; No matches
	(progn
	  (ding)
	  (setq mesg "[No match]")
	  (setq cycle-mini-last-completion nil))

      ; If we have text after point, look for that text in the list,
      ; and try to start near it if we can ...
      (if (and (null cycle-mini-last-completion)
	       (> (length tail) 0)
	       (setq taillist
		     (member (concat
			      (if filecomp
				  (file-name-nondirectory input)
				input)
			      tail) comps)))
	  (setq cycle-mini-last-completion
		(- (length comps) (length taillist))))

      (setq cycle-mini-last-completion
	    (cond
	     ; Only one exact match ...
	     ((= 1 complen)
	      (setq mesg "[Sole completion]")
	      0)

	     ; We've already started cycling ...
	     ((and (numberp cycle-mini-last-completion)
		   (>= cycle-mini-last-completion 0)
		   (< cycle-mini-last-completion complen))
	      (let ((next-completion (+ cycle-mini-last-completion incr)))
		(if (or cycle-mini-wrap
			(and (>= next-completion 0)
			     (<  next-completion complen)))
		  (mod next-completion complen)
		(ding)
		(setq mesg "[No more completions]")
		cycle-mini-last-completion)))

	     ; File name completion should start after dot files ...
	     ((and filecomp
		   (string= input (file-name-directory input)))
	      (if (< incr 0) (1- complen)
		(let ((i 0))
		  (while (and (< i complen)
			      (<= (aref (nth i comps) 0) ?.))
		    (setq i (1+ i)))
		  (if (< i complen) i (1- complen)))))

	     ;; If we have exact match, start with *next* match ...
	     ((string= input
		       (concat (if filecomp (file-name-directory input) "")
			       (car comps)))
	      (if (> incr 0) 1 (1- complen)))

	     ;; Otherwise, start at beginning (or end if going up) ...
	     (t
	      (if (> incr 0) 0 (1- complen)))))

      (goto-char (cycle-mini-input-start))
      (delete-region (point) (point-max))
      (insert (if filecomp (file-name-directory input) "")
	      (nth cycle-mini-last-completion comps)))

    (goto-char eoinput)
    (if mesg (cycle-mini-brief-message mesg))))

(defun cycle-mini-previous-completion ()
  "Replace input by previous possible completion."
  (interactive)
  (cycle-mini-next-completion -1))

(defun cycle-mini-post-command-hook ()
  "Do all necessary cleanup after a command runs in the minibuffer."
  (or (eq this-command 'cycle-mini-next-completion)
      (eq this-command 'cycle-mini-previous-completion)
      (cycle-mini-no-completion)))

(defun cycle-mini-clear-on-change (beg end &optional len)
  (or (eq this-command 'cycle-mini-next-completion)
      (eq this-command 'cycle-mini-previous-completion)
      (null cycle-mini-last-completion)
      (delete-region end (point-max))))

(defun cycle-mini-reset ()
  "Reset minibuffer completion list to the beginning before we begin."
  (let ((vars cycle-mini-local-vars))
    (while vars
      (make-local-variable (car vars))
      (setq vars (cdr vars))))
  (make-local-hook 'post-command-hook)
  (make-local-hook 'after-change-functions)

  (add-hook 'post-command-hook 'cycle-mini-post-command-hook nil t)
  (add-hook 'after-change-functions 'cycle-mini-clear-on-change nil t)
  (setq cycle-mini-completion-type
	(cond
	 ((and (symbolp minibuffer-completion-table)
	       (stringp minibuffer-completion-predicate))
	  'file)
	 ((and (listp minibuffer-completion-table)
	       (bufferp (cdr (car minibuffer-completion-table))))
	  'buffer)
	 (t
	  'other)))
  (cycle-mini-no-completion)

  ;; Handle completion-ignored-extensions
  (and (eq cycle-mini-completion-type 'file)
       ;; Build a regular expression representing the extensions list
       (or (equal completion-ignored-extensions
		  cycle-mini-ignored-extensions)
	   (setq cycle-mini-ignored-regexp
		 (concat "\\("
			 (mapconcat
			  'regexp-quote
			  (setq cycle-mini-ignored-extensions
				completion-ignored-extensions)
			  "\\|")
			 "\\)\\'"))))
  )

(add-hook 'minibuffer-setup-hook 'cycle-mini-reset)

(provide 'cycle-mini)
