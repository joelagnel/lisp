;;; -*- Mode: Emacs-Lisp -*-
;;; File: jump-def.el
;;;
;;; Copyright (C) 1993, 1994, 1995  Karl Fogel
;;;
;;; Author: Karl Fogel <kfogel@cyclic.com>
;;; Maintainer: same
;;; Version: 1.2
;;; Created: July 1993
;;; Keywords: tags
;;;
;;; Jump to a "def" (essentially, a function or variable definition)
;;; in the current buffer.  Buffer should be visiting a file with Lisp
;;; or C code inside it, and be in the right mode for that file
;;; (although it will assume Emacs-Lisp mode if no other options).
;;; Completion is available on "def" names.
;;;
;;; INSTALLATION:
;;;
;;; put something like this in your .emacs file:
;;;
;;; (autoload 'jump-to-def "jump-def" "Jump to a definition." t)
;;; (global-set-key "\C-cj" 'jump-to-def) ;or whatever key you want...
;;;
;;; Then just make sure that the file jump-def.el is installed
;;; somewhere in your load-path.  Byte-compile it for greater
;;; efficiency.
;;;
;;; This is intended as a replacement for tags when you know you are
;;; staying inside one file.  Its advantage is that it does not
;;; require a prebuilt tag table; its disadvantage is that in C mode
;;; it can be quite slow in building the completion list sometimes.
;;; In Lisp-like languages its performance seems to be as fast as
;;; tags, even in huge files.
;;;
;;; Modified 1995-03-08 by Noah Friedman <friedman@splode.com> to add
;;; mode-sensitive jump-def-regexps and better C parsing, including emacs
;;; DEFUNs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; LCD Archive Entry:
;;; jump-def|Karl Fogel|kfogel@cs.oberlin.edu|
;;; Jumping to definitions in a source file (without a tag table).|
;;; 31-July-93|Version: 1.0 $|~/misc/jump-def.el.Z|
;;;
;;; For more information about the copyright and lack of warranty,
;;; write to the author.
;;; If you do not have electronic mail access, write to me at
;;;
;;; Karl Fogel
;;; 1123 N. Oak Park Ave.
;;; Oak Park, ILL
;;; USA      60302
;;;
;;; for more information.
;;;
;;;
;;;

(defvar jump-def-regexp
  '((lisp-mode             . "^(def")
    (emacs-lisp-mode       . "^(def")
    (lisp-interaction-mode . "^(def")
    (scheme-mode           . "^(def")
    ;; searching is slower in c-mode because the regexp is more
    ;; complicated.
    ;(c-mode                . "^\\([^()0-9#\t ][a-zA-Z0-9_]+\\s-+\\)\1*(")
    (c-mode                . "^[^/*()0-9#\t ]\\(\\sw\\|\\s_\\|\\s-\\)+(")
    (perl-mode             . "^sub ")
    (cperl-mode            . "^sub ")
    (prolog-mode           . "^[a-z][a-zA-Z0-9_]+")
    )
  "*Alist of regexps for matching mode-specific defs.")


(defun jump-def-make-def-alist (&optional buf)
  "Returns an alist of all the defs BUF (defaults to current buffer).
In the alist, cars are def names, cdrs are their points in the buffer
\(names are unused right now, but included for those who like that
sort of thing.\)"
  (save-excursion
    (if buf
	(set-buffer (get-buffer buf)))
    (let ((re (cdr (assq major-mode jump-def-regexp)))
          point tem lst)
      (or re (error "No regular expression for mode %s" mode-name))
      (goto-char (point-min))
      (while (re-search-forward re (point-max) t)
	;; as long as there are still defs, do the following:
	(let ((next-from (point)))
	  ;; that's where to start looking from on next iteration.
	  ;; and keep the "let" outside the conds, even though it
	  ;; would be more efficient to have it inside each case,
	  ;; because it's clearer to read this way.
	    (cond
	     ((eq major-mode 'c-mode)
	      (progn
                (goto-char (1- (match-end 0)))
                (backward-sexp)
                (setq tem (jump-def-buffer-substring
                           (point)
                           (progn (forward-sexp)
                                  (point))))
                (setq point (progn
                              (beginning-of-line)
                              (point)))
                (cond ((string= tem "DEFUN")
                       (goto-char (match-end 0))
                       (setq tem (read (current-buffer)))))
		(setq lst (cons (list tem point) lst))))
	     ((eq major-mode 'prolog-mode)
	      (progn
                (goto-char (match-beginning 0))
                (setq tem (jump-def-buffer-substring
                           (point)
                           (progn (forward-sexp)
                                  (point))))
                (setq point (progn
                              (beginning-of-line)
                              (point)))
		(setq lst (cons (list tem point) lst))))
	     (t ; Emacs-Lisp mode
	      ;; defaults to lisp mode behavior, because Schemers can
              ;; make use of it easily too.  But written with a
	      ;; "cond" instead of "if", for future extensibility.
	      (progn
                ;; necessary because we don't know if the regexp was
                ;; "^(def", or maybe "^(defun", or perhaps "^(defun "
		(backward-sexp)
		(forward-sexp)
		(forward-char 1)
		;; make the cell (defname NUMBER) that will be in alist:
		(setq lst
		      (cons
		       (list (jump-def-buffer-substring
			      (point)
			      (progn (forward-sexp)
				     (point)))
			     (progn
			       (beginning-of-line)
			       (point)))
		       lst)))))
	    ;; go to the end of the last matched def and search again:
	    (goto-char next-from)))
      ;; return the alist, now that we're all done
      lst)))


;; ripped off from help.el:
(defun function-called-at-point ()
  (or (condition-case ()
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	      (backward-up-list 1)
	      (forward-char 1)
	      (let (obj)
		(setq obj (read (current-buffer)))
		(and (symbolp obj) obj))))
	(error nil))
      (condition-case ()
	  (save-excursion
	    (forward-sexp -1)
	    (skip-chars-forward "'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) obj)))
	(error nil))))


(defun jump-to-def (def loc)
  "Jump to a DEF at location LOC in the current buffer.
Interactively, you will be prompted completingly for DEF, and LOC will
be computed from that.

A \"def\" is a defun, defconst, or defvar in elisp, or a function
definition in C, assuming that the variable jump-def-regexp is set to
something reasonable.  Completion is available on def names; type TAB
at the prompt to see what\'s in store for you.  If you are within the
scope of a function call, it should default to that function name.

This is intended as a replacement for tags when you know you are
staying inside one file.  Its advantage is that it does not
require a prebuilt tag table; its disadvantage is that in C mode
it can be quite slow in building the completion list sometimes.
In Emacs Lisp its performance seems to be as fast as tags, even in
huge files."
  (interactive (let* ((defs (jump-def-make-def-alist))
                      (fn (function-called-at-point))
                      (sfn (and fn (symbol-name fn)))
                      (val (completing-read
                            (if (and sfn (assoc sfn defs))
                                (format "Jump to (default %s): " fn)
                              "Jump to: ")
                            defs nil t))
                      (cell
                       (assoc
                        (if (string-equal val "")
                            (and (assoc sfn defs)
                                 sfn)
                          val)
                        defs)))
                 cell))
  ;; Not using the def's name right now, but information is in def
  ;; should we ever decide to do something with it...
  (push-mark)
  (goto-char loc)
  (message def))

(defun jump-def-buffer-substring (&rest args)
  (if (fboundp 'buffer-substring-no-properties)
      (apply 'buffer-substring-no-properties args)
    (apply 'buffer-substring args)))

(provide 'jump-def)

;;; jump-def.el ends here ;;;
