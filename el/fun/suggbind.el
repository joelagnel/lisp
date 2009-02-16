;;; suggbind.el --- suggest key bindings for M-x commands AFTER they complete

;; Copyright (C) 1994, 1996, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs; works in Emacs 18 with advice.el.
;; Created: 1994-03-29

;; LCD Archive Entry:
;; suggbind|Noah Friedman|friedman@prep.ai.mit.edu|
;; suggest key bindings for M-x commands AFTER they complete|
;; $Date: 1997/05/18 10:27:40 $|$Revision: 1.5 $|~/misc/suggbind.el.gz|

;; $Id: suggbind.el,v 1.5 1997/05/18 10:27:40 friedman Exp $

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
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Emacs 19.30 changed the behavior of execute-extended-command (i.e. the
;; command which lets you run other commands by typing ``M-x foo'' so that
;; if the variable `suggest-key-bindings' is set, it prints the keybinding
;; (if any) of the command to help you learn shortcuts.

;; Unfortunately, while that behavior is useful, it is also annoying
;; because you have to wait for the message to time out before the command
;; is actually executed.  Hinting at keybindings should be instructive but
;; not force you to use them just to avoid the delay nuisance; at the same
;; time you can't make the delay so short that you have no time to read the
;; message.

;; This package provides alternate behavior: it suggests key bindings
;; *after* the command has completed, and even then only if the user is
;; idle for at least a second (or whatever is set via the variable
;; `suggbind-suggest-key-bindings').  Emacs 20.1 and later have this
;; behavior too, but there are still some other differences.

;; This package cannot be autoloaded.  To use it, put the following in your
;; .emacs:
;;
;;      (setq suggest-key-bindings nil) ; turn off builtin pre-command hints
;;      (load "suggbind")

;;; Code:

(require 'advice)

(defvar suggbind-suggest-key-bindings t
  "*Non-nil means show the equivalent key-binding when M-x command has one.
The value can be a length of idle time to wait before showing the message,
  after a command has completed.
If the value is non-nil and not a number, we wait 1 second.")

(defvar before-execute-extended-command-hook nil
  "*Forms to execute before anything else in `execute-extended-command'.")

(defvar after-execute-extended-command-hook nil
  "*Forms to execute after everything else in `execute-extended-command'.")

(defun suggbind-command-bindings (&optional cmdsym allp)
  (cond ((and (not executing-kbd-macro)
              (cond ((numberp suggbind-suggest-key-bindings)
                     (sit-for suggbind-suggest-key-bindings))
                    (suggbind-suggest-key-bindings
                     (sit-for 1))
                    (t nil)))
         (or cmdsym (setq cmdsym this-command))
         (let* ((local-keymap (and (boundp 'overriding-local-map)
                                   (symbol-value 'overriding-local-map)))
                (bindings (where-is-internal cmdsym local-keymap))
                (p bindings))
           (cond (allp)
                 (bindings
                  (while (cdr p)
                    (and (symbolp (aref (car (cdr p)) 0))
                         (setcdr p (cdr (cdr p))))
                    (setq p (cdr p)))
                  (and (symbolp (aref (car bindings) 0))
                       (setq bindings (cdr bindings)))))
           (and cmdsym
                bindings
                (message "%s is on %s" cmdsym
                         (mapconcat 'key-description bindings ", ")))))))

(defadvice execute-extended-command (around suggbind activate)
  "Run before-execute-extended-command-hook before all else.
Run after-execute-extended-command-hook after all else."
  (run-hooks 'before-execute-extended-command-hook)
  ad-do-it
  ;; Note that by searching the extended command history to find exactly
  ;; the command the user typed, we can suggest key bindings for the right
  ;; command even if the user typed input to the command---and thus
  ;; changing the value of `this-command'.  Emacs' builtin suggestions
  ;; actually do not do this!
  (let ((cmd (cond ((and (boundp 'extended-command-history)
                         extended-command-history)
                    (intern-soft (car extended-command-history)))
                   ;; xemacs doesn't have extended-command-history.
                   ((and (boundp 'command-history)
                         (listp command-history))
                    (car (car command-history))))))
    (suggbind-command-bindings cmd))
  (run-hooks 'after-execute-extended-command-hook))

(provide 'suggbind)

;;; suggbind.el ends here.
