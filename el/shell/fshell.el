;;; fshell.el --- enhancements to shell.el

;; Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995, 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions, processes
;; Created: 1994-06-21

;; $Id: fshell.el,v 1.13 2006/02/17 18:45:00 friedman Exp $

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
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; If you give M-x fshell a prefix arg after loading this, it will create a
;; new shell buffer even if one already exists.  If you give it an explicit
;; numeric prefix arg, it will try to switch to that numbered shell buffer,
;; or create it.
;;
;; The alternative is to rename the current shell buffer and invoke M-x
;; shell, which is more keystrokes, especially if you decide to name your
;; old shell back when you're done with the newer one.

;; rms declined to add this functionality in emacs' shell.el, so I'm
;; maintaining it separately.

;;; Code:

(require 'shell)

;;;###autoload
(defvar fshell-default-make-new-shell nil
  "*If non-`nil', reverse the meaning of prefix arg to \\[fshell]")

;;;###autoload
(defvar fshell-make-shell-hook nil
  "*Forms to run in a new shell buffer just before the process is started.")

;;;###autoload
(defvar fshell-buffer-name "*shell*"
  "*Buffer name for shell process.")

;; See comments near fshell-pop-to-buffer for an explanation.
;;;###autoload (add-hook 'same-window-regexps "^\\*shell\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun fshell (&optional prefix)
  "Run an inferior shell, with I/O through buffer *shell*.
The actual name of the buffer can be specified with the variable
`fshell-buffer-name', but for the sake of brevity the default will be used
in the examples below.

If buffer exists but shell process is not running, make new shell.

If buffer exists and shell process is running, just switch to buffer
 named \"*shell*\".
If an explicit numeric prefix argument is given (or this function is called
  from lisp with a numeric argument), switch to the buffer *shell*<prefix>,
  e.g. \"*shell*<2>\".  If there is no process in that buffer, start one.
If a prefix argument is given but it is not a number, create a new buffer
  and start a shell process in it.  This is the same as calling the function
  from lisp with an argument of `t'.

The previous paragraph describes the behavior of this function whenever it
is called from lisp.  If it is called interactively and the variable
`fshell-default-make-new-shell' is non-`nil', then the meaning of
non-numeric prefix arguments is reversed,
i.e. typing `\\[fshell]' without a prefix argument creates a new shell,
and `\\[universal-argument] \\[fshell]' would switch to the buffer \"*shell*\".

Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-sh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Type \\[describe-mode] in the shell buffer for a list of commands."
  (interactive "P")

  (and fshell-default-make-new-shell
       (interactive-p)
       (not (numberp prefix))
       (setq prefix (not prefix)))

  (let ((shell-buffer (fshell-make-new-buffer-name)))
    (cond
     ((and (null prefix)
           (comint-check-proc shell-buffer))
      (fshell-pop-to-buffer shell-buffer))
     ;; This next case is done all in the predicate (including side effects
     ;; like fshell-pop-to-buffer) to avoid extra string consing via multiple
     ;; concats.
     ((and (numberp prefix)
           (let ((bufname (fshell-make-new-buffer-name shell-buffer prefix)))
             (and (comint-check-proc bufname)
                  (fshell-pop-to-buffer bufname)))))
     (t
      (cond
       ((numberp prefix)
        (setq shell-buffer (fshell-make-new-buffer-name shell-buffer prefix)))
       (prefix
        (setq shell-buffer (fshell-make-new-buffer-name shell-buffer t))))
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	(set-buffer (apply 'fshell-make-shell shell-buffer prog
			   (if (file-exists-p startfile) startfile)
			   (if (and xargs-name (boundp xargs-name))
			       (symbol-value xargs-name)
			     '("-i"))))
        (shell-mode)
        (fshell-pop-to-buffer (current-buffer)))))))

(defun fshell-no-tty (&optional prefix)
  "Like \\[fshell], but never allocate a pseudo-tty for the shell."
  (interactive "P")
  (let ((process-connection-type nil))
    (if (interactive-p)
        (call-interactively 'fshell)
      (fshell prefix))))

;; This can be customized with defadvice or redefined.
(defun fshell-make-new-buffer-name (&optional name number)
  (unless name
    (setq name fshell-buffer-name))
  (cond ((numberp number)
         (format "%s<%d>" name number))
        (number
         (generate-new-buffer-name name))
        (t name)))

;; This is just like comint.el:make-comint, except that it doesn't
;; implicitly put asterisks around the buffer name; it is assumed that's
;; already been done if it's desired.
(defun fshell-make-shell (name program &optional startfile &rest switches)
  "Make a comint process NAME in a buffer, running PROGRAM.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to
the process.  Any more args are arguments to PROGRAM."
  (let ((buffer (get-buffer-create name)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode.  Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc buffer))
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)  ; Install local vars, mode, keymap, ...
             (run-hooks 'fshell-make-shell-hook)
             (comint-exec buffer name program startfile switches))))
    buffer))

;; Starting in Emacs 19.29, the variable same-window-regexps modifies how
;; pop-to-buffer works; in particular, if the name of the buffer being
;; switched to matches one of the regexps in same-window-regexps (which may
;; be buffer-local to the current buffer), then pop-to-buffer acts like
;; switch-to-buffer.  This gives users more control.
;; This also explains the related autoload cookie near the top of the file.
(defun fshell-pop-to-buffer (buffer)
  (if (fboundp 'same-window-regexps)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

;; Potentially useful function to put on fshell-make-shell-hook.
;; If you are in a shell buffer and you create a new shell, the new shell
;; inherits the current working directory of the previous buffer, but it
;; may resolve symbolic links to determine where it really is, thus making
;; the names of emacs' default-directory and the shell's internal current
;; working different.  Some shells, e.g. Bash, will not do this if they
;; inhereit a PWD environment variable.
;;
;; Note that this modification of process-environment is very short-lived.
;; Because shell-mode (called later) calls comint-mode, and the latter
;; destroys all local variables, this local process-environment is gone by
;; the time fshell returns.  That's ok, because we only needed it long
;; enough for `start-process' to see it.
(defun fshell-inherit-cwd ()
  (let ((dir (file-name-as-directory default-directory)))
    (cond ((file-accessible-directory-p dir)
           (make-local-variable 'process-environment)
           (setq process-environment
                 (copy-sequence (default-value 'process-environment)))
           ;; Bash can't handle a cwd of `~/foo', only `~'.  But since it
           ;; will abbreviate the home directory portion of an absolute path
           ;; to `~' internally anyway, expand it here.
           (and (> (length dir) 1)
                (string= "~/" (substring dir 0 2))
                (setq dir (expand-file-name dir)))
           (setenv "PWD"
                   (substring dir 0 (1- (length dir))))))))

(provide 'fshell)

;;; fshell.el ends here
