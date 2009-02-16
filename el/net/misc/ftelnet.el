;;; ftelnet.el --- remote login interface

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Noah S. Friedman

;; Author: Noah Friedman
;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;; Status: Works in Emacs 19.27 and later.
;; Keywords: unix, comm

;; LCD Archive Entry:
;; ftelnet|Noah Friedman|friedman@prep.ai.mit.edu|
;; remote login interface|
;; $Date: 2000/10/27 09:22:40 $|$Revision: 1.11 $|~/misc/ftelnet.el.gz|

;; $Id: ftelnet.el,v 1.11 2000/10/27 09:22:40 friedman Exp $

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
;; along with this program; if not, write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue.; Cambridge, MA 02139, USA.

;;; Commentary:

;; This code originally derived from emacs 19.29 rlogin.el, and was then
;; modified substantially for use with telnet.

;; Support for remote logins using `telnet'.
;; This program is layered on top of shell.el; the code here only accounts
;; for the variations needed to handle a remote process, e.g. directory
;; tracking and the sending of some special characters.

;; If you wish for ftelnet mode to prompt you in the minibuffer for
;; passwords when a password prompt appears, just enter m-x send-invisible
;; and type in your line, or add `comint-watch-for-password-prompt' to
;; `comint-output-filter-functions'.

;;; Code:

(require 'comint)
(require 'shell)

(defvar ftelnet-program "telnet"
  "*Name of program to invoke telnet")

(defvar ftelnet-explicit-args nil
  "*List of arguments to pass to telnet on the command line.")

(defvar ftelnet-mode-hook nil
  "*Hooks to run after setting current buffer to ftelnet-mode.")

(defvar ftelnet-process-connection-type nil
  "*If non-`nil', use a pty for the local telnet process.
If `nil', use a pipe (if pipes are supported on the local system).

Generally it is better not to waste ptys on systems which have a static
number of them.  On the other hand, some implementations of `telnet' assume
a pty is being used, and errors will result from using a pipe instead.")

(defvar ftelnet-directory-tracking-mode 'local
  "*Control whether and how to do directory tracking in a telnet buffer.

nil means don't do directory tracking.

t means do so using an ftp remote file name.

Any other value means do directory tracking using local file names.
This works only if the remote machine and the local one
share the same directories (through NFS).  This is the default.

This variable becomes local to a buffer when set in any fashion for it.

It is better to use the function of the same name to change the behavior of
directory tracking in a telnet session once it has begun, rather than
simply setting this variable, since the function does the necessary
re-synching of directories.")

(make-variable-buffer-local 'ftelnet-directory-tracking-mode)

(defvar ftelnet-host nil
  "The name of the remote host.  This variable is buffer-local.
There is usually no need to set this yourself.
")

(defvar ftelnet-remote-user nil
  "The username used on the remote host.
This variable is buffer-local and defaults to your local user name.
There is usually no need to set this yourself.")

;; Initialize telnet mode map.
; (setq ftelnet-mode-map nil)
(defvar ftelnet-mode-map '())
(cond
 ((null ftelnet-mode-map)
  (setq ftelnet-mode-map (if (consp shell-mode-map)
                            (cons 'keymap shell-mode-map)
                          (copy-keymap shell-mode-map)))
  (define-key ftelnet-mode-map "\C-c\C-c" 'ftelnet-send-interrupt)
  (define-key ftelnet-mode-map "\C-c\C-d" 'ftelnet-send-eof)
  (define-key ftelnet-mode-map "\C-c\C-z" 'ftelnet-send-suspend)
  (define-key ftelnet-mode-map "\C-c\C-\\" 'ftelnet-send-quit)
  (define-key ftelnet-mode-map "\C-c\C-]" 'ftelnet-send-escape)
  (define-key ftelnet-mode-map "\C-d" 'ftelnet-delete-or-send-eof)
  (define-key ftelnet-mode-map "\C-i" 'ftelnet-tab-or-complete)))

(defvar ftelnet-urgent-alist
  '((escape      . "\C-]")
    (erase       . "\C-?")
    (flushoutput . "\C-o")
    (interrupt   . "\C-c")
    (kill        . "\C-u")
    (quit        . "\C-\\")
    (eof         . "\C-d")))

(defvar ftelnet-history nil
  "*History ring for ftelnet input arguments.")


;; See comments near ftelnet-pop-to-buffer for an explanation.
;;;###autoload (add-hook 'same-window-regexps "^\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun ftelnet (input-args &optional buffer)
  "Open a network login connection to HOST via the `telnet' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer *telnet-HOST*
\(or *telnet-HOST:PORT* if using a nonstandard port number\).
If a prefix argument is given and the buffer *telnet-HOST* already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or
buffer, it names the buffer to use.

The variable `ftelnet-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ftelnet-explicit-args' is a list of arguments to give to the
telnet program when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `ftelnet-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ftelnet-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ftelnet-directory-tracking-mode' rather than simply setting the
variable."
  (interactive (list
		(read-from-minibuffer "telnet host (and optional port): "
                                      nil nil nil 'ftelnet-history)
		current-prefix-arg))
  (let* ((process-connection-type ftelnet-process-connection-type)
         (args (if ftelnet-explicit-args
                   (append (ftelnet-parse-words input-args)
                           ftelnet-explicit-args)
                 (ftelnet-parse-words input-args)))
         ;; skip args starting with `-'
         (nonopt-args (let ((l args))
                        (while (= ?- (aref (car l) 0))
                          (setq l (cdr l)))
                        l))
	 (host (car nonopt-args))
	 (port (car (cdr nonopt-args)))
         (buffer-name (if port
                          (format "*telnet-%s:%s*" host port)
                        (format "*telnet-%s*" host)))
	 proc)

    (cond ((null buffer))
	  ((stringp buffer)
	   (setq buffer-name buffer))
          ((bufferp buffer)
           (setq buffer-name (buffer-name buffer)))
          ((numberp buffer)
           (setq buffer-name (format "%s<%d>" buffer-name buffer)))
          (t
           (setq buffer-name (generate-new-buffer-name buffer-name))))

    (setq buffer (get-buffer-create buffer-name))
    (ftelnet-pop-to-buffer buffer-name)

    (cond
     ((comint-check-proc buffer-name))
     (t
      (comint-exec buffer buffer-name ftelnet-program nil args)
      (setq proc (get-buffer-process buffer))
      ;; Set process-mark to point-max in case there is text in the
      ;; buffer from a previous exited process.
      (set-marker (process-mark proc) (point-max))

      ;; comint-output-filter-functions is treated like a hook: it is
      ;; processed via run-hooks or run-hooks-with-args in later versions
      ;; of emacs.
      ;; comint-output-filter-functions should already have a
      ;; permanent-local property, at least in emacs 19.27 or later.
      (cond
       ((fboundp 'make-local-hook)
        (make-local-hook 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'ftelnet-carriage-filter
                  nil t))
       (t
        (make-local-variable 'comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'ftelnet-carriage-filter)))

      (ftelnet-mode)

      ;; initial filter to get remote user name if connecting to a telnet
      ;; login port.
      (cond
       ((or (null port)
            (string= port "23"))
        (add-hook 'comint-output-filter-functions
                  'ftelnet-user-output-filter)))

      (make-local-variable 'ftelnet-host)
      (setq ftelnet-host host)
      (make-local-variable 'ftelnet-remote-user)
      (setq ftelnet-remote-user nil)

      (cond
       ((eq t ftelnet-directory-tracking-mode))
       ((null ftelnet-directory-tracking-mode))
       (t
        (cd-absolute (concat comint-file-name-prefix "~/"))))))))

(defun ftelnet-mode ()
  "Set major-mode for ftelnet sessions.
If `ftelnet-mode-hook' is set, run it."
  (interactive)
  (kill-all-local-variables)
  (shell-mode)
  (setq major-mode 'ftelnet-mode)
  (setq mode-name "ftelnet")
  (use-local-map ftelnet-mode-map)
  (setq shell-dirtrackp ftelnet-directory-tracking-mode)
  (make-local-variable 'comint-file-name-prefix)
  (run-hooks 'ftelnet-mode-hook))

(defun ftelnet-directory-tracking-mode (&optional prefix)
  "Do remote or local directory tracking, or disable entirely.

If called with no prefix argument or a unspecified prefix argument (just
``\\[universal-argument]'' with no number) do remote directory tracking via
ange-ftp.  If called as a function, give it no argument.

If called with a negative prefix argument, disable directory tracking
entirely.

If called with a positive, numeric prefix argument, e.g.

         \\[universal-argument] 1 \\[ftelnet-directory-tracking-mode]

then do directory tracking but assume the remote filesystem is the same as
the local system.  This only works in general if the remote machine and the
local one share the same directories (through NFS)."
  (interactive "P")
  (cond
   ((or (null prefix)
        (consp prefix))
    (setq ftelnet-directory-tracking-mode t)
    (setq shell-dirtrackp t)
    (setq comint-file-name-prefix
          (concat "/" ftelnet-remote-user "@" ftelnet-host ":")))
   ((< prefix 0)
    (setq ftelnet-directory-tracking-mode nil)
    (setq shell-dirtrackp nil))
   (t
    (setq ftelnet-directory-tracking-mode 'local)
    (setq comint-file-name-prefix "")
    (setq shell-dirtrackp t)))
  (cond
   (shell-dirtrackp
    (let* ((proc (get-buffer-process (current-buffer)))
           (proc-mark (process-mark proc))
           (current-input (buffer-substring proc-mark (point-max)))
           (orig-point (point))
           (offset (and (>= orig-point proc-mark)
                        (- (point-max) orig-point))))
      (unwind-protect
          (progn
            (delete-region proc-mark (point-max))
            (goto-char (point-max))
            (shell-resync-dirs))
        (goto-char proc-mark)
        (insert current-input)
        (if offset
            (goto-char (- (point-max) offset))
          (goto-char orig-point)))))))

;; Parse a line into its constituent parts (words separated by
;; whitespace).  Return a list of the words.
(defun ftelnet-parse-words (line)
  (let ((list nil)
	(posn 0)
        (match-data (match-data)))
    (while (string-match "[^ \t\n]+" line posn)
      (setq list (cons (substring line (match-beginning 0) (match-end 0))
                       list))
      (setq posn (match-end 0)))
    (store-match-data (match-data))
    (nreverse list)))

;; Starting in Emacs 19.29, the variable same-window-regexps modifies how
;; pop-to-buffer works; in particular, if the name of the buffer being
;; switched to matches one of the regexps in same-window-regexps (which may
;; be buffer-local to the current buffer), then pop-to-buffer acts like
;; switch-to-buffer.  This gives users more control.
;; This also explains the related autoload cookie near the top of the file.
(defun ftelnet-pop-to-buffer (buffer)
  (if (boundp 'same-window-regexps)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))


;; This should go on comint-output-filter-functions initially.
;; Once it detects that a username has been prompted for, it adds an input
;; filter that saves the username.
(defun ftelnet-user-output-filter (s)
  (let ((data (match-data)))
    (cond
     ;; I fail to see how or why a process filter would get invoked with
     ;; output consisting of the empty string, but it happens.
     ((string= s ""))
     ((string-match "\\(ogin: \\)\\|\\(sername: \\)$" s)
      (add-hook 'comint-input-filter-functions 'ftelnet-user-input-filter))
     ((string-match "^[\C-m\n]+$" s))
     ((string-match "assword:[ \t]*$" s))
     ((string-match ".*\\(incorrect\\)\\|\\(authorization failure\\)$" s))
     ((null ftelnet-remote-user))
     ((and ftelnet-remote-user
           (string-match (concat ftelnet-remote-user "[\C-m\n]*$") s)))
     (t
      (remove-hook 'comint-output-filter-functions 'ftelnet-user-output-filter)
      (remove-hook 'comint-input-filter-functions 'ftelnet-user-input-filter)
      (cond
       ((eq ftelnet-directory-tracking-mode t)
        (cd-absolute comint-file-name-prefix)))))
    (store-match-data data)))

(defun ftelnet-user-input-filter (s)
  (remove-hook 'comint-input-filter-functions 'ftelnet-user-input-filter)
  (setq ftelnet-remote-user (car (ftelnet-parse-words s)))
  (cond
   ((eq ftelnet-directory-tracking-mode t)
    (setq comint-file-name-prefix
          (concat "/" ftelnet-remote-user "@" ftelnet-host ":")))))

(defun ftelnet-carriage-filter (string)
  (let* ((point-marker (point-marker))
         (end (process-mark (get-buffer-process (current-buffer))))
         (beg (or (and (boundp 'comint-last-output-start)
                       comint-last-output-start)
                  (- end (length string)))))
    (goto-char beg)
    (while (search-forward "\C-m" end t)
      (delete-char -1))
    (goto-char point-marker)))


;; Definitions for keybindings

(defun ftelnet-send-urgently (urgsym)
  (let ((s (cdr (assq urgsym ftelnet-urgent-alist))))
    (and s (comint-send-string (get-buffer-process (current-buffer)) s))))

(defun ftelnet-send-escape ()
  (interactive)
  (ftelnet-send-urgently 'escape))

(defun ftelnet-send-interrupt ()
  (interactive)
  (ftelnet-send-urgently 'interrupt))

(defun ftelnet-send-eof ()
  (interactive)
  (ftelnet-send-urgently 'eof))

(defun ftelnet-send-quit ()
  (interactive)
  (ftelnet-send-urgently 'quit))

(defun ftelnet-send-suspend ()
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun ftelnet-delete-or-send-eof (arg)
  "\
Delete ARG characters forward, or send an eof to process if at end of buffer."
  (interactive "p")
  (if (eobp)
      (ftelnet-send-eof)
    (delete-char arg)))

(defun ftelnet-tab-or-complete ()
  "Complete file name if doing directory tracking, or just insert TAB."
  (interactive)
  (if ftelnet-directory-tracking-mode
      (comint-dynamic-complete)
    (insert "\C-i")))


(provide 'ftelnet)

;; local variables:
;; vc-make-backup-files: t
;; end:

;;; ftelnet.el ends here
