;;; Saved through ges-version 0.3.3dev at 2004-02-20 13:07
;;; ;;; From: Kai Grossjohann <kai@emptydomain.de>
;;; ;;; Subject: shell-integration.el --- invoke Emacs commands from shell buffer
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Tue, 27 Jan 2004 15:14:45 +0100

;;; This is to be understood as a draft exhibiting the concept, not as a
;;; finished package.  I'm posting it here to solicit some comments.

;;; Rationale: eshell is really cool because you can type "find-file foo"
;;; at the shell prompt and it will do like C-x C-f foo RET.  But eshell
;;; tries to do everything in Emacs, and sometimes one would like to talk
;;; to a Bourne-ish (or C-ish or Z-ish, or whatever) shell.

;;; Thus, with this package you can type "vi foo" and this will do like
;;; C-x C-f foo RET.

;;; What do people think?

;;; I get strange beeps, but couldn't find out where they are coming from.
;;; debug-on-{error,signal}, at least, couldn't unearth the cause.

;;; Kai

;;; ;;; shell-integration --- invoke Emacs commands from shell buffer

;;; ;; Copyright (C) 2004 Kai Grossjohann

;;; ;; Author: Kai Grossjohann <kai.grossjohann@gmx.net>
;;; ;; Maintainer: Kai Grossjohann <kai.grossjohann@gmx.net>
;;; ;; Keywords: processes lisp

;;; ;; This file is free software; you can redistribute it and/or modify
;;; ;; it under the terms of the GNU General Public License as published by
;;; ;; the Free Software Foundation; either version 2, or (at your option)
;;; ;; any later version.

;;; ;; This file is distributed in the hope that it will be useful,
;;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; ;; GNU General Public License for more details.

;;; ;; You should have received a copy of the GNU General Public License
;;; ;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; ;; the Free Software Foundation,  Inc., 59 Temple Place - Suite 330,
;;; ;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If there is a function shell/foo, and you type "foo bar baz" at
;; your shell prompt, then this file will arrange it such that
;; (shell/foo PROC "foo bar baz") is invoked, instead of sending the
;; command to the shell.  PROC is the process object representing the
;; underlying shell; you can use it to send some modified input to the
;; shell.

;;; History:

;;; Code:

(defun shell/clear (process command)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun shell/command (process command)
  (comint-simple-send process (comint-arguments command 1 nil)))

(defun shell/vi (process command)
  (find-file (comint-arguments command 1 1) nil))

(defun shell/ec (process command)
  (compile (comint-arguments command 1 nil)))

(defun shell/make (process command)
  (compile command))

(defun shell/load-file (process command)
  (load-file (comint-arguments command 1 1)))

(defun shell/cvs-update (process command)
  (let ((directory (comint-arguments command 2 2))
	(flags (comint-arguments command 3 3)))
    (when (string= directory "") (setq directory nil))
    (when (null directory) (setq directory "."))
    (when (string= flags "") (setq flags nil))
    (setq flags (or flags t))
    (cvs-update directory flags)))

(defun shell-integration-sender (process command)
  (setq command (substring-no-properties command))
  (let* ((program (comint-arguments command 0 0))
	 (func (intern (format "shell/%s" program))))
    (if (not (fboundp func))
	(comint-simple-send process command)
      (comint-simple-send process (format "# %s" command))
      (funcall func process command))))

(defun shell-integration-hook ()
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'shell-integration-sender))

(add-hook 'shell-mode-hook  'shell-integration-hook)

(provide 'shell-integration)

;;; shell-integration.el ends here


