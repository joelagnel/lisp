;;; Saved through ges-version 0.3.3dev at 2003-12-06 13:12
;;; ;;; From: Karl Chen <quarl.emacs@SPAMREMOVE_quarl.org>
;;; ;;; Subject: smartinput.el
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 28 Nov 2003 22:13:11 -0800
;;; ;;; Organization: University of California, Berkeley

;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=kc-smartinput.el
;;; Content-Transfer-Encoding: 8bit

;;; ;;; $Id: kc-smartinput.el 5468 2003-11-29 05:42:18Z quarl $
;;; ;;; kc-smartinput.el --- smart input functions for yes/no as well as
;;; ;;;                      multiple-choice queries.

;;; ;; Copyright (C) 2001-2003 Karl Chen.

;;; ;; This file is not part of GNU Emacs.

;;; ;; This program is free software; you can redistribute it and/or modify
;;; ;; it under the terms of the GNU General Public License as published by
;;; ;; the Free Software Foundation; either version 2, or (at your option)
;;; ;; any later version.

;;; ;; This program is distributed in the hope that it will be useful,
;;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; ;; GNU General Public License for more details.

;;; ;; You should have received a copy of the GNU General Public License
;;; ;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; ;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; ;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The built-in `yes-or-no-p' is annoying.  If you type something incorrect
;; such as "y\r", Emacs will pause for two seconds without letting you go on.

;; This replacement is backwards-compatible to `yes-or-no-p', but also
;; works when you type "y\r".

;; See the documentation for `kc-smartinput-yes-or-no-p'.

;;; Installation:

;; put this in your .emacs:
;;    (require 'kc-smartinput)
;;    (kc-smartinput-install-yes-or-no-p)

;;; Code:

(require 'advice)

;; yanked from isearch.el isearch-mode
(defun kc-show-minibuffer (&optional switch)
  "Maybe make minibuffer frame visible and/or raise it.

If optional argument SWITCH is non-nil, also select the minibuffer window."
  (let ((frame (window-frame (minibuffer-window))))
    (if (not (memq (frame-live-p frame) '(nil t)))
	(progn
	  (make-frame-visible frame)
	  (if minibuffer-auto-raise
	      (raise-frame frame)))))
  (if switch
      (select-window (minibuffer-window))))

(defun kc-read-char-exclusive-1 (&optional prompt inherit-input-method)
  "Read a lowercase character.

Similar to `read-char-exclusive', but don't clear the message
area for non-char input.

Always returns an integer. Translates some common sequences for
compatibility (e.g. 'return to ?\r)."

  (let ((x (read-event prompt inherit-input-method)))
    (cond ((numberp x) (downcase x))
          ((or (eq x 'return) (eq x 'kp-enter)) ?\r)
          ((eq x 'backspace) 127)
          (t 0))))

(defun kc-smartinput-yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.

Takes one argument, which is the string to display to ask the
question.  It should end in a space; `yes-or-no-p' adds `(yes or
no) ' to it.  The user must confirm the answer with RET, and can
edit it until it has been confirmed.

This function is somewhat between the builtin `yes-or-no-p' and
`y-or-n-p'.  You do not have to type the entire word \"yes\" or
\"no\", but you must press RET.  There is no pause when you type
an incorrect key (instead, the prompt changes)."

  (save-window-excursion
    (let ((query "(yes or no) ")
          (input ""))
      (kc-show-minibuffer t)
      (block nil
        (while t
          (setq query
                (if
                    (case (kc-read-char-exclusive-1 (concat prompt query input))
                      (?\r              ; enter
                       (unless (string= input "")
                         (return (= (string-to-char input) ?y))))
                      (127 (setq input "")) ; backspace
                      (?y (setq input "y[es]"))
                      (?e (when (string= input "y[es]") (setq input "ye[s]")))
                      (?s (when (string= input "ye[s]") (setq input "yes")))
                      (?n (setq input "n[o]"))
                      (?o (when (string= input "n[o]") (setq input "no"))))
                    "(yes or no) " "(YES or NO or C-g) ")))))))

(defalias 'kc-super-yes-or-no-p 'kc-smartinput-yes-or-no-p)

;;(defun kc-smartinput-install-yes-or-no-p ()
;;  (fset 'yes-or-no-p 'kc-smartinput-yes-or-no-p))

(defun kc-smartinput-install-yes-or-no-p ()
  "Replace `yes-or-no-p' with `kc-smartinput-yes-or-no-p'.

Uses `defadvice' to do the replacement."
  (defadvice yes-or-no-p (around kc-smartinput-yes-or-no-p activate)
    "Run `kc-smartinput-yes-or-no-p' instead."
    (setq ad-return-value (kc-smartinput-yes-or-no-p (ad-get-arg 0)))))

(defsubst kc--tostring (s)
  (cond ((stringp s) s)
        ((null s) "")
        ((symbolp s) (symbol-name s))))

(defsubst kc-smartinput--choice-string (choice)
  (or (nth 2 choice) (kc--tostring (nth 1 choice))))

(defvar kc-smartinput-completion-buffer "*SmartInput Completions*")

(defun kc-smartinput-multiple-choice-help (choices)
  (with-current-buffer (get-buffer-create kc-smartinput-completion-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (setq buffer-read-only t)
      (erase-buffer)
      (insert "Possible key inputs:\n\n")
      (dolist (choice choices)
        (insert (format "[%c]  %s\n" (nth 0 choice)
                        (kc-smartinput--choice-string choice))))
      (display-buffer (current-buffer)))))

(defun kc-smartinput-read-multiple-choice (prompt choices &optional default)
  "Prompt the user with a multiple-choice query.

CHOICES is a list of (KEY VALUE &optional STRING).

The user must press a KEY followed by RET.  STRING, or VALUE if
STRING is absent or nil, will be displayed.  VALUE will be
returned.  KEYs should be lowercase characters (integers).

If DEFAULT (a key), the user can simply press RET to return the
default value.

Example invocation:

  (kc-smartinput-read-multiple-choice \"Greek letter?\"
                                      '((?a alpha) (?b beta \"Beta\")) ?b)
"
  (save-window-excursion
    (kc-show-minibuffer t)
    (setq prompt (concat prompt
                         " ["
                         (mapconcat (lambda (c) (format "%c" (car c))) choices "")
                         "] "))
    (setq default (and default (assq default choices)))
    (prog1
        (block nil
          (while t
            (let ((new-choice
                   (kc-read-char-exclusive-1
                    (concat prompt (kc-smartinput--choice-string default)))))
              (case new-choice
                (?\r (if default (return (nth 1 default))))
                (127 (setq default nil))
                (t
                 (setq new-choice (assq new-choice choices))
                 (if new-choice
                     (setq default new-choice)
                   (kc-smartinput-multiple-choice-help choices)))))))
      (if (get-buffer kc-smartinput-completion-buffer)
          (kill-buffer kc-smartinput-completion-buffer)))))

(provide 'kc-smartinput)
(provide 'kc-super-yn)

;;; --=-=-=


;;; -- 
;;; Karl Chen 2003-11-28 22:09

;;; --=-=-=--

