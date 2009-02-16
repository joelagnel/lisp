;;; mallow.el --- Hober's ERC bot for OPN's #emacs channel.

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A simple bot that I'm running in OPN's #emacs channel. I run it
;; in a detached screen session.

;;; Code:

;; For `mallow-cube'. Pretty gratuitous.
(require 'url)

;; Ensure that a *fortune* buffer exists.
(fortune)

;; Don't you think that erc-robot.el should require these?
(require 'erc)
(require 'erc-stamp)

;; Note that I've had to make some changes to erc-robot.el in
;; order to make it work. See EmacsWiki:ErcRobot for more.
(require 'erc-robot)

(defconst mallow-major-version 1 "Major version of Mallow.")
(defconst mallow-minor-version 1 "Minor version of Mallow.")
(defconst mallow-version (format "Mallow %d.%d"
                                 mallow-major-version
                                 mallow-minor-version)
  "Version of Mallow.")

;; Note that this function depends on `ted-emacs-name', defined in
;; my ~/.emacs file.
(defun mallow-version (&rest args)
  "What version of Mallow is this?"
  (format "This is %s running under %s on %s."
          mallow-version (erc-version) ted-emacs-name))

(defvar mallow-max-msg-size 500
  "*The largest message size Mallow should be happy with.")

(defun mallow-crop (string)
  "If STRING is too long, crop it to some reasonable size."
  (if (> (length string) mallow-max-msg-size)
      (concat (substring string 0 (- mallow-max-msg-size 3)) "...")
    string))
(put 'mallow-crop 'lisp-indent-function 0)

(defun mallow-about (&rest ignore)
  "All about Mallow."
  (mallow-crop
    (concat
     "I am Mallow, a happy fun IRC bot for OPN's #emacs channel. "
     "You can read more about me on the Emacs Wiki. "
     "I'm built around the erc-robot code originally written by David Edmondson, "
     "though hober is using a locally modified version for me.\n"
     "See also EmacsWiki:MallowBot, EmacsWiki:ErcRobot, and EmacsWiki:EdwardOConnor.")))

(defun mallow-help (&rest ignore)
  "Get help on using Mallow."
  (concat "Mallow Help:\n"
	  (mallow-version) "\n"
	  (mapconcat (lambda (element)
		       (sit-for 1)
		       (format "%s: %s"
			       (car element)
			       (mallow-df (symbol-name (nth 2 element)))))
		     erc-robot-commands
		     "\n")))

(defun mallow-cmds (&rest ignore)
  "List the available bot commands."
  (mallow-crop
    (concat "Mallow commands available: "
            (mapconcat 'car erc-robot-commands " "))))

(defun mallow-dv (variable-name)
  "An interface to `describe-variable'."
  (with-current-buffer (get-buffer "*scratch*")
    (mallow-crop
      (let ((sym (intern-soft variable-name)))
	(if sym
	    (concat variable-name ": "
		    (erc-replace-regexp-in-string
		     "\n" " "
		     (documentation-property
		      sym
		      'variable-documentation)))
	  (format "I'm sorry, I don't know anything about `%s'." variable-name))))))

(defun mallow-make-list-of-arg-list-symbols (min &optional max)
  "Make a list of arbitrary arg list symbols."
  (let ((retval '())
        (i 0))
    (while (< i min)
      (setq retval (nconc (list (intern (format "arg-%d" (+ i 1)))) retval))
      (setq i (+ i 1)))
    (cond ((numberp max)
           (when (/= min max)
             (setq retval (nconc (list '&optional) retval)))
           (while (< i max)
             (setq retval (nconc (list (intern (format "arg-%d" (+ i 1)))) retval))
             (setq i (+ i 1))))
          (t
           (setq retval (nconc (list (intern "arg-n") '&rest) retval))))
    (nreverse retval)))

(defun mallow-arg-list (func)
  "Fetch the arg list of FUNC."
  (cond ((and (symbolp func) (fboundp func))
         (mallow-arg-list (symbol-function func)))
        ((byte-code-function-p func)
         (aref func 0))
        ((and (listp func) (eq (car func) 'lambda))
         (nth 1 func))
        ((subrp func)
         (let* ((arity-cons (subr-arity func))
                (min (car arity-cons))
                (max (cdr arity-cons)))
           (cond ((memq max '(unevalled many))
                  (mallow-make-list-of-arg-list-symbols min))
                 (t
                  (mallow-make-list-of-arg-list-symbols min max)))))
        (t (signal 'wrong-type-argument '(functionp func)))))

(defun mallow-df (function-name)
  "An interface to `describe-function'."
  (mallow-crop
    (let ((sym (intern-soft function-name)))
      (if sym
          (concat function-name " "
                  (condition-case nil
                      (format "%s" (mallow-arg-list sym))
                    (wrong-type-argument "[Arg list not available]"))
                  ": "
                  (erc-replace-regexp-in-string
                   "\n" " "
                   (documentation sym)))
        (format "I'm sorry, I don't know anything about `%s'." function-name)))))

(defun mallow-dk (key-text)
  "An interface to `describe-key-briefly'."
  (with-current-buffer (get-buffer "*scratch*")
    (mallow-crop
      (let* ((key (read-kbd-macro key-text))
	     (binding (key-binding key)))
	(if binding
	    (format "%s is bound to `%s'"
		    key-text binding)
	  (format "%s is unbound"
		  key-text))))))

;;; VFAQ

;; Currently, the VFAQ is simply a buffer in my running Emacs.

;; Unlike traditional bot factoid databases, there are no keys;
;; each line is an entry, and to find an entry, we regexp search
;; in the buffer. This has some good points and some bad points.

;; I happen to think that the good points outweigh the bad points.

(defvar mallow-vfaq-buffer (find-file "~/.mallow-vfaq")
  "Buffer containing a list of answers to very frequently asked questions.
Mostly pilfered from Alex Schroeder's list. Each line is a
different answer.")

(defun mallow-vfaq (text)
  "Look up something in an Emacs VFAQ."
  (let ((retval (format "Sorry, I don't know anything about ``%s''." text))
        (regexp (regexp-quote text)))

    (with-current-buffer mallow-vfaq-buffer
      (goto-char (point-min))
      (let ((result (re-search-forward regexp nil t)))
        (when result
          (setq retval (buffer-substring
                        (progn (beginning-of-line) (point))
                        (progn (end-of-line) (point))))))
      (goto-char (point-min)))

    (mallow-crop retval)))

(defun mallow-vfaq-learn (text)
  "Add an entry to the Emacs VFAQ."
  (with-current-buffer mallow-vfaq-buffer
    (goto-char (point-max))
    (insert (erc-replace-regexp-in-string "\n" " " text) "\n"))
  (mallow-crop "OKl I've added that to the VFAQ. Thanks!"))

(defun mallow-whereis (command-name)
  "An interface to `where-is'."
  (mallow-crop
    (let ((sym (intern-soft command-name)))
      (if (and sym (commandp sym))
          (let ((keys (mapconcat 'key-description
                                 (where-is-internal sym)
                                 ", ")))
            (if (string-equal keys "")
                (format "%s is not on any key" sym)
              (format "%s is on %s"
                      sym keys)))
        (format "%s is not a command" sym)))))

(defun mallow-rot13 (text)
  "Caesar some text."
  (let ((current 0)
        (retval (copy-sequence text)))
    (while (< current (length retval))
      (let ((char (aref retval current)))
        (when (string-match "[A-Za-z]" (string char))
          (setq char
                (cond ((<= char ?M) (+ char 13))
                      ((>= char ?n) (- char 13))
                      ((<= char ?Z) (- char 13))
                      (t (+ char 13))))
          (aset retval current char)))
      (setq current (+ current 1)))
    retval))

(defvar mallow-cubism ""
  "The last cubism fetched by Mallow.")

(defun mallow-cube-internal ()
  "Extract the cubism from the URL buffer."
  (delete-trailing-whitespace)
  (goto-char (point-min))
  (re-search-forward "\n\n" nil t)
  (setq mallow-cubism
        (buffer-substring-no-properties (point) (- (point-max) 2)))
  (kill-buffer (current-buffer)))

(defun mallow-cube (&rest ignore)
  "Spout a random cubism."
  (url-retrieve "http://www.enweirdenment.org/cgi-bin/cube.html"
                'mallow-cube-internal)
  (sit-for 2)
  (mallow-crop
    (concat "The cube says \""
	    (erc-replace-regexp-in-string "\n" " " mallow-cubism)
	    "\"")))

(defun mallow-fortune (&rest ignore)
  "Spout a random fortune."
  (fortune-in-buffer nil "/usr/games/lib/fortunes/fortunes-homepage")
  (mallow-crop (erc-replace-regexp-in-string
                "\n" " "
                (erc-replace-regexp-in-string
                 "\\([ ]+\\)" " "
                 (with-current-buffer (get-buffer fortune-buffer-name)
                   (buffer-substring (point-min) (point-max)))))))

(defun mallow-zippy (&rest ignore)
  "Don't you just love the pinhead?"
  (mallow-crop
    (concat "The pinhead says \""
            (erc-replace-regexp-in-string "\n" " " (yow))
            "\"")))

(defun mallow-source (&rest ignore)
  "Use the source, Luke!"
  "The source to Mallow lives here: <URL:http://oconnor.cx/elisp/mallow.el>")

(setq erc-robot-commands
      '(;; First, the meta commands.
        ("about" t mallow-about)
        ("version" t mallow-version)

        ("cmds"    t mallow-cmds)
        ("help"    t mallow-help)

        ;; Mallow wrappers around Emacs help facilities
        ("dv"      t mallow-dv) ("describe-variable" t mallow-dv)
        ("df"      t mallow-df) ("describe-function" t mallow-df)
        ("dk"      t mallow-dk) ("describe-key"      t mallow-dk)
        ("whereis" t mallow-whereis)

        ;; FAQ
        ("?" t mallow-vfaq) ("faq" t mallow-vfaq)
        ("learn" t mallow-vfaq-learn)

	("source" t mallow-source)

        ;; Fun commands.
        ("cube"    t mallow-cube)
	("fortune" t mallow-fortune)
	("rot13" t mallow-rot13)
        ("zippy"   t mallow-zippy)))

(add-hook 'erc-server-PRIVMSG-hook 'erc-robot-remote t)
(add-hook 'erc-send-completed-hook 'erc-robot-local t)

;; ErcTruncation makes Mallow *sad*.
;; (setq erc-max-buffer-size 30000)
;; (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
;; (setq erc-truncate-buffer-on-save t)

(defun mallow ()
  "My ERC bot."
  (interactive)
  ;; This doesn't work.
  (erc "irc.openprojects.net" 6667 "mallow" "Hober's Mallow" t nil '()))

(provide 'mallow)
;;; mallow.el ends here
