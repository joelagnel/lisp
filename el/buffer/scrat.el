;;; Saved through ges-version 0.3.3dev at 2004-11-20 18:07
;;; ;;; From: Joe Corneli <jcorneli@math.utexas.edu>
;;; ;;; Subject: scrat.el
;;; ;;; Newsgroups: gmane.emacs.sources
;;; ;;; Date: Sun, 27 Jun 2004 14:53:56 -0500

;;; Here is a simple function that lets you access your scatch buffer
;;; with `M-x scratch'.  There are some additional bonus features
;;; included as well.  I think it would be nice if this package could be
;;; included in Emacs.

;;; scrat.el --- access *scratch* like you do *shell*

;; Copyright (C) 2004 Joe Corneli <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Sun Jun 27 14:45:20 CDT 2004>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a function `scratch' that is similar to `shell',
;; `info', `grep', and other such functions.  By this I mean M-x
;; scratch takes you to the *scratch* buffer if one exists, otherwise
;; it generates a new *scratch* buffer.  User-defined customizations
;; along the following lines allow you to choose a new name for the
;; scratch buffer, or to use a different major mode, or to add minor
;; modes:
;;
;; (defun itch () (interactive)
;;   (scratch "*itch*"))
;;
;; (defun notes () (interactive)
;;   (scratch "*notes*" 'text-mode))
;;
;; (defun music () (interactive)
;;   (scratch "*music*" 'fundamental-mode 
;;                      'musical-letters-mode 
;;                      'auto-fill-mode))
;;
;; The function `back-from-scratch' is also provided, to take you
;; back from your most recent scratch buffer to the previous buffer
;; (restoring the window configuration).
;;
;; For those who use the basic functionality, I recommend reading the
;; documentation of `lisp-interaction-mode'.
;;
;; In order to use these functions effectively, you may want to bind
;; them to keystrokes:
;;
;; Here are a couple suggested bindings for the main functions:
;; (C-s-SPC and C-s-RET):
;;
;; (global-set-key (quote [75497504]) 'scratch)
;; (global-set-key (quote [C-s-return]) 'back-from-scratch)
;;
;; And for the derived functions, you can use things like this
;; (C-s-m):
;;
;; (global-set-key (quote [8388621]) 'music)

;;; History:

;; Earlier this year, I encouraged the Emacs developers to make
;; `info' more like `shell', and they did, which I appreciated.  This
;; mode is inspired by this new more consistent interface.

;;; Code:

(defvar pre-scratch-buffer nil 
"Name of active buffer before switching to the scratch buffer.")

(defvar pre-scratch-config nil 
"Window configuration before switching to the scratch buffer.")

(defun scratch (&optional buffername initmode &rest minormodes)
  "Access an existing scratch buffer or create one anew if none exists.
This is similar to the function `shell' for accessing or creating
an interactive shell.

Optional argument BUFFERNAME specifies the scratch buffer name;
the default buffer name is *scratch*.

Optional argument INITMODE specifies the major mode in which the
scratch buffer should be initialized; the default mode is
`initial-major-mode'.

Any remaining arguments are MINORMODES to apply in the scratch
buffer."
  (interactive)
  (setq pre-scratch-buffer (buffer-name (current-buffer)))
  (setq pre-scratch-config (current-window-configuration))
  (let* ((name (if buffername
                   buffername
                 "*scratch*"))
         (live (buffer-live-p (get-buffer name)))
         (buffer (get-buffer-create name)))
    (pop-to-buffer buffer)
    ;; switch to `lisp-interaction-mode' (or whatever the initial
    ;; major mode is) if the buffer is in `fundamental-mode' (or
    ;; whatever the default major mode is)...
    (if (equal mode-name (with-temp-buffer (funcall default-major-mode)
                                           mode-name))
        ;; but only if the buffer has not been fiddled with
        (unless live
          (funcall (if initmode
                       initmode
                     initial-major-mode))
          ;; also take the opportunity to set up any minor modes
          ;; that were requested
          (mapcar 'funcall minormodes)))
    buffer))

(defun back-from-scratch ()
  "Used after running `scratch' to restore the window
configuration to the state it was in beforehand."
  (interactive)
  (if (not (get-buffer pre-scratch-buffer))
      (message "Buffer %s no longer exists!" pre-scratch-buffer)
    (set-window-configuration pre-scratch-config)))

;;; Notes:

;; There is not any explicit type checking in `scratch', so if you
;; suspect that someone might pass something other than modes in for
;; `initmode' and/or `minormodes', well, you had better hope that they
;; know what they are doing.  We could add tests similar to the
;; `with-temp-buffer' test we already use to detect whether these
;; things that are being passed in are actually functions that set up
;; modes, but that seems like overkill at this point.

;; It would be nice to be able to provide an easy way to custom
;; accessor functions, but that seems to involve interning 
;; new symbols as part of the customization process, and I'm not sure how
;; to do that.

;; (defgroup scratch nil "Customization group for scratch buffers." 
;;   :group 'convenience)

;; (defcustom scratch-buffers '()
;;   "Alist of scratch buffers other than *scratch*.  Elements have
;; the form (BUFFERNAME MAJORMODE MINORMODES), where BUFFERNAME is
;; the name of a certain flavor of scratch buffer, MAJORMODE is the
;; major editing mode for that buffer, and MINORMODES is a list of
;; minor modes for that buffer."
;;   :type
;;   '(repeat (list (string :tag "Buffer name") 
;;                  (function :tag "Major mode") 
;;                  (repeat (function :tag "Minor mode"))))
;;   :group 'scratch
;;   :initialize 'custom-initialize-default-hacked)

;; (defun custom-initialize-default-hacked (symbol value)
;;   "Initialize SYMBOL with VALUE.
;; This will do nothing if symbol already has a default binding.
;; Otherwise, if symbol has a `saved-value' property, it will evaluate
;; the car of that and use it as the default binding for symbol.
;; Otherwise, VALUE will be evaluated and used as the default binding for
;; symbol."
;;   (unless (default-boundp symbol)
;;     ;; Use the saved value if it exists, otherwise the standard setting.
;;     (set-default symbol (if (get symbol 'saved-value)
;; 			    (eval (car (get symbol 'saved-value)))
;; 			  (eval value)))))

;;; scrat.el ends here

