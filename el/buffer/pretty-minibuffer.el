;;; minibuffer-face.el --- Give the minibuffer a face

;;; Copyright (c) 2001 by Daniel Lundin <daniel@codefactory.se>
;;; Copyright (c) 2001 CodeFactory AB

;; Author: Daniel Lundin <daniel@codefactory.se>
;; Version: 1.0
;; Created: 2001/1/31 13:08:34
;; Keywords: faces, minibuffer
;;
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
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, patches, suggestions, etc. to 
;; daniel@codefactory.se.
;;

;;; Commentary:
;;
;; pretty-minibuffer.el is a quick hack to give the minibuffer some
;; color/font/properties. 
;; It is a global minor mode that creates an overlay for the
;; minibuffer  and assigns it a face, 'minibuffer-face' by default.
;;
;; Enable it with 'M-x pretty-minibuffer-mode RET' and disable it by
;; giving 0 as argument, 'C-u 0 M-x pretty-minibuffer-mode RET'.
;;
;; Put the following in your ~/.emacs to enable at startup:
;;   (require 'pretty-minibuffer)
;;   (pretty-minibuffer-mode 1)
;;
;; Customize the minibuffer-face to alter the appearance of the
;; minibuffer. 
;; For example: 'M-x customize-face RET minibuffer-face RET'.

;;; Code:


;; Customization

;;;###autoload
(defgroup pretty-minibuffer nil
  "A prettified minibuffer."
  :group 'minibuffer
  :link '(url-link "ftp://ftp.codefactory.se/pub/people/daniel/"))

(defcustom pretty-minibuffer-mode nil
  "Toggle pretty minibuffer mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `pretty-minibuffer-mode'."
  :set (lambda (symbol value)
	 (pretty-minibuffer-mode (or value 0)))
  :initialize 'custom-initialize-default
  :group 'pretty-minibuffer
  :require 'minibuffer
  :version "1.0"
  :type 'boolean)

(defcustom pretty-minibuffer-mode-hook nil
  "Hook run at the end of function `pretty-minibuffer-mode'."
  :group 'iswitchb
  :type 'hook)

(defcustom pretty-minibuffer-face 'minibuffer-face
  "Specify face used in minibuffer"
  :type 'face
  :group 'pretty-minibuffer)

(defcustom pretty-minibuffer-minibuffer-setup-hook nil
  "Pretty-minibuffer-specific customization of minibuffer setup.
This hook is rund uring minibuffer setup if 'pretty-minibuffer-mode'
is active.
Add your customization to this hook."
  :type 'hook
  :group 'pretty-minibuffer)
  

;; Faces

;; Define a face for minibuffer use. I don't know if xemacs defines
;; faces the same way.
(defface minibuffer-face 
  (if (>= emacs-major-version 23)
    '((t (:foreground "red2"
	  :family "helvetica"
	  :height 110)))
  '((t (:foreground "red2"))))
  "Face used in minibuffer")


;; Functions

(defun pretty-minibuffer-setup ()
    (overlay-put (make-overlay (point-min) (point-max) nil t t)
		 'face pretty-minibuffer-face)
    (run-hooks 'pretty-minibuffer-minibuffer-setup-hook))

;;;###autoload
(defun pretty-minibuffer-mode (&optional arg)
  "Toggle pretty minibuffer global minor mode.
With arg, turn the mode on if and only iff ARG is positive.
This mode enables a face to be used for the minibuffer."
  (interactive "P")
  (setq pretty-minibuffer-mode
	(if arg
	    (> (prefix-numeric-value arg) 0)
	  (not pretty-minibuffer-mode)))
  (if pretty-minibuffer-mode
      (add-hook 'minibuffer-setup-hook 'pretty-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook 'pretty-minibuffer-setup))
  (run-hooks 'pretty-minibuffer-mode-hook)
  (if (interactive-p)
      (message "Pretty minibuffer mode %sabled"
	       (if pretty-minibuffer-mode "en" "dis"))))

(unless (assq 'pretty-minibuffer-mode minor-mode-map-alist)
  (push 'pretty-minibuffer-mode
	minor-mode-map-alist))


(provide 'pretty-minibuffer)

;;; pretty-minibuffer.el ends here
