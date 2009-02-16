;;; col-highlight.el --- Highlight the current column.
;; 
;; Filename: col-highlight.el
;; Description: Highlight the current column.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006, Drew Adams, all rights reserved.
;; Created: Fri Sep 08 11:06:35 2006
;; Version: 22.0
;; Last-Updated: Fri Sep 08 18:36:07 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 136
;; URL: http://www.emacswiki.org/cgi-bin/wiki/col-highlight.el
;; Keywords: faces, frames, emulation, highlight, cursor, accessibility
;; Compatibility: GNU Emacs 21.x GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `column-marker'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;; 
;;  This library provides a minor mode, `column-highlight-mode', to
;;  continually highlight the current column in all buffers (until you
;;  use this command again to toggle highlighting off).  When you move
;;  the cursor, the highlighting follows (tracks the cursor).
;;
;;  It also provides a command, `toggle-highlight-column-when-idle',
;;  to turn on/off highlighting of the current column whenever Emacs
;;  is idle.  This is an alternative to`column-highlight-mode' that
;;  can help you locate the cursor without the distraction of
;;  continual highlighting.  Use command `col-highlight-set-interval'
;;  to change the number of idle seconds to wait before highlighting.
;;
;;  It also provides a command, `flash-column-highlight', that
;;  highlights the current column only on demand, for just a second or
;;  two (`col-highlight-period').  This is another alternative that
;;  helps you locate the cursor without too much distraction.
;;
;;  Note that you also bind `column-highlight-mode' to a key, instead
;;  of `flash-column-highlight', and simply hit it twice to
;;  effectively flash-highlight.
;; 
;;  To use this file, you must also have library `column-marker.el'.
;;  Put this in your Emacs init file (~/.emacs):
;;
;;    ;; Load this file (it will load `column-marker.el').
;;    (require 'col-highlight)
;;
;;  If you want to turn on continual current-column highlighting, by
;;  default, then add this to your init file:
;;
;;    (column-highlight-mode 1)
;;
;;  If you want to turn on automatic idle highlighting of the current
;;  column, then add this to your init file:
;;
;;    (toggle-highlight-column-when-idle 1)
;;
;;  If you want to use a different wait interval, before idle
;;  highlighting begins, then set it in your init file using
;;  `col-highlight-set-interval':
;;
;;    (col-highlight-set-interval 6) ; Wait 6 idle secs.
;;
;;
;;  See also:
;;
;;  * Library `hl-line+.el', which offers the same functionality, but
;;    for the current line instead of the current column.
;;
;;  * Library `crosshairs.el', which combines the features of
;;    `col-highlight.el' and `hl-line+.el', providing a crosshair
;;    highlighting effect.  It requires `col-highlight.el' and
;;    `hl-line+.el'.
;;
;;  * Library `cursor-chg.el' or library `oneonone.el', to change the
;;    cursor type when Emacs is idle.
;;
;;  User options defined here:
;;
;;    `col-highlight-period', `column-highlight-mode'.
;;
;;  Faces defined here:
;;
;;    `col-highlight-face'.
;;
;;  Commands defined here:
;;
;;    `col-highlight-flash', `col-highlight-set-interval',
;;    `col-highlight-toggle-when-idle', `column-highlight',
;;    `column-highlight-mode', `flash-column-highlight',
;;    `highlight-column', `toggle-highlight-column-when-idle'.
;;
;;  Non-interactive functions defined here:
;;
;;    `col-highlight-highlight', `col-highlight-unhighlight'.
;;
;;  Internal variables defined here:
;;
;;    `col-highlight-face', `col-highlight-idle-interval',
;;    `col-highlight-idle-timer', `col-highlight-when-idle-p',
;;    `column-highlight'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2006/09/08 dadams
;;     Created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'column-marker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom col-highlight-period 1
  "Number of seconds to highlight the current column."
  :type 'integer :group 'cursor :group 'hl-line)

(unless (fboundp 'define-minor-mode)
  (defcustom column-highlight-mode nil
    "*Toggle highlighting the current column.
Setting this variable directly does not take effect;
use either \\[customize] or command `column-highlight-mode'."
    :set (lambda (symbol value) (column-highlight-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'cursor :group 'hl-line :require 'column-marker))

(defconst col-highlight-face 'col-highlight-face
    "Face used for temporarily highlighting current column.
Do NOT change this.")

(defface col-highlight-face '((t (:background "SlateGray3")))
  "Face used for temporarily highlighting current column.
Usually a background color."
  :group 'faces)

(defvar col-highlight-idle-interval 5
  "Number of seconds to wait before highlighting current column.
Do NOT change this yourself to change the wait period; instead, use
`\\[col-highlight-set-interval]'.")

(defvar col-highlight-when-idle-p nil
  "Non-nil means highlight the current column whenever Emacs is idle.
Do NOT change this yourself; instead, use
`\\[toggle-highlight-column-when-idle]'.")

(defvar col-highlight-idle-timer
  (progn                                ; Cancel to prevent duplication.
    (when (boundp 'col-highlight-idle-timer) (cancel-timer col-highlight-idle-timer))
    (run-with-idle-timer col-highlight-idle-interval t 'col-highlight-highlight))
  "Timer used to highlight current column whenever Emacs is idle.")

;; Turn it off, by default.
;; You must use `toggle-highlight-column-when-idle' to turn it on.
(cancel-timer col-highlight-idle-timer)

;; Create the column marker.  This defines command and variable `column-highlight'.
(column-marker-create column-highlight col-highlight-face)
(defalias 'highlight-column 'column-highlight)

(if (fboundp 'define-minor-mode)
    ;; Emacs 21 and later.
    (define-minor-mode column-highlight-mode
  "Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'."
      :init-value nil :global t :group 'cursor :group 'hl-line :group 'frames
      :link `(url-link :tag "Send Bug Report"
              ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
col-highlight.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
      :link '(url-link :tag "Other Libraries by Drew"
              "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
      :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/col-highlight.el")
      :link '(url-link :tag "Description"
              "http://www.emacswiki.org/cgi-bin/wiki/ChangingCursorDynamically")
      :link '(emacs-commentary-link :tag "Commentary" "col-highlight")
  (cond (column-highlight-mode
         (add-hook 'pre-command-hook #'col-highlight-unhighlight)
         (add-hook 'post-command-hook #'col-highlight-highlight))
        (t
         (col-highlight-unhighlight)
         (remove-hook 'pre-command-hook #'col-highlight-unhighlight)
         (remove-hook 'post-command-hook #'col-highlight-highlight))))
    ;; Emacs 20
  (defun column-highlight-mode (&optional arg)
    "Toggle highlighting the current column.
With ARG, turn column highlighting on if and only if ARG is positive.

Column-Highlight mode uses the functions
`col-highlight-unhighlight' and `col-highlight-highlight'
on `pre-command-hook' and `post-command-hook'."
    (interactive "P")
    (setq column-highlight-mode
          (if arg (> (prefix-numeric-value arg) 0) (not column-highlight-mode)))
    (cond (column-highlight-mode
         (add-hook 'pre-command-hook #'col-highlight-unhighlight)
         (add-hook 'post-command-hook #'col-highlight-highlight))
        (t
         (col-highlight-unhighlight)
         (remove-hook 'pre-command-hook #'col-highlight-unhighlight)
         (remove-hook 'post-command-hook #'col-highlight-highlight)))))

(defalias 'toggle-highlight-column-when-idle 'col-highlight-toggle-when-idle)
(defun col-highlight-toggle-when-idle (&optional arg)
"Turn on or off highlighting the current column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq col-highlight-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not col-highlight-when-idle-p)))
  (cond (col-highlight-when-idle-p
         (timer-activate-when-idle col-highlight-idle-timer)
         (add-hook 'pre-command-hook 'col-highlight-unhighlight)
         (message "Turned ON highlighting current column when Emacs is idle."))
        (t
         (cancel-timer col-highlight-idle-timer)
         (remove-hook 'pre-command-hook 'col-highlight-unhighlight)
         (message "Turned OFF highlighting current column when Emacs is idle."))))

(defun col-highlight-set-interval (secs)
  "Set wait until highlight current column when Emacs is idle.
Whenever Emacs is idle for this many seconds, the current column
will be highlighted in face `col-highlight-face'.

To turn on or off automatically highlighting the current column
when Emacs is idle, use `\\[toggle-highlight-column-when-idle]."
  (interactive
   "nSeconds to idle, before highlighting current column: ")
  (timer-set-idle-time col-highlight-idle-timer (setq col-highlight-idle-interval secs) t))



(defalias 'flash-column-highlight 'col-highlight-flash)
(defun col-highlight-flash (&optional arg)
    "Highlight the current column for `col-highlight-period' seconds.
With a prefix argument, highlight for that many seconds."
    (interactive)
    (column-highlight-mode 1)
    (let ((column-period col-highlight-period))
      (when current-prefix-arg
        (setq column-period (prefix-numeric-value current-prefix-arg)))
      (run-at-time column-period nil #'column-highlight-mode -1)))

(defun col-highlight-highlight ()
  "Highlight current column."
  (column-highlight nil))

(defun col-highlight-unhighlight ()
  "Turn off highlighting of current column."
  (column-highlight '(4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'col-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; col-highlight.el ends here
