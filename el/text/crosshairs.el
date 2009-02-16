;;; crosshairs.el --- Highlight the current line and column.
;; 
;; Filename: crosshairs.el
;; Description: Highlight the current line and column.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006, Drew Adams, all rights reserved.
;; Created: Fri Sep 08 13:09:19 2006
;; Version: 22.0
;; Last-Updated: Fri Sep 08 18:37:45 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 77
;; URL: http://www.emacswiki.org/cgi-bin/wiki/crosshairs.el
;; Keywords: faces, frames, emulation, highlight, cursor, accessibility
;; Compatibility: GNU Emacs 21.x GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `col-highlight', `column-marker', `hl-line', `hl-line+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This library combines the features of libraries `col-highlight.el'
;;  and `hl-line+.el' - see those libraries for more information.
;;  This is intended as simply a convenient way to highlight both the
;;  current line and the current column at the same time.
;;
;;  Note that there are two, alternative handy ways to use a key to
;;  temporarily highlight the current line and column:
;;
;;  * Bind `crosshairs-flash'.  Use it once to flash the cross hairs.
;;  * Bind `crosshairs-mode'.   Use it twice in succession to flash.
;; 
;;
;;  See also:
;;
;;  * Library `hl-line+.el', which highlights the current line.
;;
;;  * Library `col-highlight.el', which highlights the current column.
;;
;;  * Library `cursor-chg.el' or library `oneonone.el', to change the
;;    cursor type when Emacs is idle.
;;
;;
;;  User options defined here:
;;
;;    `crosshairs-mode'.
;;
;;  Commands defined here:
;;
;;    `crosshairs-flash', `crosshairs-mode',
;;    `crosshairs-toggle-when-idle', `flash-crosshairs',
;;    `toggle-crosshairs-when-idle'.
;;
;;  Internal variables defined here:
;;
;;    `crosshairs-highlight-when-idle-p'.
;;
;;  Suggested alternative key bindings:
;;
;;    (global-set-key [(control ?+)] 'crosshairs-flash)
;;
;;  or
;;
;;    (global-set-key [(control ?+)] 'crosshairs-mode)
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

(require 'hl-line+) ; Requires `hl-line.el'.
(require 'col-highlight) ; Requires `column-marker.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (fboundp 'define-minor-mode)
  (defcustom crosshairs-mode nil
    "*Toggle highlighting the current line and column.
Setting this variable directly does not take effect;
use either \\[customize] or command `crosshairs-mode'."
    :set (lambda (symbol value) (crosshairs-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'cursor :group 'hl-line :require 'col-highlight :require 'hl-line+))

(defvar crosshairs-highlight-when-idle-p nil
  "Non-nil means highlight current line and column when Emacs is idle.
Do NOT change this yourself; instead, use
`\\[toggle-crosshairs-when-idle]'.")

(if (fboundp 'define-minor-mode)

    ;; Emacs 21 and later.
    (define-minor-mode crosshairs-mode
        "Toggle highlighting the current line and column.
With ARG, turn highlighting on if and only if ARG is positive."
      :init-value nil :global t :group 'cursor :group 'hl-line :group 'frames
      :link `(url-link :tag "Send Bug Report"
              ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
crosshairs.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
      :link '(url-link :tag "Other Libraries by Drew"
              "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
      :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/crosshairs.el")
      :link '(url-link :tag "Description"
              "http://www.emacswiki.org/cgi-bin/wiki/ChangingCursorDynamically")
      :link '(emacs-commentary-link :tag "Commentary" "crosshairs")
      (cond (crosshairs-mode
             (unless global-hl-line-mode
               (global-hl-line-mode 1)
               (global-hl-line-highlight))
             (column-highlight-mode 1))
            (t
             (global-hl-line-mode -1)
             (global-hl-line-unhighlight)
             (column-highlight-mode -1))))

  ;; Emacs 20
  (defun crosshairs-mode (&optional arg)
    "Toggle highlighting the current line and column.
With ARG, turn highlighting on if and only if ARG is positive."
    (interactive "P")
    (setq crosshairs-mode
          (if arg (> (prefix-numeric-value arg) 0) (not crosshairs-mode)))
    (cond (crosshairs-mode
             (unless global-hl-line-mode
               (global-hl-line-mode 1)
               (global-hl-line-highlight))
             (column-highlight-mode 1))
            (t
             (global-hl-line-mode -1)
             (global-hl-line-unhighlight)
             (column-highlight-mode -1)))))

(defalias 'toggle-crosshairs-when-idle 'crosshairs-toggle-when-idle)
(defun crosshairs-toggle-when-idle (&optional arg)
  "Toggle highlighting the current line and column when Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
    (setq col-highlight-when-idle-p
          (if arg (> (prefix-numeric-value arg) 0) (not col-highlight-when-idle-p)))
    (setq hl-line-when-idle-p col-highlight-when-idle-p)
    (cond (col-highlight-when-idle-p
           (timer-activate-when-idle col-highlight-idle-timer)
           (timer-activate-when-idle hl-line-idle-timer)
           (add-hook 'pre-command-hook 'col-highlight-unhighlight)
           (add-hook 'pre-command-hook 'hl-line-unhighlight-now)
           (message "Turned ON highlighting line and column when Emacs is idle."))
          (t
           (cancel-timer col-highlight-idle-timer)
           (cancel-timer hl-line-idle-timer)
           (remove-hook 'pre-command-hook 'col-highlight-unhighlight)
           (remove-hook 'pre-command-hook 'hl-line-unhighlight-now)
           (message "Turned OFF highlighting line and column when Emacs is idle."))))

(defalias 'flash-crosshairs 'crosshairs-flash)
(defun crosshairs-flash (&optional arg)
  "Highlight the current line and column temporarily.
Highlight the line for `line-show-period' and the column for
`column-show-period' seconds.  With a prefix argument, highlight
both for that many seconds."
  (interactive "P")
  (hl-line-highlight-now)
  (column-highlight-mode 1)
  (let ((line-period line-show-period)
        (column-period col-highlight-period))
    (when current-prefix-arg
      (setq line-period (prefix-numeric-value current-prefix-arg)
            column-period line-period))
    (run-at-time line-period nil #'hl-line-unhighlight-now)
    (run-at-time column-period nil #'column-highlight-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'crosshairs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; crosshairs.el ends here
