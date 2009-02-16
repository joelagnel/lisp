;;; xterm-title.el --- Update xterm titles

;; Copyright (C) 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Created: 2004-06-21

;; $Id: xterm-title.el,v 1.1 2004/06/22 00:19:06 friedman Exp $

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

;; To use this, put something like the following in your .emacs:

;;     (when (and (not window-system)
;;                (string-match "^xterm" (getenv "TERM")))
;;       (require 'xterm-title)
;;       (xterm-title-mode 1))

;; This package requires the function `format-mode-line', which does not
;; appear in Emacs 21.3 or earlier versions.  In fact, as of 2004-06-21
;; only the Emacs development sources in CVS have this function.

;; Updates of xterm-title.el may be retrieved via
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(require 'xterm-frobs)

(defgroup xterm nil
  "Emacs interaction with xterm windows"
  :group 'xterm
  :group 'extensions)

(defcustom xterm-title-frame-title-format nil
  "Window title string to use for xterm windows.

This variable takes precedence over the usual manner of setting frame
titles in Emacs \(see variables `frame-title-format' and
`mode-line-format'\).  If nil, it is ignored."
  :group 'xterm
  :type '(choice
          (string :tag "Literal text")
          (sexp   :tag "Dynamic title (see variable `mode-line-format')")))

(defcustom xterm-title-icon-title-format nil
  "Window icon label to use for xterm windows.

This variable takes precedence over the usual manner of setting frame icon
titles in Emacs \(see variables `icon-title-format' and
`mode-line-format'\).  If nil, it is ignored."
  :group 'xterm
  :type '(choice
          (string :tag "Literal text")
          (sexp   :tag "Dynamic icon title (see variable `mode-line-format')")))


;; Not user variables; just used to maintain state
(defvar xterm-title-update-last-window nil)
(defvar xterm-title-orig-frame-title   nil)
(defvar xterm-title-orig-icon-title    nil)

(define-minor-mode xterm-title-mode
  "Update xterm window and icon titles with the selected Emacs tty frame.

When this mode is enabled, the original state of the titles are saved.
If the mode is later disabled, or emacs is exited normally, these original
titles will be restored."
  :global t
  :group 'xterm
  :lighter " XTitle"
  :init-value nil
  (cond (xterm-title-mode
         (xterm-title-save-orig-titles)
         (add-hook 'post-command-hook 'xterm-title-update)
         (add-hook 'kill-emacs-hook 'xterm-title-restore-orig-titles))
        (t
         (remove-hook 'kill-emacs-hook 'xterm-title-restore-orig-titles)
         (remove-hook 'post-command-hook 'xterm-title-update)
         (xterm-title-restore-orig-titles))))

(defun xterm-title-update ()
  "Update xterm window and icon titles with the selected Emacs tty frame."
  (unless (and xterm-title-mode
               (eq (selected-window) xterm-title-update-last-window))
    (xterm-set-window-title
     (format-mode-line (or xterm-title-frame-title-format
                           (frame-parameter nil 'title)
                           ;; This would mimic the semantics in X but since
                           ;; frame ttys always have a name (even if just
                           ;; the default "F<n>"), don't bother.
                           ;;
                           ;;(frame-parameter nil 'name)
                           frame-title-format)))
    (xterm-set-icon-title
     (format-mode-line (or xterm-title-icon-title-format
                           (frame-parameter nil 'icon-name)
                           (frame-parameter nil 'title)
                           ;; See above.
                           ;;
                           ;;(frame-parameter nil 'name)

                           ;; Iconified frames won't be running commands,
                           ;; so they are never updated.  Instead, just use
                           ;; the icon-title-format unconditionally if we
                           ;; get this far.
                           ;;
                           ;; Also, calling xterm-report-window-state
                           ;; can interfere with xterm-mouse-mode.
                           ;;
                           ;;(if (eq (xterm-report-window-state) 'iconified)
                           ;;    icon-title-format
                           ;;  (or xterm-title-frame-title-format
                           ;;      frame-title-format)))))
                           icon-title-format)))
    (setq xterm-update-last-window (selected-window))))

(defun xterm-title-save-orig-titles ()
  (condition-case nil
      (progn
        (setq xterm-title-orig-frame-title (xterm-report-window-title)
              xterm-title-orig-icon-title  (xterm-report-icon-title)))
    (error
     (setq xterm-title-orig-frame-title nil
           xterm-title-orig-icon-title  nil))))

(defun xterm-title-restore-orig-titles ()
  (unwind-protect
      (progn
        (when xterm-title-orig-frame-title
          (xterm-set-window-title xterm-title-orig-frame-title))
        (when xterm-title-orig-icon-title
          (xterm-set-icon-title xterm-title-orig-icon-title)))
    (setq xterm-title-orig-frame-title nil
          xterm-title-orig-icon-title  nil)))

(provide 'xterm-title)

;;; xterm-title.el ends here
