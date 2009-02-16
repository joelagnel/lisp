;;; layoutrestore.el
;; --- keep multiwindow layout and restore it simply.

;; Copyleft (C) Vektor <Veldrin@SMTH>

;; Author: Vektor
;; Maintainer: Vektor
;; Version: 0.1
;; Keywords: extensions window layout

;; This file is free software.
;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Sometimes I use multiwindow to do my job, and when i switch to other
;; buffer and go back, the original layout is gone. I have to set it up
;; again.
;; I was very annoyed about this so I tried other packages to help me
;; out. I found `WinnerMode', which did something I didn't want, and 
;; `TaskMode', which is too powerful to be used simply.(just reading its
;; manual made me dizzy) So I wrote this by myself.
;; Actually this is my first emacs extention package and I don't really
;; know if I have done it well. But I think this package is already quite
;; useable.
;; 
;; To start using this package, add following to your emacs startup file.
;; --------------------------------------------------------------------- 
;; (require 'layoutrestore)
;; ;; save layout key
;; (global-set-key [?\C-c ?l] 'layout-save-current)
;; ;; load layout key
;; (global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
;; ;; cancel(delete) layout key
;; (global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)
;; ---------------------------------------------------------------------
;; Change the keybindings to whatever you like.
;;
;; `layout-save-current' save the current window layout so that when next
;; time you switch back to this buffer ,the layout will be brought back
;; automatically. You can also manually use `layout-restore' to restore
;; the layout.
;; When you feel a layout is useless, switch to the buffer where saved it
;; and use `layout-delete-current' to delete this layout.
;;
;; These codes are simple and well documented. you can easily hack it by
;; youself.
;;
;; Any question, or advice, please mail to XYZvektorXYZ@XYZyeahXYZ.net .
;; (remove all XYZ from above email address to get the real one.)


;;; Code:

(require 'advice)


(defvar layout-configuration-alist nil
  "This list contains  window configuration to restore for certain
buffer correlated layout. Each element of this list is a list itself,
it consists of 'active buffer of this layout', 'window-configuration
of this layout', '(buffer . buffer-name) cons of this layout'.")

(defvar layout-accept-buffer-by-name t
  "This variable decide whether we'll accept a different buffer which have
the same name in case we could find the original buffer. Useful when we want
to keep a layout after close one of its buffer and reopen it.")

(defvar layout-verbose t
  "Print verbose message.")
  
(defvar layout-restore-after-switchbuffer t
  "If we should restore layout after `switch-buffer'.")

(defvar layout-restore-after-killbuffer t
  "If we should restore layout after `kill-buffer'.")

(defvar layout-restore-after-otherwindow nil
  "If we should restore layout after `other-window', which normally invoked
by C-x o.")


(defun layout-save-current ()
  "Save the current layout, add a list of current layout to
layout-configuration-alist."
  (interactive)
  (let ((curbuf (current-buffer))
        (curwincfg (current-window-configuration))
        layoutcfg)
    (setq layoutcfg (list curbuf curwincfg))
    (dolist (window (window-list))
      (setq layoutcfg
            (append layoutcfg
                    (list (cons (window-buffer window)
                                (buffer-name (window-buffer window)))))))
    (dolist (locfg layout-configuration-alist)
      (if (eq curbuf (car locfg))
          (setq layout-configuration-alist
                (delq locfg layout-configuration-alist))))
    (setq layout-configuration-alist
          (cons layoutcfg layout-configuration-alist)))
  (if layout-verbose (message "Current layout saved.")))
  

(defun layout-restore (&optional BUFFER)
  "Restore the layout related to the buffer BUFFER, if there is such a layout
saved in `layout-configuration-alist', and update the layout if necessary."
  (interactive)
  (if (not BUFFER) (setq BUFFER (current-buffer)))
  (let (wincfg
        buflist
        buffer-changed-p
        bufname-changed-p
        new-buffer-cons-list
        (restorep t))
    (dolist (locfg layout-configuration-alist)
      (when (eq BUFFER (car locfg))
        (setq wincfg (cadr locfg))
        (setq buflist (cddr locfg))))
    (when wincfg
      (dolist (bufcons buflist)
        (if (buffer-live-p (car bufcons))
            (if (not (string= (buffer-name (car bufcons)) (cdr bufcons)))
                (setq bufname-changed-p t
                      new-buffer-cons-list
                      (append new-buffer-cons-list
                              (list (cons (car bufcons)
                                          (buffer-name (car bufcons))))))
              (setq new-buffer-cons-list (append new-buffer-cons-list (list bufcons))))
          (if (not layout-accept-buffer-by-name)
              (setq buffer-changed-p t
                    restorep nil)
            ;; accept reopen buffer by name, if any
            (progn
              (setq buffer-changed-p t)
              (let ((rebuf (get-buffer (cdr bufcons))))
                (if (not rebuf)
                    (setq restorep nil)
                  (setq new-buffer-cons-list
                        (append new-buffer-cons-list
                                (list (cons rebuf (cdr bufcons)))))))))))
      (when restorep
        (set-window-configuration wincfg)
        (dolist (window (window-list))
          (set-window-buffer window (caar new-buffer-cons-list))
          (setq new-buffer-cons-list (cdr new-buffer-cons-list)))
        (if (or bufname-changed-p buffer-changed-p)
            (layout-save-current))
        (if layout-verbose (message "Previous saved layout restored.")))
      )))

(defun layout-delete-current (&optional BUFFER)
  "Delete the layout information from `layout-configuration-alist'
if there is an element list related to BUFFER."
  (interactive)
  (if (not BUFFER) (setq BUFFER (current-buffer)))
  (dolist (locfg layout-configuration-alist)
    (when (eq BUFFER (car locfg))
      (setq layout-configuration-alist
            (delq locfg layout-configuration-alist))
      (if layout-verbose (message "Layout about this buffer deleted.")))
    ))

(defadvice switch-to-buffer (after layout-restore-after-switch-buffer (BUFFER))
  "Restore possible layout after `switch-to-buffer' funcall."
  (if layout-restore-after-switchbuffer 
      (layout-restore)))

(defadvice kill-buffer (after layout-restore-after-kill-buffer (BUFFER))
  "Restore possible layout after `kill-buffer' funcall."
  (if layout-restore-after-killbuffer
      (layout-restore)))

(defadvice other-window (after layout-restore-after-other-window (ARG))
  "Restore possible layout after `other-window' funcall."
  (if layout-restore-after-otherwindow
      (layout-restore)))

(ad-activate 'switch-to-buffer)
(ad-activate 'kill-buffer)
(ad-activate 'other-window)

(provide 'layoutrestore)

;;; layoutrestore.el ends here.
