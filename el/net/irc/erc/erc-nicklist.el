;;; erc-nicklist.el --- Display channel nicknames in a side buffer.

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Filename: erc-nicklist.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-04-30
;; Keywords: IRC chat client Internet

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; 
;; This provides a minimal mIRC style nicklist buffer for ERC.  To
;; activate, do M-x erc-nicklist RET in the channel buffer you want
;; the nicklist to appear for.  To close and quit the nicklist
;; buffer, do M-x erc-nicklist-quit RET.
;;
;; TODO:
;; o Somehow associate nicklist windows with channel windows so they
;;   appear together, and if one gets buried, then the other does.
;;
;; o Make "Query" and "Message" work.
;;
;; o Prettify the actual list of nicks in some way.
;;
;; o Add a proper erc-module that people can turn on and off, figure
;;   out a way of creating the nicklist window at an appropriate time
;;   --- probably in `erc-join-hook'.
;;
;; o Ensure XEmacs compatibility --- the mouse-menu support is likely
;;   broken.
;;
;; o Add option to display in a separate frame --- will again need to
;;   be able to associate the nicklist with the currently active
;;   channel buffer or something similar.
;;
;; o Allow toggling of visibility of nicklist via ERC commands.

;;; History:
;; 

;;; Code:

(require 'erc)

(defconst erc-nicklist-version "$Revision: 1.3.2.1 $"
  "ERC Nicklist version.")

(defgroup erc-nicklist nil
  "Display a list of nicknames in a separate window."
  :group 'erc)

(defcustom erc-nicklist-window-size 20.0
  "*The size of the nicklist window.

This specifies a percentage of the channel window width.

A negative value means the nicklist window appears on the left of the
channel window, and vice versa."
  :group 'erc-nicklist
  :type 'float)

(defun erc-nicklist-buffer-name (&optional buffer)
  "Return the buffer name for a nicklist associated with BUFFER.

If BUFFER is nil, use the value of `current-buffer'."
  (format " *%s-nicklist*" (buffer-name (or buffer (current-buffer)))))

(defun erc-nicklist-make-window ()
  "Create an ERC nicklist window.

See also `erc-nicklist-window-size'."
  (let ((width (floor (* (window-width) (/ erc-nicklist-window-size 100.0))))
        (buffer (erc-nicklist-buffer-name))
        window)
    (split-window-horizontally (- width))
    (setq window (next-window))
    (set-window-buffer window (get-buffer-create buffer))
    (with-current-buffer buffer
      (set-window-dedicated-p window t))))

(defun erc-nicklist ()
  "Create an ERC nicklist buffer."
  (interactive)
  (let ((channel (current-buffer)))
    (erc-nicklist-make-window)
    (with-current-buffer (get-buffer (erc-nicklist-buffer-name channel))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (n)
              (insert (propertize n 'erc-nicklist-nick n 'mouse-face 'region
                                  'erc-nicklist-channel channel
                                  'help-echo "Mouse-3 for menu") "\n"))
            (erc-nicklist-channel-nicks channel))
      (erc-nicklist-mode)))
  (add-hook 'erc-channel-members-changed-hook #'erc-nicklist-update))

(defun erc-nicklist-update ()
  "Update the ERC nicklist buffer."
  (let ((b (get-buffer (erc-nicklist-buffer-name)))
        (channel (current-buffer)))
    (when b
      (with-current-buffer b
        (setq buffer-read-only nil)
        (erase-buffer)
        (mapc (lambda (n)
                (insert (propertize n 'erc-nicklist-nick n 'mouse-face 'region
                                    'erc-nicklist-channel channel
                                    'help-echo "Mouse-3 for menu") "\n"))
              (erc-nicklist-channel-nicks channel))
        (erc-nicklist-mode)))))

(defvar erc-nicklist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-3>") 'erc-nicklist-menu)
    (define-key map "q" 'erc-nicklist-quit)
    map)
  "Keymap for `erc-nicklist-mode'.")

(define-derived-mode erc-nicklist-mode fundamental-mode
  "Nicklist"
  "Major mode for the ERC nicklist buffer."
  (setq buffer-read-only t))

(defun erc-nicklist-call-erc-command (command point buffer window)
  "Call an ERC COMMAND.

Depending on what COMMAND is, it's called with one of POINT, BUFFER,
or WINDOW as arguments."
  (let* ((p (text-properties-at point))
         (b (plist-get p 'erc-nicklist-channel)))
    (if (memq command '(erc-nicklist-quit ignore))
        (funcall command window)
      ;; EEEK!  Horrble, but it's the only way we can ensure the
      ;; response goes to the correct buffer.
      (erc-set-active-buffer b)
      (with-current-buffer b
        (funcall command (plist-get p 'erc-nicklist-nick))))))

(defvar erc-nicklist-menu
  (let ((map (make-sparse-keymap "Action")))
    (define-key map [erc-cmd-WHOIS]
      '("Whois" . erc-cmd-WHOIS))
    (define-key map [erc-cmd-DEOP]
      '("Deop" . erc-cmd-DEOP))
    (define-key map [erc-cmd-MSG]
      '("Message" . erc-cmd-MSG))
    (define-key map [erc-cmd-QUERY]
      '("Query" . erc-cmd-QUERY))
    (define-key map [ignore]
      '("Cancel" . ignore))
    (define-key map [erc-nicklist-quit]
      '("Close nicklist" . erc-nicklist-quit))
    map)
  "Menu keymap for the ERC nicklist.")

(defun erc-nicklist-quit (&optional window)
  "Delete the ERC nicklist.

Deletes WINDOW and stops updating the nicklist buffer."
  (interactive)
  (let ((b (window-buffer window)))
    (with-current-buffer b
      (set-buffer-modified-p nil)
      (kill-this-buffer)
      (remove-hook 'erc-channel-members-changed-hook 'erc-nicklist-update))))

(defun erc-nicklist-menu (&optional arg)
  "Show the ERC nicklist menu.

ARG is a parametrized event (see `interactive')."
  (interactive "e")
  (let* ((point (nth 1 (cadr arg)))
         (window (caadr arg))
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      (erc-nicklist-call-erc-command
       (car (x-popup-menu arg
                          erc-nicklist-menu))
       point
       buffer
       window))))
                                   
(defun erc-nicklist-channel-nicks (channel)
  "Return a sorted list of all nicks on CHANNEL."
  (let ((nicks (with-current-buffer channel
                 (erc-get-channel-nickname-list))))
    (sort nicks #'string<)))

(provide 'erc-nicklist)

;;; erc-nicklist.el ends here
