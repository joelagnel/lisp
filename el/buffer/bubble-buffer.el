;; Copyright (C) 2002-2004 by Stefan Reichoer

;; Emacs Lisp Archive Entry
;; Filename: bubble-buffer.el
;; Author: Stefan Reichoer, <xsteve@nit.at>
;; Version: 0.2

;; $Id: bubble-buffer.el 108 2004-06-18 09:41:11Z reichr $

;; bubble-buffer.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; bubble-buffer.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; bubble-buffer.el is tested with GNU Emacs 21.3

;; bubble-buffer is just another buffer switching utility for emacs
;; you can bubble up a buffer from the buffer list without destroying
;; the order the buffers in the buffer list. This way the top elements
;; in the buffer list are the most recently used buffers
;; I use it to switch between the last recently used buffers
;; Try it out and see if you like it

;; bubble-buffer provides two commands to select a buffer:
;; - bubble-buffer-next:
;;   When first invoked select the next buffer on the buffer list
;;   When invoked again: select the second buffer on the buffer list,
;;   but leave the buffer list as it is
;;   That means you can bubble up a buffer to the front of the
;;   buffer list, but the buffer-list remains unchanged
;;
;; - bubble-buffer-previous:
;;   Undo the action of the last bubble-buffer-next
;;
;; You can switch forward and backward with bubble-buffer-next and
;; bubble-buffer-previous

;; To use bubble-buffer.el put the following lines in your .emacs:

;; (require 'bubble-buffer)
;; (global-set-key [f11] 'bubble-buffer-next)
;; (global-set-key [(shift f11)] 'bubble-buffer-previous)

;; The latest version of bubble-buffer.el can be found at:
;;   http://xsteve.nit.at/prg/emacs/bubble-buffer.el


;; Comments / suggestions welcome!

;;; Code:

(defvar bubble-buffer-max-display-length (- (frame-width) 5)
  "Maximum number of characters to display in the minibuffer when bubbling.
")

(defvar bubble-buffer-omit-regexp "\\(^ .+$\\|\\*Messages\\*\\)"
  "Regexp for buffer-names that should be skipped when bubbling buffers with
bubble-buffer-next and bubble-buffer-previous.
For example you could use \"\\\\*.+\\\\*\" to exclude all buffers that contain two *'s.
")
;"\\*.+\\*"

(defun bubble-buffer-omit-buffer (buffer)
  "return nil if the buffer should be omitted, otherwise the buffer name"
  (let ((buf-name (buffer-name buffer)))
    (unless (and bubble-buffer-omit-regexp (string-match bubble-buffer-omit-regexp buf-name))
        buf-name)))


(defun bubble-buffer-next()
  "Bubble down one entry in the buffer list.
   Switch to the next buffer on the list"
  (interactive)
  (if (not (eq last-command 'bubble-buffer-next))
      (progn (setq bubble-buffer-list (copy-alist (buffer-list)))
             (delq (get-buffer " *Minibuf-0*") bubble-buffer-list)
             (delq (get-buffer " *Minibuf-1*") bubble-buffer-list)
             (setq bubble-buffer-buried-list nil)))
  (let* ((cur-buf (current-buffer))
         (b-list (delq nil (mapcar 'bubble-buffer-omit-buffer (cdr bubble-buffer-list))))
         (doit b-list)
         (rest nil)
         (s))
    (while doit
      (add-to-list 'bubble-buffer-buried-list (car bubble-buffer-list))
      (bury-buffer (car bubble-buffer-list))
      (setq bubble-buffer-list (cdr bubble-buffer-list))
      (switch-to-buffer (car bubble-buffer-list))
      (setq rest (cdr (copy-alist bubble-buffer-list)))
      (while rest
        (bury-buffer (car rest))
        (setq rest (cdr rest)))
      (setq doit (not (bubble-buffer-omit-buffer (current-buffer)))))
    ;;(message "%S" bubble-buffer-list)
    (if b-list
        (progn
          (setq b-list (cdr b-list))
          (setq s (concat
                   "Next: "
                   (if b-list (format "%S" b-list "") "")
                   "[end-of-bubble-list]"))
          (message "%s" (concat
                    (substring s 0 (min bubble-buffer-max-display-length (length s)))
                    " ...")))
      (message "Already at the end of the buffer-list"))))

(defun bubble-buffer-previous()
  "Undo one bubbling step from bubble-buffer-next.
   Switch to the buffer before the bubbled up buffer in the buffer list"
  (interactive)
  (unless (eq last-command 'bubble-buffer-next)
    (setq bubble-buffer-buried-list nil))
  (setq this-command 'bubble-buffer-next)
  (if bubble-buffer-buried-list
      (progn
        (let ((doit t)
              (s)
              (b-list))
          (while doit
            (add-to-list 'bubble-buffer-list (car bubble-buffer-buried-list))
            (switch-to-buffer (car bubble-buffer-buried-list))
            (setq bubble-buffer-buried-list (cdr bubble-buffer-buried-list))
            (setq doit (not (bubble-buffer-omit-buffer (current-buffer))))))
        (setq b-list (delq nil (mapcar 'bubble-buffer-omit-buffer bubble-buffer-buried-list)))
        (setq s (concat
                 "Previous: "
                 (if b-list (format "%S" b-list "") "")
                 "[beginning-of-bubble-list]"))
        (message "%s" (concat
                  (substring s 0 (min bubble-buffer-max-display-length (length s))) " ...")))
    (message "Already at the start of the bubble-buffer-list")))


(provide 'bubble-buffer)

; arch-tag: de0e5b2b-a5a2-4af1-97e1-be8667a0aa55
