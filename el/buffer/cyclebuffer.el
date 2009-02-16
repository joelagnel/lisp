;;; cyclebuffer.el --- select buffer by cycling through

;;; Commentary:

;; Description:
;; ------------
;; Cyclebuffer is yet another way of selecting buffers.  Instead of
;; prompting you for a buffer name, cyclebuffer-forward switches to the
;; most recently used buffer, and repeated invocations of
;; cyclebuffer-forward switch to less recently visited buffers.  If you
;; accidentally overshoot, calling cyclebuffer-backward goes back.  
;;
;; I find this to be the fastest buffer-switching mechanism; it`s like C-x
;; b <return> w/out the return, but it`s not limited to the most recently
;; accessed buffer.  Plus you never have to remember buffer names; you
;; just keep cycling until you recognize the buffer you`re searching for.

;; Installation:
;; -------------
;;   Add these lines in your .emacs:
;;     (autoload `cyclebuffer-forward "cyclebuffer" "cycle forward" t)
;;     (autoload `cyclebuffer-backward "cyclebuffer" "cycle backward" t)
;;     (global-set-key "M-N" `cyclebuffer-forward)
;;     (global-set-key "M-P" `cyclebuffer-backward)
;;
;;   You may want to adjust the keyboard bindings to avoid conflicts with
;;   whatever other packages you`re using...

;;
;; Change History:
;;	18 Feb 98 v1.2 Bug fixes, code simplification.
;;
;; Thanks to Henry Harpending for suggestions.
;;
;; Author: Kurt Partridge <kepart@cs.washington.edu>
;; Maintainer: Kurt Partridge <kepart@cs.washington.edu>
;; Created: 05 June 1996
;; Version: $Revision: 1.1.1.1 $
;; Keywords: switch-to-buffer

;; LCD Archive Entry:
;; cyclebuffer|Kurt Partridge|kepart@cs.washington.edu|
;; Select buffers by cycling.|
;; 18-Feb-98|Version 1.2|

;; Copyright (C) 1996 Kurt Partridge

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

(defvar cyclebuffer-buffer-list nil
  "List of all buffers; updated every time a new set of cyclebuffer
commands are started.")

(defvar cyclebuffer-buffer-index nil
  "Number indicating the index of the buffer in
cyclebuffer-buffer-list that is currently being displayed.")

(defun cyclebuffer-forward (&optional direction)
  "Like switch-to-buffer, but doesn`t prompt.  Repetitive invocations of
this function select progressively less recently visited buffers."
  (interactive "P")
  ;; If starting a new search, a) make sure the current buffer is at top
  ;; of the list of buffers, and b) set flag to generate a new list
  (if (not (or (eq `cyclebuffer-forward last-command)
	       (eq `cyclebuffer-backward last-command)))
      (progn
	(setq cyclebuffer-buffer-index nil)
	(switch-to-buffer (current-buffer))))
  ;; Generate new list if necessary
  (if (not (numberp cyclebuffer-buffer-index))
      (progn
	(setq cyclebuffer-buffer-list (buffer-list))
	(setq cyclebuffer-buffer-index 0)))
  ;; Cycle through buffers, skipping any invisible buffers (whose
  ;; names start with a blank space)
  (let ((start-buffer (current-buffer))
	(chosen-buffer (current-buffer)))
    (while (or (eq chosen-buffer start-buffer)
	       (char-equal ?  (string-to-char (buffer-name chosen-buffer))))
      (setq start-buffer nil)
      (if (or (null direction) (eq direction 1))
	  (setq cyclebuffer-buffer-index (+ cyclebuffer-buffer-index 1))
	(setq cyclebuffer-buffer-index (- cyclebuffer-buffer-index 1)))
      (setq cyclebuffer-buffer-index 
	    (mod cyclebuffer-buffer-index (length cyclebuffer-buffer-list)))
      (setq chosen-buffer (nth cyclebuffer-buffer-index
cyclebuffer-buffer-list)))
    (switch-to-buffer chosen-buffer)))

(defun cyclebuffer-backward ()
  "Like cyclebuffer-forward, but selects progressively more recently
visited buffers."
  (interactive)
  (cyclebuffer-forward -1))

(provide 'cyclebuffer)
