;;; idle-scroll.el --- scroll down a line when Emacs is idle

;; Copyright (C) 2003  Alex Schroeder <alex@gnu.org>
;; Author: Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Run M-x idle-scroll-mode to automatically scroll down.
;; Use M-x set-variable RET idle-scroll-seconds RET 2 RET to make it slower.

(defvar idle-scroll-timer nil
  "Timer for `idle-scroll-mode'.")

(defvar idle-scroll-buffer nil
  "Buffer for `idle-scroll-mode'.")

(defvar idle-scroll-seconds 1
  "*How many seconds to wait before scrolling down one line.")

(make-variable-buffer-local 'idle-scroll-timer)
(make-variable-buffer-local 'idle-scroll-buffer)

(define-minor-mode idle-scroll-mode
  "Scroll down line by line when idle."
  nil " Scrl" nil
  (if idle-scroll-timer
      (progn
        (cancel-timer idle-scroll-timer)
        (setq idle-scroll-timer nil))
    (setq idle-scroll-buffer (current-buffer)
	  idle-scroll-timer (run-at-time t idle-scroll-seconds
                                         'idle-scroll-scroll))))

(defun idle-scroll-scroll ()
  "Scroll if `idle-scroll-mode' is active."
  (when (and idle-scroll-timer (eq (current-buffer) idle-scroll-buffer))
    (condition-case nil
	(scroll-up 1)
      (error (idle-scroll-mode -1)))))

;;; idle-scroll.el ends here
