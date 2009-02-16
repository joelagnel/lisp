;;; intellimouse.el - do something with the scroll wheel

;; Copyright (C) 1998-2002 Sean MacLennan
;; $Revision: 1.4 $ $Date: 2002/11/22 05:34:56 $
;; XEmacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(global-set-key [button4] 'fine-scroll-down)
(global-set-key [button5] 'fine-scroll-up)
(global-set-key [(shift button4)] 'my-scroll-down-command)
(global-set-key [(shift button5)] 'my-scroll-up-command)

(defvar fine-scroll-lines 3 "*Number of lines to fine scroll")

(defun my-scroll-down-command (&optional n)
  (interactive "_P")
  (let ((mouse-window (event-window current-mouse-event)))
    (with-selected-window mouse-window
      (scroll-down-command n))))

(defun my-scroll-up-command (&optional n)
  (interactive "_P")
  (let ((mouse-window (event-window current-mouse-event)))
    (with-selected-window mouse-window
      (scroll-up-command n))))

(defun fine-scroll-down () (interactive) (my-scroll-down-command fine-scroll-lines))

(defun fine-scroll-up () (interactive) (my-scroll-up-command fine-scroll-lines))

(provide 'intellimouse)
