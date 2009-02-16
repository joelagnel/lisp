;;; dired-sort-map.el --- in Dired: press s then s, x, t or n to sort by Size, eXtension, Time or Name

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Patrick Anderson 
;; Version: 1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; inspired by Francis J. Wright's dired-sort-menu.el

;install:
;this file in your load path

;add
; (require 'dired-sort-map)
;to your .emacs file

(defvar dired-sort-map (make-sparse-keymap))

(add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map "s" dired-sort-map)))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "s" '(lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "x" '(lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "t" '(lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-sort-map "n" '(lambda () "sort by Name" (interactive) (dired-sort-other (concat dired-listing-switches ""))))))

(provide 'dired-sort-map)

;;; dired-sort-map.el ends here
