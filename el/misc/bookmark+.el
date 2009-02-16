;;; bookmark+.el --- Extensions to `bookmark.el'.
;;
;; Filename: bookmark+.el
;; Description: Extensions to `bookmark.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2006, Drew Adams, all rights reserved.
;; Created: Fri Sep 15 07:58:41 2000
;; Version: 21.0
;; Last-Updated: Wed Mar 08 14:45:22 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 88
;; URL: http://www.emacswiki.org/cgi-bin/wiki/bookmark+.el
;; Keywords: bookmarks, placeholders, annotations, search
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `bookmark', `pp'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `bookmark.el'.
;;
;; New functions defined here:
;;
;;   `bookmark-jump-other-window', `bookmark-menu-jump-other-window'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2006/03/08 dadams
;;     bookmark-jump-other-window: Handle nil arg.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2004/11/20 dadams
;;     Refined to deal with Emacs 21 < 21.3.50 (soon to be 22.x)
;; 2004/10/26 dadams
;;     Different menu-bar command, depending on Emacs version.
;; 2004/09/21 dadams
;;     Only define bookmark-menu-jump-other-window if < Emacs 21.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'bookmark)


;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-key bookmark-map "o" 'bookmark-jump-other-window)
;;;###autoload
(define-key bookmark-map "q" 'bookmark-jump-other-window)
;;;###autoload
(define-key ctl-x-map "p" bookmark-map)
;;;###autoload
(define-key ctl-x-map "pj" 'bookmark-jump-other-window)


;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
;;;###autoload
(if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
    (define-key-after menu-bar-bookmark-map [jump-other]
      '("Jump to Bookmark (Other Window)" . bookmark-jump-other-window)
      'jump)
  (define-key-after menu-bar-bookmark-map [jump-other]
    '("Jump to Bookmark (Other Window)" . bookmark-menu-jump-other-window)
    'jump))


;;;###autoload
(defun bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump'."
  (interactive
   (let ((bkm (bookmark-completing-read "Jump to bookmark (in another window)"
                                        bookmark-current-bookmark)))
     ;; TEST IS TEMPORARY - will be changed to
     ;; (string-match "22.x" emacs-version) after 22.x release
     (if (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
         (list bkm) bkm)))
  (when bookmark
    (bookmark-maybe-historicize-string bookmark)
    (let ((cell (bookmark-jump-noselect bookmark)))
      (and cell
           (switch-to-buffer-other-window (car cell))
           (goto-char (cdr cell))
           (if bookmark-automatically-show-annotations
               ;; if there is an annotation for this bookmark,
               ;; show it in a buffer.
               (bookmark-show-annotation bookmark))))))


;; Not needed for Emacs 21+.
;; TEST IS TEMPORARY - will be changed to (string-match "22.x" emacs-version) after 22.x release
(unless (or (string-match "22." emacs-version) (string-match "21.3.50" emacs-version))
  (defun bookmark-menu-jump-other-window (event)
    "Jump to BOOKMARK (a point in some file) in another window.
See `bookmark-jump-other-window'."
    (interactive "e")
    (bookmark-popup-menu-and-apply-function
     'bookmark-jump-other-window "Jump to Bookmark (in another window)"
     event)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'bookmark+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmark+.el ends here
