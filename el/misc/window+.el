;;; window+.el --- Extensions to `window.el'.
;;
;; Filename: window+.el
;; Description: Extensions to `window.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Thu Jan 25 14:22:13 1996
;; Version: 21.0
;; Last-Updated: Fri Jan 13 15:17:22 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 78
;; URL: http://www.emacswiki.org/cgi-bin/wiki/window+.el
;; Keywords: internal, window
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `window.el'.
;;
;;
;;  ***** NOTE: The following function defined in `window.el' has been
;;              REDEFINED HERE:
;;
;;  `count-windows' -
;;     Only use arg MINIBUF if current frame has a minibuffer.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `window.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "window" '(require 'window+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 1996/01/25 dadams
;;     count-windows: Returned to original meaning of arg,
;;                    but only use arg ifframe has a minibuffer.
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

 ;; Cannot do (require 'window), because `window.el' does no `provide'.
 ;; Don't want to do a (load-library "window") either, because it wouldn't
 ;; allow doing (eval-after-load "window" '(progn (require 'window+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `window.el':
;; 1. Only use arg MINIBUF if current frame has a minibuffer.
;; 2. Corrected doc string:
;;    Original doc string indicated opposite behavior for arg MINIBUF.
(defun count-windows (&optional minibuf)
  "Return the number of visible windows in selected frame.
Optional arg MINIBUF is only used if selected frame has a minibuffer.

MINIBUF = t means count the minibuffer window even if *not* active.
MINIBUF = nil or omitted means count the minibuffer iff it is active.
If MINIBUF is neither t nor nil it means not to count the minibuffer
even if it is active.  (See function `walk-windows'.)"
  (let ((count 0))
    (walk-windows (function (lambda (w) (setq count (+ count 1))))
                  (and (memq (cdr (assoc 'minibuffer (frame-parameters)))
                             '(only t)) ; If this frame has a minibuffer,
                       minibuf))        ; pass the arg.  (Else pass nil.)
    count))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'window+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window+.el ends here
