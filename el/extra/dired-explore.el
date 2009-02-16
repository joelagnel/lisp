;;; dired-explore.el --- provide Windows-like file exploration

;; Copyright (C) 2001 Francis J. Wright

;; Author: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Maintainer: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Time-stamp: <26 July 2001>
;; URL: http://centaur.maths.qmw.ac.uk/Emacs/
;; Keywords: dired, find file

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package, which is intended for use with GNU Emacs 20/21,
;; provides alternative dired-mode functions to those bound by
;; default to `return' and `mouse-2'.  The alternatives are bound to
;; `meta-return' and `meta-mouse-2'.

;; Ian Swainson suggested this functionality, based on
;; Microsoft Windows Explorer (hence the function names).
;; FJW stole most of the code from `dired.el'.

;;; Installation:

;; Put this file somewhere where Emacs can find it (i.e. in one of the
;; directories in your `load-path' such as `site-lisp'), optionally
;; byte-compile it (recommended), and put this in your .emacs:
;;
;; (add-hook 'dired-load-hook
;;           (lambda () (require 'dired-explore)))

;; To change some existing dired-mode key binding, e.g. `return' and
;; `mouse-2', put something like this in your .emacs file instead of
;; the `add-hook' above:

;; (add-hook 'dired-load-hook
;;           (lambda () (require 'dired-explore)
;;                      (define-key dired-mode-map [return] 'dired-explore-file)
;;                      (define-key dired-mode-map [mouse-2] 'dired-mouse-explore-file)))


;;; History:

;; An earlier version of this code was in `dired-sort-menu', where it
;; clearly did not belong!

;;; Code:

(require 'dired)

(defun dired-explore-file ()
  "In dired, visit *in full window* the file or directory named on this line.
If a directory, open it in the *current* buffer.
Alternative to `dired-advertised-find-file', bound to \\[dired-advertised-find-file]."
  (interactive)
  (dired-explore (dired-get-filename)))

(define-key dired-mode-map [(meta return)] 'dired-explore-file)

(defun dired-mouse-explore-file (event)
  "In dired, visit *in full window* the file or directory clicked on.
If a directory, open it in the *current* buffer.
Argument EVENT is what invoked this function.
Alternative to `dired-mouse-find-file-other-window', bound to \\[dired-mouse-find-file-other-window]."
  (interactive "e")
  (dired-explore
   (save-excursion
     (set-buffer (window-buffer (posn-window (event-end event))))
     (goto-char (posn-point (event-end event)))
     (dired-get-filename))))

(define-key dired-mode-map [M-mouse-2] 'dired-mouse-explore-file)

(defun dired-explore (file)
  "In dired, visit FILE *in full window*.
If a directory, open it in the *current* buffer."
  (setq file (file-name-sans-versions file t))
  (if (file-exists-p file)
      (if (file-directory-p file)
	  (find-alternate-file file)
	(find-file file))
    (if (file-symlink-p file)
	(error "File is a symlink to a nonexistent target")
      (error "File no longer exists; type `g' to update Dired buffer"))))

(provide 'dired-explore)

;;; dired-explore.el ends here
