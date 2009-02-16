;;; Time-stamp: <2005-01-18 21:10:38 jcgs>

;; A difference browser, in the form of a command loop that calls
;; compare-windows repeatedly, letting you make adjustments (moving
;; point in one window or the other) each time it finds a difference.

;; Written by John Sturdy, probably in the late 1980s or maybe early 90s
;; All my own work

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(require 'compare-w)
(provide 'compare-windows-interactively)

(defvar cwi-ignore-whitespace t
  "Whether cwi should ignore whitespace.")

(defmacro in-both-windows (&rest forms)
  "Evaluate FORMS in this window and the next, returning to this one."
  `(progn
     ,@forms
     (other-window 1)
     ,@forms
     (other-window -1)))

(defmacro restart-comparison (action)
  "Restart interactive comparison after doing one of ACTION."
  `(progn
     (in-both-windows (,action 1))
     (compare-windows cwi-ignore-whitespace)))

(defun cwi-top (n)
  "Go to top of buffer, ignoring N."
  (goto-char (point-min)))

(defun cwi-status ()
  "Show the comparison status"
  (message "%sgnoring whitespace; %sgnoring case" (if cwi-ignore-whitespace "I" "Not i") (if compare-ignore-case "I" "Not i")))

;;;###autoload
(defun compare-windows-interactively ()
  "Compare windows interactively, with a simple command loop.
Commands are
  e   move to end of line and resume comparison
  f   move to end of function and resume comparison
  a   move to start of line and resume comparison
  l   recenter both windows
  0   recenter both windows so point is at top of each window
  SPC move cursor to the other window
  n   move to next line in current window
  p   move to previous line in current window
  w   toggle whether whitespace is significant in comparison
  i   toggle with to ignore case in comparison
  r   recursive edit (\\[exit-recursive-edit] to finish)
  <   move to start of buffer and resume comparison
  q   quit

This probably ought to become some sort of strange mode with its own keymap!"
  (interactive)
  (let ((run t)
	(compare-ignore-case compare-ignore-case))
    (compare-windows cwi-ignore-whitespace)
 (cwi-status)
    (while run
      (let ((c (read-char)))
	(case c
	  (?e (restart-comparison end-of-line))
	  (?a (restart-comparison beginning-of-line))
	  (?f (restart-comparison end-of-defun))
	  (?l (in-both-windows (recenter nil)))
	  (?0 (in-both-windows (recenter 0)))
	  (?  (other-window 1))
	  (?n (next-line 1))
	  (?p (previous-line 1))
	  (?w (setq cwi-ignore-whitespace (not cwi-ignore-whitespace))
	      (message "%sgnoring whitespace" (if cwi-ignore-whitespace "I" "Not i")) (sit-for 2))
	  (?c (setq compare-ignore-case (not compare-ignore-case))
	      (message "%sgnoring case" (if compare-ignore-case "I" "Not i")) (sit-for 2))
          (?< (restart-comparison cwi-top))
	  (?r (message (substitute-command-keys "\\[exit-recursive-edit] to resume comparison"))
	      (recursive-edit))
	  (?? (cwi-status))
	  ((?. ?q) (setq run nil)))))))

;;; end of compare-windows-interactively.el
