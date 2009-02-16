;;; swiss-move.el --- Swiss Cursor Movement
;; $Id: swiss-move.el,v 1.1.1.1 2004/04/01 15:56:47 ska Exp $
;; Copyright (C) 2004 by Stefan Kamphausen
;; Author: Stefan Kamphausen <http://www.skamphausen.de>
;; Keywords: tools, user
;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; - Moving one line at a time is slow, but precise.
;; - Moving in screenfulls (aka scrolling) ist not fast enough for
;;   large buffers and is not precise.
;; Thus you ususally scroll to another part of your large file and
;; then use the one-line-movement for exact adjustment.
;;
;; This can be improved if subsequent keystrokes would reduce the
;; amount of lines scrolled.  Please see the docstring of
;; `swiss-move-line' for examples that try to explain it.
;; You'd probably best try it out now.
;;
;; Actually the term "Swiss Movement" is introduced here and it stems
;; from the *li-variable names that occur in these functions ;-)
;;
;; Installation:
;; ------------
;; Put
;; (require 'swiss-move)
;; somewhere in you init files and add the keybinding, e.g.:
;; (global-set-key '[(shift prior)]           'swiss-move-line-up)
;; (global-set-key '[(shift next)]            'swiss-move-line-down)
;;
;;; ChangeLog
;; 2006-12-08  GNU Emacs compat
;;
;;; Thanks
;; Christop Conrad for GNU Emacs suggestions
;;
;;; Code:
(defgroup swiss-move nil
  "Swiss movement moving with decreasing stepsizes."
  :tag "SwissMove"
  :link '(url-link :tag "Home Page"
                   "http://www.skamphausen.de/software/skamacs/")
  :link '(emacs-commentary-link
          :tag "Commentary in swiss-move.el" "swiss-move.el")
  :prefix "swiss-move-"
  :group 'environment
  :group 'extensions
  :group 'convenience)


(defcustom swiss-move-line-percent 50
  "*The percentage that is used to skip.
If set to 50 (the default) one swiss line move down from the beginning
of the buffer would bring you to the middle of it (50% ...), the next
jump to approximately 75%."
  :type 'number
  :group 'swiss-move)


(defvar swiss-move-saved-line nil
  "Used to store the last line swiss move jumped to.
This is important when changing the direction so that in that case the
stepsize is decreased, too.")

(defvar swiss-move-running nil
  "Used to track whether swiss movement is active or not.")

(if (featurep 'xemacs)
    (progn ;; XEmacs code:
      (defun swiss-move-line-number (&optional at-point)
    (line-number at-point)))
  (progn ;; GNU Emacs code
    (defun swiss-move-line-number (&optional at-point)
      (save-restriction
    (widen)
    (count-lines (point-min) (or at-point (point)))))))


(defun swiss-move-line-up ()
  "Swiss move upwards.
See `swiss-move-line' for details."
  (interactive)
  (swiss-move-line -1))

(defun swiss-move-line-down ()
  "Swiss move downwards.
See `swiss-move-line' for details."
  (interactive)
  (swiss-move-line 1))

(defun swiss-move-line (direction &optional percentage)
  "Move in decreasing numbers of lines.
Starting from current point this calculates a large jump using the
given DIRECTION.  DIRECTION.ist given as negativ number for upward and
positive number for downward movement.
Subsequent jumps will be smaller.  This works for
changing directions, too.  Thus you can close in on to your target.
Like this:


Start  press1 press2 press3
TOP
->.... ...... ...... ......
...... ...... ...... ......
...... ...... ...... ......
...... ...... ...... ......
...... ->.... ...... ......
...... ...... ...... ......
...... ...... ...... ->....
...... ...... ->.... ......
...... ...... ...... ......
...... ...... ...... ......
BOT

You might also consider the guessing a random number between 0 and
100. From you initial position 0 you jump forward half the distance to
50.  Then your quiz-master tells you that the random number is larger.
You take another jump forward, using half the last stepsize to 75.
This time your guess is too high, so take a step back, again taking
half of the last stepsize to 63.  And so on.

You'd better test it.
"
  (interactive)
  (or percentage (setq percentage swiss-move-line-percent))
  (let* ((currli (swiss-move-line-number))
         (otherli (cond
                   ((> direction 0)
                    (if (<= (or swiss-move-saved-line currli) currli)
                        (swiss-move-line-number (point-max))
                      swiss-move-saved-line))
                   ((< direction 0)
                    (if (>= (or swiss-move-saved-line currli) currli)
                        (swiss-move-line-number (point-min))
                      swiss-move-saved-line))
                   (t 0)))
         (skipli (floor (* (- otherli currli) 0.01 percentage))))
    ;;(message "from%d step%d save%s" currli skipli swiss-move-saved-line)
    (setq swiss-move-saved-line currli)
    (forward-line skipli))
  (swiss-move-hook-function))

(defun swiss-move-hook-function ()
  "Handle the swiss movement hook.
Install and remove ourself on `pre-command-hook' and control the swiss
movement active state."
  (if (and (not (or
                 (eq last-command 'swiss-move-line-up)
                 (eq last-command 'swiss-move-line-down)))
           (not (null swiss-move-running))
           (memq 'swiss-move-hook-function pre-command-hook))
      (progn
        (remove-hook 'pre-command-hook 'swiss-move-hook-function)
        (setq swiss-move-running nil)
        (setq swiss-move-saved-line nil))
    (unless (memq 'swiss-move-hook-function pre-command-hook)
      (setq swiss-move-running t)
      (add-hook 'pre-command-hook 'swiss-move-hook-function))))


(provide 'swiss-move)

;;; swiss-move.el ends here
