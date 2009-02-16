;;; Blinking cursor mode for GNU Emacs
;;; Copyright (C) 1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle_jones@wonderworks.com)
;;; or from the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;
;;; Send bug reports to kyle_jones@wonderworks.com

;; To install, put this code into a file called blinking-cursor.el in
;; one of the directories in your Emacs load-path and byte-compile,
;; i.e. M-x byte-compile-file .  Then put
;;
;; (require 'blinking-cursor)
;; (blinking-cursor-mode 1)
;;
;; in your .emacs file.

(provide 'blinking-cursor)

(require 'timer)

(defconst blinking-cursor-version "1.00")

(defvar blinking-cursor-mode nil
  "Non-nil value means Blinking Cursor mode is active.")

(defvar blinking-cursor-blink-frequency 2
  "Number of times the cursor should change color per second.
This can be a floating point value.")

(defvar blinking-cursor-colors ["gold" "blue"]
  "Array of colors that will be cycled through to blink the cursor.
You can set it like this in your .emacs file:

   (setq blinking-cursor-colors [\"gold\" \"blue\"])

The array should contain at least two color names.

The first color in the vector is the one Emacs will return to
while you are typing.  The cursor will not blink unless Emacs is
idle.")

(defun blinking-cursor-mode (&optional arg)
  "Toggle Blinking Cursor mode.
With arg, turn Blinking Cursor mode on iff arg is positive.
When Blinking Cursor mode is enabled, the cursor blinks when Emacs is idle."
  (interactive "P")
  (setq blinking-cursor-mode (or (and arg (> (prefix-numeric-value arg) 0))
				 (and (null arg) (null blinking-cursor-mode))))
  (cond ((not blinking-cursor-mode)
	 (cancel-function-timers 'blinking-cursor-start-blinking)
	 (cancel-function-timers 'blinking-cursor-blink)
	 (remove-hook 'pre-command-hook 'blinking-cursor-stop-blinking)
	 (blinking-cursor-stop-blinking))
	(t
	 (let ((timeout (/ 1.0 blinking-cursor-blink-frequency)))
	   (run-with-timer timeout timeout 'blinking-cursor-blink)
	   (run-with-idle-timer 1 1 'blinking-cursor-start-blinking)
	   (add-hook 'pre-command-hook 'blinking-cursor-stop-blinking)))))

(defvar blinking-cursor-tick 0
  "Index into the array of cursor colors.
Internal variable, do not set this.")

(defvar blinking-cursor-should-blink nil
  "Non-nil means blinking-cursor-blink should change the cursor color.
Internal variable, do not set this.")

(defun blinking-cursor-blink (&rest ignored)
  "Changes the cursor color if blinking-color-should-blink is non-nil.
Uses colors in blinking-cursor-colors and indexed by blinking-cursor-tick.
Increments blinking-cursor-tick."
  (condition-case err-data
      (cond (blinking-cursor-should-blink
	     (set-cursor-color (aref blinking-cursor-colors
				     (% blinking-cursor-tick
					(length blinking-cursor-colors))))
	     (setq blinking-cursor-tick (1+ blinking-cursor-tick))))
    ;; if Emacs can't get a color don't throw an error .
    (error
     (message "blinking-cursor-blink signaled: %S" err-data))))

(defun blinking-cursor-start-blinking ()
  "Make the cursor start blinking."
  (setq blinking-cursor-should-blink t))

(defun blinking-cursor-stop-blinking ()
  "Stop the cursor's blinking.
Returns the cursor's color to the first color in the blinking-cursor-colors
array."
  (setq blinking-cursor-tick 0
	blinking-cursor-should-blink t)
  (blinking-cursor-blink)
  (setq blinking-cursor-should-blink nil))

