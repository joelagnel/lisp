;;;; use bundled sets of font and dimensions
;;; Time-stamp: <2006-02-23 12:14:15 John.Sturdy>

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

(provide 'screen-setups)
(require 'cl)

;; (cdr (assoc 'font (frame-parameters)))

;; use x-list-fonts to get possibilities

(setq try-fonts-old-font (cdr (assoc 'font (frame-parameters)))
      try-fonts-old-width (screen-width)
      try-fonts-old-height (screen-height))

(defun try-fonts-restore ()
  (interactive)
  (set-default-font try-fonts-old-font)
  (set-screen-width try-fonts-old-width)
  (set-screen-height try-fonts-old-height))


(defun try-fonts (pattern)
  (interactive "sTry fonts matching: ")
  (switch-to-buffer (get-buffer-create "*Fonts*"))
  (erase-buffer)
  (let ((fonts (x-list-fonts pattern))
	(most-lines 0)
	(most-lines-font nil)
	)
    (setq try-fonts-old-font (cdr (assoc 'font (frame-parameters)))
	  try-fonts-old-width (screen-width)
	  try-fonts-old-height (screen-height))
    (set-screen-height 8)
    (set-screen-width 96)
    (while fonts
      (set-default-font (car fonts))
      (insert (format "%S: %dx%d\n" (car fonts) (frame-width) (frame-height)))
      (goto-char (point-max))
      ;; need to maximize frame after setting each font, for this to make sense:
      (if (> (frame-height) most-lines)
	  (setq most-lines (frame-height)
		most-lines-font (car fonts)))
      (sit-for 1)
      (setq fonts (cdr fonts)))
    (try-fonts-restore)
    (message "%s was the font with most lines, at %d" most-lines-font most-lines)))

(defvar screen-setups
  '(("normal" "6x10" 80 24))
  "Some bundled named lists of font, frame width, frame height.
These are meant as pre-packaged frame setups, to be set appropriately
on each host / X-server on which you run Emacs. I set them up so that
on each of my machines, Emacs takes all the display space available
to it -- why would I want to look at anything else? ;-)")

;;;###autoload
(defun get-screen-setup (&rest names)
  "Select the first screen setup that exists, from NAMES."
  (catch 'found
    (while names
      (let ((this (assoc (car names) screen-setups)))
	(if this
	    (throw 'found this)
	  (setq names (cdr names)))))
    (assoc "normal" screen-setups)))

(defvar current-screen-setup-name nil
  "The current screen setup, as last set by use-screen-setup.")

;;;###autoload
(defun use-screen-setup (name)
  "Use screen setup NAME."
  (interactive
   (list
    (completing-read "Use screen setup: "
		     screen-setups
		     nil
		     t)))
  (let ((setup (get-screen-setup name)))
    (if setup
	(let* ((fp (frame-parameters))
	       (oldfont (assoc 'font fp))
	       )
	  (message "Using screen setup %S" setup)
	  (when oldfont
	    (message "Changing font from %S to %S" (cdr oldfont) (second setup)))
	  (condition-case evar
	      (progn
		(set-frame-font (second setup))
		(setq current-screen-setup-name name)
		(message "Changing size from %dx%d to %dx%d"
			 (cdr (assoc 'width fp)) (cdr (assoc 'height fp))
			 (third setup) (fourth setup))
		(set-frame-width (selected-frame) (third setup))
		(set-frame-height (selected-frame) (fourth setup))
		(setq buffers-menu-max-size (/ (* 3 (fourth setup)) 4)))
	    (error "Failed to select font %s: error %s" (second setup) evar)))
      (message "No such screen setup"))))

(defvar default-screen-setup "normal"
  "Which screen setup to start with.
Will normally be pre-set from host-setup.el")

(use-screen-setup default-screen-setup)

;;; end of screen-setups.el
