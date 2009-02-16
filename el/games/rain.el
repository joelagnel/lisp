;;; rain.el --- display raindrops in a buffer

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Ryan Yeske <rcye...@vcn.bc.ca>
;; Keywords: games

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

;; This is a cheap rip off of rain(6) from BSD.

;; M-x rain RET to start raining
;; M-x rain-stop RET to stop raining

;; Note: rain.el is timer driven and thus it will keep raining even
;; when you bury the buffer.  I'm not really sure how to implement a
;; clean way to stop it.  Bind all keys to rain-stop?  Use sit-for
;; with a big value or with a loop and quit when it returns?

;;; Code:

(require 'gamegrid)

(defgroup rain nil
  "Rain."
  :group 'games)

(defcustom rain-drop-number 5
  "How many drops of rain to show at once."
  :type '(integer)
  :group 'rain)

(defcustom rain-splash-delay .1
  "Delay between splash frames in seconds."
  :type '(float)
  :group 'rain)

(defcustom rain-buffer-name "*rain*"
  "Name of rain buffer."
  :type '(string)
  :group 'rain)

(defun rain ()
  "Create a buffer and rain in it."
  (interactive)
  (rain-stop)
  (switch-to-buffer (get-buffer-create rain-buffer-name))
  (delete-other-windows)    
  (gamegrid-init (make-vector 256 nil))
  (setq rain-window-width (- (window-width) 1))
  (setq rain-window-height (- (window-height) 2))
  (gamegrid-init-buffer rain-window-width
			rain-window-height 
			? )
  (dotimes (i rain-drop-number)
    (rain-drop-start)
    (sit-for .1)))

(defun rain-stop ()
  "Quit raining."
  (interactive)
  (cancel-function-timers 'rain-drop-splash))

(defvar rain-window-width nil)
(defvar rain-window-height nil)

(defun rain-drop-start ()
  (rain-drop-splash (+ (/ rain-drop-max-width 2)
		       (random (- rain-window-width
				  rain-drop-max-width)))
		    (+ (/ rain-drop-max-height 2)
		       (random (- rain-window-height
				  rain-drop-max-height)))
		    rain-splash-delay
		    rain-drop))

(defun rain-drop-splash (x y delay frames)
  (if (not (consp frames))
      (rain-drop-start)
    (rain-drop-splash-frame x y (car frames))
    (run-with-timer delay nil 
		    'rain-drop-splash x y delay (cdr frames))))

(defun rain-drop-splash-frame (x y frame)
  (with-current-buffer rain-buffer-name
    (mapcar (lambda (cell)
	      (gamegrid-set-cell (+ x (car cell))
				 (+ y (cadr cell))
				 (caddr cell)))
	    frame)))

(defvar rain-drop-max-width 5)
(defvar rain-drop-max-height 5)
(defvar rain-drop
  (list (list (list 0 0 ?.))
 	(list (list 0 0 ?o))
 	(list (list 0 0 ?O))
 	(list (list 0 0 ?.)
 	      (list -1 0 ?|)
 	      (list 1 0 ?|)
	      (list 0 -1 ?-)
	      (list 0 1 ?-))
	(list (list -1 0 ? )
	      (list 1 0 ? )
	      (list 0 -1 ? )
	      (list 0 1 ? )
	      (list 0 0 ?O)
	      (list -2 0 ?|)
	      (list 2 0 ?|)
	      (list 0 -2 ?-)
	      (list 0 2 ?-)
	      (list -1 -1 ?/)
	      (list +1 1 ?/)
	      (list -1 1 ?\\)
	      (list +1 -1 ?\\))
	(list (list 0 0 ? )
	      (list -2 0 ? )
	      (list 2 0 ? )
	      (list 0 -2 ? )
	      (list 0 2 ? )
	      (list -1 -1 ? )
	      (list +1 1 ? )
	      (list -1 1 ? )
	      (list +1 -1 ? )))
  "List of frames of a splash animation.")

(provide 'rain)
;;; rain.el ends here