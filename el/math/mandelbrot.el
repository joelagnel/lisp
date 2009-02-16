;;; u-mandelbrot.el --- A simple fractal browser.
;;
;;  Copyright (C) 2001 by Ulf Jasper
;;
;;  Emacs Lisp Archive Entry
;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   u-mandelbrot.el
;;  Time-stamp: "10. April 2002, 21:34:18 (ulf)"
;;  Created:    January 26 2001
;;  Keywords:   Games, 
;;  Version:    $Id: u-mandelbrot.el,v 1.5 2002/08/04 11:49:12 ulf Exp $
;;
;;
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;; Philosophy:
;;
;;  The following ideas have been focussed during the development of this
;;  package (`u-mandelbrot.el').
;;  
;;  * It is intended as a small contribution to support the
;;    Neo-Post-Retro-Programming(TM) Movement.
;;
;;  * It shall give another demonstration of the fact that Emacs not only
;;    is The One True Editor, but also The Universal User Interface.
;;
;;  * It shall demonstrate that there are more ways to waste your time than
;;    you might have thought.
;;
;;  * It shall remind you of the good old days when your screen had a
;;    resolution of 160 x 120 pixels and everything was much better. 
;;
;;; Installation Instructions:
;; 
;;  In order to use this package, load this file and say `M-x u-mandelbrot
;;  RET'. For performance reasons you might want to consider byte-compiling
;;  this file. 
;;  If you have tried that, and you should feel like using this package
;;  ever again, you might want to place this file somewhere in your
;;  load-path and put the following in your Emacs startup file (~/.emacs)
;;
;;  (autoload 'u-mandelbrot "u-mandelbrot" "A simple fractal browser" t)
;;
;;; Usage:
;;
;;  Just call `u-mandelbrot' and follow the instructions. Please be patient
;;  -- it might take a while...
;;
;;; Commentary / Disclaimer:
;; 
;;  This is the result of weekend work. Don't expect too much. It does not
;;  make use of any fancy tricks. It just applies the $z \rightarrow z^2 +
;;  z_0$ algorithm in a brute force way.
;;
;;  The "ascii color-table" is quite poor. It should be enlarged.
;;
;;  This package has been tested on Emacs 20.7.1 and XEmacs 21.1.10.
;;  Apparently it runs faster with XEmacs... (!?)
;;
;;  (X)Emacs versions prior to these will never be supported.
;;
;;; History:
;; 
;;  1.0: Initial release
;;
;;; Code:

(if (string-match "XEmacs\\|Lucid" emacs-version)
    (require 'overlay))

(defvar u-mandelbrot-width 10 
  "Width of mandelbrot paint area.")
(defvar u-mandelbrot-height 10
  "Height of mandelbrot paint area")
(defvar u-mandelbrot-center-x 0
  "Real part of current center position.")
(defvar u-mandelbrot-center-y 0
  "Imaginary part of current center position.")
(defvar u-mandelbrot-re-offset 0
  "Real part of current topleft corner position.")
(defvar u-mandelbrot-im-offset 0
  "Imaginary part of current topleft corner position.")
(defvar u-mandelbrot-zoomfactor 8.0
  "Current zoom factor")
(defvar u-mandelbrot-numsteps 100 
  "Maximal number of iterations per pixel.")
(defvar u-mandelbrot-numcols 100
  "Number of colors for color mode.")
(defvar u-mandelbrot-line-width 0
  "Width of paint area, including linebreak")
(defvar u-mandelbrot-font-ratio 2
  "Height over width ratio for current font.")
(defvar u-mandelbrot-mode 'ascii
  "Current display mode. Allowed values are: `ascii' and `color'.")
(defvar u-mandelbrot-grayscale nil
  "Use grayscale colortable if t.")
(defvar u-mandelbrot-face-set nil
  "Face for painting pixels within the mandelbrot set.")
(defvar u-mandelbrot-face-vec nil
  "Vector containing all faces for display.")
(defvar u-mandelbrot-display-while-working nil
  "Display each line immediately it t.")
(defvar u-mandelbrot-ascii-map
  [?$ ?@ ?B ?& ?% ?X ?E ?C ?o ?* ?x ?+ ?, ?. ]
  "Color table for ascii mode.")


;; Does somebody remember `u-color-cycle.el'? Try these defuns instead of
;; u-color-cycle-*-val.
(defun u-mandelbrot-g-val (i)
  (cond ((< i (/ 2 3.0)) (* 255 (sin (* 1.5 pi i))))
	(t           0 )))

(defun u-mandelbrot-b-val (i)
  (cond ((< i (/ 1 3.0)) 0)
	(t               (* 255 (sin (* 1.5 pi (- i 0.333)))))))

(defun u-mandelbrot-r-val (i)
  (cond ((< i (/ 1 3.0)) (* 255 (sin (* 1.5 pi (+ i 0.333)))))
	((< i (/ 2 3.0)) 0)
	(t 	         (* 255 (sin (* 1.5 pi (- i 0.666)))))))

(defun u-mandelbrot-create-faces (num)
  "Create NUM faces for u-mandelbrot."
  (let* ((i 0)
	 (u-color "")
	 (c-face nil)
	 (r-val 0)
	 (g-val 0)
	 (b-val 0)
	 (j 0))
    (setq c-face (facemenu-get-face (intern "u-mandelbrot-face-set" )))
    (if (not c-face) (make-face c-face))
    (set-face-background c-face "black")
    (setq u-mandelbrot-face-set c-face)
    (setq u-mandelbrot-face-vec (make-vector 0 nil))
    (while (< i num)
      (setq j (/ (* i 1.0) (1- num)))
      (setq r-val (u-mandelbrot-r-val j))
      (setq g-val (u-mandelbrot-g-val j))
      (setq b-val (u-mandelbrot-b-val j))
      (setq j (* 255 (- 1 j)))
      (if u-mandelbrot-grayscale
	  (setq u-color (format "#%02x%02x%02x" j j j))
	(setq u-color (format "#%02x%02x%02x" r-val g-val b-val)))
      (setq c-face (facemenu-get-face (intern
				       (format "u-mandelbrot-face-%03d" i))))
      (setq u-mandelbrot-face-vec (vconcat u-mandelbrot-face-vec (list c-face)))
      (if (not c-face) (make-face c-face))
      (make-face c-face)
      ;;(setq c-face (aref u-mandelbrot-face-vec i))
      (set-face-background c-face u-color)
      (setq i (1+ i)))))


(defun u-mandelbrot-calculate (z)
  "...
Argument Z complex number."
  (let* ((c (car z))
	 (d (car (cdr z)))
	 (max u-mandelbrot-numsteps) (i 0)
	 (abs-value 0)
	 (x 0)
	 (y 0)
	 (x2 0)
	 (y2 0)
	 (tx 0)
	 )
    (while (and (< i max) (< abs-value 1234567))
      (setq i (+ 1 i))
      (setq x2 (* x x))
      (setq y2 (* y y))
      (setq tx (- (+ x2 c) y2))
      (setq y (+ (* 2 x y) d))
      (setq x tx)
      (setq abs-value (+ (* x x) (* y y)))
      )
    i))

(defun u-mandelbrot-paint-cell (x y)
  (let* ((value (u-mandelbrot-calculate (u-mandelbrot-cursor-to-complex x y)))
	 (start (+ x (* y u-mandelbrot-line-width) 1))
	 ;;(end (+ 1 start))
	 (color "#ffffff")
	 (t-overlay nil)
	 (c-face nil)
	)
    (cond ((equal u-mandelbrot-mode 'ascii)
	   (goto-char start)
	   (let ((c
		  (aref u-mandelbrot-ascii-map (% value u-mandelbrot-numcols))))
	     (if (>= value u-mandelbrot-numsteps) (setq c ? ))
	     (insert-char c 1))
	   (delete-char 1))
	  (t
	   (if (>= value u-mandelbrot-numsteps)
	       (progn
		 (setq c-face u-mandelbrot-face-set))
	     (setq c-face (aref u-mandelbrot-face-vec
				(% value u-mandelbrot-numcols))))
	   (setq t-overlay (u-mandelbrot-get-overlay start))
	   (overlay-put t-overlay 'value value)
	   (overlay-put t-overlay 'face c-face)
	   ;;(add-text-properties start end (list 'face c-face))
	   (face-background c-face)
	   )))
  )

(defun u-mandelbrot-cursor-to-complex (x y)
  (list (+ (/ x u-mandelbrot-zoomfactor) u-mandelbrot-re-offset)
	(* (+ u-mandelbrot-im-offset (/ y u-mandelbrot-zoomfactor))
	   u-mandelbrot-font-ratio)))

(defun u-mandelbrot-cursor-pos (&rest args)
  (interactive)
  (let* ((pos (point)) (re 0) (im 0) (z nil)
	 (x (% pos u-mandelbrot-line-width))
	 (y (/ pos u-mandelbrot-line-width)))
    (setq re (/ (* 1.0 x) u-mandelbrot-zoomfactor))
    (setq im (/ (* 1.0 y) u-mandelbrot-zoomfactor))
    (setq re (+ re u-mandelbrot-re-offset))
    (setq im (+ im u-mandelbrot-im-offset))
    (setq z (u-mandelbrot-cursor-to-complex x y))
    (message "%f + %fi (%f%%)" (car z) (car (cdr z)) u-mandelbrot-zoomfactor)
    (list re im)))

(defun u-mandelbrot-zoom (step &rest args)
  (interactive)
  (setq u-mandelbrot-numsteps
	(string-to-number
	 (read-string "Maximal number of iterations: "
		      (number-to-string u-mandelbrot-numsteps))))
  (let* ((c (u-mandelbrot-cursor-pos))
	 (x (car c))
	 (y (car (cdr c))))
    (message "Zooming from %f %f" x y)
    (setq u-mandelbrot-zoomfactor
	  (* u-mandelbrot-zoomfactor step))
    (setq u-mandelbrot-center-x x)
    (setq u-mandelbrot-center-y y)
    (u-mandelbrot-paint)
    ))

(defun u-mandelbrot-get-overlay (pos)
  "Return the (first) overlay at POS.
Argument POS is the buffer position."
  (car (overlays-in pos (+ 1 pos))))

(defun u-mandelbrot-fill ()
  (message "Preparing buffer...")
  (goto-char (point-min))
  (let ((i 0) (j 0) (pos 0) (t-overlay nil))
    (while (< i u-mandelbrot-height)
      (setq i (+ 1 i))
      (setq j 0)
      (while (< j u-mandelbrot-width)
	(setq j (+ 1 j))
	(setq pos (+ 1 pos))
	(insert " ")
	(setq t-overlay (make-overlay pos (+ 1 pos)))
	(overlay-put t-overlay 'value 0))
      (setq pos (+ 1 pos))
      (insert "\n")))
  (message "Preparing buffer... done"))

(defun u-mandelbrot-show-progress (current total)
  (if u-mandelbrot-display-while-working (sit-for 0))
  (let* ((length u-mandelbrot-width)
	 (string (make-string length ?.))
	 (value (/ (* current length) total))
	 (i 0))
    (while (<= i value)
      (aset string i ?#)
      (setq i (+ 1 i)))
    (message string)))

(defun u-mandelbrot-paint ()
  (setq u-mandelbrot-re-offset
	(- u-mandelbrot-center-x
	   (/ (* 0.5 u-mandelbrot-width) u-mandelbrot-zoomfactor)))
  (setq u-mandelbrot-im-offset
	(- u-mandelbrot-center-y
	   (/ (* 0.5 u-mandelbrot-height) u-mandelbrot-zoomfactor)))
  (if (equal u-mandelbrot-mode 'ascii)
      (setq buffer-read-only nil))
  (let ((i 0) (j 0)
	(xpm-values (list)))
    (while (< j u-mandelbrot-height)
      ;;(message "Working on line %d" j)
      (u-mandelbrot-show-progress j u-mandelbrot-height)
      (setq i 0)
      (while (< i u-mandelbrot-width)
	;;(setq xpm-values (append xpm-values (list (u-mandelbrot-paint-cell i j))))
	(u-mandelbrot-paint-cell i j)
	(setq i (+ 1 i)))
      (setq j (+ 1 j)))
    ;;(u-xpm-display (point-min) u-mandelbrot-width u-mandelbrot-height xpm-values)
    )
  (goto-char (+ (* (/ u-mandelbrot-height 2) u-mandelbrot-line-width)
		(/ u-mandelbrot-width 2)))
  (if (equal u-mandelbrot-mode 'ascii)
      (setq buffer-read-only t))
  (let ((z (u-mandelbrot-cursor-pos)))
    (u-mandelbrot-message
     "u-mandelbrot --- Copyright (C) 2001 by Ulf Jasper"
     "Type `+' to zoom in at cursor position."
     "Type `M-+' to zoom in even faster."
     "Type `-' to zoom out at cursor position."
     "Type `M--' to zoom out even faster."
     "Type `.' to center at cursor position."
     "Type `c' for another psychedelic experience (color mode only)."
     (format "Current position: %f + %fi (%f%%)" 
	     (car z) (car (cdr z))
	     u-mandelbrot-zoomfactor)))
  )

(defun u-mandelbrot-cycle ()
  (interactive)
  (cond ((equal u-mandelbrot-mode 'color)
	 (let ((ll (overlays-in (point-min) (point-max)))
	       l cur-overlay
	       (offset 0)
	       (nc u-mandelbrot-numcols)
	       (val 0)
	       (c-face nil))
	   (while (sit-for 0)
	     (setq l ll)
	     (setq offset (+ offset 1))
	     (while l
	       (setq cur-overlay (car l))
	       (setq l (cdr l))
	       (setq val (overlay-get cur-overlay 'value))
	       (if (< val u-mandelbrot-numsteps)
		   (progn
		     (setq c-face (aref u-mandelbrot-face-vec
					(% (+ val offset) nc)))
		     (overlay-put cur-overlay 'face c-face))))
	     )))))

(defun u-mandelbrot-message (&rest msg-list)
  (let ((ptr msg-list))
    (message "%s" (car ptr))
    (while (sit-for 4)
      (setq ptr (cdr ptr))
      (if (not ptr) (setq ptr msg-list))
      (message "%s" (car ptr)))))


(defun u-mandelbrot ()
  "..."
  (interactive)
  (let* ((m-buffer nil)
	(height 0)
	(width 0)
	(t-string)
	)
    (setq t-string
	  (completing-read "Mode: " '(("ascii" 1) ("color" 3))
			   nil t "ascii"))
    (if (string-equal t-string "ascii")
	(setq u-mandelbrot-mode 'ascii)
      (setq u-mandelbrot-mode 'color))
    (if (equal u-mandelbrot-mode 'color)
	(progn
	  (if (y-or-n-p "Grayscale? ")
	      (setq u-mandelbrot-grayscale t)
	    (setq u-mandelbrot-grayscale nil))
	  (setq u-mandelbrot-numcols
		(string-to-number
		 (read-string "Number of colors: " "100")))))
    (setq u-mandelbrot-numsteps
	  (string-to-number
	   (read-string "Maximal number of iterations: "
			(number-to-string u-mandelbrot-numsteps))))


    (setq m-buffer (get-buffer-create "*mandelbrot*"))
    (switch-to-buffer m-buffer)
    (delete-other-windows)
    (sit-for 0)

    (setq height (- (window-height) 1))
    (setq width (- (window-width) 1))
    ;; make sure width and height are even:
    (setq width (* 2 (/ width 2)))
    (setq height (* 2 (/ height 2)))

    (make-local-variable 'u-mandelbrot-width)
    (setq u-mandelbrot-width width)
    (make-local-variable 'u-mandelbrot-height)
    (setq u-mandelbrot-height height)
    (make-local-variable 'u-mandelbrot-line-width)
    (setq u-mandelbrot-line-width (+ 1 width))
    (make-local-variable 'u-mandelbrot-zoomfactor)
    ;;(setq u-mandelbrot-zoomfactor 3.0)
    (make-local-variable 'u-mandelbrot-re-offset)
    (make-local-variable 'u-mandelbrot-im-offset)
    (make-local-variable 'u-mandelbrot-center-x)
    (setq u-mandelbrot-center-x 0.0)
    (make-local-variable 'u-mandelbrot-center-y)
    (setq u-mandelbrot-center-y 0.0)


    (local-set-key " " 'u-mandelbrot-cursor-pos)
    (local-set-key "c" 'u-mandelbrot-cycle)
    (local-set-key "q" 'kill-buffer)
    (local-set-key "+"
		   (lambda () (interactive) (u-mandelbrot-zoom 1.5)))
    (local-set-key "\M-+"
		   (lambda () (interactive) (u-mandelbrot-zoom 3)))
    (local-set-key "-"
		   (lambda () (interactive) (u-mandelbrot-zoom 0.666)))
    (local-set-key "\M--"
		   (lambda () (interactive) (u-mandelbrot-zoom 0.333)))
    (local-set-key "."
		   (lambda () (interactive) (u-mandelbrot-zoom 1)))

    (if (equal u-mandelbrot-mode 'ascii)
	(setq u-mandelbrot-numcols (length u-mandelbrot-ascii-map))
      (u-mandelbrot-create-faces (+ 1 u-mandelbrot-numcols)))
    (u-mandelbrot-fill)
    (setq buffer-read-only t)
    (u-mandelbrot-paint)
    ))
      

(provide 'u-mandelbrot)

;;; u-mandelbrot.el ends here
