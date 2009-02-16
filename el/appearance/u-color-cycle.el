;;; u-color-cycle.el --- eye-candy

;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   u-color-cycle.el
;;  Time-stamp: "11. Oktober 2004, 21:06:27 (ulf)"
;;  Created:    Summer 2000
;;  Keywords:   Games, Fun, Eye-Candy
;;  Version:    $Id: u-color-cycle.el,v 1.6 2004/10/11 19:14:17 ulf Exp $

(defconst u-color-cycle-version 2.0
  "Version number of u-color-cycle.el.")

;; ======================================================================

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


;; ======================================================================
;;; Commentary:

;;  u-color-cycle provides a set of commands which temporarily change the
;;  colors in the current buffer.  Colors will change until the next
;;  interaction (key press, mouse event) occurs.  The original color-status
;;  of the current buffer is restored afterwards.  The contents of the
;;  buffer are unchanged! Load this file and say `M-x u-color-cycle-window
;;  RET' to find out what this means.

;;  The following color-cycling methods are available:

;;  * `u-color-cycle-window':
;;      The whole text gets the same foreground color which is for changed
;;      smoothly.
;;  * `u-color-cycle-window-rainbow':
;;      The text color changes smoothly from one character to the next.
;;      The colors move through the text.  This method was contributed by
;;      Jesse Weinstein.
;;  * `u-color-cycle-window-fade':
;;      The foreground colors slowly fade away so that the buffer contents
;;      become "invisible".
;;  * `u-color-cycle-window-swap':
;;      The faces (not only colors) in the buffer are interchanged between
;;      text portions with different faces.

;;  Each command exists in a u-color-cycle-region-* variant as well.

;;  You may want to add the u-color-cycle functions to the list of
;;  `zone-programs'. This is one way to do it:

;;  (eval-after-load 'zone 
;;    '(progn 
;;       (load-library "u-color-cycle") 
;;       (setq zone-programs (vconcat zone-programs [u-color-cycle-window
;;                                    u-color-cycle-window-fade
;;                                    u-color-cycle-window-swap
;;                                    u-color-cycle-window-rainbow]))))

;;  u-color-cycle uses overlays.  It might therefore happen that
;;  interferences with other packages like highlight-current-line or hifill
;;  occur.


;;  The overlay stuff was inspired by flyspell.el

;; ======================================================================
;;; History:

;;  Version 2.0:
;;  *  Modified by Jesse Weinstein <jessw@netwood.net> on 6/8/2004 to
;;     display different colors on each character, in a rainbow pattern.
;;  *  Added color-cycling-swap and color-cycling-fade.  
;;  *  Tested with Emacs 21.3.

;;  Version -:
;;  *  Initial release.
;;  *  Tested with Emacs 20.x and XEmacs 21.1.x.

;; ======================================================================
;;; Code:

(require 'overlay)

(defvar u-color-cycle-num-colors 50
  "Number of colors that are used for color-cycling.")
(defvar u-color-cycle-face-vec nil
  "Vector of faces for color-cycling.")
(defvar u-color-cycle-sleep 0.1
  "Number of seconds between color-cycling steps.
This is a floating point number.  Smaller values make color-cycling
animations faster.")

(defun u-color-cycle-get-overlay (pos)
  "Return the (first) color-cycle-related overlay at buffer position POS."
  (catch 'found
    (mapc (lambda (t-overlay)
            (if (overlay-get t-overlay 'u-color-cycle-overlay)
                (throw 'found t-overlay)))
          (overlays-in pos (1+ pos)))
    nil))

(defun u-color-cycle-delete-all-overlays (min max)
  "Delete all overlays used by u-color-cycle in the region from MIN to MAX."
  (let ((overlays (overlays-in min max)))
    (mapcar (lambda (cur-overlay)
	      (if (and (overlayp cur-overlay)
		       (overlay-get cur-overlay 'u-color-cycle-overlay))
		  (delete-overlay cur-overlay)))
	    overlays)))
	      

;; walk along the edges of the red-green-blue triangle
;; keep one color fully set...
(defun u-color-cycle-g-val (i)
  "Get the green-value for color no. I."
  (cond ((< i (/ 2 3.0)) (* 255 (sin (* 1.5 pi i))))
	(t 0 )))

(defun u-color-cycle-b-val (i)
  "Get the blue-value for color no. I."
  (cond ((< i (/ 1 3.0)) 0)
	(t (* 255 (sin (* 1.5 pi (- i 0.333)))))))

(defun u-color-cycle-r-val (i)
  "Get the red-value for color no. I."
  (cond ((< i (/ 1 3.0)) (* 255 (sin (* 1.5 pi (+ i 0.333)))))
	((< i (/ 2 3.0)) 0)
	(t (* 255 (sin (* 1.5 pi (- i 0.666)))))))

(defun u-color-cycle-create-faces (num)
  "Create NUM faces for u-color-cycling."
  (let* ((i 0)
	 (u-color "")
	 (c-face nil)
	 (r-val 0)
	 (g-val 0)
	 (b-val 0)
	 (j 0))
    (setq u-color-cycle-face-vec (make-vector 0 nil))
    (while (< i num)
      (setq j (/ (* i 1.0) (1- num)))
      (setq r-val (u-color-cycle-r-val j))
      (setq g-val (u-color-cycle-g-val j))
      (setq b-val (u-color-cycle-b-val j))
      ;;(message "%3d %3d %3d %3d" r-val g-val b-val (+ r-val g-val b-val))
      (setq u-color (format "#%02x%02x%02x" r-val g-val b-val))
      (setq c-face (facemenu-get-face (intern
				       (format "u-color-cycle-face-%d" i))))
      (setq u-color-cycle-face-vec (vconcat u-color-cycle-face-vec
					    (list c-face)))
      (unless c-face
	(make-face c-face))
      ;;(setq c-face (aref u-color-cycle-face-vec i))
      (set-face-foreground c-face u-color)
      (setq i (1+ i)))))

(defun u-color-cycle-create-faces-gray (num)
  "Create NUM faces for u-color-cycling, using shades of gray."
  (let* ((i 0)
	 (u-color "")
	 (c-face nil)
	 (val 0))
    (setq u-color-cycle-face-vec (make-vector 0 nil))
    (while (< i num)
      (setq val (* 255 (sin (* (/ (* i 1.0) num) pi))))
      (setq u-color (format "#%02x%02x%02x" val val val))
      (setq c-face (facemenu-get-face (intern
				       (format "u-color-cycle-face-%d" i))))
      (setq u-color-cycle-face-vec (vconcat u-color-cycle-face-vec
					    (list c-face)))
      (unless c-face
	(make-face c-face))
      (set-face-foreground c-face u-color)
      (setq i (1+ i)))))

(defun u-color-cycle-region (min max)
  "Start color cycling in the region from MIN to MAX."
  (interactive "r")
  (if (string-match "XEmacs" emacs-version)
      (zmacs-deactivate-region)
    (deactivate-mark))
  (u-color-cycle-create-faces u-color-cycle-num-colors)
  (let ((t-overlay (make-overlay min max))
	(offset 0)
	(i 0))
    (overlay-put t-overlay 'u-color-cycle-overlay t)
    (while (sit-for u-color-cycle-sleep)
      (setq offset (1+ offset))
      (let* ((c-face (aref u-color-cycle-face-vec
			   (% offset u-color-cycle-num-colors))))
	(overlay-put t-overlay 'face c-face))))
  (u-color-cycle-delete-all-overlays min max))

(defun u-color-cycle-region-rainbow (min max &optional size)
  "Start color cycling with a rainbow pattern in the region from MIN to MAX.
Optional arg SIZE specifies over how many characters a color spans. SIZE
must be larger than 0; it defaults to 10."
  (interactive "r")
  (if (string-match "XEmacs" emacs-version)
      (zmacs-deactivate-region)
    (deactivate-mark))
  (u-color-cycle-create-faces u-color-cycle-num-colors)
  (unless size
    (setq size 10))
  (setq size (max 1 size))
  (let* ((max1 (+ max 1))
	 (t-overlays (list))
	 (offset 0)
	 (i 0))
    (while (< i (- max1 min))
      (setq t-overlays (cons (make-overlay (+ min i) (+ min i size))
			     t-overlays))
      (overlay-put (car t-overlays) 'u-color-cycle-overlay t)
      (setq i (+ i size)))
    (while (sit-for u-color-cycle-sleep)
      (setq offset (1+ offset))
      (setq i 0)
      (mapc
       (lambda (ov)
	 (let ((c-face (aref u-color-cycle-face-vec
			     (% (+ i offset) u-color-cycle-num-colors))))
	   (overlay-put ov 'face c-face))
	 (setq i (1+ i)))
       t-overlays)))
  (u-color-cycle-delete-all-overlays min max))

(defun u-color-cycle-region-swap (min max)
  "Start color swapping in the region from MIN to MAX."
  (interactive "r")
  (if (string-match "XEmacs" emacs-version)
      (zmacs-deactivate-region)
    (deactivate-mark))
  (let ((i max)
	j t-overlay t-face)
    (while (> i min)
      (setq j (previous-single-property-change i 'face nil min))
      (setq t-face (or (get-text-property j 'face) 'default))
      (setq t-overlay (make-overlay j i))
      (overlay-put t-overlay 'u-color-cycle-overlay t)
      (overlay-put t-overlay 'u-color-cycle-face t-face)
      (setq i j)))
  
  (let ((num (- max min))
	pos a-face b-face t-overlay)
    (setq pos (+ min (random num)))
    (setq t-overlay (u-color-cycle-get-overlay pos))
    (setq a-face (overlay-get t-overlay 'u-color-cycle-face))
    (while (sit-for u-color-cycle-sleep)
      (setq pos (+ min (random num)))
      (setq t-overlay (u-color-cycle-get-overlay pos))
      (setq b-face (overlay-get t-overlay 'u-color-cycle-face))
      (overlay-put t-overlay 'u-color-cycle-face a-face)
      (overlay-put t-overlay 'face a-face)
      (setq a-face b-face)))
  (u-color-cycle-delete-all-overlays min max))

(defun u-color-cycle-region-fade (min max &optional duration)
  "Start color fading in the region from MIN to MAX.
Optional argument DURATION specifies the fading speed. Larger values of
DURATION make the fading slower. Must be larger than 0; it defaults to 10."
  (interactive "r")
  (if (string-match "XEmacs" emacs-version)
      (zmacs-deactivate-region)
    (deactivate-mark))
  (unless duration
    (setq duration 10))
  (setq duration (max duration 1))
  (let (face-name (face-list (list)))
    (let ((to max)
	  from
	  t-overlay
	  t-face
	  new-face)
      (while (> to min)
	(setq from  (previous-single-property-change to 'face nil min))
	(setq t-face (or (get-text-property from 'face) 'default))
	(setq face-name (format "u-color-cycle-face-%s" t-face))
	(setq new-face (facemenu-get-face (intern face-name)))
	(unless new-face
	  (make-face new-face))
	(setq new-face (copy-face t-face new-face))
	(setq t-overlay (make-overlay from to))
	(overlay-put t-overlay 'u-color-cycle-overlay t)
	(overlay-put t-overlay 'face new-face)
	(add-to-list 'face-list new-face)
	;; make sure that fg and bg are set
	(or (face-background new-face)
	    (set-face-background new-face (face-background 'default)))
	(or (face-foreground new-face)
	    (set-face-foreground new-face (face-foreground 'default)))
	;; move on to next face
	(setq to from)))
    
    (let ((dur (1+ duration)))
      (while (sit-for u-color-cycle-sleep)
        (mapc
         (lambda (f)
           (let (r g b
                 (cur-bg (color-values (face-background f)))
                 (cur-fg (color-values (face-foreground f))))
             (setq r (/ (+ (nth 0 cur-bg) (* duration (nth 0 cur-fg))) dur))
             (setq g (/ (+ (nth 1 cur-bg) (* duration (nth 1 cur-fg))) dur))
             (setq b (/ (+ (nth 2 cur-bg) (* duration (nth 2 cur-fg))) dur))
             ;;(message "f %s, %s, %s -> %d %d %d" f cur-fg cur-bg r g b)
             (set-face-foreground f (format "#%02x%02x%02x" r g b))))
         face-list))))
  (u-color-cycle-delete-all-overlays min max))
  
;; ======================================================================
;;; window
;; ======================================================================

(defun u-color-cycle-window ()
  "Start color cycling for the whole window."
  (interactive)
  (u-color-cycle-region (window-start) (1- (window-end))))

(defun u-color-cycle-window-rainbow (&optional size)
  "Start color cycling for the whole window."
  (interactive)
  (u-color-cycle-region-rainbow (window-start) (1- (window-end)) size))

(defun u-color-cycle-window-swap ()
  "Start color cycling for the whole window."
  (interactive)
  (u-color-cycle-region-swap (window-start) (1- (window-end))))

(defun u-color-cycle-window-fade ()
  "Start color cycling for the whole window."
  (interactive)
  (u-color-cycle-region-fade (window-start) (1- (window-end))))


(provide 'u-color-cycle)
;;; u-color-cycle.el ends here
