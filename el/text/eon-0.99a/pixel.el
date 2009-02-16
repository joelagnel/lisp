;;; pixel.el --- wysiwyg xpm editor

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto1138@gmail.com>
;; Keywords: multimedia
;; Package-Version: 0.2
;; Version: $Id: pixel.el,v 1.9 2007/09/11 09:30:12 dto Exp dto $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is an example program for eon. It is quite rudimentary at the
;; moment, but will be developed into a full-fledged XPM pixel art editor.
;; To try it out, do M-x pixel-demo RET
;;
;; C-t   changes the "tool". only :set-color is supported right now.
;; M-t   changes the "tool-data". this string may be any color 
;;       supported by emacs.
;; t     apply the current tool and tool-data to the selected cell
;;       (in other words, paint a pixel)

;;
;; You can also see the more detailed demo at the end of this file.

;;; Code:

(require 'cell)

;;;; Pixels
   
(define-prototype :pixel "A colored pixel."
  ((:protocols (:default-value (:cell)))
   (:color (:documentation "String name of the pixel's color." :default-value "black"))))

(define-method :calculate :pixel ()
  "Produce a colored cell label."
  (with-slots (color label)
    (setf label
	  (apply #'propertize 
		 `(" " face (:background ,color)
		   display ,(cons 'space '(:width (5 . mm)
						  :height (5 . mm))))))))

(define-method :set-color :pixel (data)
  "Set the color of this pixel to NAME."
  (setf (@ self :color) data))

;;;; Pixel sheets (based on cell-mode)

(define-prototype (:pixel-sheet :cell-sheet)
  "A grid of pixels suitable for XPM editing."
  ((:xpm-buffer (:documentation "Buffer where XPM is cached."))
   (:xpm-name (:documentation "Name to use in XPM file."))))

(define-method :initialize :pixel-sheet (rows columns name)
  "Create a blank pixel sheet of size ROWS x COLUMNS."
  (with-slots (raw-display-p tool tool-data)
    (super>> :initialize :rows rows :columns columns :name name)
    (self>> :clone-fill :with-object :pixel)
    (setf raw-display-p t
	  tool :set-color
	  tool-data "blue")))

;;;; Making XPMs 

(defsubst xpm-string (string)
  (format "%S,\n" string))

(defun insert-xpm (name width height ncolors chars-per-pixel colors pixel-rows)
  (apply #'insert
	 (append 
	  (format "/* XPM */\nstatic char* %s[] = {\n" name)
	  (xpm-string (format "%S %S %S %S" width height ncolors chars-per-pixel))
	  (mapcar #'xpm-string colors)
	  (mapcar #'xpm-string  
		  (mapcar (lambda (pixels)
			    (apply #'concat pixels))
			  pixel-rows))
	  (list "};\n"))))

(defun color-to-hex (color-string)
  (if (string-match "#" color-string)
      color-string
    (destructuring-bind (red green blue)
	(mapcar (lambda (c)
		  (lsh c -8))
		(color-values color-string))
      (format "%02x%02x%02x" red green blue))))

(define-method :make-xpm :pixel-sheet ()
  "Create an XPM in a new buffer. Return the buffer."
  (with-slots (grid sheet-name)
    (with-current-buffer (get-buffer-create (concat sheet-name "::XPM"))
      (delete-region (point-min) (point-max))
      (let ((colors (make-hash-table :test #'equal))	    
	    (rows (grid-rows grid))
	    (columns (grid-columns grid))
	    pixels pixel-rows cell color)
	(dotimes (row rows)
	  (setf pixels nil)
	  (dotimes (column columns)
	    (setf cell (gref grid row column))
	    (setf color (color-to-hex (@ cell :color)))
	    (puthash color t colors)
	    (push color pixels))
	  (push (nreverse pixels) pixel-rows))
	(let ((ncolors (hash-table-count colors))
	      (chars-per-pixel 6)
	      color-strings)
	  (maphash (lambda (k ignore)
		     (push (format "%s\tc %s" k (concat "#" k))
			   color-strings))
		   colors)
	  (insert-xpm "emacs_xpm" columns rows
		      ncolors 6 (nreverse color-strings)
		      (nreverse pixel-rows))))
      (current-buffer))))

;;;; Demonstration

;; To try it out, do M-x pixel-demo RET.
;; You can then use C-h m to see what keybindings are available.
;; 
;; To see things operate in more detail, see the quoted progn below.

;;;###autoload
(defun pixel-demo ()
  "Open a simple pixel sheet."
  (interactive)
  (let ((pix (eon-clone :pixel-sheet 
			:rows 16 :columns 16 :name "pixel-demo")))
    (>> pix :calculate)
    (>> pix :render)
    (>> pix :show)))

;; '(progn 

;;    ;; You can execute the sexps below one at a time by placing point
;;    ;; at the end of the sexp and pressing C-x C-e
;;    ;;
;;    ;; You should have loaded both the present file, and cell.el
;;    ;; by this point. Otherwise you will get errors.

;;    (eon-dump-object :cell-sheet)
;;    (eon-dump-object :pixel-sheet)

;;    ;; Let's make a new object and then inspect it.

;;    (defvar s)
;;    (setf s (eon-clone :pixel-sheet :rows 20 :columns 20 :name "Mona Lisa"))
;;    (eon-dump-object s)

;;    ;; We can freely add new slots.

;;    (setf (@ s :foo) 'bar)
;;    (eon-dump-object s)

;;    ;; Get the pixel sheet on the screen.

;;    (>> s :calculate)
;;    (>> s :render)
;;    (>> s :show)

;;    ;; Switch to the displayed buffer and try painting some pixels.
;;    ;;
;;    ;; C-t   changes the "tool". only :set-color is supported right now.
;;    ;; M-t   changes the "tool-data". this string may be any color 
;;    ;;       supported by emacs.
;;    ;; t     apply the current tool and tool-data to the selected cell
;;    ;;       (in other words, paint a pixel)

;;    ;; Now we set some pixels programatically.

;;    (let ((c (>> s :cell-at :row 3 :column 3)))
;;      (setf (@ c :color) "cyan"))
;;    (let ((c (>> s :cell-at :row 7 :column 5)))
;;      (setf (@ c :color) "magenta"))
;;    (let ((c (>> s :cell-at :row 4 :column 6)))
;;      (setf (@ c :color) "yellow"))
;;    (let ((c (>> s :cell-at :row 10 :column 2)))
;;      (setf (@ c :color) "red"))
;;    (let ((c (>> s :cell-at :row 9 :column 2)))
;;      (setf (@ c :color) "blue"))
;;    (>> s :calculate)
;;    (>> s :render)
;;    ;;
;;    ;; Now we can export our image to XPM!
;;    ;; 
;;    (display-buffer (>> s :make-xpm)))
;;    ;; 
;;    ;; 


(provide 'pixel)
;;; pixel.el ends here

