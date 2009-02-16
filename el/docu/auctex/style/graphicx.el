;;; -*- emacs-lisp -*-
;;; graphicx.el - Support for the graphicx style option.

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Ryuichi Arafune <arafune@debian.org>
;; Created: 1999/3/20
;; Version: $Id: graphicx.el,v 1.8 2001/11/01 21:35:54 ataka Exp $
;; Keywords: tex

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
;;  This package suppors the includegraphcics macro in graphicx style (LaTeX2e)
;;  If you want to use bb, angle or totalheight as arguments of includegraphics,
;;  set TeX-include-graphics-simple nil (default t).

;; Acknowledgements
;;  Dr. Thomas Baumann <thomas.baumann@ch.tum.de>
;;  David Kastrup <David.Kastrup@t-online.de>
;;  Masayuki Akata <ataka@milk.freemail.ne.jp>
;;; Code:

(TeX-add-style-hook 
 "graphicx"
 (function (lambda () 
	     (TeX-add-symbols
	      "protect" "clip" "keepaspectratio" 
	      "width" "height" "bb" "angle" "totalheight"
	      '("includegraphics" TeX-arg-includegraphics)))))

(defvar TeX-include-graphics-simple t
  "if nil, AUC TeX asks the following arguments: Bounding box (bb), Rotation angle (angle), Total height (totalheight) in addition to the normal arguments.")

(defvar TeX-default-unit-for-image "cm"
  "default unit for includegraphics command")

(defun TeX-arg-includegraphics (optional)
  "Ask for file name (eps file only), width, height, keepaspectratio, and clip. Insert includegraphics macro"
  (let ((maybe-left-brace "[") (maybe-comma "")
	(psfile 
	 (if current-prefix-arg
	     (read-file-name "Image file: " "" "" t)
	   (cond
	    ;; dvipdfm support.
	    ((member "dvipdfm" TeX-active-styles)
	     (completing-read "Image file: "
			      (mapcar 'list (directory-files "./" nil
					       "\\.eps$\\|\\.jpe?g$\\|\\.pdf$\\|\\.png$" nil))
			      nil nil
			      (caar (mapcar 'list (directory-files "./" nil
					       "\\.eps$\\|\\.jpe?g$\\|\\.pdf$\\|\\.png$" nil)))))
	    (t (completing-read "PS (eps only) file: "
				(mapcar 'list (directory-files "./" nil "\\.eps$" nil))
				nil nil
				(caar (mapcar 'list (directory-files "./" nil "\\.eps$" nil))))))))
	(figwidth (read-input (concat "Figure width ("TeX-default-unit-for-image"): ")))
	(figheight (read-input (concat "Figure height ("TeX-default-unit-for-image"): ")))
	(keepaspectratio (y-or-n-p "Keep Aspectratio ? "))
	(clip (y-or-n-p "Clipping figure ? ")))
    (when (not (zerop (length figwidth)))
      (insert maybe-left-brace maybe-comma "width=" (car (TeX-string-divide-nuber-unit figwidth))
	      (if (zerop (length (car (cdr (TeX-string-divide-nuber-unit figwidth)))))
		  TeX-default-unit-for-image (car (cdr (TeX-string-divide-nuber-unit figwidth)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length figheight)))
      (insert maybe-left-brace maybe-comma "height=" (car (TeX-string-divide-nuber-unit figheight))
	      (if (zerop (length (car (cdr (TeX-string-divide-nuber-unit figheight)))))
		  TeX-default-unit-for-image (car (cdr (TeX-string-divide-nuber-unit figheight)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when keepaspectratio
      (insert maybe-left-brace maybe-comma "keepaspectratio")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when clip
      (insert maybe-left-brace maybe-comma "clip")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
;;; Insert more arguments when TeX-include-graphics-simple is nil
    (if (not TeX-include-graphics-simple)
	(let ((angle (read-input "Rotation angle: "))
	      (totalheight (read-input (concat "Total Height ("TeX-default-unit-for-image"): ")))
	      (bbset-flag (y-or-n-p "Set Bounding Box : "))
	      (bbllx nil) (bblly nil) (bburx nil) (bbury nil))
	  (when bbset-flag
	    (setq bbllx (read-input "Bounding Box Lower Left x: "))
	    (setq bblly (read-input "Bounding Box Lower Left y: "))
	    (setq bburx (read-input "Bounding Box Upper right x: "))
	    (setq bbury (read-input "Bounding Box Upper right y: "))
	    (insert maybe-left-brace maybe-comma 
		    "bb=" bbllx " " bblly " " bburx " " bbury)
	    (setq maybe-comma ",")
	    (setq maybe-left-brace ""))
	  (when (not (zerop (length angle)))
	    (insert maybe-left-brace maybe-comma "angle=" angle)
	    (setq maybe-comma ",")
	    (setq maybe-left-brace ""))
	  (when (not (zerop (length totalheight)))
	    (insert maybe-left-brace maybe-comma "totalheight=" (car (TeX-string-divide-nuber-unit totalheight))
		    (if (zerop (length (car (cdr (TeX-string-divide-nuber-unit totalheight)))))
			TeX-default-unit-for-image (car (cdr (TeX-string-divide-nuber-unit totalheight)))))
	    (setq maybe-left-brace ""))))
;;;
    (if (zerop (length maybe-left-brace))
	(insert "]"))
    (TeX-insert-braces 0)
    (insert psfile)
    ))

(defun TeX-string-divide-nuber-unit (string)
 (if (string-match "[0-9]*\\.?[0-9]+" string)
      (list (substring string 0 (string-match "[^.0-9]" string))
	    (substring string (if (string-match "[^.0-9]" string) 
				  (string-match "[^.0-9]" string) 
				(length string))))
   (list "" string)))

						 
;;; graphicx.el ends here
