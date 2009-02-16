;;; gnus-fun.el --- various frivoluos extension functions to Gnus
;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defcustom gnus-x-face-directory (expand-file-name "x-faces" gnus-directory)
  "*Directory where X-Face PBM files are stored."
  :group 'gnus-fun
  :type 'directory)

(defcustom gnus-convert-pbm-to-x-face-command "pbmtoxbm '%s' | compface"
  "Command for converting a PBM to an X-Face."
  :group 'gnus-fun
  :type 'string)

(defcustom gnus-convert-image-to-x-face-command "giftopnm '%s' | ppmnorm 2>/dev/null | pnmscale -width 48 -height 48 | ppmtopgm | pgmtopbm | pbmtoxbm | compface"
  "Command for converting a GIF to an X-Face."
  :group 'gnus-fun
  :type 'string)

;;;###autoload
(defun gnus-random-x-face ()
  "Insert a random X-Face header from `gnus-x-face-directory'."
  (interactive)
  (when (file-exists-p gnus-x-face-directory)
    (let* ((files (directory-files gnus-x-face-directory t "\\.pbm$"))
	   (file (nth (random (length files)) files)))
      (when file
	(shell-command-to-string
	 (format gnus-convert-pbm-to-x-face-command file))))))

;;;###autoload
(defun gnus-x-face-from-file (file)
  "Insert an X-Face header based on an image file."
  (interactive "fImage file name:" )
  (when (file-exists-p file)
    (shell-command-to-string
     (format gnus-convert-image-to-x-face-command file))))

(defun gnus-convert-image-to-gray-x-face (file depth)
  (let* ((mapfile (make-temp-name (expand-file-name "gnus." mm-tmp-directory)))
	 (levels (expt 2 depth))
	 (step (/ 255 (1- levels)))
	 color-alist bits bits-list mask pixel x-faces)
    (with-temp-file mapfile
      (insert "P3\n")
      (insert (format "%d 1\n" levels))
      (insert "255\n")
      (dotimes (i levels)
	(insert (format "%d %d %d\n"
			(* step i) (* step i) (* step i)))
	(push (cons (* step i) i) color-alist)))
    (when (file-exists-p file)
      (with-temp-buffer
	(insert (shell-command-to-string (format "giftopnm '%s' | ppmnorm 2>/dev/null | pnmscale -width 48 -height 48 | ppmquant -map %s 2>/dev/null | ppmtopgm | pnmnoraw"
			       file mapfile)))
	(goto-char (point-min))
	(forward-line 3)
	(while (setq pixel (ignore-errors (read (current-buffer))))
	  (push (cdr (assq pixel color-alist)) bits-list))
	(setq bits-list (nreverse bits-list))
	(dotimes (bit-number depth)
	  (setq mask (expt 2 bit-number))
	  (with-temp-buffer
	    (insert "P1\n48 48\n")
	    (dolist (bits bits-list)
	      (insert (if (zerop (logand bits mask)) "0 " "1 ")))
	    (shell-command-on-region
	     (point-min) (point-max)
	     "pbmtoxbm | compface"
	     (current-buffer) t)
	    (push (buffer-string) x-faces))))
      (dotimes (i (length x-faces))
	(insert (if (zerop i) "X-Face:" (format "X-Face-%s:" i))
		(nth i x-faces))))
    (delete-file mapfile)))

;;;###autoload
(defun gnus-convert-gray-x-face-to-xpm (faces)
  (let* ((depth (length faces))
	 (scale (/ 255 (1- (expt 2 depth))))
	 bit-list bit-lists pixels pixel)
    (dolist (face faces)
      (with-temp-buffer
	(insert face)
	(shell-command-on-region
	 (point-min) (point-max)
	 "uncompface -X | xbmtopbm | pnmnoraw"
	 (current-buffer) t)
	(goto-char (point-min))
	(forward-line 2)
	(while (not (eobp))
	  (cond
	   ((eq (following-char) ?0)
	    (push 0 bit-list))
	   ((eq (following-char) ?1)
	    (push 1 bit-list)))
	  (forward-char 1)))
      (push bit-list bit-lists))
    (dotimes (i (* 48 48))
      (setq pixel 0)
      (dotimes (plane depth)
	(setq pixel (+ (* pixel 2) (nth i (nth plane bit-lists)))))
      (push pixel pixels))
    (with-temp-buffer
      (insert "P2\n48 48\n255\n")
      (dolist (pixel pixels)
	(insert (number-to-string (* scale pixel)) " "))
      (shell-command-on-region
       (point-min) (point-max)
       "ppmtoxpm 2>/dev/null"
       (current-buffer) t)
      (buffer-string))))

;;;###autoload
(defun gnus-convert-gray-x-face-region (beg end)
  "Convert the X-Faces in region to a PPM file."
  (interactive "r")
  (let ((input (buffer-substring beg end))
	faces)
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (not (eobp))
	(save-restriction
	  (mail-header-narrow-to-field)
	  (push (mail-header-field-value) faces)
	  (goto-char (point-max)))))
    (gnus-convert-gray-x-face-to-xpm faces)))

(provide 'gnus-fun)

;;; gnus-fun.el ends here
