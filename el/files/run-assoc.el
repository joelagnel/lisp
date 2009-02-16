;;; run-assoc.el -- Run program associated with a file.
;;
;; Filename: run-assoc.el
;; Description: Run program associated with a file.
;; Author: Emacs Wiki, pinetr2e
;;
;;; Commentary
;;
;; run-assoc.el was tested with GNU Emacs 21.4 on debian linux.
;;
;; The idea and some portion of codes were borrowed from 'w32-browser'
;; which is only for w32 and from 'TrivialMode'.
;;
;; It is very simple to use:
;;
;;		(require 'run-assoc)
;;  	(setq associated-program-alist 
;;  		  '(( "gnochm" "\\.chm$")
;;  			( "evince" "\\.pdf$")
;;				( "totem"  "\\.mp3$")
;;  			))
;;		
;;		Then, you can run the associated program in dired mode by
;;		Control-Return on a specific file or you can run the program
;;		directly by M-x run-associated-program
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



(defvar associated-program-alist nil
  "associated program list depending on file name regexp.")

(defun run-associated-program(file-name-arg)
  "Run program associated with file-name-arg.
   If no application is associated with file, then `find-file'."
  (interactive "ffile:")
  (let ((items associated-program-alist) item program regexp file-name 
		result 
		)
	(setq file-name (expand-file-name file-name-arg))
	(while (and (not result) items)
	  (setq item (car items))
	  (setq program (nth 0 item))
	  (setq regexp (nth 1 item))
	  (if (string-match regexp file-name) 
		  (progn
			(setq result (start-process program nil program file-name))
			))
	  (setq items (cdr items)))
	;; fail to run
	(unless result (find-file file-name))
	))

(defun dired-run-associated-program ()
  "Run program associated with current line's file.
   If file is a directory, then `dired-find-file' instead.  If no
   application is associated with file, then `find-file'."
(interactive)
(let ((file (dired-get-filename)))
  (if (file-directory-p file)
	  (dired-find-file)
	(run-associated-program file))))

(eval-after-load "dired"
'(progn
   (define-key dired-mode-map [C-return] 'dired-run-associated-program)
   (define-key dired-mode-map [menu-bar immediate dired-run-associated-program]
	 '("Open Associated Application" . dired-run-associated-program))))

(provide 'run-assoc)
