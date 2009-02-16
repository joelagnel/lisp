;;; etalk-lgame --- game directory search routines
;;
;; Copyright (C) 1994 Free Software Foundation
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
;; along with this program; if not, you can either send email to this
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.

;;; Commentary:
;;   This file contains the stuff needed to look at the load path
;; and find all "games" that might be used within tyrant-mode
;;
;; $Id: etalk-lgame.el,v 1.3 1997/08/09 12:46:58 zappo Exp $
;;

;;; Code:
(defun etalk-search-load-path-for-games (aiflag)
  "Search each member of `load-path' for a games directory.
Append all files therin into tuples of symbols.  Append AIFLAG
to each list member."
  (let ((paths load-path)		;temp path list
	(syms nil))			;new list of symbols
    (while paths
      ;; 9/6/94 - Added check for nil in list of paths.
      (if (and paths (file-exists-p (car paths)))
	  (if (directory-files (car paths) nil "^games$")
	      (setq syms (append syms (etalk-read-symbols
				       (car paths) aiflag)))))
      (setq paths (cdr paths)))
    syms))

(defun etalk-read-symbols (path aiflag)
  "Search PATH for games from the .el files to the game tuples.
Use AIFLAG to determine if we are looking for -ai extensions on the
file names."
  (let ((syms nil)
	(dlist (directory-files
		(concat path
			(if (/= (aref path (1- (length path))) ?/) "/")
			"games")
		;; 9/6/94 - need to quote in .
		nil "\\.el"))
	(omd (match-data)))
    (while dlist
      (if (string-match "-ai" (car dlist))
	  (if (and aiflag (string-match "\\(-ai\\)\\.el" (car dlist)))
	      (let* ((newsym (substring (car dlist) 0 (match-beginning 1)))
		     (newaisym (substring (car dlist) 0 (match-end 1)))
		     (newid (read newsym))
		     (newai (read newaisym)))
		(setq syms (cons (list (etalk-casafy-game-string newsym)
				       newid newai)
				 syms))))
	(if (and (not aiflag)
		 (not (string-match "-lib\\.el" (car dlist)))
		 (string-match "\\(\\.el\\)" (car dlist)))
	    (let* ((newsym (substring (car dlist) 0 (match-beginning 1)))
		   (newid (read newsym))
		   (newstr (etalk-casafy-game-string newsym)))
	      ;; 9/6/94 make sure we don't get duplicates
	      (if (not (assoc newstr syms))
		  (setq syms (cons (cons newstr newid) syms))))))
      (setq dlist (cdr dlist)))
    (store-match-data omd)
    syms))

(defun etalk-casafy-game-string (str)
  "Convert the file name STR into a readable word.
For example, turn word-thing into Word Thing."
  (let ((l (length str))
	(tmp 0))
    (while (< tmp l)
      (if (= (aref str tmp) ?-)
	  (aset str tmp ? ))
      (setq tmp (1+ tmp))))
  (capitalize str))

(defvar etalk-legal-multiuser-functions (etalk-search-load-path-for-games nil)
  "Association list of programs which are 2 or more person talk utilities." )

(defvar etalk-legal-tyrant-ai-functions  (etalk-search-load-path-for-games t)
  "Association list programs which are 2 person games whith AI extensions.")

(provide 'etalk-lgame)
;;; etalk-lgame ends here
