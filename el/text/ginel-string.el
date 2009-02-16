;;; ginel-string.el --- GINEL string manipulation functions

;; Copyright (C) 1998 Stefan Hornburg

;; Author: Stefan Hornburg <racke@gundel.han.de>
;; Maintainer: Stefan Hornburg <racke@gundel.han.de>
;; Version: 0.2.0
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * User functions
;; ginel-string-split STRING REGEXP
;;  Splits STRING into list of strings and returns the result.
;;  Anything matching REGEXP is taken to be a field delimiter.
;; ginel-string-replace STRING REGEXP REPLACEMENT
;;  Replaces all occurences of REGEXP within STRING by REPLACEMENT. 
;; ginel-string-rtrim STRING
;;  Removes all blanks from the end of STRING and returns the result.
;;
;; * Splitting Strings
;; (ginel-string-split "1.13.r2" "\\.")
;;   => ("1" "13" "r2")
;; (ginel-string-split "::a:b::C:" ":")
;;   => ("" "" "a" "b" "" "C" "")

;;; Change log:
;; Thu Dec  3 02:53:50 1998  Stefan Hornburg  <racke@gundel.han.de>
;;  * new function `ginel-string-split'
;; Thu Dec  3 01:23:21 1998 Stefan Hornburg  <racke@gundel.han.de>
;;  * ginel-string.el v0.1.0 released
;; Wed Nov 25 23:14:09 1998 Dave Love <d.love@dl.ac.uk>
;;  * simple regexp based implementation of `ginel-string-rtrim'
;; Wed Oct 21 09:55:56 1998 Stefan Hornburg	<racke@gundel.han.de>
;;  * ginel-string.el v0.0.0 released.

;;; Code: 

;;;###autoload
(defun ginel-string-split (string regexp)
  "Splits STRING into list of strings and returns the result.
Anything matching REGEXP is taken to be a field delimiter."
  (save-match-data
	(let (list)
	  (while (string-match regexp string)
		(setq list (cons (substring string 0 (match-beginning 0)) list))
		(setq string (substring string (match-end 0))))
	  (if (length string)
		  (setq list (cons string list)))
	  (reverse list))))

;;;###autoload
(defun ginel-string-replace (string regexp replacement)
  "Replaces all occurences of REGEXP within STRING by REPLACEMENT."
  (save-match-data 
	(let ((pos 0) (newstring ""))
	  (while (string-match regexp string pos) 
		(setq newstring (concat newstring 
								(substring string pos (match-beginning 0))
								replacement))
		(setq pos (match-end 0)))
	  (concat newstring (substring string pos)))))

;;;###autoload
(defun ginel-string-rtrim (string)
  "Removes all blanks from the end of STRING and returns the result."
  (save-match-data
	(substring string 0 (string-match " *\\'" string))))

(provide 'ginel-string)

;;; Local Variables:
;;; generated-autoload-file: "ginel.el"
;;; End:

;;; ginel-string.el ends here
