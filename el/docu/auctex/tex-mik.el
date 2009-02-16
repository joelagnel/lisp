;;; tex-mik.el --- MikTeX support for AUC TeX.
;;
;; Copyright (C) 1999, 2000, 2001 Per Abrahamsen 

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.dk>
;; Version: 11.14
;; Keywords: wp

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This file contains variables customized for MikTeX.

;;; Code:

;; The MikTeX commands.
(setq TeX-command-list
  (list (list "TeX" "tex \\nonstopmode\\input %t" 'TeX-run-TeX nil t)
	(list "LaTeX" "%l \\nonstopmode\\input{%t}" 'TeX-run-LaTeX nil t)
	(list "LaTeX PDF" "pdflatex \\nonstopmode\\input{%t}" 'TeX-run-LaTeX nil t)
	(list "View" "%v" 'TeX-run-discard nil nil)
	(list "Print" "gsview32 %f" 'TeX-run-command t nil)
	(list "File" "dvips %d -o %f " 'TeX-run-command t nil)
	(list "BibTeX" "bibtex %s" 'TeX-run-BibTeX nil nil)
	(list "Index" "makeindex %s" 'TeX-run-command nil t)
	(list "Check" "lacheck %s" 'TeX-run-compile nil t)
	(list "Other" "" 'TeX-run-command t t)))

;; DVI to source correspondence (stolen from the German magazine c't).
(setq LaTeX-command-style '(("." "latex --src-specials")))
(setq TeX-view-style '(("^a5$" "yap %d -paper a5")
		       ("^landscape$" "yap %d -paper a4r -s 4")
		       ("^epsf$" "gsview32 %f")
		       ("." "yap -1 -s%n%b %d")))

(provide 'tex-mik)

;;; tex-mik.el ends here
