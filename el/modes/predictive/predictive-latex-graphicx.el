
;;; predictive-latex-graphicx.el --- predictive mode LaTeX graphicx
;;;                                  package support


;; Copyright (C) 2004-2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: predictive, latex, package, graphicx
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion package.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.1
;; * initial version


;;; Code:

(require 'predictive-latex)
(provide 'predictive-latex-graphicx)

;; add load and unload functions to alist
;;(assoc-delete-all "smartref" predictive-latex-usepackage-functions)
(push '("graphicx" predictive-latex-load-graphicx
	predictive-latex-unload-graphicx)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-graphicx ()
  ;; Load regexp
  (auto-overlay-load-compound-regexp
   `(start "\\\\includegraphics\\(\\[.*?\\]\\)?{"
	   (dict . t) (priority . 2)
	   (face . (background-color . ,predictive-latex-debug-color)))
   'predictive 'brace t 'graphicx)
)



(defun predictive-latex-unload-graphicx ()
  ;; Unload sref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'graphicx)
)

;;; predictive-latex-graphicx ends here
