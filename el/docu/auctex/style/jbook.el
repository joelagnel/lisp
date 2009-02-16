;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.2 1993/09/06 22:28:42 amanda Exp $

;;; Code:

(TeX-add-style-hook "jbook"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; jbook.el ends here
