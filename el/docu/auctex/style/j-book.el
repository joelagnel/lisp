;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.2 1993/09/06 22:28:37 amanda Exp $

;;; Code:

(TeX-add-style-hook "j-book"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-book.el ends here
