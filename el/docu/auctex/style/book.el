;;; book.el - Special code for book style.

;; $Id: book.el,v 1.4 1993/09/06 22:28:26 amanda Exp $

;;; Code:

(TeX-add-style-hook "book"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; book.el ends here
