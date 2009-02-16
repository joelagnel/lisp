;;; article.el - Special code for article style.

;; $Id: article.el,v 1.3 1993/09/06 22:28:24 amanda Exp $

;;; Code:

(TeX-add-style-hook "article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; article.el ends here
