;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.3 1993/09/06 22:28:36 amanda Exp $

;;; Code:

(TeX-add-style-hook "j-article"
 (function (lambda ()
  (setq LaTeX-largest-level (LaTeX-section-level "section")))))

;;; j-article.el ends here
