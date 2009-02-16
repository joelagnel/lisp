;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.2 1993/09/06 22:28:39 amanda Exp $

;;; Code:

(TeX-add-style-hook "j-report"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))

;;; j-report.el ends here
