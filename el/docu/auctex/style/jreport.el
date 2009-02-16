;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.2 1993/09/06 22:28:43 amanda Exp $

;;; Code:

(TeX-add-style-hook "jreport"
 (function (lambda () (setq LaTeX-largest-level
			    (LaTeX-section-level "chapter")))))


;;; jreport.el ends here
