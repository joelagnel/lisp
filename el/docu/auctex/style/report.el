;;; report.el - Special code for report style.

;; $Id: report.el,v 1.2 1993/09/06 22:28:52 amanda Exp $

;;; Code:

(TeX-add-style-hook "report"
 (function (lambda () 
  (setq LaTeX-largest-level (LaTeX-section-level "chapter")))))

;;; report.el ends here
