;;; swedish.el - Setup AUC TeX for editing Swedish text.

;; $Id: swedish.el,v 1.2 1996/07/18 09:23:40 abraham Exp $

;;; Commentary:
;;
;; Apparently the Swedes use ''this style'' quotations.

(TeX-add-style-hook "swedish"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (setq TeX-open-quote "''")
   (run-hooks 'TeX-language-sv-hook))))
