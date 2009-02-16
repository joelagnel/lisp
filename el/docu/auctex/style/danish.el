;;; danish.el - Setup AUC TeX for editing Danish text.

;; $Id: danish.el,v 1.2 1999/12/20 18:09:07 abraham Exp $

;;; Code:

(TeX-add-style-hook "danish"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (setq TeX-open-quote "\"`")
   (setq TeX-close-quote "\"'")
   (run-hooks 'TeX-language-dk-hook))))

;;; danish.el ends here
