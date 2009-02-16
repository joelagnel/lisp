;;; czech.el - Setup AUC TeX for editing Czech text.

(TeX-add-style-hook "czech"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (setq TeX-open-quote "\\uv{")
   (setq TeX-close-quote "}")
   (run-hooks 'TeX-language-cz-hook))))

