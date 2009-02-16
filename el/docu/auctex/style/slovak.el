;;; slovak.el - Setup AUC TeX for editing Slovak text.

(TeX-add-style-hook "slovak"
 (function (lambda ()
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (setq TeX-open-quote "\\uv{")
   (setq TeX-close-quote "}")
   (run-hooks 'TeX-language-sk-hook))))
