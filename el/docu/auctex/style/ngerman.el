;;; ngerman.el - Setup AUC TeX for editing German text.

;; $Id: ngerman.el,v 1.1 1999/10/26 20:05:18 abraham Exp $

;;; Commentary:
;;
;; `ngerman.sty' use `"' to give next character an umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook "ngerman"
 (function (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (make-local-variable 'TeX-quote-after-quote)
   (setq TeX-quote-after-quote t)
   (setq TeX-open-quote "\"`")
   (setq TeX-close-quote "\"'")
   (run-hooks 'TeX-language-de-hook))))

;;; ngerman.el ends here
