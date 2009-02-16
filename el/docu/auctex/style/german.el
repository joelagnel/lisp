;;; german.el - Setup AUC TeX for editing German text.

;; $Id: german.el,v 1.6 1994/08/20 05:44:08 amanda Exp $

;;; Commentary:
;;
;; `german.sty' use `"' to give next character an umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook "german"
 (function (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   (make-local-variable 'TeX-open-quote)
   (make-local-variable 'TeX-close-quote)
   (make-local-variable 'TeX-quote-after-quote)
   (setq TeX-quote-after-quote t)
   (setq TeX-open-quote "\"`")
   (setq TeX-close-quote "\"'")
   (run-hooks 'TeX-language-de-hook))))

;;; german.el ends here
