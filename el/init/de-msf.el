
(load "de-vars")
(setq load-path (cons (concat root-path "modes") load-path))

(require 'msf-abbrev)


;; ensure abbrev mode is always on
(setq-default abbrev-mode t)

;; do not bug me about saving my abbreviations
(setq save-abbrevs nil)

;; load up modes I use
(require 'cc-mode)
;(require 'perl-mode)
(require 'cperl-mode)
;(require 'sh-script)
(require 'shell)
;; (require 'tex-site) ;; I use AUCTeX
;; (require 'latex)    ;; needed to define LaTeX-mode-hook under AUCTeX
;; (require 'tex)      ;; needed to define TeX-mode-hook under AUCTeX
;; ;; (require 'python)   ;; I use python.el from Emacs CVS, uncomment if you do also

;; load up abbrevs for these modes
(require 'msf-abbrev)
(setq msf-abbrev-verbose t) ;; optional


(setq msf-abbrev-root (concat root-path "modes/mode-abbrevs"))

(global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)
(global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
(msf-abbrev-load)

(provide 'de-msf)

