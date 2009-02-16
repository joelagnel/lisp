;; eldoc
(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-mode-hook")) 'turn-on-eldoc-mode))
      '(emacs-lisp lisp-interaction ielm fundamental))

;;completion of lisp-symbols
(define-key emacs-lisp-mode-map [S-return] 'lisp-complete-symbol)
(global-set-key (kbd "M-<return>") 'complete-tag)



(load "de-vars")
(setq load-path (cons (concat root-path "modes/lisp/elisp") load-path))




;;find functions matching a pattern, that have interactive definitions

(require 'find-commands)


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(require 'dwim-find)

(require 'rfind-lib)



(require 'auto-show-doc)