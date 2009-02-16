(load "de-vars")
(setq load-path (cons (concat root-path "modes/lisp/slime") load-path))

(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup :autodoc t)


(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(define-key slime-mode-map [S-return] 'slime-complete-symbol)
(define-key slime-mode-map "\C-cs" 'slime-selector)

(setq
 lisp-indent-function 'common-lisp-indent-function
 slime-complete-symbol-function 'slime-fuzzy-complete-symbol
 slime-startup-animation t)

;; (setq common-lisp-hyperspec-root "~/lisp/docs/HyperSpec/")
;; (setq common-lisp-hyperspec-symbol-table "~/lisp/docs/HyperSpec/Data/Map_Sym.Txt")

;; ;;firefox browser

(setq browse-url-generic-program (executable-find "flock")
      browse-url-browser-function 'browse-url-generic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
