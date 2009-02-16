(load "de-vars")
(require 'tramp)

(setq load-path (cons (concat root-path "modes/net/tramp/lisp") load-path))
(setq load-path (cons (concat root-path "modes/net/tramp/contrib") load-path))
(setq tramp-default-method "ssh")
;; (add-to-list 'Info-default-directory-list "modes/net/tramp/info/")


