(load "de-vars")
(setq load-path (cons (concat root-path "text") load-path))

(require 'markerpen)
(require 'underline)
(global-set-key "\C-c\C-u" 'underline-region)
