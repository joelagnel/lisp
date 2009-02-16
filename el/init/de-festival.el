(load "de-vars")
(setq load-path (cons (concat root-path "audio") load-path))

;(require 'festival)

(autoload 'say-minor-mode "festival" "Menu for using Festival." t)
(say-minor-mode t)


(defalias 'say 'festival-say-string)

