(load "de-vars")
(setq load-path (cons (concat root-path "modes/rails") load-path))

(defun my-ruby-indent ()
  (setq tab-width 2
        rb-indent-offset 2
        indent-tabs-mode t
        rb-smart-indentation t)
        (pabbrev-mode t))

(add-hook 'rails-mode-hook 'my-ruby-indent)

(require 'rails)
