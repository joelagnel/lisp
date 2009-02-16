(setq load-path (cons (concat root-path "workflow") load-path))

(require 'org)

(autoload 'org-mode "org" "Org mode" t)

(autoload 'org-diary "org" "Diary entries from Org mode")

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key global-map "\C-cl" 'org-store-link)