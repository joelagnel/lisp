;;; initialize ruby mode

(load "de-vars")
(setq load-path (cons (concat root-path "modes/ruby") load-path))

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

(require 'ruby-mode)

(defun my-ruby-mode-hook ()
;;   (pabbrev-mode t)
;;   (ruby-electric-mode t)

(define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer))

(defun disable-flymake ()
        (flymake-mode nil))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)


(defun my-ruby-indent ()
  (setq tab-width 2
        rb-indent-offset 2
        indent-tabs-mode t
        rb-smart-indentation nil))

(add-hook 'ruby-mode-hook 'my-ruby-indent)
(add-hook 'ruby-mode-hook 'pretty-greek)

(require 'pabbrev)
(require 'ruby-electric)

(defun ruby-eval-buffer () (interactive)
       "Evaluate the buffer with ruby."
       (shell-command-on-region (point-min) (point-max) "ruby18"))


(require 'rinari)
(setq auto-mode-alist (cons '("\\.rhtml\\'" . rhtml-mode) auto-mode-alist))
