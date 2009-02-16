(load "de-vars")
(setq load-path (cons (concat root-path "appearance") load-path))

(require 'color-theme)
(require 'color-theme-dark)
(require 'blinking-cursor)
(require 'header-line)
(require 'highline)

;;(require 'fade-out-kill-buffer)
(color-theme-dark)

(blinking-cursor-mode 1)

;; Turn on local highlighting for Dired (C-x d)
(add-hook 'dired-after-readin-hook 'highline-on)
;; Turn on local highlighting for list-buffers (C-x C-b)

(defadvice list-buffers (after highlight-line activate)
  (save-excursion
    (set-buffer "*Buffer List*")
    (highline-on)))
(global-set-key "\C-h\C-i" 'highline-on)
(global-set-key "\C-h\C-o" 'highline-off)



(setq
 menu-bar-mode t
 tool-bar-mode t
 frame-title-format "Emacs"
 inhibit-startup-message nil
 column-number-mode nil
 isearch-allow-scroll t
 transient-mark-mode t
 display-time-format "%H:%M")

;; enable visual feedback on selections

(tool-bar-mode nil)
(scroll-bar-mode nil)
(set-scroll-bar-mode nil)
(menu-bar-mode nil)
(mouse-wheel-mode t)
(global-font-lock-mode t)
(show-paren-mode 't)
(auto-compression-mode 1)
(display-time-mode 1)

(setq fancy-splash-image (expand-file-name "~/repository/lisp/el/appearance/mango.xpm"))

;; (add-to-list 'mode-line-modes "*LISP*   ")


;; (require 'highlight-tail)
;; (setq highlight-tail-colors '(("red" . 0)
;;                  ("black" . 25)
;;                  ("black" . 66)))


;; (setq highlight-tail-steps 500   highlight-tail-timer 0.01)

;; (setq highlight-tail-posterior-type 'const)

;; (highlight-tail-reload)





