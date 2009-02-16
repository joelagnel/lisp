;; .emacs

;; (setq-default
;;  fill-column 70
;;  indent-tabs-mode nil
;;  major-mode 'text-mode
;;  tab-width 4
;;  auto-fill-function 'do-auto-fill)
(setq load-path (cons "/home/joel/repository/lisp/el/init/" load-path))
(load "de-vars")

(setq load-path (cons (concat root-path "init") load-path))

;;(load "de-festival")
;;(say "Loading Emacs          ")

(load "de-appearance")
(load "de-buffer")
(load "de-c")
(load "de-elisp")
(load "de-emms")
;(load "de-eshell")
;(load "de-guile")
(load "de-files")
(load "de-modes")
(load "de-msf")
;; (load "de-perl")
(load "de-shell")
;;(load "de-slime")
(load "de-symbols")
(load "de-sys")
(load "de-text")
(load "de-ruby")
(load "de-rails")
(load "de-jabber")
(load "de-tramp")
(load "de-tabbar")

; Joel's key bindings
(global-set-key [f6] 'tabbar-forward)
(global-set-key [f5] 'tabbar-backward)
(global-set-key (kbd "C-;") 'color-theme-comidia)
(global-set-key "\C-c\C-l" `set-default-font)
(global-set-key "\C-x\C-k" 'clipboard-kill-region)
;;(global-set-key "\C-x\C-f" 'lusty-file-explorer)
(global-set-key (kbd "C-x C-1") 'eval-expression)
;;(global-set-key "\C-x\C-i" 'irc)

(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "<f4>") 'speedbar-get-focus) ;jump to the speedbar.
(global-set-key (kbd "C-`") 'capitalize-word)
(global-set-key [f2] 'goto-line)
(global-set-key "\C-cc" `compile)
(global-set-key "\C-cl" `goto-line)
(global-set-key "\C-cp" `list-processes)
(global-set-key "\C-cr" `toggle-read-only)
(global-set-key "\C-cs" `sort-lines)

(require 'browse-url)
(global-set-key "\C-c\C-m" 'browse-url-at-point)

;;browser

(setq browse-url-generic-program (executable-find "firefox")
      browse-url-browser-function 'browse-url-generic)

(global-set-key "\C-cu." 'browse-url-at-point)
(global-set-key "\C-cub" 'browse-url-of-buffer)
(global-set-key "\C-c\C-zr" 'browse-url-of-region)
(global-set-key "\C-c\C-zu" 'browse-url)
(global-set-key "\C-cuf" 'browse-url-of-file)

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-cuf" 'browse-url-of-dired-file)))

(if (boundp 'browse-url-browser-function)
    (global-set-key "\C-cuu" browse-url-browser-function)
  (eval-after-load
   "browse-url"
   '(global-set-key "\C-cuu" browse-url-browser-function)))


(setq mouse-yank-at-point 't)

;;woman-setting
(setq woman-use-own-frame nil)

;;(say "Emacs is now loaded. Happy Hacking ")
(defconst animate-n-steps 3)
(defun emacs-reloaded ()
  (animate-string "Initialization successful, welcome to the most powerful editor in the World"  0 0)
  (newline-and-indent)  (newline-and-indent))

(add-hook 'after-init-hook 'emacs-reloaded)

(message "Emacs Reloaded")

(defun exit () (interactive)
      (if (yes-or-no-p "Are you sure you want to exit?")
          (eval (kill-emacs)))
      (message "back to hacking!"))

(provide 'de-emacs)

;; (load-file "/home/joel/repository/lisp/el/modes/cedet/common/cedet.el")
;; (load-file "/home/joel/repository/lisp/el/modes/cedet/common/working.el")

;; Load CEDET
(load-file "~/repository/cedet-1.0pre4/common/cedet.el")
(load-file "~/repository/cedet-1.0pre4/common/working.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * This turns on which-func support (Plus all other code helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; This turns on modes that aid in grammar writing and semantic tool
;; development.  It does not enable any other features such as code
;; helpers above.
;; (semantic-load-enable-semantic-debugging-helpers)

;; (ecb-activate)
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

