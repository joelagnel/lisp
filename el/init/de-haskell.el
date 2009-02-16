;; Font Locking, Programming Modes, and Compilation settings
;;

(global-font-lock-mode 1)
;; maximum colors
(setq font-lock-maximum-decoration t)

;; extra key bindings
(global-set-key "\M-C" 'compile)
(global-set-key "\C-^" 'next-error)
(global-set-key "\C-\M-g" 'goto-line)

;; use spaces instead of tabs
(setq indent-tab-mode nil)

;; haskell mode configuration
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.[hg]s$"  . haskell-mode)
                ("\\.hic?$"     . haskell-mode)
                ("\\.hsc$"     . haskell-mode)
  ("\\.chs$"    . haskell-mode)
                ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)

;adding the following lines according to which modules you want to use:
;; (require 'inf-haskell)

;; (add-hook 'haskell-mode-hook 'turn-on-font-lock)
;; ;(add-hook 'haskell-mode-hook 'turn-off-haskell-decl-scan)
;; ;(add-hook 'haskell-mode-hook 'turn-off-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; ;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; ;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; (add-hook 'haskell-mode-hook
;;    (function
;;     (lambda ()
;;       (setq haskell-program-name "ghci")
;;       (setq haskell-ghci-program-name "ghci6"))))
