;;;ILISP

(load "de-vars")
(setq load-path (cons (concat root-path "modes/lisp/ilisp") load-path))


;;; If you always want partial minibuffer completion
;;(require 'completer)


;; (setq common-lisp-hyperspec-root "~/repository/lisp/cl/docs/HyperSpec/")
;; (setq common-lisp-hyperspec-symbol-table "~/repository/lisp/cl/docs/HyperSpec/Data/Map_Sym.Txt")
;; (setq inferior-lisp-program "/usr/bin/clisp")



;;; Uncomment the following lines if you want TMC completion (see
;;; completion.el among the Emacs ELisp sources).

; (load "completion")
; (initialize-completions)


;;; If you want to redefine typeout-window keys.

; (add-hook 'ilisp-load-hook
;   '(lambda ()
;      (define-key global-map "\C-c1" 'ilisp-bury-output)
;      (define-key global-map "\C-cv" 'ilisp-scroll-output)
;      (define-key global-map "\C-cg" 'ilisp-grow-output)))


;;; Autoload based on your Lisp. You only really need the one you
;;; use. If called with a prefix, you will be prompted for a
;;; buffer and program.

(autoload 'run-ilisp   "ilisp" "Select a new inferior Lisp." t)
(autoload 'common-lisp "ilisp" "Inferior generic Common Lisp." t)


;; CLISP (Bruno Haible and Michael Stoll)
;; (autoload 'clisp-hs   "ilisp"
;;   "TURING LISP LAB" t)

;; (autoload 'cmucl "ilisp"
;;    "Inferior GNU Common Lisp." t)

;; (autoload 'sbcl "ilisp"
;;   "Inferior SBCL" t)

;; ;(autoload 'gcl "ilisp"
;; ;   "Inferior GNU Common Lisp." t)

;; ;; XLisp
;; ;;(autoload 'xlisp      "ilisp" "Inferior XLisp." t)
;; (autoload 'xlispstat  "ilisp" "Inferior XLisp-Stat." t)

;; Scheme
;;(autoload 'scheme     "ilisp" "Inferior generic Scheme." t)

(autoload 'guile      "ilisp" "Inferior GUILE Scheme." t)
(setq scheme-program "/usr/bin/guile")
(setq guile-program "/usr/bin/guile")


;;; Define where Lisp programs are found. (This may already have
;;; been done at your site).
;;; (setq lucid-program "/usr/misc/.lucid/bin/lisp")
;;; (setq clisp-hs-program "/usr/bin/clisp")
;;; (setq gcl-program "/usr/bin/gcl")

;;; (setq xlispstat-program "/usr/local/bin/xlispstat")
;; (setq cmucl-program "/usr/bin/lisp")
;; (setq sbcl-program "/usr/bin/sbcl")

;;; This makes reading a Lisp or Scheme file load in ILISP.
(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)
                       ("\\.lsp$" . lisp-mode)
                       ("\\.cl$" . lisp-mode))
                     auto-mode-alist))
(add-hook 'lisp-mode-hook '(lambda () (require 'ilisp)))
;(add-hook 'scheme-mode-hook '(lambda () (require 'ilisp)))


;;; Configuration of Erik Naggum's HyperSpec access package.
(setq common-lisp-hyperspec-root
      "/home/ike/lisp/docs/HyperSpec/")
(setq common-lisp-hyperspec-symbol-table
     "/home/ike/lisp/docs/HyperSpec/Data/Map_Sym.Txt")
;;; Configuration of Utz-Uwe Haus' CLtL2 access package.
(setq cltl2-root-url
      "home/ike/lisp/docs/cltl2/")


;;; Sample load hook
(add-hook 'ilisp-load-hook
          '(lambda ()
             ;; Change default key prefix to C-c
             (setq ilisp-*prefix* "\C-c")
             ;; Set a keybinding for the COMMON-LISP-HYPERSPEC command
             (defkey-ilisp "\C-ch" 'common-lisp-hyperspec)
	     (defkey-ilisp "\C-ct" 'cltl2-lookup)

             ;; Make sure that you don't keep popping up the 'inferior
             ;; Lisp' buffer window when this is already visible in
             ;; another frame. Actually this variable has more impact
             ;; than that. Watch out.
             ; (setq pop-up-frames t)
             (message "Running ilisp-load-hook")
             ;; Define LispMachine-like key bindings, too.
             ; (ilisp-lispm-bindings) Sample initialization hook.

             ;; Set the inferior Lisp directory to the directory of
             ;; the buffer that spawned it on the first prompt.
             (add-hook 'ilisp-init-hook
                       '(lambda ()
                          (default-directory-lisp ilisp-last-buffer)))
             ))


