;;;  -*- Mode: Emacs-Lisp -*-
;;;
;;; Copyright (c) 2002 Kevin Rosenberg GNU License

(defvar ilisp-*directory* "/usr/lib/ilisp/")

(require 'completer)

(setq ilisp-*use-fsf-compliant-keybindings* t)
(when (file-exists-p "/etc/ilisp/ilisp-keybindings.el")
  (load "/etc/ilisp/ilisp-keybindings.el"))
(setq ilisp-*use-frame-for-output* nil)
(setq ilisp-*use-frame-for-arglist-output-p* nil)
(setq ilisp-motd nil)

(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)
(autoload 'clisp-hs  "ilisp" "Inferior CLISP Common LISP." t)
(autoload 'allegro "ilisp" "Inferior Allegro Common Lisp." t)
(autoload 'lispworks "ilisp" "Inferior Lispworks Common Lisp." t)
(autoload 'openmcl "ilisp" "Inferior OpenMCL Common Lisp." t)
(autoload 'scheme  "ilisp" "Inferior generic Scheme." t)
(autoload 'guile "ilisp" "Inferior Guile Scheme." t)

(defun alisp ()
  (interactive)
  (setq allegro-program "/usr/bin/alisp")
  (allegro))
(defun alisp8 ()
  (interactive)
  (setq allegro-program "/usr/bin/alisp8")
  (allegro))
(defun mlisp ()
  (interactive)
  (setq allegro-program "/usr/bin/mlisp")
  (allegro))
(defun mlisp8 ()
  (interactive)
  (setq allegro-program "/usr/bin/mlisp8")
  (allegro))

(defun cmucl-normal ()
  "Inferior CMU Common LISP -- normal core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-normal.core")
  (cmulisp))
(defun cmucl-small () 
  "Inferior CMU Common LISP -- small core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-small.core")
  (cmulisp))
(defun cmucl-safe ()
 "Inferior CMU Common LISP -- safe core."
  (interactive)
  (setq cmulisp-program "/usr/bin/lisp -core /usr/lib/cmucl/lisp-safe.core")
  (cmulisp))

(defun sbcl-mt ()
 "SBCL multithreading."
  (interactive)
  (setq sbcl-program "/usr/bin/sbcl-mt --noinform")
  (sbcl))

(autoload 'sbcl  "ilisp" "Inferior Steel Bank Common LISP." t)
(autoload 'cmulisp  "ilisp" "Inferior CMU Common LISP." t)

(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
(setq cltl2-root-url "file:///usr/share/doc/cltl/")

(setq ilisp-site-hook
      '(lambda ()
	 ;; delete any keybindings that start with this sequence, such
	 ;; as oo-browser's  bindings 
	 (global-unset-key [(control c) (control v)])

	 (setq clisp-hs-progam "clisp -a -I")
	 (setq allegro-program "/usr/bin/acl")
	 (setq lispworks-program "/usr/bin/lw-console -multiprocessing")
	 (setq openmcl-program "/usr/bin/openmcl")
	 (setq cmulisp-program "/usr/bin/lisp")
	 (setq guile-program "/usr/bin/guile1.4")
	 (setq cmulisp-local-source-directory "/usr/src/cmucl/")
	 ))

(add-hook 'ilisp-mode-hook
	  '(lambda ()))

;; Loading lisp files starts ilisp
(set-default 'auto-mode-alist
	     (append '(("\\.lisp$" . lisp-mode)
		       ("\\.lsp$" . lisp-mode)
		       ("\\.cl$" . lisp-mode))
		     auto-mode-alist))

(add-hook 'lisp-mode-hook
	  (function (lambda () (require 'ilisp))))

;;; Load hooks
(add-hook  'scheme-mode-hook
	   (function (lambda () (require 'ilisp))))

(add-hook 'ilisp-load-hook
	  '(lambda ()
	     (when ilisp-*use-fsf-compliant-keybindings*
	       (setq ilisp-*prefix* "\C-c"))
	     (setq lisp-no-popper t)
	     
	     ;; Make sure that you don't keep popping up the 'inferior
             ;; Lisp' buffer window when this is already visible in
             ;; another frame. Actually this variable has more impact
             ;; than that. Watch out.
             ; (setq pop-up-frames t)

             ;;(message "Running ilisp-load-hook")
             ;; Define LispMachine-like key bindings, too.
             ; (ilisp-lispm-bindings) Sample initialization hook.

             ;; Set the inferior Lisp directory to the directory of
             ;; the buffer that spawned it on the first prompt.
             (add-hook 'ilisp-init-hook
                       '(lambda ()
                          (default-directory-lisp ilisp-last-buffer)))

             ))


