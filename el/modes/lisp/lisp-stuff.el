

;;; -*- Mode: Emacs-Lisp -*-
;;; Lisp-Settings
(autoload 'run-lisp "inf-lisp" "Inferior Lisp" t)


;;; ******************
;;; Customization of ILISP - this also defines the places where LISPS are found
;;; Autoload based on your LISP.  You only really need the one you use.
;;; If called with a prefix, you will be prompted for a buffer and
;;; program.
;;; **************
;;; Lisp settings - on Lothlorien most of this is in site-start.el
;;; Note: loom-settings can be found in loom-stuff.el
(setq ilisp-prefix "\C-z")
(autoload 'run-ilisp "ilisp" "Select a new inferior LISP." t)
(autoload 'gcl "ilisp" "Inferior GNU Common LISP." t)
(autoload 'clisp-hs "ilisp" "Inferior CMU Common LISP." t)
(autoload 'lucid     "ilisp" "Inferior Lucid Common LISP." t)
(autoload 'loom       "ilisp" "Inferior Loom." t)
(autoload 'groom      "ilisp" "Inferior Loom with garnet." t)
(autoload 'cmulisp "ilisp" "Inferior CMUCL." t)
(autoload 'stk "ilisp" "Run STk in ILISP." t)
(autoload 'allegro "ilisp" "Run ACL in ILISP." t)

;; Define where LISP programs are found., and some LISP-system
;; specific settings.
(setq gcl-program "gcl")
(setq clisp-hs-program "clisp -I")
(setq allegro-program "/usr/local/acl/bin/clim2xm_composer")
(setq lucid-program "/opt/lisp/lucidlisp-sol")
(setq loom-program "/usr/local/coling/images/loom/loom3.0.im")
(setq groom-program "/usr/local/coling/images/loom/groom3.0.im")

(setq cmulisp-program "/usr/bin/lisp")
(setq cmulisp-local-source-directory "/usr/src/cmucl/17f/")
(add-hook 'cmulisp-hook
	  '(lambda ()
	     (setq ilisp-init-binary-command 'nil)
	     (setq ilisp-binary-command 'nil)
	     ))

(add-hook 'ilisp-load-hook
          '(lambda ()
	     ;; define STk-Scheme dialect
	     (defdialect stk "STk Scheme"
	       scheme
	       (setq comint-prompt-regexp "^STk> ")
	       (setq ilisp-program "stk -interactive")
	       (setq comint-ptyp t)
	       (setq comint-always-scroll t)
	       (setq ilisp-last-command "*"))
))

	       
;; local-clman uses the clman from Allegro via a small script
(autoload 'local-clman "local-clman" "Local CLMAN" t)

; CLtL2-setting
(setq *cltl2-url* "http://zeus.gmd.de/~schreck/cltl/")
(setq *cltl2-local-file-pos*
      (concat "/usr/local/www/htdocs/miscellaneous/"
	      "interest-links/programming/lisp/steele/"))

; ANSI-Common Lisp HyperSpec
(setq *hyperspec-url* "c:/lisp/docs/HyperSpec/")

;; hyperspec is nearly the same but for the ANSI HyperSpec
;; as collected by Harlequin
(autoload 'hyperspec-lookup "hyperspec" "Load hyperspec" t)

;; and hooking things together ...
(defun local-ilisp-addons ()
  "Some local additions to ilisp. Hooked into ilisp-mode-hook."
  (local-set-key "\C-zD" 'local-clman)
  (local-set-key [(control z) h] 'hyperspec-lookup)
  (load-library "ilisp-easy-menu")  
  (cltl2-lisp-mode-install))
(add-hook 'ilisp-mode-hook 'local-ilisp-addons)

(defun local-lisp-addons ()
  "Some local additions to lisp-mode. 
   Hooked into lisp-mode-hook or inf-lisp-mode."
  (require 'ilisp)
  (cltl2-lisp-mode-install)
  (local-set-key [(control c) h] 'hyperspec-lookup))
(add-hook 'lisp-mode-hook 'local-lisp-addons)
(add-hook 'inf-lisp-mode-hook 'local-lisp-addons)


;;; Allegro CL comes with its own interface (no Ilisp !)
(defun fi:acl ()
 "Starts up the Allegro Lisp in an inferior lisp-buffer. Wrapper for
  fi:common-lisp."
  (interactive) 
  (setq load-path (pushnew "/usr/local/acl4.3/home/emacs/fi" load-path))
  (load-library "fi-site-init")
  (fi:common-lisp))


	       
;; browse-cltl2 is a package for browsing Common Lisp the Language
;; via www or a local copy using browse-url
(setq *cltl2-fetch-method* 'local)
(when running-xemacs
  (load-library "/home/schauer/emacs/elisp/development/browse-cltl2"))
(autoload 'cltl2-view-function-definition "browse-cltl2")
(autoload 'cltl2-view-index "browse-cltl2")
(autoload 'cltl2-lisp-mode-install "browse-cltl2")
(cond ((and (string-match "XEmacs\\|Lucid" emacs-version)
	    (= emacs-major-version 19)
	    (<= emacs-minor-version 14))
       (setq *cltl2-old-find-file-noselect* 't)))


(defun lispmodehooks ()
 "All you ever wanted in Lisp-mode .. aehem. Defined in HS .emacs"
 (turn-on-font-lock)
 (if (not (featurep 'ilisp))
     (load-library "ilisp"))
 (cltl2-lisp-mode-install)
 (setq lisp-no-popper t)
 (local-set-key "\C-c)" 'find-unbalanced-lisp)
 (local-set-key "\C-ct" 'find-tag))

;; enable loading of Ilisp-Menu - overwrites previous hooks ???
(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       (load-library "ilisp-easy-menu")
       (add-hook 'lisp-mode-hook 'lispmodehooks)))

; Setze den richtigen Programmnamen fuer das Ilisp-Paket und Allegro
(setq load-path (append (list "/usr/local/acl501/eli/") load-path))
(setq fi:common-lisp-image-name 
      (substitute-in-file-name "/usr/local/acl/bin/clim2xm_composer"))
(autoload 'run-acl "fi-site-init" "Start an inferior Allegro Lisp process" t)
(autoload 'fi:acl "fi-site-init" "Start an inferior Allegro Lisp process" t)
(autoload 'fi:common-lisp 
    "fi-site-init" "Start an inferior Allegro Lisp process" t)

;; Nachladen der LOOM-spezifischen Settings
(load-library "loom-stuff")

