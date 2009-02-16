;; This is just a quick hack to make the htmlizer independent of my
;; .emacs and callable from the command line.
;; Author and version: Eduardo Ochs, 2005apr07
;; This file is in the Public Domain.

(add-to-list 'load-path default-directory)
(load-library "eev.el")         ; (find-eev "eev.el")
(load-library "eev-langs.el")   ; (find-eev "eev-langs.el")
(load-library "eev-insert.el")  ; (find-eev "eev-insert.el")
(load-library "eev-glyphs.el")  ; (find-eev "eev-glyphs.el")
(load-library "eev-compose.el") ; (find-eev "eev-compose.el")
(eev-set-aliases)               ; (find-eev "eev.el")
(eev-set-default-glyphs)
;;
(load-library "htmlize-eev.el") ; (find-eev "htmlize-eev.el")
(code-c-d-anchor "angg" "~/")

;; (if (getenv "DEBUG_ON_ERROR") (setq debug-on-error t))
(setq debug-on-error t)

(htmlize-eev-files)

;; The e-script below needs this in ~/elisp/:
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el

'
(eev "
cd ~/eev-current/
emacs-cvs -Q --geometry +200+100 -l ~/elisp/htmlize.el -l htmlize-all.el
")

;; Note: emacs21 is not so happy with htmlize-all.el as CVS emacs, and
;; its font-locking modes have fewer colors. Let's stick to CVS emacs
;; for now.
