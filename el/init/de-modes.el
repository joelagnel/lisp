;;Modes

(load "de-vars")
(setq load-path (cons (concat root-path "modes") load-path))
;;tt-stuff


(autoload 'tt-mode "tt-mode")
(setq auto-mode-alist
(append '(("\\.tt$" . tt-mode))  auto-mode-alist ))
(autoload 'javascript-mode "javascript-mode") 
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))




(add-hook 'text-mode-hook
	  (lambda ()
	    (setq fill-column 72)
	    (turn-on-auto-fill)))



;;sawfish-mode initialization
(require 'sawfish)
(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)
(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))

(require 'jump-def)

(autoload 'jump-to-def "jump-def" "Jump to a definition." t)
(global-set-key "\C-cj" 'jump-to-def)

(require 'comment)
(autoload 'comment-out-region "comment" nil t)
(global-set-key "\C-cq" 'comment-out-region)

;;Modes
;(which-func-mode)

(require 'yaml-mode)


;;a bit of mysql ...
(setq load-path (cons (concat root-path "modes/sql") load-path))


(eval-after-load "sql" '(load-library "sql-indent"))

(require 'mysql)
(require 'sql-completion)
(setq sql-interactive-mode-hook
      (lambda ()
        (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
        (sql-mysql-completion-init)))


;;This small piece of ugly code allows you to open .pdf files and run pdftotext on them before viewing them.

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . no-pdf))

(defun no-pdf ()
  "Run pdftotext on the entire buffer."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (erase-buffer)
    (shell-command
     (concat "pdftotext " (buffer-file-name) " -")
     (current-buffer)
     t)
    (set-buffer-modified-p modified)))


;;ecb
;; (setq load-path (cons "~/repository/lisp/el/modes/ecb/" load-path))

;; (load "ecb.el")


(setq load-path (cons (concat root-path "modes/predictive") load-path))

(require 'predictive)

(autoload 'predictive-mode (concat root-path "modes/predictive/predictive.elc"))