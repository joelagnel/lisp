
;;;EMACS - GUILE


(load "de-vars")
(setq load-path (cons (concat root-path "modes/lisp/guile") load-path))

(require 'guile-scheme)
(require 'guile-c)

(setq initial-major-mode 'guile-scheme-mode)
(add-hook 'c-mode-hook
    (lambda ()
      (require 'guile-c)
      (define-key c-mode-map "\C-c\C-g\C-p" 'guile-c-insert-define)
      (define-key c-mode-map "\C-c\C-g\C-e" 'guile-c-edit-docstring)
      (define-key c-mode-map "\C-c\C-g\C-d" 'guile-c-deprecate-region)
      ))

(define-key scheme-mode-map "\C-x\C-e" 'guile-scheme-eval-last-sexp)
(define-key scheme-mode-map [S-return] 'guile-scheme-complete-symbol)
(define-key guile-scheme-mode-map [S-return] 'guile-scheme-complete-symbol)



(require 'guile)


;; Keywords

 (defun scheme-add-keywords (face-name keyword-rules)
   (let* ((keyword-list (mapcar #'(lambda (x)
                                    (symbol-name (cdr x)))
                                keyword-rules))
          (keyword-regexp (concat "(\\("
                                  (regexp-opt keyword-list)
                                  "\\)[ \n]")))
     (font-lock-add-keywords 'guile-scheme-mode
                             `((,keyword-regexp 1 ',face-name))))
   (mapc #'(lambda (x)
             (put (cdr x)
                  'scheme-indent-function
                  (car x)))
         keyword-rules))
 
;;  (scheme-add-keywords
;;   'font-lock-keyword-face
;;   '((1 . when)
;;     (1 . unless)
;;     (2 . let1)
;;     (1 . error)
;;     (1 . mapc)
;;     ))



(add-to-list 'load-path "~/repository/lisp/el/modes/lisp/scheme-lookup/")

(require 'scheme-lookup)
(require 'scheme-lookup-guile)


(require 'quack)


(define-key inferior-scheme-mode-map "\t" 'guile-scheme-complete-symbol)
(provide 'de-guile)