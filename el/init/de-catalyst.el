(load "de-vars")
(setq load-path (cons (concat root-path "modes/html/htmlcleaner") load-path))
(setq load-path (cons (concat root-path "modes") load-path))



   ;; HTMLcleaner settings
   (autoload 'html-cleaner "htmlcleaner" "Cleans up your crappy HTML and XML code" t)

   ;; Map the HTMLcleaner to a shortcut, f.eks. Control-H
   (global-unset-key "\C-H")
   (global-set-key "\C-H" 'html-cleaner)

   ;; You can set three variables: 
   ;; When to wrap text e.g. in paragraph with a lot of text,
   ;; whether or not to have lowercase tags and
   ;; how many spaces indentation you want.
   (setq element_value_width 45)
   (setq lowercase_elements t)
   (setq html-cleaner-indent-level 3)

   ;; If you want to turn off tag and element checking.
   (setq clouseau nil)                 





;; (require 'html-helper-mode)
;; (require 'html-font)


;;  ;;Setting for html-helper-mode with html-tt:

;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq auto-mode-alist
;;       (cons
;;        '("\\.tt$" . html-helper-mode) auto-mode-alist))
;; (require 'html-tt)
;; (add-hook 'html-helper-mode-hook 'html-tt-load-hook)

;; ;; Setting values:

;;      ;; change sequence face
;; (make-face 'my-sequence-face)
;; (set-face-foreground 'my-sequence-face "blue")
;; (set-face-background 'my-sequence-face "bisque")
;; (setq html-tt-sequence-face 'my-sequence-face)
;; ;; or
;; (setq html-tt-sequence-face 'bold)
;; (setq html-tt-sequence-face 'italic)
;; (setq html-tt-sequence-face 'underline)

;;      ;; change sequence for insert
;; (setq html-tt-sequence-start "[% ")
;; (setq html-tt-sequence-end " %]")




;; ;; (require 'mmm-mode)
;; ;; (require 'mmm-auto)
;; ;; (require 'mmm-vars)

;; ;; (mmm-add-classes
;; ;;  '((embedded-tt
;; ;;     :submode tt
;; ;;     :face mmm-code-submode-face
;; ;;     :front "\\[%"
;; ;;     :back "%\\]"
;; ;;     :include-front t
;; ;;     :include-back t
;; ;;     :match-name "tt"
;; ;;     )))

;; ;; (provide 'mmm-classes-custom)

;; ;; ;;; mmm-classes-custom.el ends here

;; ;; ;;; from .emacs
;; ;; (require 'tt-mode)                      ;Template Toolkit

;; ;; (setq mmm-global-mode 'maybe)
;; ;; (require 'mmm-classes-custom)
;; ;; (setq mmm-mode-ext-classes-alist
;; ;;       (append '((html-helper-mode "\\.tt" embedded-tt))
;; ;; 	      mmm-mode-ext-classes-alist ))



