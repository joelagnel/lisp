(load "de-vars")
(setq load-path (cons (concat root-path "modes/perl") load-path))

(require 'perl-find-library)

(defun perltidy-region ()
        "Run perltidy on the current region."
        (interactive)
        (save-excursion
                (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
        "Run perltidy on the current defun."
        (interactive)
        (save-excursion (mark-defun)
                        (perltidy-region)))

(global-set-key "\C-xt" 'perltidy-region)


(add-hook 'cperl-mode-hook 'hs-minor-mode)


(define-key global-map "\C-c\}" 'hs-hide-all)
(define-key global-map "\C-c\{" 'hs-show-all)
(define-key global-map "\C-c\]" 'hs-hide-block)
(define-key global-map "\C-c\[" 'hs-show-block)


(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))


(defalias 'perl-mode 'cperl-mode)


(setq cperl-invalid-face nil)
(setq cperl-electric-keywords nil)
(setq cperl-electric-parens nil)
(setq cperl-highlight-variables-indiscriminately t)
(setq cperl-hairy nil)
(setq cperl-extra-newline-before-brace t)

;; ;; (setq cperl-indent-level 2
;; ;;       cperl-close-paren-offset (- cperl-indent-level))


(setq cperl-mode-hook
      '(lambda ()
        (require 'perlplus)
        (define-key cperl-mode-map [S-return] 'perlplus-complete-symbol)
        (perlplus-setup)))

(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))


(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
    (cperl-get-help))))

(add-hook 'cperl-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-documentation-function)
                 'my-cperl-eldoc-documentation-function)))



(global-set-key "\C-c\C-p" 'cperl-perldoc-at-point)
(global-set-key "\C-c\C-d" 'cperl-perldoc)

(setq cperl-indent-parens-as-block t)
(add-hook
      'cperl-mode-hook
      (lambda ()
        (setq cperl-lazy-help-time 0.001)
        (cperl-lazy-install)
        (cperl-set-style "K&R")))


;;pod customizations , Use outine with cperl-mode

(setq cperl-mode-hook 'my-cperl-customizations)

(defmacro join (join-char &rest others) `(mapconcat 'identity ',others ,join-char))

(setq my-cperl-outline-regexp
      (concat
       "^"                              ; Start of line
       "[ \\t]*"                        ; Skip leading whitespace
       "\\("                            ; begin capture group \1
       (join "\\|"
             "=head[12]"                  ; POD header
             "package"                    ; package
             "=item"                      ; POD item
             "sub"                        ; subroutine definition
           )
       "\\)"                            ; end capture group \1
       "\\b"                            ; Word boundary
       ))


(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)


(defun my-cperl-customizations ()
  "cperl-mode customizations that must be done after cperl-mode loads"
  (outline-minor-mode)
  (abbrev-mode)

  (defun cperl-outline-level ()
    (looking-at outline-regexp)
    (let ((match (match-string 1)))
      (cond
       ((eq match "=head1" ) 1)
       ((eq match "package") 2)
       ((eq match "=head2" ) 3)
       ((eq match "=item"  ) 4)
       ((eq match "sub"    ) 5)
       (t 7)
       )))

  (setq cperl-outline-regexp  my-cperl-outline-regexp)
  (setq outline-regexp        cperl-outline-regexp)
  (setq outline-level        'cperl-outline-level)
)




(defun catalyst-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'cperl-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                  'cperl-indent-function
                  (car x)))
         keyword-rules))

 (catalyst-add-keywords
  'font-lock-keyword-face
  '((1 . stash)
    (1 . model)
    (2 . forward)
    (1 . redirect)
    (1 . req)
    ))






(provide 'de-perl)