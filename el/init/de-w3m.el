
(load "de-vars")
(setq load-path (cons (concat root-path "net/w3m/emacs-w3m") load-path))

(require 'w3m-load)

(eval-after-load "w3m.elc"
  '(progn
     ;; Mozilla-like navigation:
     (define-key w3m-mode-map [(meta right)] 'w3m-view-this-url)
     (define-key w3m-mode-map [(meta left)]  'w3m-view-previous-page)
     (define-key w3m-mode-map [(meta shift right)] 'w3m-view-this-url-new-session)
     (define-key w3m-mode-map [(control return)] 'w3m-view-this-url-new-session)
     ;; Lynx-like navigation:
     (define-key w3m-mode-map [(meta up)]
       (lambda ()
         (interactive)
         (my-prev-link-or-scroll-page-backward
          (save-excursion
            (ignore-errors (w3m-previous-anchor))
            (point)))))
     (define-key w3m-mode-map [(meta down)]
       (lambda ()
         (interactive)
         (my-next-link-or-scroll-page-forward
          (save-excursion
            (ignore-errors (w3m-next-anchor))
            (point)))))
     ;; more/less scrolling style if point is not on URL
     (define-key w3m-mode-map [return]
       (lambda ()
         (interactive)
         (if (or (not (w3m-anchor))
                 (eq (point) (save-excursion (move-to-window-line -1) (point))))
             (View-scroll-line-forward)
           (w3m-view-this-url))))
     ;; Tabs navigation (useful when tabs are visible):
     ;; to avoid conflict with (control tab) calling ee-buffers,
     ;; w3m could be used in the separate frame
     (define-key w3m-mode-map [(control tab)] 'w3m-next-buffer)
     (define-key w3m-mode-map [(control shift tab)] 'w3m-previous-buffer)
     (define-key w3m-mode-map [(control shift iso-lefttab)] 'w3m-previous-buffer)
     ;; Add emacs version and gnu/linux version
     (setq w3m-user-agent (concat
                           "Emacs-w3m/" emacs-w3m-version
                           " " w3m-version
                           " Emacs/" emacs-version
                           (if (string-match "[Ll]inux" system-configuration)
                               (concat " (" system-configuration ")")
                             "")))
     ;; (my-faces-set)
     ))

;; (add-hook
;;  'w3m-display-hook
;;  (lambda (url)
;;    ;; but better idea is display these names only in buffer list
;;    (rename-buffer
;;     (generate-new-buffer-name
;;      (concat "*w3m*<"
;;              w3m-current-title
;;              ;; (substring w3m-current-title 0 (min (length w3m-current-title) 11))
;;              ">")))))


(setq w3m-command-arguments
      (nconc w3m-command-arguments
             '("-o"
      "http_proxy=http://isaac:isaac@192.168.1.5:3128")))

;; (setq browse-url-browser-function 'w3m-browse-url)
;;  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;  (global-set-key "\C-xm" 'browse-url-at-point)


;; ;;  (defun w3m-new-tab ()
;; ;;    (interactive)
;; ;;    (w3m-copy-buffer nil nil nil t))

;; ;;  (defun w3m-browse-url-new-tab (url &optional new-session)
;; ;;    (interactive)
;; ;;    (w3m-new-tab)
;; ;;    (w3m-browse-url url))

;; ;;  (setq browse-url-browser-function 'w3m-browse-url-new-tab)

;;  (setq browse-url-browser-function 'w3m-browse-url
;;           browse-url-new-window-flag t)