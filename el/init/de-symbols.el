;; (defun pretty-alpha ()
;;   (font-lock-add-keywords
;;    nil `(("(\\(alpha\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ,(make-char 'greek-iso8859-7 128))
;;                     nil))))))


;; (defun pretty-delta ()
;;   (font-lock-add-keywords
;;    nil `(("(\\(delta\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ,(make-char 'greek-iso8859-7 100))
;;                     nil))))))



;; (add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
;; (add-hook 'lisp-mode-hook 'pretty-lambdas)

;; (add-hook 'guile-scheme-mode-hook 'pretty-lambdas)

;; (add-hook 'scheme-mode-hook 'pretty-lambdas)
;; (add-hook 'scheme-mode-hook 'pretty-delta)


;; (set-language-environment "English")



(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code)))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))

(add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'ruby-mode-hook 'pretty-greek)
(add-hook 'jabber-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)


;;  (defvar big-lambda-image
;;     (create-image
;;      (base64-decode-string
;;       "/9j/4AAQSkZJRgABAQEASABIAAD/2wBDAAsICAoIBwsKCQoNDAsNERwSEQ8PESIZGhQcKSQrKigk
;;   JyctMkA3LTA9MCcnOEw5PUNFSElIKzZPVU5GVEBHSEX/2wBDAQwNDREPESESEiFFLicuRUVFRUVF
;;   RUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUVFRUX/wAARCAAwAEADASIA
;;   AhEBAxEB/8QAGgABAQEBAAMAAAAAAAAAAAAAAAUEAwIGB//EACgQAAEEAQIEBgMAAAAAAAAAAAEA
;;   AgMEERIhBTFhcQYUIjJBgRVRkf/EABcBAQEBAQAAAAAAAAAAAAAAAAABAwL/xAAcEQEBAQACAwEA
;;   AAAAAAAAAAAAAQIRIQMxQVH/2gAMAwEAAhEDEQA/APriIiAThZqvEKd5zxUtQzmP3CN4dj+Lyu1G
;;   XqklaRzmxyDDtJwSM7jsRsehWHiUbKlrh1qJgYWTCu7TtmN/p09g7QfpaYznXX1FVERZqIiICIiA
;;   ofiK6IvJV2t1vdZhkfv7I2yNy4/ZAHforM80deCSaZ4ZHG0uc48gBuSoTqE13g1+1Kwi5dj1MYec
;;   bW7xs+uZ6krfwyTU1r0lewIuNOyy5ThsR+yZgeOxGV2WNnF4qiIigIiIJd+N/EL8NIsd5WPE87iN
;;   n4PoZ13GT0AHyqiIurrmSfgixT/gpJa9hjxRc4vgmYwuEeTksdgbYJJB5YOPjffQuOvNllETo4Ne
;;   InPBaZG4GXYO4Gc4/eMrWi61ua7s7QREWav/2Q==")
;;      'jpeg t))

;;   (defvar big-lambda-font-lock-keywords
;;     '((".+" (0 (prog1 nil
;;                  (big-lambda-remove-region
;;                   (match-beginning 0) (match-end 0)))))
;;       ("(\\(lambda\\)\\>"
;;        (0 (prog1 nil
;;             (big-lambda-region (match-beginning 1) (match-end 1)))))))

;;   (defun big-lambda-remove-region (beg end)
;;     "Remove big lambda property in region between BEG and END."
;;     (let (pos)
;;       (while (setq pos (text-property-any beg end 'display big-lambda-image))
;;         (remove-text-properties
;;          pos
;;          (or (next-single-property-change pos 'display) end)
;;          '(display)))))

;;   (defun big-lambda-region (beg end)
;;     "Add big lambda property in region between BEG and END."
;;     (put-text-property beg end 'display big-lambda-image))

;;   (define-minor-mode big-lambda-mode
;;     "Display big lambda."
;;     :lighter " Lambda"
;;     (if big-lambda-mode
;;         (progn
;;           (save-restriction
;;             (widen)
;;             (let ((font-lock-keywords big-lambda-font-lock-keywords))
;;               (font-lock-fontify-buffer)))
;;           (font-lock-add-keywords nil big-lambda-font-lock-keywords))
;;       (font-lock-remove-keywords nil big-lambda-font-lock-keywords)
;;       (save-restriction
;;         (widen)
;;         (let ((modified-p (buffer-modified-p)))
;;           (big-lambda-remove-region (point-min) (point-max))
;;           (set-buffer-modified-p modified-p)))))

;;   (defun big-lambda-mode-turn-on ()
;;     "Turn on `big-lambda-mode'."
;;     (interactive)
;;     (big-lambda-mode 1))

;;   (add-hook 'emacs-lisp-mode 'big-lambda-mode-turn-on)
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

