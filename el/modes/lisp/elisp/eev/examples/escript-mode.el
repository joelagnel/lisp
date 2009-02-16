;; From a posting by Rubikitch at the eev mailing list:
;; <http://lists.gnu.org/archive/html/eev/2005-06/msg00006.html>
;; <http://article.gmane.org/gmane.emacs.eev.devel/15>

;; Note that Rubikitch hasn't signed the FSF papers yet -- this file
;; cannot be included in Emacs until he signs the transfer of
;; copyright, but it isn't part of the "kernel" of eev that is ready
;; for inclusion...

;; To do (for Edrx): reorder; make the `global-set-key's less global;
;; understand why this uses view-mode; understand the defadvices,
;; think about creating hooks in `find-elinks', `find-estring' and
;; `find-sh' to make the defadvice unnecessary; see if I can get rid
;; of the other defadvices; add comments and examples with eesteps.

;; --snip--snip--

;; I once created escript-mode.
;; This major-mode highlights sexp in comment and so on.
;; But I *failed* to highlights sexp in comment in emacs-lisp-mode...

;;;; [2003/07/27]
;;;; escript-mode by rubikitch <address@hidden>


;;;; escript-mode

(defface ee-link-underline
  '((t (:foreground "blue" :underline t))) t)

(setq ee-link-underline (make-face 'ee-link-underline))

(define-derived-mode escript-mode text-mode "ES"
  "major-mode for e-scripts"
  ;; sexp search
  (define-key escript-mode-map "\M-p"     'ee-prev-sexp-in-comment)
  (define-key escript-mode-map "\M-n"     'ee-next-sexp-in-comment)
  (define-key escript-mode-map "\C-c\C-t" 'escript-goto-top-page)
  (define-key escript-mode-map "\C-c\C-c" 'ee-save-sexp-eol)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'truncate-lines)
  (setq truncate-lines t)
  ;;
  (make-local-variable 'font-lock-comment-face)
  (setq font-lock-comment-face 'eev-glyph-face-red)
  ;;
  (setq font-lock-keywords
        '(("^ *#[^\(]+\\((.*)\\)$" 1 ee-link-underline)
          ("^ *\\(#[^\(]+\\)\\((.*)\\)$" 1 font-lock-comment-face)))
  (setq font-lock-defaults '((font-lock-keywords) t nil))  
  (font-lock-mode))

(add-to-alist 'auto-mode-alist '("\\.e$" . escript-mode))

(make-variable-buffer-local 'escript-top-page)
(defun escript-goto-top-page ()
  (interactive)
  (when (boundp 'escript-top-page)
    (find-file escript-top-page)))

;;;
;;; sexp search
;;;

(defun ee-next-sexp-sub (re)
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (unless (re-search-forward re nil t)
      (goto-char pt)
      (error "sexp not found")))
  (beginning-of-line)
  (search-forward "("))

(defun ee-prev-sexp-sub (re)
  (interactive)
  (when (eq (char-before) ?\) )
    (forward-char -1))
  (if (re-search-backward re nil t)
      (goto-char (match-end 0))
    (error "sexp not found"))
  (beginning-of-line)
  (search-forward "("))

(defun ee-next-sexp ()
  (interactive)
  (ee-next-sexp-sub ")$"))
(defun ee-prev-sexp ()
  (interactive)
  (ee-prev-sexp-sub ")$"))

;;;
;;; sexp search in comment
;;;

(defun ee-sexp-in-comment-regexp ()
  (let ((c (if comment-start
               (substring comment-start 0 1)
             "")))
    (format "[%s;#].*([A-Za-z].+)$" c)))

(defun ee-next-sexp-in-comment ()
  (interactive)
  (ee-next-sexp-sub (ee-sexp-in-comment-regexp)))

(defun ee-prev-sexp-in-comment ()
  (interactive)
  (ee-prev-sexp-sub (ee-sexp-in-comment-regexp)))

(global-set-key "\M-F" 'ee-next-sexp-in-comment)
(global-set-key "\M-B" 'ee-prev-sexp-in-comment)
(global-set-key "\M-p" 'ee-prev-sexp-in-comment)
(global-set-key "\M-n" 'ee-next-sexp-in-comment)

;;;; anchor movement
;;;; [2005/06/12] revised

(defun ee-next-anchor ()
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (unless (re-search-forward (format ee-anchor-format ".+") nil t)
      (goto-char pt)
      (error "anchor not found"))))

(defun ee-prev-anchor ()
  (interactive)
  (let ((p (point)))
    (when (eq (char-before) (string-to-char (substring ee-anchor-format -1)) )
      (beginning-of-line))
    (if (re-search-backward (format ee-anchor-format ".+") nil t)
        (goto-char (match-end 0))
      (goto-char p)
      (error "anchor not found"))))

;;[2003/08/10] 
;; eev anchor
(global-set-key "\M-P" 'ee-prev-anchor)
(global-set-key "\M-N" 'ee-next-anchor)

;; save-sexp
(defvar ee-saved-sexp nil)
(defun ee-save-sexp-eol ()
  (interactive)
  (setq ee-saved-sexp (buffer-substring-no-properties
		       (+ (length ee-hyperlink-prefix) (ee-bol)) (ee-eol)))
  (bury-buffer))

(defun ee-comment-prefix ()
  (concat (replace-regexp-in-string " +$" "" (or comment-start "#")) " "))

;; replace yank
(defun ee-yank3 (arg)
  (interactive "P")
  (when ee-saved-sexp
    (kill-new (concat (ee-comment-prefix) ee-saved-sexp "\n"))
    (setq ee-saved-sexp nil))
  (yank arg))
(global-set-key "\C-y" 'ee-yank3)

;;;; [2005/06/02] set escript-mode in *Elisp hyperlinks*
(defadvice find-elinks (after escript-mode activate)
  (escript-mode)
  (view-mode-writable))

;;;; [2005/05/23] set view-mode
;; (find-efile "view.el")
(defun view-mode-writable ()
  (interactive)
  (view-mode 1)
  (setq buffer-read-only nil))

(defmacro ee-view-mode-writable-advice (f)
  `(defadvice ,f (after read-only activate)
     (view-mode-writable)))

(ee-view-mode-writable-advice find-estring)
(ee-view-mode-writable-advice find-sh)



;; Local Variables:
;; coding:           raw-text-unix
;; ee-anchor-format: "%s"
;; modes:            (fundamental-mode emacs-lisp-mode escript-mode)
;; End:
