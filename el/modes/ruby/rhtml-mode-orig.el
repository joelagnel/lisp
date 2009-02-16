;;; rhtml-mode

;; Sets up an rhtml mode for embedded Ruby (ERB)
;; (C) 2006 Phil Hagelberg

(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defvar rhtml-font-lock-keywords
  '(("<%=?" . font-lock-preprocessor-face)
    ("%>" . font-lock-preprocessor-face)

    ("\\(<%\\#[^%]*%>\\)" . (1 font-lock-comment-face t nil))

    ("<%[=]?\\([^%]*\\)%>" . (1 'erb-face keep t))

    ("<\\(/?[[:alnum:]][-_.:[:alnum:]]*\\)" 1 font-lock-function-name-face) ; tags
    ("\\([a-zA-Z0-9]*[ ]?\\)=" 1 font-lock-variable-name-face) ; attributes

    ("\\(\"[^\"\n]*\"\\)" .(1 font-lock-string-face prepend nil))
    ("\\('[^'\n]*'\\)" .(1 font-lock-string-face prepend nil))
    ("\\(<!--.*?-->\\)" . (1 font-lock-comment-face prepend nil))))

(defvar rhtml-in-erb-keywords
  '(("\\([A-Z][0-9a-zA-Z_]*\\)" . font-lock-type-face-erb)
    ("[^_]\\<\\(alias\\|and\\|begin\\|break\\|case\\|catch\\|class\\|def\\|do\\|elsif\\|else\\|fail\\|ensure\\|for\\|end\\|if\\|in\\|module\\|next\\|not\\|or\\|raise\\|redo\\|rescue\\|retry\\|return\\|then\\|throw\\|super\\|unless\\|undef\\|until\\|when\\|while\\|yield\\|render\\)\\>[^_]" .
     font-lock-keyword-face-erb)
    ("\\(@[0-9a-zA-Z_]*\\)" . font-lock-variable-name-face-erb)
    ("\\(:[0-9a-zA-Z_]*\\)" . font-lock-constant-face-erb)))


(defmacro each-search (re &rest body)
  `(save-excursion
     (beginning-of-buffer)
     (while (re-search-forward ,re nil t)
       (let ((start (match-beginning 0))
	     (end (match-end 0)))
	 ,@body))))

(defun in-erb (point)
  (interactive)
  (save-excursion
    (let ((erb-begin (re-search-backward "<%=?" nil t))
	  (erb-end (re-search-forward "%>" nil t)))
      (and erb-begin erb-end
	   (> erb-end point)
	   (< erb-begin point)))))
    

(defun rhtml-fontify-region (begin end &optional loudly)
  (interactive "r")
  (setq case-fold-search nil)
  (font-lock-fontify-keywords-region begin end)
  (mapc (lambda (matcher) (each-search (car matcher)
				       (if (in-erb start) (put-text-property start end 'face (cdr matcher)))))
	rhtml-in-erb-keywords))

(defun rhtml-fontify-buffer ()
  (interactive)
  (save-restriction
    (widen)
    (rhtml-fontify-region (point-min) (point-max))))

;; Set up ERB faces with proper background

(defcustom erb-background "grey18"
  "Background for embedded Ruby")

(defface erb-face
  `((t (:background "grey18")))
  "Basic face for Ruby embedded into HTML"
  :group 'basic-faces)

(mapc (lambda (faces)
	(copy-face (car faces) (cdr faces))
	(set-face-background (cdr faces) erb-background))
      '((font-lock-keyword-face . font-lock-keyword-face-erb)
	(font-lock-variable-name-face . font-lock-variable-name-face-erb)
	(font-lock-string-face . font-lock-string-face-erb)
	(font-lock-type-face . font-lock-type-face-erb)
	(font-lock-comment-face . font-lock-comment-face-erb)
	(font-lock-constant-face . font-lock-constant-face-erb)))


;; Handy RHTML functions

(defun rhtml-controller-name-from-view ()
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name "."))
	  "_controller.rb"))

(defun rhtml-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rhtml-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward (concat "def " action))
    (recenter)))

(defun extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (kill-region begin end)
  (find-file (concat "_" partial-name ".rhtml"))
  (yank)
  (pop-to-buffer nil)
  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))


;; Defining the mode

(define-derived-mode rhtml-mode
  html-mode "RHTML"
  "Embedded Ruby Mode (RHTML)"
  (interactive)
  (abbrev-mode)
  (setq font-lock-defaults '(rhtml-font-lock-keywords t))

  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'rhtml-fontify-region)

  (make-local-variable 'font-lock-fontify-buffer-function)
  (setq font-lock-fontify-buffer-function 'rhtml-fontify-buffer))

(define-key rhtml-mode-map
  "\C-c\C-v" 'rhtml-find-action)
(define-key rhtml-mode-map
  "\C-c\C-\M-f" 'find-file-in-project)

; for debugging font-lock
(global-set-key "\C-c\C-r" 'rhtml-mode-test)
(global-set-key "\C-x\C-\M-e" 'eval-defun)

(provide 'rhtml-mode)