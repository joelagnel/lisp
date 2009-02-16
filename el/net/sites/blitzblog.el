;; blitzblog XEmacs frontend.
;; Might work with FSF Emacs, too.

(require 'xml-rpc)
(require 'cl)

(defvar blitzblog-rpc-url
  "http://localhost:8000/rpc")

(defvar blitzblog-user "admin")
(defvar blitzblog-password "admin")


(defconst blitzblog-header-separator "-- Entry follows this line --")

(defun blitzblog-post (title cat body id)
  (xml-rpc-method-call blitzblog-rpc-url "blitzblog.postEntry"
		       blitzblog-user blitzblog-password
		       `(("title" . ,title)
			 ("category" . ,cat)
			 ("body" . ,body)
			 ("id" . ,(or id 0)))))

(defun blitzblog-narrow-to-headers ()
  (save-excursion
    (narrow-to-region (point-min)
		      (progn (blitzblog-goto-marker) (point)))))

(defun blitzblog-delete-headers ()
  (save-restriction
    (blitzblog-narrow-to-headers)
    (delete-region (point-min) (point-max))))

(defun blitzblog-narrow-to-body ()
  (save-excursion
    (narrow-to-region (progn
			(blitzblog-goto-marker)
			(forward-line)
			(point))
		      (point-max))))

(defun blitzblog-goto-marker ()
  (save-match-data
    (goto-char (point-min))
    (re-search-forward (regexp-quote blitzblog-header-separator) nil 'move)
    (beginning-of-line)))

(defun blitzblog-insert-header (name value)
  (save-excursion
    (cond
      ((blitzblog-goto-header name)
       (delete-region (point) (progn (end-of-line) (point)))
       (insert value))
      (t
       (insert (format "%s: %s\n" name value))))))

(defun blitzblog-goto-header (header)
  "Goes to header or returns NIL if no such header is found."
  (save-restriction
    (save-match-data
      (narrow-to-region
       (goto-char (point-min))
       (progn
	 (re-search-forward
	  (concat "^" (regexp-quote blitzblog-header-separator) "$"))
	 (match-beginning 0)))
      (goto-char (point-min))
      (re-search-forward (format "^%s: *" (regexp-quote header)) nil 'move))))

(defun blitzblog-get-header (name)
  (save-excursion
    (blitzblog-goto-header name)
    (buffer-substring (point)
		      (progn (end-of-line) (point)))))

(defun blitzblog-setup-layout ()
  (goto-char (point-min))
  (insert (format "%s\n" blitzblog-header-separator))
  (blitzblog-insert-header "Title" "")
  (blitzblog-insert-header "Category" "Misc")
  (blitzblog-goto-header "Title"))

(defun blitzblog-ensure-buffer ()
  "Ensure that the *blitzblog* buffer exists"
  (save-excursion
    (unless (get-buffer "*blitzblog*")
      (let ((blog-buffer (get-buffer-create "*blitzblog*")))
	(set-buffer blog-buffer)
	(blitzblog-mode)))))

(defun blitzblog-start-entry ()
  "Start a new entry."
  (interactive)
  (blitzblog-ensure-buffer)
  (switch-to-buffer "*blitzblog*")
  (blitzblog-setup-layout))

(defun blitzblog-fetch-entry (id)
  "Fetch an article"
  (interactive "nEntry id to edit: ")
  (let ((entry (xml-rpc-method-call blitzblog-rpc-url "blitzblog.getEntry"
				    blitzblog-user blitzblog-password
				    id)))
    (save-restriction
      (widen)
      (blitzblog-narrow-to-body)
      (delete-region (point-min) (point-max))
      (insert (cdr (assoc "body" entry))))
    (blitzblog-delete-headers)
    (blitzblog-insert-header "Title" (cdr (assoc "title" entry)))
    (blitzblog-insert-header "Category" (cdr (assoc "category" entry)))
    (blitzblog-insert-header "Id" (number-to-string id))))

(defun blitzblog-set-title (title)
  (interactive "sTitle of blog entry: ")
  (blitzblog-insert-header "Title" title))

(defun blitzblog-set-category (cat)
  (interactive "sCategory of blog entry: ")
  (blitzblog-insert-category "Category" cat))

(defun blitzblog-save-entry ()
  "Save entry."
  (interactive)
  ;; TODO: Save it for later in ~/.blitzblog or something.
  )

(defun blitzblog-do-post ()
  (interactive)
  (switch-to-buffer "*blitzblog*")
  (when (= 0 (length (blitzblog-get-header "Title")))
    (blitzblog-set-title (read-string "Title of blog entry: ")))
  (let ((content (save-restriction
		   (blitzblog-narrow-to-body)
		   (buffer-substring))))
    (blitzblog-post (blitzblog-get-header "Title")
		    (blitzblog-get-header "Category")
		    content
		    ;; string-to-number happens to return 0 if the
		    ;; string is empty.
		    (string-to-number (blitzblog-get-header "Id")))
    (message "Posted '%s' to %s."
	     (blitzblog-get-header "Title")
	     blitzblog-rpc-url))
  (kill-buffer "*blitzblog*"))

;; Mode implementation

(defvar blitzblog-mode-hook nil
  "Hook run when entering blitzblog mode.")

(defvar blitzblog-mode-map (make-sparse-keymap))

(defvar blitzblog-font-lock-keywords
  '(("^\\([A-Za-z]+:\\)" . font-lock-keyword-face)
    ("\\(/?<[^>]+>\\)" . font-lock-function-name-face)))

(put 'blitzblog-mode 'font-lock-defaults '(blitzblog-font-lock-keywords t))

(define-key blitzblog-mode-map '[(control ?c) (control ?c)]
  'blitzblog-do-post)

(defun blitzblog-mode ()
  "Mode for editing and posting blitzblog entries.

In addition to any hooks its parent mode `html-mode' might have run,
this mode runs the hook `blitzblog-mode-hook', as the final step
during initialization.

\\{blitzblog-mode-map}"
  (interactive)
  (delay-mode-hooks
   (let ((html-helper-build-new-buffer nil))
     (html-mode))
   (setq major-mode 'blitzblog-mode)
   (setq mode-name "blitzblog")
   ;; Keymap setup
   (unless
       (keymap-parent blitzblog-mode-map)
     (set-keymap-parents
      blitzblog-mode-map
      (list (current-local-map))))
   (use-local-map blitzblog-mode-map))
  (message "Let's blog!")
  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'blitzblog-mode-hook)
      (run-hooks 'blitzblog-mode-hook)))

;; Buffer local variables

(make-variable-buffer-local 'blitzblog-current-id)
(make-variable-buffer-local 'blitzblog-current-title)
(make-variable-buffer-local 'blitzblog-current-category)

(provide 'blitzblog)

;; EOF
