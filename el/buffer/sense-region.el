;;; sense-region.el - minor mode to toggle region and rectangle.
;;; $Id: sense-region.el,v 1.9 2002/10/16 13:47:14 komatsu Exp $
;;;
;;; AUTHOR:  Hiroyuki KOMATSU <komatsu@taiyaki.org>
;;; LICENSE: GPL2
;;; ORIGINAL-SOURCE: http://www.taiyaki.org/elisp/sense-region/ (in Japanese)
;;;

;;; ------------------------------------------------------------
;;; mell (basic)
;;; ------------------------------------------------------------

;;; Checking Emacs or XEmacs.
(if (not (boundp 'running-xemacs))
    (defconst running-xemacs nil))

(defun mell-check-value (value)
  (and (boundp value)
       (symbol-value value)))

(defun mell-require (feature &optional filename noerror)
  (or (featurep feature)
      (if noerror
	  (condition-case nil
	      (require feature filename)
	    (file-error nil)
	    )
	(require feature filename)
	)))

(defun mell-defvar (symbol value &optional doc-string)
  (if (not (boundp symbol))
      (set symbol value))
  (if doc-string
      (put symbol 'variable-documentation doc-string))
  symbol)

(defun mell-defvar-locally (symbol initvalue &optional docstring)
  (mell-defvar symbol initvalue docstring)
  (make-variable-buffer-local symbol)
  symbol)

(defun mell-column-at-point (point &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (goto-char point)
    (current-column)
    ))

(defun mell-point-at-column (column &optional point buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (and point (goto-char point))
    (move-to-column column)
    (point)
    ))

(defun mell-point-at-eol (&optional point)
  (save-excursion
    (and point (goto-char point))
    (end-of-line)
    (point)
    ))

;;; add-local-hook
(or (fboundp 'add-local-hook)
    (defun add-local-hook (hook function &optional append)
      (make-local-hook hook)
      (add-hook hook function append t))
    )

;;; remove-local-hook
(or (fboundp 'remove-local-hook)
    (defun remove-local-hook (hook function)
      (if (local-variable-p hook (current-buffer))
	  (remove-hook hook function t)))
    )

;; mell-marker
(defun mell-marker-make (&optional position buffer type)
  (let ((marker (make-marker)))
    (or position
	(setq position (point)))
    (set-marker marker position buffer)
    (set-marker-insertion-type marker type)
    marker
    ))

(defun mell-marker-set (marker &optional position buffer type)
  (or (markerp (eval marker))
      (set marker (make-marker)))
  (or position
      (setq position (point)))
  (set-marker (eval marker) position buffer)
  (set-marker-insertion-type (eval marker) type)
  (eval marker)
  )

;;; ------------------------------------------------------------
;;; mell-mode
;;; ------------------------------------------------------------

;;; This function requires mell-alist.
(defun mell-set-minor-mode (name modeline &optional key-map)
  (make-variable-buffer-local name)
  (setq minor-mode-alist
	(mell-alist-add minor-mode-alist (list name modeline)))
  (and key-map
       (setq minor-mode-map-alist
	     (mell-alist-add minor-mode-map-alist (cons name key-map)))
       )
  )

;; ------------------------------------------------------------
;; mell-alist
;; ------------------------------------------------------------
(defun mell-alist-add! (alist new-cons)
  (if (null alist)
      (error "mell-alist-add! can not deal nil as an alist.")
    (let ((current-cons (assoc (car new-cons) alist)))
      (if current-cons
	  (setcdr current-cons (cdr new-cons))
	(if (car alist)
	    (nconc alist (list new-cons))
	  (setcar alist new-cons))
	)
      alist)))
  
(defun mell-alist-add (alist new-cons)
  (if (null alist)
      (list new-cons)
    (let ((return-alist (copy-alist alist)))
      (mell-alist-add! return-alist new-cons)
      return-alist)))
  
(defun mell-alist-delete (alist key)
  (if key
      (let (return-alist)
	(mapcar '(lambda (x)
		   (or (equal key (car x))
		       (setq return-alist (cons x return-alist))))
		alist)
	(if return-alist
	    (reverse return-alist)
	  (list nil)))
    alist)
  )

;;; ------------------------------------------------------------
;;; mell-advice
;;; ------------------------------------------------------------

(defmacro mell-advice-strip (advice-list &rest body)
  `(progn
     (apply 'ad-disable-advice ,advice-list)
     (ad-activate (car ,advice-list))
     ,@body
     (apply 'ad-enable-advice ,advice-list)
     (ad-activate (car ,advice-list))
     ))

;;; ------------------------------------------------------------
;;; mell-sign
;;; ------------------------------------------------------------
(mell-require 'overlay nil t)

(defcustom mell-sign-blink-time 0.5
  "A highlighting time for mell-sign-<region>-blink.")

(defun mell-sign-region-highlight (start end &optional buffer face)
  (save-excursion
    (or buffer (setq buffer (current-buffer)))
    (prog1
	(setq overlay (make-overlay start end buffer nil t))
      (overlay-put overlay 'face (or face 'highlight))
      (overlay-put overlay 'evaporate t)
      )))

(defun mell-sign-region-highlight-off (overlay)
  (delete-overlay overlay)
  )

(defun mell-sign-region-blink (start end &optional buffer face)
  (let ((overlay (mell-sign-region-highlight start end buffer face)))
    (sit-for mell-sign-blink-time)
    (mell-sign-region-highlight-off overlay)
    ))

(defun mell-sign-rectangle-highlight (start end &optional buffer face)
  (mell-sign-selection-highlight
   (mell-region-get-visible-rectangle-list start end) buffer face)
  )

(defun mell-sign-rectangle-highlight-off (overlay-list)
  (mapcar
   '(lambda (overlay)
      (delete-overlay overlay))
   overlay-list)
  )
  
(defun mell-sign-rectangle-blink (start end &optional buffer face)
  (let ((overlay (mell-sign-rectangle-highlight start end buffer face)))
    (sit-for mell-sign-blink-time)
    (mell-sign-rectangle-highlight-off overlay)
    ))

(defun mell-sign-selection-highlight (selection-list &optional buffer face)
  (save-excursion
    (or buffer (setq buffer (current-buffer)))
    (mapcar
     '(lambda (region)
	(prog1
	    (setq overlay
		  (make-overlay (car region) (cdr region) buffer nil t))
	  (overlay-put overlay 'face (or face 'highlight))
	  (overlay-put overlay 'evaporate t)
	  ))
     selection-list)
    ))

(defun mell-sign-selection-highlight-off (overlay-list)
  (mapcar
   '(lambda (overlay)
      (delete-overlay overlay))
   overlay-list)
  )
  
(defun mell-sign-selection-blink (selection-list &optional buffer face)
  (let ((overlay (mell-sign-selection-highlight selection-list buffer face)))
    (sit-for mell-sign-blink-time)
    (mell-sign-selection-highlight-off overlay)
    ))

(defun mell-sign-reset-face (face)
  (if running-xemacs
      (reset-face face)
    (set-face-foreground face nil)
    (set-face-background face nil)
    (set-face-background-pixmap face nil)
    (set-face-underline-p face nil)
    (set-face-stipple face nil)
    ))

;;; ------------------------------------------------------------
;;; mell-region
;;; ------------------------------------------------------------

;; mell-region-face
(if running-xemacs
    (defconst mell-region-face 'zmacs-region)
  (defconst mell-region-face 'region)
  )

;; mell-region-active-p
(if running-xemacs
    (defun mell-region-active-p ()
      (region-active-p))
  (defun mell-region-active-p ()
    (mell-check-value 'mark-active))
  )

;; mell-transient-mode-p
(if running-xemacs
    (defun mell-transient-mode-p ()
      (mell-check-value 'zmacs-regions))
  (defun mell-transient-mode-p ()
    (mell-check-value 'transient-mark-mode))
  )

(defvar mell-transient-mode-last-value nil)

;; mell-transient-mode-on
(if running-xemacs
    (defun mell-transient-mode-on ()
      (setq mell-transient-mode-last-value zmacs-regions)
      (setq zmacs-regions t))
  (defun mell-transient-mode-on ()
    (setq mell-transient-mode-last-value transient-mark-mode)
    (setq transient-mark-mode t))
  )

;; mell-transient-mode-off
(if running-xemacs
    (defun mell-transient-mode-off ()
      (setq zmacs-regions mell-transient-mode-last-value))
  (defun mell-transient-mode-off ()
    (setq transient-mark-mode mell-transient-mode-last-value))
  )

;; Define mell-transient-region-active-p
(defun mell-transient-region-active-p ()
  (and (mell-transient-mode-p)
       (mell-region-active-p)))

(defun mell-transient-region-stay ()
  (and running-xemacs
       (setq zmacs-region-stays t))
  )

(defun mell-transient-region-deactivate ()
  (if running-xemacs
      (zmacs-deactivate-region)
    (setq deactivate-mark t)
  ))

(defun mell-transient-region-activate ()
  (if running-xemacs
      (zmacs-activate-region)
    (setq mark-active t)
  ))

(defun mell-region-get-region-line-list (start end &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let (region-line-list
	  (point-start (min start end))
	  (point-min   (min start end))
	  (point-max   (max start end)))
      (goto-char point-min)
      (while (< point-start point-max)
	(end-of-line)
	(setq region-line-list (cons (cons point-start (min point-max (point)))
				     region-line-list))
	(or (= (point) (point-max))
	    (forward-char 1))
	(setq point-start (point))
	)
      (reverse region-line-list)
      )))

(defun mell-region-get-rectangle-list (start end &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let* (rectangle-alist
	   (column-min (min (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (column-max (max (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (point-min (min (mell-point-at-column column-min start)
			   (mell-point-at-column column-min end)))
	   (point-max (max (mell-point-at-column column-max start)
			   (mell-point-at-column column-max end)))
	   )
      (goto-char point-min)
      (while (< (point) point-max)
	(move-to-column column-min)
	(setq rectangle-alist 
	      (cons (cons (point) (mell-point-at-column column-max))
		    rectangle-alist))
	(forward-line 1)
	)
      (reverse rectangle-alist)
      )))

(defun mell-region-get-visible-rectangle-list (start end &optional buffer)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let* (rectangle-alist
	   (column-min (min (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (column-max (max (mell-column-at-point start)
			    (mell-column-at-point end)))
	   (point-min (min (mell-point-at-column column-min start)
			   (mell-point-at-column column-min end)))
	   (point-max (max (mell-point-at-column column-max start)
			   (mell-point-at-column column-max end)))
	   )
      (goto-char (max point-min (window-start)))
      (while (< (point) (min point-max (window-end)))
	(move-to-column column-min)
	(setq rectangle-alist 
	      (cons (cons (point) (mell-point-at-column column-max))
		    rectangle-alist))
	(forward-line 1)
	)
      (reverse rectangle-alist)
      )))
      
(put 'mell-region-rectangle-while 'lisp-indent-function 1)
(defmacro mell-region-rectangle-while (rectangle &rest body)
  `(let ((rectangle-markers
	  (mell-region-get-rectangle-marker-list
	   (nth 0 ,rectangle) (nth 1 ,rectangle) (nth 2 ,rectangle)))
	 )
     (mapcar
      (lambda (region)
	(let ((line-beginning (car region))
	      (line-end (cdr region)))
	  ,@body
	  ))
      rectangle-markers)
     (mapcar
      (lambda (region)
	(set-marker (car region) nil)
	(set-marker (cdr region) nil)
	)
      rectangle-markers)
     ))

(put 'mell-region-selection-while 'lisp-indent-function 1)
(defmacro mell-region-selection-while (selection-alist &rest body)
  `(let ((selection-markers
	  (mell-region-get-selection-marker-list ,selection-alist)
	  ))
     (mapcar
      (lambda (region)
	(let ((line-beginning (car region))
	      (line-end (cdr region)))
	  ,@body
	  ))
      selection-markers)
     (mapcar
      (lambda (region)
	(set-marker (car region) nil)
	(set-marker (cdr region) nil)
	)
      selection-markers)
     ))

(defun mell-region-get-selection-marker-list (selection-alist &optional buffer)
  (and buffer (set-buffer))
  (mapcar
   '(lambda (region)
      (cons (mell-marker-make (car region)) (mell-marker-make (cdr region)))
      )
   selection-alist)
  )

(defun mell-region-get-rectangle-marker-list (start end &optional buffer)
  (and buffer (set-buffer))
  (mapcar
   '(lambda (region)
      (cons (mell-marker-make (car region)) (mell-marker-make (cdr region)))
      )
   (mell-region-get-rectangle-list start end buffer))
  )

(defun mell-region-rectangle-right-edge-p (start end)
  (save-excursion
    (let ((list (mell-region-get-rectangle-list start end))
	  (result t))
      (while (and list
		  (progn (goto-char (cdr (car list)))
			 (eolp)))
	(setq list (cdr list))
	)
      (null list)
      )))

;;; ------------------------------------------------------------
;;; mell-string
;;; ------------------------------------------------------------
(defun mell-string-split (string regexp)
  (let ((start 0) match-list splited-list)
    (while (string-match regexp string start)
      (setq match-list
	    (append match-list (list (match-beginning 0) (match-end 0))))
      (setq start (match-end 0))
      )
    (setq match-list (append '(0) match-list (list (length string))))
    (while match-list
      (setq splited-list 
	    (cons (substring string (nth 0 match-list) (nth 1 match-list))
		  splited-list))
      (setq match-list (nthcdr 2 match-list))
      )
    (reverse splited-list)))

(defun mell-string-replace (target-string from-regexp to-string)
  (if (string-match from-regexp target-string)
      (setq target-string
	    (mapconcat '(lambda (x) x)
		       (mell-string-split target-string from-regexp)
		       to-string))
    )
  target-string)

;;; ------------------------------------------------------------
;;; sense-region-mode
;;; ------------------------------------------------------------

;(global-set-key "\C-c\C-r" 'sense-region-replace-regexp)
(global-set-key "\C-cr" 'sense-region-replace-regexp)
(global-set-key "\M-r" 'sense-region-replace-regexp)

(defcustom sense-region-on-hook nil
  "Function or functions called when sense-region-on is executed.")
(defcustom sense-region-off-hook nil
  "Function or functions called when sense-region-off is executed.")
(defcustom sense-region-adviced-functions
  '((set-mark-command . sense-region-set-mark)
    kill-ring-save kill-region yank yank-rectangle
    isearch-forward open-line
    comment-region indent-for-tab-command query-replace query-replace-regexp)
  "List of defadviced functions for sense-region."
  )

(defcustom sense-region-url-regexp 
  (or (mell-check-value 'mime-browse-url-regexp)
      "http://[-a-zA-Z0-9.:#~_+/%&?]+")
  "Regexp for URL")
(defcustom sense-region-email-regexp "[-a-zA-Z0-9.:_]+@[-a-zA-Z0-9.:_]+"
  "Regexp for Email")
(defcustom sense-region-yank-rectangle-style 'confirm
  "")

(if (not (facep 'sense-region-region-face))
    (copy-face mell-region-face 'sense-region-region-face))
(if (not (facep 'sense-region-rectangle-face))
    (copy-face mell-region-face 'sense-region-rectangle-face))
(if (not (facep 'sense-region-offset-face))
    (copy-face mell-region-face 'sense-region-offset-face))
(if (not (facep 'sense-region-selection-highlight-face))
    (copy-face 'highlight 'sense-region-selection-highlight-face))
(if (not (facep 'sense-region-selection-base-face))
    (progn
      (defcustom sense-region-selection-base-face
	(make-face 'sense-region-selection-base-face)
	"Face for sense-region mode")
      (set-face-background sense-region-selection-base-face "#666688")
      ))

;; 変数が増えたら, sense-region-save-region も変更する.
(defvar sense-region-mode             nil)
(defvar sense-region-overlay-list     nil)
(defvar sense-region-face             nil)
(mell-defvar-locally 'sense-region-status 'region)
(defvar sense-region-last-status 'region)
(defvar sense-region-top-of-kill-ring nil)
(mell-defvar-locally 'sense-region-track-status nil)
(defvar sense-region-selection-list nil)
(defvar sense-region-selection-regexp "")
(defvar killed-rectangle nil)

(defadvice set-mark-command (around sense-region-set-mark disable)
  (if (and (mell-transient-region-active-p)
	   sense-region-mode)
      (if (and (eq last-command this-command)
	       (or (and (eq (region-beginning) (region-end))
			(eq sense-region-status 'region))
		   sense-region-track-status))
	  (sense-region-track)
	(setq sense-region-track-status nil)
	(sense-region-toggle))
    (setq sense-region-track-status nil)
    ad-do-it
    )
  (mell-transient-region-stay)
  )

(defun sense-region-track (&optional position)
  (cond
   ((eq sense-region-track-status nil)
    (sense-region-set-word position)
    (setq sense-region-track-status 'word))
   ((eq sense-region-track-status 'word)
    (let ((start (region-beginning)))
      (cond ((sense-region-set-url position)
	     (setq sense-region-track-status 'url))
	    ((sense-region-set-email position)
	     (setq sense-region-track-status 'email))
	    ((sense-region-set-symbol position (region-beginning))
	     (setq sense-region-track-status 'next-word))
	    (t
	     (sense-region-set-next-word position)
	     (setq sense-region-track-status 'next-word))
	    )
;      (set-mark (min (region-beginning) start))
      ))
   ((eq sense-region-track-status 'url)
    (sense-region-to-selection)
    'url)
   ((eq sense-region-track-status 'email)
    (sense-region-to-selection)
    'email)
   ((eq sense-region-track-status 'next-word)
    (sense-region-set-next-word position)
    'next-word)
   (t
    (message (format "sense-region-track: Wrong type status `%S'"
		     sense-region-track-status))
    (sense-region-set-word position)
    (setq sense-region-track-status 'word))
   )
  (mell-transient-region-stay)
  )

(defun sense-region-set-word (&optional position) ; mell 行きかな?
  (interactive)
  (or position (setq position (point)))
  (goto-char position)
  (let ((syntax (string (char-syntax (char-after)))))
    (if (member syntax '("w" " "))
	(skip-syntax-backward syntax))
    (set-mark (point))
    (if (string= syntax " ")
	(skip-syntax-forward " ")
      (sense-region-forward-word))
    (mell-transient-region-stay)
    t))

(defun sense-region-set-next-word (&optional position) ; mell 行きかな?
  (interactive)
  (or position (setq position (region-end)))
  (goto-char position)
  (sense-region-forward-word)
  (mell-transient-region-stay)
  t)


(defun sense-region-forward-word ()
  (cond
   ((eolp)
    (skip-chars-forward " \t\n")
    (beginning-of-line))
   ((= (char-syntax (char-after)) ?\()
    (forward-sexp 1))
   ((= (char-syntax (char-after)) ?\<)
    (forward-comment 1)
    (and (bolp) (backward-char 1)))
   ((= (char-syntax (char-after)) ?\ )
    (cond
     ((bolp)
      (skip-syntax-forward " "))
     (t
      (skip-syntax-forward " ")
      (cond
       ((member (char-syntax (char-after)) '(?w ?_))
	(skip-syntax-forward "w_"))
       ((= (char-syntax (char-after)) ?\()
	(forward-sexp 1))
       ((= (char-syntax (char-after)) ?\")
	(re-search-forward
	 (concat "[^\\]\\(\\\\\\\\\\)*" (string (char-after)))))
       ))
     ))
   ((= (char-syntax (char-after)) ?\")
    (re-search-forward
     (concat "[^\\]\\(\\\\\\\\\\)*" (string (char-after)))))
   (t
    (skip-syntax-forward (string (char-syntax (char-after)))))
   ))

(defun sense-region-set-symbol (&optional position start) ; mell 行きかな?
  (interactive)
  (or position (setq position (point)))
  (or start    (setq start    (point)))
  (goto-char position)

  (mell-transient-region-stay)
  (let (symbol-start)
    (skip-syntax-backward "w_")
    (setq symbol-start (point))
    (skip-syntax-forward  "w_")
    (if (or (and (< symbol-start start) (>= (point) position))
	    (and (> (point) position)   (<= symbol-start start)))
	(progn
	  (set-mark symbol-start)
	  t)
      (goto-char position)
      nil)
    ))

(defun sense-region-set-url (&optional position) ; mell 行きかな?
  (interactive)
  (or position (setq position (point)))
  (goto-char position)

  (mell-transient-region-stay)
  (save-match-data
    (if (and (< (skip-chars-backward "-a-zA-Z0-9.:#~_+/") 0)
	     (looking-at "http:"))
	(progn
	  (set-mark (point))
	  (re-search-forward "[-a-zA-Z0-9.:#~_+/]+")
	  t)
      (goto-char position)
      nil)
    ))

(defun sense-region-set-email (&optional position) ; mell 行きかな?
  (interactive)
  (or position (setq position (point)))
  (goto-char position)

  (mell-transient-region-stay)
  (save-match-data
    (if (and (< (skip-chars-backward "-a-zA-Z0-9.:@_") 0)
	     (looking-at "[-a-zA-Z0-9.:_]+@[-a-zA-Z0-9.:_]+"))
	(progn
	  (set-mark (point))
	  (re-search-forward "[-a-zA-Z0-9.:@_]+")
	  t)
      (goto-char position)
      nil)
    ))

(defun sense-region-redisplay ()
  (and t
       (sense-region-auto-switch))
  (if (window-minibuffer-p (selected-window))
      nil ; Do nothing.
    (mell-sign-selection-highlight-off sense-region-overlay-list)
    (if (mell-transient-region-active-p)
	(cond
	 ((eq sense-region-status 'rectangle)
	  (setq sense-region-overlay-list
		(mell-sign-rectangle-highlight (region-beginning) (region-end)
					       nil 'sense-region-region-face))
	  )
	 ((eq sense-region-status 'offset)
	  (setq sense-region-selection-list (sense-region-get-offset-list))
	  (setq sense-region-overlay-list
		(mell-sign-selection-highlight sense-region-selection-list
					       nil 'sense-region-offset-face))
	  )
	 ((eq sense-region-status 'selection)
	  (setq sense-region-selection-list
		(sense-region-get-regexp-selection-list
		 sense-region-selection-regexp))
	  (setq sense-region-overlay-list
		(append
		 ;; non-selected words
		 (mell-sign-selection-highlight
		  (list (cons (region-beginning) (region-end)))
		  nil 'sense-region-selection-base-face)
		 ;; selected words
		 (mell-sign-selection-highlight 
		  sense-region-selection-list
		  nil 'isearch)
		 ))
	  )
	 (t
	  (sense-region-to-region)
	  ))
      (sense-region-to-region)
      )))

(defun sense-region-auto-switch ()
  (cond
   ((window-minibuffer-p (selected-window))
    nil) ; Do nothing.
   ((not (mell-transient-region-active-p))
    nil) ; Do nothing.
   ((eq sense-region-status 'rectangle)
    (if (= (mell-column-at-point (mark)) (mell-column-at-point (point)))
	(progn
	  (sense-region-to-offset)
	  (setq sense-region-status-history
		(cons 'rectangle sense-region-status-history))
	  )))
   ((eq sense-region-status 'offset)
    (if (/= (mell-column-at-point (mark)) (mell-column-at-point (point)))
	(sense-region-to-previous-status)
      ))
   (t
    (setq sense-region-status-history nil)
    )
   ))

(defun sense-region-to-previous-status ()
  (let ((status (car sense-region-status-history)))
    (setq sense-region-status-history
	  (cdr sense-region-status-history))
    (cond
     ((eq status 'region)
      (sense-region-to-region))
     ((eq status 'rectangle)
      (sense-region-to-rectangle))
     ((eq status 'offset)
      (sense-region-to-offset))
     (t
      (message (format "Unkown sense-region-status: %S" status)))
     )))

(defun sense-region-on ()
  (interactive)
  (and sense-region-mode
       (sense-region-off))
  (setq sense-region-mode t)
  (mell-transient-mode-on)
  (copy-face mell-region-face 'sense-region-face)
  (add-hook 'post-command-hook 'sense-region-redisplay)

  (mapcar 
   '(lambda (function)
      (if (consp function)
	  (progn
	    (ad-enable-advice (car function) 'around (cdr function))
	    (ad-activate (car function))
	    )
	(ad-enable-advice function 'around 
			  (intern (format "sense-region-%S" function)))
	(ad-activate function)
	))
   sense-region-adviced-functions)
  (run-hooks 'sense-region-on-hook)
  (mell-transient-region-stay)
  )

(defun sense-region-off ()
  (interactive)
  (setq sense-region-mode nil)
  (mell-transient-mode-off)
  (mell-sign-rectangle-highlight-off sense-region-overlay-list)
  (remove-hook 'post-command-hook 'sense-region-redisplay)
  (copy-face 'sense-region-face mell-region-face)
  (mapcar 
   '(lambda (function)
      (if (consp function)
	  (progn
	    (ad-disable-advice (car function) 'around (cdr function))
	    (ad-activate (car function))
	    )
	(ad-disable-advice function 'around 
			  (intern (format "sense-region-%S" function)))
	(ad-activate function)
	))
   sense-region-adviced-functions)
  (run-hooks 'sense-region-off-hook)
  (sense-region-to-region)
;  (setq sense-region-status 'region)
  )


(defun sense-region-toggle ()
  (interactive)
  (cond
   ((eq sense-region-status 'region)
    (sense-region-to-rectangle))
   ((eq sense-region-status 'rectangle)
    (sense-region-to-region))
   ((eq sense-region-status 'offset)
    (sense-region-to-region))
   ((eq sense-region-status 'selection)
    (sense-region-to-region))
   ))

(defun sense-region-to-rectangle ()
  (interactive)
  (setq sense-region-status 'rectangle)
;  (copy-face mell-region-face 'sense-region-face)
;  (copy-face 'default mell-region-face)
  (mell-sign-reset-face mell-region-face)
  )  
  
(defun sense-region-to-region ()
  (interactive)
  (if (eq sense-region-status 'region)
      ;; Do Nothing.
      nil
    (setq sense-region-status 'region)
    (mell-sign-rectangle-highlight-off sense-region-overlay-list)
    (setq sense-region-overlay-list nil)
    (copy-face 'sense-region-face mell-region-face)
    ))

(defun sense-region-to-offset ()
  (interactive)
  (setq sense-region-status 'offset)
  ;; DO SOMETHING.
  )

(defun sense-region-to-selection (&optional regexp)
  (interactive)
  (setq sense-region-status 'selection)
  (mell-sign-reset-face mell-region-face)

  (setq sense-region-selection-regexp
	(cond
	 (regexp
	  regexp)
	 ((eq sense-region-track-status 'url)
	  sense-region-url-regexp)
	 ((eq sense-region-track-status 'email)
	  sense-region-email-regexp)
	 (t
	  "")
	 ))
  ;; DO SOMETHING.
  )

(defun sense-region-get-offset-list (&optional start end buffer)
  (save-excursion
    (or start (setq start (mark)))
    (or end   (setq end   (point)))
    (and buffer (set-buffer buffer))
    (let (selection-alist
	  (column (mell-column-at-point start))
	  (point-max (max start end)))
      (goto-char (min start end))
      (while (and (<= (point) point-max) (not (eobp)))
	(move-to-column column)
	(setq selection-alist
	      (cons (cons (point) (progn (end-of-line) (point)))
		    selection-alist))
	(forward-line 1)
	)
      (reverse selection-alist)
      )))

(defun sense-region-get-regexp-selection-list (regexp &optional
						      start end buffer)
  (save-excursion
    (or start (setq start (mark)))
    (or end   (setq end   (point)))
    (and buffer (set-buffer buffer))
    (let (selection-alist)
      (goto-char (min start end))
      (save-match-data
	(while (and (re-search-forward regexp (max start end) t)
		    (< (match-beginning 0) (match-end 0)))
	  (setq selection-alist 
		(cons (cons (match-beginning 0) (match-end 0))
		      selection-alist))
	  ))
      (reverse selection-alist)
      )))

(defun sense-region-get-selection-string-list (selection-alist
					       &optional buffer)
  (and buffer (set-buffer buffer))
  (mapcar
   '(lambda (region)
      (buffer-substring (car region) (cdr region))
      )
   selection-alist)
  )

(put 'sense-region-save-region 'lisp-indent-function 0)
(defmacro sense-region-save-region (&rest body)
  `(let ((mark (mark))
	 (active-p (mell-transient-region-active-p))
	 (cur-buffer (current-buffer))
	 global-mark-ring mark-ring
	 kill-ring kill-ring-yank-pointer
	 overlay
;	 (status sense-region-status)
	 sense-region-mode
	 sense-region-overlay-list
	 sense-region-face
	 (sense-region-status 'region)
	 (sense-region-last-status 'region)
	 (sense-region-selection-regexp "")
	 killed-rectangle)

     (if active-p
	 (setq overlay (mell-sign-region-highlight (mark) (point) 
						   nil mell-region-face))
       )
     (unwind-protect
	 (progn
	   ,@body)
       (if active-p
	   (progn
	     (mell-transient-region-activate)
	     (mell-sign-region-highlight-off overlay)
	     ))
       )
     ))

(defun sense-region-read-string (prompt
				 &optional initial history default inherit)
  (sense-region-save-region
    (read-string prompt initial history default inherit)
    ))

;;; ------------------------------------------------------------

(defun sense-region-set-selection-regexp (&optional regexp)
  (interactive)
  (setq regexp
	(or regexp
	    (sense-region-read-string
	     "Selection-Regexp: "
	     (if (eq sense-region-status 'selection)
		 (cond 
		  ((eq sense-region-track-status 'url)
		   "[url]")
		  ((eq sense-region-track-status 'email)
		   "[email]")
		  (sense-region-selection-regexp
		   (setq sense-region-track-status nil)
		   sense-region-selection-regexp)
		  )))
	    ))

  (if (string-match "\\[url\\]" regexp)
      (setq regexp
	    (mapconcat '(lambda (x) x) (split-string regexp "\\[url\\]")
		       sense-region-url-regexp))
    )
  (if (string-match "\\[e?-?mail\\]" regexp)
      (setq regexp
	    (mapconcat '(lambda (x) x) (split-string regexp "\\[e?-?mail\\]")
		       sense-region-email-regexp))
    )
  (sense-region-to-selection regexp)
  (mell-transient-region-stay)
  )

;;; ------------------------------------------------------------

(defadvice kill-ring-save (around sense-region-kill-ring-save disable)
  (if (mell-transient-region-active-p)
      (cond ((eq sense-region-status 'region)
	     ad-do-it
	     (setq sense-region-last-status 'region)
	     )
	    ((eq sense-region-status 'rectangle)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq killed-rectangle
		   (extract-rectangle (region-beginning) (region-end)))
	     (setq sense-region-last-status 'rectangle)
	     )
	    ((eq sense-region-status 'offset)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq killed-rectangle (sense-region-get-selection-string-list
				     (sense-region-get-offset-list)))
	     (setq sense-region-last-status 'offset)
	     )
	    ((eq sense-region-status 'selection)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq killed-rectangle (sense-region-get-selection-string-list
				     sense-region-selection-list))
	     (setq sense-region-last-status 'selection)
	     )
	    (t
	     (message (format "Unkown sense-region-status: %S"
			      sense-region-status))
	     ad-do-it
	     (setq sense-region-last-status 'region))
	     )
    ad-do-it
    (setq sense-region-last-status 'region))
  (mell-transient-region-deactivate)
  (sense-region-to-region)
;  (setq sense-region-status 'region)
  )

(defadvice kill-region (around sense-region-kill-region disable)
  (if (and (mell-transient-region-active-p) (interactive-p))
      (cond ((eq sense-region-status 'region)
	     ad-do-it
	     (setq sense-region-last-status 'region))
	    ((eq sense-region-status 'rectangle)
	     (call-interactively 'kill-rectangle)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq sense-region-last-status 'rectangle))
	    ((eq sense-region-status 'offset)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq killed-rectangle (sense-region-get-selection-string-list
				     (sense-region-get-offset-list)))
	     (mell-region-selection-while 
		 (sense-region-get-offset-list)
	       (delete-region line-beginning line-end)
	       )
	     (setq sense-region-last-status 'offset)
	     )
	    ((eq sense-region-status 'selection)
	     (setq sense-region-top-of-kill-ring (car kill-ring))
	     (setq killed-rectangle (sense-region-get-selection-string-list
				     sense-region-selection-list))
	     (mell-region-selection-while 
		 sense-region-selection-list
	       (delete-region line-beginning line-end)
	       )
	     (setq sense-region-last-status 'selection)
	     )
	    (t
	     (message (concat "Unkown sense-region-status: "
			      (symbol-name sense-region-status)))
	     ad-do-it
	     (setq sense-region-last-status 'region))
	    )
    ad-do-it
    (setq sense-region-last-status 'region))
  (mell-transient-region-deactivate)
  (sense-region-to-region)
;  (setq sense-region-status 'region)
  )

(defadvice yank (around sense-region-yank disable)
  (let ((begin (point))
	status overlay)
    (setq status
	  (cond ((eq sense-region-last-status 'region)
		 'region)
		((member sense-region-last-status 
			 '(rectangle offset selection))
		 (if (string= (car kill-ring) sense-region-top-of-kill-ring)
		     sense-region-last-status
		   'region))
		(t
		 (message (concat "Unkown sense-region-status: "
				  (symbol-name sense-region-status)))
		 'region)
		))
    (cond ((eq status 'region)
	   ad-do-it
	   (setq overlay (mell-sign-region-highlight begin (point)))
	   (sit-for 0.5)
	   (mell-sign-region-highlight-off overlay)
	   )
	  ((eq status 'rectangle)
	   (call-interactively 'yank-rectangle)
	   (setq overlay (mell-sign-rectangle-highlight begin (point)))
	   (sit-for 0.5)
	   (mell-sign-rectangle-highlight-off overlay)
	   )
	  ((eq status 'offset)
	   (call-interactively 'yank-rectangle)
	   (setq overlay (mell-sign-selection-highlight
			  (sense-region-get-yanked-selection begin)))
	   (sit-for 0.5)
	   (mell-sign-selection-highlight-off overlay)
	   )
	  ((eq status 'selection)
	   (call-interactively 'yank-rectangle)
	   (setq overlay (mell-sign-selection-highlight
			  (sense-region-get-yanked-selection begin)))
	   (sit-for 0.5)
	   (mell-sign-selection-highlight-off overlay)
	   )
	  )
    (mell-transient-region-deactivate)
    (sense-region-to-region)
;    (setq sense-region-status 'region)
    ))

(defadvice yank-rectangle (around sense-region-yank-rectangle disable)
  (let ((rect-len (length killed-rectangle))
	(cur-point (point))
	)
;    (if (and (or (and (bolp) (looking-at "[ \t]*$"))
    (if (and (or (bolp)
		 (eolp))
	     (cond
	      ((and (eq sense-region-yank-rectangle-style 'confirm)
		    (progn
		      (while (and (looking-at "[ \t]*$")
				  (> rect-len 0))
			(forward-line 1)
			(setq rect-len (1- rect-len))
			)
		      (goto-char cur-point)
		      (> rect-len 0))
		    )
	       (y-or-n-p "Insert Enter? "))
	      ((eq sense-region-yank-rectangle-style 'rectangle)
	       nil)
	      )
	     )
	(progn
	  (insert (make-string rect-len ?\n))
	  (goto-char cur-point)
	  )
      )
    ad-do-it
    ))

(defun sense-region-get-yanked-selection (begin)
  (save-excursion
    (goto-char begin)
    (let ((column (current-column)))
      (mapcar
       '(lambda (string)
	  (prog1 (cons (point) (+ (point) (length string)))
	    (forward-line 1)
	    (move-to-column column)
	    ))
       killed-rectangle)
      )))
  
;;; ------------------------------------------------------------

(defadvice isearch-forward (around sense-region-isearch-forward disable)
  (if (and sense-region-mode
	   (mell-transient-region-active-p))
      (call-interactively 'sense-region-set-selection-regexp)
    ad-do-it)
  )

(defadvice open-line (around sense-region-open-line disable)
  (if (mell-transient-region-active-p)
      (let ((open-lines (count-lines (region-beginning) (region-end))))
	(cond
	 ((eq sense-region-status 'region)
	  (goto-char (region-beginning))
	  (mell-advice-strip
	   '(open-line around sense-region-open-line)
	   (open-line open-lines))
	  )
	 ((eq sense-region-status 'rectangle)
	  (call-interactively 'open-rectangle))
	 ((eq sense-region-status 'offset)
	  (let ((topleft (mell-point-at-column 0 (region-beginning)))
		(topright (region-beginning))
		(bolp (bolp))
		killed-rectangle)
	    (setq killed-rectangle
		  (extract-rectangle topleft (region-end)))
	    (goto-char topleft)
	    (insert (make-string (if bolp (1+ open-lines) open-lines)
				 ?\n))
	    (goto-char topleft)
	    (mell-advice-strip
	     '(yank-rectangle around sense-region-yank-rectangle)
	     (yank-rectangle))
	    (goto-char topright)
	    ))
	 ((eq sense-region-status 'selection)
	  ad-do-it)
	 (t
	  (message (concat "Unkown sense-region-status: "
			   (symbol-name sense-region-status)))
	  ad-do-it)
	 ))
    ad-do-it)
  )

(defadvice comment-region (around sense-region-comment-region disable)
  (mell-advice-strip
   '(comment-region around sense-region-comment-region)
   (if (mell-transient-region-active-p)
       (cond ((eq sense-region-status 'region)
	      ad-do-it)
	     ((eq sense-region-status 'rectangle)
	      (call-interactively 'sense-region-comment-rectangle))
	     ((eq sense-region-status 'offset)
	      (call-interactively 'sense-region-comment-offset))
	     ((eq sense-region-status 'selection)
	      (sense-region-comment-selection (region-beginning) (region-end)
					      sense-region-selection-regexp))
	     (t
	      (message (concat "Unkown sense-region-status: "
			       (symbol-name sense-region-status)))
	      ad-do-it)
	     )
     ad-do-it)
   (mell-transient-region-deactivate)
   (sense-region-to-region)
;   (setq sense-region-status 'region)
   ))

(defun sense-region-comment-rectangle (start end &optional arg)
  (interactive "r\nP")
  (let* ((original-ce comment-end)
	 (comment-end "")
	 indent-next-line-p)
    (if (and (string= original-ce "")
	     (not (mell-region-rectangle-right-edge-p start end))
	     (y-or-n-p "Insert enter and indent? "))
	(progn
	  (setq indent-next-line-p t)
	  (if (and (not running-xemacs)
		   (>= (string-to-number emacs-version) 21)) ; FIX-ME: ADHOC!!!
	      (setq comment-end "")
	    (setq comment-end "\n")
	    ))
      (setq comment-end original-ce)
      )

    (mell-region-rectangle-while (list start end)
      (or (= (marker-position line-beginning) (marker-position line-end))
	  (comment-region line-beginning line-end arg))
      (if indent-next-line-p
	  (progn
	    (goto-char line-beginning)
	    (forward-line 1)
	    (indent-for-tab-command)
	    )
	))
    ))

(defun sense-region-comment-offset (start end &optional arg)
  (interactive "r\nP")
  (mell-region-selection-while (sense-region-get-offset-list start end)
      (or (= (marker-position line-beginning) (marker-position line-end))
	  (comment-region line-beginning line-end arg))
    ))

;;; TODO: Wrroy about where comment-end would be "".
(defun sense-region-comment-selection (start end regexp &optional arg)
  (interactive "r\nsString: \nP")
  (mell-region-selection-while
      (sense-region-get-regexp-selection-list regexp start end)
      (or (= (marker-position line-beginning) (marker-position line-end))
	  (comment-region line-beginning line-end arg))
    ))



(defadvice indent-for-tab-command (around sense-region-indent-for-tab-command
					  disable)
  (if (mell-transient-region-active-p)
      (cond ((eq sense-region-status 'region)
	     (call-interactively 'indent-region))
	   ((eq sense-region-status 'rectangle)
	     (if (functionp 'table-rectangle)
		 (call-interactively 'table-rectangle)
	       nil))
	   ((eq sense-region-status 'offset)
	     (if (functionp 'table-offset)
		 (call-interactively 'table-offset)
	       nil))
	   ((eq sense-region-status 'selection)
	    (message "Nothing to do"))
	   (t
	    (message (concat "Unkown sense-region-status: "
			     (symbol-name sense-region-status)))
	    ad-do-it)
	   )
    ad-do-it)
  (mell-transient-region-deactivate)
  (sense-region-to-region)
;  (setq sense-region-status 'region)
  )

(defadvice query-replace-regexp (around sense-region-query-replace-regexp
					disable)
  (if (mell-transient-region-active-p)
      (cond ((eq sense-region-status 'region)
	     ad-do-it)
	    ((eq sense-region-status 'rectangle)
	     (mell-region-rectangle-while
		 (list (region-beginning) (region-end))
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace-regexp
		      around sense-region-query-replace-regexp)
		    (eval '(query-replace-regexp (ad-get-arg 0) (ad-get-arg 1)
						 nil line-beginning line-end))
		    ))
		 ))
	     )
	    ((eq sense-region-status 'offset)
	     (mell-region-selection-while
		 (sense-region-get-offset-list)
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace-regexp
		      around sense-region-query-replace-regexp)
		    (eval '(query-replace-regexp (ad-get-arg 0) (ad-get-arg 1)
						 nil line-beginning line-end))
		    ))
		 ))
	     )
	    ((eq sense-region-status 'selection)
	     (mell-region-selection-while
		 sense-region-selection-list
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace-regexp
		      around sense-region-query-replace-regexp)
		    (eval '(query-replace-regexp (ad-get-arg 0) (ad-get-arg 1)
						 nil line-beginning line-end))
		    ))
		 ))
	     )
	    (t
	     (message (concat "Unkown sense-region-status: "
			      (symbol-name sense-region-status)))
	     ad-do-it)
	    )
    ad-do-it)
  (mell-transient-region-deactivate)
  (sense-region-to-region)
;  (setq sense-region-status 'region)
  )

(defadvice query-replace (around sense-region-query-replace disable)
  (if (mell-transient-region-active-p)
      (cond ((eq sense-region-status 'region)
	     ad-do-it)
	    ((eq sense-region-status 'rectangle)
	     (mell-region-rectangle-while
		 (list (region-beginning) (region-end))
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace around sense-region-query-replace)
		    (eval '(query-replace (ad-get-arg 0) (ad-get-arg 1) nil
					  line-beginning line-end))
		    ))
		 ))
	     )
	    ((eq sense-region-status 'offset)
	     (mell-region-selection-while
		 (sense-region-get-offset-list)
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace around sense-region-query-replace)
		    (eval '(query-replace (ad-get-arg 0) (ad-get-arg 1) nil
					  line-beginning line-end))
		    ))
		 ))
	     )
	    ((eq sense-region-status 'selection)
	     (mell-region-selection-while
		 sense-region-selection-list
	       (save-excursion
		 (set-mark line-beginning)
		 (goto-char line-end)
		 (and (fboundp 'zmacs-activate-region)
		      (zmacs-activate-region))
		 (if (or running-xemacs ; FIX-ME: ADHOC!!!
			 (< (string-to-number emacs-version) 21))
		     ad-do-it
		   (mell-advice-strip
		    '(query-replace around sense-region-query-replace)
		    (eval '(query-replace (ad-get-arg 0) (ad-get-arg 1)
					  nil line-beginning line-end))
		    ))
		 ))
	     )
	    (t
	     (message (concat "Unkown sense-region-status: "
			      (symbol-name sense-region-status)))
	     ad-do-it)
	    )
    ad-do-it)
  (mell-transient-region-deactivate)
  (sense-region-to-region)
  )

;;; ------------------------------------------------------------

(defun sense-region-replace ()
  (interactive)
  (if (mell-transient-region-active-p)
      (let ((string (sense-region-read-string "Replace To: ")))
	(cond
	 ;; region
	 ((eq sense-region-status 'region)
	  (sense-region-replace--selection
	   (mell-region-get-region-line-list (mark) (point)) string)
	  (setq sense-region-last-status 'region))

	 ;; rectangle
	 ((eq sense-region-status 'rectangle)
;	  (message "rectangle")
	  (sense-region-replace--selection
	   (mell-region-get-rectangle-list (mark) (point)) string)
	  (setq sense-region-last-status 'rectangle))

	 ;; offset
	 ((eq sense-region-status 'offset)
	  (sense-region-replace--selection
	   (sense-region-get-offset-list) string)
	  (setq sense-region-last-status 'offset))

	 ;; selection
	 ((eq sense-region-status 'selection)
	  (sense-region-replace--selection
	   sense-region-selection-list string)
	  (setq sense-region-last-status 'selection))

	 (t
	  (message (concat "Unkown sense-region-status: "
			   (symbol-name sense-region-status)))
	  (setq sense-region-last-status 'region))
	 ))
    (message "Region is not active.")
    (setq sense-region-last-status 'region))
  (mell-transient-region-deactivate)
  (sense-region-to-region)
  )

(defun sense-region-replace--selection (selection-list string)
  (let (inserted-selection)
    (save-excursion
      (mell-region-selection-while
	  selection-list
	(delete-region line-beginning line-end)
	(goto-char line-beginning)
	(insert string)
	(setq inserted-selection
	      (cons (cons (marker-position line-beginning) (point))
		    inserted-selection))
	))
    (mell-transient-region-deactivate)
    (mell-sign-selection-blink inserted-selection)
    ))

(defun sense-region-replace-regexp ()
  (interactive)
  (if (mell-transient-region-active-p)
      (let ((string 
	     (sense-region-read-string "Replace To: " '("\\0" . 0))))
	(message (symbol-name sense-region-status))
	(cond
	 ;; region
	 ((eq sense-region-status 'region)
	  (sense-region-replace-regexp--selection
	   (mell-region-get-region-line-list (mark) (point)) string)
	  (setq sense-region-last-status 'region))

	 ;; rectangle
	 ((eq sense-region-status 'rectangle)
	  (sense-region-replace-regexp--selection
	   (mell-region-get-rectangle-list (mark) (point)) string)
	  (setq sense-region-last-status 'rectangle))

	 ;; offset
	 ((eq sense-region-status 'offset)
	  (sense-region-replace-regexp--selection
	   (sense-region-get-offset-list) string)
	  (setq sense-region-last-status 'offset))

	 ;; selection
	 ((eq sense-region-status 'selection)
	  (sense-region-replace-regexp--selection
	   sense-region-selection-list string)
	  (setq sense-region-last-status 'selection))

	 (t
	  (message (concat "Unkown sense-region-status: "
			   (symbol-name sense-region-status)))
	  (setq sense-region-last-status 'region))
	 ))
    (message "Region is not active.")
    (setq sense-region-last-status 'region))
  (mell-transient-region-deactivate)
  (sense-region-to-region)
  )

(defun sense-region-replace-regexp--selection (selection-list regexp)
  (save-match-data
    (let (inserted-selection
	  insert-string orig-string
	  (count 1) (count-step 1))
      (if (string-match "\\\\#" regexp)
	  (setq count (string-to-number
		       (sense-region-read-string "Start number: "
						 '("1" . 0) nil "1"))
		count-step (string-to-number
			    (sense-region-read-string "Steps: "
						      '("1" . 0) nil "1"))))
      (save-excursion
	(mell-region-selection-while
	    selection-list
	  (setq orig-string (buffer-substring line-beginning line-end))
	  (goto-char line-beginning)
	  (delete-region line-beginning line-end)
;	  (setq insert-string regexp)
	  (setq insert-string
		(mell-string-replace regexp "\\\\#" (int-to-string count)))
	  (setq insert-string
		(mell-string-replace insert-string "\\\\0" orig-string))
	  (insert insert-string)
	  (setq inserted-selection
		(cons (cons (marker-position line-beginning) (point))
		      inserted-selection))
	  (setq count (+ count count-step))
	  ))
      (mell-transient-region-deactivate)
      (mell-sign-selection-blink inserted-selection)
      )))

(provide 'sense-region)
