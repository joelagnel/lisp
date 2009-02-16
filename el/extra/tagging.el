;;; file tagging.el
;;  By Conrad Barski, M.D.
;;  This program is in the public domain
;;  Some logic for this program was taken from hide-lines.el from Mark Hulme-Jones.

(require 'cl)

(defvar tagging-tagline-indicator "^\\*")
(defvar tagging-tag-definition "-?[0-9,a-z,A-Z,_]+")
(defvar tagging-filter-cur nil)
(defvar tagging-invisible-areas ())

(defun tagging-parse-tags (str) ;(tagging-parse-tags "foo -bar 340fdfv sd9fwe8dcmm _--3")
  (labels ((F (pos)
	     (let ((x (string-match tagging-tag-definition str pos)))
	       (if x
		   (cons (if (string= (substring str x (+ x 1)) "-")
			     (cons nil (substring str (+ x 1) (match-end 0)))
			     (cons t (substring str x (match-end 0))))
			 (F (match-end 0)))))))
    (F 0)))

(defun tagging-tags-to-string (tags) ;(tagging-tags-to-string '((t . "foo") (nil . "bar")))
  (apply #'concat (mapcar (lambda (tag)
			    (concat (if (car tag)
					""
					"-")
				    (cdr tag)
				    " "))
			  tags)))

(defun tagging-search (tags)
  (let ((fails t))
    (while fails
      (setq fails nil)
      (if (re-search-forward tagging-tagline-indicator nil t)
	  (progn
	    (beginning-of-line)
	    (mapc (lambda (tag)
		    (when (let ((x (re-search-forward (concat tagging-tagline-indicator "\\(.* \\)?" (cdr tag) "\\( .*\\)?$") (point-at-eol) t)))
			    (or (and (car tag) (not x)) (and (not (car tag)) x)))
		      (setq fails t))
		    (beginning-of-line))
		  tags)
	    (if fails
		(forward-line 1)))
	  (goto-char (point-max))))))

(defun tagging-add-invisible-overlay (start end) 
  (let ((overlay (make-overlay start end))) 
    (setq tagging-invisible-areas (cons overlay tagging-invisible-areas)) 
    (overlay-put overlay 'invisible 'hl)))

(defun tagging-make-visible () 
  (mapcar (lambda (overlay) (delete-overlay overlay))  
          tagging-invisible-areas) 
  (setq tagging-invisible-areas ()))

(defun tagging-perform-filter (tags)
  (save-excursion
    (tagging-make-visible)
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (let ((x (point)))
	(tagging-search tags)
	(tagging-add-invisible-overlay x (point))
	(forward-line 1)
	(if (re-search-forward tagging-tagline-indicator nil t)
	    (beginning-of-line)
	    (goto-char (point-max)))))))
	
(defun tagging-show-all () 
  (interactive)
  (tagging-make-visible)
  (setq tagging-filter-cur nil))

(defun tagging-filter-set (tags)
  (interactive (list (read-from-minibuffer "Tags:" (tagging-tags-to-string tagging-filter-cur))))
  (setq tagging-filter-cur (tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur))

(defun tagging-filter-add (tags)
  (interactive (list (read-from-minibuffer "Tags:" (thing-at-point 'word))))
  (setq tagging-filter-cur (append tagging-filter-cur (tagging-parse-tags tags)))
  (tagging-perform-filter tagging-filter-cur))

(defun tagging-filter-subtract (tags)
  (interactive (list (read-from-minibuffer "Tags:" (thing-at-point 'word))))
  (mapc (lambda (x)
	  (setq tagging-filter-cur (delete x tagging-filter-cur)))
	(tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur))

(define-derived-mode tagging-mode fundamental-mode "Tagging"
    "Major mode for using del.icio.us- like tagging."
    (setq comment-start tagging-tagline-indicator)
    (local-set-key "\C-c\C-a" 'tagging-show-all)
    (local-set-key "\C-c\C-t" 'tagging-filter-set)
    (local-set-key "\C-c\C-s" 'tagging-filter-add)
    (local-set-key "\C-c\C-d" 'tagging-filter-subtract)
    (setq tagging-font-lock-keywords
          (list `(,(concat tagging-tagline-indicator ".*$")
                  . font-lock-keyword-face)))
    (font-lock-mode)
     (setq font-lock-keywords tagging-font-lock-keywords))

(add-to-list 'auto-mode-alist '(".tagged\\'" . tagging-mode))

(define-minor-mode tagging-minor-mode
  "Toggle Tagging Minor mode."
  nil
  " Tagging"
  ;; The minor mode bindings.
  `(("\C-c\C-a" . tagging-show-all)
    ("\C-c\C-t" . tagging-filter-set)
    ("\C-c\C-s" . tagging-filter-add)
    ("\C-c\C-d" . tagging-filter-subtract)))

;(setq tagging-tagline-indicator "^//\\*") ;tagging for java, C, C#, etc. 
;(setq tagging-tagline-indicator "^;;\\*") ;taging for Lisps & Schemes
