;; ttp.el - tools for editing text with arbitrary text properties
;; Author and version: Edrx, 2005feb02
;; (find-anggfile "TCL/tcl.ttp")

;;;;
;;;; ttp-pack and ttp-unpack: use the format ^Atext^Bprops^C
;;;; (or just the text when there are no properties)
;;;;

(defvar ttp-regexp
  "\\(\^A\\)\\([^\^A\^B\^C]+\\)\\(\^B\\([^\^A\^B\^C]+\\)\^C\\)")

(defun ttp-pack (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward-regexp ttp-regexp nil t)
	(let ((props (read (match-string 4))))
	  (set-text-properties (match-beginning 2) (match-end 2) props)
	  (delete-region (match-beginning 3) (match-end 3))
	  (goto-char (match-end 2))
	  (delete-region (match-beginning 1) (match-end 1))))
      (point-max))))
  
;; Experimental: pack by the ^A and ^B...^C parts invisible.
;; Invisibily of the ^A and of the ^B...^C must be done by text
;; properties, not overlays, because we need to be able to move links
;; around preserving the invisible prefixes and suffixes.
;; (find-efile "outline.el" "defun outline-flag-region")
;;
(setq ttp-left  '(invisible t))
(setq ttp-right '(invisible t))

(defun ttp-ipack (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward-regexp ttp-regexp nil t)
	(let ((props (read (match-string 4)))) ; change to support L: and B:
	  (set-text-properties (match-beginning 1) (match-end 1) ttp-left)
	  (set-text-properties (match-beginning 2) (match-end 2) props)
	  (set-text-properties (match-beginning 3) (match-end 3) ttp-right)
	  (goto-char (match-end 0)))))))
  
(defun ttp-unpack (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((plist (text-properties-at (point)))
	      (next-change (copy-marker
			    (or (next-property-change (point))
				(point-max)))))
	  (when plist
	    (set-text-properties (point) next-change nil)
	    (insert "\^A")
	    (goto-char next-change)
	    (insert-before-markers "\^B\n" (prin1-to-string plist) "\n\^C"))
	  (goto-char next-change))))))


;;;;
;;;; region-as-insert: use the formats "text" and (p "text" p1 v2 p2 v2 ...)
;;;; 2005jan27
;;;;

;; (find-elnode "Property Search" "while (not (eobp))")
;; (find-elnode "Expansion")

(defmacro p (&rest args) `(propertize ',args))

(defun region-as-objects-to-insert (start end &optional fun)
  (or fun (setq fun (lambda (&rest args) (cons 'p args)))) 
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let (inserts-reversed)
	(while (not (eobp))
	  (let* ((nextpos (or (next-property-change (point)) (point-max)))
		 (plist (text-properties-at (point)))
		 (string (buffer-substring-no-properties (point) nextpos))
		 (code (if plist
			   (apply fun string plist)
			 string)))
	    (setq inserts-reversed (cons code inserts-reversed))
	    (goto-char nextpos)))
	(nreverse inserts-reversed)))))

(defun region-as-insert (start end)
  (let ((objects-to-insert (region-as-objects-to-insert start end))
	(print-escape-newlines t))
    (with-temp-buffer
      (insert "(insert\n")
      (mapc (lambda (obj) (prin1 obj (current-buffer)) (insert "\n"))
	    objects-to-insert)
      (insert ")\n")
      (buffer-substring (point-min) (point-max)))))

;; For tests:
;; (foo (point) (mark))
'
(defun foo (start end)
  (interactive "r")
  (find-estring (region-as-insert start end)))





;;;;
;;;; ttp-all-properties and ttp-filter-keep
;;;; These are used by `tunpack' to filter out irrelevant properties.
;;;;

;; (find-elnode "Creating Markers")
;; (find-elnode "Marker Insertion Types")
;; (find-elnode "Index" "* while:")
;; (find-elnode "Iteration")
;; (find-elnode "Property Search")
;; (find-elnode "Changing Properties")
;; (find-elnode "Property Search" "(while (not (eobp))")

(defun ttp-plist-to-keys (pvlist)
"Take a list of the form (p1 v1 p2 v2 ...) and return a list like (p1 p2 ...)."
  (if pvlist (cons (car pvlist) (ttp-plist-to-keys (cddr pvlist)))))

(defun ttp-keys-to-plist (keys)
"Take a list of the form (p1 p2 ...) and return a list like (p1 v1 p2 v2 ...)."
  (if keys (cons (car keys) (cons nil (ttp-keys-to-plist (cdr keys))))))

(if (not (fboundp 'remove-list-of-text-properties))
    (defun remove-list-of-text-properties (start end keys &optional object)
      (remove-text-properties start end (ttp-keys-to-plist keys) object))
  )

(defun ttp-get-text-properties-and-erase-them ()
  (let ((proplist (ttp-plist-to-keys (text-properties-at (point)))))
    (remove-list-of-text-properties (point) (point-max) proplist)
    proplist))

(defun ttp-next-property-change ()
  (goto-char (or (next-property-change (point)) (point-max))))

(defun ttp-all-properties (beg end)
  "Determine the \"names\" of all text properties that appear from BEG to END.
The answer is a list of symbols - for example, `(face fontified category)'."
  (let ((contents (buffer-substring beg end)))
    (with-temp-buffer
      (insert contents)
      (goto-char 0)
      (let ((proplist (ttp-get-text-properties-and-erase-them)))
	(while (progn (ttp-next-property-change)
		      (not (eobp)))
	  (setq proplist (append (ttp-get-text-properties-and-erase-them)
				 proplist)))
	proplist))))

;; (ttp-all-properties (point) (mark))

(defun ttp-set-difference (list1 list2)
  "Example: (ttp-set-difference '(a b c d) '(c b f))  |-->  (a c)."
  (if list1 (let ((diff (ttp-set-difference (cdr list1) list2)))
	      (if (member (car list1) list2)
		  diff
		(cons (car list1) diff)))))

(defun ttp-filter-keep (beg end ttp-good-properties)
  (let* ((ttp-all-properties (ttp-all-properties beg end))
	 (ttp-bad-properties (ttp-set-difference ttp-all-properties
						 ttp-good-properties)))
    (remove-list-of-text-properties beg end ttp-bad-properties)))



;;;;
;;;; qref - buttons for "quick reference" pages
;;;;

(defun eeflash-prop-region ()
  (interactive)
  (eeflash (previous-char-property-change (1+ (point)))
	   (next-char-property-change (point))
	   '(:background "Orange")))

(defun qref-do-help ()
  (interactive)
  (message "%S" (get-text-property (point) 'action))
  (eeflash-prop-region))

(defun qref-do-action ()
  (interactive)
  (eval (get-text-property (point) 'action)))

(defvar qref-keymap
  '(keymap (13 . qref-do-action)
	   (?? . qref-do-help)))

(defvar qref-link nil
  "Text properties for qref's \"underlined\" links.
Only the proplist of this variable matters.
See: (find-epp (symbol-plist 'qref-link))")

(defvar qref-button nil
  "Text properties for qref's \"button\" links.
Only the proplist of this variable matters.
See: (find-epp (symbol-plist 'qref-link))")

(setplist 'qref-link
	  `(face (:underline t)
	    active-face (:background "Orange")
	    mouse-face (:foreground "green")
	    keymap ,qref-keymap))

(setplist 'qref-button
	  `(face (:foreground "LightGray"
		  :background "DarkOliveGreen4")
	    active-face (:background "Orange")
	    mouse-face (:foreground "green")
	    keymap ,qref-keymap))



;;;;
;;;; highlighting the link and showing its action
;;;;

(setq qref-overlay nil)

(defun qref-point-at-link-p ()
  (get-text-property (point) 'action))

(defun qref-point-at-overlay-p ()
  (if qref-overlay
      (memq qref-overlay (overlays-at (point)))))

(defun qref-destroy-overlay ()
  (delete-overlay qref-overlay)
  (setq qref-overlay nil))

(defun qref-create-overlay ()
  (setq qref-overlay
	(make-overlay (previous-char-property-change (1+ (point)))
		      (next-char-property-change (point))))
  (overlay-put qref-overlay 'face
	       (get-text-property (point) 'active-face)))

(defun qref-show-target ()
  (message (ee-pp1 (get-text-property (point) 'action))))

(defun qref-highlight-current-link ()
  (if qref-overlay
      (qref-destroy-overlay))
  (when (qref-point-at-link-p)
    (qref-create-overlay)
    (qref-show-target)))




;;;;
;;;; <f3>
;;;;

(setq ttp-good-properties '(category action face))

(defun tpack (start end)
  (interactive "r")
  (ttp-pack start end))
  
(defun tunpack (start end)
  (interactive "r")
  (ttp-filter-keep start end ttp-good-properties)
  (ttp-unpack start end))

(defun tflip (start end)
  (interactive "r")
  (cond ((eq current-prefix-arg 1) (tpack start end))
	((eq current-prefix-arg 2) (tunpack start end))
	(t (error "Bad prefix arg; must be 1 to pack or 2 to unpack"))))

(eeb-define 'eeb-tpack   'tpack    'ee-delimiter-semicolon   nil t t)
(eeb-define 'eeb-tunpack 'tunpack  'ee-delimiter-semicolon   nil t t)
(eeb-define 'eeb-tflip   'tflip    'ee-delimiter-semicolon   nil t t)



;;;;
;;;; Tests / demos
;;;;

;; (find-efunction 'yank)
;; (find-efunction 'insert-for-yank-1)
;; (find-efunction 'remove-yank-excluded-properties)
;; (find-es "emacs" "flet")
;;
(defun ttp-yank (&optional arg)
  (interactive "*P")
  (flet ((remove-yank-excluded-properties (start end)))
    (yank arg)))

(defun ttp-mode (arg)
  "Determine if links are highlighted and targets shown or not."
  (interactive "p")
  (if (> arg 0)
      (add-hook  'post-command-hook 'qref-highlight-current-link nil 'local)
    (remove-hook 'post-command-hook 'qref-highlight-current-link nil 'local)))

;; (add-hook    'post-command-hook 'qref-highlight-current-link nil 'local)
;; (remove-hook 'post-command-hook 'qref-highlight-current-link nil 'local)

(defun pq (text category action)
  (propertize text 'category category 'action action))

'
(insert "\n;;\n"
	";; (ttp-mode 1)\n"
        ";; (setq eeb-defaults eeb-tflip)\n"
        (pq "'foo" 'qref-button '(next-line 1))     "\n"
	(pq "'foo" 'qref-button '(previous-line 1)) "\n"
        ";;\n"
	)

;; (find-elnode "Special Properties" "`invisible'")
;; (find-elnode "Sticky Properties")
;; (find-elnode "Invisible Text" "main editing loop moves point")


'
(insert "\n;;\n"
	";; (ttp-mode 1)\n"
        ";; (setq eeb-defaults eeb-tflip)\n"
        (pq "'foo" 'qref-button '(next-line 1))     "\n"
	(pq "'foo" 'qref-button '(previous-line 1)) "\n"
        ";;\n"
	)


;; (ttp-ipack (point) (ee-search-forward ";;\^O"))
;; (set-text-properties (point) (ee-search-forward ";;\^O") nil)

;; foogreen(face (:foreground "green"))bar

;;


foo<green>green</green>bar


;;
;; (ttp-mode 1)
;; (setq eeb-defaults eeb-tflip)
'foo
'foo
;;


;; (load "~/elisp/ttp.el")

(provide 'ttp)


;; Local Variables:
;; coding:  raw-text-unix
;; modes:   (fundamental-mode emacs-lisp-mode)
;; End:
