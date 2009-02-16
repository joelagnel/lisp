;; Goby: goby.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Aug  9, 2003
;; Revised: Aug 26, 2005

;;; Commentary:

;; Home page: http://www.mew.org/~kazu/proj/goby/

;;; Code:

(defconst goby-version "0.91")

(require 'goby-vars)
(require 'goby-emacs)

(autoload 'goby-view-mode   "goby-view" nil t)
(autoload 'goby-dump-screen "goby-view" nil t)
(autoload 'goby-make-ps "goby-ps" nil t)
(add-to-list 'auto-mode-alist '("\\.gby\\'" . goby-edit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal
;;;

(defvar goby-initialized nil)

(defvar goby-bar-image nil)
(defvar goby-pause-image nil)
(defvar goby-item-images nil)

(defvar goby-default-face-pixel-width  nil)
(defvar goby-default-face-pixel-height nil)

(defvar goby-left-pixel-fringe nil)
(defvar goby-right-pixel-fringe nil)

(defvar goby-buffer-keymap nil)

(mapcar 'make-variable-buffer-local
	(list 'goby-buffer-keymap))

(defvar goby-a-fonts (list goby-helvetica goby-courier goby-times))
(defvar goby-j-fonts (list goby-gothic goby-mincho))

(defvar goby-a-fonts-base 0)
(defvar goby-j-fonts-base 10)
(defvar goby-m-fonts-base 100)

(defvar goby-a-fonts-lim (+ goby-a-fonts-base (length goby-a-fonts)))
(defvar goby-j-fonts-lim (+ goby-j-fonts-base (length goby-j-fonts)))

(defun goby-get-family (num)
  (cond
   ((= goby-m-fonts-base num) goby-times)
   ((and (<= goby-j-fonts-base num) (< num goby-j-fonts-lim))
    (nth (- num goby-j-fonts-base) goby-j-fonts))
   ((and (<= goby-a-fonts-base num) (< num goby-a-fonts-lim))
    (nth (- num goby-a-fonts-base) goby-a-fonts))
   (t
    (car goby-a-fonts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constants
;;;

(defconst goby-ascii-regex   "[ -~]")
(defconst goby-control-regex "[\000-\037]")
(defconst goby-emacs-pixel-base 10) ;; see info

(defconst goby-face-prefix "goby-face")
(defconst goby-face-regex "^goby-face-\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\(-i\\)?")

(defun goby-get-face-family (name)
  (string-to-number (goby-match-string 1 name)))
(defun goby-get-face-ratio (name)
  (string-to-number (goby-match-string 2 name)))
(defun goby-get-face-color (name)
  (string-to-number (goby-match-string 3 name)))
(defun goby-get-face-italic (name)
  (string= (goby-match-string 4 name) "-i"))

(defconst goby-math-regex "[^- \n(){}]+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher for Edit mode
;;;

(defun goby-width-height ()
  (cond
   ((eq window-system 'mac)
    (let* ((fontset (or (cdr (assoc 'font default-frame-alist))
			"fontset-default"))
	   (finfo (font-info (fontset-font fontset ?a))))
      (list (aref finfo 2) (aref finfo 3))))
   (t
    (list (frame-char-width) (frame-char-height)))))

(defun goby-init ()
  (unless goby-initialized
    (let* ((width-height (goby-width-height))
	   (width (nth 0 width-height))
	   (height (nth 1 width-height)))
      (or goby-default-face-pixel-width
	  (setq goby-default-face-pixel-width width))
      (or goby-default-face-pixel-height
	  (setq goby-default-face-pixel-height height)))
    (setq goby-left-pixel-fringe
	  (or (frame-parameter (selected-frame) 'left-fringe)
	      goby-default-face-pixel-width))
    (setq goby-right-pixel-fringe
	  (or (frame-parameter (selected-frame) 'right-fringe)
	      goby-default-face-pixel-width))
    (setq goby-bar-image (goby-create-bar))
    (setq goby-pause-image (goby-create-pause))
    (setq goby-item-images (goby-create-item))
    (setq goby-initialized t)))

;;;###autoload
(defun goby ()
  "Make a Goby frame."
  (interactive)
  (goby-init)
  (let ((frames (frame-list)) frame width height)
    (catch 'loop
      (while frames
	(if (string= (frame-parameter (car frames) 'name) goby-frame)
	    (throw 'loop (setq frame (car frames))))
	(setq frames (cdr frames))))
    (if frame
	(progn
	  (select-frame frame)
	  (modify-frame-parameters (selected-frame) '((visibility . t))))
      (switch-to-buffer-other-frame goby-buffer)
      (setq width  (- (ceiling (/ (float (display-pixel-width)) goby-default-face-pixel-width)) 2))
      (setq height (- (/ (display-pixel-height) goby-default-face-pixel-height) 2))
      (if (/= (% (display-pixel-height) goby-default-face-pixel-height) 0)
	  (setq height (1- height)))
      (goby-decorate-initial-frame
       width height
       goby-default-face-pixel-width)
      (goby-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit mode
;;;

(defvar goby-mode nil)
(make-variable-buffer-local 'goby-mode)
(add-minor-mode 'goby-mode goby-mode-lighter goby-mode-map)

(defun goby-local-unset-map ()
  (use-local-map goby-buffer-keymap)
  (setq goby-buffer-keymap nil))

(defun goby-local-set-map ()
  (setq goby-buffer-keymap (current-local-map))
  (use-local-map (if (current-local-map)
		     (copy-keymap (current-local-map))
		   (make-sparse-keymap)))
  (local-set-key [?\t]      'goby-insert-tab)
  (local-set-key [?\C-x ?u] 'goby-undo)
  (local-set-key [?\C-_]    'goby-undo)
  (local-set-key [?\C-/]    'goby-undo))

(defun goby-insert-tab (&optional arg)
  "Insert a TAB character."
  (interactive "P")
  (insert-char ?\t (if (numberp arg) arg 1)))

(defun goby-undo ()
  "Get a thing undone."
  (interactive)
  (let ((after-change-functions nil))
    (undo)))

;;;###autoload
(defun goby-mode (&optional arg)
  "Goby Edit mode.
Minor mode for editing large TrueType fonts and images.
\\{goby-mode-map}"
  (interactive "P")
  ;; All buffer local variables are killed after a major mode
  ;; is called. 
  (unless (eq major-mode goby-major-mode)
    (funcall goby-major-mode))
  (setq goby-mode (if (null arg)
		      (not goby-mode)
		    (> (prefix-numeric-value arg) 0)))
  (cond
   (goby-mode
    (make-local-variable 'vertical-centering-font-regexp)
    (setq vertical-centering-font-regexp nil)
    (unless (eq arg 'noimage)
      (goby-highlight-buffer 'image))
    (add-hook 'after-change-functions 'goby-highlight nil 'local)
    (add-hook 'after-save-hook 'goby-save-property nil 'local)
    (setq tab-width goby-tab-width)
    (setq indent-tabs-mode t) ;; buffer local
    (goby-local-set-map))
   (t
    (kill-local-variable 'vertical-centering-font-regexp)
    (kill-local-variable 'after-change-functions)
    (kill-local-variable 'after-save-hook)
    (kill-local-variable 'tab-width)
    (widen)
    (let ((modifiedp (buffer-modified-p)))
      (put-text-property (point-min) (point-max) 'face 'default)
      (remove-text-properties (point-min) (point-max) '(goby-face nil))
      (remove-text-properties (point-min) (point-max) '(display nil))
      (goby-save-property 'to-buffer)
      (unless modifiedp (set-buffer-modified-p nil)))
    (goby-local-unset-map)))
  (setq buffer-undo-list nil))

(defalias 'goby-edit-mode 'goby-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions
;;;

(defun goby-get-pixel-height (ratio)
  (/ (* (display-pixel-height) ratio) 100))

(defun goby-get-height (ratio)
  (let ((ret (* (goby-get-pixel-height ratio) goby-emacs-pixel-base)))
    ;; Mac is 72dpi, Windows is 96dpi
    (if (eq window-system 'w32) (setq ret (* (/ ret 4) 3)))
    ret))

(defun goby-make-face (family-num ratio color-num)
  (let* ((spec (goby-get-tab-spec color-num))
	 (color (goby-get-tab-color spec))
	 (family (goby-get-family family-num))
	 (italicp (= family-num goby-m-fonts-base))
	 (face-name (format "%s-%d-%d-%d"
			    goby-face-prefix family-num ratio color-num))

	 face height)
    (if italicp (setq face-name (concat face-name "-i")))
    (setq face (intern face-name))
    (unless (facep face)
      (setq face (intern face-name))
      (setq height (goby-get-height ratio))
      (make-empty-face face)
      (goby-set-face-attribute face family height color italicp))
    face))

(defun goby-count-tabs ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\(\t*\\)")
    (list (- (match-end 1) (match-beginning 1)) (match-end 0))))

(defun goby-get-tabnum-num (ent) (nth 0 ent))
(defun goby-get-tabnum-end (ent) (nth 1 ent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Highlight
;;;

(defun goby-face-get ()
  (let* ((tabnum (goby-count-tabs))
	 (num (goby-get-tabnum-num tabnum))
	 (end (goby-get-tabnum-end tabnum))
	 (spec (goby-get-tab-spec num))
	 (ratio (goby-get-tab-ratio spec))
	 (color num)
	 (a-face (goby-make-face goby-a-fonts-base ratio color))
	 (j-face (goby-make-face goby-j-fonts-base ratio color)))
    (list a-face j-face end)))

(defun goby-get-aje-a (ent) (nth 0 ent))
(defun goby-get-aje-j (ent) (nth 1 ent))
(defun goby-get-aje-e (ent) (nth 2 ent))

(defun goby-highlight-fence (beg end)
  (let* ((aje (goby-face-get))
	 (j-face (goby-get-aje-j aje)))
    (put-text-property beg end 'face j-face)))

(defun goby-highlight-char (aje)
  (let ((pos (point))
	(a-face (goby-get-aje-a aje))
	(j-face (goby-get-aje-j aje))
	keep)
    (cond
     ((goby-extent-image-p pos)
      ())
     ((get-text-property pos 'goby-math)
      (setq keep t))
     ((looking-at goby-control-regex)
      (put-text-property pos (1+ pos) 'face 'default))
     ((or (get-text-property (point) 'egg-start)
	  (get-text-property (point) 'egg-end))
      ;; fences should be j-face
      (put-text-property pos (1+ pos) 'face j-face))
     ((looking-at goby-ascii-regex)
      (put-text-property pos (1+ pos) 'face a-face))
     (t
      (put-text-property pos (1+ pos) 'face j-face)))
    (unless keep
      (remove-text-properties pos (1+ pos) '(goby-face nil)))))

(defun goby-highlight-word (beg end)
  (let ((aje (goby-face-get))
	(char (buffer-substring-no-properties beg end)))
    (cond
     ((string= char "\n")  ;; folding
      (put-text-property beg end 'face 'default)
      (remove-text-properties beg end '(goby-face nil))
      (save-excursion
	(goby-highlight-line aje)))
     ((string= char "\t") ;; indentation
      (save-excursion
	(beginning-of-line)
	(goby-highlight-line aje)))
     (t
      (save-excursion
	(goto-char beg)
	(goby-highlight-char aje))))))

(defun goby-highlight-line (aje)
  (while (not (eolp))
    (goby-highlight-char aje)
    (forward-char))
  (when (looking-at "\n")
    (put-text-property (point) (1+ (point)) 'face 'default)
    (remove-text-properties (point) (1+ (point)) '(goby-face nil))))

(defun goby-rehighlight-line (beg)
  (let* ((aje (goby-face-get))
	 (tab-end (goby-get-aje-e aje))
	 (undo buffer-undo-list)
	 ent found)
    (when (listp undo)
      (setq ent (car undo))
      (setq undo (cdr undo))
      (unless ent
	(setq ent (car undo))
	(setq undo (cdr undo)))
      (catch 'loop 
	(while ent
	  (if (stringp (car-safe ent)) (throw 'loop (setq found (car ent))))
	  (setq ent (car undo))
	  (setq undo (cdr undo))))
      (when found
	(cond
	 ((and (<= (point) tab-end) (string= found "\t"))
	  (save-excursion
	    (goto-char beg)
	    (goby-highlight-line aje)))
	 ((and (not (eolp)) (string= found "\n"))
	  (save-excursion
	    (goto-char beg)
	    (goby-highlight-line aje))))))))

(defun goby-highlight-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (goby-highlight-line (goby-face-get))
      (forward-line))))

;; 
;; 

(defvar goby-highlight-ignore-command-list '(yank))

(defun goby-highlight (beg end len)
  (let ((after-change-functions nil)
	(buffer-read-only nil))
    (unless (memq this-original-command goby-highlight-ignore-command-list)
      (cond
       ((goby-extent-image-p beg) ())
       ((get-text-property beg 'intangible) ;; input method
	(goby-highlight-fence beg end))
       ((= (- end beg) 1) ;; inserting a char
	(goby-highlight-word beg end))
       ((= beg end) ;; deletring a char
	(goby-rehighlight-line beg))
       (t
	(goby-highlight-region beg end))))))
;;

(defun goby-highlight-image-buffer ()
  (let ((center-regex (concat "^" (regexp-quote goby-centering-string))))
    (save-excursion
      (goby-visualize-image)
      (goby-visualize-bar)
      (goby-visualize-item)
      (goby-visualize-pause)
      (goto-char (point-min))
      (while (re-search-forward center-regex nil t)
	(goby-center-line 'no-insert)
	(forward-line)))))

(defun goby-highlight-buffer (&optional image)
  "Highlight the entire buffer."
  (interactive "P")
  (let ((after-change-functions nil)
	(props (goby-load-property)))
    (goby-highlight-region (point-min) (point-max))
    (when props
      (goby-put-property props))
    (when image
      (goby-highlight-image-buffer)))
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Face region
;;;

(defun goby-face-get-frc (name)
  (list (goby-get-face-family name)
	(goby-get-face-ratio name)
	(goby-get-face-color name)))

(defun goby-face-get-family (frc) (nth 0 frc))
(defun goby-face-get-ratio (frc)  (nth 1 frc))
(defun goby-face-get-color (frc)  (nth 2 frc))

(defun goby-face-set-family (frc family)
  (setcar frc family))

(defun goby-face-set-ratio (frc ratio)
  (setcar (nthcdr 1 frc) ratio))

(defun goby-face-set-color (frc color)
  (setcar (nthcdr 2 frc) color))

(defun goby-face-region (beg end func &optional func2)
  (let* ((after-change-functions nil)
	 (aje (goby-face-get))
	 (a-face (goby-get-aje-a aje))
	 (j-face (goby-get-aje-j aje))
	 face name new-face next frc)
    (save-excursion
      (goto-char beg)
      (while (and (setq next (next-property-change (point) nil end))
		  (< (point) end))
	(setq face (get-text-property (point) 'face))
	(setq name (symbol-name face))
	(if (not (string-match goby-face-regex name))
	    ;; for "\n", etc. just in case
	    (remove-text-properties (point) next '(goby-face nil))
	  (setq frc (goby-face-get-frc name))
	  (funcall func frc)
	  (setq new-face (apply 'goby-make-face frc))
	  (if (functionp func2) (funcall func2 new-face))
	  (put-text-property (point) next 'face new-face)
	  (if (and (not (face-italic-p new-face))
		   (or (face-equal new-face a-face)
		       (face-equal new-face j-face)))
	      ;; do not save the default property
	      (remove-text-properties (point) next '(goby-face nil))
	    ;; save the non-default property
	    (put-text-property (point) next 'goby-face frc)))
	(goto-char next)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ratio
;;;

(defun goby-face-increase-ratio-region (beg end)
  "Enlarge the characters in the region."
  (interactive "r")
  (goby-face-region
   beg end 
   (lambda (frc) (goby-face-set-ratio frc (1+ (goby-face-get-ratio frc))))))

(defun goby-face-decrease-ratio-region (beg end)
  "Shrink the characters in the region."
  (interactive "r")
  (goby-face-region
   beg end 
   (lambda (frc) (goby-face-set-ratio frc (1- (goby-face-get-ratio frc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Font
;;;

(defun goby-face-next-font-region (beg end)
  "Convert the characters in the region to the next font."
  (interactive "r")
  (goby-face-region
   beg end
   (lambda (frc)
     (let* ((a-fontp (looking-at goby-ascii-regex))
	    (base (if a-fontp goby-a-fonts-base goby-j-fonts-base))
	    (lim (if a-fontp goby-a-fonts-lim goby-j-fonts-lim))
	    (num (goby-face-get-family frc)))
       (setq num (1+ num))
       (if (= num lim) (setq num base))
       (goby-face-set-family frc num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color
;;;

(defun goby-face-next-color-region (beg end)
  "Convert the characters in the region to the next color."
  (interactive "r")
  (goby-face-region
   beg end 
   (lambda (frc)
     (let* ((lim (length goby-tab-spec))
	    (num (goby-face-get-color frc)))
       (setq num (1+ num))
       (if (= num lim) (setq num 0))
       (goby-face-set-color frc num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Math
;;;

(defun goby-face-math-set-prop (beg end prop)
  (let ((after-change-functions nil))
    (if (eq prop t)
	(remove-text-properties beg end '(display nil))
      (put-text-property beg end 'display prop))
    (put-text-property beg end 'goby-math prop)
    (put-text-property beg end 'rear-nonsticky t)))

(defun goby-face-math-region (beg end)
  "Convert the characters in the region to italic times."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward goby-math-regex end t)
      (goby-face-region
       (match-beginning 0)
       (match-end 0)
       (lambda (frc)
	 (goby-face-set-family frc goby-m-fonts-base)))))
  (goby-face-math-set-prop beg end t))

(defun goby-face-math-set-raise (beg end factor)
  (let ((prop (list 'raise factor)))
    (goby-face-math-set-prop beg end prop)))

(defun goby-face-math-get-raise (pos)
  (let ((disp (get-text-property pos 'display)))
    (nth 1 (memq 'raise disp))))

(defun goby-face-math-power-region (beg end)
  "Shrink the characters in the region and raise them."
  (interactive "r")
  (goby-face-region
   beg end 
   (lambda (frc)
     (let* ((ratio (goby-face-get-ratio frc))
	    raise)
       (cond
	((= (% ratio 2) 0)
	 (setq ratio (/ ratio 2))
	 (setq raise 0.6))
	(t
	 (setq ratio (1+ (/ ratio 2)))
	 (setq raise 0.5)))
       (goby-face-set-ratio frc ratio)
       (goby-face-set-family frc goby-m-fonts-base)
       (goby-face-math-set-raise beg end raise))))) ;; tricky

(defun goby-face-math-aux-region (beg end)
  "Shrink the characters in the region."
  (interactive "r")
  (goby-face-region
   beg end 
   (lambda (frc)
     (goby-face-set-ratio frc (- (goby-face-get-ratio frc) 3))
     (goby-face-set-family frc goby-m-fonts-base))))

(defun goby-face-math-raise-region (beg end)
  "Raise the characters in the region."
  (interactive "r")
  (let ((raise (or (goby-face-math-get-raise beg) 0)))
    (if (eq raise t) (setq raise 0.0))
    (setq raise (+ raise 0.1))
    (if (zerop raise) (setq raise t))
    (goby-face-math-set-raise beg end raise)))

(defun goby-face-math-lower-region (beg end)
  "Lower the characters in the region."
  (interactive "r")
  (let ((raise (or (goby-face-math-get-raise beg) 0)))
    (if (eq raise t) (setq raise 0.0))
    (setq raise (- raise 0.1))
    (if (zerop raise) (setq raise t))
    (goby-face-math-set-raise beg end raise)))

(defun goby-insert-iso-8859-1 (char &optional lower)
  (let* ((str (decode-coding-string (format "%c" char) 'iso-8859-1))
	 (pos (point))
	 (len (length str)))
    (insert str)
    (goby-face-math-set-raise pos (+ pos len) -0.1)))

(defun goby-face-math-1/2 ()
  "Insert the 1/2 character."
  (interactive)
  (goby-insert-iso-8859-1 ?\275))

(defun goby-face-math-3/4 ()
  "Insert the 3/4 character."
  (interactive)
  (goby-insert-iso-8859-1 ?\276))

(defun goby-face-math-1/4 ()
  "Insert the 1/4 character."
  (interactive)
  (goby-insert-iso-8859-1 ?\274))

(defun goby-face-math-dot ()
  "Insert the dot character."
  (interactive)
  (goby-insert-iso-8859-1 ?\267 'lower))

(defun goby-face-math-times ()
  "Insert the multiply character."
  (interactive)
  (goby-insert-iso-8859-1 ?\327 'lower))

(defun goby-face-math-divide ()
  "Insert the division character."
  (interactive)
  (goby-insert-iso-8859-1 ?\367 'lower))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Images
;;;

;; see goby-image-regex
(defun goby-image-get-file ()  (goby-match-string 1))
(defun goby-image-get-scale () (goby-match-string 2))

(defun goby-image-type (file)
  (let (type)
    (cond
     ((string-match "\\.jpe?g$" file)
      (setq type 'jpeg))
     ((string-match "\\.tiff?$" file)
      (setq type 'tiff))
     ((string-match "\\.png$" file)
      (setq type 'png))
     ((string-match "\\.gif$" file)
      (setq type 'gif))
     ((string-match "\\.xpm$" file)
      (setq type 'xpm))
     ((string-match "\\.xbm$" file)
      (setq type 'xbm)))
    (if (and type (memq type image-types)) type)))

(defun goby-which (file path)
  (catch 'loop
    (while path
      (if (file-exists-p (expand-file-name file (car path)))
	  (throw 'loop (expand-file-name file (car path)))
	(setq path (cdr path))))))

(defun goby-which-exec (execfile)
  (or (goby-which execfile exec-path)
      (goby-which (concat execfile ".exe") exec-path)))

(defun goby-get-topnm (file)
  (let ((spec goby-image-spec)
	ent)
    (catch 'loop
      (while spec
	(setq ent (car spec))
	(setq spec (cdr spec))
	(if (string-match (goby-get-image-suffix ent) file)
	    (throw 'loop (goby-get-image-program ent)))))))

(defun goby-scale-pixel-width (scale)
  (let (numerator denominator pixel-width)
    (cond
     ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" scale)
      (setq numerator   (string-to-number (goby-match-string 1 scale)))
      (setq denominator (string-to-number (goby-match-string 2 scale))))
     ((string-match "^\\([0-9]+\\)$" scale)
      (setq numerator   (string-to-number (goby-match-string 1 scale)))
      (setq denominator 100)))
    (setq pixel-width (/ (* (display-pixel-width) numerator) denominator))
    (int-to-string pixel-width)))

(defun goby-create-image (file &optional scale)
  (let ((type (goby-image-type file))
	topnm pixel-width data)
    (when (and (file-readable-p file)
	       (or (null scale)
		   (setq topnm (goby-get-topnm file))))
      (when scale
	(setq pixel-width (goby-scale-pixel-width scale))
	(with-temp-buffer
	  (goby-image-safe
	   (set-buffer-multibyte nil)
	   (call-process topnm file '(t nil))
	   (call-process-region (point-min) (point-max) goby-prog-pnmscale
				t '(t nil) nil
				"-xsize" pixel-width)
	   ;; pnm and png freezes Emacs, sigh.
	   (call-process-region (point-min) (point-max) goby-prog-pnmtojpeg
				t '(t nil) nil)
	   (setq data (buffer-substring-no-properties (point-min) (point-max))))
	  (setq type 'jpeg)))
      (if data
	  (create-image data type 'data)
	(create-image file type)))))

(defun goby-visualize-image ()
  (goto-char (point-min))
  (let ((after-change-functions nil)
	beg end file image scale)
    (while (re-search-forward goby-image-regex nil t)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq file (goby-image-get-file))
      (setq scale (goby-image-get-scale))
      (if (string= scale "") (setq scale nil))
      ;; default-directory is not set when loading
      (unless (file-name-absolute-p file)
	(setq file (expand-file-name
		    file (file-name-directory (buffer-file-name)))))
      (setq image (goby-create-image file scale))
      (goby-put-image beg end image))))

(defun goby-insert-image (&optional scalep)
  "Insert an image file."
  (interactive "P")
  (let* ((after-change-functions nil)
	 (scale (if scalep (read-string "Scale [% or A/B]: ")))
	 (prompt (concat "[" (mapconcat 'symbol-name image-types "/") "] file: "))
	 (file (read-file-name prompt))
	 (efile (expand-file-name file))
	 (defdir (expand-file-name default-directory))
	 (regex (concat "^" (regexp-quote defdir)))
	 (beg (point))
	 image)
    (message "Inserting the image...")
    (if (string-match regex efile)
	(setq file (substring efile (length defdir))))
    (setq image (goby-create-image efile scale))
    (if scale
	(insert (format goby-image-format2 file scale))
      (insert (format goby-image-format file)))
    (goby-put-image beg (point) image)
    (if (/= (forward-line) 0) (insert "\n"))
    (goto-char beg)
    (message "Inserting the image...done")))

(defun goby-change-scale ()
  "Change the scale of the image."
  (interactive)
  (let (scale beg end file image)
    (if (not (looking-at goby-image-regex))
	(message "No image here")
      (setq file (goby-image-get-file))
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq scale (read-string "Scale [% or A/B]: "))
      (message "Scaling the image...")
      (delete-region beg end)
      (setq image (goby-create-image file scale))
      (setq beg (point))
      (insert (format goby-image-format2 file scale))
      (goby-put-image beg (point) image)
      (goto-char beg)
      (message "Scaling the image...done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bar
;;;

(defun goby-create-bar ()
  (let* ((width (- (display-pixel-width)
		   goby-left-pixel-fringe
		   goby-right-pixel-fringe
		   goby-default-face-pixel-width)) ;; prevent folding
	 (lside-len goby-bar-size-pixel-margin)
	 (lside (make-string lside-len ?-))
	 (rside-len (- goby-bar-size-pixel-margin goby-default-face-pixel-width))
	 (rside (make-string rside-len ?-))
	 (bar (make-string (- width (+ lside-len rside-len)) ?=))
	 (data (concat "/* XPM */\n"
		       "static char *a[] = {\n"
		       (format "\"%d 2 2 1\",\n" width)
		       (format "\"- c %s\",\n" goby-background-color)
		       (format "\"= c %s\",\n" goby-view-bar-color)
		       "\"" lside bar rside "\",\n"
		       "\"" lside bar rside "\",\n"
		       "};")))
    (create-image data 'xpm 'data :ascent 'center)))

(defun goby-draw-bar (&optional beg end)
  (let ((after-change-functions nil))
    (unless beg
      (setq beg (point))
      (insert goby-bar-string)
      (setq end (point)))
    (put-text-property beg end 'face 'default)
    (goby-put-image beg end goby-bar-image)
    (put-text-property beg end 'goby 'bar)
    (put-text-property beg end 'rear-nonsticky t)))

(defun goby-visualize-bar ()
  (goto-char (point-min))
  (let ((regex (concat "^" (regexp-quote goby-bar-string))))
    (while (re-search-forward regex nil t)
      (goby-draw-bar (match-beginning 0) (match-end 0))
      (forward-line))))

(defun goby-insert-bar ()
  "Insert a bar."
  (interactive)
  (if (not (and (bolp) (eolp)))
      (message "Use this command at a null line")
    (let ((after-change-functions nil))
      (goby-draw-bar)
      (if (/= (forward-line) 0) (insert "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Item
;;;

(defun goby-create-item ()
  (let* ((spec goby-tab-spec)
	 ent pixel-size color line square data image images)
    (while spec
      (setq ent (car spec))
      (setq spec (cdr spec))
      (setq pixel-size (goby-get-pixel-height (goby-get-tab-ratio ent)))
      (setq pixel-size (/ (* pixel-size goby-item-ratio) 100))
      (setq color (goby-get-tab-tbclr ent))
      (setq line (concat "\"" (make-string pixel-size ?-) "\",\n"))
      (setq square (make-vector pixel-size line))
      (setq data (concat "/* XPM */\n"
			 "static char *a[] = {\n"
			 (format "\"%d %d 1 1\",\n" pixel-size pixel-size)
			 (format "\"- c %s\",\n" color)
			 (mapconcat 'identity square "")
			 "};"))
      (setq image (create-image data 'xpm 'data :ascent 'center))
      (setq images (cons image images)))
    (nreverse images)))

(defun goby-draw-item (num &optional beg end)
  (let* ((after-change-functions nil)
	 (len (length goby-item-images))
	 (image (nth (if (< num len) num (1- len)) goby-item-images))
	 (aje (goby-face-get))
	 (j-face (goby-get-aje-j aje)))
    (unless beg
      (setq beg (point))
      (insert goby-item-string)
      (setq end (point)))
    (put-text-property beg end 'face j-face)
    (goby-put-image beg end image 'nodefault)
    (put-text-property beg end 'goby 'item)
    (put-text-property beg end 'goby-item num)
    (put-text-property beg end 'rear-nonsticky t)))

(defun goby-visualize-item ()
  (goto-char (point-min))
  (let ((regex (concat "^\\(\t*\\)\\(" (regexp-quote goby-item-string) "\\)"))
	num)
    (while (re-search-forward regex nil t)
      (setq num (- (match-end 1) (match-beginning 1)))
      (goby-draw-item num (match-beginning 2) (match-end 2))
      (forward-line))))

(defun goby-insert-item ()
  "Insert a itemize sign."
  (interactive)
  ;; after-change-functions raises the item mark to the middle...
  (let* ((tabnum (goby-count-tabs))
	 (num (goby-get-tabnum-num tabnum))
	 (end (goby-get-tabnum-end tabnum)))
    (if (/= (point) end)
	(message "Use this command after TABs at the beginning of a line.")
      (goby-draw-item num))))

(defun goby-insert-item-region (beg end)
  "Insert itemize signs in the region."
  (interactive "r")
  ;; after-change-functions raises the item mark to the middle...
  (let (tabnum num here)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (save-restriction
	(narrow-to-region (point) end)
	(while (< (point) (point-max))
	  (setq tabnum (goby-count-tabs))
	  (setq num (goby-get-tabnum-num tabnum))
	  (setq here (goby-get-tabnum-end tabnum))
	  (goto-char here)
	  (unless (or (eolp) (goby-extent-p (point)))
	    (goby-draw-item num)
	    (insert " "))
	  (forward-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pause
;;;

(defun goby-create-pause ()
  (let ((data
	 (concat
	  "/* XPM */\n"
	  "static char *a[] = {\n"
	  "\"16 16 2 1\",\n"
	  (format "\". c %s\",\n" goby-background-color)
	  (format "\"# c %s\",\n" goby-view-pause-color)
	  "\"................\",\n"
	  "\"................\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"..####....####..\",\n"
	  "\"................\",\n"
	  "\"................\"\n"
	  "};\n")))
    (create-image data 'xpm 'data :ascent 'center)))

(defun goby-draw-pause (&optional beg end)
  (let* ((after-change-functions nil)
	 (aje (goby-face-get))
	 (j-face (goby-get-aje-j aje)))
    (unless beg
      (setq beg (point))
      (insert goby-pause-string)
      (setq end (point)))
    (put-text-property beg end 'face j-face)
    (goby-put-image beg end goby-pause-image 'nodefault)
    (put-text-property beg end 'goby 'pause)
    (put-text-property beg end 'rear-nonsticky t)))

(defun goby-visualize-pause ()
  (goto-char (point-min))
  (let ((regex (regexp-quote goby-pause-string)))
    (while (re-search-forward regex nil t)
      (goby-draw-pause (match-beginning 0) (match-end 0))
      (forward-line))))

(defun goby-insert-pause ()
  "Insert a pause sign."
  (interactive)
  (goby-draw-pause))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun goby-top-line ()
  "Move this line onto the first line of this window."
  (interactive)
  (if (re-search-backward "^\014" nil t) (forward-line))
  (recenter 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving property
;;;

(defun goby-prop-construct (beg end frc)
  (cons beg (cons end frc)))

(defun goby-prop-get-beg (prop)
  (nth 0 prop))

(defun goby-prop-get-end (prop)
  (nth 1 prop))

(defun goby-prop-get-math (prop)
  (nthcdr 2 prop))

(defun goby-prop-get-frc (prop)
  (nthcdr 2 prop))

(defun goby-get-property1 (target)
  (let (next frc prop props)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
		  (setq next (next-single-property-change (point) target)))
	(when (setq frc (get-text-property (point) target))
	  (setq prop (goby-prop-construct (point) next frc))
	  (setq props (cons prop props)))
	(goto-char next)))
    (nreverse props)))

(defun goby-get-property ()
  (let ((faces (goby-get-property1 'goby-face))
	(math  (goby-get-property1 'goby-math)))
    (if math (setq faces (cons (cons 'goby-math math) faces)))
    faces))
    
(defun goby-save-property (&optional to-buffer)
  (let ((props (goby-get-property))
	(file (buffer-file-name))
	nl)
    (when props
      (save-excursion
	(goto-char (point-max))
	(unless (bolp) (setq nl t))
	(if to-buffer
	    (progn
	      (if nl (insert "\n"))
	      (insert goby-properties-string "\n" (pp props)))
	  (with-temp-buffer
	    (if nl (insert "\n"))
	    (insert goby-properties-string "\n" (pp props))
	    (write-region (point-min) (point-max) file 'append)))))))

(defun goby-put-property-face (props)
  (let (prop beg end face frc tabnum color family old-family)
    (while props
      (setq prop (car props))
      (setq props (cdr props))
      (setq beg (goby-prop-get-beg prop))
      (setq end (goby-prop-get-end prop))
      (setq frc (goby-prop-get-frc prop))
      ;; transition
      (unless (numberp (goby-face-get-color frc))
	(save-excursion
	  (goto-char beg)
	  (setq tabnum (goby-count-tabs)))
	(setq color (goby-get-tabnum-num tabnum))
	(goby-face-set-color frc color))
      (setq old-family (goby-face-get-family frc))
      (when (stringp old-family)
	(cond
	 ((= (length frc) 4)
	  (setq family goby-m-fonts-base))
	 ((or (string-match "gothic" old-family)
	      (string-match "mincho" old-family))
	  (setq family goby-j-fonts-base))
	 (t
	  (setq family goby-a-fonts-base)))
	(goby-face-set-family frc family))
      (when (= (length frc) 4)
	(setcdr (nthcdr 2 frc) nil)
	(goby-face-set-family frc goby-m-fonts-base))
      ;;
      (setq face (apply 'goby-make-face frc))
      (put-text-property beg end 'face face)
      (put-text-property beg end 'goby-face frc))))

(defun goby-put-property-math (props)
  (let (prop beg end math)
    (while props
      (setq prop (car props))
      (setq props (cdr props))
      (setq beg (goby-prop-get-beg prop))
      (setq end (goby-prop-get-end prop))
      (setq math (goby-prop-get-math prop))
      (goby-face-math-set-prop beg end math))))

(defun goby-put-property (props)
  (catch 'loop
    (while props
      (cond
       ((eq 'goby-math (car (car props)))
	(goby-put-property-math (cdr (car props))))
       (t
	(goby-put-property-face props)
	(throw 'loop nil)))
      (setq props (cdr props)))))

(defun goby-load-property ()
  (let ((regex (concat "^" (regexp-quote goby-properties-string) "$"))
	beg props)
    (save-excursion
      (when (re-search-forward regex nil t)
	(beginning-of-line)
	(setq beg (point))
	(forward-line)
	(setq props (read (buffer-substring (point) (point-max))))
	(delete-region beg (point-max))
	props))))

(defun goby-insert-newpage ()
  (interactive)
  (insert 12))

(provide 'goby)

;;; Copyright Notice:

;; Copyright (C) 2003 Kazu Yamamoto
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; goby.el ends here
