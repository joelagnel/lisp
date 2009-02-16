;;; slashdot.el --- Get slashdot headings from the net

;; Copyright (C) 2001 Sean MacLennan
;; $Revision: 1.18 $ $Date: 2003/10/28 00:02:58 $
;; XEmacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License version
;; 2 as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABIL`ITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This is for all us /. addicts out there :-) A little /. glyph shows up in
;; the modeline like the mail glyph. When you click on the glyph, it pops up
;; a buffer with the current slashdot headings. If you left click on the
;; header, `browse-url' is called. If you right click, a menu pops up to
;; allow you to choose where to send the url.
;;
;; The headings are stored in a file when/if you exit Emacs and loaded
;; upon startup. See `slashdot-headings-file'. A nil value turns this
;; feature off.
;;
;; Usage: (require 'slashdot)
;;        (slashdot-start)

(add-to-list 'load-path "~/repository/lang/lisp/emacs-el/sites")
(require 'http)
(require 'itimer)
(require 'browse-url)
(unless (boundp 'running-xemacs) (require 'extent))

(provide 'slashdot)

(defvar slashdot-xemacs (boundp 'running-xemacs))

(defvar slashdot-icons-dir "~/.xemacs/"
  "*Directory containing slashdot icons")

(defvar slashdot-wget-options nil
  "*If non-nil, overrides the default `http-wget-options'.")

;; From http://slashdot.org/code.shtml
;; Do whatever you want, but don't access the file more than
;; once every 30 minutes.
(defvar slashdot-interval 1800 ; = 30 minutes
  "*Interval to check for new headings in seconds.")

(defvar slashdot-headings-file "~/.xemacs/slashdots"
  "*File to save current headings in")

(defvar slashdot-hide nil
  "*If non-nil, hides all read entries")

;; If you edit this, run (slashdot-load-headings)
(defvar slashdot-url-alist
  '(
    ("Slashdot" . "http://slashdot.org/slashdot.rdf")
    ("The Register" . "http://www.theregister.co.uk/tonys/slashdot.rdf")
;;    ("Freshmeat" . "http://freshmeat.net/backend/fm.rdf")
    ("NewsForge" . "http://newsforge.com/newsforge.rdf")
    ("OS news" . "http://osnews.com/files/recent.rdf")
    ("MadPenguin" . "http://www.madpenguin.org/backend.php")
    ("Kerneltrap" . "http://kerneltrap.org/node/feed")
    )
  "*Alist of names and urls.
If you change this, you must call `slashdot-start' for it to take effect.")

(defvar slashdot-keep-raw-buffers nil
  "*For debugging, keep the raw http buffers")

(defconst slashdot-url-browser-menu
  (list "Send URL to ..."
	["Emacs W3" (slashdot-browseit 'browse-url-w3)
	 (fboundp 'w3-fetch)]
	["Netscape" (slashdot-browseit 'browse-url-netscape)
	 (exec-installed-p browse-url-netscape-program)]
	["Opera" (slashdot-browseit 'browse-url-opera)
	 (exec-installed-p "opera")]
	["Mosaic"   (slashdot-browseit 'browse-url-mosaic)
	 (exec-installed-p browse-url-mosaic-program)]
	["Copy link URL" (slashdot-copy-url) t]
	))

;; Create the comment-warn face
(defface slashdot-heading-face
  '((((class color))  (:foreground "red" :bold t :underline t))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments.")

(defface slashdot-new-face
  '((((class color))  (:foreground "blue" :bold t))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments.")

(defface slashdot-anchor-face
  '((((class color))  (:foreground "blue"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments.")

(defface slashdot-red-face
  '((((class color))  (:foreground "red"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments.")

(defface slashdot-failed-face
  '((((class color))  (:foreground "black" :background "DeepPink"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:bold t)))
  "Face for failed downloads.")

;; local variables

;; If you change layout of slashdot-headings you *must* update
;; slashdot-add-entry along with the obvious places.
(defvar slashdot-headings nil
  "Lists of name url time state ((headings link) ...)")
(defvar slashdot-procs nil
  "List of active http processes and names.")

(defvar slashdot-keymap (make-sparse-keymap "slashdot"))

(defvar slashdot-sign nil)
(defvar slashdot-no-sign nil)
(defvar slashdot-err-sign nil)
(defvar slashdot-current-sign nil)
(defvar slashdot-new nil)
(defvar slashdot-anchor nil)

(defconst slashdot-month-array
  ["January" "February" "March" "April" "May" "June" "July"
   "August" "September" "October" "November" "December"])


;;;###autoload
(defun slashdot-start ()
  (interactive)
  (unless slashdot-sign (slashdot-init-once))

  (unless (memq 'slashdot-current-sign global-mode-string)
    (setq slashdot-current-sign slashdot-no-sign)
    (setq global-mode-string
	    (append global-mode-string '(slashdot-current-sign))))

  (add-hook 'kill-emacs-hook 'slashdot-write-headings)

  (slashdot-load-headings)

  ;; run it once
  (slashdot-headings)

  (unless (itimer-live-p "slashdot")
    (start-itimer "slashdot" 'slashdot-headings
		  slashdot-interval slashdot-interval))
  t)

(defun slashdot-init-once ()
  "This creates all the keymaps and glyphs."
    (let (keymap)
      (if slashdot-xemacs
	  (progn
	    (setq keymap (make-sparse-keymap "slashdot modeline"))
	    (define-key keymap 'button1 'slashdot-show-headings)
	    (define-key slashdot-keymap 'button1 'slashdot-mousable)
	    (define-key slashdot-keymap 'button2 'slashdot-mousable)
	    (define-key slashdot-keymap 'button3 'slashdot-mousable))
	;; SAM unused (define-key keymap [mouse1] 'slashdot-show-headings)
	(define-key slashdot-keymap [mouse-1] 'slashdot-mousable)
	(define-key slashdot-keymap [mouse-2] 'slashdot-mousable)
	(define-key slashdot-keymap [mouse-3] 'slashdot-mousable))

      (setq slashdot-sign
	    (slashdot-make-sign " /." "slash.xpm" keymap))
      (setq slashdot-no-sign
	    (slashdot-make-sign nil "no-slash.xpm" keymap))
      (setq slashdot-err-sign
	    (slashdot-make-sign " /?" "bad-slash.xpm" keymap))))

;;;###autoload
(defun slashdot-stop ()
  (interactive)
  (when (itimer-live-p (get-itimer "slashdot"))
    (delete-itimer "slashdot"))

  (dolist (proc slashdot-procs)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq slashdot-procs nil)

  (when (memq 'slashdot-current-sign global-mode-string)
    (setq global-mode-string
	  (remove 'slashdot-current-sign global-mode-string))))

;;;###autoload
(defun slashdot-headings ()
  "Read the slashdot headings.
Usually called from a timer."
  (interactive)
  ;; reap dead procs
  (let (list)
    (dolist (proc slashdot-procs)
      (when (process-live-p (car proc))
	(setq list (cons proc list))))
    (setq slashdot-procs list))
  (if slashdot-procs
      (when (interactive-p)
	(error "Request already in progress"))
    (setq slashdot-new nil)
    (let ((http-wget-options (if slashdot-wget-options
				 slashdot-wget-options http-wget-options)))
      (dolist (entry slashdot-url-alist)
	(setq slashdot-procs
	      (cons (list (http-get-page (cdr entry)
					 (concat "*" (downcase (car entry)) "*")
					 'slashdot-sentinel)
			  (car entry))
		    slashdot-procs))))))

(defun slashdot-set-state (list hlist)
  (let ((old (assoc (car list) (nth 4 hlist))))
    (if old
	(if (eq (nth 2 old) 'read)
	    (nconc list (list 'read))
	  (nconc list (list 'old)))
      (nconc list (list 'new))))
  list)

(defun slashdot-rebuild-list (hlist &optional list)
  (let (headings)
    (setq slashdot-new t)
    (setf (nth 2 hlist) (current-time))
    (slashdot-set-state list hlist)
    (when list (setq headings (list list)))
    (while (setq list (slashdot-find-item))
      (slashdot-set-state list hlist)
      (nconc headings (list list)))
    (setf (nth 4 hlist) headings)
    (unless slashdot-keep-raw-buffers
      (kill-buffer nil))))

(defun slashdot-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (let (name hlist)
      (unless (setq name (cadr (assoc proc slashdot-procs)))
	(error "slashdot-sentinel called for a non-slashdot proc"))
      (setq hlist (assoc name slashdot-headings))
      (when (looking-at "HTTP/.\\.. 30[0-9]")
	;; redirect - look for good heading
	(when (re-search-forward "HTTP/.\\.. 200" nil t)
	  (goto-char (match-beginning 0))))
      ;; slashdot.org sometimes returnes 202
      (if (looking-at "HTTP/.\\.. 20[02]")
	  ;; Find the items
	  (let (list entry)
	    ;; Slashdot sometimes gzips the contents
	    (when (search-forward "Content-Encoding: gzip" nil t)
	      (http-gunzip))
	    (if hlist
		(progn
		  (setf (nth 3 hlist) 'heading) ;; reset
		  (if (setq list (slashdot-find-item))
		      ;; If the first heading in the buffer matches the first
		      ;; heading in the cached headings, we can assume all the
		      ;; headings match
		      (unless (equal (car list) (car (car (nth 4 hlist))))
			;; We have new headings - just rebuild the entire list
			(slashdot-rebuild-list hlist list))
		    ;; No items is considered an error
		    (unless slashdot-new (setq slashdot-new 'err))))
	      ;; New entry in slashdot-url-alist?
	      (if (setq entry (assoc name slashdot-url-alist))
		  ;; new entry
		  (progn
		    (slashdot-rebuild-list (slashdot-add-entry entry)))
		(unless slashdot-new (setq slashdot-new 'err)))))
	;; Failed to read headings
	(if hlist (setf (nth 3 hlist) 'err))
	(unless slashdot-new (setq slashdot-new 'err))))
    ;; Cleanup
    (setq slashdot-procs (remassoc proc slashdot-procs))
    (unless slashdot-procs
      (when slashdot-new
	(setq slashdot-current-sign
	      (if (eq slashdot-new 'err) slashdot-err-sign slashdot-sign)))
      ;; May need to update buffer
      (when (get-buffer-window "*slashdot headings*")
	(slashdot-update-buffer)))))

;; Format of rdf item
;; [ws]<item[ rdf:about="link"]>\n
;; [ws]<title>[ws]title</title>\n
;; [[ws]<description>description</description>\n]
;; [ws]<link>link</link>[\n]
;; [ws]</item>
(defun slashdot-find-item ()
  "Find the next item and return the title and link"
  (let (title link)
    (and (re-search-forward "<item[> ]" nil t) ;; don't match <items>
	 (re-search-forward "<title> *\\(.*\\)</title>" nil t)
	 (setq title (match-string 1))
	 (re-search-forward "<link>\\(.*\\)</link>" nil t)
	 (setq link (match-string 1))
	 (search-forward "</item>" nil t)
	 (list title (slashdot-link-quote link)))))

;; This is really for madpenguin.org
;; It is not safe to run through slashdot-quote
(defun slashdot-link-quote (link)
  (while (string-match "&amp;" link)
    (setq link (replace-match "&" nil nil link)))
    link)

;; SAM FIX THIS
(defun slashdot-quote (heading)
  "This handles the special chars used by slashdot"
  (while (string-match "&amp;" heading)
    (setq heading (replace-match "&" nil nil heading)))
  (while (string-match "&quot;" heading)
    (setq heading (replace-match "\"" nil nil heading)))
  (while (string-match "&lt;" heading)
    (setq heading (replace-match "<" nil nil heading)))
  (while (string-match "&gt;" heading)
    (setq heading (replace-match ">" nil nil heading)))
  (while (string-match "&#163;" heading)
    (setq heading (replace-match "\243" nil nil heading)))
  (while (string-match "</?[a-zA-Z]*>" heading)
    (setq heading (replace-match "" nil nil heading)))
  heading)

(defun slashdot-show-headings (event)
  "Called when user clicks on /. in mode line."
  (interactive "e")
  (slashdot-display))

(defun slashdot-display ()
  (interactive)
  (unless slashdot-headings
    (error "No headings to display"))
  (slashdot-update-buffer)
  (setq slashdot-current-sign slashdot-no-sign))

;; For Emacs we must do a set-buffer since many low-level calls
;; do not accept a buffer arg (e.g. bobp)
(defun slashdot-update-buffer ()
  "Low level routine to update the headings buffer."
  (save-excursion
    (with-output-to-temp-buffer "*slashdot headings*"
      (let ((buff (get-buffer "*slashdot headings*"))
	    start time extent)
	(set-buffer buff)
	(dolist (entry slashdot-headings)
	  (setq time (decode-time (nth 2 entry)))
	  (unless (bobp) (princ "\n\n"))
	  (setq start (point))
	  (princ (car entry))
	  (slashdot-make-extent start (point) buff (nth 1 entry) (nth 3 entry))
	  (princ (format " for %s %d %d:%d\n\n"
			 (aref slashdot-month-array (1- (nth 4 time)))
			 (nth 3 time) (nth 2 time) (nth 1 time)))
	  (dolist (heading (nth 4 entry))
	    (setq start (point))
	    (princ (slashdot-quote (car heading)))
	    (princ "\n") ;; SAM
	    (setq extent
		  (slashdot-make-extent start (point) buff (nth 1 heading)
					(nth 2 heading)))
	    (set-extent-property extent 'heading heading)
	    (when (eq (nth 2 heading) 'read)
	      (set-extent-property extent 'invisible t))
;; SAM	    (princ "\n")))
	    ))

	;; Mark read/Popdown
	(princ "\n")
	(setq start (point))
	(princ "Mark All Read")
	(slashdot-make-extent start (point) buff)
	(princ "\n")
	(setq start (point))
	(princ "Close Window")
	(slashdot-make-extent start (point) buff)
	))))

(defun slashdot-write-headings ()
  "Write the current headings to a file."
  (when slashdot-headings-file
    (save-excursion
      (let ((buff (find-file-noselect slashdot-headings-file)))
	(set-buffer buff)
	(erase-buffer)
	(pp slashdot-headings buff)
	(save-buffer)
	(kill-buffer buff)))))

;; Add a new `slashdot-url-alist' entry to `slashdot-headings'
;; Returns new entry
(defun slashdot-add-entry (entry)
  (let ((url (cdr entry))
	hlist)
    (when (string-match "http://[^/]+/" url)
      (setq url (match-string 0 url)))
    (setq hlist
	  (list (list (car entry) url nil 'heading nil)))
    (setq slashdot-headings
	  (nconc slashdot-headings hlist))
    hlist))

(defun slashdot-load-headings ()
  "Load the `slashdot-headings-file'.
This will always overwrite the current headings list."
  (setq slashdot-headings nil)

  (when (file-readable-p slashdot-headings-file)
    (save-excursion
      (let ((buff (find-file-noselect slashdot-headings-file))
	    list)
	(set-buffer buff)
	(goto-char (point-min))
	(setq list (read buff))
	(when (listp list)
	    (setq slashdot-headings list)
	    (message "Slashdot headings loaded."))
	(kill-buffer buff))))

  (dolist (entry slashdot-url-alist)
    (unless (assoc (car entry) slashdot-headings)
      (slashdot-add-entry entry))))

;; ---------------------------------------------------------------------

(unless (fboundp 'remassoc)
  ;; This is probably not optimal
  (defun remassoc (key alist)
    (let (new)
      (dolist (elmt alist)
	(unless (and (consp elmt) (equal (car elmt) key))
	  (setq new (cons elmt new))))
      (nreverse new)))
  )

(unless (fboundp 'process-live-p)
  (defun process-live-p (object)
    (let ((status (process-status object)))
      (cond ((eq status 'run) t)
	    ((eq status 'stop) t)
	    ((eq status 'open) t)
	    (t nil))))
  )

(defun slashdot-make-sign (str xpm keymap)
  (if slashdot-xemacs
      (let ((sign (cons (make-extent nil nil)
			(make-glyph  (concat slashdot-icons-dir xpm)))))
	(set-extent-property (car sign) 'keymap keymap)
	sign)
    str))

(defun slashdot-make-extent (start end buff &optional anchor state)
  (let ((extent (make-extent start end buff)))
    (set-extent-mouse-face extent 'highlight)
    (set-extent-keymap extent slashdot-keymap)
    (when anchor
      (set-extent-property extent 'anchor anchor))
    (cond ((eq state 'heading)
	   (set-extent-face extent 'slashdot-heading-face))
	  ((eq state 'read)
	   (set-extent-face extent 'default))
	  ((eq state 'old)
	   (set-extent-face extent 'slashdot-anchor-face))
	  ((eq state 'err)
	   (set-extent-face extent 'slashdot-failed-face))
	  (anchor (set-extent-face extent 'slashdot-new-face))
	  (t (set-extent-face extent 'slashdot-red-face)))
    extent))

(defun slashdot-mark-all-read ()
  (interactive)
  (let* ((buff (get-buffer "*slashdot headings*"))
	 (extents (extent-list buff))
	 heading)
    (dolist (extent extents)
      (setq heading (extent-property extent 'heading))
      (when heading
	;; Note: Headers do not have a heading
	(setf (nth 2 heading) 'read)
	(set-extent-face extent 'default))
      )))

(defun slashdot-hide-read ()
  (interactive)
  (let* ((buff (get-buffer "*slashdot headings*"))
	 (extents (extent-list buff))
	 heading)
    (setq slashdot-hide t)
    (dolist (extent extents)
      (setq heading (extent-property extent 'heading))
      (when (and heading (eq (nth 2 heading) 'read))
	(set-extent-property extent 'invisible t)))))

(defun slashdot-mousable (event)
  "This is called on a mouse click in the display window.
This version can handle urls, the browser menu, and deleting the window."
  (interactive "e")
  (let ((extent (extent-at (event-point event) (event-buffer event)))
	(button (event-button event)))
    (unless extent (error "No extent at point"))
    (setq slashdot-anchor extent)
    (if (extent-property extent 'anchor)
	(if (eq button 3)
	    (popup-menu slashdot-url-browser-menu)
	  (slashdot-browseit 'browse-url))
      (if (string= (extent-string extent) "Close Window")
	  (delete-window (event-window event))
	(slashdot-mark-all-read))
	)))

(defun slashdot-browseit (browse-function)
  (let ((url (extent-property slashdot-anchor 'anchor))
	(heading (extent-property slashdot-anchor 'heading)))
    (apply browse-function url nil)
    (setq slashdot-current-sign slashdot-no-sign)
    (when heading
      ;; Note: Headers do not have a heading
      (setf (nth 2 heading) 'read)
      (set-extent-face slashdot-anchor 'default))
    ))

(defun slashdot-copy-url ()
  (let ((url (extent-property slashdot-anchor 'anchor)))
    (own-clipboard url t)))
