;;; xsteve-functions.el --- Misc Emacs functions from Stefan Reichör
;written by Stefan Reichör, stefan@xsteve.at


;;; Commentary:
;;

(require 'dired)

;;; History:
;;

;;; Code:
(eval-when-compile
  (require 'pcl-cvs "pcl-cvs" t)
  (require 'bookmark)
  (require 'bbdb-com nil t))

(defun xsteve-insert-date (dayincr)
  "Inserts a date-stamp at point - Format: \"DD-MM-YYYY (wd)\""
  (interactive "p")
  (if (null current-prefix-arg) (setq dayincr 0))
  (let* ((base 65536.0)
         (nowlist (current-time))
         (datenum (+ (*  (nth 0 nowlist) base) (nth 1 nowlist)
                     (* dayincr 60.0 60.0 24.0)))
         (s (current-time-string
             (list (truncate( / datenum base)) (truncate(mod datenum base)))))
         (date))
    (if (equal current-prefix-arg '(4))
        (progn
          (let ((bound (line-beginning-position))
                (datenum)
                (datestring))
            (goto-char (min (point-max) (+ (point) 10)))
            (re-search-backward "[0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9][0-9][0-9]" bound)
            (setq datestring (buffer-substring (point) (+ (point) 10)))
            (setq datenum (calendar-absolute-from-gregorian
                           (list
                            (string-to-number (substring datestring 3 5))
                            (string-to-number (substring datestring 0 2))
                            (string-to-number (substring datestring 6 10)))))
            (setq dayincr (string-to-number (read-string "Increment by days: " "7")))
            (delete-region (point) (+ 10 (point)))
            (setq date (calendar-gregorian-from-absolute (+ datenum dayincr)) datestring)))
      (setq date (list (length (member (substring s 4 7)
                                       '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
                                         "Jun" "May" "Apr" "Mar" "Feb" "Jan")))
                       (string-to-number (substring s 8 10))
                       (string-to-number (substring s 20 24)))))
                       ;;(cdr (assoc (substring s 0 3)
                       ;;            '(("Son" . "So")("Mon" . "Mo")("Tue" . "Di")
                       ;;              ("Wed" . "Mi")("Thu" . "Do")("Fri" . "Fr")
                       ;;              ("Sat" ."Sa")))))))

    (insert (format "%02d.%02d.%04d" (nth 1 date) (nth 0 date) (nth 2 date)))))
    ;(message "%s" date)))

;(xsteve-insert-date 0)

;02.05.2001
;note that function does not work reliably, it works now for 2005
(defun calendar-week-number (date)
  "Return the week number for DATE.
The week starts on MONDAY."
  (let* ((year (extract-calendar-year date))
         (day-number (calendar-day-number date))
         (day-of-week-first-day (calendar-day-of-week (list 1 1 year)))
         (adjust))
    (when (eq 0 day-of-week-first-day)
      (setq day-of-week-first-day 7))
    (setq adjust (% (- 9 day-of-week-first-day) 8))
    (if (< day-number adjust)
        (calendar-week-number (list 12 31 (- year 1)))
      (+ 1 (/ (- day-number adjust) 7)))))
;(calendar-week-number '(12 31 2004))
;(calendar-week-number '(1 1 2005))
;(calendar-week-number '(2 1 2005))
;(calendar-week-number '(3 1 2005))

;taken from vhdl-mode
(defun xsteve-remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d Trailing spaces removed from buffer." remove-count))))))

;02.02.2000
(defun xsteve-remove-control-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

;24.07.2000
(defun xsteve-untabify-buffer (prefix)
  "Untabify the whole buffer. Calls untabify for the whole buffer. If called with
prefix argument: use prefix argument as tabwidth"
  (interactive "p")
  (let ((tab-width (or current-prefix-arg tab-width)))
    (untabify (point-min) (point-max)))
  (message "Untabified buffer."))

(defun xsteve-replace-identifier ()
  "Replace thing at point with another string."
  (interactive)
  (let* ((old-string (xsteve-get-symbol-at-point "Replace: " current-prefix-arg))
         (new-string (read-string (concat "Replace " old-string " with: ") "" nil old-string)))
    (save-excursion
      (deactivate-mark)
      (goto-char (point-min))
      (query-replace-regexp old-string new-string))))

(defun xsteve-occur-identifier ()
  "Open occur buffer with identifier at point."
  (interactive)
  (occur (xsteve-get-symbol-at-point "Find occurances in buffer (regex): ") current-prefix-arg)
  (xsteve-resize-other-window))

; (defadvice occur (after turn-occur-into-compile-buffer activate)
;   (let ((this-buffer-file-name (file-relative-name buffer-file-name)))
;     (save-excursion
;       (other-window 1)
;       (if (equal major-mode 'occur-mode)
;         (progn
;             (toggle-read-only)
;             (beginning-of-buffer)
;             (while (re-search-forward "^ *[0-9]+:" nil t)
;               (replace-match (concat this-buffer-file-name
;                                      ":\\&" nil nil)))
;             (compilation-mode)))
;       (other-window -1))))

;10.05.2000
(defvar app-window-name nil "Application window name")
(defun xsteve-switch-to-app-window()
  "Switch to the window specified by the variable app-window-name, when that
Variable exists."
  (interactive)
  (if app-window-name
      (sww app-window-name)
    (message "app-window-name not set")))

(defun xsteve-toggle-narrow()
  "Narrow to region, iff region is marked, otherwise widen"
  (interactive)
  (if mark-active
      (narrow-to-region (region-beginning) (region-end))
    (widen)))

;(add-text-properties 0 12 '(is-region t) hu)
;(text-property-any 0 12 'is-region t hu)

;(add-text-properties 0 1 '(is-region t) (current-kill 0 t))

;(text-property-any 0 1 'is-region t (current-kill 0 t))

;17.02.2000
(defvar xsteve-find-in-files-excludes '("semantic.cache" "TAGS"))
(defun xsteve-find-in-files (regexp)
  "Find REGEXP in files using grep."
  (interactive (list (xsteve-get-symbol-at-point  "Find in Files (regex): " t)))
  (let* ((grep-default-arg (concat "-n -i --directories=skip" (mapconcat '(lambda (arg) (concat " --exclude=" arg))  xsteve-find-in-files-excludes "")))
         (grepcmd
          (if current-prefix-arg
              (read-string (concat "Find \"" regexp "\" Grep command: ")
                           (concat "grep " grep-default-arg))
            (concat "grep " grep-default-arg)))
         (fileglob (read-string (concat "Find \"" regexp "\" in Files (filemask): ") "*")))
    (grep (concat grepcmd " -e \"" regexp "\" " fileglob))))

;22.04.2003
(defun xsteve-find-in-files-and-subdirs ()
  "Find regexp in files using grep."
  (interactive)
  (let* ((regexp (xsteve-get-symbol-at-point  "Find in Files (regex): " t))
         (grep-default-arg "-R -n -i --exclude=semantic.cache --exclude=TAGS")
         (default-directory
           (if current-prefix-arg
               (read-string "Search directory: " default-directory)
             default-directory))
         (grepcmd
          (if current-prefix-arg
              (read-string (concat "Find \"" regexp "\" Grep command: ")
                           (concat "grep " grep-default-arg))
            (concat "grep " grep-default-arg)))
         (fileglob (read-string (concat "Find \"" regexp "\" in Files (filemask): ") "*")))
    (grep (concat grepcmd " -e \"" regexp "\" " fileglob))))

(unless (fboundp 'substring-no-properties)
  (defun substring-no-properties (string)
    (substring string 0)))
;18.02.2000
(defun xsteve-get-symbol-at-point (&optional msg-prompt prompt-always no-regexp-quote)
  (interactive)
  (let* ((region-string (if mark-active
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          nil))
         (symbol (cond
                  (mark-active
                   (progn
                     (setq region-string (if no-regexp-quote region-string (regexp-quote region-string)))))
                  (t (thing-at-point 'symbol)))))
    (when (or prompt-always
              (not symbol))
      (setq symbol (read-string (or msg-prompt "Enter String: ") symbol)))
    (substring-no-properties symbol)))

;07.03.2000
(defun xsteve-resize-other-window ()
  (interactive)
  (save-excursion
    (other-window 1)
    (resize-temp-buffer-window)
    (other-window -1)))

;10.10.2000
(defun xsteve-quit-buffer ()
  "Delete the current buffer and the corresponding window also"
  (interactive)
  (kill-buffer (current-buffer))
  (when (> (count-windows) 1)
    (delete-window)))

(add-hook 'diff-mode-hook '(lambda () (define-key diff-mode-shared-map "q" 'xsteve-quit-buffer)))
(define-key occur-mode-map "q" 'xsteve-quit-buffer)
;(define-key compilation-mode-map "q" 'xsteve-quit-buffer)

(defun xsteve-show-message-buffer (arg)
  "Show the *message* buffer.
When called with a prefix argument, show the *trace-output* buffer."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (pop-to-buffer (if arg "*trace-output*" "*Messages*"))
    (goto-char (point-max))
    (recenter -12)
    (pop-to-buffer buffer)))

;16.05.2002
(defun xsteve-split-window ()
  "Split the current window and show in the window below the next buffer in the buffer list.
When called twice restore the window configuration before the split."
  (interactive)
  (if (eq last-command 'xsteve-split-window)
      (progn
        (set-window-configuration xsteve-split-window-configuration)
        (setq this-command 'xsteve-unsplit-window))
    (let ((buf-list)
          (cur-buf (current-buffer)))
      (setq xsteve-split-window-configuration (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (setq buf-list (buffer-list))
      (delq (get-buffer " *Minibuf-0*") buf-list)
      (delq (get-buffer " *Minibuf-1*") buf-list)
      (pop-to-buffer (cadr buf-list))
      (pop-to-buffer cur-buf))))

;; 25.08.2004
(defun xsteve-flip-windows ()
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))


;07.03.2000
(defun xsteve-exchange-slash-and-backslash ()
  "Exchanges / with \ and in the current line or in the region when a region-mark is active."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((replace-count 0)
            (eol-pos (if mark-active (region-end) (progn (end-of-line) (point))))
            (bol-pos (if mark-active (region-beginning) (progn (beginning-of-line) (point)))))
        (goto-char bol-pos)
        (while (re-search-forward "/\\|\\\\" eol-pos t)
          (setq replace-count (+ replace-count 1))
          (cond ((string-equal (match-string 0) "/") (replace-match "\\\\" nil nil))
                ((string-equal (match-string 0) "\\") (replace-match "/" nil nil)))
          (message (format "%d changes made." replace-count)))))))

(defun xsteve-copy-line (&optional append)
  "Copies the actual line to the kill ring.
if the optional argument append eq t then append the line to the kill ring."
  (interactive)
  (save-excursion
    (let (start end)
      (beginning-of-line)
      (setq start (point))
      (forward-line 1)
      (setq end (point))
      (if append
          (progn
            (append-next-kill)
            (message "appended line: %s" (buffer-substring start end)))
        (message "%s" (concat "copied: " (buffer-substring start end))))
      (copy-region-as-kill start end)))
  (next-line 1))

;;; Copyright (C) 1997, 1998 Thien-Thi Nguyen
(defun another-line ()
  "Copy line, preserving cursor column, and increment any numbers found.
This should probably be generalized in the future."
  (interactive)
  (let* ((col (current-column))
	 (bol (progn (beginning-of-line) (point)))
	 (eol (progn (end-of-line) (point)))
	 (line (buffer-substring bol eol)))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" eol 1)
      (let ((num (string-to-int (buffer-substring
				  (match-beginning 0) (match-end 0)))))
	(replace-match (int-to-string (1+ num)))))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col)))

;;; by Alex Schroeder
;;; Number regex improved by Charlie Hethcoat
(defun calculator-sum-column (start end)
  "Adds all integer, decimal, and floating-point numbers found in the
selected rectangle."
  (interactive "r")
  (save-excursion
    (kill-rectangle start end)
    (exchange-point-and-mark)
    (yank-rectangle)
    (set-buffer (get-buffer-create "*calc-sum*"))
    (erase-buffer)
    (yank-rectangle)
    (exchange-point-and-mark)
    (let ((sum 0))
      (while (re-search-forward
              "[-+]?\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?"
              nil t)
        ;; Examples of numbers it reads (nonexhaustive):  2 +2 -2
        ;; 2. +2. -2. 2.0 +2.0 -2.0 2e0 +2e0 -2e0 2E0 2e+0 2e-0,
        ;; 2.e0, 2.0e0, etc.
        (setq sum (+ sum (string-to-number (match-string 0)))))
      (message "Sum: %f" sum))))

;;From: Joe Fineman <jcf@TheWorld.com>
(defun xsteve-transpose-appropriate-chars (char)
  "Transpose the last 2 chars, or the 1 after CHAR with CHAR."
  (interactive "cFirst character of pair:  ")
  (let ((origin (point)))
    (if (< char ? )
        (forward-char -2)
      (search-backward (char-to-string char)))
    (forward-char 2)
    (insert (char-after (- (point) 2)))
    (delete-region (- (point) 3) (- (point) 2))
    (goto-char origin)))

;;From: Henrik Enberg
(defun query-remove-doubled-words (&optional force)
  "Find all doubled words and ask to remove them.
With optional arg FORCE remove them without asking."
  (interactive "P")
  (let ((case-fold-search t)
	(del-counter 0))
    (while (re-search-forward
	    "\\(\\<\\w\\{3,\\}\\>\\)[ \t\n]*\\(\\1\\)" nil t)
      (replace-highlight (match-beginning 2) (match-end 2))
      (unwind-protect
	  (when (or force (y-or-n-p "Remove this doubled word? "))
	    (delete-region (match-beginning 2) (match-end 2))
	    (canonically-space-region (match-beginning 0) (match-end 0))
	    (setq del-counter (1+ del-counter)))
	(replace-dehighlight)))
    (if (> del-counter 0)
	(message "Removed %d doubled %s." del-counter
		 (if (< del-counter 1) "words" "word"))
      (message "No doubled words found or removed."))))

;;From: Joe Fineman <jcf@TheWorld.com>
(defun xsteve-replace-char (char1 char2)
  "Remove the most recent instance of CHAR1; replace it with CHAR2 if not the same."
  (interactive "cCharacter to remove:  \ncReplace it with:  ")
  (save-excursion
    (search-backward (char-to-string char1))
    (delete-char 1)
    (unless (= char2 char1)
      (insert char2))))

;;From: Joe Fineman <jcf@TheWorld.com>
(defun xsteve-insert-missing-char (predecessor insertion)
  "After the most recent PREDECESSOR, insert INSERTION."
  (interactive "cAfter:  \ncInsert:  ")
  (save-excursion
    (search-backward (char-to-string predecessor))
    (forward-char 1)
    (insert insertion)))

;diff-buffer-with-file is now part of emacs
;From EmacsWiki:KillKey
;; (defun diff-buffer-with-associated-file ()
;;   "View the differences between BUFFER and its associated file.
;;  This requires the external program \"diff\" to be in your `exec-path'."
;;   (interactive)
;;   (let ((buf-filename buffer-file-name)
;;         (buffer (current-buffer)))
;;     (unless buf-filename
;;       (error "Buffer %s has no associated file" buffer))
;;     (let ((diff-buf (get-buffer-create
;;                      (concat " *Assoc file diff: "
;;                              (buffer-name)
;;                              "*"))))
;;       (with-current-buffer diff-buf
;;         (setq buffer-read-only nil)
;;         (erase-buffer))
;;       (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
;;         (unwind-protect
;;             (progn
;;               (with-current-buffer buffer
;;                 (write-region (point-min) (point-max) tempfile nil 'nomessage))
;;               (if (zerop
;;                    (apply #'call-process "diff" nil diff-buf nil
;;                           (append
;;                            (when (and (boundp 'ediff-custom-diff-options)
;;                                       (stringp ediff-custom-diff-options))
;;                              (list ediff-custom-diff-options))
;;                            (list buf-filename tempfile))))
;;                   (message "No differences found")
;;                 (progn
;;                   (with-current-buffer diff-buf
;;                     (goto-char (point-min))
;;                     (if (fboundp 'diff-mode)
;;                         (progn (diff-mode)
;;                                (diff-context->unified (point-min) (point-max))))
;;                       (fundamental-mode)))
;;                   (display-buffer diff-buf)))
;;           (when (file-exists-p tempfile)
;;             (delete-file tempfile)))))
;;     nil))
;; ;; tidy up diffs when closing the file
;; (defun kill-associated-diff-buf ()
;;   (let ((buf (get-buffer (concat " *Assoc file diff: "
;;                                  (buffer-name)
;;                                  "*"))))
;;     (when (bufferp buf)
;;       (kill-buffer buf))))
;; (add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

;;(global-set-key (kbd "C-c d") 'diff-buffer-with-associated-file)
;(defun de-context-kill (arg)
;  "Kill buffer, taking gnuclient into account."
;  (interactive "p")
;  (when (and (buffer-modified-p)
;             (not (string-match "\\*.*\\*" (buffer-name)))
;             (= 1 arg))
;    (diff-buffer-with-associated-file)
;    (error "Buffer has unsaved changes"))
;  (if (and (boundp 'gnuserv-minor-mode)
;           gnuserv-minor-mode)
;      (gnuserv-edit)
;    (set-buffer-modified-p nil)
;    (kill-buffer (current-buffer))))
; (global-set-key (kbd "C-x k") 'de-context-kill)

;; From Charles Sebold, 29.10.2001
; I can cycle one way with bury-buffer, and the other way with this
(defun xsteve-unbury-buffer ()
  "Reverse bury-buffer."
  (interactive)
  (switch-to-buffer (nth (- (length (buffer-list)) 1) (buffer-list))))

;; 29.10.2001
; From Philip Lijnzaad <lijnzaad@ebi.ac.uk> in gnu.emacs.help
; Functions to switch dos/unix modes
(defun dos-line-endings ()
  "sets the buffer-file-coding-system to undecided-dos; changes the buffer
    by invisibly adding carriage returns"
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun unix-line-endings ()
  "sets the buffer-file-coding-system to undecided-unix; changes the buffer
    by invisibly removing carriage returns"
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))


;08.04.2003: Kai Großjohann
(defun increment-number-at-point (amount)
  "Increment number at point by given AMOUNT."
  (interactive "NIncrement by: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (old-num (number-at-point)))
    (unless old-num
      (error "No number at point"))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "%d" (+ old-num amount)))))

;20.08.2001, From: Cyprian Laskowski <cyp@swagbelly.net>
(defun describe-map (map)
  "Describe the key bindings of MAP."
  (interactive
   (list (intern (completing-read "Describe keymap: " obarray
				  #'(lambda (e)
                                      (and (boundp e)
                                           (string-match "-map$"
                                                         (symbol-name e))))
				  t))))
  (let (beg end)
    (with-temp-buffer
      (use-local-map (eval map))
      (describe-bindings))
    (set-buffer "*Help*")
    (rename-buffer (generate-new-buffer-name (concat "*" (symbol-name map) " bindings*")))
    (setq beg (and (re-search-forward "^Major Mode Bindings:$" nil t) (1+ (match-end 0)))
	  end (and (re-search-forward "^Global Bindings:$" nil t) (match-beginning 0)))
    (if (and beg end)
	(narrow-to-region beg end)
      (narrow-to-region 1 1)
      (error (concat (symbol-name map) " has no bindings set.")))))

;;04.12.2001 from Henrik Enberg
(defun info-link-to-kill-ring ()
  (interactive)
  (let ((curr-node Info-current-node)
	(node-list nil)
	(file (if Info-current-subfile
		  (concat default-directory
			  Info-current-subfile)
		Info-current-file)))
    (push curr-node node-list)
    (with-temp-buffer
      (insert-file-literally file)
      (goto-char (point-min))
      (when (re-search-forward "\\* \\(.*\\): (" nil t)
	(push (match-string 1) node-list)))
    (kill-new
     (concat "info://"
	     (mapconcat
	      (lambda (str)
		(apply 'concat
		       (mapcar (lambda (c)
				 (if (eq c ? )
				     "+"
				   (char-to-string c)))
			       str)))
	      node-list "/")))))

;22.08.2001: From: Mario Lang <mlang@home.delysid.org>
(defun rainorshine (ARG)
  "Uses www.rainorshine.com to extract international Weather
information. If called with a prefix argument, directly prompts
your for a location.
Otherwise it uses a predefined location URL."
  (interactive "P")
  (if ARG
      (progn
	(w3-fetch "http://www.rainorshine.com/index.ssf?world_search.tp")
	(search-forward "Afghanistan" nil nil nil)
	(widget-button-press (point))
	(search-forward "[Go" nil nil nil)
	(widget-button-press (point)))
    (w3-fetch "http://www.rainorshine.com/index.ssf?%24%24ZIPCITY=Linz%2C+Austria&type=international&Get+Forecast.x=0&Get+Forecast.y=0"))
  (goto-char (point-min))
  (search-forward "LINZ, AUSTRIA" nil nil nil)
  (forward-word -2)
  (let ((inhibit-read-only t)
	(start (point))
	(end (progn (search-forward "5-DAY" nil nil nil)
		    (end-of-line 11)
		    (point))))
    (kill-rectangle start end)
    (kill-buffer nil)
    (switch-to-buffer "*Weather in Linz*")
    (kill-all-local-variables)
    (kill-region (point-min) (point-max))
    (yank-rectangle)
    (goto-char (point-min))
    (text-mode)))

;12.04.2000
(defun xsteve-copy-symbol-at-point ()
  "Copies the actual symbol to the kill ring."
  (interactive)
  (let ((string (xsteve-get-symbol-at-point)))
    ;(kill-new string)
    (if (eq last-command 'kill-region)
        (progn
          (kill-append (concat " " string) nil)
          (message "%s appended" string))
      (kill-new string)
      (message "%s copied" string))))


;02.05.2000
(defun xsteve-embrace-selection (&optional front-arg rear-arg)
  (interactive)
  (let* ((front (or front-arg (read-string "Front brace: ")))
         (rear (or rear-arg (read-string "Rear brace: "))))
    (if mark-active
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert front))
          (save-excursion
            (goto-char (region-end))
            (insert rear)))
      (insert front)
      (save-excursion
        (insert rear)))))

; (defun xsteve-test-embrace()
;   (interactive)
;   (xsteve-embrace-selection "<" ">"))
; (global-set-key [(meta f10)] 'xsteve-test-embrace)

;12.05.2000
(defun xsteve-show-euro-as-ats()
  (interactive)
  (save-match-data
    (save-excursion
      (let* ((beg (+ (re-search-backward "[^0-9\\.]") 1))
             (end (- (progn (forward-char)(re-search-forward "[^0-9\\.]")) 1))
             (euro-str (buffer-substring-no-properties beg end))
             (euro-val (or current-prefix-arg
                        (string-to-number euro-str))))
        (message "%9.2f EURO = %9.2f ATS" euro-val (* euro-val 13.7603))))))

(defun xsteve-smart-home()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'xsteve-smart-home)
           (= (line-beginning-position) (point)))
    (beginning-of-line-text)
    (beginning-of-line)))

;17.05.2001
(defun xsteve-scroll-right()
  (interactive)
  (let ((line-length (- (line-end-position) (line-beginning-position))))
    (move-to-column (min (+ (current-column) 30) line-length))))

;17.05.2001
(defun xsteve-scroll-left()
  (interactive)
  (let ((line-length (- (line-end-position) (line-beginning-position))))
    (move-to-column (max (- (current-column) 30) 0))))


;10.10.2000
(defun zap-up-to-char (arg char)
  "Kill up to and excluding ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point)
               (progn
                 (search-forward
                  (char-to-string char) nil nil arg)
                 (progn (goto-char
                         (if (> arg 0) (1- (point)) (1+ (point))))
                        (point)))))

;25.07.2000
(defun xsteve-copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy BufferName (f)ull, (d)irectory, (n)ame, (w)ikiname or (q)uit?")
  ;(message "your choice %c" choice)
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?w)
           (setq new-kill-string (run-hook-with-args-until-success 'planner-annotation-functions))))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;(global-set-key [(meta f10)] 'xsteve-copy-buffer-file-name-as-kill)


;; hippie expand functions
;;28.11.2003: from MicheleBini (emacs wiki page)
;; completes via calc:
;; You must be on the end of the line. The line must end with: " = "
;; Then you can invoke this completion function!!
(defun xsteve-try-complete-with-calc-result (arg)
  (and
   (not arg) (eolp)
   (save-excursion
     (beginning-of-line)
     (when (and (boundp 'comment-start)
		comment-start)
       (when (looking-at
	      (concat
	       "[ \n\t]*"
	       (regexp-quote comment-start)))
	 (goto-char (match-end 0))
	 (when (looking-at "[^\n\t ]+")
	   (goto-char (match-end 0)))))
     (looking-at ".* \\(\\([;=]\\) +$\\)"))
   (save-match-data
     (require 'calc nil t))
   ;;(require 'calc-aent)
   (let ((start (match-beginning 0))
	 (op (match-string-no-properties 2)))
   (save-excursion
     (goto-char (match-beginning 1))
     (if (re-search-backward (concat "[\n" op "]") start t)
	 (goto-char (match-end 0)) (goto-char start))
     (looking-at (concat " *\\(.*[^ ]\\) +" op "\\( +\\)$"))
     (goto-char (match-end 2))
     (let* ((b (match-beginning 2))
	    (e (match-end 2))
	    (a (match-string-no-properties 1))
	    (r (calc-do-calc-eval a nil nil)))
       (when (string-equal a r)
	 (let ((b (save-excursion
		    (and (search-backward "\n\n" nil t)
			 (match-end 0))))
	       (p (current-buffer))
	       (pos start)
	       (s nil))
	   (setq r
		 (calc-do-calc-eval
		  (with-temp-buffer
		    (insert a)
		    (goto-char (point-min))
		    (while (re-search-forward
			    "[^0-9():!^ \t-][^():!^ \t]*" nil t)
		      (setq s (match-string-no-properties 0))
		      (let ((r
			     (save-match-data
			       (save-excursion
				 (set-buffer p)
				 (goto-char pos)
				 (and
				  ;; TODO: support for line
				  ;; indentation
				  (re-search-backward
				   (concat "^" (regexp-quote s)
					   " =")
				   b t)
				  (progn
				    (end-of-line)
				    (search-backward "=" nil t)
				    (and (looking-at "=\\(.*\\)$")
					 (match-string-no-properties 1))))))))
			(if r (replace-match (concat "(" r ")") t t))))
		    (buffer-substring (point-min) (point-max)))
		  nil nil))))
       (and
	r
	(progn
	  (he-init-string b e)
	  (he-substitute-string (concat " " r))
	  t)))))))

; From Sacha Chua:
(defun xsteve-try-expand-factoid-from-bbdb (old)
  "Try to expand from BBDB. If OLD is non-nil, cycle through other possibilites."
  (unless old
      ;; First time, so search through the BBDB records for the factoid.
    (progn
      (he-init-string (he-dabbrev-beg) (point))
      (setq he-expand-list nil)
      (mapc
       (lambda (item)
         ;(setq he-expand-list (append he-expand-list (list (bbdb-record-getprop item 'blog))))
         ;(setq he-expand-list (append he-expand-list (list (bbdb-record-getprop item 'web))))
         ;(setq he-expand-list (append he-expand-list (list (bbdb-record-getprop item 'notes))))
         (setq he-expand-list (append he-expand-list (list (car (bbdb-record-net item))))))
       (bbdb-search (bbdb-records) he-search-string he-search-string he-search-string he-search-string nil))
      (setq he-expand-list (delq nil he-expand-list))))
  (while (and he-expand-list
              (or (not (car he-expand-list))
                  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        nil)
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))


;; 10.12.2001
(setq buchstabier-list
'(("A" . "Anton") ("B" . "Berta") ("C" . "Cäsar") ("D" . "Dora") ("E" . "Emil")
  ("F" . "Friedrich") ("G" . "Gustav") ("H" . "Heinrich") ("I" . "Ida") ("J" . "Julius")
  ("K" . "Karl") ("L" . "Ludwig") ("M" . "Martha") ("N" . "Nordpol") ("O" . "Otto")
  ("P" . "Paul") ("Q" . "Quelle") ("R" . "Richard") ("S" . "Siegfried") ("T" . "Theodor")
  ("U" . "Ulrich") ("V" . "Victor") ("W" . "Wilhelm") ("X" . "Xanthippe")
  ("Y" . "Ypsilon") ("Z" . "Zacharias") ("Ö" . "Österreich")))

(defun buchstabiere (str)
  (interactive "sBuchstabiere: ")
  (message "%s" (mapcar (lambda (ch)
                          (cdr (assoc (char-to-string ch) buchstabier-list))) (upcase str))))

;25.01.2001
(defvar minibuffer-insert-directory-number 0)
(defvar minibuffer-insert-directory-start-point 0)
(defvar minibuffer-insert-directory-end-point 0)
(defvar minibuffer-insert-directory-keep-content 0)
(defun minibuffer-insert-directory-name (keep-content)
  (interactive "P")
  (let ((dir-name))
    (save-window-excursion
      (if (eq last-command 'minibuffer-insert-directory-name)
          (progn
            (delete-region minibuffer-insert-directory-start-point
                           minibuffer-insert-directory-end-point)
            (setq minibuffer-insert-directory-number
                  (+ 1 (mod minibuffer-insert-directory-number (- (count-windows) 1)))))
        (setq minibuffer-insert-directory-number 1)
        (setq minibuffer-insert-directory-keep-content keep-content))
      (other-window minibuffer-insert-directory-number)
      (setq dir-name (default-directory)))
    (unless minibuffer-insert-directory-keep-content (delete-region (point-min) (point-max)))
    (setq minibuffer-insert-directory-start-point (point))
    (insert dir-name)
    (setq minibuffer-insert-directory-end-point (point))))

;minibuffer-scroll-window
;(define-key minibuffer-local-map [(super d)] 'minibuffer-insert-directory-name)
;(define-key minibuffer-local-completion-map [(super d)] 'minibuffer-insert-directory-name)
;(global-set-key [(super i)] 'minibuffer-insert-directory-name)

;08.09.2000
(defun xsteve-define-alternatives (var alternatives)
"Provide a command to toggle a variable value between various alternatives."
  (let* ((alternatives-list (intern (concat (symbol-name var) "-alternative-list")))
         (toggle-function (intern (concat "toggle-" (symbol-name var))))
         (toggle-function-doc (concat "Toggles the values of " (symbol-name var))))
    (set alternatives-list (cons var alternatives))
    (set var (car alternatives))
    (fset toggle-function
          (` (lambda (&optional arg)
               (, toggle-function-doc)
               (interactive "P")
               (cond ((car-safe arg)
                      (message "%s: %s" (, (symbol-name var)) (, var)))
                     (t
                      (let ((next (cadr (memq (, var) (, alternatives-list)))))
                        (setq (, var) (or next (cadr (, alternatives-list)))))
                      (message "set %s to %s " (, (symbol-name var)) (, var)))))))))

;;(xsteve-define-alternatives 'shell-file-name
;;                            '("C:/emacs-20.4/bin/cmdproxy.exe" "c:/bin/sh.exe"))
;;(toggle-shell-file-name)

;;;comint stuff
;19.06.2000
(defun xsteve-comint-send-string(cmd)
  (interactive "sSend Command: ")
  (comint-goto-process-mark)
  (when (> (point-max) (point))
    (kill-line))
  (insert cmd)
  (comint-send-input))

;;; html
; 16.09.2003
(defun xsteve-html-dwim ()
  (interactive)
  (cond ((string= "tmpl" (file-name-extension (buffer-file-name)))
         (compile "scons -u"))
        (t
         (browse-url-of-file))))

(defun xsteve-html-redisplay ()
  "Just reload the page displayed in the *w3m* buffer.
This function does not ensure, that the *w3m* buffer is correctly displayed."
  (interactive)
  (save-window-excursion
    (set-buffer "*w3m*")
    (w3m-reload-this-page))
  (message "*w3m* page reloaded"))


;;;python
(defvar py-execute-cmd nil "Python command to execute.
If this is nil: just send the current buffer to the python interpreter.
For wxPython applications on windows: set py-execute-cmd to \"start hello_world.py\"")
(defvar py-update-app-via-sww-params nil)
(defvar xsteve-py-execute-python-buffer nil)
;(defvar xsteve-py-execute-startup-delay 0.5)
;(defvar xsteve-py-execute-startup-delay-on-restart 1.0)

;; When using ipython's run functions the delays are no longer needed ;-)
(defvar xsteve-py-execute-startup-delay 0.0)
(defvar xsteve-py-execute-startup-delay-on-restart 0.0)
;;03.08.2000
(defun xsteve-py-execute-buffer(arg)
  "Start a python shell and evaluate this buffer.
If called with the prefix argument -1, kill a running python process first.
When the prefix argument is 0: Only switch to the buffer, run no python code."
  (interactive "p")
  (if py-execute-cmd
      (shell-command-to-string py-execute-cmd)
    (if py-update-app-via-sww-params
        (apply 'sww-type-keys py-update-app-via-sww-params)
      ;; kill the *Python* process buffer, when called with the prefix argument -1
      (let ((buffer (current-buffer))
            (py-proc (get-process py-which-bufname))
            (cmd))
        (when (and (= arg -1) py-proc)
          (kill-process py-proc)
          (sit-for xsteve-py-execute-startup-delay-on-restart))
        (py-shell)
        (unless (= arg 0)
          (sleep-for xsteve-py-execute-startup-delay))
        (setq xsteve-py-execute-python-buffer buffer)
        (set-buffer buffer)
        (unless (= arg 0)
          (setq cmd (concat "%run " (buffer-file-name buffer)))
          (message "Executing ipython function %s" cmd)
          (comint-simple-send (get-process py-which-bufname) cmd))))))
          ;;(py-execute-buffer)))))) ;; that function is probably needed on windows

(defun xsteve-py-kill-python-process ()
  (interactive)
  (let ((py-proc (get-process py-which-bufname)))
    (when py-proc
      (kill-process py-proc)
      (message "Killed process %s" py-proc))))

(defun xsteve-py-pop-to-source-buffer ()
  (interactive)
  (when xsteve-py-execute-python-buffer
    (let ((flip (eq 0 (nth 1 (window-edges (selected-window))))))
      (pop-to-buffer xsteve-py-execute-python-buffer)
      (when flip (xsteve-flip-windows)))))

;25.10.2000
(defun xsteve-py-up-arrow (arg)
  (interactive "p")
  (if (and (get-buffer-process (current-buffer)) (comint-after-pmark-p))
      (comint-previous-matching-input-from-input arg)
    (previous-line arg))
  (setq this-command 'comint-previous-matching-input-from-input))

;25.10.2000
(defun xsteve-py-down-arrow (arg)
  (interactive "p")
  (if (and (get-buffer-process (current-buffer)) (comint-after-pmark-p))
      (comint-next-matching-input-from-input arg)
    (next-line arg))
  (setq this-command 'comint-next-matching-input-from-input))

;;26.09.2006
(defun xsteve-python-insert-debug-print-statement ()
  "Insert a debug print statement for the variable at point or for an actually marked expression"
  (interactive)
  (let ((sy (xsteve-get-symbol-at-point "print: " current-prefix-arg t)))
    (save-excursion
      (beginning-of-line)
      (indent-for-tab-command)
      (insert (format "print \"%s: \", %s" sy sy))
      (newline-and-indent))))

;;;eval a specific elisp file for a loaded file
;; 20.06.2000
;; put the following to your local variable list:
;; eval-elisp-file: "hurz.el"
(defvar eval-elisp-file nil "Emacs lisp file, that should be loaded after loading this file.
This variable is buffer local.")
(make-variable-buffer-local 'eval-elisp-file)
(defun xsteve-load-dedicated-elisp()
  (interactive)
  (when eval-elisp-file
    (load (concat (file-name-directory (buffer-file-name)) eval-elisp-file) t t)))
;install xsteve-load-dedicated-elisp as hook function
(add-hook 'hack-local-variables-hook 'xsteve-load-dedicated-elisp)


;;;command to start automatic documentation generation
;;21.06.2000
(defvar generate-doc-command "doc.bat" "Shell command to start the automatic documentation generation.")
(make-variable-buffer-local 'generate-doc-command)
(defun xsteve-generate-documentation ()
  "Start the automatic documentation generation"
  (interactive)
  (message "Running %s ..." generate-doc-command)
  (shell-command-to-string generate-doc-command)
  (message "Running %s ... finished." generate-doc-command))

;;;command to start automatic TAGS generation
;;21.06.2000
(defvar generate-tags-command "create_tags.bat" "Shell command to regenerate the TAGS file.")
(make-variable-buffer-local 'generate-tags-command)
(defun xsteve-recreate-tags ()
  "Recreate the TAGS file"
  (interactive)
  (message "%s => %s" generate-tags-command (shell-command-to-string generate-tags-command)))


;;; emacs lisp stuff
(defun balance-defuns (buffname)
  "Check that every defun in BUFF is balanced (current-buffer if interactive)."
  (interactive "bBuffer to balance: ")
  (let ((buff (get-buffer buffname)))
    (set-buffer buff)
    (let ((next-end (point-min)))
      (condition-case ddd
          (progn
            (while (setq next-end (scan-lists next-end 1 0)))
            (if (interactive-p)
                (message "All defuns balanced.")
              t))
        (error
         (push-mark nil t)
         (goto-char next-end)
         (re-search-forward "\\s(\\|\\s)")
         (backward-char 1)
         (cond ((interactive-p)
                (ding)
                (message "Unbalanced defun."))
               (t nil)))))))

;; 10.09.2004
(defun xsteve-trace-function (arg)
  (interactive "p")
   (let* ((untracing (< arg 0))
          (function (intern (completing-read (if untracing "Untrace function: " "Trace function: ")
                                             obarray 'fboundp t (symbol-name (function-at-point))))))
     (cond ((eq current-prefix-arg nil)
            (message "tracing %S in background" function)
            (trace-function-background function))
           ((> arg 0)
            (message "tracing %S" function)
            (trace-function function))
           (untracing
            (message "untracing %S" function)
            (untrace-function function)))))


;;;VHDL stuff
;06.04.2000
(defun vhdl-beautify-this-block()
  "Beautify a block. A block is deliminited with an empty line.
If the region is active beautify the marked region. The beautification
is done via vhdl-beautify-region."
  (interactive)
  (save-excursion
    (let ((block-sep "\n[ \t]*\n")
          (beg)
          (end))
      (if mark-active
          (progn
            (setq beg (region-beginning))
            (setq end (region-end)))
        (re-search-backward block-sep (point-min))
        (next-line 2)
        (beginning-of-line)
        (setq beg (point))
        (re-search-forward block-sep (point-max))
        (next-line -1)
        (setq end (point)))
      (vhdl-beautify-region beg end))))


(defun vhdl-finish-statement ()
  (interactive)
  (let ((indent (- (current-indentation) 2))
        (pos (point))
        (insert-string))
    (save-excursion
      (beginning-of-line -0)
      (while (and (not (bobp))
                  (or (looking-at "^\\s-*\\(--.*\\)?$")
                      (> (current-indentation) indent)))
        (beginning-of-line -0))
      (if (= (current-indentation) indent)
          (progn
            (back-to-indentation)
            (cond ((looking-at "\\(if\\|else\\|elsif\\) ")
                   (message "looking-at if")
                   (setq insert-string "end if;"))))))
    (when insert-string
      (back-to-indentation)
      (insert insert-string)
      (indent-according-to-mode)
      (insert "\n")
      (indent-according-to-mode))))

;;(defvar vhdl-actual-port-name '(".*" . "\\&_i"))
(defun vhdl-set-actual-port-name-postfix (postfix)
  (interactive "sPortname Postfix: ")
  (setq vhdl-actual-port-name (cons ".*" (concat "\\&" postfix))))

;(define-key vhdl-mode-map "\C-c]" 'vhdl-finish-statement)

(defvar compilation-file-regexp-alist nil) ;avoid a waring on the first call to vhdl-save-and-compile
(defvar vhdl-sim-directory "./" "vhdl simulation directory")
(defvar vhdl-compiler-options "-93 -quiet" "vhdl compiler options")
(defun vhdl-save-and-compile ()
  "Saves the buffer and starts the modelsim compiler vcom.
If the file vhdl-project.el exists this is evalueated first.
If C-u is given as prefix argument: the command line can be edited."
  (interactive)
  (vhdl-compile-init)
  (save-buffer)
  (when (file-exists-p "vhdl-project.el")
    (load (expand-file-name "vhdl-project.el")))
  (let* ((options vhdl-compiler-options)
        (default-directory (expand-file-name vhdl-sim-directory))
        (file-name buffer-file-name)
        (command "vcom")
        (before-compile-cmd (concat "cd " default-directory " && "))
        (cmd-line (concat before-compile-cmd command " " options " \"" file-name "\"")))
    (when (equal current-prefix-arg '(4))
      (setq cmd-line (read-string "Compile cmd: " cmd-line)))
    (compile cmd-line)))

;-- vhdl-sim-directory: "../../sim/"
;-- vhdl-compiler-options: "-93 -quiet"

(defun vhdl-activate-new-font-lock-settings ()
  (interactive)
  ;(normal-mode)
  (hack-local-variables)
  (vhdl-font-lock-init)
  (vhdl-fontify-buffer))
;-- vhdl-special-syntax-alist: (("type" "T_\\w+" "ForestGreen" "ForestGreen") ("constant" "C_\\w+" "DarkGoldenrod" "DarkGoldenrod"))

;28.09.2001
(defun xsteve-vhdl-electric-return ()
  "newline-and-indent or indent-new-comment-line if in comment and preceding
character is a space."
  (interactive)
  (if (and (= (preceding-char) ? ) (vhdl-in-comment-p))
      (indent-new-comment-line)
    (when (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
      (vhdl-fix-case-word -1))
    (reindent-then-newline-and-indent)))


(defvar vhdl-modelsim-server nil "modelsim socket connection")
(defun vhdl-modelsim-server-connect()
  (interactive)
  (unless vhdl-modelsim-server
    (setq vhdl-modelsim-server (open-network-stream "modelsim" "*modelsim*" "localhost" 9005))
    (message "modelsim-server connected")))

(defun vhdl-modelsim-server-disconnect()
  (interactive)
  (when vhdl-modelsim-server
    (delete-process vhdl-modelsim-server)
    (setq vhdl-modelsim-server nil)
    (message "modelsim-server disconnected")))

(defun vhdl-modelsim-server-reconnect()
  (interactive)
  (vhdl-modelsim-server-disconnect)
  (vhdl-modelsim-server-connect))

(defvar vhdl-modelsim-point nil)
(defun vhdl-modelsim-server-send(cmd)
  (interactive "sModelsim cmd: ")
  (unless vhdl-modelsim-server
    (if (y-or-n-p "Modelsim Connection not established. Open a connection? ")
        (vhdl-modelsim-server-connect)))
  (when vhdl-modelsim-server
    (condition-case nil
        (save-window-excursion
          (switch-to-buffer "*modelsim*")
          (goto-char (point-max))
          (insert ">> " cmd "\n")
          (setq vhdl-modelsim-point (point-max))
          (set-marker (process-mark (get-buffer-process (current-buffer))) (point-max))
          (process-send-string "modelsim" (concat cmd "\n")))
      (error (progn
               (setq vhdl-modelsim-server nil))))))

(defun vhdl-modelsim-server-getresponse(&optional delay)
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*modelsim*")
    (sit-for (or delay 0.4))
    (let ((response (buffer-substring-no-properties vhdl-modelsim-point (point-max))))
      (setq response (dired-string-replace-match "\n" response "" nil t))
      response)))


(defun vhdl-modelsim-server-activate-and-send(cmd)
  (interactive "sModelsim cmd: ")
  (when win32p
    (sww "ModelSim")
    (sww "fastcmd"))
  (vhdl-modelsim-server-send cmd))

;13.04.2000 - show connection status with ModelSim
(if (not (assq 'vhdl-modelsim-server minor-mode-alist))
    (setq minor-mode-alist
          (cons '(vhdl-modelsim-server " ModelSim")
                minor-mode-alist)))


;(vhdl-modelsim-server-activate-and-send "sim_syncwrapper; run 8000")
(defvar vhdl-modelsim-cmd "" "modelsim command to use for simulation")
(defvar vhdl-modelsim-cmd-history nil "modelsim command history")
(defvar vhdl-modelsim-last-cmd nil "the last command sent to modelsim")

;(setq vhdl-modelsim-cmd "sim_irda_test1; run 8000")
;(setq vhdl-modelsim-cmd '(("sim hac". "vsim hac; run 100") ("sim rtl". "vsim rtl; run 100")))
;(setq vhdl-modelsim-cmd '("vsim hac; run 100" "vsim rtl; run 100"))

(defun vhdl-modelsim-server-send-fast-command(force-ask)
  "Send `vhdl-modelsim-cmd' to the vhdl simulator.
When called with a prefix argument, ask the user, which command to execute,
if `vhdl-modelsim-cmd' is a list."
  (interactive "P")
  (when (file-exists-p "vhdl-project.el")
    (load (expand-file-name "vhdl-project.el")))
  (let ((cmd (if (listp vhdl-modelsim-cmd)
                 (if (or force-ask (not vhdl-modelsim-last-cmd))
                     (ido-completing-read "Select Modelsim command: "  vhdl-modelsim-cmd nil nil
                                          (car vhdl-modelsim-cmd-history) 'vhdl-modelsim-cmd-history)
                   vhdl-modelsim-last-cmd)
               vhdl-modelsim-cmd)))
    (when (listp vhdl-modelsim-cmd)
      (setq cmd (or (cdr (assoc cmd vhdl-modelsim-cmd)) cmd)))
    (setq vhdl-modelsim-last-cmd cmd)
    (message "sending vsim command: '%s'" cmd)
    (vhdl-modelsim-server-activate-and-send cmd)))


;;C stuff
;24.10.2000
(defun c++-convert-to-method-body ()
  "Take a function prototype from the class definition and convert it
to the implementation body"
  (interactive)
  (let ((class-name)
        (doit))
    (save-excursion
      (back-to-indentation)
      (setq doit (looking-at ".+(.*); *$")))
    (if doit
        (progn
          (save-excursion
            (re-search-backward "^[^ \t].+\\(\\<\\w+\\>*::\\)")
            (setq class-name (match-string-no-properties 1)))
          (back-to-indentation)
          (when (looking-at "virtual")
            (message class-name)
            (delete-region (match-beginning 0) (match-end 0)))
          (beginning-of-line)
          (re-search-forward "(")
          (re-search-backward "[ \t]")
          ;;(forward-char 1)
          (delete-horizontal-space)
          (insert " ")
          (insert class-name)
          (end-of-line)
          (delete-region (point) (- (point) 1))
          (indent-according-to-mode)
          (insert " {\n\n}\n")
          (next-line 1))
      (message "This line does not contain a valid method declaration"))))

;14.11.2000
(defun xsteve-c-indent-command (&optional whole-exp)
  (interactive "P")
  (let ((indent t))
    (when (eq 'c (caar (c-guess-basic-syntax)))
      (message "In Comment")
      (save-excursion
        (beginning-of-line-text 0)
        (when (looking-at "@param")
          (message "@param - do not indent")
          (setq indent nil))))
    (when indent
      (c-indent-command whole-exp))))

;; LaTeX stuff
;18.07.2001
(defun latex-insert-unit-command (value)
  "Ask the user for a unit and insert a properly formated unit command for latex"
  (interactive "sEnter a unit (e.g. 2.3kHz): ")
  (save-match-data
    (if (string-match "\\([0-9\.]+\\)\\(.+\\)" value)
        (insert (format "\\unit[%s]{%s} " (match-string 1 value) (match-string 2 value)))
      (message "not a valid unit"))))

;; 09.12.2002, from Jesper Harder
(defun acrobat-close-all-docs ()
  "Close all open documents in Acrobat."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create " *ddeclient*"))
    (erase-buffer)
    (insert "[CloseAllDocs()]")
    (call-process-region (point-min) (point-max)
 "ddeclient" t t nil "acroview" "control")
    (if (= 0 (string-to-int (buffer-string))) t nil)))


(defun write-file-no-select ()
  "`write-file' without changing to new filename.
Useful for making a copy of working file in development."
  (interactive)
  (let ((old-filename (buffer-file-name)))
    (call-interactively 'write-file)
    (write-file old-filename)))
(global-set-key [(control x) w] 'write-file-no-select)
(define-key ctl-x-map "w" 'write-file-no-select)

(defun xsteve-ps-print-buffer (nup)
  "Print the current buffer.
- With no prefix argument, ask the user about the number of rows per page (nup).
- With a negative prefix argument: use the absolute value of the prefix argument for
  the ps-font-size, ask the user about the number of rows per page.
- With a positive prefix argument (>0) use the prefix argument for nup
- With a prefix argument = 0: calculate the best font size to avoid line splitting
  in the printed page"
  (interactive "P")
  (let* ((ps-font-size (if (eq nup 0)
                           (progn
                             (setq nup nil)
                             (calc-best-font-size (xsteve-ps-print-get-line-length)))
                         (if (and nup (< nup 0))
                             (- nup)
                           ps-font-size)))
         (nup (or (when (and nup (> nup 0)) nup) (string-to-number (read-string "N-up printing: " "1"))))
         (ps-spool-tumble (/= (or nup 1) 1))
         (file "~/emacs_preview.ps")
         (buf-name (buffer-name)))
    (message "ps-spool-tumble: %s nup: %s ps-font-size: %d" ps-spool-tumble nup ps-font-size)
    (when (eq major-mode 'w3m-mode)
      (rename-buffer w3m-current-title t))
    (pr-ps-buffer-ps-print nup file)
    (when (eq major-mode 'w3m-mode)
      (rename-buffer buf-name nil)))
  (xsteve-ps-print-buffer-view))


(defun xsteve-ps-print-region ()
  (interactive)
  ;;(ps-print-region-with-faces (region-beginning) (region-end) "~/emacs_preview.ps")
  (let ((nup 1))
    (pr-ps-region-print nup "~/emacs_preview.ps")
    (xsteve-ps-print-buffer-view)))

(defun xsteve-ps-print-buffer-view ()
  (interactive)
  (if win32p
      (sww-with-app (concat "emacs_preview" (expand-file-name "~/emacs_preview.ps"))
                    (concat ghostview-executable " " (expand-file-name "~/emacs_preview.ps")))
    (shell-command (concat ghostview-executable " " (expand-file-name "~/emacs_preview.ps")))))

(defvar ps-print-line-length nil "Line length for ps printing")
;(make-variable-buffer-local 'ps-print-line-length)

;24.08.2001
(defun xsteve-ps-print-get-line-length ()
  (or ps-print-line-length (max-line-length)))

;08.03.2000
(defvar a2ps-switches "" "switches passed to a2ps")
(defun xsteve-a2ps-print-buffer ()
  "Print the current file using a2ps"
  (interactive)
  (let* ((file-name (buffer-file-name))
         (a2ps-cmd "c:\\utils\\4\\4nt -c a2ps.bat")
         (result (shell-command-to-string (concat a2ps-cmd " " a2ps-switches " " file-name)))
         (msg ""))
    (string-match "\\([0-9]+\\) pages on \\([0-9]+\\)" result)
    (setq msg (concat "a2ps: " (match-string 1 result) " pages on " (match-string 2 result) " sheets"))
    (when (string-match "\\[\\([0-9]+\\) line" result)
      (setq msg (concat msg ", " (match-string 1 result) " line(s) wrapped")))
    ;(message result)
    (message msg)))
;[Total: 3 pages on 2 sheets] saved into the file `c:\temp\a2ps_result.ps'
;[4 lines wrapped]

;; 26.01.2001
(defun max-line-length ()
  "Return the max line length in the current buffer"
  (let ((max-len 0))
    (save-excursion
      (goto-char (point-min))
      (while (eq (forward-line) 0)
        (end-of-line)
        (when (> (current-column) max-len)
          (setq max-len (current-column))))
      max-len)))

(defun calc-best-font-size (line-length)
  (save-excursion
    (let ((max-len (+ 1 line-length))
          (b1)
          (font-size))
      (ps-line-lengths)
      (set-buffer "*Line-lengths*")
      (goto-char (point-min))
      (re-search-forward "nb char per line / font size")
      (re-search-forward (concat (number-to-string max-len) " +"))
      (setq b1 (point))
      (end-of-line)
      (setq font-size (string-to-number (buffer-substring-no-properties b1 (point))))
      (message "max len= %d [best: %f] (ps-font-size: %f)" max-len font-size ps-font-size)
      (kill-buffer "*Line-lengths*")
      font-size)))

;(ps-line-lengths)
;(setq ps-font-size   '(7 . 8.5))
;(setq ps-font-size   '(5 . 6.5))
;(setq ps-font-size   '(14 . 6.5))
;(setq ps-font-size   14)
;(ps-nb-pages)


;; 16.11.2006: Org-mode helpers
(defun xsteve-org-fixup-ascii-export ()
  (interactive)
  (let ((search-string)
        (string-len)
        (num-levels 3))
    (dotimes (num num-levels)
      (setq string-len (+ (* (- num-levels num) 2) 1))
      (setq search-string (concat "^" (regexp-quote (make-string string-len ?*))))
      (setq replace-string (concat (make-string (- string-len 1) ? ) "*"))
      (goto-char (point-min))
      ;;(message "search-string '%s' -> '%s'" search-string replace-string)
      (while (re-search-forward search-string nil t)
        ;;(message "match: %s" (match-string 0))
        (replace-match replace-string)))))

(defun xsteve-org-print-ascii ()
  "Print the current org buffer as ascii text"
  (interactive)
  (xsteve-org-fixup-ascii-export)
  (save-buffer)
  (xsteve-ps-print-buffer 1)
  (undo)
  (save-buffer))

;;;dedicated mode 13.04.2000
(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'dedicated-mode)

(defun dedicated-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq dedicated-mode (not dedicated-mode))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))


;  "define [(super control kp-0...kp-9)] with quickmark set,
;[(super kp-0...kp-9)] with jump to quickmark."

;;;Quickmarks
(defun xsteve-define-quickmark-keys ()
  (interactive)
  (let ((simple-key)
        (key-nr 0))
    (while (<= key-nr 9)
      (setq simple-key (make-symbol (concat "kp-" (number-to-string key-nr))))
      (global-set-key (vector (list 'super 'control simple-key))
                      (` (lambda()
                           (interactive)
                           (point-to-register (, key-nr))
                           (message (concat "quickmark " (symbol-name '(, simple-key)) " defined")))))
      (global-set-key (vector (list 'super simple-key))
                      (` (lambda()
                           (interactive)
                           (jump-to-register (, key-nr))
                           (message (concat "jumped to quickmark " (symbol-name '(, simple-key)))))))
      (setq key-nr (1+ key-nr)))))

(defun xsteve-apply-initial-frame-alist ()
  (interactive)
  (modify-frame-parameters (selected-frame) initial-frame-alist))

;; 27.01.2003
(defun xsteve-apply-ecb-frame-alist ()
  (interactive)
  (modify-frame-parameters (selected-frame) ecb-frame-alist))

;; 27.01.2003
(defun xsteve-ecb-toggle ()
  "Toggle the visibility of the ecb windows.
If ecb is not active start ecb with ecb-activate
If this function is called with a prefix argument: call ecb-deactivate"
  (interactive)
  (if current-prefix-arg
      (ecb-deactivate)
    (if ecb-activated-window-configuration
        (progn
          ;;(if (not ecb-windows-hidden)
          ;;    (xsteve-apply-initial-frame-alist)
          ;;  (xsteve-apply-ecb-frame-alist))
          (ecb-toggle-ecb-windows))
      (ecb-activate))))


;12.04.2000
(defun indirect-elisp-buffer (start end)
  "Edit region in an indirect emacs-lisp mode buffer."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect lisp statement*")))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (narrow-to-region start end)
    (emacs-lisp-mode)))

;05.10.2000
(defun toggle-debug-on-error ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (message "debug-on-error is now %s" debug-on-error))

;;;W3 etc.
(defvar w3-fix-frame-height (frame-height))
(defun fix-w3-fonts nil
  "Restore faces created by W3 so that they're all the same size.
This assumes that the standard faces `default', `italic', `bold',
and `bold-italic' are sensibly defined."
  (interactive)
  (mapatoms
   (lambda (x)
     (and (string-match "^w3-style-face" (symbol-name x))
          (facep x)
          (set-face-font x
                         (let ((font (face-font x)))
                           (and font
                                (face-font
                                 (if (string-match "-bold-" font)
                                     (if (string-match "-bold-[io]-" font)
                                         'bold-italic
                                       'bold)
                                   (if (string-match "-normal-[io]-" font)
                                       'italic
                                     'default)))))))))
  (set-frame-height (selected-frame) w3-fix-frame-height))

;;(defun w3-open-this-buffer ()
;;  "Show this buffer in w3"
;;  (interactive)
;;  (w3-open-local (buffer-file-name)))

(defun w3m-open-this-buffer ()
  "Show this buffer in w3m"
  (interactive)
  (w3m-find-file (buffer-file-name)))

;;(defun webjump-w3 ()
;;  (interactive)
;;  (let ((browse-url-browser-function 'browse-url-w3))
;;    (webjump)))

(defun webjump-w3m ()
  (interactive)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (webjump)))

(defun w3m-copy-link ()
  "Copy anchor's target to the kill-ring."
  (interactive)
  (kill-new (w3m-anchor))
  (message "Copied %s to the killring" (w3m-anchor)))

(defun xsteve-w3m-dwim ()
  "Open w3m or bury it.
When in a html-file, display that file in w3m."
  (interactive)
  (if (eq major-mode 'w3m-mode)
      (bury-buffer)
    (if (eq major-mode 'html-helper-mode)
        (w3m-open-this-buffer)
      (w3m))))

;; bbdb stuff
(defun bbdb-mail-with-default-mailer (&optional bbdb-record)
  (interactive (list (if (bbdb-do-all-records-p)
                         (mapcar 'car bbdb-records)
                       (bbdb-current-record))))
  ;(unless bbdb-record (setq bbdb-record (bbdb-current-record)))
  (let ((net-address))
    (if (not (consp bbdb-record))
        (progn
          (message "sending to one recipant")
          (setq net-address (car (bbdb-record-net bbdb-record))))
      (message "sending to more than one recipent")
      (let ((b bbdb-record))
        (while b
          (message "%S" (car (bbdb-record-net (car b))))
          (setq net-address (concat net-address ", " (car (bbdb-record-net (car b)))))
          (setq b (cdr b)))
        (setq net-address (substring net-address 2))))
    (if net-address
        (progn
          (unless (consp bbdb-record)
            (x-select-text
             (format "%s\n\n\nmfg\n  %s."
                     (bbdb-mail-salutation bbdb-record)
                     user-full-name)))
          (browse-url (concat "mailto:" net-address)))
      (message "Record does not have a net address"))))


(defun bbdb-and-mail-with-default-mailer()
  (interactive)
  (bbdb-mail-with-default-mailer (bbdb-completing-read-one-record "email address: ")))

;; 09.01.2003
(defun bbdb-mail-salutation (bbdb-record)
  (or (bbdb-record-getprop bbdb-record 'anrede)
      (format "Sehr geehrter Herr %s," (bbdb-record-lastname bbdb-record))))

;27.02.2002
(defun bbdb-getcategories ()
  "get the available categories in the bbdb"
  (let ((records (bbdb-records))
        (cs nil)
        (cl nil))
    (while records
      (setq cs (bbdb-record-getprop (car records) 'category))
      (when cs
        (setq cs (split-string cs " "))
        (message "%S" cs)
        (while cs
          (add-to-list 'cl (car cs))
          (setq cs (cdr cs))))
      (setq records (cdr records)))
    cl))

;27.02.2002
(defun bbdb-category ()
  (interactive)
  (bbdb-notes "category" (completing-read "list category: " (mapcar 'list (bbdb-getcategories)) nil t) nil))

;; 03.02.2005
(defun xsteve-bbdb-copy-mail-address ()
  (interactive)
  (let ((adr (bbdb-dwim-net-address (bbdb-current-record))))
    (kill-new adr)
    (message "Copied '%s'" adr)))

;; From Thomas Gerds
(defun bbdb-switch-to-other-bbdb-file (&optional db dont-ask)
  (interactive)
  (bbdb-save-db)
  (unless db
    (setq db (if dont-ask (expand-file-name "~/.bbdb")
 	       (read-file-name "Use bbdb database "))))
  (setq bbdb-file db
 	bbdb-buffer (get-file-buffer db)))

;;;misc
;;dired
(defun dired-view-file-w3 ()
  "In dired, view a file in w3 mode, returning to dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
    (or (dired-goto-subdir file)
        (dired file))
      (w3-open-local file))))
(define-key dired-mode-map "W" 'dired-view-file-w3)

(defun xsteve-dired-launch-file ()
  "In dired, launch the file or directory named on this line."
  (interactive)
  (let* ((f-name (dired-get-filename))
         (ext (file-name-extension f-name))
         (command))
    (cond ((and (>= (length ext) 3) (string= (substring ext 0 3) "htm"))
           (if current-prefix-arg
               (browse-url f-name)
             (w3m-find-file f-name)))
          ((fboundp 'w32-shell-execute)
           (w32-shell-execute "open" f-name))
          (t
           (setq command (dired-read-shell-command (concat "& on " "%s: ") nil (list (dired-get-filename))))
           (call-process command nil 0 nil (dired-get-filename))))))
           ;;(shell-command (concat "\"" (dired-get-filename) "\" &"))))
           ;;(call-interactively 'dired-do-shell-command)))
(define-key dired-mode-map "l" 'xsteve-dired-launch-file)

;15.09.2000
(defun dired-find-alternate-dir ()
  (interactive)
  (let ((file (dired-get-filename))
        (buffer))
    (if (file-directory-p file)
        (progn
          (message "directory")
          (setq buffer (current-buffer))
          (dired-find-file)
          (kill-buffer buffer))
      (dired-find-file))))
;(define-key dired-mode-map "a" 'dired-find-alternate-dir)
(define-key dired-mode-map  "\C-m" 'dired-find-alternate-dir)

;15.09.2000
(defun dired-start-explorer ()
  (interactive)
  (let ((directory-sep-char ?\\))
    (shell-command-to-string (concat "explorer " (dired-string-replace-match "/" (dired-current-directory) "\\\\" nil t)))))
(define-key dired-mode-map "E" 'dired-start-explorer)

;15.09.2000
(defun dired-up-directory-this-buffer ()
  (interactive)
  (let ((buffer))
    (setq buffer (current-buffer))
    (dired-up-directory)
    (kill-buffer buffer)))
(define-key dired-mode-map "\177" 'dired-up-directory-this-buffer)

;28.03.2001
(defun dired-insert-dirs-recursive (dirname)
  (interactive
   (list (dired-get-filename)))
  (dired-maybe-insert-subdir dirname "-laR"))
(define-key dired-mode-map [(meta i)] 'dired-insert-dirs-recursive)

(defun dired-add-extension-to-cvs-wrappers ()
  (interactive)
  (let ((ext (file-name-extension (dired-get-filename))))
    (find-file ".cvswrappers")
    (goto-char (point-min))
    (unless (search-forward (concat "*." ext) nil t)
      (goto-char (point-max))
      (when (> (current-column) 0) (newline))
      (insert (concat "*." ext " -k 'b'"))
      (save-buffer))))
(define-key dired-mode-map [(b)] 'dired-add-extension-to-cvs-wrappers)

;! rar a -idp f1.rar *
(defun dired-rar-add-files (archive)
  "Runs rar a -idp -ep1 *"
  (interactive "sAdd marked files to archive: ")
  (message "Adding marked files to %s" archive)
  (dired-do-shell-command (format "rar a -idp -ep1 %s *" archive) nil (dired-get-marked-files))
  (revert-buffer))

(defvar xsteve-tramp-hosts nil)
(defun xsteve-tramp ()
  "Open my home directory on an other host.
A list of possible hosts can be given by `xsteve-tramp-hosts'"
  (interactive)
  (let ((host (ido-completing-read "Host: " xsteve-tramp-hosts))
        (user-name (user-login-name)))
    (find-file (concat "/ssh:" user-name "@" host ":~/"))))

; 09.10.2006
(defun xsteve-find-file-as-root ()
  "Open the current open file via tramp and the su:// protocol"
  (interactive)
  (find-file (concat "/su::" (buffer-file-name))))

; 26.09.2005
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

;;; edit-variable from John Wiegley
(defvar edit-variable-buffer)
(defvar edit-variable-symbol)
(make-variable-buffer-local 'edit-variable-buffer)
(make-variable-buffer-local 'edit-variable-symbol)

(defun edit-variable (variable)
  "Edit the value of VARIABLE."
  (interactive (list (completing-read "Name: " obarray 'boundp)))
  (let* ((symbol (intern variable))
	 (value (symbol-value symbol))
	 (buffer (current-buffer)))
    (with-current-buffer (get-buffer-create (format "*var %s*" variable))
      (erase-buffer)
      (emacs-lisp-mode)
      (setq edit-variable-buffer buffer
	    edit-variable-symbol symbol)
      (insert (pp-to-string value))
      (goto-char (point-min))
      (window-configuration-to-register ?V)
      (select-window (display-buffer (current-buffer)))
      (define-key (current-local-map) [(control ?c) (control ?c)]
	(function
	 (lambda ()
	   (interactive)
	   (goto-char (point-min))
	   (let ((symbol edit-variable-symbol)
		 (value (read (current-buffer))))
	     (with-current-buffer edit-variable-buffer
	       (set symbol value)))
	   (jump-to-register ?V)))))))



;;cvs stuff
(defun cvs-mode-idiff-other-frame ()
  "Run cvs-mode-idiff in a new frame"
  (interactive)
  (let ((frame (make-frame)))
    (select-frame frame)
    (cvs-mode-idiff)))

(defun cvs-mode-idiff-previous-version ()
  (interactive)
  (let* ((fi (cvs-mode-marked nil nil :one t))
         (file (cvs-fileinfo->full-path fi))
         (version-nr (cvs-fileinfo->base-rev fi))
         (major (car (split-string version-nr "\\.")))
         (minor (string-to-number (cadr (split-string version-nr "\\."))))
         (last-version-nr (concat major "." (number-to-string (- minor 1))))
         (rev1-buf (cvs-retrieve-revision fi last-version-nr))
         (rev2-buf );(if rev2 (cvs-retrieve-revision fi rev2)))
         ;; this binding is used by cvs-ediff-startup-hook
         (cvs-transient-buffers (list rev1-buf rev2-buf)))
    (funcall (car cvs-idiff-imerge-handlers)
             rev1-buf (or rev2-buf (find-file-noselect file)))))

;;(add-hook 'pcl-cvs-load-hook
(add-hook 'cvs-mode-hook
          '(lambda ()
             (define-key cvs-mode-diff-map "f" 'cvs-mode-idiff-other-frame)
             (define-key cvs-mode-diff-map "p" 'cvs-mode-idiff-previous-version)))


(defun cvs-close-cvs-buffers ()
  "Close all open cvs buffers"
  (interactive)
  (mapcar '(lambda (buf)
             (when (string-match "\\*cvs" (buffer-name buf))
               (kill-buffer buf)))
          (buffer-list)))

;18.09.2001
(defun cvs-mode-diff-other-window ()
  (interactive)
  (let ((buffer (current-buffer)))
    (cvs-mode-diff)
    (switch-to-buffer buffer)
    (pop-to-buffer "*cvs-diff*")
    (pop-to-buffer buffer)))

(defun cvs-mode-pop-to-cvs-buffer ()
  (interactive)
  (pop-to-buffer "*cvs*"))

;25.10.2002
(defun cvs-mode-file-dos2unix ()
  (interactive)
  (let ((f-name (expand-file-name (cvs-fileinfo->full-path (cvs-mode-marked nil nil :one t)))))
    (message "dos2unix %s" f-name)
    (shell-command-to-string (format "dos2unix %s" f-name))
    (message "dos2unix %s ... done" f-name)))

;changed by XSteve
(defun vc-version-other-window (rev)
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (vc-ensure-vc-buffer)
  (let* ((version (if (string-equal rev "")
              (vc-latest-version buffer-file-name)
            rev))
     (filename (concat (file-name-sans-extension buffer-file-name)
                       "-" version "."
                       (file-name-extension buffer-file-name))))
    (or (file-exists-p filename)
    (vc-backend-checkout buffer-file-name nil version filename))
    (find-file-other-window filename)))


; 08.03.2001
;; (defun xsteve-mark-cvs-modified ()
;;   "Called when a file has changed. Updates any RCS Id and Header keywords it
;; finds to show that the file is modified."
;;   (let ((buffer-undo-list t))
;;     ; don't let this change get into the undo list
;;     ; because of this, we must ensure that the edit is in-place, and doesn't
;;     ; move any text.
;;     (when (and (buffer-modified-p) (boundp 'vc-mode) vc-mode)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward
;;                 (concat "\\(\\$\\(?:Id\\|Header\\): "
;;                         "[^\"'#;$]* \\)\\(Exp \\$\\)")
;;               nil t)
;;           (replace-match "\\1Mod $" t))))))
;;
;; (defadvice basic-save-buffer (before xsteve-basic-save-buffer first activate)
;;   (xsteve-mark-cvs-modified))

;;From: "Sandip Chitale" <sandipchitale@attbi.com>
(defun choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu."
  (let ((item)
        (item-list))
    (while menu-items
      (setq item (car menu-items))
      (if (consp item)
          (setq item-list (cons (cons (car item) (cdr item) ) item-list))
        (setq item-list (cons (cons item item) item-list)))
      (setq menu-items (cdr menu-items)))
    (x-popup-menu t (list menu-title (cons menu-title (nreverse item-list))))))

;; (defun xsteve-popup-commands ()
;;   "Show a popup menu of commands."
;;   (interactive)
;;   (eval-expression (car (read-from-string (choose-from-menu "Commands"
;;      (list
;;       (cons "Goto Line"                            "(call-interactively 'goto-line)")
;; ;     (cons "comand-name1"                         "elisp expression1")
;; ;     (cons "comand-name2"                         "elisp expression2")
;; ;     (cons "comand-name3"                         "elisp expression3")
;;      )))))
;;   )
;; (global-set-key [S-down-mouse-3]                   'xsteve-popup-commands)


;; compilation - 02.04.2001 from Klaus Berndl, klaus.berndl@sdm.de
(defvar compilation-source-overlay (make-overlay 1 1)
  "Internal overlay used for the source-line in the source-buffer")

(defcustom compilation-highlight-source 'secondary-selection
  "*If not nil and a face then highlight the source-line in the source-
buffer with this face."
  :group 'compilation
  :set '(lambda (symbol value)
          (set symbol value)
          (if (and value (facep value))
              (overlay-put compilation-source-overlay 'face value)))
  :type '(radio (const :tag "No highlighting of source-line" :value nil)
                (face :tag "Face for the source-line")))

(defvar compilation-error-overlay (make-overlay 1 1)
  "Internal overlay used for the error-line in the compilation-buffer")

(defcustom compilation-highlight-error 'secondary-selection
  "*If not nil and a face then highlight the error-line in the
compilation-buffer with this face."
  :group 'compilation
  :set '(lambda (symbol value)
          (set symbol value)
          (if (and value (facep value))
              (overlay-put compilation-error-overlay 'face value)))
  :type '(radio (const :tag "No highlighting of error-line" :value nil)
                (face :tag "Face for the error-line")))

(add-hook 'pre-command-hook
          (lambda ()
            (delete-overlay compilation-source-overlay)))

(defadvice compilation-goto-locus (after highlight)
  "If `compilation-highlight-error' is non-nil, highlight the ERROR line.
If `compilation-highlight-source' is non-nil, highlight the SOURCE line."
    (let ((error-marker (ad-get-arg 0))
          (source-marker (ad-get-arg 1)))
    (if compilation-highlight-error
        (save-excursion
          (set-buffer (marker-buffer error-marker))
          (goto-char (marker-position error-marker))
          (move-overlay compilation-error-overlay
                        (line-beginning-position)
                        (line-end-position)
                        (current-buffer))))
    (if compilation-highlight-source
        (save-excursion
          (set-buffer (marker-buffer source-marker))
          (goto-char (marker-position source-marker))
          (move-overlay compilation-source-overlay
                        (line-beginning-position)
                        (line-end-position)
                        (current-buffer))))))

;; this function´s advice only enables the advice of `compilation-goto-locus'
;; temporally so the highlighting is performed. This is necessary because
;; other packages use `compilation-goto-locus' and these packages don´t need
;; the highlight stuff.
(defadvice next-error(around highlight activate)
  "If `compilation-highlight-error' is non-nil, highlight the ERROR line.
If `compilation-highlight-source' is non-nil, highlight the SOURCE line."
  (unwind-protect
      (progn
        (ad-enable-advice 'compilation-goto-locus 'after 'highlight)
        (ad-activate 'compilation-goto-locus)
        ad-do-it)
    (ad-disable-advice 'compilation-goto-locus 'after 'highlight)
    (ad-activate 'compilation-goto-locus)))



;; Semantic
;; 20.09.2004: taken from http://www.emacswiki.org/cgi-bin/wiki/SeanO

(unless (fboundp 'ido-completing-read)
  (defun ido-completing-read (prompt choices)
    (flet ((ido-make-buffer-list (default)
                                 (setq ido-temp-list choices)))
      (ido-read-buffer prompt))))

(defun my-semanticdb-minor-mode-p ()
  "Query if the current buffer has Semanticdb mode enabled."
  (condition-case blah
      (and (semanticdb-minor-mode-p)
           (eq imenu-create-index-function
               'semantic-create-imenu-index))
    (error nil)))

(defun xsteve-jump-to-function ()
  "Jump to a function found by either Semantic or Imenu within the
    current buffer."
  (interactive)
  (cond
   ((my-semanticdb-minor-mode-p) (my-semantic-jump-to-function))
   ((boundp 'imenu-create-index-function) (my-imenu-jump-to-function))))

(defun my-imenu-jump-to-function ()
  "Jump to a function found by Semantic within the current buffer
    with ido-style completion."
  (interactive)
  (save-excursion
    (setq imenu--index-alist (funcall imenu-create-index-function)))
  (let ((thing (assoc
                (ido-completing-read "Go to: "
                                     (mapcar #'car imenu--index-alist))
                imenu--index-alist)))
    (when thing
      (funcall imenu-default-goto-function (car thing) (cdr thing))
      (recenter))))

(defun xsteve-semantic-jump-to-function ()
  "Jump to a function found by Semantic within the current buffer
    with ido-style completion."
  (interactive)
  (let ((tags
         (remove-if
          (lambda (x)
            (or (getf (semantic-tag-attributes x) :prototype-flag)
                (not (member (cadr x) '(function variable type)))))
          (semanticdb-file-stream (buffer-file-name (current-buffer)))))
        (names (make-hash-table :test 'equal)))
    (dolist (tag tags)
      (let ((sn (semantic-tag-name tag)))
        (when (gethash sn names)
          (setq sn
                (loop for i = 1 then (1+ i)
                      for name = (format "%s<%d>" sn i)
                      while (gethash name names)
                      finally return name)))
        (puthash sn tag names)))
    (goto-char (semantic-tag-start
                (gethash
                 (ido-completing-read "Go to: " (ange-ftp-hash-table-keys names))
                 names)))
    (recenter)))

;;etags
(defun xsteve-tags-search ()
  "Run tags-search with thing at point"
  (interactive)
  (tags-search (xsteve-get-symbol-at-point "Search in TAGS: " t)))

(defun xsteve-tags-query-replace ()
  "Run tags-query-replace with thing at point"
  (interactive)
  (let* ((old-string (xsteve-get-symbol-at-point "Tags query replace (regexp): " current-prefix-arg))
         (new-string (read-string (concat "Replace " old-string " with: "))))
    (save-excursion
      (tags-query-replace old-string new-string))))

(defun xsteve-find-next-tag ()
  "Finds the next tag using find-tag with a positive prefix arg"
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'find-tag)))

;ctags -e -R -V --lang=c e:\work\1 *.txt


;23.01.2001
(defun switch-bookmark-file (file)
  (bookmark-load file t)
  (setq bookmark-default-file file))
;(switch-bookmark-file "c:/temp/1.bmk")
;(switch-bookmark-file "~/.emacs.bmk")
(xsteve-define-alternatives 'bmk-file-name '("c:/temp/1.bmk" "~/.emacs.bmk"))

(defun bookmark-toggle-file ()
  (interactive)
  (toggle-bmk-file-name)
  (switch-bookmark-file bmk-file-name))

(defun xsteve-locate (search-string)
"Run locate, but delete meta directories from svn"
  (interactive "sLocate: ")
  (locate search-string)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (delete-matching-lines "\\(\.svn-\\(base\\|work\\)$\\|.arch./..pristine-trees\\|/.arch-ids/.+\.id$\\)"))))

(defun master-search ()
  (interactive)
  (let* ((directory (directory-file-name (read-string "Directory: " default-directory)))
        (files (read-string "Files: " "*.txt"))
        (tags-file-name (expand-file-name (concat (getenv "TEMP") "/replace-tags")))
        (from-string (xsteve-get-symbol-at-point "Tags search (regexp): " t))
        (cmd (concat "ctags -e -R --lang=c -f " tags-file-name " " directory " " files)))
    (shell-command-to-string cmd)
    (tags-search from-string)
    (kill-buffer (get-file-buffer tags-file-name))))


(defun master-replace ()
  (interactive)
  (let* ((directory (directory-file-name (read-string "Directory: " default-directory)))
        (files (read-string "Files: " "*.txt"))
        (tags-file-name (expand-file-name (concat (getenv "TEMP") "/replace-tags")))
        (from-string (xsteve-get-symbol-at-point "Tags query replace (regexp): " t))
        (to-string (read-string (concat "Replace " from-string " with: ")))
        (cmd (concat "ctags -e -R --lang=c -f " tags-file-name " " directory " " files)))
    (shell-command-to-string cmd)
    (tags-query-replace from-string to-string)
    (kill-buffer (get-file-buffer tags-file-name))))

(defun xsteve-save-current-directory ()
  "Save the current directory to the file ~/.emacs.d/current-directory"
  (interactive)
  (let* ((dir default-directory)
         (file-name (shell-quote-argument (expand-file-name dir))))
    (with-current-buffer (find-file-noselect "~/.emacs.d/current-directory")
      (delete-region (point-min) (point-max))
      (insert (concat file-name "\n"))
      (save-buffer)
      (message "Saved directory '%s' to ~/.emacs.d/current-directory" file-name)
      (kill-buffer (current-buffer)))))

;(defun test-propertize () ""
;  (interactive)
;  (message (propertize "hello" 'face '(foreground-color . "blue"))))

;(setq browse-url-browser-function 'browse-url-w3)

;(global-set-key [(control meta o)]   'xsteve-kill-ring-save)

;(sww-with-app "emacs_preview" (concat ghostview-executable " c:\\temp\\emacs_preview.ps"))
;;;some interesting eieio stuff
; (car ede-projects)
; (obj-fields (car ede-projects))
; (obj-fields (car ede-projects))
; (object-print (car ede-projects))
; (slot-value (car ede-projects) 'name)
; (setq ede-projects (list (nth 0 ede-projects) (nth 1 ede-projects)))

;;;Misc
; (defun my-open-file-query-replacer (arg1 arg2)
;   (interactive "sQuery Replace From: \nsQuery Replace To: ")
;   (let ((list-without-nils
;    (lambda (ls)
;      (if (null ls)
;          ls
;        (if (null (car ls))
;        (list-without-nils (cdr ls))
;          (cons (car ls) (list-without-nils (cdr ls))))))))
;     (mapcar
;      (lambda (x)
;        (find-file x)
;        (save-excursion
;    (beginning-of-buffer)
;    (query-replace arg1 arg2)))
;      (list-without-nils
;       (mapcar
;        (lambda (x)
;    (buffer-file-name x))
;        (buffer-list))))))

(provide 'xsteve-functions)


;; Emacs Local Variables
;; Local Variables:
;; a2ps-switches: "-l90"
;; xsteve-parse-align-header: "^[ \t]*;#palign"
;; toc-regexp: ";;; *"
;; End:

;;; xsteve-functions.el ends here

;; arch-tag: 152426f1-c687-4bd3-8633-d18b57fecb06


