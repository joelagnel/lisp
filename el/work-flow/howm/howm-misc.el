;;; howm-misc.el --- Wiki-like note-taking tool
;;; Copyright (c) 2002, 2003, 2004, 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-misc.el,v 1.72 2006/05/01 15:07:05 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;;--------------------------------------------------------------------

(require 'howm-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc.

(defun howm-version ()
  (interactive)
  (message "howm-%s" howm-version))

(defun howm-keyword-file ()
  ;; create .howm-keys
  (when (not (file-exists-p howm-keyword-file))
    (save-excursion
      (find-file howm-keyword-file)
      (when howm-menu-top
        (goto-char (point-min))
        (insert howm-menu-top "\n"))
      (set-buffer-modified-p t)
      (save-buffer)
      (kill-buffer nil)))
  howm-keyword-file)

(add-hook 'howm-view-open-hook 'howm-set-mode)
(defun howm-set-mode ()
  (when (howm-set-mode-p)
    (howm-set-configuration-for-major-mode major-mode)
    (howm-mode 1)))

(defun howm-set-mode-p (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let ((hdir (car (howm-search-path))))
      (and (buffer-file-name)
           (howm-folder-territory-p hdir (buffer-file-name))))))

(defun howm-normalize-file-name (filename)
  (let ((f (expand-file-name filename)))
    ;; for meadow
    (if (string-match "^[A-Z]:" f)
        (let ((drive (substring f 0 1))
              (rest (substring f 1)))
          (concat (downcase drive) rest))
      f)))

(defvar howm-configuration-for-major-mode nil)
;; ;; sample
;; (setq howm-configuration-for-major-mode
;;   '(
;;     ;; fix me
;;     (emacs-lisp-mode
;;      . (
;;         (howm-keyword-format . "(def[a-z]+ +%s[ \t\r\n]")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "(\\(def[a-z]+\\) +'?\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)") ;; ' for (defalias 'xxx ...)
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^(.*$")
;; ;;         (howm-view-title-regexp . "^[^; \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^; \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (scheme-mode
;;      . (
;;         (howm-keyword-format . "(def[a-z]+ +[(]?%s[) \t\r\n]")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "(\\(def[a-z]+\\) +[(]?\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^[^; \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^; \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (ruby-mode
;;      . (
;;         (howm-keyword-format . "\\(def\\|class\\) +%s\\b")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "\\(def\\|class\\) +\\([-+=*/_~!@$%^&:<>{}?a-zA-Z0-9]+\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "^[^# \t\r\n].*$")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "^[^# \t\r\n].*$")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     (yatex-mode
;;      . (
;;         (howm-keyword-format . "\\\\label%s")
;;         (howm-keyword-regexp-format . "%s")
;;         (howm-keyword-regexp . "\\(\\\\label\\)\\({[^}\r\n]+}\\)")
;;         (howm-keyword-regexp-hilit-pos . 1)
;;         (howm-keyword-regexp-pos . 2)
;;         (howm-view-title-regexp . "\\\\\\(\\(sub\\)*section\\|chapter\\|part\\|begin\\)")
;;         (howm-view-title-regexp-pos . 0)
;;         (howm-view-title-regexp-grep . "\\\\((sub)*section|chapter|part|begin)")
;;         (howm-mode-title-face . nil)
;;         (howm-keyword-list-alias-sep . nil)
;;         (howm-view-preview-narrow . nil)
;;         ))
;;     ))

(defun howm-set-configuration-for-file-name (f)
  (let ((mode (howm-auto-mode f)))
    (howm-set-configuration-for-major-mode mode)))

(defun howm-set-configuration-for-major-mode (mode)
  (let ((a (cdr (assoc mode howm-configuration-for-major-mode))))
    (when a  ;; I know this is redundant.
      (mapc (lambda (sv)
              (let ((symbol (car sv))
                    (value (cdr sv)))
                (set (make-local-variable symbol) value)))
            a))))

;; copied and modified from set-auto-mode in /usr/share/emacs/21.2/lisp/files.el
;; (I don't want to set the mode actually. Sigh...)
(howm-dont-warn-free-variable auto-mode-interpreter-regexp)
(defvar howm-auto-mode-interpreter-regexp
  (if (boundp 'auto-mode-interpreter-regexp) ;; xemacs doesn't have it.
      auto-mode-interpreter-regexp
    "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
(defun howm-auto-mode (&optional file-name)
  "Major mode appropriate for current buffer.
This checks for a -*- mode tag in the buffer's text,
compares the filename against the entries in `auto-mode-alist',
or checks the interpreter that runs this file against
`interpreter-mode-alist'.

It does not check for the `mode:' local variable in the
Local Variables section of the file; for that, use `hack-local-variables'.

If `enable-local-variables' is nil, this function does not check for a
-*- mode tag.

This function merely returns the mode; it does not set the mode.
"
  ;; Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
  (let (beg end done modes ans)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (and enable-local-variables
           ;; Don't look for -*- if this file name matches any
           ;; of the regexps in inhibit-first-line-modes-regexps.
           (let ((temp inhibit-first-line-modes-regexps)
                 (name (file-name-sans-versions (or file-name ""))))
             (while (let ((sufs inhibit-first-line-modes-suffixes))
                      (while (and sufs (not (string-match (car sufs) name)))
                        (setq sufs (cdr sufs)))
                      sufs)
               (setq name (substring name 0 (match-beginning 0))))
             (while (and temp
                         (not (string-match (car temp) name)))
               (setq temp (cdr temp)))
             (not temp))
           (search-forward "-*-" (save-excursion
                                   ;; If the file begins with "#!"
                                   ;; (exec interpreter magic), look
                                   ;; for mode frobs in the first two
                                   ;; lines.  You cannot necessarily
                                   ;; put them in the first line of
                                   ;; such a file without screwing up
                                   ;; the interpreter invocation.
                                   (end-of-line (and (looking-at "^#!") 2))
                                   (point)) t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-"
                             (save-excursion (end-of-line) (point))
                             t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (if (save-excursion (search-forward ":" end t))
                 ;; Find all specifications for the `mode:' variable
                 ;; and execute them left to right.
                 (while (let ((case-fold-search t))
                          (or (and (looking-at "mode:")
                                   (goto-char (match-end 0)))
                              (re-search-forward "[ \t;]mode:" end t)))
                   (skip-chars-forward " \t")
                   (setq beg (point))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
                         modes))
               ;; Simple -*-MODE-*- case.
               (push (intern (concat (downcase (buffer-substring beg end))
                                     "-mode"))
                     modes)))))
    ;; If we found modes to use, set done.
    (dolist (mode (nreverse modes))
      (when (functionp mode)
        (setq ans mode)
        (setq done t)))
    ;; If we didn't find a mode from a -*- line, try using the file name.
    (if (and (not done) file-name)
        (let ((name file-name)
              (keep-going t))
	  ;; Remove backup-suffixes from file name.
	  (setq name (file-name-sans-versions name))
	  (while keep-going
	    (setq keep-going nil)
	    (let ((alist auto-mode-alist)
		  (mode nil))
	      ;; Find first matching alist entry.
	      (let ((case-fold-search
		     (memq system-type '(vax-vms windows-nt))))
		(while (and (not mode) alist)
		  (if (string-match (car (car alist)) name)
		      (if (and (consp (cdr (car alist)))
			       (nth 2 (car alist)))
			  (setq mode (car (cdr (car alist)))
				name (substring name 0 (match-beginning 0))
				keep-going t)
			(setq mode (cdr (car alist))
			      keep-going nil)))
		  (setq alist (cdr alist))))
	      (if mode
                  (setq ans mode)
		;; If we can't deduce a mode from the file name,
		;; look for an interpreter specified in the first line.
		;; As a special case, allow for things like "#!/bin/env perl",
		;; which finds the interpreter anywhere in $PATH.
		(let ((interpreter
		       (save-excursion
			 (goto-char (point-min))
			 (if (looking-at howm-auto-mode-interpreter-regexp)
			     (match-string 2)
			   "")))
		      elt)
		  ;; Map interpreter name to a mode.
		  (setq elt (assoc (file-name-nondirectory interpreter)
				   interpreter-mode-alist))
                  (if elt
                      (setq ans (cdr elt)))))))))
    ans
    ))

;; copied from /usr/share/emacs/21.2/lisp/subr.el
;; for emacs20 and xemacs
(when (not (fboundp 'replace-regexp-in-string))
  (defun replace-regexp-in-string (regexp rep string &optional
                                          fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacments it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
          (start (or start 0))
          matches str mb me)
      (save-match-data
        (while (and (< start l) (string-match regexp string start))
          (setq mb (match-beginning 0)
                me (match-end 0))
          ;; If we matched the empty string, make sure we advance by one char
          (when (= me mb) (setq me (min l (1+ mb))))
          ;; Generate a replacement for the matched substring.
          ;; Operate only on the substring to minimize string consing.
          ;; Set up match data for the substring for replacement;
          ;; presumably this is likely to be faster than munging the
          ;; match data directly in Lisp.
          (string-match regexp (setq str (substring string mb me)))
          (setq matches
                (cons (replace-match (if (stringp rep)
                                         rep
                                       (funcall rep (match-string 0 str)))
                                     fixedcase literal str subexp)
                      (cons (substring string start mb) ; unmatched prefix
                            matches)))
          (setq start me))
        ;; Reconstruct a string from the pieces.
        (setq matches (cons (substring string start l) matches)) ; leftover
        (apply #'concat (nreverse matches)))))
  )

(defvar howm-kill-all-enable-force nil)
(defun howm-kill-all (&optional force-p)
  "Kill all buffers which is howm-mode and unmodified."
  (interactive "P")
  (let ((anyway (and force-p howm-kill-all-enable-force)))
    (when (if anyway
              (yes-or-no-p "Discard all unsaved changes on howm-mode buffers? ")
            (y-or-n-p "Kill all howm-mode buffers? "))
      (when (eq major-mode 'howm-view-summary-mode)
        (howm-view-restore-window-configuration))
      (mapc (lambda (b)
              (when (howm-buffer-p b)
                (when anyway
                  (switch-to-buffer b)
                  (set-buffer-modified-p nil))  ;; dangerous!
                (when (not (buffer-modified-p b))
                  (kill-buffer b))))
            (buffer-list))
      (message "Done."))))

(defun howm-toggle-buffer ()
  (interactive)
  (if (howm-buffer-p)
      (howm-switch-to-nonhowm-buffer)
    (howm-switch-to-howm-buffer)))
(defun howm-switch-to-howm-buffer ()
  (interactive)
  (let ((b (howm-find-buffer #'howm-buffer-p)))
    (if b
        (switch-to-buffer b)
      (howm-menu))))
(defun howm-switch-to-nonhowm-buffer ()
  (interactive)
  (switch-to-buffer (or (howm-find-buffer #'(lambda (b)
                                              (not (howm-buffer-p b))))
                        (error "No nonhowm buffer"))))

(defun howm-find-buffer (pred)
  (catch :found
    (mapc (lambda (b)
            (cond ((howm-internal-buffer-p b) nil) ;; skip
                  ((funcall pred b) (throw :found b))
                  (t t)))
          (buffer-list))
    nil))

(defun howm-internal-buffer-p (buf)
  (string= (substring (buffer-name buf) 0 1) " "))

(defun howm-buffer-p (&optional buf)
  (let* ((indep-dirs (cons nil *howm-independent-directories*))
         (keyword-bufs (mapcar
                        (lambda (d)
                          (let ((default-directory (or d default-directory)))
                            (howm-keyword-buffer)))
                        indep-dirs)))
    (with-current-buffer (or buf (current-buffer))
      (or howm-mode
          (member major-mode
                  '(howm-view-summary-mode
                    howm-view-contents-mode))
          (member buf keyword-bufs)))))

(defun howm-mode-add-font-lock ()
  (when howm-use-color
    (let ((ks `(,@howm-user-font-lock-keywords
                (,howm-view-title-regexp
                  (0 howm-mode-title-face prepend))
                (,howm-keyword-regexp
                 (,howm-keyword-regexp-hilit-pos howm-mode-keyword-face prepend))
                (,howm-ref-regexp
                 (,howm-ref-regexp-hilit-pos howm-mode-ref-face prepend))
                (,howm-wiki-regexp
                 (,howm-wiki-regexp-pos howm-mode-wiki-face prepend))
                )))
      (cheat-font-lock-append-keywords ks))))

;;; unofficial. may be removed if no one needs them.

(defun howm-show-buffer-as-howm ()
  (interactive)
  (let* ((name (buffer-name))
         (pos (point))
         (s (buffer-substring-no-properties (point-min) (point-max)))
         (b (get-buffer-create (format "*howm[%s]*" name))))
    (set-buffer b)
    (howm-rewrite-read-only-buffer
      (insert s)
      (howm-mode 1)
      (howm-initialize-buffer))
;;     (howm-fontify)
    (goto-char pos)
    (switch-to-buffer b)))

;;; narrowing

(defun howm-narrow-to-memo ()
  (interactive)
  (apply #'narrow-to-region (howm-view-paragraph-region t)))

(defun howm-toggle-narrow ()
  (interactive)
  (if (howm-narrow-p)
      (widen)
    (howm-narrow-to-memo)))

(put 'howm-narrow-to-memo 'disabled t)
(put 'howm-toggle-narrow 'disabled t)

(defun howm-narrow-p ()
  (let ((b (point-min))
        (e (point-max)))
    (save-restriction
      (widen)
      (not (and (equal b (point-min))
                (equal e (point-max)))))))

(defun howm-auto-narrow ()
  (when (cond (*howm-view-item-privilege* nil)
              ((eq howm-auto-narrow t) t)
              (t (member (howm-command) howm-auto-narrow)))
    (howm-narrow-to-memo)))
;;   (when (and (member (howm-command) howm-auto-narrow)
;;              (not *howm-view-item-privilege*))

;;; select file for new memo by hand

(defun howm-create-interactively (&optional use-current-directory)
  (interactive "P")
  (find-file (read-file-name "Memo file: "
                             (if use-current-directory
                                 nil
                               howm-directory)))
  (goto-char (point-max))
  (howm-create-here))

;;; next/previous memo

(put 'howm-save-narrowing 'lisp-indent-hook 0)
(defmacro howm-save-narrowing (&rest body)
  `(let ((narrowp (howm-narrow-p)))
     (when narrowp
       (widen))
     (unwind-protect
         (progn
           ,@body)
       (when narrowp
         (howm-narrow-to-memo)))))

(defun howm-next-memo (n)
  (interactive "p")
  (howm-save-narrowing
    (when (looking-at howm-view-title-regexp)
      (setq n (+ n 1)))
    (re-search-forward howm-view-title-regexp nil nil n)))

(defun howm-previous-memo (n)
  (interactive "p")
  (howm-save-narrowing
    (when (not (looking-at howm-view-title-regexp))
      (setq n (+ n 1)))
    (re-search-backward howm-view-title-regexp nil nil n)))

(defun howm-first-memo ()
  (interactive)
  (howm-save-narrowing
    (goto-char (point-min))))

(defun howm-last-memo ()
  (interactive)
  (howm-save-narrowing
    (goto-char (point-max)))
  (re-search-backward howm-view-title-regexp))

;;; random walk

(defvar howm-random-walk-buf nil "for internal use")
(defvar howm-random-walk-ro t "for internal use")

(defun howm-random-walk ()
  (interactive)
  (let ((orig-bufs (buffer-list))
        (howm-history-file nil))
    (while (let ((v (frame-visible-p (selected-frame))))
             (and v (not (eq v 'icon))
                  (not (input-pending-p))))
      (unwind-protect
          (cond ((eq major-mode 'howm-view-summary-mode)
                 (howm-random-walk-summary))
                (howm-mode
                 (howm-random-walk-text))
                (t
                 (howm-list-all)
                 (howm-random-walk-summary)))
        (mapc (lambda (b)
                (when (and (not (member b orig-bufs))
                           (null (get-buffer-window b)))
                  (kill-buffer b)))
              (buffer-list)))
      (sit-for howm-random-walk-wait))))

(defun howm-random-walk-summary ()
  (let ((n (length (riffle-item-list))))
    (goto-char (point-min))
    (next-line (random n))
    (howm-view-summary-check)
    (sit-for howm-random-walk-wait)
    (howm-view-summary-open)))

(defun howm-random-walk-text ()
  (let* ((ks (howm-keyword-for-goto))
         (k (nth (random (length ks)) ks))
         (d (- (point-max) (point-min))))
    (goto-char (+ (point-min) (random d)))
    (if (search-forward k nil t)
        (goto-char (match-beginning 0))
      (search-backward k nil t))
    (sit-for howm-random-walk-wait)
    (howm-keyword-search k)))

;; named note

(defun howm-open-named-file ()
  "Ask a file name and open it as howm note if it is under howm directory."
  (interactive)
  (let* ((item-dir (lambda (item) (file-name-directory (howm-item-name item))))
         (dir (cond ((eq major-mode 'howm-view-summary-mode)
                     (funcall item-dir (howm-view-summary-current-item)))
                    ((eq major-mode 'howm-view-contents-mode)
                     (funcall item-dir (howm-view-contents-current-item)))
                    (t
                     howm-directory)))
         (fname (read-file-name "Howm file name: " dir)))
    (find-file fname)
    (if (file-exists-p fname)
        (howm-set-mode)
      (progn
        (howm-insert-template "")
        (howm-create-finish)))))

;; imitation of remember.el
;; http://www.emacswiki.org/cgi-bin/emacs-en/RememberMode

;; shamelessly copied from http://newartisans.com/johnw/Emacs/remember.el
;; (I cannot browse http://sacha.free.net.ph/notebook/emacs/dev today.)

(defvar howm-remember-wconf nil
  "for internal use")
(defvar howm-remember-buffer-name "*howm-remember*")
(defvar howm-remember-mode-hook nil)
(let ((m (make-sparse-keymap)))
  (define-key m "\C-c\C-c" 'howm-remember-submit)
  (define-key m "\C-c\C-k" 'howm-remember-discard)
  (defvar howm-remember-mode-map m))
(defvar howm-remember-first-line-to-title nil)

(defun howm-remember ()
  "Add text to new note in howm."
  (interactive)
  (setq howm-remember-wconf (current-window-configuration))
  (switch-to-buffer-other-window (get-buffer-create howm-remember-buffer-name))
  (howm-remember-mode)
  (apply #'message
         `("Remember (%s) or discard (%s)."
           ,@(mapcar (lambda (f)
                       (key-description
                        (where-is-internal f howm-remember-mode-map t)))
                     '(howm-remember-submit howm-remember-discard)))))

(defun howm-remember-mode ()
  "Major mode for `howm-remember'.

\\{howm-remember-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (text-mode)
  (use-local-map howm-remember-mode-map)
  (setq major-mode 'howm-remember-mode
        mode-name "HowmRemember")
  (run-hooks 'howm-remember-mode-hook))

(defun howm-remember-submit ()
  (interactive)
  (save-excursion
    (let* ((title (howm-remember-get-title)) ;; has side effect
           (s (buffer-substring-no-properties (point-min) (point-max))))
      (set-window-configuration howm-remember-wconf)
      (howm-create-file-with-title title)
      (insert s "\n")
      (save-buffer)
      (kill-buffer (current-buffer))))
  (howm-remember-discard))

(defun howm-remember-get-title ()
  (if (not howm-remember-first-line-to-title)
      ""
    (progn
      (goto-char (point-min))
      (prog1
          (buffer-substring-no-properties (point-min)
                                          (line-end-position))
        (forward-line 1)
        (delete-region (point-min) (point))))))

(defun howm-remember-discard ()
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration howm-remember-wconf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fellowship

;; xemacs: add-to-list doesn't have APPEND
;; (add-to-list 'auto-mode-alist '("\\.howm$" . text-mode) t)
(setq auto-mode-alist (append auto-mode-alist 
                              (list '("\\.howm$" . text-mode))))

;; xyzzy doesn't have eval-after-load.
;; It will be useless anyway.
(when (not (fboundp 'eval-after-load))
  (defun eval-after-load (file form)
    nil))

;; xemacs canna doesn't use minor-mode. [2004-01-30]
(defvar action-lock-mode-before-canna nil)
(make-variable-buffer-local 'action-lock-mode-before-canna)
(defadvice canna:enter-canna-mode (around action-lock-fix activate)
  (setq action-lock-mode-before-canna action-lock-mode)
  (setq action-lock-mode nil)
  ad-do-it)
(defadvice canna:quit-canna-mode (around action-lock-fix activate)
  (setq action-lock-mode action-lock-mode-before-canna)
  ad-do-it)

;; (obsolete)
;; 
;; If you have a trouble on behavior of RET key, check this first:
;; (progn (print (mapcar #'car minor-mode-map-alist)) nil)

(defvar howm-ime-fix nil)
(when howm-ime-fix
  (progn
    (defun howm-raise-in-minor-mode-map-alist (mode)
      "Raise MODE to the top in minor-mode-map-alist"
      (let* ((pair (assoc mode minor-mode-map-alist)))
        (when pair
          (setq minor-mode-map-alist
                (cons pair
                      ;; Duplications must be removed for canna. Sigh...
                      (remove pair minor-mode-map-alist))))))

    ;; for canna [2003/09/21]
    ;; canna modes should be prior to howm modes.
    (defun howm-canna-fix ()
      (mapc #'howm-raise-in-minor-mode-map-alist
            '(canna:*fence-mode* canna:*select-mode*)))
    ;; I don't understand curious behavior on duplicate canna:*fence-mode*
    ;; in minor-mode-map-alist. It happens *after* '.emacs'.
    (defadvice canna-toggle-japanese-mode (around howm-fix activate)
      (howm-canna-fix)
      ad-do-it
      (howm-canna-fix))

    ;; for yc.el [2003-11-29][2004-01-15]
    ;; http://www.ceres.dti.ne.jp/~knak/yc.html
    (defun howm-yc-fix ()
      (mapc #'howm-raise-in-minor-mode-map-alist
            '(yc-mode
              yc-henkan-mode
              yc-input-mode
              yc-edit-mode
              yc-select-mode
              yc-defword-mode
              yc-wclist-mode)))
    (defadvice yc-rK-trans (around howm-fix activate)
      (howm-yc-fix)
      ad-do-it)
    ;; (eval-after-load "yc"
    ;;   '(mapc #'howm-raise-in-minor-mode-map-alist
    ;;          '(yc-mode
    ;;            yc-henkan-mode
    ;;            yc-input-mode
    ;;            yc-edit-mode
    ;;            yc-select-mode
    ;;            yc-defword-mode
    ;;            yc-wclist-mode)))

    ;; for tamago [2003-12-20]
    (eval-after-load "egg"
      '(mapc #'howm-raise-in-minor-mode-map-alist
             '(egg:henkan-mode-in-use egg-mode egg:*in-fence-mode*)))

    ;; for anthy [2003-12-29]
    (eval-after-load "anthy"
      '(mapc #'howm-raise-in-minor-mode-map-alist
             '(anthy-minor-mode)))
    ))
 
;; for mcomplete.el [2003-12-17]
;; http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el
;; error when this-command is (lambda () (interactive) ...)
(defadvice mcomplete-p (around symbol-check activate)
  (and (symbolp this-command)
       ad-do-it))

;; for auto-save-buffers.el [2004-01-10]
;; http://www.namazu.org/~satoru/auto-save/
;; http://homepage3.nifty.com/oatu/emacs/misc.html
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=auto%20save
(defvar howm-auto-save-buffers-disposed nil)
(howm-dont-warn-free-variable auto-save-buffers-regexp)
(howm-dont-warn-free-variable auto-save-reject-buffers-regexp)
(defun howm-auto-save-buffers-p ()
  (let ((f (howm-file-name)))
    (and (if (boundp 'auto-save-buffers-regexp)
             (string-match auto-save-buffers-regexp f)
           nil)
         (if (boundp 'auto-save-reject-buffers-regexp)
             (not (string-match auto-save-reject-buffers-regexp f))
           t))))
(defun howm-auto-save-buffers-dispose ()
  (setq howm-menu-refresh-after-save nil)
  (setq howm-refresh-after-save nil)
  (setq howm-auto-save-buffers-disposed t)
  (message "howm: Automatic refresh is disabled when auto-save-buffers is called."))
(defadvice auto-save-buffers (around howm-dispose activate)
  (if (or howm-auto-save-buffers-disposed
          (not (howm-auto-save-buffers-p)))
      ad-do-it
    (howm-auto-save-buffers-dispose)))
(defun howm-basic-save-buffer ()
  (basic-save-buffer)
  ;; Okuyama's auto-save-buffers modifies write-region temporally.
  ;; As a side effect, basic-save-buffer does not update buffer-modified-p.
  ;; http://homepage3.nifty.com/oatu/emacs/misc.html
  (let ((in-auto-save-buffers (eq (symbol-function 'write-region)
                                  'auto-save-buffers-write-region)))
    (when in-auto-save-buffers
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))))

;; howm on ChangeLog Memo
(defun howm-setup-change-log ()
  (setq howm-keyword-format "\t* %s")
  (setq howm-keyword-regexp "^\t\\(\\*\\)[ \t]+\\([^:\r\n]+\\)")
  (setq howm-keyword-regexp-hilit-pos 1) ;; 「関連キーワード」用
  (setq howm-keyword-regexp-pos 2)
  (setq howm-view-title-regexp "^$")
  (setq howm-view-title-regexp-pos 0)
  (setq howm-view-title-regexp-grep 'sorry-not-yet)
  (setq howm-use-color nil)
  (setq howm-menu-top nil)
  (defadvice howm-exclude-p (around change-log (filename) activate)
    (setq ad-return-value
          (not (find-if (lambda (dir)
                          (string= (howm-file-name)
                                   (file-relative-name filename dir)))
                        (howm-search-path)))))
  (defadvice howm-create-file-with-title (around change-log (title) activate)
    (howm-create-file)
    (when (string-match howm-keyword-regexp title)
      (setq title (match-string-no-properties howm-keyword-regexp-pos
                                              title)))
    (insert title))
  (defadvice howm-create-file (around change-log
                                      (&optional keep-cursor-p) activate)
    (let* ((default (howm-file-name))
           (file (expand-file-name default howm-directory))
           (dir (file-name-directory file))
           (buffer-file-name file)) ;; don't insert file name
      (make-directory dir t)
      (add-change-log-entry nil file)))
  (add-hook 'change-log-mode-hook 'howm-mode)
  )

;; howm with ChangeLog Memo
(defvar howm-change-log-file-name "ChangeLog")
(defun howm-to-change-log ()
  (interactive)
  (let* ((title (howm-title-at-current-point))
         (file (expand-file-name howm-change-log-file-name howm-directory))
         ;; cheat add-change-log-entry
         (buffer-file-name title)
         (default-directory howm-directory))
    (add-change-log-entry nil file)))
(defun howm-from-change-log ()
  (interactive)
  (let* ((title-regexp "^\t[*][ \t]*\\(.*\\)$")
         (title-regexp-pos 1)
         (title (howm-title-at-current-point nil
                                             title-regexp title-regexp-pos)))
    (howm-create-file-with-title title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bug Report

;; Japanese is assumed at now.

(defun howm-test ()
  (howm-set-lang)
  (howm-bug-report))

(defun howm-set-lang ()
  (set-language-environment "Japanese")
  (set-default-coding-systems 'euc-jp)
  (set-buffer-file-coding-system 'euc-jp-unix)
  (set-terminal-coding-system 'euc-jp)
  (set-keyboard-coding-system 'euc-jp)
  )

(defun howm-compiled-p ()
  (byte-code-function-p (symbol-function 'howm-compiled-p)))
(defun howm-make-file-p ()
  (eval-when-compile
    (getenv "HOWM_MAKE")))
(defun howm-test-p ()
  (getenv "HOWM_TEST"))

(defun howm-bug-report (&optional show-sym)
  (interactive "P")
  (let ((report-buf (format-time-string "howm-bug-report-%Y%m%d-%H%M%S"))
        (template "sample/bug-report.txt"))
    (switch-to-buffer report-buf)
    (when (not (howm-buffer-empty-p))
      (error "Buffer %s exists (and not empty)." report-buf))
    (if (file-exists-p template)
        (insert-file-contents template)
      (insert "Please copy the following text to your bug report.\n\n"))
    (goto-char (point-max))
    (mapcar (lambda (sv)
              (insert (format "%s: %s\n" (car sv) (cdr sv))))
            `(
              ("howm" . ,(howm-version-long))
              ,@(honest-report-version-assoc)
              ))
    (when show-sym
      (goto-char (point-max))
      (insert "\n(List of variables)\n")
      (insert (howm-symbols-desc)))
    (goto-char (point-min))))

(defun howm-version-long ()
  (format "%s (compile: %s, make: %s, test: %s)"
          howm-version
          (howm-compiled-p)
          (howm-make-file-p)
          (howm-test-p)))

(defun howm-emacs-build-time ()
  (if (stringp emacs-build-time)
      emacs-build-time  ;; xemacs
    (format-time-string "%Y-%m-%d"
                        emacs-build-time)))

(defun howm-symbols-desc (&optional max-desc-len)
  (when (null max-desc-len)
    (setq max-desc-len 50))
  (apply #'concat
         (mapcar (lambda (sym)
                   (when (boundp sym)
                     (let ((v (format "%S" (symbol-value sym))))
                       (when (and (numberp max-desc-len)
                                  (< max-desc-len (length v)))
                         (setq v
                               (let* ((tl (/ max-desc-len 4))
                                      (hd (- max-desc-len tl)))
                                 (concat (substring v 0 hd)
                                         " ... "
                                         (substring v (- tl))))))
                       (format "%s: %s\n" (symbol-name sym) v))))
                 (sort (howm-symbols)
                       (lambda (x y)
                         (string< (symbol-name x) (symbol-name y)))))))

(defvar howm-required-features '(
                                cheat-font-lock
                                action-lock
                                riffle
                                gfunc
                                illusion
                                )
  "List of features which are required for, and distributed with, howm itself.")

(defun howm-prefix-names ()
  (mapcar #'symbol-name (cons 'howm howm-required-features)))

(defun howm-symbols ()
  (let* ((reg (format "^%s" (regexp-opt (howm-prefix-names) t)))
         (a nil))
    (mapatoms (lambda (s)
                (when (string-match reg (symbol-name s))
                  (setq a (cons s a)))))
    a))

(defun howm-elp ()
  (interactive)
  (mapcar #'elp-instrument-package
          (howm-prefix-names)))

(defvar howm-sample-directory (expand-file-name "sample/")
  "for internal use")
(defun howm-bug-shot ()
  (interactive)
  (let* ((version (concat "[howm] " (howm-version-long)))
         (init (and (howm-test-p)
                    (let ((f (expand-file-name "dot.emacs"
                                               howm-sample-directory)))
                      (and (file-readable-p f)
                           (with-temp-buffer
                             (insert-file-contents f)
                             (buffer-substring-no-properties (point-min)
                                                             (point-max)))))))
         (header (if init
                     (concat version "\n\n[init]\n" init)
                   version))
         (footer "--- your comment ---"))
    (honest-report header footer)
    (message "Please copy this buffer to your report.")))

;;;

(provide 'howm-misc)

;;; howm-misc.el ends here
