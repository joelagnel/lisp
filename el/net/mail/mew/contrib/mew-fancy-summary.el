;;; mew-fancy-summary.el --- Fontify Mew summary buffer
;;
;; Author: Shun-ichi TAHARA <jado@flowernet.gr.jp>
;;	   Hideyuki SHIRAI <shirai@mew.org>
;;
;; Time-stamp: <03/13/2002 20:45 shirai>

;;; Commentary:
;;
;; HOW TO USE
;;
;; 1. Set some customize variables and faces as you like.
;;
;; 2. Require this file in your init file of Emacsen.
;;
;;    (add-hook 'mew-init-hook (lambda () (require 'mew-fancy-summary)))
;;
;; 3. BE SURE TO USE jit-lock, lazy-shot or lazy-lock.
;;
;;    ;;; common setting
;;    (require 'font-lock)
;;    ;; (global-font-lock-mode t) ;; as you like.
;;
;;    ;;; jit-lock-mode (for Emacs-21)
;;    (setq font-lock-support-mode 'jit-lock-mode) ;; system default value.
;;
;;    ;;; lazy-shot-mode (for XEmacs)
;;    (require 'font-lock)
;;    (setq font-lock-support-mode 'lazy-shot-mode)
;;    (setq lazy-shot-verbose nil)
;;    (setq lazy-shot-stealth-verbose nil)
;;
;;    ;;; lazy-lock-mode (for All Emacsen)
;;    (require 'font-lock)
;;    (setq font-lock-support-mode 'lazy-lock-mode)
;;    (when (featurep 'xemacs)
;;      (setq lazy-lock-minimum-size 0))
;;
;; 4. If you use 'jit-lock-mode' and feel 'scan is too slow'
;;    , please put bellow code to your init file.
;;
;;    (add-hook 'mew-init-hook
;;              (lambda ()
;;                (defvar mew-fancy-disable nil)
;;                (make-variable-buffer-local 'mew-fancy-disable)
;;                (defadvice mew-scan (before fancy-disable activate)
;;                  (setq mew-fancy-disable t))
;;                (defadvice mew-scan-filter (before fancy-disable activate)
;;                  (when (and mew-fancy-disable font-lock-mode)
;;                    (setq mew-fancy-disable nil)
;;                    (font-lock-mode -1)))
;;                ))
;;
;; BUGS
;;
;; 1. If you choice 'lazy-lock-mode', mew-summary-cook-folders() becomes
;;    too slow.
;;

;;; Code:

(eval-when-compile (require 'mew))
(defconst mew-fancy-summary-version "mew-fancy-summary 0.23")

;;; User-customizable variables

(defcustom mew-fancy-summary-face-spec
  '((num  . mew-fancy-summary-face-marginal)
    (size . mew-fancy-summary-face-marginal)
    (type . mew-fancy-summary-face-type)
    (from . "From:")
    (subj . "Subject:")
    (date . "Date:")
    (year . "Date:")
    (time . "Date:"))
  "*Alist of face specs for fancy-summary mode, corresponding to
scan-form entries. Each entry consists of a pair of symbol or string
and face, header name or function.

The symbol is what can be set in scan-form, including user-extended
entry.

If you want to highlight the raw string in 'mew-scan-form', put the
string and the corresponding face (or another possible) in this alist.
Also define the face, if needed.

The face and the function are both set as a symbol in cdr of each
entry. As a special case, you can specify 'nil' not to highlight the
field.

If the symbol is a function name, the function is called with an
integer argument indicating the width of the field (or 0 means
unlimited) and with the pointer pointing the top of the field to be
highlighted. In the function, you can fontify it as you like, and no
return values are needed (even the cursor position need not be cared).

The string in cdr of the entry indicates a header name, and a face is
decided by 'mew-field-spec' with the header name."
  :group 'mew-highlight
  :type '(repeat (cons (choice symbol string) (choice face string function))))

(defcustom mew-fancy-summary-extended-face-spec
  '((truncated . mew-fancy-summary-face-truncated)
    (thread    . mew-fancy-summary-face-marginal)
    (special   . mew-fancy-summary-face-special)
    (ml	       . mew-fancy-summary-face-tag)
    (attach    . mew-fancy-summary-face-marginal)
    (to	       . "To:"))
  "*Alist of face specs for fancy-summary mode, not corresponding to
scan-form entries. Each entry consists of a pair of symbol and face,
header name or function.

Symbol is one of those: 'truncated' for 'T' mark in the 'type' field
in scan-form, 'thread' for a thread indent, 'special' for the special
person (see also 'mew-fancy-summary-special-*'), 'ml' for a subject
prefix of ML (see also 'mew-fancy-summary-ml-regex'), and 'attach' for
a attachment line.

For cdr of each element, see 'mew-fancy-summary-face-spec'."
  :group 'mew-highlight
  :type '(repeat (cons (choice symbol string) (choice face string function))))

(defcustom mew-fancy-summary-special-persons nil
  "*List of the 'special' persons.

If the From: header matches a member of this list, 'special' entry of
'mew-fancy-summary-extended-face-spec' is used for highlighting 'from'
part of scan-form."
  :group 'mew-highlight
  :type '(repeat string))

(defcustom mew-fancy-summary-special-addrbook nil
  "*If non-nil, nicknames in the addrbook are treated as 'special'
person."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-fancy-summary-special-to t
  "*If non-nil, 'from' part of scan-form about an entry of the mail
destinated to 'special' person (then displayed To: header) are also
highlighted specially."
  :group 'mew-highlight
  :type 'boolean)

(defcustom mew-fancy-summary-ml-regex "[\[(][^])|\n\r]*[\])|]"
  "*Regex string as a prefix of the subject of mails posted to ML."
  :group 'mew-highlight
  :type 'regexp)

(defcustom mew-fancy-summary-external-highlighting-hook nil
  "*Hook called to highlight an abnormal line.

If the function in the hook highlighted the line successfully, it must
return t, otherwise nil."
  :group 'mew-highlight
  :type 'hook)

;;; Faces

(defface mew-fancy-summary-face-marginal nil
  "*Face to highlight the marginal part of Summary buffer."
  :group 'mew-highlight)

(defface mew-fancy-summary-face-type nil
  "*Face to highlight 'type' part of Summary buffer (except 'T' mark)."
  :group 'mew-highlight)

(defface mew-fancy-summary-face-truncated nil
  "*Face to highlight 'T' mark at 'type' part of Summary buffer."
  :group 'mew-highlight)

(defface mew-fancy-summary-face-special nil
  "*Face to highlight the special person in 'from' part of Summary buffer."
  :group 'mew-highlight)

(defface mew-fancy-summary-face-tag nil
  "*Face to highlight the tag part of Summary buffer."
  :group 'mew-highlight)

;;; End of user-customizable stuffs

;;; Internal variables

(defvar mew-fancy-summary-special-list nil)
(defvar mew-fancy-summary-special-alist nil)

(defvar mew-fancy-summary-scan-form nil)
(make-variable-buffer-local 'mew-fancy-summary-scan-form)

(defvar mew-fancy-summary-thread-column nil)
(make-variable-buffer-local 'mew-fancy-summary-thread-column)

;;; Initialization

(add-hook 'mew-summary-mode-hook 'mew-fancy-summary-enable)
(add-hook 'mew-virtual-mode-hook 'mew-fancy-summary-enable)
(add-hook 'mew-thread-display-hook 'mew-fancy-summary-thread-enable)
(add-hook 'mew-pop-sentinel-hook 'mew-fancy-summary-block)
(add-hook 'mew-scan-sentinel-hook 'mew-fancy-summary-block)

(setq mew-use-highlight-mark nil)	; Hilight mark by itself, not by Mew

(defalias 'mew-summary-cook-region 'mew-fancy-summary-make-invisible-region)
(defalias 'mew-highlight-mark-line 'mew-fancy-summary-mark-line)
(defalias 'mew-highlight-unmark-line 'mew-fancy-summary-mark-line)

;;; Setup

(defadvice mew-addrbook-setup (after hl-setup activate)
  "Setup fancy-summary when starting up Mew or executing \"Z\"."
  (mew-fancy-summary-special-setup))

(defun mew-fancy-summary-special-setup ()
  (setq mew-fancy-summary-special-list mew-fancy-summary-special-persons)
  (when mew-fancy-summary-special-addrbook
    (let ((addrbook mew-addrbook-alist) nickname)
      (while addrbook
	(setq nickname (nth 2 (car addrbook)))
	(when (and (stringp nickname)
		   (not (member nickname mew-fancy-summary-special-list)))
	  (setq mew-fancy-summary-special-list
		(cons nickname mew-fancy-summary-special-list)))
	(setq addrbook (cdr addrbook))))))

;;; Workarounds about highlighting in Mew

(defun mew-fancy-summary-remove-invisible ()
  "Remove the invisible hook in Mew."
  (when mew-gemacs-p
    (jit-lock-unregister 'mew-summary-cook-region))
  (remove-hook 'window-scroll-functions 'mew-summary-cook-window 'local)
  (remove-hook 'pre-idle-hook 'mew-summary-cook-window))

(defun mew-fancy-summary-make-invisible-region (beg end &optional interrupt)
  "Faster invisible function for summary with many text properties."
  (when (and (memq major-mode
		   '(mew-summary-mode mew-virtual-mode mew-refile-view-mode))
	     mew-summary-buffer-raw)
    (let ((inhibit-point-motion-hooks t) ret start)
      (catch 'loop
	(mew-elet
	 (save-excursion
	   (goto-char beg)
	   (while (and (< (point) end)
		       (search-forward "\r" end t))
	     (setq start (match-beginning 0))
	     (unless (get-text-property start 'invisible)
	       (put-text-property start (line-end-position) 'invisible t))
	     (forward-line)
	     (when (and interrupt (input-pending-p))
	       (throw 'loop (setq ret t)))))))
      (set-buffer-modified-p nil)
      ret)))

(defun mew-fancy-summary-mark-line (&rest args)
  "Highlighting mark and unmark line is done by mew-fancy-summary."
  (save-excursion
    (mew-fancy-summary-region (point) (point) nil 'strip)))

;;; Activating fancy summary

(defun mew-fancy-summary-enable ()
  "Activate mew-fancy-summary in summary or virtual buffer."
  (mew-fancy-summary-remove-invisible)
  (unless (mew-thread-p)
    (set (make-local-variable 'font-lock-fontify-buffer-function)
	 'mew-fancy-summary-buffer)
    (when (boundp 'font-lock-function)
      (set (make-local-variable 'font-lock-function)
	   'mew-fancy-summary-font-lock-function))
    (set (make-local-variable 'font-lock-fontify-region-function)
	 'mew-fancy-summary-region)
    (setq mew-fancy-summary-scan-form
		  (mew-get-summary-form (mew-summary-folder-name 'ext)))
    (setq mew-fancy-summary-thread-column -1)
    (font-lock-mode 1)))

(defun mew-fancy-summary-font-lock-function (font-lock-mode)
  (when font-lock-mode
    (add-hook 'after-change-functions 'font-lock-after-change-function t t)
    (font-lock-turn-on-thing-lock))
  (unless font-lock-mode
    (remove-hook 'after-change-functions 'font-lock-after-change-function t)
    (font-lock-unfontify-buffer)
    (font-lock-turn-off-thing-lock)))

(when mew-xemacs-p
  (defadvice mew-summary-folder-cache-load (after fancy-summary activate)
    (font-lock-mode 1)))

(defun mew-fancy-summary-thread-enable ()
  "Activate mew-fancy-summary in thread buffer."
  (when (mew-thread-p)
    (mew-fancy-summary-remove-invisible)
    (setq mew-fancy-summary-scan-form
		  (mew-get-summary-form (substring (mew-summary-folder-name 'ext) 1)))
    (setq mew-fancy-summary-thread-column (mew-vinfo-get-column))
    ;; XEmacs + lazy-lock ???
    (when mew-xemacs-p
      (let* ((win (selected-window))
	     (beg (window-start win))
	     (end (window-end win t)))
	(save-excursion
	  (mew-fancy-summary-region
	   (progn (goto-char beg)
		  (forward-line (- (/ (frame-height) 2)))
		  (point))
	   (progn (goto-char end)
		  (forward-line (/ (frame-height) 2))
		  (point))))))
    (when (boundp 'font-lock-function)
      (set (make-local-variable 'font-lock-function)
	    'mew-fancy-summary-font-lock-function))
    (set (make-local-variable 'font-lock-fontify-buffer-function)
	 'mew-fancy-summary-buffer)
    (set (make-local-variable 'font-lock-fontify-region-function)
	 'mew-fancy-summary-region)
    (font-lock-mode 1)))

;;; macros

(defmacro mew-fancy-summary-get-face (key extended)
  `(let ((face (cdr (assoc ,key (if ,extended
				    mew-fancy-summary-extended-face-spec
				  mew-fancy-summary-face-spec)))))
     (if (stringp face)
	 (or (mew-nspec-valface (mew-nspec-by-key face))
	     'mew-face-header-marginal)
       face)))

(defsubst mew-fancy-summary-special-p (key len)
  (when (and mew-fancy-summary-special-list (> (length key) 0))
    (member key (or (cdr (assq len mew-fancy-summary-special-alist))
		    (let ((special mew-fancy-summary-special-list)
			  lst str)
		      (while special
			(setq str (truncate-string-to-width (car special) len))
			(when (string-match "^.+\\( +\\)$" str)
			  (setq str (substring str 0 (match-beginning 1))))
			(setq lst (cons str lst))
			(setq special (cdr special)))
		      (setq mew-fancy-summary-special-alist
			    (cons (cons len lst) mew-fancy-summary-special-alist))
		      lst)))))

;;; Main

(defun mew-fancy-summary-block ()
  (font-lock-mode 1))

(defun mew-fancy-summary-buffer ()
  "Highlight summary buffer with font-lock-mode."
  (interactive)
  (mew-fancy-summary-region (point-min) (point-max)))

(defun mew-fancy-summary-region (beg end &optional loudly strip)
  "Highlight the region of summary buffer with font-lock-mode."
  (interactive "r")
  (let ((pos (point)) linebeg lineend col numend)
    (when (memq major-mode
		'(mew-summary-mode mew-virtual-mode mew-refile-view-mode))
      (mew-elet
       (setq end (progn (goto-char end)
			(forward-line)
			(point)))
       (setq beg (progn (goto-char beg)
			(beginning-of-line)
			(point)))
       (when strip
	 (put-text-property beg end 'face nil))
       (mew-fancy-summary-make-invisible-region beg end)
       ;; Highlight each line
       (while (< (point) end)
	 (setq linebeg (point))
	 (setq col 0)
	 (cond
	  ;; unread line
	  ((and (not (mew-in-decode-syntax-p))
                (setq numend (point-min))
		(eq (char-after) mew-mark-unread))
	   (let ((form mew-fancy-summary-scan-form) entry)
	     (while form
	       (setq entry (car form))
	       (when (= col mew-fancy-summary-thread-column)
                 (setq col (+ col (mew-fancy-summary-do-highlight 'thread t))))
	       (setq col (+ col (mew-fancy-summary-do-highlight entry nil numend)))
	       (setq form (cdr form))))
	   (looking-at "[^\r\n]*")
	   (setq lineend (match-end 0))
	   (put-text-property linebeg (1+ linebeg) 'face 'mew-face-mark-unread))
	  ;; Normal line
	  ((and (not (mew-in-decode-syntax-p))
                (setq numend (point-min))
                (not (looking-at mew-regex-mark)))
	   (let ((form mew-fancy-summary-scan-form) entry)
	     (while form
	       (setq entry (car form))
	       (when (= col mew-fancy-summary-thread-column)
                 (setq col (+ col (mew-fancy-summary-do-highlight 'thread t))))
	       (setq col (+ col (mew-fancy-summary-do-highlight entry nil numend)))
	       (setq form (cdr form))))
	   (looking-at "[^\r\n]*")
	   (setq lineend (match-end 0)))
	  ;; Marked line
	  ((and (not (mew-in-decode-syntax-p))
                (looking-at mew-regex-mark))
           (let ((mark (string-to-char (match-string 0))))
	     (looking-at "[^\r\n]*")
	     (setq lineend (match-end 0))
	     (put-text-property linebeg lineend
				'face (mew-highlight-mark-get-face mark))))
	  ;; Others (usually, expanded multi-part)
	  (t
	   (unless (run-hook-with-args-until-success
		    ;; Highlight thread separator or folder of refile-view
		    ;; by your custom function
		    'mew-fancy-summary-external-highlighting-hook nil)
	     (setq lineend (progn (end-of-line) (point)))
	     (put-text-property linebeg lineend
				'face (mew-fancy-summary-get-face 'attach t))
	     (when mew-use-highlight-mouse-line
	       (remove-text-properties linebeg (min (1+ lineend) (point-max))
				       '(mouse-face))))))
	 (when mew-use-highlight-mouse-line
	   (put-text-property linebeg lineend
			      'mouse-face mew-highlight-mouse-line-face))
	 (forward-line)))
      (set-buffer-modified-p nil)
      (goto-char pos))))

;;;
(defun mew-fancy-summary-do-highlight (entry &optional extended numend)
  (let ((l 0) (col (current-column)) beg end range face
	(spec (mew-fancy-summary-get-spec entry extended)))
    (while spec
      (setq range (car (car spec)))
      (setq face (cdr (car spec)))
      (cond
       ((eq range 'num)
	(if (fboundp face)
	    (funcall face (- numend (point)))
	  (put-text-property (point) numend 'face face))
	(setq l (+ l (- numend (point))))
	(goto-char numend)
	(setq col (current-column)))
       ((stringp range)
	(when (looking-at range)
	  (setq beg (match-beginning 0))
	  (setq end (match-end 0))
	  (if (fboundp face)
	      (funcall face range)
	    (put-text-property beg end 'face face))
	  (goto-char end)
	  (setq l (+ l (- (current-column) col)))
	  (setq col (current-column))))
       ((> range 0)
	(setq col (+ col (- range l)))
	(setq beg (point))
	(if (fboundp face)
	    (funcall face (- range l))
	  (move-to-column col)
	  (put-text-property beg (point) 'face face))
	(setq l range)))
      (setq spec (cdr spec)))
    l))

(defun mew-fancy-summary-get-spec (entry extended)
  (let ((range (cond
		((stringp entry) (string-width entry))
		((listp entry) (abs (car entry)))
		(t 1)))
	(elem (cond
	       ((listp entry) (nth 1 entry))
	       (t entry)))
	face)
    (if (= range 0)			; To the tail of line
	(setq range "[^\r\n]*[^ \r\n]"))
    (setq face (mew-fancy-summary-get-face elem extended))
    (cond
     ((eq elem t)	;; thread indent
      nil)
     ((eq elem 'num)
      (list (cons elem face)))
     ((eq elem 'type)
      (cond
       ((looking-at "T")
	(list (cons range (mew-fancy-summary-get-face 'truncated t))))
       ((looking-at "[^ ]")
	(list (cons range face)))
       (t
	(list (cons range nil)))))
     ((eq elem 'thread)
      (let ((indent (get-text-property (point) 'mew-thread-indent)))
	(if (or (null indent) (= indent 0))
	    nil
	  (list (cons (string-width
		       (mew-buffer-substring
			(point)
			(next-single-property-change (point) 'mew-thread-indent)))
		      face)))))
     ((eq elem 'from)
      (let* ((beg (point))
	     (entry (mew-buffer-substring
		     beg
		     (prog2
			 (move-to-column (+ (current-column) range))
			 (point)
		       (goto-char beg))))
			 (prelen (and (stringp mew-summary-form-from-me-prefix)
						  (string-width mew-summary-form-from-me-prefix)))
	     elen)
	(if (string-match "[^\r\n]*[^ \r\n]" entry)
	    (setq entry (substring entry (match-beginning 0) (match-end 0)))
	  (setq entry ""))
	(setq elen (string-width entry))
	(if (not (and prelen (> prelen 0)
					  (eq (string-match mew-summary-form-from-me-prefix entry) 0)))
	    ;; Mail to me
	    (if (mew-fancy-summary-special-p entry range)
		(list (cons elen (mew-fancy-summary-get-face 'special t))
		      (cons range nil))
	      (list (cons elen face) (cons range nil)))
	  ;; Mail from me
	  (setq face (mew-fancy-summary-get-face 'to t))
	  (if (and prelen mew-fancy-summary-special-to
		   (mew-fancy-summary-special-p (substring entry prelen)
						(- range prelen)))
	      (list (cons prelen face)
		    (cons elen (mew-fancy-summary-get-face 'special t))
		    (cons range nil))
	    (list (cons elen face) (cons range nil))))))
     ((eq elem 'subj)
      (list (cons mew-fancy-summary-ml-regex
		  (mew-fancy-summary-get-face 'ml t))
 	    (cons
	     (if (> mew-fancy-summary-thread-column 0)
		 (save-excursion
 		   (move-to-column mew-fancy-summary-thread-column)
		   (- range
		      (* (mew-thread-get-property (point))
			 mew-thread-indent-width)))
	       range)
	     face)))
     (t
      (list (cons range face))))))

;;;

(provide 'mew-fancy-summary)

;;; Copyright Notice:

;; Copyright (C) 2001 Shun-ichi TAHARA <jado@flowernet.gr.jp>
;; Copyright (C) 1999-2001 Hideyuki SHIRAI <shirai@mew.org>
;; Copyright (C) 1994-2001 Mew developing team.
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
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-fancy-summary.el ends here
