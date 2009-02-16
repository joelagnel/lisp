;;; blank-mode --- Minor mode to visualize blanks (SPACE and TAB).

;; Copyright (C) 2000 Vinicius Jose Latorre

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Keywords:	data, wp
;; Time-stamp:	<2000/11/22 18:30:12 vinicius>
;; Version:	3.1
;; X-URL:	http://www.cpqd.com.br/~vinicius/emacs/

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to visualize blanks (SPACE and TAB).
;;
;; To use blank-mode, insert in your ~/.emacs:
;;
;;    (require 'blank-mode)
;;
;; Or:
;;
;;    (autoload 'blank-mode-on        "blank-mode"
;;      "Turn on blank visualization."   t)
;;    (autoload 'blank-mode-off       "blank-mode"
;;      "Turn off blank visualization."  t)
;;    (autoload 'blank-mode           "blank-mode"
;;      "Toggle blank visualization."    t)
;;    (autoload 'blank-mode-customize "blank-mode"
;;      "Customize blank visualization." t)
;;
;; For good performance, be sure to byte-compile blank-mode.el, e.g.
;;
;;    M-x byte-compile-file <give the path to blank-mode.el when prompted>
;;
;; This will generate blank-mode.elc, which will be loaded instead of
;; blank-mode.el.
;;
;; blank-mode was tested with GNU Emacs 20.6.1.
;;
;;
;; Using blank-mode
;; ----------------
;;
;; To activate blank-mode, type:
;;
;;    M-x blank-mode-on RET
;;
;; Or:
;;
;;    C-u 1 M-x blank-mode RET
;;
;; To deactivate blank-mode, type:
;;
;;    M-x blank-mode-off RET
;;
;; Or:
;;
;;    C-u 0 M-x blank-mode RET
;;
;; To toggle blank-mode, type:
;;
;;    M-x blank-mode RET
;;
;; To customize blank-mode, type:
;;
;;    M-x blank-mode-customize RET
;;
;; You can also bind `blank-mode', `blank-mode-on', `blank-mode-off' and
;; `blank-mode-customize' to some key, like:
;;
;;    (global-set-key "\C-c\C-a" 'blank-mode-on)
;;    (global-set-key "\C-c\C-e" 'blank-mode-off)
;;    (global-set-key "\C-c\C-t" 'blank-mode)
;;    (global-set-key "\C-c\C-c" 'blank-mode-customize)
;;
;;
;; Hooks
;; -----
;;
;; blank-mode has the following hook variables:
;;
;; `blank-mode-hook'
;;    It is evaluated always when blank-mode is turned on.
;;
;; `blank-load-hook'
;;    It is evaluated after blank-mode package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of blank-mode options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `blank-space-face'		Face used to visualize SPACE.
;;
;; `blank-tab-face'		Face used to visualize TAB.
;;
;; `blank-verbose'		Non-nil means generate messages.
;;
;; `blank-chars'		Specify which kind of blank is visualized.
;;
;; `blank-space-regexp'		Specify space characters regexp.
;;
;; `blank-tab-regexp'		Specify tab characters regexp.
;;
;; `blank-priority'		Specify blank overlay priority.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq blank-space-face 'underline)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET blank-space-face RET underline RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Data* group,
;;	 expand *Blank* group
;;	 and then customize blank-mode options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v blank-space-face RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x blank-mode-customize RET
;;
;;    and then customize blank-mode options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
;; whitespace-mode on XEmacs.
;;
;; Thanks to all who emailed comments.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;; GNU Emacs
(or (fboundp 'set-window-redisplay-end-trigger)
    (defalias 'set-window-redisplay-end-trigger 'ignore))


;; XEmacs needs overlay emulation package
(eval-and-compile
  (and (let (case-fold-search)
	 (string-match "XEmacs\\|Lucid\\|Epoch" emacs-version))
       (not (require 'overlay))
       (error "`blank-mode' requires `overlay' package.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Variables:


;;; Interface to the command system


(defgroup blank nil
  "Visualize blanks (SPACE and TAB)"
  :link '(emacs-library-link :tag "Source Lisp File" "blank-mode.el")
  :group 'wp
  :group 'data)


(defcustom blank-space-face 'blank-space-face
  "*Symbol face used to visualize SPACE."
  :type 'face
  :group 'blank)


(defface blank-space-face '((t (:background "LightGray")))
  "Face used to visualize SPACE.")


(defcustom blank-tab-face 'blank-tab-face
  "*Symbol face used to visualize TAB."
  :type 'face
  :group 'blank)


(defface blank-tab-face '((t (:inverse-video t)))
  "Face used to visualize TAB.")


(defcustom blank-verbose t
  "*Non-nil means generate messages."
  :type 'boolean
  :group 'blank)


(defcustom blank-chars 'tabs-and-spaces
  "*Specify which kind of blank is visualized.

Valid values are:

   'tabs-and-spaces     TABs and SPACEs are visualized.
   'tabs                only TABs are visualized.
   'spaces              only SPACEs are visualized.

Any other value is treated as `tabs-and-spaces'."
  :type '(radio :tag "Kind of Blank"
		(const tabs-and-spaces)
		(const tabs)
		(const spaces))
  :group 'blank)


(defcustom blank-space-regexp "\\( +\\)"
  "*Specify space characters regexp.

If you're using `mule' package, it may exists other characters besides \" \"
that it should be considered space.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualize only leading spaces.
   \"\\\\( +$\\\\)\"		visualize only trailing spaces.
   \"\\\\(^ +\\\\| +$\\\\)\"	visualize leading and/or trailing spaces.
   \"\\t\\\\( +\\\\)\\t\"	visualize only spaces between tabs."
  :type '(regexp :tag "Space Chars")
  :group 'blank)


(defcustom blank-tab-regexp "\\(\t+\\)"
  "*Specify tab characters regexp.

If you're using `mule' package, it may exists other characters besides \"\\t\"
that it should be considered tab.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualize only leading tabs.
   \"\\\\(\\t+$\\\\)\"		visualize only trailing tabs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	visualize leading and/or trailing tabs.
   \" \\\\(\\t+\\\\) \"	visualize only tabs between spaces."
  :type '(regexp :tag "Tab Chars")
  :group 'blank)


(defcustom blank-priority 0
  "*Specify blank overlay priority.

Higher integer means higher priority, so blank overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'blank)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros I


(defmacro blank-message (&rest body)
  (` (and blank-verbose (interactive-p)
	  (message (,@ body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun blank-mode-customize ()
  "Customize blank-mode options."
  (interactive)
  (customize-group 'blank))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User commands


(defvar blank-mode nil)
(make-variable-buffer-local 'blank-mode)


;;;###autoload
(defun blank-mode (&optional arg)
  "Toggle blank visualization.

If ARG is null, toggle blank visualization.
If ARG is a number and is greater than zero, turn on visualization; otherwise,
turn off visualization."
  (interactive "P")
  (if (if arg
	  (> (prefix-numeric-value arg) 0)
	(not blank-mode))
      (blank-mode-on)
    (blank-mode-off))
  (blank-message "Blank Mode is now %s." (if blank-mode "on" "off")))


;;;###autoload
(defun blank-mode-on ()
  "Turn on blank visualization."
  (interactive)
  (or (and (boundp 'blank-mode) blank-mode)
      (let ((inhibit-point-motion-hooks t))
	(setq blank-mode t)
	(blank-after-scroll-on (get-buffer-window (current-buffer))
			       (window-start))
	(run-hooks 'blank-mode-hook)
	(make-local-hook 'after-change-functions)
	(add-hook 'after-change-functions 'blank-after-change-function t t)
	(make-local-hook 'window-scroll-functions)
	(remove-hook 'window-scroll-functions 'blank-after-scroll-off t)
	(add-hook 'window-scroll-functions 'blank-after-scroll-on t t)
	(blank-message "Blank Mode is now on."))))


;;;###autoload
(defun blank-mode-off ()
  "Turn off blank visualization."
  (interactive)
  (and (boundp 'blank-mode) blank-mode
       (let ((inhibit-point-motion-hooks t))
	 (setq blank-mode nil)
	 (remove-hook 'after-change-functions 'blank-after-change-function t)
	 (remove-hook 'window-scroll-functions 'blank-after-scroll-on t)
	 (add-hook 'window-scroll-functions 'blank-after-scroll-off t t)
	 (blank-after-scroll-off (get-buffer-window (current-buffer))
				 (window-start))
	 (blank-message "Blank Mode is now off."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros II (adapted from lazy-lock.el)


;; This is to preserve/protect things when modifying text properties.
(defmacro blank-save-buffer-state (&rest body)
  "Eval BODY restoring buffer state."
  `(save-excursion
     (save-restriction
       (save-match-data
	 (let ((modified (buffer-modified-p))
	       (buffer-undo-list t)
	       (inhibit-read-only t)
	       (inhibit-point-motion-hooks t)
	       before-change-functions
	       after-change-functions
	       deactivate-mark
	       buffer-file-name
	       buffer-file-truename
	       inhibit-quit)
	   (widen)
	   ,@body
	   (set-buffer-modified-p modified))))))

(put 'blank-save-buffer-state 'lisp-indent-function 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions


(defvar blank-space-match-list nil)
(defvar blank-tab-match-list   nil)


(defvar blank-space-mark "")
(make-variable-buffer-local 'blank-space-mark)
(defvar blank-tab-mark "")
(make-variable-buffer-local 'blank-tab-mark)


(defun blank-after-scroll-on (window window-start)
  (blank-save-buffer-state
    ;; Called from `window-scroll-functions'.
    ;; Visualize blanks on WINDOW from WINDOW-START following the scroll.
    (let ((start (if window
		     window-start
		   (point-min)))
	  (end (if window
		   (window-end window t)
		 (min (+ (point-min) 4096) (point-max)))))
      (cond
       ((not (and (string= blank-space-mark blank-space-regexp)
		  (string= blank-tab-mark blank-tab-regexp)))
	(setq blank-space-match-list (blank-see-regexp 'blank-space-regexp)
	      blank-tab-match-list   (blank-see-regexp 'blank-tab-regexp))
	(blank-remove-prop (point-min) (point-max))
	(blank-add-prop start end)
	(setq blank-space-mark blank-space-regexp
	      blank-tab-mark blank-tab-regexp))
       ((text-property-not-all start end 'blank-mode t)
	(blank-add-prop start end))
       ))
    ;; A prior deletion that did not cause scrolling, followed by a scroll,
    ;; would result in an unnecessary trigger after this if we did not cancel
    ;; it now.
    (and window
	 (set-window-redisplay-end-trigger window nil))))


(defun blank-after-scroll-off (window window-start)
  (blank-save-buffer-state
    ;; Called from `window-scroll-functions'.
    ;; Don't visualize blanks on WINDOW from WINDOW-START following the
    ;; scroll.
    (let ((start (if window
		     window-start
		   (point-min)))
	  (end (if window
		   (window-end window t)
		 (min (+ (point-min) 4096) (point-max)))))
      (cond
       ((text-property-any start end 'blank-mode t)
	(blank-remove-prop start end))
       ((not (text-property-any (point-min) (point-max) 'blank-mode t))
	(remove-hook 'window-scroll-functions 'blank-after-scroll-off t))
       ))
    ;; A prior deletion that did not cause scrolling, followed by a scroll,
    ;; would result in an unnecessary trigger after this if we did not cancel
    ;; it now.
    (and window
	 (set-window-redisplay-end-trigger window nil))))


(defun blank-after-change-function (beg end oldlen)
  ;; Called from `after-change-functions'.
  ;; Visualize blanks from BEG to END.
  (blank-save-buffer-state
    ;; Rescan between start of lines enclosing the region.
    (goto-char beg)
    (beginning-of-line)
    (setq beg (point))
    (goto-char end)
    (forward-line 1)
    (setq end (point))
    (blank-remove-prop beg end)
    (blank-add-prop beg end)))


(defun blank-remove-prop (beg end)
  (let ((overlays (overlays-in beg end)))
    (while overlays
      (and (overlay-get (car overlays) 'blank-mode)
	   (delete-overlay (car overlays)))
      (setq overlays (cdr overlays))))
  (remove-text-properties beg end '(blank-mode nil)))


(defun blank-add-prop (beg end)
  (and (memq blank-chars '(spaces tabs-and-spaces))
       (blank-add-prop-regexp beg end blank-space-regexp blank-space-face
			      blank-space-match-list))
  (and (memq blank-chars '(tabs tabs-and-spaces))
       (blank-add-prop-regexp beg end blank-tab-regexp blank-tab-face
			      blank-tab-match-list)))


(defun blank-add-prop-regexp (beg end regexp face match-list)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward regexp end 'NOERR)
      (let ((match match-list))
	(while match
	  (let ((the-beg (match-beginning (car match)))
		(the-end (match-end (car match)))
		overlay)
	    (let ((overlays (overlays-in the-beg the-end)))
	      (while overlays
		(let ((ov (car overlays)))
		  (and (overlay-get ov 'blank-mode)
		       (let ((oface (overlay-get ov 'face)))
			 (cond ((eq oface face)
				(setq overlay  ov
				      overlays nil))
			       ((eq oface blank-space-face))
			       ((eq oface blank-tab-face))
			       (t
				(delete-overlay ov))))))
		(setq overlays (cdr overlays))))
	    (if overlay
		(move-overlay overlay the-beg the-end)
	      (setq overlay (make-overlay the-beg the-end))
	      (overlay-put overlay 'face face)
	      (overlay-put overlay 'blank-mode t))
	    (overlay-put overlay 'priority blank-priority))
	  (setq match (cdr match)))))
    (add-text-properties beg end '(blank-mode t))))


(defun blank-see-regexp (var-sym)
  (let ((start 0)
	(index 0)
	(pair  0)
	mlist)
    (while (setq start (string-match "\\\\[()]" (symbol-value var-sym) start))
      (setq start (1+ start))
      (if (= (aref (symbol-value var-sym) start) ?\))
	  (setq pair (1- pair))
	(setq index (1+ index))
	(and (zerop pair)
	     (setq mlist (cons index mlist)))
	(setq pair (1+ pair))))
    (when (< pair 0)
      (setq index 0)
      (while (< pair 0)
	(set var-sym (concat "\\(" (symbol-value var-sym)))
	(setq pair (1+ pair))))
    (while (> pair 0)
      (set var-sym (concat (symbol-value var-sym) "\\)"))
      (setq pair (1- pair)))
    (when (zerop index)
      (set var-sym (concat "\\(" (symbol-value var-sym) "\\)"))
      (setq mlist '(1)))
    mlist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'minor-mode-alist '(blank-mode " Blank"))


(provide 'blank-mode)


(run-hooks 'blank-load-hook)


;;; blank-mode.el ends here
