;;; desktop-menu.el --- menu for managing emacs desktops

;; Copyright (C) 1999 2000 2001 2002 Olaf Sylvester

;; Author: Olaf Sylvester <ole_i_dont_like_spam at geekware . de>
;; Maintainer: Olaf Sylvester <ole_i_dont_like_spam at geekware . de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Version: 0.2
;; X-URL: http://www.geekware.de

;; This packages provides a Desktop Menu for managing different Emacs desktops
;; in a directory.
;; 
;; Start menu with
;; M-x desktop-menu
;; 
;; Use key ? for help in Desktop Menu.

;; Why a Desktop Menu?
;; The Emacs package `desktop' is a really nice menu for saving and reading
;; various Emacs desktops in different directories.
;; But often I have the problems:
;; - I want to save various Emacs desktops in the same directory and
;; - I lost the overview for directories with a Emacs desktop.
;; So I've developed a menu which handles various Emacs desktops
;; in one directory and I can give each desktop a nice name.

;;; Customization:

;; There is an customization group called desktop-menu in group desktop
;; Start customization by M-x `desktop-menu-customize'

;;; History:
;; 

;;; Code:

(require 'desktop)

;; ----------------------------------------------------------------------------
;; Variables for customization
;; ----------------------------------------------------------------------------

(defgroup desktop-menu nil
  "Managing different desktop."
  :group 'desktop)

(defcustom desktop-menu-directory "~"
  "*Directory of desktop files."
  :type  'directory
  :group 'desktop-menu)

(defcustom desktop-menu-base-filename
  (convert-standard-filename ".emacs.desktop")
  "*Base filename for different desktop files."
  :type 'file
  :group 'desktop-menu)

(defcustom desktop-menu-list-file
  (convert-standard-filename ".emacs.desktops")
  "*Filename which contains all desktop files."
  :type 'file
  :group 'desktop-menu)

(defcustom desktop-menu-mode-hook nil
  "Hook run after desktop saves the state of Emacs.
This is useful for truncating history lists, for example."
  :type 'hook
  :group 'desktop-menu)

(defcustom desktop-menu-clear 'ask
  "*Specifies the strategy of clearing current desktop.
A desktop will be cleared by `desktop-clear'.
Possible values
 `ask' -- Ask user what to do.
 `no'  -- don't clear current desktop.
 `yes' -- clear current desktop."
  ;;:type 'symbol
  :type '(choice (const :tag "Ask user" ask)
		 (const :tag "Don't delete desktop" no)
		 (const :tag "Always delete desktop" yes))
  :group 'desktop-menu)

(defcustom desktop-menu-ask-user-on-delete t
  "*Flag whether ask user before deleting a desktop."
  :group 'desktop-menu
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom desktop-menu-sort-p t
  "*Flag whether sort desktops by names."
  :group 'desktop-menu
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom desktop-menu-mode-font-lock-keywords
  (list ;; header in font-lock-type-face
        (list "\\(^ Desktops in directory\\) \\(.+\\)"
	      '(1 font-lock-type-face append)
	      '(1 'bold append)
	      '(2 font-lock-function-name-face append))
        (list "^.  \\(.+\\)\\([0-9]+ Buffer\\)"
	      '(1 font-lock-function-name-face)
	      '(2 font-lock-constant-face append))
	)
  "*Fontlock settings for Desktop Menu."
  :type 'sexp
  :group 'desktop-menu)

;; ----------------------------------------------------------------------------
;; Variables for internal use only
;; ----------------------------------------------------------------------------

(defvar desktop-menu-desktops nil
  "List of all known desktops.")

(defvar desktop-menu-current-desktop-name nil
  "Name of current desktop.")

(defvar dtm--window-comming-from nil
  "WIndow we started Desktop Menu.")

(defvar dtm--window-config-comming-from nil
  "Window configuration before starting Desktop Menu.")

(defvar desktop-menu-mode-map ()
  "Keymap of `desktop-menu-mode'.")

(if (and nil desktop-menu-mode-map)
    ()
  (setq desktop-menu-mode-map (make-sparse-keymap))

  (let ((key ?1))
    (while (<= key ?9)
      (define-key desktop-menu-mode-map (char-to-string key) 'digit-argument)
      (setq key (1+ key))))

  (define-key desktop-menu-mode-map "-"       'negative-argument)
  (define-key desktop-menu-mode-map "\e-"     'negative-argument)

  (define-key desktop-menu-mode-map " "       'desktop-menu-down)
  (define-key desktop-menu-mode-map "\C-m"    'desktop-menu-select)
  (define-key desktop-menu-mode-map "m"       'desktop-menu-merge)
  (define-key desktop-menu-mode-map "s"       'desktop-menu-save)
  (define-key desktop-menu-mode-map "s"       'desktop-menu-save)
  (define-key desktop-menu-mode-map [up]      'desktop-menu-up)
  (define-key desktop-menu-mode-map "n"       'desktop-menu-down)
  (define-key desktop-menu-mode-map "p"       'desktop-menu-up)
  (define-key desktop-menu-mode-map [down]    'desktop-menu-down)
  (define-key desktop-menu-mode-map "m"       'desktop-menu-merge)
  (define-key desktop-menu-mode-map "d"       'desktop-menu-delete)
  (define-key desktop-menu-mode-map "n"       'desktop-menu-new)
  (define-key desktop-menu-mode-map "g"       'desktop-menu-refresh)
  (define-key desktop-menu-mode-map "r"       'desktop-menu-rename)
  (define-key desktop-menu-mode-map "c"       'desktop-menu-change-directory)
  (define-key desktop-menu-mode-map "x"       'desktop-menu-clear)
  (define-key desktop-menu-mode-map "^"       'desktop-menu-up-directory)
  (define-key desktop-menu-mode-map "?"       'desktop-menu-help)
  (define-key desktop-menu-mode-map "\C-g"    'desktop-menu-abort)
  (define-key desktop-menu-mode-map "q"       'desktop-menu-quit))

;; ----------------------------------------------------------------------------
;; Desktop Menu functions
;; ----------------------------------------------------------------------------

(defun desktop-menu-initialise ()
  "Create default directory for different desktop files."
  (if (not (file-exists-p desktop-menu-directory))
      (make-directory desktop-menu-directory)))

(defun desktop-menu-save-into (filename)
  "Save current desktop into FILENAME."
  (interactive "F")
  (let ((desktop-basefilename (file-name-nondirectory filename)))
    (desktop-save (file-name-directory filename))))

(defun desktop-menu-load (filename)
  "Load desktop of FILENAME."
  (interactive "F")
  (let ((desktop-dirname      (file-name-directory filename))
	(desktop-basefilename (file-name-nondirectory filename)))
    (cd desktop-dirname) ;; because desktop-read doesn't recognize
                         ;; desktop-dirname correctly.
    (message (format "File: %S Desktopdir: %S"
		     desktop-basefilename
		     desktop-dirname))
    (desktop-read)))

(defun desktop-menu-mode ()
  "Major mode for editing Emacs desktops.
\\<desktop-menu-mode-map>
Aside from a header lines each line describes one Emacs desktop
in directory `desktop-menu-directory' as described in first line.
Move to a line representing a desktop and load desktop by
\\[desktop-menu-select] and save current Emacs desktop by \\[desktop-menu-save].
Leave buffer by \\[desktop-menu-quit].  Abort buffer list by \\[desktop-menu-abort].

For faster navigation each digit key is a digit argument.

\\[desktop-menu-select] -- Select current lines desktop.
\\[desktop-menu-quit]   -- Leave Desktop Menu.
\\[desktop-menu-new]   -- Create a new desktop.
\\[desktop-menu-delete]   -- Delete current lines desktop.
\\[desktop-menu-rename]   -- Rename current lines desktop.
\\[desktop-menu-clear]   -- Clear current Emacs desktop.
\\[desktop-menu-merge]   -- Merge current lines desktop to current desktop.
\\[desktop-menu-change-directory]   -- Change to another directory.
\\[desktop-menu-up-directory]   -- Go up one directory.
\\[desktop-menu-help]   -- Display this help text."
  (interactive)
  (kill-all-local-variables)
  (use-local-map desktop-menu-mode-map)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-verbose)
  (setq major-mode 'desktop-menu-mode
	mode-name "Desktop Menu"
	buffer-read-only t
	truncate-lines t
	font-lock-defaults '(desktop-menu-mode-font-lock-keywords t)
	font-lock-verbose nil)
  (run-hooks 'desktop-menu-mode-hook))

(defun desktop-menu-save-main-list (desktops directory)
  "Save desktops DESKTOPS in DIRECTORY in file `desktop-menu-list-file'."
  (desktop-menu-initialise)
  (let ((mainfile (expand-file-name desktop-menu-list-file
				    directory)))
    (find-file mainfile)
    (erase-buffer)
    (insert ";; ------------------------------------------------------------\n"
            ";; Desktop Files for Emacs in directory "
	    (expand-file-name "." directory) "\n"
	    ";; ------------------------------------------------------------\n"
	    ";; Created " (current-time-string) "\n"
	    ";; Emacs version " emacs-version "\n\n"
	    "(setq desktop-menu-desktops\n"
	    "      (list\n")
    (while desktops
      (insert (format "       '(%S %S)\n"
		      (car (car desktops))
		      (car (cdr (car desktops)))))
      (setq desktops (cdr desktops)))
    (insert "       ))\n")
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun desktop-menu-read-main-list (directory)
  "Read list of desktops in file `desktop-menu-list-file' in DIRECTORY.
Return list of desktops."
  (let (desktop-menu-desktops
	desktops
	(mainfile (expand-file-name desktop-menu-list-file
				    directory)))
    (if (file-exists-p mainfile)
	(save-window-excursion
	  (save-excursion
	    (load-file mainfile))))
    ;; delete non existing desktop files.
    (setq desktops desktop-menu-desktops)
    (let ((list (mapcar 'identity desktop-menu-desktops)))
      (while list
	(if (not (file-exists-p (expand-file-name (car (cdr (car list)))
						  directory)))
	    (setq desktops
		  (remove (car list) desktops)))
	(setq list (cdr list))))
    ;; find more desktop files
    (let* ((base desktop-menu-base-filename)
	   (existing-in-main (mapcar (function (lambda (e) (nth 1 e)))
				     desktops))
	   (all (directory-files directory
				 nil
				 (concat (regexp-quote base)
					 "[0-9]*$"))))
      (while all
	(if (not (member (car all) existing-in-main))
	    (setq desktops
		  (append desktops
			  (list (list (if (string= (car all) base)
					  "Default"
					"No Name")
				      (car all))))))
	(setq all (cdr all))))
    (if desktop-menu-sort-p
	(sort desktops
	      (function (lambda (e1 e2)
			  (string< (car e1) (car e2)))))
      desktops)))

(defun desktop-menu-list (directory &optional read-main-p)
  "List all desktops in DIRECTORY in buffer *Desktop Menu*.
Optional argument READ-MAIN-P: non nil means to read file
`desktop-menu-list-file' for desktop list."
  (switch-to-buffer (get-buffer-create "*Desktop Menu*"))
  (cd directory)
  ;; (y-or-n-p directory)
  (unless (file-readable-p directory)
    (error "%S isn't a valid directory" directory))
  (setq desktop-menu-directory directory)
  (if read-main-p
      (setq desktop-menu-desktops
	    (desktop-menu-read-main-list directory)))
  (desktop-menu-mode)
  (let ((inhibit-read-only t)
	(desktops desktop-menu-desktops))
    (erase-buffer)
    (insert-string " Desktops in directory "
		   (expand-file-name "." desktop-menu-directory)
		   "\n")
    (while desktops
      (let ((desktop (car desktops)))
	(insert (if (string= (car desktop)
			     desktop-menu-current-desktop-name)
		    "."
		  " ")
		(format "  %-30s %20s  %s"
			(car desktop)
			(desktop-menu-extra-desktop-description
			 (car (cdr desktop)))
			(car (cdr desktop))))
	(newline))
      (setq desktops (cdr desktops)))
    (backward-delete-char 1)
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (font-lock-fontify-buffer)
    (desktop-menu--set-window-height)
    (switch-to-buffer "*Desktop Menu*") ;; assert buffer *Desktop Menu*
    (beginning-of-buffer)
    (if (and (not (search-forward-regexp "^\\." nil t))
	     (not (eq (line-end-position) (point-max))))
	(next-line 1))
    (beginning-of-line)))

(defun desktop-menu-up (arg)
  "Move cursor vertically up ARG lines in Desktop Menu."
  (interactive "p")
  (if (> 0 arg)
      (desktop-menu-down (- arg))
    (let ((arg (mod arg (length desktop-menu-desktops)))
	  (lines (count-lines (point-min) (point))))
      (if (>= arg lines)
	  (desktop-menu-down (- (count-lines (point-min) (point-max))
				(1+ arg)))
	(previous-line arg)))))

(defun desktop-menu-down (arg)
  "Move cursor vertically down ARG lines in Desktop Menu."
  (interactive "p")
  (if (eq 0 (count-lines (point-min) (point))) ;; on first line
      (progn (next-line 1)
	     (setq arg (1- arg))))
  (if (> 0 arg)
      (desktop-menu-up (- arg))
    (let ((arg (mod arg (length desktop-menu-desktops)))
	  (lines (count-lines (point) (point-max))))
      (if (>= arg lines)
	  (desktop-menu-up (- (count-lines (point-min) (point-max)) arg 1))
	(next-line arg)))))

(defun desktop-menu-new (name)
  "Create a new desktop with name NAME."
  (interactive "sName of new desktop: ")
  (setq desktop-menu-desktops
	(append desktop-menu-desktops
		(list (list name
			    (desktop-menu-new-file desktop-menu-directory)))))
  (desktop-menu-list desktop-menu-directory)
  (message "Now you can save the new created desktop."))

(defun desktop-menu-refresh ()
  "Refresh Desktop Menu."
  (interactive)
  (desktop-menu-list desktop-menu-directory))

(defun desktop-menu-change-directory (directory)
  "Change to desktop list in directory DIRECTORY."
  (interactive "DChange to directory: ")
  (message "READ? %S %S" directory (file-readable-p directory))
  (unless (file-readable-p directory)
    (error "%S isn't a valid directory" directory))
  (desktop-menu-save-main-list desktop-menu-desktops
			       desktop-menu-directory)
  (setq desktop-menu-directory directory
	default-directory directory)
  (desktop-menu-list desktop-menu-directory t))

(defun desktop-menu-up-directory ()
  "Change to one upper directory respect to `desktop-menu-directory'."
  (interactive)
  (desktop-menu-change-directory
   (file-name-directory (directory-file-name desktop-menu-directory))))

(defun desktop-menu-rename (name)
  "Rename current lines desktop to name NAME."
  (interactive "sNew name: ")
  (setcar (desktop-menu-lines-desktop)
	  name)
  (desktop-menu-list desktop-menu-directory))

(defun desktop-menu-new-file (directory)
  "Create a new filename for a non existing file in DIRECTORY."
  (let ((n -1)
	(filenames (mapcar (function (lambda (e) (nth 1 e)))
			   desktop-menu-desktops))
	filename
	)
    (while (not filename)
      (setq n (1+ n))
      (let ((relative-filename (concat desktop-menu-base-filename
				       (if (eq 0 n) "" (int-to-string n)))))
	(setq filename
	      (and (not (file-exists-p (expand-file-name relative-filename
							 directory)))
		   (not (member relative-filename filenames))
		   relative-filename))))
    (concat desktop-menu-base-filename
	    (if (eq 0 n) "" (int-to-string n)))))

(defun desktop-menu-quit ()
  "Leave desktop menu and save current desktop list."
  (interactive)
  (bury-buffer (current-buffer))
  (desktop-menu-save-main-list desktop-menu-desktops
			       desktop-menu-directory)
  (set-window-configuration dtm--window-config-comming-from))

(defun desktop-menu-abort ()
  "Ding and leave desktop menu without saving current desktop list."
  (interactive)
  (ding)
  (bury-buffer (current-buffer))
  (set-window-configuration dtm--window-config-comming-from))

(defun desktop-menu-clear ()
  "Clear current desktop with `desktop-clear'."
  (interactive)
  (let ((desktop-clear-preserve-buffers (cons "*Desktop Menu*"
					      desktop-clear-preserve-buffers)))
    (desktop-clear)
    (set-window-configuration dtm--window-config-comming-from)))

(defun desktop-menu-select (&optional clear-p)
  "Load current lines desktop.
Optional argument CLEAR-P `ask', t, `yes' or `no'.
See function `desktop-menu-clear' for more explanation."
  (interactive)
  (let ((desktop-clear-preserve-buffers (cons "*Desktop Menu*"
					      desktop-clear-preserve-buffers))
	(desktop (desktop-menu-lines-desktop)))
    (setq clear-p (or clear-p desktop-menu-clear))
    (cond ((or (eq clear-p t) (eq clear-p 'yes))
	   (desktop-clear))
	  ((eq clear-p 'ask)
	   (if (y-or-n-p "Clear desktop? ")
	       (desktop-clear))))
    (set-window-configuration dtm--window-config-comming-from)
    (desktop-menu-load (expand-file-name (car (cdr desktop))
					 desktop-menu-directory))
    (setq desktop-menu-current-desktop-name (car desktop))))

(defun desktop-menu-delete ()
  "Delete current lines desktop.
Ask user respect variable `desktop-menu-ask-user-on-delete'."
  (interactive)
  (let ((desktop (desktop-menu-lines-desktop)))
    (when (or (not desktop-menu-ask-user-on-delete)
	      (y-or-n-p (format "Delete Desktop %S? " (car desktop))))
      (if (file-exists-p (car (cdr desktop)))
	  (delete-file (car (cdr desktop))))
      (setq desktop-menu-desktops
	    (remove desktop
		    desktop-menu-desktops))
      (desktop-menu-list desktop-menu-directory))))

(defun desktop-menu-merge ()
  "Load current lines desktop; don't clear current desktop."
  (interactive)
  (desktop-menu-select 'no))

(defun desktop-menu-save ()
  "Save current desktop into current lines desktop file."
  (interactive)
  (let ((desktop (desktop-menu-lines-desktop)))
    (desktop-menu-save-into (expand-file-name (car (cdr desktop))
					      desktop-menu-directory))
    (setq desktop-menu-current-desktop-name (car desktop))
    (desktop-menu-list desktop-menu-directory)
    (message "Save into desktop %s." (car desktop))))

(defun desktop-menu-lines-desktop ()
  "Return current lines desktop."
  (let ((line (1- (count-lines (point-min) (if (eobp) (point) (1+ (point)))))))
    (if (and (>= line 1)
	     (<= line (length desktop-menu-desktops)))
	(nth (1- line) desktop-menu-desktops)
      (error "You aren't on a desktop line"))))

(defun desktop-menu--set-window-height ()
  "Change the height of the selected window to suit the desktop list."
  (unless (one-window-p t)
    (shrink-window (- (window-height (selected-window))
		      ;; window-height in xemacs includes mode-line
		      (+ (if (string-match "XEmacs" (emacs-version)) 4 2)
			 (max 4 (length desktop-menu-desktops)))))))

(defun desktop-menu-help ()
  "Help for `desktop-menu-mode'."
  (interactive)
  (describe-function 'desktop-menu-mode))

(defun desktop-menu-extra-desktop-description (filename)
  "Return a desktop description string.
The string contains some informations about the saved
desktop in FILENAME."
  (let ((file (expand-file-name filename desktop-menu-directory)))
    (if (file-exists-p file)
	(save-excursion
	  (set-buffer (get-buffer-create "*Desktop Menu ttt*"))
	  (erase-buffer)
	  (insert-file file)
	  (goto-char (point-min))
	  (let ((time (if (search-forward-regexp "^;; Created \\(.*\\)" nil t)
			  (match-string 1)
			""))
		(count 0))
	    (while (search-forward-regexp "^(desktop-create-buffer" nil t)
	      (setq count (1+ count)))
	    (format "%2d Buffer %s" count time)))
      "empty")))

;; ----------------------------------------------------------------------------
;; Desktop Menu main functions
;; ----------------------------------------------------------------------------

;;;###autoload
(defun desktop-menu-customize ()
  "Customization of group `desktop-menu' for Desktop Menu."
  (interactive)
  (customize-group "desktop-menu"))

;;;###autoload
(defun desktop-menu-in (directory)
  "Make a menu of available desktops in directory DIRECTORY."
  (interactive "D")
  (setq dtm--window-comming-from (selected-window)
	dtm--window-config-comming-from (current-window-configuration))
  (let ((active-desktop-window nil))
    (walk-windows (function (lambda (window)
			      (if (string= (buffer-name (window-buffer window))
					   "*Desktop Menu*")
				  (setq active-desktop-window window)))))
    (if active-desktop-window
	(select-window active-desktop-window)
      (if (> (window-height (selected-window)) 7) ; can split
	  (split-window (selected-window)))
      (other-window 1)))
  (desktop-menu-list directory t))

;;;###autoload
(defun desktop-menu ()
  "Make a menu of available Emacs desktops in `desktop-menu-directory'."
  (interactive)
  (desktop-menu-in desktop-menu-directory))

(provide 'desktop-menu)


;;(global-set-key "\C-x\C-p" 'desktop-menu)
;;; desktop-menu.el ends here
