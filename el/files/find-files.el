;;; find-files.el -- handle globing or regexp patterns for find-file
;;
;; Copyright (C) 2000-2003 by Robert Fenk
;;
;; Author:	Robert Fenk
;; Status:	Tested with XEmacs 21.4.13
;; Keywords:	find-file, glob pattern,convenience 
;; X-URL:       http://www.robf.de/Hacking/elisp
;;
;; $Id: find-files.el,v 1.15 2006-06-13 23:27:04 fenk Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting started:
;; 
;; To use the functions provided by this package you should put this file into
;; your load-path and add the following line to your .emacs file:
;;    (require 'find-files)
;; and you may rebind "C-x C-f" by also adding the following line 
;;    (global-set-key [(control x) (control f)] 'find-files-glob)
;;
;; Examples: To visit all elisp files in a directory enter the directory name
;; and "*.el".  e.g. "~/prog/xemacs/*.el".  With globing you can even open
;; matching files recursively by preceding the pattern with a "-r",
;; e.g. "~/prog/xemacs/-r*.el" will open all files matching *.el found in
;; "~/prog/xemacs" and its subdirectories.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History 
;;
;; $Log: find-files.el,v $
;; Revision 1.15  2006-06-13 23:27:04  fenk
;; Fixed find-files-find.
;;
;; Revision 1.14  2003/08/01 08:23:51  fenk
;; - Added and getting started and examples section
;; - Recursive is "-r" by default now.
;;
;; Revision 1.13  2003/07/30 15:09:57  fenk
;; Added old logs.
;;
;; Revision 1.12  2003/07/30 15:04:49  fenk
;; Added CVS Log keyword substitution to source.
;;
;; Revision 1.11  2003/07/30 14:51:59  fenk
;; - Open directories with dired by default (René Kyllingstad)
;; - Added some defcustoms replacing defvars
;; - Added recursively find-files-glob
;;
;; Revision 1.10  2003/05/20 15:32:19  fenk
;; - Removed email adresses
;; - Added note that this package is found at http://www.robf.de
;; - Added find-files-ignore-no-match
;;
;; Revision 1.9  2002/12/12 15:44:24  fenk
;; Typo fixes
;;
;; Revision 1.8  2001/12/27 12:04:05  fenk
;; Applied a patch from Reuben Thomas to allow a new file to be created
;; even when find-files-ignore-no-match is set, provided the filename
;; contains no metacharacters.
;;
;; Revision 1.7  2001/08/27 18:24:35  fenk
;; Glob pattern fix.
;;
;; Revision 1.6  2001/01/19 15:12:25  fenk
;; Reomoval of useless space
;;
;; Revision 1.5  2000/06/27 16:32:37  fenk
;; Fixed check for existence of function replace-in-string!
;; 
;; Files:  find-files.el
;;
;; Revision 1.4  2000/02/25 18:11:51  fenk
;; Honor XEmacs
;;
;; Revision 1.3  2000/02/04 16:48:38  fenk
;; - support for GNU Emacs
;; - fix handling of no match
;;
;; Revision 1.2  2000/01/20 18:51:35  fenk
;; Several bugfixes in the creation of the "glob pattern"
;;
;; Revision 1.1  2000/01/19 18:11:34  fenk
;; First version of find-files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup find-files nil
  "Finding files with glob patterns or regular expressions."
  :group 'find-file)

(defcustom find-files-ignore-no-match nil
  "*Setting this to t will cause complains about patterns matching no files."
  :group 'find-files
  :type 'boolean)

(defcustom find-files-open-dirs-with-dired t
  "*Setting this to t will cause directories to be opened with dired."
  :group 'find-files
  :type 'boolean)

(defcustom find-files-recursively-pattern "^-r"
  "*A regexp matched against the pattern indicating a recursively find."
  :group 'find-files
  :type '(choice (regexp :tag "Enabled" "^-R")
                 (const :tag "Disabled" nil)))

;; Emacs 20.3 seems to miss the function replace-in-string?
(if (not (functionp 'replace-in-string))
    ;; actually this is dired-replace-in-string slightly modified 
    (defun replace-in-string (string regexp newtext &optional literal)
      ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
      ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
      (let ((result "") (start 0) mb me)
	(while (string-match regexp string start)
	  (setq mb (match-beginning 0)
		me (match-end 0)
		result (concat result (substring string start mb) newtext)
		start me))
	(concat result (substring string start)))))

(defun find-files-glob-internal (filepattern codesys find-file-function)
  (setq filepattern (expand-file-name filepattern))
  
  (let* ((directory (file-name-directory filepattern))
         (fp (file-name-nondirectory filepattern))
         (recursively (string-match find-files-recursively-pattern fp))
         orig-fp globbed-fp filename strlist
         (files nil))
    (if recursively (setq fp (replace-match "" nil nil fp)))
    ;; create a regexp which acts like a glob pattern
    (setq fp (replace-in-string fp "\\." "\\." t))
    (setq fp (replace-in-string fp "\\+" "\\+" t))
    (setq orig-fp fp) ; note original fp with metacharacters escaped
    (setq fp (replace-in-string fp "\\?" "." t))
    (if (string-match "^\\*" fp) (setq fp (concat "[^.]" fp)))
    (setq fp (replace-in-string fp "\\*" ".*" t))
    (if (string-match "{\\([^}]+\\)}" fp)
	(progn
	  (setq strlist (substring fp (match-beginning 1) (match-end 1)))
	  (setq strlist (replace-in-string strlist "," "\\|" t))
	  (string-match "{\\([^}]+\\)}" fp)
	  (setq fp (replace-match (concat "\\(" strlist "\\)")
				  t t fp))))
    (setq globbed-fp fp)
    (setq fp (concat "^" fp "$"))
    
    ;; now get all matching files 
    (setq files (directory-files directory nil fp nil))
    (if recursively
        (progn
          (nconc files (directory-files directory nil nil 'dirs))
          (setq files (delete "." files)
                files (delete ".." files))))
    (if (not files)
	(if (or find-files-ignore-no-match
                (string-equal globbed-fp orig-fp))
	    (funcall find-file-function filepattern codesys)
	  (error "No matching files for `%s'!" filepattern)))
    (while files
      (setq filename (car files))
      (if directory (setq filename (concat directory filename)))
      (if (file-directory-p filename)
          (if recursively
              (find-files-glob-internal
               (concat filename "/" (file-name-nondirectory filepattern))
               codesys find-file-function)
            (if find-files-open-dirs-with-dired
                (funcall find-file-function filename)))
        (message "Reading %s" filename)
        (if (string-match "XEmacs" emacs-version)
            (funcall find-file-function filename codesys)
          (funcall find-file-function filename)))
      (setq files (cdr files)))))

(defun find-files-glob (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file))

(defun find-files-glob-other-frame (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file-other-frame))

(defun find-files-glob-other-window (filepattern &optional codesys)
  "Edit files matching glob pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-glob-internal filepattern codesys 'find-file-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-files-regexp-internal (filepattern codesys find-file-function)
  (let ((directory (file-name-directory filepattern))
	(fp (file-name-nondirectory filepattern))
	filename
	(files nil))
    (setq files (directory-files directory nil fp nil))
    (if (not files)
	(if find-files-ignore-no-match
	    (funcall find-file-function filepattern codesys)
	  (error "No matching files for `%s'!" filepattern)))
    (while files
      (setq filename (car files))
      (if directory (setq filename (concat directory filename)))
      (if (not (file-directory-p filename))
	  (progn (message "Reading %s" filename)
		 (if (string-match "XEmacs" emacs-version)
		     (funcall find-file-function filename codesys)
		   (funcall find-file-function filename))))
      (setq files (cdr files)))))

(defun find-files-regexp (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file))

(defun find-files-regexp-other-frame (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file-other-frame))

(defun find-files-regexp-other-window (filepattern &optional codesys)
  "Edit files matching regexp pattern FILEPATTERN.
Under XEmacs/Mule, optional second argument specifies the
coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive "FFind files matching: \nZCoding system: ")
  (find-files-regexp-internal filepattern codesys 'find-file-other-window))

(defun find-files-find-at-point (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (let ((s (point))
        (e (save-excursion
             (end-of-line)
             (point))))
    (find-file (buffer-substring s e (current-buffer)))))
  
(defun find-files-find (filepattern)
  "Edit files found by find matching FILEPATTERN."
  (interactive "FFind files matching: ")
  (let ((b (get-buffer-create "*find-files*"))
        (directory-abbrev-alist '(("./" . "")))
        s e)
    (switch-to-buffer b)
    (erase-buffer)
    (setq default-directory (file-name-directory filepattern))
    (shell-command (format "find . -name '%s'" filepattern) b)
    (goto-char (point-min))
    (while (not (eobp))
      (setq s (point))
      (end-of-line)
      (setq e (point))
      (insert (abbreviate-file-name (buffer-substring s e) t))
      (delete-region s e)
      (beginning-of-line)
      (next-line 1))
    (local-set-key [button2] 'find-files-find-at-point)))

(defun read-file-name-filtered (prompt filter &optional dir default must-match initial-contents history)
  ;;(completing-read 
  ;; TODO
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'find-files)
;;; find-files.el ends here
