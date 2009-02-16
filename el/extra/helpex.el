;;; Saved through ges-version 0.3.3dev at 2004-02-20 13:05
;;; ;;; From: Michael Schierl <schierlm-usenet@gmx.de>
;;; ;;; Subject: helpex.el 0.2 --- useful extensions for the C-h prefix
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Mon, 19 Jan 2004 20:25:56 +0100
;;; ;;; Reply-To: schierlm@gmx.de

;;; [1. application/emacs-lisp; helpex.el]

;;; helpex.el --- useful extensions for the C-h prefix

;; Copyright (C) 2003, 2004 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 16 November 2003
;; Keywords: C-h help info find function documentation keymap
;; Version: 0.2

(defconst helpel-version "0.2"
  "Version of helpex.el.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides additions to the C-h help prefix.

;; To install it, (auto)load this file and run (helpex-setup-keys).

;; This will add the following key bindings to the C-h prefix (As a
;; mnemonic, think about what the same commands with non-capital
;; letters do):

;; C-h A 	`apropos'
;; C-h D	`find-function'
;; C-h V	`find-variable'
;; C-h K	`find-function-on-key'
;; C-h 4 f	`find-function-other-window'
;; C-h 5 f	`find-function-other-frame'
;; C-h 4 k	`find-variable-other-window'
;; C-h 5 k	`find-variable-other-frame'
;; C-h L	`helpex-find-library'
;; C-h o	`helpex-locate-library'
;; C-h M	`helpex-show-keymap'

;; Additionally, it changes the behaviour of Info-goto-node, so that,
;; if it is called interactively, the entered string starts with `('
;; and does not contain any `)', info file names are auto-completed.

;; Example: If you press C-h i g ( e m TAB, it will---if you don't have
;; other info files starting with `em'---complete it to `(emacs)'.

;;; Bugs:

;; If you have non-info files in your info tree, you will get an error
;; message after completing one of those file names (you would get one
;; if you typed that name completely as well)

;; It is not possible to complete node names in other files (there is
;; some code below for doing that, but it is commented out since it
;; does not work).

;;; History:

;; i hate history ...

;;; Code:

(defcustom helpex-use-library-complete t
  "*Whether to use completion for `locate-library' and `find-library'.
This will slow down these commands as `load-path' must be scanned."
  :type 'boolean)

(defun helpex-library-at-point ()
  "Return the library name at or before point."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (skip-syntax-forward "_w")
      (let ((p (point)) name)
	(skip-syntax-backward "_w")
	(setq name (buffer-substring-no-properties p (point)))
	(if (string= name "") nil name)))))

(defun helpex-library-read (type)
  "Read and return a library name, defaulting to the one near point.
With a prefix arg, negate the value of `helpex-use-library-complete'.
TYPE is a string -- either \"Find\" or \"Locate\" -- used in the
prompt."
  (let ((symb (helpex-library-at-point))
	(enable-recursive-minibuffers t)
	(libcomp helpex-use-library-complete)
	val)
    (if (equal symb 0)
	(setq symb nil))
    (if current-prefix-arg
	(setq libcomp (not libcomp)))
    (setq val (if libcomp
		  (completing-read
		   (concat type " library"
			   (if symb
			       (format " (default %s)" symb))
			   ": ")
		   (mapcar (lambda (el) (list el))
			   (helpex-get-all-libraries))
		   nil t nil)
		(read-string  (concat type " library"
				      (if symb
					  (format " (default %s)" symb))
				      ": "))))
    (list (if (equal val "")
	      symb
	    val))))

(defun helpex-get-all-libraries ()
  "Return all names of libraries available in source code."
  (apply 'append
	 (mapcar (lambda (dir)
		   (let (df)
		     (condition-case nil
			 (setq df (directory-files dir nil "\.el\\'" t))
		       (error (setq df nil)))
		     (mapcar (lambda (file)
			       (substring file 0 (- (length file) 3))) df)))
		 load-path)))

;;;###autoload
(defun helpex-find-library (library)
  "Open the file containing LIBRARY."
  (interactive (helpex-library-read "Find"))
  (let* ((lib (if (symbolp library)
		  (symbol-name library)
		library))
	 (filename (locate-library (concat lib ".el"))))
    (if filename
	(let (current-prefix-arg)
	  (find-file filename))
      (error (concat "Library " lib " not found.")))))

;;;###autoload
(defun helpex-locate-library (library)
  "Show the precise file name of Emacs library LIBRARY.
This command uses `locate-library', so see there for more
information.  The only difference is that it uses completion when
called interactively."
  (interactive (helpex-library-read "Locate"))
  (locate-library library nil nil t))

(defun helpex-info-files-alist ()
  "Generate an alist of existing info files (with parentheses around)."
  (require 'info)
  (info-initialize)
  (let (rslt)
    (mapcar
     (lambda (ent)
       (if (and ent (not (member ent rslt)))
	   (setq rslt (append rslt (list  ent)))))
     (apply 'append (mapcar
		     (lambda (dir)
		       (let ((filelist (directory-files dir)))
			 (mapcar
			  (lambda (file)
			    (if (file-directory-p (concat dir file))
				nil
			      (let ((result file))
				(if (string-match "-[1-9][0-9]*$" result)
				    (setq result
					  (substring result 0
						     (match-beginning 0))))
				(mapc
				 (lambda (entry)
				   (let ((nm (car entry)))
				     (if (and
					  (> (length result) (length nm))
					  
					  (string= (substring result
							      (- (length nm)))
						   nm))
					 (setq result
					       (substring result 0
							  (- (length nm)))))))
				 Info-suffix-list)
				
				(list (concat "(" result ")")))))
			  filelist)))
		     
		     Info-directory-list)))
    rslt))
  
;;;###autoload
(defun helpex-show-keymap (map)
  "Show the keymap MAP (and its documentation)."
  (interactive
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 (val))
     (if (and (symbolp v) (boundp v) (keymapp (eval v)))
	 (setq v (symbol-name v))
       (setq v nil))
     (setq val (completing-read (if v
				    (format "Show keymap (default %s): " v)
				  "Show keymap: ")
				obarray
				#'(lambda (x)
				    (and (boundp x)
					 (keymapp (eval x))))
				t nil nil v))
     (list (if (equal val "")
	       (intern v)
	     (intern val)))))
  (if (keymapp (eval map))
      (with-output-to-temp-buffer "*Help*"
	(prin1 map)
	(princ " is a keymap.\n\n")
	(princ "Documentation:\n")
	(let ((doc (documentation-property map 'variable-documentation)))
	  (princ (or doc "not documented as a variable.")))
	(princ "\n\n")
	(princ (substitute-command-keys
		(concat "\\{" (symbol-name map) "}"))))
    (message "You did not specify a keymap")))

(defvar helpex-info-files nil)

(defun helpex-better-info-node-completion-mode ()
  "Enable better completion of interactive `Info-goto-node' command."
  (defadvice Info-read-node-name (around helpex-Info-read-node-name activate)
    (let ((helpex-info-files (helpex-info-files-alist)))
      ad-do-it))
  (defadvice Info-read-node-name-1 (around helpex-Info-read-node-name-1
					   activate)
    (let ((res (helpex-read-node-name-1 string predicate code)))
      (if (eq res 'old-value)
	  ad-do-it
	(setq ad-return-value res)))))

(defun helpex-read-node-name-1 (string predicate code)
  "Callback used for completing info file names.
For STRING, PREDICATE and CODE, see `completing-read',
`try-completion' and `all-completions'"
  (let ((no-completion (and (> (length string) 0) (eq (aref string 0) ?\()))
	(name-completion (string-match ")" string)))
    (cond
     ((not no-completion)
      'old-value)
     (name-completion
;;       ;; this code does not work, so I commented it out.
;;       (let ((prefix (substring string 0 (1+ (string-match ")" string))))
;; 	    (suffix (substring string (1+ (string-match ")" string))))
;; 	    Info-current-file-completions Info-read-node-completion-table)
;; 	(save-excursion
;; 	  (Info-goto-node (concat prefix "Top") "*helpex-info-hack*")
;; 	  (setq Info-read-node-completion-table
;; 		(mapcar (lambda (elem)
;; 			  (list (concat prefix (car elem))))
;; 			(Info-build-node-completions))))
;; 	(Info-read-node-name-1 suffix predicate code))
      'old-value
      )
     ((eq code nil)
      (try-completion string helpex-info-files predicate))
     ((eq code t)
      (all-completions string helpex-info-files predicate))
     ((eq code 'lambda)
      (assoc string helpex-info-files)))))

;;;###autoload
(defun helpex-setup-keys ()
  "Define keys with `C-h' prefix which are useful.
Of course you may use the functions in this file without binding them
to these keys.  Usually these commands use capital letters where the
`base' command use lowercase letters."
  (interactive)
  (helpex-setup-keys-internal)
  (helpex-better-info-node-completion-mode))

(defun helpex-setup-keys-internal ()
  "Define keys with `C-h' prefix which are useful.
Called by `helpex-setup-keys'."
  (define-key help-map (kbd "A") 'apropos)
  (define-key help-map (kbd "D") 'find-function)
  (define-key help-map (kbd "V") 'find-variable)
  (define-key help-map (kbd "L") 'helpex-find-library)
  (define-key help-map (kbd "o") 'helpex-locate-library)
  (define-key help-map (kbd "K") 'find-function-on-key)
  (define-key help-map (kbd "4 f") 'find-function-other-window)
  (define-key help-map (kbd "5 f") 'find-function-other-frame)
  (define-key help-map (kbd "4 k") 'find-variable-other-window)
  (define-key help-map (kbd "5 k")'find-variable-other-frame)
  (define-key help-map (kbd "M") 'helpex-show-keymap))

(provide 'helpex)

;;; helpex.el ends here

