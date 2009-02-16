;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lispdir.el --- Lisp code directory formatter and apropos
;; Authors         : Ashwin Ram (Ram-Ashwin@cs.yale.edu)
;;                 ; Dave Sill (dsill@relay.nswc.navy.mil)
;;                 ; David Lawrence (tale@pawl.rpi.edu)
;; Created On      : Wed Jan 25, 1989
;; Last Modified By: k30b
;; Last Modified On: Tue Jun 27 15:49:12 1989
;; Update Count    : 13
;; Status          : No known bugs.
;; Version         : 3.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History 		
;; 27-Jun-1989		dsill
;;    Added support for "archive" field containing anonymous FTP location.

;; 28-Feb-1989		dsill	
;;    Changed format-lcd-line-Sill to be smart about GNU-distributed code.
;;    Changed format-lcd-line-Sill to take advantage of 12-char max name.

;; 22-Feb-1989		dsill	
;;    Changed format-lisp-code-directory and lisp-dir-apropos to call the
;;      line formatter indirectly.  The variable 
;;      format-lisp-code-directory-line contains a function to format a single
;;      line, and format-lcd-line-Ram, format-lcd-line-tale, and
;;      format-lcd-line-Sill are the three possibilities at this time.

;; 20-Feb-1989		tale	
;;    changed file's name to lispdir.el
;;    format-lisp-code-directory makes separate buffer
;;    removed lisp-dir-apropos-buffer -- why use more space in memory?
;;    added lisp-dir-[apropos-]hook
;;      (I like (setq lisp-dir-hook 'delete-other-windows))
;;    other aesthetic changes

;; 16-Feb-1989		dsill	
;;    Added lisp-dir-apropos function

(require 'picture)			;provides move-to-column-force

(defvar lisp-code-directory "~/emacs/lisp/LCD-datafile"
  "*Database of free lisp code.  Entries are in the form:
Name|Author|Contact|Description|Date|Version|Archive")

(defvar format-lisp-code-directory-line 'format-lcd-line-Sill
  "*Function that formats one line of GNU Emacs Lisp code directory.\n
Provided as a variable for customizability.  Should not insert
final newline.")

(defvar lisp-code-directory-header 'lcd-header-Sill
  "*Function that inserts header appropriate for 
format-lisp-code-directory-line.")

(defvar elisp-archive-host "tut.cis.ohio-state.edu"
  "*Site of elisp archives available via anonymous ftp.")

(defvar elisp-archive-directory "pub/gnu/emacs/elisp-archive/"
  "*Root directory of elisp archives on elisp-archive-host.")

(defun format-lisp-code-directory ()
   "Convert GNU Emacs Lisp code directory into something a human could read.
Calls value of lisp-dir-hook with no args if that value is non-nil."
   (interactive)
   (pop-to-buffer "*Lisp Code Directory*")
   (fundamental-mode)
   (setq buffer-read-only nil)
   (erase-buffer)
   (buffer-flush-undo (current-buffer))
   (insert-file lisp-code-directory)
   (insert " GNU Emacs Lisp code directory.  " (current-time-string) ".\n\n")
   (message "Formatting %s ..." lisp-code-directory)
   (delete-region (progn (beginning-of-line) (point))
		  (progn (end-of-line) (point)))
   (funcall lisp-code-directory-header)
   (while (re-search-forward
	   "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
            (author (buffer-substring (match-beginning 2) (match-end 2)))
            (contact (buffer-substring (match-beginning 3) (match-end 3)))
            (description (buffer-substring (match-beginning 4) (match-end 4)))
            (date (buffer-substring (match-beginning 5) (match-end 5)))
            (version (buffer-substring (match-beginning 6) (match-end 6)))
	    (archive (buffer-substring (match-beginning 7) (match-end 7))))
       (delete-region (progn (beginning-of-line) (point))
	(progn (end-of-line) (point)))
       (funcall format-lisp-code-directory-line
	name author contact description date version archive)))
   (goto-char (point-min))
   (center-line)
   (message "Formatting %s ... done" lisp-code-directory)
   (set-buffer-modified-p nil)
   (run-hooks 'lisp-dir-hook))

(defun lisp-dir-apropos (topic)
  "Display entries in Lisp Code Directory for TOPIC in separate window.
Calls value of lisp-dir-apropos-hook with no args if that value is non-nil."
  (interactive (list
		(read-string
		 (concat "Lisp Directory apropos (" (current-word) "): "))))
  (if (equal "" topic) (setq topic (current-word)))
  (save-excursion
    (set-buffer (get-buffer-create "*Lisp Directory Apropos*"))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (insert-file lisp-code-directory)
    (message "Searching for %s ..." topic)
    (delete-non-matching-lines topic)
    (insert "Emacs Lisp Code Apropos -- \"" topic "\"\n\n\n")
    (backward-char 1)
    (funcall lisp-code-directory-header)
    (while (re-search-forward
	    "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
	    (author (buffer-substring (match-beginning 2) (match-end 2)))
	    (contact (buffer-substring (match-beginning 3) (match-end 3)))
	    (description (buffer-substring (match-beginning 4) (match-end 4)))
	    (date (buffer-substring (match-beginning 5) (match-end 5)))
	    (version (buffer-substring (match-beginning 6) (match-end 6)))
	    (archive (buffer-substring (match-beginning 7) (match-end 7))))
	(delete-region (progn (beginning-of-line) (point))
		       (progn (end-of-line) (point)))
	(funcall format-lisp-code-directory-line
	 name author contact description date version archive)))
    (goto-char (point-min))
    (center-line)
    (message "Searching for %s ... done" topic)
    (set-buffer-modified-p nil))
  (display-buffer "*Lisp Directory Apropos*")
  (run-hooks 'lisp-dir-apropos-hook))

(defun format-lcd-line-Ram
  (name author contact description date version archive)
  "Columnar formatter for Lisp code directory that tries to use as few lines
as possible.  Doesn't fit Contact within first 80 columns."
   (insert-at-column 1  name)
   (insert-at-column 17 description)
   (insert-at-column 49 author)
   (insert-at-column 65 date)
   (insert-at-column 74 "/")
   (insert-at-column 75 version)
   (insert-at-column 84 contact))

(defun format-lcd-line-tale
  (name author contact description date version archive)
  "Multi-line columnar formatter for Lisp code directory that tries not
to write anything past column 79."
   (insert-at-column 0  name)
   (insert-at-column 17 description)
   (insert-at-column 56 author)
   (insert-at-column 4  contact)
   (insert-at-column 56 date)
   (insert-at-column 72 version))

(defun format-lcd-line-Sill
  (name author contact description date version archive)
  "Multi-line non-columnar line formatter for Lisp code directory."
  (insert-at-column 0 name)
  (if (not (equal version ""))
      (insert " (" version ")"))
  (insert-at-column 18 date)
  (insert "\n")
  (if (and (string-match "[0-9]+\.[0-9]+ dist" contact)
	   (equal author "FSF"))
      (insert-at-column 5 contact)
    (progn
      (insert-at-column 5 author)
      (insert ", <" contact ">\n")
      (if (not (equal archive ""))
	  (progn
	    (if (string-match "~" archive)
		(setq archive (concat elisp-archive-host ":" elisp-archive-directory
				      (substring archive 2))))
	    (insert-at-column 5 archive)))))
  (insert-at-column 5 description))

(defun lcd-header-Ram/tale ()
  "Inserts header for column-formatted Lisp code directory."
  (funcall format-lisp-code-directory-line
    "Name" "Author" "Contact" "Description" "Date" "Version")
  (insert "\n")
  (insert-char ?- 79)
)

(defun lcd-header-Sill ()
  "Inserts empty header for non-columnar Lisp code directory"
)

(defun insert-at-column (col string)
   (if (> (current-column) col) (insert "\n"))
   (move-to-column-force col)
   (insert string))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg end)
	 (re-search-backward "\\w" nil 2)
	 (re-search-backward "\\b" nil 2)
	 (setq beg (point))
	 (re-search-forward "\\w*\\b" nil 2)
	 (setq end (point))
	 (buffer-substring beg end))))

(provide 'lispdir)
