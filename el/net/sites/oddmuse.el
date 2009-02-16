;;; oddmuse.el -- edit pages on an Oddmuse wiki using curl
;; Copyright 2006  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; A simple mode to edit pages on Oddmuse wikis using Emacs and a
;; command-line HTTP client such as curl.

;; Since text formatting rules depend on the wiki you're writing for,
;; the font-locking can only be an approximation.

;; Put this file in a directory on your `load-path' and 
;; add this to your init file:
;; (autoload 'oddmuse-edit "oddmuse" "Edit a page on an Oddmuse wiki." t)
;; And then use M-x oddmuse-edit to start editing.

;;; Code:

(defcustom oddmuse-wikis
  '(("TestWiki" . "http://www.emacswiki.org/cgi-bin/test")
    ("EmacsWiki" . "http://www.emacswiki.org/cgi-bin/emacs")
    ("CommunityWiki" . "http://www.communitywiki.org/cw")
    ("OddmuseWiki" . "http://www.oddmuse.org/cgi-bin/oddmuse"))
  "Alist mapping wiki names to URLs."
  :type '(repeat (cons (string :tag "Wiki")
		       (string :tag "URL")))
  :group 'oddmuse)

(defcustom oddmuse-username user-full-name
  "Username to use when posting.
Setting a username is the polite thing to do."
  :type '(string)
  :group 'oddmuse)

(defcustom oddmuse-password ""
  "Password to use when posting.
You only need this if you want to edit locked pages and you
know an administrator password."
  :type '(string)
  :group 'oddmuse)

(defvar oddmuse-get-command
  "curl --silent '%w?action=browse;raw=1;id=%t'"
  "Command to use for publishing pages.
It must print the page to stdout.

%w  URL of the wiki as provided by `oddmuse-wikis'
%t  URL encoded pagename, eg. HowTo, How_To, or How%20To")

(defvar oddmuse-post-command
  (concat "curl --silent"
	  " --form 'title=%t'"
	  " --form 'summary=%s'"
	  " --form 'username=%u'"
	  " --form 'password=%p'"
	  " --form 'text=<-'"
	  " '%w'")
  "Command to use for publishing pages.
It must accept the page on stdin.

%w  URL of the wiki as provided by `oddmuse-wikis'
%t  pagename
%s  summary
%u  username
%p  password")

(defvar oddmuse-link-pattern
  "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
  "The pattern used for finding WikiName.")

(defvar oddmuse-wiki nil
  "The current wiki.
Must match a key from `oddmuse-wikis'.")

(defvar oddmuse-page-name nil
  "Pagename of the current buffer.")

(define-derived-mode oddmuse-mode text-mode "Odd"
  "Simple mode to edit wiki pages.

Use \\[oddmuse-follow] to follow links. With prefix, allows you
to specify the target page yourself.

Use \\[oddmuse-post] to post changes. With prefix, allows you to
post the page to a different wiki.

Use \\[oddmuse-edit] to edit a different page. With prefix,
forces a reload of the page instead of just popping to the buffer
if you are already editing the page.

Customize `oddmuse-wikis' to add more wikis to the list.

\\{oddmuse-mode-map}"
  (font-lock-add-keywords
   nil
   '(("^ .+?$" . font-lock-comment-face)
     ("<\\(/?[a-z]+\\)" 1 font-lock-function-name-face)
     ("^[*#]\\([*#]+\\)" . font-lock-constant-face)
     ("^\\([*#]\\)[^*#]" 1 font-lock-builtin-face)))
  (font-lock-add-keywords
   nil
   (list (cons (symbol-value 'oddmuse-link-pattern)
	       'font-lock-keyword-face)))
  (font-lock-mode 1)
  (goto-address)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
	 ("pre" \n) ("tt") ("u")))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))

(autoload 'sgml-tag "sgml-mode" t)
(define-key oddmuse-mode-map (kbd "C-c C-t") 'sgml-tag)
(define-key oddmuse-mode-map (kbd "C-c C-o") 'oddmuse-follow)
(define-key oddmuse-mode-map (kbd "C-c C-c") 'oddmuse-post)

(defun oddmuse-edit (wiki pagename)
  "Edit a page on a wiki.
WIKI is the name of the wiki as defined in `oddmuse-wikis',
PAGENAME is the pagename of the page you want to edit.
Use a prefix argument to force a reload of the page."
  (interactive
   (list (completing-read "Wiki: " oddmuse-wikis
			  nil t oddmuse-wiki)
	 (read-from-minibuffer "Pagename: ")))
  (let ((name (concat wiki ":" pagename)))
    (if (and (get-buffer name)
	     (not current-prefix-arg))
	(pop-to-buffer (get-buffer name))
      (let ((command oddmuse-get-command)
	    (url (cdr (assoc wiki oddmuse-wikis)))
	    (buf (get-buffer-create name)))
	(setq command (replace-regexp-in-string "%w" url command t t)
	      command (replace-regexp-in-string "%t" pagename command t t))
	(shell-command command buf)
	(pop-to-buffer buf)
	(oddmuse-mode)
	(set (make-local-variable 'oddmuse-wiki) wiki)
	(set (make-local-variable 'oddmuse-page-name) pagename)))))

(autoload 'word-at-point "thingatpt")

(defun oddmuse-follow (arg)
  "Figure out what page we need to visit
and call `oddmuse-edit' on it."
  (interactive "P")
  (let (pagename)
    (if arg
	(setq pagename (read-from-minibuffer "Pagename: "))
      (setq pagename (word-at-point))
      (unless (and pagename
		   (string-match (concat "^" oddmuse-link-pattern "$")
				 pagename))
	(save-excursion
	  (let* ((pos (point))
		 (start (search-backward "[[" nil t))
		 (end (search-forward "]]" nil t)))
	    (when (or (not start)
		      (not end)
		      (< end pos))
	      (error "No link found at point"))
	    (setq pagename (buffer-substring (+ start 2) (- end 2)))))))
    (oddmuse-edit (or oddmuse-wiki
		      (read-from-minibuffer "URL: "))
		  pagename)))

(defun oddmuse-post (summary)
  "Post the current buffer to the current wiki.
The current wiki is taken from `oddmuse-wiki'."
  (interactive "sSummary: ")
  ;; when using prefix or on a buffer that is not in oddmuse-mode
  (when (or (not oddmuse-wiki) current-prefix-arg)
    (set (make-local-variable 'oddmuse-wiki)
	 (completing-read "Wiki: " oddmuse-wikis nil t)))
  (when (not oddmuse-page-name)
    (set (make-local-variable 'oddmuse-page-name)
	 (read-from-minibuffer "Pagename: " (buffer-name))))
  (let ((command oddmuse-post-command)
	(url (cdr (assoc oddmuse-wiki oddmuse-wikis))))
    (setq command (replace-regexp-in-string "%w" url command t t)
	  command (replace-regexp-in-string "%t" oddmuse-page-name command t t)
	  command (replace-regexp-in-string "%s" summary command t t)
	  command (replace-regexp-in-string "%u" oddmuse-username command t t)
	  command (replace-regexp-in-string "%p" oddmuse-password command t t))
    (shell-command-on-region (point-min) (point-max) command)))

(provide 'oddmuse)

;;; oddmuse.el ends here
