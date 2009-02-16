;;; Saved through ges-version 0.3.3dev at 2003-01-23 13:17
;;; ;;; From:  Nevin Kapur <nevin@jhu.edu>
;;; ;;; Subject: bibfind.el 1.1a
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Thu, 23 Jan 2003 09:53:41 -0500
;;; ;;; Organization: Mathematical Sciences, The Johns Hopkins University

;;; Changes since last version:

;;; - Support for MRLookup and Zentralblatt added by Djalil CHAFAI.

;;; ------------------------------------------------------------------------
;;; bibfind.el --- Client to get BibTeX entries from the web

;; Copyright (C) 2002,2003 by Nevin Kapur

;; Author: Nevin Kapur <kapur@mts.jhu.edu>
;; Keywords: tex, www, wp
;; Created: May 2, 2002
;; Version: 1.1a
;; URL: http://www.mts.jhu.edu/~kapur/emacs/bibfind.el
;; Compatibility: Tested on XEmacs 21.1, 21.4, should work on Emacs
;; Supersedes: msn.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.



;;; Commentary:

;; This packages provides an Emacs Lisp interface to obtaining BibTeX
;; entries from an online source.  Currently supported sources are
;;
;; o MathSciNet <http://www.ams.org/mathscinet>
;; o Mathematics ArXive <http://front.math.ucdavis.edu/>
;; o MRLookup <http://www.ams.org/mrlookup>
;; o Zentralblatt <http://www.emis.de/ZMATH/>
;;
;; On invocation, the function bibfind queries the user for an author
;; and title and fetches the BibteX entries corresponding to the
;; search results. The result will be a bibtex-mode buffer with the
;; search results parsed out.

;; Note: Access to the some databases is restricted and subscription
;; is required.

;;; Usage:
;;
;; byte-compile-file and put it in your load-path. In your
;; initialization file:
;;
;; (autoload 'bibfind "bibfind" "Search for BibTeX entries on the web" t)
;;
;; M-x bibfind invokes the interface.

;; This packages supersedes msn.el.

;;; Todo:
;;
;; - Improve the search results presentation.

;;; Contributors:
;;
;;  - Carl Mueller <cmlr@math.rochester.edu>: cleaner version of
;;    bibfind-msn-fetch, originally msn-fetch
;;  - julius ross <julius.ross@ic.ac.uk>:
;;      o ArXive suggestions.
;;      o bugfixes
;;  - Djalil CHAFAI <djalil.chafai@laposte.net> 2003-01-21: 
;;      o experimental support for Zentralblatt
;;        http://www.emis.de/ZMATH/
;;      o experimental support for MRLookup 
;;        http://www.ams.org/mrlookup

;; $Id: bibfind.el,v 1.16 2003/01/22 19:22:42 nevin Exp $


;;; Code:
(require 'w3-forms)
(require 'url-cookie)
(require 'bibtex)

(defconst bibfind-version "1.1a"
  "bibfind version.")

(defvar bibfind-mrl-buffer "*MR Lookup Results*"
  "Buffer for MRLookup results.")

(defvar bibfind-msn-buffer "*MathSciNet Results*"
  "Buffer for MathSciNet results.")

(defvar bibfind-zbm-buffer "*ZentralblattMath Results*"
  "Buffer for ZentralblattMath results.")

(defvar bibfind-arxive-buffer "*ArXive Results*"
  "Buffer for ArXive results.")

(defvar bibfind-results-buffer "*Bibfind Results*"
  "Buffer for bibfind results.")

(defvar bibfind-use-font-lock t
  "Whether to fontify the results.")

(defconst bibfind-sources '("MRLookup" "MathSciNet" "ArXive"
			    "Zentralblatt" "All")
  "List of sources to search.  Currently supported sources:
MRLookup        MRLookup
MathSciNet      MathSciNet
ArXive          Mathematics ArXive
Zentralblatt    Zentralblatt Math
All             All sources")

;;;###autoload
(defun bibfind ()
  "Search for BibTeX entries on the web."
  (interactive)
  (let* ((author (read-string "Author: "))
	 (title (read-string "Title: "))
	 (completion-ignore-case t)
	 (source (completing-read "Source (TAB for completion): " 
				  (mapcar 'list bibfind-sources))))
    (cond
     ((and
       (string-match "^[ \t]*$" author)
       (string-match "^[ \t]*$" title))
      (error 
       "bibfind error: At least one of author or title must be given."))
     ((string= source "MRLookup")
      (bibfind-mrl author title))
     ((string= source "MathSciNet")
      (bibfind-msn author title))
     ((string= source "Zentralblatt")
      (bibfind-zbm author title))
     ((string= source "ArXive")
      (bibfind-arxive author title))
     (t
      (bibfind-mrl author title)
      (bibfind-msn author title)
      (bibfind-zbm author title)
      (bibfind-arxive author title)
      (message "C-x C-b for other results.")))))
				 

;; These functions are modified versions from Eric Marsden's
;; dictweb.el.

(defun bibfind-msn (author title)
  "Run the MathSciNet client."
  (pop-to-buffer bibfind-msn-buffer)
  (erase-buffer)
  (bibfind-msn-fetch author title)
  (bibfind-msn-wash)
  (bibfind-treat-buffer bibfind-msn-buffer))

(defun bibfind-msn-fetch (author title)
  "Fetch data and print it in the current buffer."
  (let* ((pairs `(("bdlback" . "r=1")
		  ("dr" . "all")
		  ("l" . "20")
		  ("pg3" . "TI")
		  ("s3" . ,title)
		  ("pg4" . "ICN")
		  ("s4" . ,author)
		  ("fn" . "130")
		  ("fmt" . "bibtex")
		  ("bdlall" . "Retrieve+All")))
	 (url-request-data (bibfind-form-encode pairs))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.ams.org/msnmain/MathSci")))

(defun bibfind-msn-wash ()
  "Wash the output returned."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "^@" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    ;; Handle the case when nothing matches
    (goto-char (point-min))
    (when (re-search-forward "No Records Selected" nil t)
      (replace-match "@Comment No matches")
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</title>" nil t)
      (delete-region (point-max) (match-beginning 0)))))

;; Zentralblatt Math support

(defun bibfind-zbm (author title)
  "Run the Zentrallblatt Math client."
  (pop-to-buffer bibfind-zbm-buffer)
  (erase-buffer)
  (bibfind-zbm-fetch author title)
  (bibfind-zbm-wash)
  (bibfind-treat-buffer bibfind-zbm-buffer))

(defun bibfind-zbm-fetch (author title)
  "Fetch data and print it in the current buffer."
  (let* ((pairs `(("ti" . ,title)
		  ("au" . ,author)
		  ("type" . "bibtex")
		  ("format" . "short")
		  ("maxdocs" . "20")))
	 (url-request-data (bibfind-form-encode pairs))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.emis.de/cgi-bin/zmen/ZMATH/en/quick.html")))

(defun bibfind-zbm-wash ()
  "Wash the output returned."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "<pre>@" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "^<pre>\\|^</pre>" nil t)
      (replace-match ""))
    ;; Handle the case when nothing matches
    (goto-char (point-min))
    (when (re-search-forward "There is no answer" nil t)
	  (delete-region (point-min) (point-max))
	  (insert "@Comment No matches"))))

;; MR Lookup support

(defun bibfind-mrl (author title)
  "Run the MR Lookup client."
  (pop-to-buffer bibfind-mrl-buffer)
  (erase-buffer)
  (bibfind-mrl-fetch author title)
  (bibfind-mrl-wash)
  (bibfind-treat-buffer bibfind-mrl-buffer))

(defun bibfind-mrl-fetch (author title)
  "Fetch data and print it in the current buffer."
  (let* ((pairs `(("s3" . ,author)
		  ("pg3" . "ICN")
		  ("s4" . ,title)
		  ("pg4" . "TI")
		  ("s5" . "")
		  ("pg5" . "JOUR")
;		  ("ipage" . "")
;		  ("fpage" . "")
;		  ("arg3" . "")
		  ("format" . "bibtex")
		  ("Search" . "Search")))
	 (url-request-data (bibfind-form-encode pairs))
	 (url-request-method "POST")
	 (url-request-extra-headers
	  '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.ams.org/mrlookup")))

(defun bibfind-mrl-wash ()
  "Wash the output returned."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "<pre>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward 
		"^<pre>\\|^</pre>\\|^<tr><td .*>\\|</td>\\|</tr>\\|<a href=.*?>.*</a>" nil t)
      (replace-match ""))
    ;; Handle the case when nothing matches
    (goto-char (point-min))
    (when (re-search-forward "Sorry, no documents found matching this search" nil t)
	  (delete-region (point-min) (point-max))
	  (insert "@Comment No matches"))))

;; ArXive support
(defun bibfind-arxive (author title)
  "Get ArXive entries for `author' and `title'."
    (pop-to-buffer bibfind-arxive-buffer)
    (erase-buffer)
    (bibfind-arxive-fetch author title)
    (bibfind-arxive-wash)
    (bibfind-treat-buffer bibfind-arxive-buffer))

(defun bibfind-arxive-fetch (author title)
  "Fetch data and print it in the current buffer."
  (let ((url (concat
	      "http://front.math.ucdavis.edu/search?"
	      "a="
	      (w3-form-encode-xwfu author)
	      "&"
	      "t="
	      (w3-form-encode-xwfu title)
	      "&"
	      "s=BibTeX")))
    (url-insert-file-contents url)))

(defun bibfind-arxive-wash ()
  "Clean up the ArXive buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (re-search-forward "<dt><pre>@" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward
	    "^<dt><pre>\\|^</pre></td>\\|<a href=.*?>\\|</a>"
	    nil t)
      (replace-match ""))
    ;; Handle the case when no matches are found
    (goto-char (point-min))
    (when (re-search-forward "Query not found" nil t)
      (replace-match "@Comment No matches")
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</title>" nil t)
      (delete-region (point-max) (match-beginning 0)))))


;; Utility functions
	 
(defun bibfind-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun bibfind-treat-buffer (buffer)
  "Treat buffer with bib entries."
  (with-current-buffer buffer
    (bibtex-mode)
    (when bibfind-use-font-lock
      (font-lock-fontify-buffer))
    (goto-char (point-min))
    (setq buffer-file-name nil)))


(provide 'bibfind)

;;; bibfind.el ends here
;;; ------------------------------------------------------------------------

