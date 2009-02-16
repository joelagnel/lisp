;;; es-wiki.el -- Elserv interface for emacs-wiki.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: HTTP

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
;;

;;; Commentary:
;;

;;; Code:
(require 'emacs-wiki)
(require 'elserv)

;; Set action as relative path!
(defvar elserv-wiki-http-edit-form
  "
<form method=\"POST\" action=\"changewiki?post\">
  <textarea name=\"%PAGE%\" rows=\"25\" cols=\"80\">%TEXT%</textarea>
  <center>
    <input type=\"submit\" value=\"Submit changes\">
  </center>
</form>\n")

(defvar elserv-wiki-http-search-form
    "
<form method=\"GET\" action=\"searchwiki?get\">
  <center>
    Search for: <input type=\"text\" size=\"50\" name=\"q\" value=\"\">
    <input type=\"submit\" value=\"Search!\">
  </center>
</form>\n")

(defvar elserv-wiki-publishing-footer
  "
    <!-- Page published by Emacs Wiki ends here -->
    <div class=\"navfoot\">
      <hr>
      <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
	<tr>
	  <td width=\"33%\" align=\"left\">
	    <span class=\"footdate\">
	      <lisp>
		(if buffer-file-name
		    (concat
		     \"Updated: \"
		     (format-time-string emacs-wiki-footer-date-format
		      (nth 5 (file-attributes buffer-file-name)))
		     (and emacs-wiki-serving-p
			  (emacs-wiki-editable-p (emacs-wiki-page-name))
			  (concat
			   \" / \"
			   (emacs-wiki-link-href
			    (concat \"editwiki?\" (emacs-wiki-page-name))
			    \"Edit\")))))
	      </lisp>
	    </span>
	  </td>
	  <td width=\"34%\" align=\"center\">
	    <span class=\"foothome\">
	      <lisp>
		(concat
		 (and (emacs-wiki-page-file emacs-wiki-home-page t)
		      (not (emacs-wiki-private-p emacs-wiki-home-page))
		      (concat
		       (emacs-wiki-link-href emacs-wiki-home-page \"Home\")
		       \" / \"))
		 (emacs-wiki-link-href emacs-wiki-index-page \"Index\")
		 (and (emacs-wiki-page-file \"ChangeLog\" t)
		      (not (emacs-wiki-private-p \"ChangeLog\"))
		      (concat
		       \" / \"
		       (emacs-wiki-link-href \"ChangeLog\" \"Changes\"))))
	      </lisp>
	    </span>
	  </td>
	  <td width=\"33%\" align=\"right\">
	    <span class=\"footfeed\">
	      <lisp>
		(if emacs-wiki-serving-p
		    (concat
		     (emacs-wiki-link-href \"searchwiki?get\" \"Search\")
		     (and buffer-file-name
			  (concat
			   \" / \"
			   (emacs-wiki-link-href
			    (concat \"searchwiki?q=\" (emacs-wiki-page-name))
			    \"Referrers\")))))
	      </lisp>
	    </span>
	  </td>
	</tr>
        <tr>
          <td>Powered by<a href=\"http://www.gohome.org/elserv\"> <lisp>(elserv-version t)</lisp></a></td>
        </tr>
      </table>
    </div>
  </body>
</html>\n")

(defun elserv-wiki-interwiki-page ()
  (if emacs-wiki-interwiki-names
      (concat "- [["
	      (mapconcat 'car emacs-wiki-interwiki-names "]]\n- [[")
	      "]]\n")
    "No WikiNames"))

(defun elserv-wiki-render-page (result name)
  (cond ((string= name emacs-wiki-index-page)
	 (with-current-buffer (emacs-wiki-generate-index t t)
	   (emacs-wiki-replace-markup "Wiki Index")
	   (elserv-set-result-header
	    result
	    (list 'content-type "text/html; charset=iso-2022-jp"))
	   (elserv-set-result-body result (encode-coding-string
					   (buffer-string)
					   'iso-2022-jp))
	   (kill-buffer (current-buffer))))
	((string= name "WikiNames")
	 (with-temp-buffer
	   (insert (elserv-wiki-interwiki-page))
	   (emacs-wiki-replace-markup "WikiNames")
	   (elserv-set-result-header
	    result (list 'content-type "text/html"))
	   (elserv-set-result-body result (buffer-string))))
	(t
	 (let ((file (and (not (emacs-wiki-private-p name))
			  (cdr (assoc name (emacs-wiki-file-alist)))))
	       (system-time-locale "C")
	       (inhibit-read-only t))
	   (if (null file)
	       (signal 'elserv-file-not-found
		       (concat "Page not found"
			       (format "Wiki page %s not found" name)))
	     (with-temp-buffer
	       (let ((modified-time (nth 5 (file-attributes file))))
		 (insert-file-contents-as-binary file)
		 (decode-coding-region (point-min) (point-max) 'iso-2022-jp)
		 (setq buffer-file-name name)
		 (emacs-wiki-replace-markup name)
		 (set-buffer-modified-p nil)
		 (elserv-set-result-header
		  result
		  (list 'content-type "text/html; charset=iso-2022-jp"
			'last-modified (format-time-string "%a, %e %b %Y %T %Z"
							   modified-time)))
		 (elserv-set-result-body result
					 (encode-coding-string
					  (buffer-string)
					  'iso-2022-jp)))))))))

(defun elserv-wiki-edit-page (result page-name)
  (let ((emacs-wiki-http-edit-form elserv-wiki-http-edit-form))
    (if (not (emacs-wiki-editable-p page-name))
	(signal 'elserv-forbidden "Editing this Wiki page is not allowed")
      (with-temp-buffer
	(emacs-wiki-setup-edit-page page-name)
	;; this is required because of the : in the name
	(elserv-set-result-header
	 result
	 (list 'content-type "text/html; charset=iso-2022-jp"))
	(elserv-set-result-body result
				(encode-coding-string
				 (buffer-string)
				 'iso-2022-jp))))))

(defun elserv-wiki-change-page (result request)
  (let* ((res (elserv-url-decode (plist-get request 'body)))
	 (page (caar res))
  	 (text (cdar res))
	 (len (length text))
	 (require-final-newline t)
	 (pos 0) illegal user)
    (if (not (emacs-wiki-editable-p page))
	(signal 'elserv-forbidden
		(format "Editing Wiki page %s is not allowed" page)))
    (while (and (null illegal)
		(setq pos (string-match "<\\s-*\\([^> \t]+\\)"
					text pos)))
      (setq pos (match-end 0))
      (if (assoc (match-string 1 text) emacs-wiki-dangerous-tags)
	  (setq illegal (match-string 1 text))))
    (if illegal
	(signal 'elserv-forbidden
		(format "Public use of &lt;%s&gt; tag not allowed"
			illegal)))
    (emacs-wiki-find-file page)
    (if (setq user (file-locked-p buffer-file-name))
	(signal 'elserv-forbidden
		(format
		 "The page \"%s\" is currently being edited by %s."
		 page (if (eq user t) (user-full-name) user))))
    (let ((inhibit-read-only t)
	  (delete-old-versions t))
      (erase-buffer)
      (insert (if (eq (aref text (1- len)) ?%)
		  (substring text 0 (1- len))
		text))
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match "" t t))
      (save-buffer)
      (if (/= (file-modes buffer-file-name) ?\666)
	  (set-file-modes buffer-file-name ?\666))
      (kill-buffer (current-buffer)))
    (with-temp-buffer
      (emacs-wiki-file-alist)	; force re-check
      (insert "<redirect url=\"wiki?" page "\" delay=\"3\">")
      (insert "Thank you, your changes have been saved to " page)
      (insert ".  You will be redirected to "
	      "the new page in a moment.")
      (insert "</redirect>")
      (emacs-wiki-replace-markup "Change Saved")
      (elserv-set-result-header result
				'(content-type "text/html"))
      (elserv-set-result-body result (buffer-string)))))

(defun elserv-wiki-search-input-page (result)
  (let ((emacs-wiki-http-search-form elserv-wiki-http-search-form))
    (with-temp-buffer
      (insert "<verbatim>" emacs-wiki-http-search-form "</verbatim>")
      (emacs-wiki-replace-markup "Search Wiki Pages")
      (elserv-set-result-header
       result
       (list 'content-type "text/html"))
      (elserv-set-result-body result (buffer-string)))))

(defun elserv-wiki-search-page (result term)
  (let ((compilation-scroll-output nil))
    (with-current-buffer (emacs-wiki-grep term)
      (emacs-wiki-wikify-search-results term)
      (emacs-wiki-replace-markup "Search Results")
      (elserv-set-result-header
       result
       (list 'content-type "text/html;charset=iso-2022-jp"))
      (elserv-set-result-body result
			      (encode-coding-string
			       (buffer-string) 'iso-2022-jp))
      (kill-buffer (current-buffer)))))
  
(defun elserv-wiki-function (result path ppath request)
  (if (string= path "")
      (elserv-make-redirect result 
			    (concat "http://" (plist-get request 'host)
				    (unless (string= ppath "/") ppath)
				    path "/"))
    (let ((emacs-wiki-serving-p t)
	  (emacs-wiki-publishing-footer elserv-wiki-publishing-footer))
      (cond
       ((string= path "/") ; default page.
	(elserv-wiki-render-page result emacs-wiki-default-page))
       ((string-match "\\`/wiki\\?\\(.+\\)" path)
	(elserv-wiki-render-page result (match-string 1 path)))
       ((string-match "\\`/editwiki\\?\\(.+\\)" path)
	(elserv-wiki-edit-page result (match-string 1 path)))
       ((string-match "\\`/changewiki\\?post" path)
	(elserv-wiki-change-page result request))
       ((string-match "\\`/searchwiki\\?get" path)
	(elserv-wiki-search-input-page result))
       ((string-match "\\`/searchwiki\\?q=\\(.+\\)" path)
	(elserv-wiki-search-page result (match-string 1 path)))
       (t (signal 'elserv-file-not-found
		  "Specified Wiki page was not found."))))))

(defun elserv-wiki-publish (process path)
  "Publish Wiki service.
PROCESS is the elserv server process.
PATH is the path to publish Wiki content."
  (elserv-publish process path :function 'elserv-wiki-function
		  :description "Emacs Wiki."))

(defun elserv-wiki-start (&optional port)
  "Start a Wiki Server."
  (interactive (if current-prefix-arg
		   (list (string-to-number (read-from-minibuffer "Port: ")))))
  (elserv-publish (elserv-start port) "/" :function 'elserv-wiki-function))

(require 'product)
(product-provide (provide 'es-wiki) (require 'elserv))

;;; es-wiki.el ends here
