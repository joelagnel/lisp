;;; rss-helper.el - Utilities for using RSS files with Gnus

;; Copyright (C) 2002 Mark A. Hershberger.

;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 1.2
;; Created: 2002 Sep 24
;; Keywords: weblog rss headlines
;; URL: http://elisp.info/package/rss-helper/

;; This file is not yet part of GNU Emacs.

;; rss-helper.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; rss-helper.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; rss-helper.el implements functions to aid in the use of Emacs/Gnus
;; as an RSS reader/aggregator.

;; Currently rss-helper-gnus-subscribe is the only really interesting
;; function and you may want to bind a key to it, e.g:
;;
;;    (global-set-key "\C-crf" 'rss-helper-gnus-subscribe)
;;
;; Now, whenever you come across a weblog or news site for which
;; you what to subscribe, invoke rss-helper-gnus-subscribe and it
;; will find the RSS feed (using a partial implementation Mark
;; Pilgrim's (http://diveintomark.org/) Ultra-Liberal RSS Locator
;; algorithm) and subscribe you to it in Gnus.

;; NOTE: This file has the function nnrss-save-server-data
;; redefined so that any new subscriptions you make are saved
;; between sessions.

;; TODO:
;;   - Allow the user to pick from among multiple feeds any time
;;     they are found.  Currently, you are allowed to choose from
;;     among multiple fees only if the Syndic8 is contacted.
;;   - Find a way to bail early if Gnus is not up and running.
;;   - Offer to synchronize RSS subscriptions with Syndic8
;;     account so that a user can read news from multiple
;;     machines.
;;   - Integrate with nnrss.

(require 'nnrss)
(require 'xml-rpc)
(require 'xml)
(require 'w3)
(require 'gnus-group)

(defun rss-helper-rss-p (data)
  "Test if data is an RSS feed.  Simply ensures that the first
element is rss or rdf."
  (or (eq (caar data) 'rss)
      (eq (caar data) 'rdf:RDF)))

(defun rss-helper-rss-title-description (data)
  "Return the title of an RSS feed."
  (if (rss-helper-rss-p data)
      (list 
       (cons 'description (cddar (rss-helper-find-el 'description data)))
       (cons 'title (cddar (rss-helper-find-el 'title data))))))

(defun rss-helper-rsslink-p (el)
  "Test if the element we are handed is an RSS autodiscovery link."
  (and (eq (car-safe el) 'link)
       (string-equal (cdr (assoc 'rel (cadr el))) "alternate")
       (or (string-equal (cdr (assoc 'type (cadr el))) 
			 "application/rss+xml")
	   (string-equal (cdr (assoc 'type (cadr el))) "text/xml"))))

(defun rss-helper-get-rsslinks (data)
  "Extract the <link> elements that are links to RSS from the parsed data"
  (delq nil (mapcar 
	     (lambda (el)
	       (if (rss-helper-rsslink-p el) el))
	     (rss-helper-find-el 'link data))))

(defun rss-helper-fetch (url)
  "Fetch the url and put it in a the expected lisp structure."
  ;some CVS versions of url.el need this to close the connection quickly
  (let* ((url-request-extra-headers (list (cons "Connection" "close")))
	 (rss-helper-buf (get-buffer-create " *rss-helper-fetch*"))
	xmlform htmlform)
    (set-buffer rss-helper-buf)
    ;; bit o' work necessary for w3 pre-CVS and post-CVS
    (setq url-current-object (url-generic-parse-url
			      (let ((ret (mm-url-insert url)))
				(if (listp ret)
				    (car ret)
				  url))))
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))

;; Because xml-parse-region can't deal with anything that isn't
;; XML and w3-parse-buffer can't deal with some XML, we have to
;; parse with xml-parse-region first and, if that fails, parse
;; with w3-parse-buffer.  Yuck.  Eventually, someone should find out
;; why w3-parse-buffer fails to parse some well-formed XML and
;; fix it.

    (condition-case nil
	(setq xmlform (xml-parse-region (point-min) (point-max)))
      (error (setq htmlform (caddar (w3-parse-buffer (current-buffer))))))
    (kill-buffer rss-helper-buf)
    (if htmlform
	htmlform
      xmlform)))

(defmacro rss-helper-match-macro (base-uri item 
					   onsite-list offsite-list)
  `(cond ((or (string-match (concat "^" ,base-uri) ,item)
	       (not (string-match "://" ,item)))
	   (setq ,onsite-list (append ,onsite-list (list ,item))))
	  (t (setq ,offsite-list (append ,offsite-list (list ,item))))))

(defun rss-helper-order-hrefs (base-uri hrefs)
  "Given a list of HREFS, sort them using the following priorities:
  1. links ending in .rss
  2. links ending in .rdf
  3. links ending in .xml
  4. links containing the above
  5. offsite links

BASE-URI is used to determine the location of the links and
whether they are `offsite' or `onsite'."
  (let (rss-onsite-end  rdf-onsite-end  xml-onsite-end
	rss-onsite-in   rdf-onsite-in   xml-onsite-in
	rss-offsite-end rdf-offsite-end xml-offsite-end
	rss-offsite-in rdf-offsite-in xml-offsite-in)
    (mapcar (lambda (href)
	      (if (not (null href))
	      (cond ((string-match "\\.rss$" href)
		     (rss-helper-match-macro
		      base-uri href rss-onsite-end rss-offsite-end))
		    ((string-match "\\.rdf$" href)
		     (rss-helper-match-macro 
		      base-uri href rdf-onsite-end rdf-offsite-end))
		    ((string-match "\\.xml$" href)
		     (rss-helper-match-macro
		      base-uri href xml-onsite-end xml-offsite-end))
		    ((string-match "rss" href)
		     (rss-helper-match-macro
		      base-uri href rss-onsite-in rss-offsite-in))
		    ((string-match "rdf" href)
		     (rss-helper-match-macro
		      base-uri href rdf-onsite-in rdf-offsite-in))
		    ((string-match "xml" href)
		     (rss-helper-match-macro
		      base-uri href xml-onsite-in xml-offsite-in)))))
	    hrefs)
    (append 
     rss-onsite-end  rdf-onsite-end  xml-onsite-end
     rss-onsite-in   rdf-onsite-in   xml-onsite-in
     rss-offsite-end rdf-offsite-end xml-offsite-end
     rss-offsite-in rdf-offsite-in xml-offsite-in)))

(defun rss-helper-extract-hrefs (data)
  "Recursively extract hrefs from a page's source.  DATA should be
the output of xml-parse-region or w3-parse-buffer."
  (mapcar (lambda (ahref)
	    (cdr (assoc 'href (cadr ahref))))
	  (rss-helper-find-el 'a data)))

(defun rss-helper-discover-feed (url)
  "Given a page, find an RSS feed using Mark Pilgrim's
`Ultra-Liberal RSS Locator' (http://diveintomark.org/2002/08/15.html)."

  (let ((parsed-page (rss-helper-fetch url)))
;;    1. If this URL is the RSS, use it.
    (if (rss-helper-rss-p parsed-page)
	(let ((data (rss-helper-rss-title-description parsed-page)))
	  (list (assoc 'title data)
		(assoc 'description data)
		(cons  'href url)))
;;    2. Look for the <link rel="alternate"
;;    type="application/rss+xml" and use that if it is there.
      (let ((links (rss-helper-get-rsslinks parsed-page)))
	(if links
	    (let ((data (rss-helper-rss-title-description
			 (rss-helper-fetch
			  (cdr (assoc 'href (cadar links)))))))
	      (list (assoc 'title data)
		    (assoc 'description data)
		    (assoc 'href (cadar links))))
;;    3. Look for links on the site in the following order:
;;       - onsite links ending in .rss, .rdf, or .xml
;;       - onsite links containing any of the above
;;       - offsite links ending in .rss, .rdf, or .xml
;;       - offsite links containing any of the above

	  (let* ((base-uri (progn (string-match ".*://[^/]+/?" url)
				  (match-string 0 url)))
		 (hrefs (rss-helper-order-hrefs 
			 base-uri (rss-helper-extract-hrefs parsed-page)))
		 (rss-link nil))
	  (while (and (eq rss-link nil) (not (eq hrefs nil)))
	    (let ((href-data (rss-helper-fetch (car hrefs))))
	      (if (rss-helper-rss-p href-data)
		  (let ((data (rss-helper-rss-title-description
			       href-data)))
		    (setq rss-link (list (assoc 'title data)
					 (assoc 'description data)
					 (cons  'href url))))
		(setq hrefs (cdr hrefs)))))
	  (if rss-link rss-link

;;    4. Check syndic8
	    (rss-helper-find-rss-via-syndic8 url))))))))

(defun rss-helper-find-rss-via-syndic8 (url)
  "Query syndic8 for the RSS feeds it has for the URL."
  (let ((feedid (xml-rpc-method-call "http://www.syndic8.com/xmlrpc.php"
				     'syndic8.FindSites
				     url)))
    (if feedid
	(let* ((feedinfo (xml-rpc-method-call 
			 "http://www.syndic8.com/xmlrpc.php"
			 'syndic8.GetFeedInfo
			 feedid))
	      (urllist
	       (delq nil (mapcar (lambda (listinfo)
				   (if (string-equal 
					(cdr (assoc "status" listinfo))
					"Syndicated")
				       (cons (cdr (assoc "sitename" listinfo))
					     (list (cons 'title
							 (cdr (assoc 
							       "sitename" listinfo)))
						   (cons 'href
							 (cdr (assoc
							       "dataurl" listinfo)))))))
				   feedinfo))))
	  (if (> (length urllist) 1)
	      (let ((completion-ignore-case t)
		    (selection 
		     (mapcar (lambda (listinfo)
			       (cons (cdr (assoc "sitename" listinfo)) 
				     (string-to-int 
				      (cdr (assoc "feedid" listinfo)))))
			     feedinfo)))
		(cdr (assoc 
		       (completing-read
		       "Multiple Feeds found.  Select one: "
		       selection nil t) urllist)))
	  (cdar urllist))))))

(defun rss-helper-gnus-subscribe (&optional url)
  "Given a URL, discover if there is an RSS feed.  If there is,
use Gnus' new nnrss to subscribe to it."
  (interactive)
  (if (not url)
      (setq url (read-from-minibuffer "URL to Search for RSS: ")))
  (let ((rss-helper-feedinfo (rss-helper-discover-feed url)))
    (if rss-helper-feedinfo
	(let ((title (read-from-minibuffer "Title: " 
					   (cdr (assoc 'title 
						       rss-helper-feedinfo))))
	      (desc  (read-from-minibuffer "Description: " 
					   (cdr (assoc 'description
						       rss-helper-feedinfo))))
	      (href (cdr (assoc 'href rss-helper-feedinfo))))
	  (push (list title href desc)
		nnrss-group-alist)
	  (gnus-group-unsubscribe-group
	   (concat "nnrss:" title))
	  (nnrss-save-server-data nil))
      (error "No feeds found for %s" url))))

(defun rss-helper-find-el (tag data &optional found-list)
  "Find the all matching elements in the data."
  (if (listp data)
      (mapcar (lambda (bit)
		(if (car-safe bit)
		    (progn (if (eq tag (car bit))
			       (setq found-list
				     (append found-list
					     (list bit))))
			   (if (listp (car-safe (caddr bit)))
			       (setq found-list
				     (append found-list
					     (rss-helper-find-el
					      tag (caddr bit))))
			     (setq found-list
				   (append found-list
					   (rss-helper-find-el
					    tag (cddr bit))))))))
		data))
  found-list)

(defun rss-helper-head-contents (data)
  "Return a uniform structure representing the contents of the <head> argument"
  (let ((head (cddr (rss-helper-find-el 'head data))))
    (if (eq (length head) 1)
	(car head)
      head)))


(defun nnrss-save-server-data (server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name
	       (nnrss-translate-file-chars
		(concat "nnrss" (and server
				     (not (equal server ""))
				     "-")
			server ".el"))
	       nnrss-directory)))
    (let ((coding-system-for-write 'binary)
	  print-level print-length)
      (with-temp-file file
	(insert "(setq nnrss-group-alist '"
		(prin1-to-string nnrss-group-alist)
		")\n")
	(insert "(setq nnrss-server-data '"
		(prin1-to-string nnrss-server-data)
		")\n")))))


