;;; smime-ldap.el --- client interface to LDAP for Emacs

;; Copyright (C) 1998, 1999, 2000, 2005 Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <Oscar.Figueiredo@di.epfl.ch>
;; Maintainer: Arne Jist of attribute/value pairs."

;;; Commentary:

;; This file has a slightly changed implementation of Emacs 21.3's
;; ldap-search and ldap-search-internal from ldap.el. The changes are
;; made to achieve compatibility with OpenLDAP v2 and to make it
;; possible to retrieve LDAP attributes that are tagged ie ";binary".

;; When Gnus drops support for Emacs 21.x this file can be removed and
;; smime.el changed to

;;   - (require 'smime-ldap)   =>   (require 'ldap)
;;   - (smime-ldap-search ...) =>   (ldap-search ...)

;; If we are running in Emacs 22 or newer it just uses the build-in
;; version of ldap-search.

;;; Code:

(require 'ldap)

(defun smime-ldap-search (filter &optional host attributes attrsonly withdn)
  "Perform an LDAP search.
FILTER is the search filter in RFC1558 syntax.
HOST is the LDAP host on which to perform the search.
ATTRIBUTES are the specific attributes to retrieve, nil means
retrieve all.
ATTRSONLY, if non-nil, retrieves the attributes only, without
the associated values.
If WITHDN is non-nil, each entry in the result will be prepended with
its distinguished name WITHDN.
Additional search parameters can be specified through
`ldap-host-parameters-alist', which see."
  (interactive "sFilter:")
  (if (>= emacs-major-version 22)
      (ldap-search filter host attributes attrsonly)
    (or host
	(setq host ldap-default-host)
	(error "No LDAP host specified"))
    (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	    result)
      (setq result (smime-ldap-search-internal
		        (append host-plist
				    (list 'host host
					    'filter filter
					      'attributes attributes
					        'attrsonly attrsonly
						  'withdn withdn))))
      (if ldap-ignore-attribute-codings
	    result
	(mapcar (function
		  (lambda (record)
		       (mapcar 'ldap-decode-attribute record)))
		result)))))

(defun smime-ldap-search-internal (search-plist)
  "Perform a search on a LDAP server.
SEARCH-PLIST is a property list describing the search request.
Valid keys in that list are:
`host' is a string naming one or more (blank-separated) LDAP servers to
to try to connect to.  Each host name may optionally be of the form HOST:PORT.
`filter' is a filter string for the search as described in RFC 1558.
`attributes' is a list of strings indicating which attributes to retrieve
for each matching entry. If nil, return all available attributes.
`attrsonly', if non-nil, indicates that only attributes are retrieved,
not their associated values.
`base' is the base for the search as described in RFC 1779.
`scope' is one of the three symbols `sub', `base' or `one'.
`binddn' is the distinguished name of the user to bind as (in RFC 1779 syntax).
`passwd' is the password to use for simple authentication.
`deref' is one of the symbols `never', `always', `search' or `find'.
`timelimit' is the timeout limit for the connection in seconds.
`sizelimit' is the maximum number of matches to return.
`withdn' if non-nil each entry in the result will be prepended with
its distinguished name DN.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs."
  (let ((buf (get-buffer-create " *ldap-search*"))
	(bufval (get-buffer-create " *ldap-value*"))
	(host (or (plist-get search-plist 'host)
		    ldap-default-host))
	(filter (plist-get search-plist 'filter))
	(attributes (plist-get search-plist 'attributes))
	(attrsonly (plist-get search-plist 'attrsonly))
	(base (or (plist-get search-plist 'base)
		    ldap-default-base))
	(scope (plist-get search-plist 'scope))
	(binddn (plist-get search-plist 'binddn))
	(passwd (plist-get search-plist 'passwd))
	(deref (plist-get search-plist 'deref))
	(timelimit (plist-get search-plist 'timelimit))
	(sizelimit (plist-get search-plist 'sizelimit))
	(withdn (plist-get search-plist 'withdn))
	(numres 0)
	arglist dn name value record result)
    (if (or (null filter)
	        (equal "" filter))
	(error "No search filter"))
    (setq filter (cons filter attributes))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (if (and host
	              (not (equal "" host)))
	    (setq arglist (nconc arglist (list (format "-h%s" host)))))
      (if (and attrsonly
	              (not (equal "" attrsonly)))
	    (setq arglist (nconc arglist (list "-A"))))
      (if (and base
	              (not (equal "" base)))
	    (setq arglist (nconc arglist (list (format "-b%s" base)))))
      (if (and scope
	              (not (equal "" scope)))
	    (setq arglist (nconc arglist (list (format "-s%s" scope)))))
      (if (and binddn
	              (not (equal "" binddn)))
	    (setq arglist (nconc arglist (list (format "-D%s" binddn)))))
      (if (and passwd
	              (not (equal "" passwd)))
	    (setq arglist (nconc arglist (list (format "-w%s" passwd)))))
      (if (and deref
	              (not (equal "" deref)))
	    (setq arglist (nconc arglist (list (format "-a%s" deref)))))
      (if (and timelimit
	              (not (equal "" timelimit)))
	    (setq arglist (nconc arglist (list (format "-l%s" timelimit)))))
      (if (and sizelimit
	              (not (equal "" sizelimit)))
	    (setq arglist (nconc arglist (list (format "-z%s" sizelimit)))))
      (eval `(call-process ldap-ldapsearch-prog
			      nil
			         buf
				    nil
				       ,@arglist
				       "-tt"; Write values to temp files
				          "-x"
					     "-LL"
					;   ,@ldap-ldapsearch-args
					        ,@filter))
      (insert "\n")
      (goto-char (point-min))

      (while (re-search-forward "[\t\n\f]+ " nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))

      (if (looking-at "usage")
	    (error "Incorrect ldapsearch invocation")
	(message "Parsing results... ")
	(while (progn
		  (skip-chars-forward " \t\n")
		   (not (eobp)))
	    (setq dn (buffer-substring (point) (save-excursion
						        (end-of-line)
							       (point))))
	      (forward-line 1)
	        (while (looking-at (concat "^\\(\\w*\\)\\(;\\w*\\)?[=:\t ]+"
					        "\\(<[\t ]*file://\\)?\\(.*\\)$"))
		      (setq name (match-string 1)
			      value (match-string 4))
		          (save-excursion
			          (set-buffer bufval)
				        (erase-buffer)
					      (insert-file-contents-literally value)
					            (delete-file value)
						          (setq value (buffer-substring (point-min) (point-max))))
			      (setq record (cons (list name value)
						        record))
			          (forward-line 1))
		  (setq result (cons (if withdn
					  (cons dn (nreverse record))
				              (nreverse record)) result))
		    (setq record nil)
		      (skip-chars-forward " \t\n")
		        (message "Parsing results... %d" numres)
			  (1+ numres))
	(message "Parsing results... done")
	(nreverse result)))))

(provide 'smime-ldap)

;;; smime-ldap.el ends here
