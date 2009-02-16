;;; ldap --- LDAP searching routines
;;
;; Copyright (c) 1997 Andrew J. Cosgriff
;;
;; Author: Andrew J Cosgriff <ajc@bing.wattle.id.au>
;; Creation Date : Wed May 21 13:54:57 1997
;; Version: 0.04
;; Keywords: ldap, search
;; X-RCS: !Id: ldap.el,v 1.7 1997/05/31 06:33:03 ajc Exp ajc !
;; URI: http://bing.bofh.asn.au/sw/emacs-lisp/
;;
;; This is not part of GNU Emacs.
;;
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
;; along with this program; if not, you can either send email to this
;; program's author (see above or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; This code is available from http://bing.bofh.asn.au/sw/emacs-lisp/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Since I'm currently working on testing an LDAP server, I needed
;; (well wanted) some routines to look up people on the server and
;; return me their email addresses.
;;
;; It currently relies on having the "ldapsearch" executable in your
;; path (it's part of the Uni of Michigan LDAP stuff, which you can
;; find at http://www.umich.edu/~rsug/ldap/)
;;
;;; TO DO:
;;
;; - Allow choosing of LDAP server at search time or search one after
;;   another.
;; - Allow searching on more than just CN, and look for more than just
;;   mail addresses (partly done, although we still only search for
;;   people and one of their attributes)
;; - Maybe write our own ldap connection routines ?
;; - Better error handling :P
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 0.01 - Initial version
;; 0.02 - Use ldap-host-info to allow use of more than one LDAP
;;        service.
;;      - Make ldap-do-search a bit more generic (you can now specify
;;        the attribute you're after, as well as the attribute name of
;;        the search key).
;; 0.03 - GPL'ed it.
;;      - Added completing read for ldap server when you pass a
;;        prefix-arg to the two interactive routines.
;; 0.04 - If there's more than one match, prompt the user to pick one.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Things we need
;;
(require 'backquote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Variables:
;;
(defvar ldap-host-info
  '((anubhav
     (comment . "Deeproot LDAP service")
     (host . "192.168.1.5")
     (port . "389")
     (maxhits . "200")))
  "List of ldap hosts and their properties.
Format is something like the following :



((mycompany
  (comment . \"MyCompany's LDAP service\")
  (host . \"ldap.company.com.au\")
  (port . \"389\")
  (maxhits . \"100\")
  (base-dn . \"o=My Company, c=AU\")))

the first line's \"mycompany\" is just a random identifier for use
with ldap-default-host.  Most of the others are optional (except of
course, the host !")

(defvar ldap-default-host 'anubhav
  "Default ldap host to use")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;;
;; High-level (user/interactive) functions
;;
(defun ldap-mail-search (name)
  "Do an LDAP search for NAME's email address and display in echo area.
With prefix arg, prompt for the LDAP host to search from."
  (interactive "sName to search for : ")
  (let ((host (if current-prefix-arg
		    (car (read-from-string
			  (completing-read "Host : "
					    (mapcar
					       (lambda (thing) (list (prin1-to-string (car thing))))
					         ldap-host-info)
					     nil t (prin1-to-string ldap-default-host)))))))
    (message "Performing LDAP Search for mail address...")
    (let ((addr (ldap-do-search "mail" name nil host)))
      (if (null addr)
	    (message "User not found")
	(let ((msg ""))
	    (mapcar
	        (lambda (thing)
		       (setq msg (concat msg (if (string-equal msg "") "" ", ") thing))) addr)
	    (message msg))))))
  
(defun insert-ldap-mail-search (name)
  "Do an LDAP search for NAME's email address and insert in buffer.
With prefix arg, prompt for the LDAP host to search from."
  (interactive "sName to search for : ")
  (let ((host (if current-prefix-arg
		    (car (read-from-string
			  (completing-read "Host : "
					    (mapcar
					       (lambda (thing) (list (prin1-to-string (car thing))))
					         ldap-host-info)
					     nil t (prin1-to-string ldap-default-host)))))))
    (message "Performing LDAP Search for mail address...")
    (let ((addr (ldap-do-search "mail" name nil host)))
      (if (null addr)
	    (message "User not found")
	(progn (insert
		(if (> (length addr) 1)
		        (completing-read "Which address ? : "
					      (mapcar (lambda (thing) (list thing)) addr) nil t (car addr))
		    (car addr)))
	              (message ""))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lower level functions
;;
(defun ldap-get-host-attr (attr &optional host)
  "Return attribute ATTR for HOST from the ldap info list"
  (cdr (assoc attr (assoc (if host host ldap-default-host) ldap-host-info)))
)

(defun ldap-do-search (attr-wanted searchkey &optional searchkeytype ldap-host)
  "Do an LDAP search.
ATTR-WANTED is the attribute we want returned, eg. \"mail\".
SEARCHKEY is pretty self-explanatory, eg. \"Andrew Cosgriff\".
Optional args -
 SEARCHKEYTYPE specifies the attribute type of the searchkey (if it isn't
the person's full name (which is normall called \"cn\")
 LDAP-HOST if you want a server other than the default.

Returns a list of matching things."
  (let ((ldap-search-buffer (get-buffer-create "*ldap-search*"))
	(host (ldap-get-host-attr 'host ldap-host))
	(port (ldap-get-host-attr 'port ldap-host))
	(base-dn (ldap-get-host-attr 'base-dn ldap-host))
	(maxhits (ldap-get-host-attr 'maxhits ldap-host))
	(arglist) (ldap-result)
	)
    (save-excursion
      (set-buffer ldap-search-buffer)
      (delete-region (point-min) (point-max))
      (if (not (or (equal nil host) (equal "" host)))
	    (setq arglist (nconc arglist (list (format "-h%s" host)))))
      (if (not (or (equal nil port) (equal "" port)))
	    (setq arglist (nconc arglist (list (format "-p%s" port)))))
      (if (not (or (equal nil base-dn) (equal "" base-dn)))
	    (setq arglist (nconc arglist (list (format "-b%s" base-dn)))))
      (if (not (or (equal nil maxhits) (equal "" maxhits)))
	    (setq arglist (nconc arglist (list (format "-z%s" maxhits)))))
      (setq ldap-result
	         (eval `(call-process "/var/easypush/ldap/bin/ldapsearch" nil ldap-search-buffer nil
				        ,@arglist
					  (format
					      "(&(objectclass=person)(%s=%s))"
					         (if searchkeytype searchkeytype "cn")
						    searchkey)
					    ;;
					    ;; Note - if using Four11, and we're
					    ;; after a CN, telling ldapsearch
					    ;; that we only want it to return
					    ;; the CN results in no output.
					    ;; Hmm.
					    ;;
					    attr-wanted)))

      (if (equal 0 ldap-result)
	    (let ((result))
	          (goto-char (point-min))
		      (forward-line)
		          (while (re-search-forward (concat "^" attr-wanted "=\\(.*\\)$") nil t)
			    (setq result (nconc result (list (match-string 1)))))
			      result)
	nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(provide 'ldap)
;;; ldap ends here


