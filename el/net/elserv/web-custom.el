;;; web-custom.el -- Customization via web browser.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: Customize, WWW

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

;; This program requires Elserv -- Yet another HTTP server on Emacsen
;; which is available at: http://www.gohome.org/elserv/
;; Please install it first.

;;; History:
;;

(require 'elserv)
(require 'custom)
(require 'cus-edit)
(require 'mcharset)

;;; Code:
(defvar web-custom-default-port 8080
  "*Default port for web custom.")

(defun web-custom-group-body (group)
  (let ((members (custom-group-members group nil)))
    (concat
     "<H2> group " (symbol-name group) "</H2>"
     (mapconcat
      (lambda (member)
	(if (eq (nth 1 member) 'custom-group)
	    (web-custom-group-body (car member))
	  (web-custom-option-body (car member))))
      members
      ""))))

(defun web-custom-option-body (option)
  (let* ((type (custom-variable-type option))
	 (ptype (car type)))
    (concat "<H3>" (symbol-name option) "</H3>"
	    "<pre>"
	    (when (boundp option)
	      (if (eq (string-to-char
		       (documentation-property option 'variable-documentation))
		      ?*)
		  (substring
		   (documentation-property option 'variable-documentation) 1)
		(documentation-property option 'variable-documentation)))
	    "</pre>"
	    "<form action=\"/set\" method=POST>"
	    (cond
	     ((or (eq ptype 'string) (eq ptype 'regexp)
		  (eq ptype 'integer) (eq ptype 'symbol)
		  (eq ptype 'file) (eq ptype 'directory)
		  (eq ptype 'coding-system))
	      (concat
	       (symbol-name ptype) ": "
	       "<input type=\"text\" name=\""
	       (concat (symbol-name ptype) ":"
		       (symbol-name option)) "\" value=\""
		       (elserv-replace-in-string
			(or (cond
			     ((eq ptype 'integer)
			      (and (symbol-value option)
				   (number-to-string (symbol-value option))))
			     ((or (eq ptype 'symbol)
				  (eq ptype 'coding-system))
			      (symbol-name (symbol-value option)))
			     (t (symbol-value option)))
			    "")
			"\"" "&quot;")
		       "\" cols=\"60\">"))
	     ((eq ptype 'boolean)
	      (concat
	       "<select name=\"" (concat "boolean:" (symbol-name option))
	       "\"><option"
	       (and (symbol-value option) " selected")
	       ">ON<option"
	       (unless (symbol-value option) " selected")
	       ">OFF</select>"))
	     ((eq ptype 'face) ; ignore..
	      "cannot customize. sorry.")
	     (t
	      (concat
	       (prin1-to-string type) ":<br>"
	       "<textarea name=\""
	       (concat "sexp:" (symbol-name option))
	       "\" rows=\"10\" cols=\"60\">"
	       (with-temp-buffer (pp (and (boundp option)
					  (symbol-value option))
				     (current-buffer))
				 (buffer-string)) "</textarea>")))
	    "<input type=\"submit\" value=\"SET\"></form>")))

(defun web-custom-function (result path ppath request)
  (let (charset)
    (with-temp-buffer
      (insert "<html><head><title>Customize</title></head><body>"
	      (web-custom-group-body
	       (intern
		(if (plist-get request 'body)
		    (cdr (assoc "group"
				(elserv-url-decode (plist-get request 'body))))
		  (substring path 1))))
	      "</body></html>")
      (setq charset (detect-mime-charset-region (point-min)(point-max)))
      (elserv-set-result-header
       result
       (list 'content-type
	     (concat "text/html; charset=" (symbol-name charset))))
      (elserv-set-result-body
       result (encode-mime-charset-string (buffer-string) charset)))))

(defun web-custom-set-function (result path ppath request)
  (let ((options (elserv-url-decode (plist-get request 'body)))
	charset type value)
    (with-temp-buffer
      (insert
       "<html><head><title>Customization Completed</title></head><body>")
      (dolist (option options)
	(when (string-match "^\\([^:]+\\):\\(.*\\)$" (car option))
	  (setq type (intern (substring (car option) 0 (match-end 1))))
	  (setcar option (substring (car option) (match-beginning 2))))
	(setcdr option (decode-coding-string (cdr option) 'undecided))
	(cond
	 ((eq type 'integer)
	  (setq value (string-to-number (cdr option))))
	 ((eq type 'boolean)
	  (setq value (string= "ON" (cdr option))))
	 ((eq type 'sexp)
	  (setq value (read (cdr option))))
	 ((or (eq type 'coding-system)(eq type 'symbol))
	  (setq value (intern (cdr option))))
	 (t
	  (setq value (cdr option))))
	;; Set actual value!
	(set (intern (car option)) value)
	(insert "Value of `" (car option) "' is set as:<br>"
		"<pre>")
	(pp value (current-buffer))
	(insert "</pre>"))
      (insert "<a href=\"/\">top</a></body></html>")
      (setq charset (detect-mime-charset-region (point-min)(point-max)))
      (elserv-set-result-header
       result
       (list 'content-type
	     (concat "text/html; charset=" (symbol-name charset))))
      (elserv-set-result-body
       result (encode-mime-charset-string (buffer-string) charset)))))

(defun web-custom (&optional port)
  (interactive (if current-prefix-arg
		   (list (string-to-number (read-from-minibuffer "Port: ")))))
  (elserv-start (or port web-custom-default-port))
  (elserv-publish (elserv-find-process)
		  "/group"
		  :function 'web-custom-function)
  (elserv-publish (elserv-find-process)
		  "/set"
		  :function 'web-custom-set-function)
  (elserv-publish (elserv-find-process 8080)
		  "/"
		  :function
		  (lambda (result path ppath request)
		    (elserv-set-result-header
		     result '(content-type "text/html"))
		    (elserv-set-result-body
		     result
		     (concat
		      "<html><head><title>Customize</title></head>"
		      "<body><H2>Customize</H2>"
		      "<form action=\"/group\" method=\"POST\">"
		      "group: <input type=\"text\" name=\"group\"><br>"
		      "<input type=\"submit\" value=\"customize\"></form>"
		      "</body></html>"))))
  (message "Access 'http://%s:%d/' to customize."
	   (system-name) (or port web-custom-default-port)))

(provide 'web-custom)

;;; web-custom.el ends here
