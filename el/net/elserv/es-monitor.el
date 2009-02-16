;;; es-monitor.el -- Elserv monitor.

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
(require 'mcharset)
(require 'elserv)

(defun elserv-monitor-function (result path ppath request)
  (let (paths content charset)
    (mapatoms
     (lambda (x)
       (setq paths (cons
		    (cons (symbol-name x) (symbol-value x))
		    paths)))
     elserv-buffer-publish-hash)
    (with-temp-buffer
      (setq paths (sort paths (lambda (x y) (string< (car x) (car y)))))
      (insert "<HTML><HEAD><TITLE>Elserv Monitor</TITLE></HEAD>\n"
	      "<BODY><H1>Elserv Monitor</H1>\n"
	      "<TABLE><TR><TD><i>Path</i></TD>"
	      "<TD><i>Content Description</i></TD><TD><i>Type</i></TD></TR>\n")
      (dolist (exp paths)
	(setq content
	      (concat
	       content
	       (if (or (not (eq (car (cdr exp)) 'elserv-service-function))
		       (nth 1 (cdr exp)))
		   (concat
		    "<TR><TD>"
		    "<A HREF=\"" (car exp) "\">" (car exp) "</A>"
		    "</TD><TD>"
		    (cond
		     ((eq (car (cdr exp)) 'elserv-service-function)
		      (concat (nth 1 (cdr exp))
			      " (Function: "
			      (symbol-name (nth 4 (cdr exp)))
			      ")</TD><TD>"
			      (or (nth 5 (cdr exp)) "-")
			      "</TD></TR>"))
		     ((eq (car (cdr exp)) 'elserv-service-directory)
		      (concat (nth 1 (cdr exp))
			      "(Directory)</TD><TD>-</TD></TR>"))
		     ((eq (car (cdr exp)) 'elserv-service-string)
		      (concat (nth 1 (cdr exp)) "(String)</TD><TD>"
			      (nth 5 (cdr exp)) "</TD></TR>"))))))))
      (insert content "</TABLE>"
	      "<HR>Powered by " (elserv-version)
	      "</HTML>")
      (setq charset (detect-mime-charset-region (point-min)(point-max)))
      (elserv-set-result-header
       result
       (list 'content-type
	     (concat "text/html; charset=" (symbol-name charset))))
      (elserv-set-result-body
       result (encode-mime-charset-string (buffer-string) charset)))))

(defun elserv-monitor-publish (process path)
  "Publish Monitor.
PROCESS is the elserv server process.
PATH is the path to publish Wiki content."
  (elserv-publish process path :function 'elserv-monitor-function
		  :description "Elserv Monitor"))

(provide 'es-monitor)

;;; es-monitor.el ends here
