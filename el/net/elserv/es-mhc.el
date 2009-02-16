;;; es-mhc.el -- Elserv interface for MHC.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: HTTP, Schedule

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

(require 'elserv)
(require 'mhc)
(require 'mhc-face)
(require 'mime-view)
(require 'custom)
;;; Code:
(eval-when-compile (require 'cl))

(defgroup elserv-mhc nil
  "Elserv interface for MHC."
  :group 'elserv
  :group 'mail)

(defcustom elserv-mhc-default-port 10000
  "*Default port for MHC."
  :type 'integer
  :group 'elserv-mhc)

(defcustom elserv-mhc-icon-publish-path elserv-icon-publish-path
  "*Path to publish an icon directory specified by `elserv-mhc-icon-path'."
  :type 'string
  :group 'elserv-mhc)

(defcustom elserv-mhc-icon-path elserv-icon-path
  "*Icon image file path."
  :type 'directory
  :group 'elserv-mhc)

(defcustom elserv-mhc-background-image "background.png"
  "*Background image file."
  :type 'string
  :group 'elserv-mhc)

(defcustom elserv-mhc-article-icon-image "article.png"
  "*Article icon image file."
  :type 'string
  :group 'elserv-mhc)

(defcustom elserv-mhc-icon-image-alist
  '(("Conflict"   . "Conflict.png")
    ("Private"    . "Private.png")
    ("Holiday"    . "Holiday.png")
    ("Todo"       . "CheckBox.png")
    ("Done"       . "CheckedBox.png")
    ("Link"       . "Link.png"))
  "*Alist to define icons.
Each element should have the form
 (NAME . ICON-FILE)
It defines icon named NAME created from ICON-FILE.
Example:
  '((\"Holiday\"     . \"Holiday.png\")
    (\"Work\"        . \"Business.png\")
    (\"Private\"     . \"Private.png\")
    (\"Anniversary\" . \"Anniversary.png\")
    (\"Birthday\"    . \"Birthday.png\")
    (\"Other\"       . \"Other.png\")
    (\"Todo\"        . \"CheckBox.png\")
    (\"Done\"        . \"CheckedBox.png\")
    (\"Conflict\"    . \"Conflict.png\"))"
  :group 'mhc
  :type '(repeat
	  :inline t
	  (cons (string :tag "Icon Name")
		(string :tag "Image File Name"))))

(defcustom elserv-mhc-todo-title-format "<h2>TODO(s) at %s/%s/%s</h2>"
  "*Todo title line format."
  :type 'string
  :group 'elserv-mhc)

(defcustom elserv-mhc-calendar-title-format "<h2>Calendar of %s/%s</h2>"
  "*Calendar titleline format."
  :type 'string
  :group 'elserv-mhc)

(defcustom elserv-mhc-calendar-cell-width 100
  "*width of the calender cell."
  :type 'integer
  :group 'elserv-mhc)

(defcustom elserv-mhc-calendar-cell-height 100
  "*height of the calender cell."
  :type 'integer
  :group 'elserv-mhc)

(defvar elserv-mhc/path nil)
(defvar elserv-mhc/icon-image-alist nil)

(defun elserv-mhc-publish-image (process ppath path file)
  (let ((file (expand-file-name file elserv-mhc-icon-path)))
    (when (file-exists-p file)
      (elserv-publish process
		      (expand-file-name path ppath)
		      :content-type
		      (elserv-mime-type file)
		      :string
		      (with-temp-buffer
			(insert-file-contents-literally
			 file)
			(buffer-string)))
      t)))

(defun elserv-mhc-icon-string (icon alt)
  (if (setq icon (assoc (downcase icon) elserv-mhc/icon-image-alist))
      (concat "<img src=\""
	      (expand-file-name (concat 
				 elserv-mhc-icon-publish-path "/"
				 (cdr icon))
				elserv-mhc/path)
	      "\" alt=\"" alt "\">")
    alt))

(defun elserv-mhc-make-todo-list (day category-predicate secret)
  (let ((schedules (mhc-db-scan-todo day))
	(mhc-tmp-day day)
	priority check deadline)
    (when schedules
      (insert (mhc-day-let day
		(format elserv-mhc-todo-title-format
			year month day-of-month)))
      (insert "<table>")
      (dolist (schedule schedules)
	(when (and (if (mhc-schedule-in-category-p schedule "done")
		       mhc-todo-display-done t)
		   (funcall category-predicate schedule))
	  (setq priority (mhc-schedule-priority schedule)
		check (mhc-schedule-in-category-p schedule "done")
		deadline (mhc-schedule-todo-deadline schedule))
	  (insert
	   "<tr>"
	   (if (mhc-schedule-priority schedule)
	       (format "<td>%s</td><td>"
		       (elserv-mhc-string-with-face
			(format "[%d]" priority)
			(cond
			 ((null priority) 'default)
			 ((>= priority 80) 'mhc-summary-face-sunday)
			 ((>= priority 50) 'mhc-summary-face-saturday))))
	     "<td></td><td>"))
	  (insert
	   (if check
	       (elserv-mhc-icon-string "done" "■")
	     (elserv-mhc-icon-string "todo" "□")))
	  (dolist (category (delete "todo"
				    (delete "done"
					    (copy-sequence
					     (mhc-schedule-categories
					      schedule)))))
	    (when (and category
		       (assoc (downcase category)
			      elserv-mhc/icon-image-alist))
	      (insert (elserv-mhc-icon-string category "○"))))
	  (insert
	   (elserv-mhc-string-with-face
	    (or (mhc-schedule-subject schedule) "")
	    (or (mhc-face-category-to-face
		 (car (mhc-schedule-categories schedule)))
		'default))
	   (elserv-mhc-string-with-face
	    (or (mhc-schedule-location schedule) "")
	    'mhc-summary-face-location)
	   (if (mhc-schedule-in-category-p schedule "done")
	       ""
	     (elserv-mhc-string-with-face
	      (or (and deadline
		       (if (mhc-date= deadline day)
			   mhc-todo-string-deadline-day
			 (let ((remaining (mhc-date- deadline day)))
			   (if (> remaining 0)
			       (format mhc-todo-string-remaining-day remaining)
			     (format mhc-todo-string-excess-day
				     (abs remaining)))))) "")
	      (or (and deadline
		       (if (> (mhc-date- deadline day) 0)
			   'mhc-summary-face-default
			 'mhc-summary-face-sunday)) 'default)))
	   (if (mhc-record-name (mhc-schedule-record schedule))
	       (concat
		"<A href=\""
		(if (string-match (regexp-quote (expand-file-name
						 mhc-mail-path))
				  (mhc-record-name (mhc-schedule-record
						    schedule)))
		    (expand-file-name
		     (substring (mhc-record-name (mhc-schedule-record
						  schedule))
				(+ 1 (match-end 0)))
		     elserv-mhc/path)
		  "")
		"\">"
		(if elserv-mhc-article-icon-image
		    (concat "<img src=\"" (expand-file-name
					   "article" elserv-mhc/path)
			    "\" alt=\"→\">")
		  "→")
		"</A>")
	     "")
	   "</td></tr>")))
      (insert "</table>"))))

(defun elserv-mhc-face-foreground (face)
  "Return foreground color name of FACE."
  (static-if (fboundp 'face-foreground-name)
      (face-foreground-name face)
    (face-foreground face)))

(defun elserv-mhc-face-background (face)
  "Return background color name of FACE."
  (static-if (fboundp 'face-background-name)
      (face-background-name face)
    (face-background face)))

(defun elserv-mhc-string-with-face (string face)
  (concat "<font style=\"color:" (or (elserv-mhc-face-foreground face)
				     (elserv-mhc-face-foreground 'default))
	  (if (and (elserv-mhc-face-background face)
		   (not (string= (elserv-mhc-face-background 'default)
				 (elserv-mhc-face-background face))))
	      (concat ";background-color:" (elserv-mhc-face-background face))
	    "")
	  "\">"
	  string
	  "</font>"))

(defun elserv-mhc-insert-dayinfo (dayinfo today category-predicate secret)
  (let* ((time-max -1)
	 (schedules (mhc-day-schedules dayinfo))
	 day-face begin end priority next-begin conflict)
    (setq day-face (cond ((mhc-schedule-in-category-p
			     (car schedules) "holiday")
			    'mhc-category-face-holiday)
			   ((eq (mhc-day-day-of-week dayinfo) 0)
			    'mhc-summary-face-sunday)
			   ((eq (mhc-day-day-of-week dayinfo) 6)
			    'mhc-summary-face-saturday)
			   (t 'mhc-summary-face-default)))
    (if (mhc-date= (mhc-day-date dayinfo) (mhc-date-now))
	(setq day-face (mhc-face-get-today-face day-face)))
    (insert (format "<td valign=\"top\" width=\"%s\" height=\"%s\">"
		    elserv-mhc-calendar-cell-width
		    elserv-mhc-calendar-cell-height)
	    (elserv-mhc-string-with-face
	     (number-to-string (mhc-day-day-of-month dayinfo))
	     day-face)
	    "<br>")
    (while schedules
      (setq begin (mhc-schedule-time-begin (car schedules))
	    end (mhc-schedule-time-end (car schedules))
	    priority (mhc-schedule-priority (car schedules))
	    next-begin (if (car (cdr schedules))
			   (mhc-schedule-time-begin
			    (car (cdr schedules))))
	    conflict (or (and end next-begin
			      (< next-begin end))
			 (and begin time-max
       			      (< begin time-max))))
      (insert
       (elserv-mhc-string-with-face
	(if begin
	    (format "%02d:%02d" (/ begin 60) (% begin 60))
	  "")
	'default)
       (elserv-mhc-string-with-face
	(if end
	    (format "-%02d:%02d" (/ end 60) (% end 60))
	  "")
	'default)
       (if conflict
	   (elserv-mhc-icon-string "conflict" "※") ""))
      (dolist (category
	       (if (mhc-schedule-in-category-p (car schedules) "done")
		   (delete "todo"
			   (copy-sequence
			    (mhc-schedule-categories
			     (car schedules))))
		 (mhc-schedule-categories (car schedules))))
	(insert (elserv-mhc-icon-string category "○")))
      (insert
       (elserv-mhc-string-with-face
	(or (mhc-schedule-subject (car schedules)) "")
	(mhc-face-category-to-face
	 (car (mhc-schedule-categories (car schedules)))))
       (if (and (mhc-schedule-location (car schedules))
		(> (length (mhc-schedule-location (car schedules))) 0))
	   (elserv-mhc-string-with-face
	    (concat "[" (mhc-schedule-location (car schedules)) "]")
	    'mhc-summary-face-location)
	 "")
       (elserv-mhc-string-with-face
	(if priority (format "(%d)" priority) "")
	'default)
       (if (mhc-record-name (mhc-schedule-record (car schedules)))
	   (concat
	    "<A href=\""
	    (if (string-match (regexp-quote (expand-file-name mhc-mail-path))
			      (mhc-record-name (mhc-schedule-record
						(car schedules))))
		(expand-file-name
		 (substring (mhc-record-name (mhc-schedule-record
					      (car schedules)))
			    (+ 1 (match-end 0)))
		 elserv-mhc/path)
	      "")
	    "\">"
	    (if elserv-mhc-article-icon-image
		(concat "<img src=\"" (expand-file-name
				       "article" elserv-mhc/path)
			"\" alt=\"→\">")
	      "→")
	    "</A>")
	 "")
       "<br>")
      (setq schedules (cdr schedules)))
    (insert "</td>")))

(defun elserv-mhc-make-calendar (from to today category-predicate secret)
  (let ((count 0))
    (dolist (dayinfo (mhc-db-scan from to))
      (when (zerop (% count 7))
	(insert "<tr>"))
      (elserv-mhc-insert-dayinfo dayinfo today category-predicate secret)
      (when (zerop (% (incf count) 7))
	(insert "</tr>")))))

(defun elserv-mhc-article-function (result path ppath request)
  (let (raw-buffer mime-view-ignored-field-list charset)
    (with-temp-buffer
      (insert-file-contents-as-binary
       (expand-file-name (concat "schedule" path) mhc-mail-path))
      (setq raw-buffer (current-buffer))
      (with-temp-buffer
	(mime-view-buffer raw-buffer (current-buffer))
	(setq charset (detect-mime-charset-region (point-min)(point-max)))
	(elserv-set-result-header
	 result
	 (list 'content-type
	       (concat "text/plain; charset=" (symbol-name charset))))
	(elserv-set-result-body
	 result (encode-mime-charset-string (buffer-string) charset))))))

(defun elserv-mhc-function (result path ppath request)
  (with-temp-buffer
    (let ((month (or (if (plist-get request 'body)
			 (cdr (assoc "month"
				     (elserv-url-decode
				      (plist-get request 'body))))
		       path)))
	  charset)
      (if (and (not (eq (length month) 0))
	       (eq (aref month 0) ?/))
	  (setq month (substring month 1)))
      (if (<= (length month) 1)
	  (setq month (mhc-date-now))
	(setq month (apply
		     (lambda (x y z) (mhc-date-new x y z))
		     (nconc (mapcar 'string-to-number
				    (split-string month "/"))
			    (list 1)))))
      (elserv-mhc-month month)
      (setq charset (detect-mime-charset-region (point-min)(point-max)))
      (elserv-set-result-header
       result
       (list 'content-type
	     (concat "text/html; charset=" (symbol-name charset))))
      (elserv-set-result-body
       result (encode-mime-charset-string (buffer-string) charset)))))

(defun elserv-mhc-month (month)
  (elserv-mhc-content month
		      mhc-default-category-predicate-sexp
		      'secret))

(defun elserv-mhc-path (day)
  (expand-file-name
   (format "%d/%d" (mhc-date-yy day) (mhc-date-mm day))
   elserv-mhc/path))

(defun elserv-mhc-content (date predicate secret)
  (let ((from  (mhc-date-mm-first date))
	(to    (mhc-date-mm-last date))
	(today (mhc-date-now)))
    (insert "<html><header><title></title></header><body style=\"color:"
	    (elserv-mhc-face-foreground 'default) ";background-color:"
	    (elserv-mhc-face-background 'default) "\""
	    (if (file-exists-p (expand-file-name
				elserv-mhc-background-image
				elserv-mhc-icon-path))
		(concat " background=\"" (expand-file-name
					  "background" elserv-mhc/path)
			"\"")
	      "")
	    " link=\"" (elserv-mhc-face-foreground 'mhc-summary-face-saturday)
	    "\" vlink=\"" (elserv-mhc-face-background
			   'mhc-summary-face-location)
	    "\">")
    (elserv-mhc-make-todo-list
     today predicate secret)
    (setq from (mhc-date- from (mhc-date-ww from)))
    (setq to (mhc-date+ to (- 6 (mhc-date-ww to))))
    (insert (format elserv-mhc-calendar-title-format
		    (mhc-date-yy date)
		    (mhc-date-mm date)))
    (insert "<a href=\""
	    (elserv-mhc-path (mhc-date- from 1))
	    "\"> [prev] </a>"
	    "<a href=\""
	    (elserv-mhc-path today)
	    "\"> [today] </a>"
	    "<a href=\""
	    (elserv-mhc-path (mhc-date+ to 1))
	    "\"> [next] </a>")
    (insert "<table border=1>"
	    "<tr>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'mhc-summary-face-sunday) "\">日</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'default) "\">月</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'default) "\">火</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'default) "\">水</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'default) "\">木</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'default) "\">金</font></td>"
	    "<td><font color=\""
	    (elserv-mhc-face-foreground 'mhc-summary-face-saturday) "\">土</font></td>"
	    "</tr>")
    (elserv-mhc-make-calendar
     from to today mhc-default-category-predicate-sexp secret)
    (insert "</table><hr>Powered by "
	    (elserv-version)
	    "</body></html>")))

(defun elserv-mhc-icon-setup (process path)
  (setq elserv-mhc/icon-image-alist
	(mapcar (lambda (x) (cons (downcase (car x))
				  (cdr x)))
		elserv-mhc-icon-image-alist)))

(defun elserv-mhc-publish (process path)
  "Publish MHC service.
PROCESS is the elserv server process.
PATH is the path to publish MHC content."
  (mhc-face-setup)
  (setq elserv-mhc/path path)
  (elserv-publish process path :function 'elserv-mhc-function
		  :description "MHC Calendar")
  (elserv-publish process
		  (expand-file-name "schedule" path)
		  :function 'elserv-mhc-article-function)
  (elserv-mhc-icon-setup process path)
  (elserv-mhc-publish-image process path "background"
			    elserv-mhc-background-image)
  (elserv-mhc-publish-image process path "article"
			    elserv-mhc-article-icon-image))
  
(defun elserv-mhc-start (&optional port)
  "Start MHC server on PORT."
  (interactive (if current-prefix-arg
		   (list (string-to-number (read-from-minibuffer "Port: ")))))
  (elserv-mhc-publish (elserv-start (or port elserv-mhc-default-port)) "/")
  (message "Access 'http://%s:%d/' to get your schedule."
	   (system-name) (or port elserv-mhc-default-port)))

(require 'product)
(product-provide (provide 'es-mhc) (require 'elserv))

;;; es-mhc.el ends here
