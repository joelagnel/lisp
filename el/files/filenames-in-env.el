;;;; filenames-in-env.el -- put environment variables into filenames where possible
;;; Time-stamp: <2006-05-01 11:54:57 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'filenames-in-env)

;; todo: look at using abbreviate-file-name and directory-abbrev-alist for this

(defvar reverse-environment-variables '("HOME" "COMMON" "GATHERED")
  "Environment variables that we want unsubstitute-in-file-name to unsub.")

(defvar reverse-env-directories-alist nil
  "Directories given in known environment variables, as an alist.")

(defun match-any-initial-string (string alist)
  "Return whether STRING occurs as the start of the car of any pair in ALIST.
The actual value is the pair concerned."
  (catch 'found
    (let ((string-length (length string)))
      (while alist
	(let* ((pair (car alist))
	       (this (car pair))
	       (this-length (length this)))
	  (if (and (>= string-length this-length)
		   (string= (substring string 0 this-length)
			    this))
	      (throw 'found pair))
	  (setq alist (cdr alist))))
      nil)))

(defun ensure-reverse-env-directories-alist ()
  (if (null reverse-env-directories-alist)
      ;; Construct a list of directories named in environment
      ;; variables that I expect to be defined in all my environments.
      ;; Put the longest ones first, to avoid sub-optimal selection
      ;; of a variable to use.
      (setq reverse-env-directories-alist
	    (sort
	     (mapcar 
	      (lambda (name)
		(cons (concat (getenv name) "/")
		      name))
	      reverse-environment-variables)
	     (lambda (a b)
	       (> (length (car a)) (length (car b))))))))

(defun unsubstitute-in-file-name (filename)
  "In filename, put in substitutions for any of the variables in reverse-environment-variables."
  (ensure-reverse-env-directories-alist)
  ;; (message "unsubstitute-in-file-name<-%s" filename)
  (let ((result
	 (let* ((is-from-env (match-any-initial-string filename reverse-env-directories-alist)))
	   (if is-from-env
	       (concat "$"
		       (cdr is-from-env)
		       "/"
		       (substring filename
				  (length (car is-from-env))))
	     filename))))
    ;; (message "unsubstitute-in-file-name->%s" result)
    result))

;;; end of filenames-in-env.el
