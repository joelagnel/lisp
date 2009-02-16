;; bbdb-query.el
;; version: 2.0  Oct 10 1998
;;
;; Copyright (C) 1993 1994 Cengiz Alaettinoglu <cengiz@isi.edu>
;;
;; LCD Archive Entry:
;; bbdb-query|Cengiz Alaettinoglu|cengiz@isi.edu|
;; BBDB Query/Append/Flush/Keep functions.|
;; 20-Oct-1998|2.0|~/misc/bbdb-query.el|
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
;; You didn't receive a copy of the GNU General Public License along
;; with this program; so, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;----------------------------------------------------------------------------
;;
;; DESCRIPTION:
;;   All of the following commands asks for a field name 
;;   and a regular expression, then
;;     bbdb-query 
;;       displays only the matching records in the BBDB buffer
;;     bbdb-append 
;;       appends the matching records to the BBDB buffer
;;     bbdb-flush 
;;       deletes the matching records from the BBDB buffer
;;     bbdb-keep 
;;       keeps only the matching records in the BBDB buffer
;;
;;   To search for all fields press RET at the field prompt.
;;   To search for all note fields enter "notes" at the field prompt.
;;
;; INSTALLATION:
;;
;;   Add this to your .emacs:
;;
;;     (require 'bbdb-query)
;;
;;   and put this elisp file somewhere on your load-path and byte-compile it.
;;
;; Revision 2.0  1998/10/20 colin@xemacs.org
;; Modified to work with BBDB 2.0.  Created `bbdb-query-display-records'
;; function to simplify append/non-append and running hooks.  Added
;; autoload tags.
;;

(require 'bbdb-com)
(provide 'bbdb-query)

(define-key bbdb-mode-map "B" 'bbdb-query)
(define-key bbdb-mode-map "A" 'bbdb-append)
(define-key bbdb-mode-map "F" 'bbdb-flush)
(define-key bbdb-mode-map "K" 'bbdb-keep)

;;;###autoload
(defun bbdb-query (elidep)
  "Load all entries in the BBDB matching the regexp STRING 
 in either the name(s), company, network address, or notes."
  (interactive "P")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(bbdb-append nil))
    (bbdb-query-display-records
     (bbdb-search-field (bbdb-records)) bbdb-append)))

;;;###autoload
(defun bbdb-append (elidep)
  "Append all entries in the BBDB matching the regexp STRING 
 in either the name(s), company, network address, or notes."
  (interactive "P")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(bbdb-append t))
    (bbdb-query-display-records
     (bbdb-search-field (bbdb-records)) bbdb-append)))

;;;###autoload
(defun bbdb-keep (elidep)
  "Keep only the entries in the BBDB buffer matching the regexp STRING 
 in either the name(s), company, network address, or notes."
  (interactive "P")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(bbdb-append nil)
	(ca-bbdb-records nil))
    (setq ca-bbdb-records (mapcar 'car bbdb-records))
    (bbdb-query-display-records
     (bbdb-search-field ca-bbdb-records) bbdb-append)))

;;;###autoload
(defun bbdb-flush (elidep)
  "Keep only the entries in the BBDB buffer matching the regexp STRING 
 in either the name(s), company, network address, or notes."
  (interactive "P")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep))
	(bbdb-append nil)
	(ca-bbdb-records nil)
	(ca-bbdb-records-matched nil)
	(ptr nil))
    (setq ca-bbdb-records (mapcar 'car bbdb-records))
    (setq ca-bbdb-records-matched 
	  (bbdb-search-field ca-bbdb-records))
    (setq ptr (cdr ca-bbdb-records)
	  parent ca-bbdb-records)
    (while ptr
      (if (member (car ptr) ca-bbdb-records-matched)
	  (setcdr parent (cdr ptr))
	(setq parent ptr))
      (setq ptr (cdr ptr)))
    (if (and ca-bbdb-records 
	     (member (car ca-bbdb-records) ca-bbdb-records-matched))
	  (setq ca-bbdb-records (cdr ca-bbdb-records)))
    (bbdb-query-display-records ca-bbdb-records bbdb-append)))

(defun bbdb-search-field (records)
  "Read field name and string. Display all entries in RECORDS matching string in the named field."
  (let ((which nil)
	(string "")
	(which-notes ""))
    (setq which 
	  (completing-read 
	   "Field to search (RET for all): "
	   (append '(("name")("company")("net")("notes")("phones")) 
		   (bbdb-propnames))
	   nil t))
    (and (string= which "notes")
	 (setq which-notes ""))
    (if (assoc which (bbdb-propnames))
	(progn (setq which-notes which)
	       (setq which "notes")))
    (setq string (read-string "Regular Expression: "))
    (if (string= which "")
	(bbdb-query-all records string)
      (if (string= which "notes")
	  (bbdb-query-notes records (if (string= which-notes "")
					(cons '* string)
				      (cons (intern which-notes) string)))
	(funcall (intern (concat "bbdb-query-" which)) records string)))))
    

;;;###autoload
(defun bbdb-query-all (records string)
  "Display all entries in the BBDB matching the regexp STRING 
in either the name(s), company, network address, or notes."
  (interactive "sRegular Expression: \nP")
  (let ((notes (cons '* string)))
    (bbdb-search records string string string notes nil)))

(defun bbdb-query-name (records string)
  "Display all entries in the BBDB matching the regexp STRING in the name
\(or ``alternate'' names\)."
  (bbdb-search records string))

(defun bbdb-query-company (records string)
  "Display all entries in BBDB matching STRING in the company field."
  (bbdb-search records nil string))

(defun bbdb-query-net (records string)
  "Display all entries in BBDB matching regexp STRING in the network address."
  (bbdb-search records nil nil string))

(defun bbdb-query-notes (records string)
  "Display all entries in BBDB matching STRING in the named notes field."
  (bbdb-search records nil nil nil string))

(defun bbdb-query-phones (records string)
  "Display all entries in BBDB matching the regexp STRING in the phones field."
  (bbdb-search records nil nil nil nil string))

(defun bbdb-query-display-records (records append)
  ""
  (bbdb-display-records-1 records append)
  (save-excursion (run-hooks 'bbdb-list-hook)))

;; end bbdb-query.el
