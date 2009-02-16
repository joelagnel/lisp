;;; Slurp.el
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;
;; Author: Jason Meade (jemeade@gmail.com)
;; Last Update: March 18, 2006
;; Version: 1.2
;;

;; slurp.el is a handly tool used to search a file hierarchy for files with
;; names that match a regular expression. Slurp will load those files into 
;; emacs buffers for you.

(defvar orig-max-lisp-eval-depth max-lisp-eval-depth)

;; You might need to increase this value on your system if you are getting
;; "max-lisp-eval-depth exceeded" errors.
(defvar slurp-max-lisp-eval-depth 1000)
(if (> max-lisp-eval-depth slurp-max-lisp-eval-depth)
    (setq slurp-max-lisp-eval-depth max-lisp-eval-depth))

(defvar slurp-file-count 0)
(defvar slurp-buffer "*slurped-files*")

;; A list of regular expressions to compare files against. Items
;; in this list won't be slurped.
(defvar slurp-ignore-filelist
  (list "^\.#"	   ; session recovery files
	"~$"	   ; backup files
	"\.tar$"   ; common binary files
	"\.gz$"
	"\.tgz$"
	"\.bz2$"
	"\.zip$"
	"\.doc$"
	"\.ppt$"
	"\.xls$"
	"\.class$"
	"\.o$"
	"\.obj$"
	))

;; 
(defun slurp ()
  "Search for and load files from a directory hierarchy matching a regexp"
  (interactive
   (let ((filter (read-from-minibuffer "Slurp files by regexp: "))
	 (slurp-dir default-directory))
     (get-buffer-create slurp-buffer)
     (switch-to-buffer-other-window slurp-buffer)
     (erase-buffer)
     (cd slurp-dir)
     (setq slurp-file-count 0)
     (insert (concat "Slurping contents of "
		     slurp-dir " using regexp filter: " filter))
     (insert "\n\n")
     ;; Temporarily override the default max-lisp-eval-depth value.
     (setq max-lisp-eval-depth slurp-max-lisp-eval-depth)
     (slurp-files filter)
     ;; Reset this value now that we are done.
     (setq max-lisp-eval-depth orig-max-lisp-eval-depth)
     (insert (concat "\nDone. " (int-to-string slurp-file-count)
		     " files slurped\n")))))


;; 
(defun slurp-files (filter)
  "Slurp files in the current directory matching a regexp"
  (dolist (file (directory-files "."))
    (if (slurp-good-directory file)
	(progn
	  (cd file)
	  (slurp-files filter)
	  (cd "..")))
    (if (slurp-do-slurp file filter)
	(slurp-file file))))

;;
(defun slurp-good-directory (file)
  "Check to see if this is a good directory"
  (and (file-directory-p file) (file-executable-p file)
       (not (equal file "."))(not (equal file ".."))))

;; 
(defun slurp-file (file)
  "Load an individual file"
  (setq slurp-file-count (+ slurp-file-count 1))
  (find-file-noselect file)
  (insert (concat "Slurped: " file "\n")))

;;
(defun slurp-do-slurp (file filter)
  "Fetch the files you want"
  (let ((do-slurp nil))
    (if (string-match filter file)
	(setq do-slurp t))
    (dolist (expr slurp-ignore-filelist)
      (if (string-match expr file)
	  (setq do-slurp nil)))
    do-slurp))

(provide 'slurp)

;;; End Slurp
