;;; ;;; From: Sriram Karra <karra@shakti.homelinux.net>
;;; ;;; Subject: random-man.el -- display a unix man page at random
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 25 Oct 2002 16:29:06 +0530
;;; ;;; Organization: The Klingon High Council
;;; ;;; Mail-Copies-To: nobody


;;; I remember posting this to g.e.s a loong time back.  I cannot recall
;;; all the differences between this version and the one I posted.  There
;;; is a good chance this one might actually work (Tested on Debian and
;;; Solaris)

;;; random-man.el - generate a random unix manual page each time.

;; Copyright (C) 2000-2002 Sriram Karra

;; Author     : Sriram Karra <karra@shakti.homelinux.net>
;; Created    : March 27, 2000
;; License    : please do as you please
;; Version    : 1.1
;;
;; $Id: random-man.el,v 1.4 2001/03/22 01:25:23 karra Exp $

;;; ChangeLog (woefully wrong and incomplete)

;; Oct 29 2001 : * New variable ran-man--debug
;;	         * Correctly parse page names containing `.' like
;;	           adduser.conf.5.gz
;;		 
;; Mar 20 2001 : New variable ran-man--section-flag
;; Mar 20 2001 : New variable ran-man--max-tries
;; Mar 21 2001 : Fixed a bug in the usage of file-name-sans-extension, when
;;		 what was needed was something like sans-all-extensions.
;;		 The function ran-man--file-name-sans-all-extensions was
;;		 written.

;;; Installation:

;; Put this file in a directory accesible via your `load-path', and include
;; (require 'random-man) in your ~/.emacs file

;;; Commentary:

;; Then when you want to look at a random Unix man page, use the command
;; `random-man' - If all goes well, after a short while, a man page should
;; pop up in a new window.
;;
;; We attempt to look at all sections of the manual.  The biggest problem
;; is the non-standard layout of the files and differences in syntax of the
;; `man' command that is eventually called to display the stuff.  However,
;; we try to be a little smart about it and grok the man syntax of some
;; popular Unix variants like GNU/Linux, BSD, SunOS and IRIX.  If you are
;; using some other OS, then you might want to manually set the variable
;; `ran-man--section-flag'.
;;
;; Quite frequently, there are problems with your $MANPATH, or the
;; random-man package just does not grok the way man pages are laid out in
;; your file system.  This would, naturally, result in errors, but you can
;; instruct random-man to try again.  the variable `ran-man--max-tries'
;; does just that.

;;; TODO:

;; * Allow user to specify Manual Section number to get a random page
;;   from.
;; 
;; * Atleast in systems that I have access to, there seem to be a ton of
;;   Tcl/Perl kind of man pages - essentially stuff that I do not feel very
;;   interested in.  Should figure out a way to weed out these garbage
;;   pages.

;;; Code:

;; woman.el, which is a part of GNU Emacs 21 and above., is used to
;; display the man pages.  It includes a lot of other cool functionality
;; like caching all man page topics and stuff, that it is a good idea to
;; use it instead of man.el.
(require 'woman)

(defvar ran-man--debug nil
  "Print verbose messages while choosing random man page.
This is meant for debugging purposes.")

(defvar ran-man--section-flags-alist
  '((irix           . " ")
    (gnu/linux      . "-S ")
    (usg-unix-v     . "-s ")
    (berkeley-unix  . "-S ")
    (Misc           . " "))
   "Alist specifying flag to be used to get to sections of Man pages.
It consists of (system-type . flag) pairs, where `flag' is the option to
the `man' command that specifies section number.  random-man appends a
section number (say \"3C\") to this flag and passes it on to `man'")

(defvar ran-man--section-flag
  (let ((lis (assoc system-type ran-man--section-flags-alist)))
    (if lis
	(cdr lis)
      (cdr (assoc 'Misc ran-man--section-flags-alist))))
      
  "Flag passed to the `man' command to specify section number.
This variable (a string), specifies the flag used by the `man' command to
get to specific sections of the Unix man pages.  This is initialised to
something meaningful based on the current OS.  You should set this manually
if you are using some arcane variant.  NOTE: The string should end with one
more whitespace.")

(defvar ran-man--max-tries
  5
  "Maximum number of tries in case of error while fetching man page.
If there is an error in your $MANPATH setting, the random man page
generator can produce an error.  In such a case, you can keep trying.  This
variable specfies the number of such tries allowed.")

;; `parse-colon-path' from files.el has a reputation for creating null
;; elements for redundant semi-colons and trailing /.  lets clean house
;; this function copied verbatim from woman.el of Francis J. Wright.
(defun ran-man--parse-colon-path (cd-path)
  (if (and (eq system-type 'windows-nt) (string-match "//" cd-path))
      (let ((path-separator ":"))
	(mapcar
	 (function
	  (lambda (path)			 
	    (cond ((string-match "\\`//" path)	 
		   (setq path (substring path 1))
		   (aset path 0 (aref path 1))	 
		   (aset path 1 ?:)))
	    path))
	 (parse-colon-path cd-path)))
    (parse-colon-path cd-path)))

(defconst ran-man--manpath
  (let ((manpath (getenv "MANPATH")))
    (or (and manpath (woman-parse-colon-path manpath))
	'("/usr/man" "/usr/share/man" "/usr/local/man")))
  "Path used to search man pages by the random-man package.")

(defconst ran-man--file-regexp
  "\\(.*\\)\\.\\([0-9lmnt]\\w*\\)\\(\\.\\(g?z\\|bz2\\)\\)?\\'"
  "Regexp used to match a filename with a man page.
Care is taken to match certain kinds of compresses man pages as well.
The first parenthesised expression is used to obtain the topic name.")

(defun ran-man--file-name-sans-all-extensions (filename)
  (when ran-man--debug
    (message "parsing... %s" filename))
  (when (string-match ran-man--file-regexp filename)
    (match-string 1 filename)))
 
;; the work horse.  Look at the manual page for man to find out more about
;; how the unix command "man" works.  our function works like this:
;; 1. make a list of all components of your MANPATH var.
;; 2. generate a random number, and use this to index into the list created
;;    above to get the name of one directory containing man pages.
;; 3. check to see if this is a directory, and then make a list of all
;;    files in this directory with names man*.  these should be directories
;;    themselves.  make a note of the part of the filename that appears
;;    after "man".  this is the section number that "man" takes as argument
;; 4. look inside this directory, and create a list of all files.  strip
;;    off extension and leading path name from their filenames before
;;    creating the list.
;; 5. generate another random number and index into this list to get a
;;    random page.
;; 6. call man (from man.el) with the word just created, and the section
;;    number remembered earlier.
;;
;; We are generating random numbers three times.  and the three "levels" in
;; the function below correspond to those three steps.

(defun ran-man--get-file-recursively (level vec &optional str)

  (when ran-man--debug
    (message "Entered level %d.  str = %s" level str))  

  (cond
     ((eq level 0)
      (let* ((path ran-man--manpath)
	     (pathv (vconcat [] path))
	     (size (length pathv))
	     (ran (random size)))
	(when ran-man--debug
	  (message "random no at level 0 = %d" ran))

	(if (eq size 0)
	    (error "manpath is empty.  Impossible...")
	  (ran-man--get-file-recursively 1 (vector (aref pathv ran)) str))))

     ((eq level 1)
      (let* ((man-dir (aref vec 0)))
	(if man-dir
	    (if (file-directory-p man-dir)
		(let* ((filess (vconcat []
					(directory-files man-dir t
							 "^man\\|^cat")))
		       (size (length filess))
		       (ran (if (> size 0)
				(random size)
			      0)))
		  (when ran-man--debug
		    (message "random no at level 1 = %d" ran))
		  
		  (if (= size 0)
		      (ran-man--get-file-recursively 0 [])
		    ;; (message "10")
		    (let* ((dir (aref filess ran))
			   (maan (file-name-nondirectory dir))
			   (section (substring maan 3 (length maan))))
		      ;; (message section)
		      (ran-man--get-file-recursively 2
						     (vector dir)
						     section))))
	      nil)
	  nil)))

     ((eq level 2)
      (let ((man-dir (aref vec 0)))
	(if man-dir
	    (if (file-directory-p man-dir)
		(let* ((filess (vconcat []
					(directory-files man-dir t
							 "[^\.].*")))
		       (size (length filess))
		       (ran (if (> size 0)
				(random size)
			      0)))

		  (when ran-man--debug
		    (message "random no at level 2 = %d" ran))
		  
		  (if (= size 0)
		      (ran-man--get-file-recursively 0 [])
		    ;;(message "20")
		    (let* ((fil (ran-man--file-name-sans-all-extensions
				 (aref filess ran)))
			   (fi (file-name-nondirectory fil)))
		      (condition-case err
			  (if (string= str "")
			      (man fi)
			    (man (concat ran-man--section-flag
					 str  " " fi)))
			(error (random-man)))
			  
		      t)))
	      nil)
	  nil)))))

(random t)


;; the interface to the world
;;;###autoload
(defun random-man ()
  "Generate a Unix man page at random.
The $MANPATH environment variable is consulted for this purpose.  If it is
invalid, then it will terminate with an error.  Termination might occur
even if one component of your $MANPATH is invalid.  The function `man' from
man.el, which is distributed with GNU Emacs, is used to do the atcual
displaying."
  (interactive)
  (let ((i 0))
    (while (and (< i ran-man--max-tries)
		(not (ran-man--get-file-recursively 0 [])))
      ;; (message (format "Failed attempt number %d" i))
      (setq i (+ i 1)))
    (when (= i ran-man--max-tries)
      (error (format "Failed %d attempts.  Giving up"
		     ran-man--max-tries)))))

(provide 'random-man)

;;; random-man.el ends here.

;;; -- 
;;; Well, all's well that ends.

