;;; fetch.el --- Never type the long remote file name again..
;; Time-stamp: <02/10/27 17:52:18 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: fetch.el
;; Package: fetch
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:  Tramp ange-ftp mirror remote refresh update file
;; Version: 0.3
;; Author's homepage: http://deego.gnufans.org/~deego/
;; For latest version: 

(defconst fetch-home-page
  "http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/fetch/")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; Namespace issues: The only builtin emacs lisp function with fetch-
;; prefix seems to be fetch-bytedode.

;; See also: mirror.el


;; Quick start:
(defconst fetch-quick-start
  "Put this file somewhere in load-path.  Add (require 'fetch) to
.emacs.  Also consider adding (fetch-install-for-eshell) to .emacs

Customize fetch-remote-host-string.  See its doc. 

Now run commands like M-x fetch, fetch-put-file, fetch-diff,
fetch-revdiff, fetch-buffer, fetch-put-buffer. 

From eshell, run commands like fetch, fput, fdiff, frevdiff eg:
 $ fdiff -cw file.el
 or 
 $ fput file.el

Finally, bind your choice of keys to your favorite fetch commands.."
)

(defun fetch-quick-start ()
  "Provides electric help from variable `fetch-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert fetch-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst fetch-introduction
  "[See also mirror.el on Emacs Lisp List---that did not seem to do 
what I had in mind, so I wrote fetch.el...]

Tired of typing out stuff like below? 
 $ cp file.el \
  /deego@24.197.159.102:~/emacs/emacspub/lisp-mine/file/dev/file.el 

Fetch tries to allow you to save having to type the remote file name
when using tramp or ange-ftp to fetch or put, or when seeing diffs, or
whether you do all this in eshell.  

And you can even try to abuse it to use, instead of using
remote-file-names, a different directory structure on the same host..

Written in a platform independent-way, but tested only on
gnulinux, emacs21.3.50 with tramp 2.0.25.  

Type M-x fetch-quick-start" )

;;;###autoload
(defun fetch-introduction ()
  "Provides electric help from variable `fetch-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert fetch-introduction) nil) "*doc*"))

;;; Commentary:
(defconst fetch-commentary
  "
OLD stuff:
New features in 0.2:

* Minor internal changes to allow greater eshell compatibility. 
 
  Thus, more complex commands like
  $ fetch a.el b.el c.el 

  and

  $ fetch foo/bar/a* 
  work. \"
"
)

(defun fetch-commentary ()
  "Provides electric help from variable `fetch-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert fetch-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst fetch-new-features
  "
New in 0.3 onwards:
* Introduce fetch-diff-buffer and fetch-revdiff-buffer.

* Provide a hook fetch-before-replace-hooks

* Provide an OPTIONAL function for the above hook \(needs mkback.el >=
0.4\) (posted here) which optionally makes an backup of the
local/remote file being replaced via mkback.el

* diffrev-->revdiff in the names of functions.
"
)

(defun fetch-new-features ()
  "Provides electric help from variable `fetch-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert fetch-new-features) nil) "*doc*"))

;;; TO DO:
(defconst fetch-todo
  "2002-10-22 T09:52:01-0400 \(Tuesday\)    D. Goel	
TODO: for v. > 0.2 

[1] Allow for multiple remote hosts...

[2] defun a fetch-dired ")

(defun fetch-todo ()
  "Provides electric help from variable `fetch-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert fetch-todo) nil) "*doc*"))

(defconst fetch-version "0.3")
(defun fetch-version (&optional arg)
   "Display fetch's version string. 
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "fetch version %s" fetch-version))
    (message "fetch version %s" fetch-version)))

;;==========================================
;; Requires:
(eval-when-compile (require 'cl))

;;; Code:





(defgroup fetch nil 
  "The group fetch"
  :group 'applications)
(defcustom fetch-before-load-hooks nil 
  "Hooks to run before loading fetch."
  :group 'fetch)
(defcustom fetch-after-load-hooks nil 
  "Hooks to run after loading fetch."
  :group 'fetch)
(run-hooks 'fetch-before-load-hooks)

(defcustom fetch-verbosity 0
  "How verbose to be.  From -100 to 100. 
Once you are experienced with this lib, 0 is the recommended value."
  :type 'integer
  :group 'fetch)
(defcustom fetch-interactivity 0
  "How interactive to be.  From -100 to 100. 
Once you are experienced with this lib, 0 is the recommended value."
  :type 'integer
  :group 'fetch)
(defcustom fetch-y-or-n-p-function'fetch-y-or-n-p
  "Function to use for interactivity-dependent  y-or-n-p.
Format same as that of `fetch-y-or-n-p'"
  :group 'fetch)

(defcustom fetch-n-or-y-p-function'fetch-y-or-n-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `fetch-n-or-y-p'"
  :type 'function
  :group 'fetch)
(defun fetch-message (points &rest args)
  "Signal message, depending on POINTS andfetch-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points fetch-verbosity))
    (apply #'message args)))
(defun fetch-y-or-n-p (add prompt)
  "Query or assume t, based on `fetch-interactivity'"
  (if (minusp (+ add fetch-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun fetch-n-or-y-p (add prompt)
  "Query or assume t, based on `fetch-interactivity'"
  (if (minusp (+ add fetch-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))


;; BEGIN REAL CODE


(defvar fetch-putp nil
  "Internal.. should always be nil normally..")


(defvar fetch-source nil
  "for use by hooks..")
(defvar fetch-target nil
  "")


(defcustom fetch-before-replace-hooks nil
  "Hooks to run before replacing a pre-existing file..  
These functions will be called with no arguments, but at such time,
fetch-source will be bound to the source and fetch-target to target..
Can add here, for instance fetch-mkback.

If any of these hooks setqs fetch-go-ahead-p to nil, the fetch action
will be canceled. 
"
:group 'fetch
)

(defvar fetch-err-var nil)
(defmacro fetch-ignore-errors (&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  `(condition-case fetch-err-var (progn ,@body)
     (error
      (ding t)
      (ding t)
      (ding t)
      (message "IGNORED ERROR: %s" (error-message-string
				    fetch-err-var))
      (sit-for 1)
      nil)))


(defcustom fetch-get-remote-name-function 'fetch-get-remote-name
  "Function to construct the name of the remote file.  "
  :type 'function
  :group 'fetch)

(defcustom fetch-remote-host-string "/deego@24.197.159.102:"
  "The default here is an example that works with my tramp version 2.0.25
--the one present in emacscvs 21.3.50.. and is expected to work with
subsequent versions too..

For emacs 21.2 and lesser, this same format should make emacs use
ange-ftp instead of tramp."
  :group 'dope
)


(defcustom fetch-diff-switches "-c"
  "Switches to pass to diff..."
  :group 'fetch)

(defcustom fetch-substitutions nil
  "A list of substritutions to make, in turn.. Example: 
        ((\"/home/deego\" \"/home/deego1\") (\"/home\" \"/foo\")).

The second arguments are passed as the second arguments to
replace-regexp-in-string, so may be a function instead of string..."
  :type 'list
  :group 'dope)


(defvar fetch-go-ahead-p nil
  "Internal. ")

(defun fetch-get-remote-name (dir file)
  (let ((str 
	 (concat fetch-remote-host-string (expand-file-name 
					   file dir)))
	(subs fetch-substitutions))
    (while subs
      (setq str (replace-regexp-in-string str (caar subs) (cadar
							   subs)))
      (setq subs (cdr subs)))
    str))
    

(defmacro fetch-withit (expr &rest rest)
  "Caution: var-capture by its very nature.."
  `(let ((it ,expr))
     ,@rest))

;;;###autoload
(defun fetch-install-for-eshell ()
  (interactive)
  (defalias 'eshell/fetch 'fetch-file)
  (defalias 'eshell/fget 'fetch-file)
  (defalias 'eshell/fput 'fetch-put-file)
  (defalias 'eshell/fdiff 'fetch-eshell/fdiff)
  (defalias 'eshell/frevdiff 'fetch-eshell/frevdiff)
  )

;;;###autoload
(defalias 'fetch 'fetch-file)

;;;###autoload
(defalias 'fetch-fput 'fetch-put-file)



;;;###autoload
(defun fetch-put-file (&optional file &rest morefiles)
    (interactive "F")
  (unless file
    (setq file (read-file-name "File: ")))
  (unless file (error "No filaname supplied to fetch-put: nil"))
  (let ((fetch-putp t))
    (apply 'fetch-file-basic-no-errors file morefiles)))


(defun fetch-file (&optional file &rest morefiles)
    (interactive "F")
  (unless file
    (setq file (read-file-name "File: ")))
  (unless file (error "No filaname supplied to fetch: nil"))
  (let ((fetch-putp nil))
    (apply 'fetch-file-basic-no-errors file morefiles)))


(defun fetch-file-basic-no-errors (&rest args)
  (fetch-ignore-errors
   (apply 'fetch-file-basic args)))


(defun fetch-file-basic (file &rest morefiles)
  "Fetches or puts file FILE. depending on the value of fetch-putp. "
  (interactive "F")
  (unless file
    (setq file (read-file-name "File: ")))
  (unless file (error "No filaname supplied to fetch: nil"))

  ;; this happens from eshell for multifile globs..
  (cond
   (morefiles
     (progn
       (fetch-file-basic-no-errors file)
       (mapcar 'fetch-file-basic-no-errors morefiles)))
   ((and file (listp file))
     (mapcar 'fetch-file-basic-no-errors file))
   (t
    (let* ((remotefile
	     (funcall fetch-get-remote-name-function 
		      default-directory file))
	    
	    (source (if fetch-putp file remotefile))
	    (target (if fetch-putp remotefile file))
	    (existsp (file-exists-p target))
	    (fetch-source source)
	    (fetch-target target)
	    (fetch-go-ahead-p t)
	    dop )
       ;; cond1 
       (cond  
	((file-exists-p source) 

	 ;; begin file-exists-p conditional
	 (setq dop
	       (or (not existsp)
		   (and
		    (fetch-y-or-n-p 0 
				    (format 
				     "Replace existing file %S by %S?" 
				     target source))
		    (progn
		      (run-hooks 'fetch-before-replace-hooks)
		      t))))
	 (cond 
	  ((and dop fetch-go-ahead-p)
	   (fetch-message 5 "Copying %s to %s.." source target)
	   (copy-file source target t)
	   (fetch-message 6 "Copying %s to %s .. done" source target)
	   ;; return somethign meaningful
	   (list source target t)
	   )
	  (t 
	   (fetch-message 1 "Not copying %s to %s.." source target)
	   ;; return somethign meaningful
	   (list source target nil)
	   )))
	;; end file-exists-p conditional

	;; in cond1
	(t (error "Remote source does not exist:  %S" source)))))
    ))



;;;###autoload
(defun fetch-diff-buffer ()
  (interactive)
  (fetch-withit 
   (buffer-file-name)
   (if it 
       (fetch-diff it)
     (fetch-message 0 "Buffer has no associated file: %S"
		    (buffer-name)))))

;;;###autoload
(defun fetch-revdiff-buffer ()
  (interactive)
  (fetch-withit 
   (buffer-file-name)
   (if it 
       (fetch-revdiff it)
     (fetch-message 0 "Buffer has no associated file: %S"
		    (buffer-name)))))


;;;###autoload
(defun fetch-put-buffer ()
  (interactive)
  (fetch-withit 
   (buffer-file-name)
   (if it 
	 (fetch-put-file it)
     (fetch-message 0 "Buffer has no associated file: %S"
		    (buffer-name)))))



;;;###autoload
(defun fetch-buffer ()
  (interactive)
  (fetch-withit 
   (buffer-file-name)
   (if it 
       (progn
	 (fetch-file it)
	 (when (fetch-y-or-n-p 
		-10 "Done fetching.. Revert buffer now?")
	   (fetch-message 1 "Reverting to fetched buffer..")
	   (revert-buffer t t)
	   (fetch-message 1 "Reverting to fetched buffer..done")
	   ))

     (fetch-message 0 "Buffer has no associated file: %S"
		    (buffer-name)))))




;;;###autoload
(defun fetch-diff (&optional file reversep)
  (interactive "F")
  (unless file
    (setq file (read-file-name "File: ")))
  (unless file (error "No filaname supplied to fetch: nil"))
  (let* ((remotefile
	  (funcall fetch-get-remote-name-function 
		   default-directory file))

	 (source (if reversep file remotefile))
	 (target (if reversep remotefile file))
 	 (existsp (file-exists-p target))
	 dop )
    ;; cond1 
    (diff target source fetch-diff-switches)))



  
;;;###autoload
(defun fetch-revdiff (&optional file reversep)
  (interactive "F")
  (unless file
    (setq file (read-file-name "File: ")))
  (unless file (error "No filaname supplied to fetch: nil"))
  (fetch-diff file 'reversep))



;;;###autoload
(defun fetch-eshell/fdiff (&rest args)
  (let* ((rev (reverse args))
	 (file (first rev))
	 (switches (reverse (cdr rev))))
    (unless file (error "No file supplied"))
    (apply 'eshell/diff 
	   (append
	    switches
	    (list
	     file 
	     (funcall fetch-get-remote-name-function 
		      default-directory
		      file))))))

	     

;;;###autoload
(defun fetch-eshell/frevdiff (&rest args)
  (let* ((rev (reverse args))
	 (file (first rev))
	 (switches (reverse (cdr rev))))
    (unless file (error "No file supplied"))
    (apply 'eshell/diff 
	   (append
	    switches
	    (list
	     (funcall fetch-get-remote-name-function 
		      default-directory
		      file)
	     file
	     )))))




(defcustom fetch-mkback-host-string 
  ""

  "String mathching the main host.  We want to make backup of files
residing on this string. When this string is \"\", it will match any
host. If this string matches the target file, we try to make a backup
of such a file. 


This check is done when putting files..

  An example of such a string matching 2 hosts is is 
\"\\(^24\\.197\\.159\\.102$\\)\\|\\(^deego\\.gnufans\\.org$\\)\"

If this variable is nil, this particular critereon never succeeds.."
  :type 'string
  :group 'fetch)

(defcustom fetch-mkback-system-name 
  ""
  "String mathching the system-name of the main host.  We want to make
backup of files residing on this string. When this string is \"\", it
will match any host. If this string matches the target file, we try to
make a backup of such a file.  

This check is done when fetching files..

  An example of such a string matching 2 hosts is is 
 \"computer\\.localdomain\"

If this variable is nil, this particular critereon never succeeds.."
 :type 'string
 :group 'fetch)

;; defined just to avoid compilation error.. becuase can't (require
;; 'mkback). 
(defvar fetch-mkback-function-name 'mkback)

(defun fetch-mkback ()
  "An interface to mkback before replacing existing files..

The aim of this function is to mkback iff the file being replaced lies
on the main computer..

Will work only with mkback.el version 0.4 or later. 

If you get too annoyed by frequent \"backup-needed\" prompts,
customize fetch-mkback-system-name and fetch-mkback-remote-host-string
so that such prompts occur only for certain (or none) targets..
"
  ;;(fetch-ignore-errors
  ;; setting it nil here, so that if an error occurs, we get a nil,
  ;; and don't return t. 
  (let ((old-val fetch-go-ahead-p))
    (setq fetch-go-ahead-p nil)
    (let (backup-needed backup-made)
      (setq backup-needed
	    (and 
	     fetch-target
	     (if fetch-putp
		 ;; are putting
		 (and
		  fetch-mkback-host-string
		  (string-match fetch-mkback-host-string fetch-target))
	       ;; are fetching file here..
	       (and fetch-mkback-system-name
		    (string-match fetch-mkback-system-name system-name))
	       
	       )
	     (fetch-y-or-n-p 60 "Create backup of existing file? ")))


      (if backup-needed
	  (setq backup-made
		(funcall fetch-mkback-function-name fetch-target)))
      (if (and backup-needed (not backup-made))
	  (progn
	    (message "WILL NOT COPY %S TO %S!!" fetch-source fetch-target)
	    (setq fetch-go-ahead-p nil)
	    (sleep-for 1))
	(setq fetch-go-ahead-p old-val))
    fetch-go-ahead-p)))

(defun fetch-some (&rest args)
  (or 
   (null args)
   (car args)
   (apply 'fetch-some (cdr args))))


(provide 'fetch)
(run-hooks 'fetch-after-load-hooks)



;;; fetch.el ends here
