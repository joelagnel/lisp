;;-*- auto-recompile: t -*-
;;; edlib.el ---  EdLib = help EDit emacs-lisp LIBraries..
;; Time-stamp: <02/09/01 21:08:00 deego>
;; Copyright (C) 2001 Deepak Goel
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: edlib.el
;; Package: edlib
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 2.2.0
;; For latest version/info: http://24.197.159.102/~deego
;; Requires:  cl
;; See also: checkdoc, which edlib calls :)
;;
;;; Quick start:


(defvar edlib-quick-start
"
Drop edlib.el somewhere in load-path, add \(require 'edlib\) to .emacs,
Create your package-file and type M-x edlib
")



(defun edlib-quick-start ()
  "Provides electric help for function `edlib-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert edlib-quick-start) nil) "*doc*"))



;;; New features:
(defvar edlib-new-features
       "  New in 2.1.2
* Takes care to allow your kill-ring to be unmodified.  See
edlib-add-to-kill-ring-p. 

   New in 1.8.0alpha:
* Will now accept a list of different copyright-holders.. 


	New in version 1.4
* Fixed a bug in edlib-update-archive-entry.

	New in version 1.2
* Further improved edlib-doc, improved user-interface, enhanced
  rendering of documentation-strings, including conversion of \( or \)
  to \\\( or \\\) when necessary, and if the user so desires.


	New in version 1.0

* VASTLY improved user-interface, and everything else.
* Complete checkdoc compliance.
")



;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar edlib-introduction
 "EDLIB helps you EDit emacs-lisp LIBraries:

* Creates and updates your emacs-lisp archive entry and other
  library-paraphernalia for you..  updates version info in the
  elisp-archive-header-info based on <lib>-version..

* Allows you to use one source for <library>-Introduction. When you
  want to post code to gnu.emacs.sources :\) , type M-x
  <lib>-introduction, and then cut and paste.. the same introduction
  that you posted to gnu.emacs.sources is also available to users by
  either typing <lib>-introduction or visiting your source-file.

* Does the same thing for New-Features, as well as for Commentary.
  Allows intro and commentary be accessible both from file and a
  function.

* Allow easy editing of doc-strings... editing them as the user would
  see it, so no need to type \\\" when you need \".. and moreover, in the
  text-mode, not emacs-lisp-mode , that's how i am editing this
  one.. to do so, go to the relevant doc-string and type M-x edlib-doc


* It also helps you post them to gnu.emacs.sources, it extracts the
  information from <lib>-introduction and presents that as
  introduction to the package. M-x edlib-post-source.


* While doing everything, it lets you have complete control, it does
  nothing without asking you.

* As of 8/3/01, defines macro called edlib-read-string

* Features and patches welcome :\)")


(defun edlib-introduction ()
  "Provides electric help for function `edlib-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert edlib-introduction) nil) "*doc*"))


;;;
;;
;;
;;
;;; Commentary:
(defvar edlib-commentary
 "
Type M-x edlib-introduction for intro, and read it. Then, read this:

To take full advantage of edlib, Don't forget to see the documentation
for edlib-update-version-info and thus update your
write-file-hook. And don't forget to define <lib>-version somewhere
for your <lib>.

Also, see edlib.el and add the very few suggested  global-set-keys to your
.emacs..

Main Functions to use:
Write some code and call 'edlib on it. 
Can also use sub-functions like edlib-insert-archive-entry...

To edit \(documentation-\)strings, use edlib-doc on the documentation,
this allows you to easily format your documentation, since everything
is done in text-mode.

I am thus now in the process of using this package to convert all my
existing packages to this form..  remove all source-code commented-out
commentaries and define them in M-x <lib>-commentary..  One thing that
needs is uncommenting some code.. and my emacs-20.3.1's comment-region
is terrible at uncommenting more than one comment.. So, also check out
'edlib-uncomment-region.

Customizations:
i should add more of them.  any suggestions/patches very welcome!
i would like edlib to capture most authors' styles, yet still maintain
some automations.. like automatic version-updates..

For now, these are the variables you can change \(in yr .emacs\):

edlib-name-function:
				bind it to a function of your choice
				that determines the name of the
				package based on the name of the
				buffer etc. See also the default..

edlib-author-name/email		you the author

edlib-home-page:		for latest versions.

edlib-license-string:		your preferred license  etc.

edlib-annoying-doc-string:	your default edlib-documentations..

edlib-filename-or-package:
				default = both, so you can delete the
				one you do not like

NOTE:
edlib-name, edlib-found-forward, edlib-found-backward: *NOT*
	    customizable.. any changes you make will be rendered
	    ineffective..

Do not confuse edlib-home-page, your homepage, with <lib>-home-page,
that is the page leading to the latest version of your library.  Of
course, you may choose to have them be the same.. <lib>-home-page
defaults to your page, but you can change it.. If you do change it
inside the defvar, don't forget that it has no effect unless you first
undefine the variable..

\(the only place the 2 variables have the same name is edlib itself!\)

should be platform independent, but Tested on emacs20.3 and now on
emacs21 on a sun sparc station/unix...

")

(defun edlib-commentary ()
  "Provides electric help for function `edlib-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert edlib-commentary) nil) "*doc*"))

;;
;;; History:
;;
;;=============================================================
;;; Code:

(eval-when-compile (require 'cl))

(defvar edlib-version "2.2.0")
(defcustom edlib-doc-buffer "*doc*" "")


(defcustom edlib-annoying-doc-string
  " ... USE M-X EDLIB-DOC OR C-X SPC SPC D HERE...
        TO EDIT DOCUMENTATION EASILY   "
  "When you know what it is, you can change it.
to whatever you like..")


;;;###autoload
(defcustom edlib-name-function
  'edlib-name-default-function
 "Function which determines the name of your package..
Set it to your own *function* if you like..
 The default looks at 'buffer-name."
)


(defmacro edlib-withit (expr &rest rest)
  "Caution: var-capture by its very nature.."
  `(let ((it ,expr))
     ,@rest))



;;;###autoload
(defun edlib-name-default-function ()
  "."
  (ignore-errors
    (setq edlib-name
	  (file-name-sans-extension
	   (file-name-nondirectory (edlib-buffer-name)))))
  
  )

(defvar edlib-name
  "my-package-name"
  "Name of package.
Do not setq this, your changes will have no
effect.. instead, rebind `edlib-name-function' to your desired
function..")


;;;###autoload
(defcustom edlib-author-name "D. Goel"
  "Change this to your own name."
)
;;; 2002-04-15 T10:19:14-0400 (Monday)    Deepak Goel
(defcustom edlib-copyright-author-name "Your Name"
  "This can be a string or a list.. A list implies dual
copyright-holders.. for instance, you may desire two lines of
copyright---one for yourself, and one for Free Software Foundation,
Inc. 

Please do consider assigning a copyright to Free Software Foundation,
Inc..  AFAIK, that ensures more safety for your work.  I
didn't make that the default, so as to now annoy you :\)

"

)
(defcustom edlib-author-email "deego@glue.umd.edu"
  "Change this to your own email.."
)
(defcustom edlib-home-page "http://24.197.159.102/~deego"
  "Change this to your own homepage."
)

(defcustom edlib-license-string
  "
 
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
 \n"

"")


;;;Mon Jan 15 23:42:51 2001
;;;###autoload
(defun edlib-insert-time-stamp ()
  "Inserts the time-stamp in the current file.
Will very seldom be used when the file is first created...  Will be
bound to C-c spc spc t"

  (interactive)
  (require 'time-stamp)
  (beginning-of-line)
  (edlib-ask-and-insert
   (concat ";; Time-stamp: <"
   (time-stamp-string)  ">"))
)

(defcustom edlib-filename-or-package
  'both
  "Whether insert filename or package entries.
For the emacs-lisp archive-entry.
'both		     ==>	  both
'filename	     ==>	  filename only
'package	     ==>	  package only"
)

(defun edlib-ask-and-insert (string
			     &optional string2)
  "Asks whether to insert STRING, and insert STRING2.
If no STRING2 provided, inserts STRING instead.
In the latter case, a newline is appended to inserted chars.

Also returns the result of y-or-n-p..

If string is too long to fit in a `y-or-n-p' it is adequately
formatted.
"
  (if (null string2) (setq string2 (concat string "\n")))
  (let ((aa  (y-or-n-p (concat "Insert " (edlib-shorten string) " ?"))))
    (if aa
	(insert string2 ))
    aa))


;;;###autoload
(defun edlib-insert-archive-entry ()
"Call this at the top of yr buffer..
Or let M-x edlib do it for you..
Note: If your <package> defined <package>-version somewhere, this
function will always update the Version: entry based on that..
for doing that, this function tries to evaluate your package.."
  (interactive)
  (edlib-ask-and-insert (concat ";;; " (edlib-buffer-name) " --- "))
  (edlib-insert-time-stamp)
  (ignore-errors 
    (when (equal major-mode 'emacs-lisp-mode)
      (eval-buffer)))
  (apply edlib-name-function nil)
  (edlib-ask-and-insert
   (mapconcat
    '(lambda (arg)
	      (concat ";; Copyright (C) " 
		      (format "%S " (sixth (decode-time)))
		      arg))
    (if (listp edlib-copyright-author-name)
	edlib-copyright-author-name
      (list edlib-copyright-author-name))
    "\n"
    ))
      

	   
  (edlib-ask-and-insert
    "Insert emacs-lisp-archive-entry"
   (concat
    ";; Emacs Lisp Archive entry\n"
    (if
	(or
	 (equal edlib-filename-or-package 'both)
	 (equal edlib-filename-or-package 'filename))
	 (concat ";; Filename: " (edlib-buffer-name)  "\n"))
    (if
	(or
	 (equal edlib-filename-or-package 'both)
	 (equal edlib-filename-or-package 'package))
	(concat ";; Package: " edlib-name "\n"))
    ";; Author: " edlib-author-name " <" edlib-author-email ">" "\n"
    ";; Version:  "
    (let ((tmpstring
	   (ignore-errors
	     (with-temp-buffer
	       (insert edlib-name)
	       (insert "-version ")
	       (eval-last-sexp 11)
	       "\n"))))
      (if tmpstring
	  tmpstring ""))
    "\n"))
  (edlib-ask-and-insert
   (concat ";; Author's homepage: "  edlib-home-page))
  (if (edlib-ask-and-insert
       ";; For latest version: \n")
   (insert "(defvar " edlib-name "-home-page\n  \""
                  edlib-home-page "\")\n\n"))
  (edlib-ask-and-insert edlib-license-string)
  (edlib-ask-and-insert ";; See also:\n\n")
  (if
      (edlib-ask-and-insert ";; Quick start:")
      (edlib-insert-doc "-quick-start"))
  (if (edlib-ask-and-insert 
       (concat ";;; Introduction:\n"
	       ";; Stuff that gets posted to gnu.emacs.sources\n"
	       ";; as introduction"))
      (edlib-insert-doc "-introduction" nil t
			))
  
  (if (edlib-ask-and-insert ";;; Commentary:"
			    )
      (edlib-insert-doc "-commentary"))
  (edlib-ask-and-insert ";;; History:\n")
  (edlib-ask-and-insert ";;; Bugs:\n")
  (if (edlib-ask-and-insert ";;; New features:")
      (edlib-insert-doc "-new-features"
			))
  (if (edlib-ask-and-insert ";;; TO DO:")
      (edlib-insert-doc "-todo"
			))
  (edlib-ask-and-insert
   (concat "(defvar " edlib-name "-version \"99.99\")\n"))
  (edlib-ask-and-insert";;==========================================")
  (edlib-ask-and-insert ";;; Code:\n")
  (edlib-ask-and-insert 
   (concat 
    "(defgroup " edlib-name "nil \n  \"The group " edlib-name"\""
    "\n   :group" " 'applications)"))
  (edlib-ask-and-insert
   (concat "(defcustom " edlib-name "-before-load-hooks nil \"\""
	   " :group '" edlib-name
	   ")\n"
	   "(defcustom " edlib-name "-after-load-hooks nil \"\""
	   " :group '" edlib-name
	   ")\n"
	   "(run-hooks '" edlib-name "-before-load-hooks)\n"))

)

(defun edlib-insert-doc (docstring &optional extra-doc autoloadp)
  "Internal..
Argument DOCSTRING hh.  EXTRA-DOC"
  (if (not (equal major-mode 'emacs-lisp-mode))
      (edlib-message
       "Warning: Does not look like emacs-lisp-mode"))
  (if (null extra-doc) (setq extra-doc ""))
  (apply edlib-name-function nil)
  (indent-for-tab-command)
  (insert "(defvar " edlib-name docstring "\n")
  (indent-for-tab-command)
  (insert "\""  edlib-annoying-doc-string "\"\n")
  (insert ")\n\n")
  (indent-for-tab-command)
  (when autoloadp (insert ";;;###autoload\n"))
  (indent-for-tab-command)
  (insert "(defun " edlib-name docstring " ()")
  (insert "\n")
  (indent-for-tab-command)
  (insert "\"Provides electric help regarding variable `"
	  edlib-name docstring)
  (insert "'.\"")
  (insert extra-doc)
  (insert "\n")
  (indent-for-tab-command)
  (insert "(interactive)")
  (insert "\n")
  (indent-for-tab-command)
  (insert
   "(with-electric-help\n")
  (indent-for-tab-command)
  (insert "'(lambda () ")
  (indent-for-tab-command)
  (insert "(insert ")
  (indent-for-tab-command)
  (insert   edlib-name docstring)
  (indent-for-tab-command)
  (insert ") nil)")
  (insert " \"" edlib-doc-buffer "\"")
  (insert "))\n\n"))




;;;###autoload
(defun edlib-update-version-info ()
  "Look at variable <package>-version and update the archive-entry.
;;Version:

Suggest that you: \(add-hook 'write-file-hooks
'edlib-update-version-info\) If you do that: then, whenever you save
your file, the variable <file>-version will be read, and the version
info in the emacs-lisp archive-entry (viz. the commented stuff at the
beginning of the file) will be automatically updated..

SHOULD RETURN NIL, since called by write-file-hooks..

"

  (interactive)
  (edlib-ignore-errors (edlib-update-version-info-with-errors))
  nil
)

;;;###autoload
(defun edlib-update-version-info-with-errors ()
  "Internal.."
  (interactive)
  (apply edlib-name-function nil)
  (save-excursion
    (when (string= (file-name-extension (edlib-buffer-name)) "el")
      (with-temp-buffer
	(insert "(makunbound  (quote " edlib-name)
	(insert "-version )) ")
	(eval-last-sexp 1))
      (eval-buffer)
      (goto-char (point-min))
      (if
	  (search-forward "\n;; Version:" nil t)
	  (progn
	    (if
		(member (following-char)
			'(32))
		(forward-char 1)
	      (insert " " ))
	    (unless
		(member (following-char) 
			'(10 13 32 9))
	      (edlib-delete-line))
	    ;;(insert " ")
	    (insert 
	     (let 
		 ((ver-string
		   (with-temp-buffer
		     (insert edlib-name "-version ")
		     (eval-last-sexp 1))))
	       (if (stringp ver-string) ver-string
		 (format "%S" ver-string)))))
	(edlib-message "NOT FOUND VERSION-STRING")))))






;;;###autoload
(defun edlib-insert-file-ender ()
  "Insert optionally, a 'provide and ;;ends here.."
  (interactive)
  (save-excursion
   (goto-char (point-max))
   (apply edlib-name-function nil)
   (let ((provide-string (concat "(provide '" edlib-name ")")))
     (when (y-or-n-p
	    (concat "Insert " provide-string " ? " ))
       (insert "\n\n" provide-string)
       (edlib-message "Please verify end-of-file.. yourself")))
   (goto-char (point-max))
   (edlib-ask-and-insert
    (concat "\n(run-hooks '" edlib-name "-after-load-hooks)\n"))
   (if (search-backward "ends here" nil t)
       (edlib-message "\"ends here\" detected in file.")
     (let ((string (concat "\n\n;;; " (edlib-buffer-name) " ends here\n")))
       (insert string)
       (edlib-message (concat "Inserted: " string)))))
)


;;;###autoload
(defalias 'edlib-checklist 'edlib)

;;;###autoload
(defun edlib ()
  "Main function...
See also documentation for `edlib-insert-archive-entry' regarding package-version.."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (y-or-n-p
	   "Skip first line for file-variables ? ")
      (ignore-errors (forward-line 1)))
    (edlib-insert-archive-entry)
    (goto-char (point-max))
    (when (y-or-n-p
	   "Insert 'provide and \"ends here...\" ? ")
      (edlib-insert-file-ender)))
  (edlib-message "Thanks. Use edlib-doc to edit those DOC-STRINGs...")
  (edlib-message "Now brace for checkdoc!")
  (if (y-or-n-p "Run checkdoc on your document? ")
      (call-interactively 'checkdoc))
  (edlib-message "Done with checkdoc. Use edlib-doc to edit those DOC-STRINGs...")
  )

(defun edlib-message (&rest args)
  (apply 'message args)
  (sit-for 1))

(defvar edlib-doc-found-forward "not customizable")
(defvar edlib-doc-found-backward  "not customizable")
(defvar edlib-buffer-name  "not customizable")

; C-x SPC SPC l..

; suggest you add this to your .emacs
(global-set-key "\C-x  d" 'edlib-doc)
(global-set-key "\C-x  l" 'edlib)
; optional:
; (global-set-key "\C-c\C-e" 'edlib-doc-commit)



;;;###autoload
(defun edlib-doc  ()
  "Easy editing of any strings.

Edits doc-strings inside a buffer, as the user would see it..
The advantage is that you do not have to write \\\" when you want the
user to see \".. you kinda write the wysiwig and this function
generates the needed 'source-code'..
Requires 'thingatpt which comes with Emacs..
If you are found to be not present at a string, this function simply
does nothing.  Do NOT start more than one `edlib-doc' sessions at the
same time..
Caution: If the cursor is right at the ending \" when you invoke
`edlib-doc', it will misjudge the string, as you will be able to
judge from your *edlib* buffer.. DO NOT CONTINUE.. if you go ahead, it
will replace some of your code!!!! with this string...

Can be used to edit *any* strings easily, not just doc-strings."
  (interactive)
  (setq edlib-buffer-name (edlib-buffer-name))
  ;; restore case-fold-search, because of a bug in emacs20.3
  (let ((tmp case-fold-search))
    (save-excursion
      (setq edlib-doc-found-forward
	    (re-search-forward "[^\\]\"" nil t)))
    (setq edlib-doc-found-backward
	  (re-search-backward "[^\\]\"" nil t))
    (setq case-fold-search tmp))
  (when
      (and
       edlib-doc-found-backward edlib-doc-found-forward)
    (goto-char edlib-doc-found-forward)
    (let ((doc-string (buffer-substring (+ edlib-doc-found-backward  1)
					edlib-doc-found-forward)))
      (when (stringp doc-string)
	(let (doc-string-lesser )
	  (with-temp-buffer
	    (insert "(setq doc-string-lesser " doc-string " )")
		 (eval-buffer))
	       (switch-to-buffer "*edlib*")
	       (edlib-kill-region (point-min) (point-max))
	       (text-mode)
	       (insert doc-string-lesser)
	       (local-set-key "\C-c\C-e" 'edlib-doc-commit)
	       (edlib-message "Press C-c C-e when done" )
	       (run-with-idle-timer
		2 nil
		'(lambda () (edlib-message "Press C-c C-e when done")))
	       )))))

(global-set-key "\C-c  c" 'edlib-doc-commit)


;;;###autoload
(defun edlib-doc-commit-no-quotes ()
  ""
  (interactive)
  (edlib-doc-commit 'no-quotes))

;;;###autoload
(defun edlib-doc-commit (&optional arg)
  "Commits the doc-string from doc-buffer to current buffer..
Can be done either by pressing C-c C-e or C-x SPC SPC c.
"
  (interactive)
  (switch-to-buffer "*edlib*")
  (set-text-properties (point-min) (point-max) nil)
  (let ((doc-string (buffer-substring (point-min) (point-max))))
    (switch-to-buffer "*edlib*")
    (if (null arg)
	(progn
	  (edlib-message "Press C-c C-e if not auto-committed..")
	  (run-with-idle-timer 
	   2 nil
	   '(lambda ()
	      (edlib-message
	       "Press C-c C-e if not auto-committed..")))
	  (edlib-kill-region (point-min) (point-max))
	  (insert (format "%S" doc-string))
	  (local-set-key "\C-c\C-e" 'edlib-doc-commit-no-quotes)
	  (goto-char (point-min))
	  (if
	      (or
	       (progn
		 (goto-char (point-min))
		 (search-forward "\(" nil t))
	       (progn
		 (goto-char (point-min))
		 (search-forward "\)" nil t)))
	      (when
		  (y-or-n-p 
		   "Convert \( or \) to \\\( or \\\) ? ")
		(goto-char (point-min))
		(while (search-forward "\(" nil t)
		  (replace-match "\\\(" nil t))
		(goto-char (point-min))
		(while (search-forward "\)" nil t)
		  (replace-match "\\\)" nil t))))    
	  (edlib-doc-commit-no-quotes)
	    )
      (progn
	(kill-buffer "*edlib*")
	(switch-to-buffer edlib-buffer-name)
	(edlib-kill-region (+ 1 edlib-doc-found-backward) edlib-doc-found-forward)
	(insert doc-string)
	))))
	


;;;###autoload
(defun edlib-uncomment-region (&optional number dontremovespace)
  "A better uncomment-region.
comment-region somehow never manages to uncomment more than one \";\",
and if `comment-padding' is set to > 0, there is no way you can
uncomment a region starting with \";;; \".  This function allows you to
do that, and assumes a comment-padding of 1..  The first argument
NUMBER is the number of comments to uncomments..  If second argument
DONTREMOVESPACE is non-nil, will not remove the padding.."
  (interactive "p")
  (if (null number) (setq number 1))
  (let
      ((comment-padding 0))
       (comment-region (mark) (point) (- number)))
  (unless dontremovespace
    (let ((comment-padding 0)
	  (comment-start " "))
      (comment-region (mark) (point) -1)))
)

(defcustom edlib-post-archive-file
  "nnfolder+archive:sent-mail-gnus"
"If you do not want any, make it an empty string."
)

;;;###autoload
(defun edlib-post-source (file)
  "Posts source from FILE to ng.  May not work unless a news-program
is already loaded into emacs.  Tested only with gnus. Patches
welcome. "
  (interactive "f")
  (find-file file)
  (apply edlib-name-function nil)
  (ignore-errors (unload-feature (edlib-string-to-symbol edlib-name)))
  (ignore-errors (load file))
  (let ((one-liner (edlib-get-one-line-description file)))
  (let (
	(edlib-buffer-name (edlib-buffer-name)))
    (switch-to-buffer "*edlib-post*")
    (edlib-kill-region (point-min) (point-max))
    (insert
     "Newsgroups: gnu.emacs.sources \n"
     "Reply-To: " edlib-author-name " <" edlib-author-email ">" "\n"
     "Subject: "
     (if (equal edlib-filename-or-package 'package)
	 edlib-name edlib-buffer-name)
     ))
  (ignore-errors
    (insert (concat " v. " (with-temp-buffer
				(insert edlib-name)
				(insert "-version " )
				(eval-last-sexp 1)))))
  (insert "\n"
"Gcc: " edlib-post-archive-file "\n"
"--text follows this line--\n\n" )

  (edlib-withit
   one-liner
   (when it
     (edlib-ask-and-insert (concat it "\n"))))
      

  (ignore-errors
    (when (y-or-n-p
	   "Include New features ? ")
      (insert
       (concat "\n"
	"\nNEW FEATURES:"
	"\n=============" "\n"
	(with-temp-buffer
	  (insert  edlib-name "-new-features ")
	  (eval-last-sexp 1))))
      (insert
	 "\n-----------------------------------------------------")))
  (ignore-errors
    (when (y-or-n-p
	   "Include Introduction ? ")
      (insert
       (concat "\n"
	"\nINTRODUCTION:"
	"\n============" "\n"
	(with-temp-buffer
	  (insert  edlib-name "-introduction ")
	  (eval-last-sexp 1))))
       (insert
	 "\n-----------------------------------------------------")))
  (switch-to-buffer "*edlib-post*")
  (delete-other-windows)
  (if (y-or-n-p "Include file (else pointer to website) ? ")
      (progn
	(insert "\n----------------CUT HERE -------------------------------\n")
	(insert "\n")
	(insert-file-contents file)
	;;(kill-buffer (file-name-nondirectory file))
	)
    (progn
      (insert "\nThe latest version can be had from\n"
	      "<")
      (ignore-errors
	(insert (with-temp-buffer
		  (insert edlib-name "-home-page ")
		  (eval-last-sexp 1))))
      (insert ">")))
  (message-mode)
  (delete-other-windows)
  ))

(defun edlib-shorten (string)
  "Internal, return a shortened version with no newlines.
Internal, returns a shortened version of STRING with no newlines."
  (let
      ((string-no-enter
	(with-temp-buffer
	  (insert string)
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (replace-match " " nil t))
	  (buffer-substring (point-min) (point-max)))))
    (if (> (length string-no-enter) 65)
	(substring string-no-enter 0 65)
		   string-no-enter)))

(defun edlib-buffer-name ()
  "If file exists, function `buffer-file-name', else  `buffer-name'."
  (if
      (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defun edlib-string-to-symbol (string)
  (if (stringp string)
      (with-temp-buffer
	(insert "(quote " string ")")
	(eval-last-sexp 1))
    (error "Not supplied a string")))



(defmacro edlib-with-read-string (prompt default function)
  "Like read-string, except that does it in a buffer..
   The function is then applied on the string that has been so read.
FUNCTION takes one argument, the string..
"
  `(progn
     (switch-to-buffer "*edlib-read-string*")
     (edlib-kill-region (point-min) (point-max))
     (text-mode)
     (message "%s %s " ,prompt "---press C-cC-e when done")
     (local-set-key
      "\C-c\C-e"
      (lambda ()
	(interactive)
	(let ((text (buffer-substring (point-min) (point-max))))
	  (kill-buffer "*edlib-read-string*")
	  (funcall ,function text)
	  ))) 
     ; returning "" because don't want this to generate spurious
     ; messages when called interactively..
     ""))


(defmacro edlib-ignore-errors(&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i should have done long ago.
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (require 'cl)
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))


(defun edlib-get-one-line-description (file)
  (interactive "f")
  (with-temp-buffer
    (let ((aa (buffer-name))
	  (result nil))
      ;;(switch-to-buffer file)
      (insert-file file)
      (goto-char (point-min))
      (edlib-withit 
       (search-forward-regexp 
	
	(concat "^" ";*" " *" "\\(" (regexp-quote
				     (file-name-nondirectory file) )
		
		"\\)\\|\\("
		(file-name-sans-extension 
		 (file-name-nondirectory file)) "\\)"
		 " *"
		 "--") 
	nil t)
       (setq result
	     (if it
		 (edlib-get-line-contents it)
	       nil)))
      ;;(switch-to-buffer aa)
      (message "%S" result)
      result)))



(defun edlib-get-line-contents (point)
  (save-excursion
    (goto-char point)
    (buffer-substring
     (progn
       (beginning-of-line) (point))
     (progn (end-of-line ) (point)))))


(defun edlib-delete-line (&optional arg)
  "Is not really the counterpart of kill-line.. because the two handle
whitespace differently...  This does NOT delete the new-line character
unless given an argument..."
  (save-excursion
   (unless (eolp)
     (delete-region
      (point)
      (progn
	(end-of-line)
	(point)))))
  (when arg 
    (ignore-errors (delete-char 1))))


(defcustom edlib-add-to-kill-ring-p t "")

(defun edlib-kill-region (&rest args)
  (apply
   (if edlib-add-to-kill-ring-p
       'kill-region
     'delete-region)
   args))


(provide 'edlib)

;;; edlib.el ends here
