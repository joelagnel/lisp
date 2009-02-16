;;; ges.el --- Gnu.Emacs.Sources --> ~/elisp/ retriever. 
;; Time-stamp: <2002-12-19 08:46:50 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: ges.el
;; Package: ges
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords: gnu emacs sources repository elisp site-lisp ell
;; Version: 0.3.2
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst ges-home-page
  "http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/ges/")



 
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
 

;; See also:


;; Quick start:



(defconst ges-quick-start
  "Drop ges.el into load-path.  \(require 'ges\) in .emacs.  Customize
ges-site-lisp and ges-site-lisp-not.  On any article, type M-x ges
\(or type z as below\)

Optionally, also add to .gnus
 \(define-key gnus-summary-mode-map \"z\" 'ges\) 

Finally, customize stuff like `ges-interactivity'. 
When using ges.el pressing q will work most everywhere.

PS: If you see weird problems, your gnus treats multipart articles

")

(defun ges-quick-start ()
  "Provides electric help from variable `ges-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ges-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst ges-introduction
  "Requires: querer.el, and optionally uses mkback.el >=0.4, 
both posted recently. 
 
ges.el helps maintain your local elisp repository when browsing
g.e.sources in gnus.  When browinsg g.e.s libraries, ges.el looks for
previous versions, and shows various context-sensitive choices, like
diff, replace, backup, namspace investigation etc.

Type M-x ges-quick-start.  Tested with gnus 5.9.0 in Emacs 21.2. 
Uses querer.el which needs emacs>=21. 
 

Suggestions for Gnu.Emacs.Sources AUTHORS:

* Make the subject look like: 
   lib[.el] v. 5.7, or 
   lib[.el] version 5.7, or 
   lib[.el] 5.7, or 
   lib[.el] 5.7 --- description  

* Include exactly one elisp file per article, and preferably not as an
  attachment.

"  )

;;;###autoload
(defun ges-introduction ()
  "Provides electric help from variable `ges-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ges-introduction) nil) "*doc*"))

;;; Commentary:
(defconst ges-commentary
  "
Thanks for various hints to:
   Cyprian Laskowski              
   Gareth
   Neil W. Van Dyke               
   Roman Belenov
   Jesper Harder
"
)

(defun ges-commentary ()
  "Provides electric help from variable `ges-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ges-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:


;;; New features:
(defconst ges-new-features
  "Lots of Internal Improvements.  
The new defcustoms in here will not take effect unless emacs
restarted. ")

(defun ges-new-features ()
  "Provides electric help from variable `ges-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ges-new-features) nil) "*doc*"))

;;; TO DO:
(defconst ges-todo
  "Help..."
)

(defun ges-todo ()
  "Provides electric help from variable `ges-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ges-todo) nil) "*doc*"))

(defconst ges-version "0.3.2")

(defun ges-version (&optional arg)
   "Display ges's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "ges version %s" ges-version))
    (ges-message 30 "ges version %s" ges-version)))

;;==========================================

;;; Requires: querer.el, gnus
(eval-when-compile (require 'gnus))
(eval-when-compile (require 'gnus-art))
(eval-when-compile (require 'gnus-sum))
(require 'querer)
(eval-when-compile (require 'cl))
;;; Code:


(defgroup ges nil
  "The group ges."
  :group 'applications)
(defcustom ges-before-load-hooks nil
  "Hooks to run before loading ges."
  :group 'ges)
(defcustom ges-after-load-hooks nil
  "Hooks to run after loading ges."
  :group 'ges)
(run-hooks 'ges-before-load-hooks)

(defcustom ges-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
 rest are for
debugging."
  :type 'integer
  :group 'ges)
(defcustom ges-interactivity 0
  "How interactive to be.  
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'number
  :group 'ges)
(defcustom ges-y-or-n-p-function 'ges-y-or-n-p
  "Function to use for interactivity-dependent  y-or-n-p.
Format same as that of `ges-y-or-n-p'."
  :type 'function
  :group 'ges)
(defcustom ges-n-or-y-p-function 'ges-y-or-n-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `ges-n-or-y-p'."
  :type 'function
  :group 'ges)


(defun ges-sleep-for (points &rest args)
  "Signal message, depending on POINTS andges-verbosity.
ARGS are passed to `message'."
  (unless (< (+ points ges-verbosity) 0)
    (apply #'sleep-for args)))


(defun ges-message (points &rest args)
  "Signal message, depending on POINTS andges-verbosity.
ARGS are passed to `message'."
  (unless (< (+ points ges-verbosity) 0)
    (apply #'message args)))
(defun ges-y-or-n-p (add prompt)
  "Query or assume t, based on `ges-interactivity'.
ADD is added to `ges-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (< (+ add ges-interactivity) 0)
        t
      (funcall 'y-or-n-p prompt)))
(defun ges-n-or-y-p (add prompt)
  "Query or assume t, based on `ges-interactivity'.
ADD is added to `ges-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add ges-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))





;;; Real Code:

(defcustom ges-signature ""
  ""
  :group 'ges)

(defcustom ges-save-heuristic t
  "
Whether to try to avoid multiple recalculations from the same
article.  Articles can be very long, and this helps avoid ges redo all
the work if you invoke ges on the same article in rapid succession. 

Set it to nil if you don't like heuristics.")

(defvar ges-string-message-id nil
  "message-id of the article stored in ges-string. ")

(defvar ges-temp-message-id  nil
  "Info on article last stored to temp directory. This will
be a list of message-id and the full expanded temp-file-name
directory and the length of the ges-string before calling ges-save. 
")


(defcustom ges-prefix-max-length 2
  "" :group 'ges)

(defcustom ges-prefix-def
  ;; match everything including defadvice..
  "\\(?:provide\\|run-hooks\\|def\\(?:al\\|\\|advice\\|c\\|f\\|g\\|ine\\|m\\|s\\|u\\|v\\)\\)"  "" :group 'ges)

;; separators..
(defcustom ges-prefix-sep
  "[ \n\t'`()]" 
  "" :group 'ges
  )

;; parts of symbols..
(defcustom ges-prefix-sym
  "[^ \n\t'`()]" "" :group 'ges)


(defcustom ges-prefix-regexps
  '(list
    ;; should match all defuns etc. etc. 
    (list 
     (format 
      "^(%s%s*%s+\\(%s+\\)"
      ges-prefix-def 
      ges-prefix-sym
      ges-prefix-sep
      ges-prefix-sym
      ) 1)
    (list
     (format "^(defadvice%s+%s+%s+%s+%s+\\(%s+\\)"
	     ges-prefix-sep 
	     ges-prefix-sym
	     ges-prefix-sep
	     ges-prefix-sym
	     ges-prefix-sep
	     ges-prefix-sym)
     1))
  "" :group 'ges)

(defcustom ges-file-name-frob-function
  'ges-force-el 
  ""
  :group 'ges)

(defcustom ges-comment-0-hooks nil
  ""
  :group 'ges)
(defcustom ges-comment-1-hooks nil
  ""
  :group 'ges)

(defcustom ges-max-iterations-beginning 5
  ""
  :type 'integer
  :group 'ges)
(defcustom ges-max-iterations-end 4
  ""
  :type 'integer
  :group 'ges)

(defcustom ges-byte-compile-p 'nil
  "Whether to optionally byte-compile after saving a library. 
When nil, no bye-compilation is done. 
When 'always, byte compilation is done.
Else you are asked whether to compile. 

If this is nil, yet a compiled file previously exists, you are warned
about it and compilation is suggested.

This variable affects the behavior of the function
ges-recompile-maybe, which itself is called in
the customizable ges-after-save-hook-expression. 
"
:group 'ges)

(defcustom ges-diff-switches "-u"
  "nil means the value of diff-switches gets used.."
  :group 'ges) 

(defcustom ges-comment-buffer-function 
  'ges-comment-buffer
  "" :group 'ges)

(defcustom ges-comment-region-function
  'comment-region
  "Function to comment out the signatures, headers etc. 
If you prefer deleting those signatures etc. setq this to
ges-kill-region or ges-delete-region. 
"
  :group 'ges)

;; we want all of them with respect to NEWLINE here, since may get
;; commented and called again
(defcustom ges-comment-beginning-alist
  '(
    (ges-search-forward ";?;?;?\\s-*[-=]*[-=]? ?cut here ?[-=]" t)
    (ges-search-forward (concat "^;;;?\\s-*" 
				(file-name-sans-extension
				 ges-file-name)
				"\\(\\.el\\)\\s-*[-:]") nil)
    (ges-search-forward "^;;;?\\s-*?commentary" nil)
    (ges-search-forward "^;;;?\\s-* ?code" nil)
    (ges-search-forward "^\\s-*(def")
    (ges-search-forward "^\\s-*(require\\s-*")
    (ges-search-forward (concat 
			 ;; "^;;;? ?" 
			 "\\("
			 (file-name-sans-extension ges-file-name)
			 "\\|"
			 "\\.el\\)"
			 "\\s-*starts\\s-*here")
			nil))


  ""
  :group 'ges) 

(defcustom ges-comment-end-alist
  '(
    (ges-search-backward "^-- $" nil )
    (ges-search-backward (concat 
			  ;; "^;;;? ?" 
			  "\\("
			  (file-name-sans-extension ges-file-name)
			  "\\|"
			  "\\.el\\)"
			  "\\s-*ends here") t)
    (ges-search-backward 
     (concat 
      "(provide\\s-*.*" 

      ")")
     t)

    ;;(ges-search-backward 
    ;;  "ends here\\s-*$" t)

    ;(ges-search-backward 
    ; "--=-=" nil)
    )

    "Tries to find an acceptable position for beginning-of-file..
Each element is an expression which should return a point or nil. 
The smallest match is chosen. "
    :group 'ges) 



;; this shuts off compiler warnings, but does not affect
;; compilation-mode-map. 
(defvar compilation-mode-map)

(defvar ges-err nil)

(defmacro ges-ignore-errors-shout (&rest body)
  `(condition-case ges-err (progn ,@body)
     (error
      (ding t)
      (ding t)
      (ding t)
      (ges-message 69 "IGNORED ERROR: %s" (error-message-string ges-err))
      (sit-for 1)
      nil)))

(defmacro ges-ignore-errors (&rest body)
  `(condition-case ges-err (list (progn ,@body) nil)
     (error
      (list nil ges-err))))

(defcustom ges-write-file-hooks-expression 'write-file-hooks
  ""
  :group 'ges)
(defcustom ges-after-save-hook-expression  
  '(if after-save-hook
       (if (listp after-save-hook) (append after-save-hook 
					   (list 'ges-recompile-maybe))
	 (list after-save-hook 'ges-recompile-maybe))
     (list 'ges-recompile-maybe))
  ""
  :group 'ges)


(defcustom ges-mkback-function 'ges-mkback
  "This function should return non-nil upon successful backup
creation. " 
  :group 'ges)

(defcustom ges-groups 
  '("gnu.emacs.sources")
  ""
  :group 'ges)


(defcustom ges-buffer 
  "*GES*"
  ""
  :group 'ges)

(defcustom ges-site-lisp 
  '("~/emacs/emacspub/site-lisp")
  "List of locations where you store downloaded elisp files.
     .. for use in your own emacs. "
  :group 'ges
)


(defcustom ges-site-lisp-not
  '("~/emacs/emacspub/site-lisp-not")
  "Locations for lisp files that are not part of load-path. 

For example, I often store interesting elisp files in site-lisp-not
while pondering over whether to make them part of my regular
load-path.  This is that path. 

Sometimes, I store stuff in site-lisp-not with comments so that
pressing z then g takes me there and reminds me exactly what is so
wrong about that library I want to avoid. 
"
  :group 'ges)
  


(defcustom ges-coding-menu
  '(
    (?p ges-coding-prefix-find-used)
    (?a ges-coding-apropos )
    (?s ges-coding-strange-lines )    
    (?q ges-basic)
    (?X ges-quit)
    )

  "" :type 'group)

(defcustom ges-coding-menu-msg 
  "  p: find prefixes used by this file (heuristic). 
  a: Apropos search if this prefix already exists.
  s: flag possibly riSky/Strange expressions (heuristic). 

  q: quit
  X: exit ges"
  ""
  :type 'group)

(defcustom ges-menu-basic
   '(
     (?M ges-multipart)
     (?n ges-save-site-lisp-not)
     (?s ges-save-site-lisp)
     ;; nope, no menu desired here..
     (?t ges-save-temp 'force-save nil)
     (?V ges-version-current)
     (?X ges-exit)
     (?y ges-coding-menu)
     (?c ges-change-name)
     (?q ges-exit)
     )
   ""
   :group 'ges)

(defcustom ges-menu-basic-msg
  "
  c: Change name/type. 
  M: treat Multipart article (imperfect). 
=>n: save file to site-lisp-Not.
=>s: save file to Site-lisp.
  t: save file to Temporary directory. 
  V: Version-info for current file (not already in subject??)
=>y: coding stYle checks to see if library dangerous!  (imperfect)
  q: quit to main menu
  X: exit"
  ""
  :group 'ges)


(defcustom ges-menu-replace
   '(
     (?b ges-backup)
     (?d ges-diff)
     (?g ges-goto-prior)     
     (?l ges-locate-all-libraries)
     (?p ges-patch)
     (?P ges-patch-with-backup)

     (?r ges-replace)
     (?R ges-replace-with-backup)
     (?T ges-patch-temp)
     (?v ges-version-prior)
     (?x ges-delete)
     )
   ""
   :group 'ges)




(defcustom ges-menu-replace-msg
  "
  b: backup previous file
  d: Save to temporary directory and show Diff between old and new. 
  g: Go to old file
  l: Locate all preexisting files for this library. 
  p: treat the mail as a Patch instead and do the Patch
  P: backup existing file, and Patch it from current mail
=>r: Replace file.  R: backup old file, and replace it with current file. 
  T: copy old file to temporary-directory and Patch it
  v: Version-info of old file. 
  x: delete previous file."
  ""
  :group 'ges)

(defcustom ges-load-path 'load-path
  ""
  :group 'ges)

(defvar ges-window-configuration nil "")

(defvar ges-prior nil
  "Internal.")

(defvar ges-string "" "Internal")

(defcustom ges-search-prior-function
  'ges-search-prior
  ""
  :group 'ges)

(defvar ges-menu
  'ges-menu)

(defvar ges-file-name nil
  "Name of the library..")

(defvar ges-body-point-min nil
  "internal.")


(defun ges-coding-menu  ()
  (interactive)
  (querer ges-coding-menu 
	  ges-coding-menu-msg))

(defun ges-search-backward-string (string1 &rest args)
  "Similar to search-forward, but searches in a string1."
  (let ((aa nil) (bb nil))
    (with-temp-buffer
      (insert string1)
      (goto-char (point-max))
      (apply 'search-backward args))))


(defun ges-coding-apropos ()
  (interactive)
  (ges-coding-apropos1 
   (read-string
    "Prefix: "
    (file-name-sans-extension ges-file-name))))

(defun ges-coding-apropos1 (&optional name arg)
  (unless name
    (setq name 
	  (read-string "Prefix: " 
		       (file-name-sans-extension ges-file-name))))
  ;;(unless name (setq name 
  ;;(file-name-sans-extension ges-file-name)))
  (ges-prefix-search name arg))
  
;;;###autoload
(defun ges-prefix-search (&optional name allmatches )
  (interactive)
  (unless name
    (setq name
	  (read-string "Enter string for ges apropos searches: ")))
  (ges-message 10 "Ges Apropos Computing...")
  (let ((names (list name))
	last-match choices0 choices1 choice0 choice1 thisres rem)
    (while 
	(setq last-match 
	      (ges-search-backward-string (first names) "-" nil t))
      (when (>= last-match 0)
	(add-to-list 'names
		     (substring name 0 (- last-match 1)))))
    
    (if allmatches
	(setq choices0 
	      (list (funcall 'regexp-opt 
			     (list (first (reverse names))) 
			     t)))
      (setq choices0 
	    (mapcar 'regexp-quote (reverse names))))
    (if allmatches
	(setq choices1 
	      (list (concat "^" (car choices0) "\\(-\\|$\\)")))
      ;; not allmatches
      (while choices0
	(setq choices1
	      (cons 
	       (concat "^\\(" (car choices0) "\\)-")
	       (cons
		(concat "^\\(" (car choices0) "\\)$")
		choices1)))
	(setq choices0 (cdr choices0)))
      (setq choices1 (reverse choices1)))
    (setq rem choices1)
    (while (and (null thisres) rem)
      (setq thisres (funcall 'apropos (first rem)))
      (setq rem (cdr rem))
      (ges-sleep-for 25 1))
    (when thisres
      (select-window (get-buffer-window "*Apropos*"))
      (delete-other-windows)
      (if (not allmatches)
	  (querer 
	   `((?A ges-prefix-search ,name t)
	     (?q ges-coding-menu)
	     (?X ges-exit)
	     )
	   "Press A to show all apropos conflicts together. "
	   "")
	(querer
	 `(?q ges-basic)
	 "Press q or X")))))

	



(defun ges-parse-name-version (subject-string)
  "returns a list of subject, version. 
Not fully implemented yet."
  (while (and (> (length subject-string) 0)
	      (member (aref subject-string 0)
		      '(
			20 ;; tab
			32 ;; space
			59 ;; ;
			)))
    (setq subject-string (substring subject-string 1)))
  (let ((strings (split-string subject-string))
	name sans ext) 
    (while (and strings
		(member (downcase (car strings))
			'("re" "re:" "Fwd:" "[FWD]")))
      (setq strings (cdr strings)))
    (setq name (or (car strings) "NOSUBJECT"))
    (setq sans (file-name-sans-extension name))
    (setq ext (file-name-extension name))
    
    ;; take care of such cases as foo.el--- , foo.el: 
    (when (and ext (stringp ext)
	       (not (string-match "diff" ext))
	       (not (string-match "patch$" ext))
	       (string-match "^el" ext))
      (setq ext "el"))
    (when (stringp ext)
      (setq name (concat sans "." ext)))
    (list name)))


(defcustom ges-summary-show-article-option nil
  "t may help fetch parts, but NIL IS good"
  :group 'ges)

(defvar ges-saved-to-tmp-p nil)
(defvar ges-top-level-p nil)

;;;###autoload
(defun ges (&optional arg)
  "Run ges on the news article.  Works only inside *Group* or *Article*. 
The higher or lower the prefix argument, the more or less interactive
the process is. 
NB: This calls (gnus-summary-stop-page-breaking). 
" 
  (interactive "p")
  (unless arg (setq arg 1))
  ;; Thanks Cyprian Laskowski <
  ;;(gnus-summary-stop-page-breaking)
  (let* 
      ((ges-top-level-p t)
       (ges-window-configuration (current-window-configuration))
       (ges-interactivity (+ arg -1 ges-interactivity))
       (sub (gnus-summary-article-subject))
       (name-version (ges-parse-name-version sub))
       (ges-saved-to-tmp-p ges-saved-to-tmp-p)
       (ges-file-name 
	(or (and name-version 
		 (funcall
		  ges-file-name-frob-function
		  (car name-version)))
	    "SHOULDNOTHAVEBEENHERE"))
       ges-header-string 
       ges-body-point-min ges-string)
    (save-window-excursion
      ;; ensure that in summary-mode
      (cond 
       ((equal major-mode 'gnus-summary-mode) (gnus-summary-scroll-up 1))
       ((equal major-mode 'gnus-article-mode) (gnus-article-show-summary))
       (t (error "Need to be either in the Summary or Article buffer.")))
      
      (let (gnus-break-pages)      
	(gnus-summary-show-article ges-summary-show-article-option)
	(gnus-summary-select-article-buffer)
	(gnus-summary-toggle-header -1)
	(message-goto-body)
	(setq ges-body-point-min (point))
	(ges-string-fetch)
	))
    (ges-basic)))


(defun ges-string-fetch ()
"
;; to be called from within ges and called when in the article
buffer.."

  (let ((fetch (gnus-fetch-field "Message-Id")))
    (unless
	(and ges-save-heuristic
	     (stringp fetch)
	     (equal fetch ges-string-message-id))
      (setq ges-string-message-id fetch)
      (setq ges-string 
	    (buffer-substring-no-properties (point-min) (point-max))))))



(defun ges-multipart ()
  (ges-message 3 "Fetching multipart article...done")
  (sleep-for 2)
  ; ges-string should be frobbed here only inside a let so that it is
  ; rebound to normal self when restored..
  (let ((ges-summary-show-article-option t)
	(ges-string nil)
	(ges-string-message-id nil)
	(ges-temp-message-id nil)
	)
    (ges 1)))


(defun ges-change-name ()
  (querer
   '((?c ges-rename)
     (?p ges-choose-prior))
   (concat
    "c: Change the name of the current file\n"
    "p: force the choice of the Pre-existing library to be replaced\n")))


(defun ges-choose-prior ()
  (let ((ges-prior
	 (read-file-name
	  "Enter new location: " (file-name-directory ges-prior)
	  ges-prior nil (file-name-nondirectory ges-prior))))
    (ges-menu)))
   

(defun ges-rename ()
  (let 
      ((ges-file-name
	(ges-read-file-name)))     
    (ges-basic)))

;; the main "driver" of other functions..
(defun ges-basic ()
  (unless ges-top-level-p
    (error "Bug! shouldn't be here.."))
  (let
      (
       (ges-saved-to-tmp-p ges-saved-to-tmp-p)
       ;; why this??
       ;;ges-sans-ext 
       
       ;; make these local variables
       (ges-prior ges-prior)
       (ges-file-name ges-file-name)
       )
    (unless ges-file-name 
      (setq ges-file-name 
	    (ges-read-file-name 
	     "Sorry, could not parse filename.  Enter filename:")
	    ))
    (setq ges-prior (funcall ges-search-prior-function))
    (funcall ges-menu)))


(defun ges-read-file-name (&optional prompt)
  (unless prompt
    (setq prompt "Enter Filename (example: lib.el):  "))
  (setq ges-file-name
	(read-string 
	 prompt
	 (and ges-file-name ges-file-name))))


(defun ges-buffer (arg)
  (interactive "p")
  (unless arg (setq arg 0))
  (let ((ges-interactivity (+ arg -1 ges-interactivity))
	(ges-string
	 (buffer-substring-no-properties (point-min) (point-max))))
    (ges-basic)))



(defun ges-search-prior ()
  "
This weird logic here, to try to locate the file.el even when it is
not in the same directory as file.elc...as happens in debian. 

Note: We want to handle even the case when the author didn't supply
the .el in the subject line. 
"
  (interactive)
  (let* (
	 (sans (file-name-sans-extension ges-file-name))
	 (sans-el (concat sans "." "el"))
	 (strange (not (member
			ges-file-name
			(list sans sans-el)))))

    (or
     (locate-library sans-el  t   (eval ges-load-path))
     (locate-library sans          nil (eval ges-load-path))

     ;; weird case: a strange extension
     (when strange
       (locate-library ges-file-name t (eval ges-load-path)))

     (locate-library sans-el  t   ges-site-lisp-not)
     (locate-library sans          nil ges-site-lisp-not)
     
     (when strange
       (locate-library ges-file-name t ges-site-lisp-not)))))

(defun ges-menu ()
  (require 'querer)
  (let ((str0 ""))
    (when ges-summary-show-article-option
      (setq str0 "TREATING MULTIPART ARTICLE AS ONE..."))
    (if ges-prior
	(querer
	 (append ges-menu-replace ges-menu-basic)
	 (concat
	  (format 
	   "%sLibrary for %S previously exists at %s" str0 
	   ges-file-name ges-prior)
	  ges-menu-replace-msg ges-menu-basic-msg))
      (querer
       ges-menu-basic 
       (concat 
	(format "%s No previous library found for file %S.\n\n" 
		str0 ges-file-name)
	ges-menu-basic-msg)))))



(defun ges-save (dir &optional overwrite)
  "MSG is no use as of now.."
  (let 
      ((dest
	(expand-file-name ges-file-name dir)))
    (switch-to-buffer ges-buffer)
    (delete-region (point-min) (point-max))
    (insert ges-string)
    (if
	(or 
	 (funcall ges-comment-buffer-function)
	 (not
	  (ges-y-or-n-p 
	   175
	   "I think no valid elisp file found. Cancel action now?")))
	(progn
	  (save-excursion
	    (goto-char (point-min))
	    (insert (eval ges-signature)))
	  (ges-write-file dest)
	  (ges-view-mode)
	  (ges-message 1 "Press e to exit view-mode, q to quit.")
	  (ges-sleep-for 1 1)
	  )
      ;; else try to revert to where we were..
      (progn
	(set-window-configuration ges-window-configuration)
	(error "Invalid elisp source.  Could not save to %s" dest)))))


(defun ges-view-mode ()
  (view-mode-enter nil 'kill-buffer))

(defun ges-patch-with-backup ()
  (ges-backup)
  (ges-patch))

(defun ges-patch-temp ()
  (ges-patch-basic t))

(defcustom ges-shell-buffer 
  "*ges-shell*"
  ""
  :group 'ges)

(defcustom ges-patch-command 
  '(format "patch %s < %s" target patch)
  ""
  :group 'ges)

(defun ges-patch-basic (&optional temp)
  (let*
      ((target ges-prior)
       (ext (file-name-extension target))
       (dir (file-name-directory target))
       (fileext (file-name-nondirectory target))
       (dirfile  (file-name-sans-extension target))
       tmpname
       patch
       )
    (when (member ext '("elc"))
      (if (plusp (+ ges-interactivity 90))
	  (setq target
		(read-file-name "File is elc! Change/confirm: "
				dir fileext nil fileext)))
      (setq target 
	    (concat dirfile ".el"))
      (setq ext (file-name-extension target)
	    fileext (file-name-nondirectory target)
	    dir (file-name-directory target)
	    dirfile (file-name-sans-extension target)))
    (when temp
      (setq tmpname
	    (expand-file-name fileext temporary-file-directory))
      (copy-file target tmpname t)
      (setq target tmpname)
      (setq dir temporary-file-directory)
      (setq dirfile (file-name-sans-extension target)))

    (setq patch 
	  (expand-file-name (concat dirfile ".patch") 
			    temporary-file-directory))
    (with-temp-file
	patch
      (delete-region (point-min) (point-max))
      (insert ges-string))
    (switch-to-buffer ges-shell-buffer)
    (ges-buffer-local-set-keys 
     '("q" ges-kill-buffer-current-and-restore))
    (shell-command
     (eval ges-patch-command)
     t)))
	
(defun ges-patch ()
  (ges-patch-basic))
  

(defun ges-save-site-lisp ()
  (ges-save (car ges-site-lisp) ))

(defun ges-save-temp (&optional force showmenu)
  (unless
      (and (null force)
	   (and ges-saved-to-tmp-p
		(file-exists-p 
		 (expand-file-name ges-file-name
				   temporary-file-directory))))
    (progn
      ;; this setq should only take effect within a let in another
      ;; function. 
      (setq ges-saved-to-tmp-p t)
      (let ((thisinfo
	     (list ges-string-message-id (length ges-string)
		   ges-file-name temporary-file-directory)))
	(unless
	    (and ges-save-heuristic (null force)
		 (equal ges-temp-message-id
			thisinfo))
	  (setq ges-temp-message-id thisinfo)
	  (ges-save temporary-file-directory t)))


      ;; return to ges-basic, since this may not be the end..
      (when showmenu (ges-basic)))))

(defun ges-save-site-lisp-not ()
  (ges-save (car ges-site-lisp-not) ))

(defun ges-backup ()
  (funcall ges-mkback-function))


(defun ges-delete (&optional backup delete-file)
  (unless delete-file (setq delete-file ges-prior))
  (ges-message 30 "Deleting file %S" delete-file)
  (if backup
      (unless (funcall 'ges-backup)
	(error "Could not create backup")))
  (delete-file delete-file)
  (ges-message 30 "Deleting file %S..done" delete-file)
  (ges-sleep-for 25 1))

(defun ges-backup-delete ()
  (ges-delete t))

(defun ges-replace (&optional backup)
  (if backup
      (ges-backup-delete)
    (ges-delete))
  (ges-save (file-name-directory ges-prior) 
	    ))

(defun ges-replace-with-backup ()
  (ges-replace t))


;; not really meant to be used now.. see quick start 
(defun ges-gnus-article-prepare-hook ()
 "If you do use it, 'gnus-article-prepate-hook is the place to put it.."
 (when 
     (member gnus-newsgroup-name 
	     ges-groups)
   (ges 1)))

;; this variable introduced just to shut off compiler warning.. We do
;; not want to (require 'mkback) in this file. 
(defvar ges-mkback-actual-function 'mkback)
(defun ges-mkback ()
  (require 'mkback)
  ;; trying this to shut off compiler warning..
  (save-window-excursion
    (ges-ignore-errors-shout
     (funcall ges-mkback-actual-function (ges-file-el ges-prior)))))


(defun ges-file-el (name)
  (if (equal (file-name-extension name) "elc")
      (concat (file-name-sans-extension name) ".el")
    name))

(defun ges-force-el (name)
  (concat (file-name-sans-extension name) ".el"))


(defun ges-diff ()
  ;; no window excursion here please..
  (let ((ges-interactivity (- ges-interactivity 69))
	result)
    (ges-save-temp)
    (ges-message 1 nil)
    (setq result
	  (diff (ges-file-el ges-prior)
		(expand-file-name ges-file-name temporary-file-directory)
		ges-diff-switches))
    (select-window (get-buffer-window "*diff*"))
    (delete-other-windows)
    ;;(diff-mode )
    ;; because this is what diff seems to use by default
    (ges-buffer-local-set-keys 
     '("q" ges-kill-buffer-current-and-restore
       ;;"x" view-mode-exit
       ))
    result))


(defun ges-write-file (name &rest args)
  (let 
      ((write-file-hooks (eval ges-write-file-hooks-expression))
       (after-save-hook (eval ges-after-save-hook-expression)))
    (ges-message 30 "Writing file to %S..." name)
    (apply 'write-file name args)
    (ges-message 30 "Writing file to %S...done" name)
    (ges-sleep-for 25 1)))




;;;====================================================
;; this section deals with the funky commenting part..


(defun ges-search-forward (regexp &optional linep)
  "Searches forward for REGEXP.  
When match found, goes to the beinning of line (when LINEP nil) or end
of line (otherwise) and returns the point there.  "
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp regexp nil t)
	  (progn (if linep (end-of-line) (beginning-of-line)) (point))
	nil))))
	
	
(defun ges-search-backward (regexp &optional linep)
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-max))
      (if (search-backward-regexp regexp nil t)
	  (progn
	    (if linep (end-of-line) (beginning-of-line)) (point))
	nil))))
	




(defun ges-comment-beginning  (criteria)
  (ges-comment-criteria criteria '< '(point-min)))

(defun ges-comment-end (criteria)
  (ges-comment-criteria criteria '> '(point-max)))

(defun ges-comment-criteria (criteria compare-fn till)
  "comment the region and return the modified criteria.. the
triggerred criterion should be removed...
If none of the criteria match, it returns nil. (because all criteria
are useless now...!). "
  (let ((results (mapcar 'eval criteria))
	newcriteria bestpos bestresult
	(ctr -1)
	(ctrmax (- (length criteria) 1)))

    (while 
	(< ctr ctrmax)
      (setq ctr (+ ctr 1))
      (let ((thisans (nth ctr results)))
	(when (and (numberp thisans) 
		   (or (null bestresult) (funcall compare-fn
						  thisans bestresult)))
	  (setq bestpos ctr)
	  (setq bestresult thisans))))
    (if (numberp bestresult)
	(progn
	  (ges-ask-and-comment (eval till) bestresult bestresult)
	  (append
	   (subseq 
	    criteria 0 bestpos)
	   (subseq criteria (+ bestpos 1))))
      nil)))

(defcustom ges-comment-strange-stuff 
  '("^--=" ;; a multipart demarker..
     "^__"     ;; this must be a demarcer..
     )
  ""
  :group 'ges)


(defun ges-comment-strange-stuff (regexp)
  (let ((last-pt (point-min)))
    (while
	(progn
	  (goto-char last-pt)
	  (search-forward-regexp regexp nil t))
      (ges-ask-and-comment
       (progn (beginning-of-line) (point))
       (progn (end-of-line) (setq last-pt (point)))))))


(defun ges-comment-buffer ()
  (let ((beglist (copy-tree ges-comment-beginning-alist))
	(endlist (copy-tree ges-comment-end-alist))
	donebeg doneend)
    (delete-other-windows)
    (goto-char (point-max))
  
    
    ;; this helps assure proper ges-comment-end-p
    (insert "\n")
    (run-hooks 'ges-comment-0-hooks)
    (ges-ask-and-comment (point-min) ges-body-point-min)
    (run-hooks 'ges-comment-1-hooks)
    
    (unless (ges-decent-beginning-p)
      (setq beglist (ges-comment-beginning beglist)))
    ;; not good to test (ges-decent-end-p) the very first time.. since
    ;; signatures often contain elisp or parens..
    (setq endlist (ges-comment-end endlist))

    ;; next, try to comment some strange stuff.. should we skip this step??
    (mapcar 'ges-comment-strange-stuff ges-comment-strange-stuff)
    
    (let ((counter 0))
      (while (and (null donebeg) (< counter ges-max-iterations-beginning))
	(setq counter (+ counter 1))
	(setq donebeg (ges-decent-beginning-p))
	(unless donebeg
	  (setq beglist (ges-comment-beginning beglist)))))
    (let ((counter 0))
      (while (and (null doneend) (< counter ges-max-iterations-end))
	(setq counter (+ counter 1))
	(setq doneend (ges-decent-end-p))
	(unless doneend (setq endlist (ges-comment-end endlist)))))

    (and donebeg doneend)))

(defun ges-decent-beginning-p ()
  (interactive)
  (goto-char (point-min))
  (ges-ignore-errors (forward-sexp 1))
  (ges-decent-sexp-p (sexp-at-point)))

(defun ges-decent-sexp-p (sexp)
  (and sexp 
       (listp sexp)
       (not 
	(member
	 (car sexp)
	 (list 'quote (quote \`))))))

(defun ges-decent-end-p ()
  (interactive)
  (goto-char (point-max))
  (ges-ignore-errors (backward-sexp 1))
  (ges-decent-sexp-p (sexp-at-point)))

(defun ges-ask-and-comment (a b &optional dest)    
  (goto-char (or dest a))
  (replace-highlight a b)
  (when (ges-y-or-n-p 1
		      "Comment this region? ")
    (ges-ignore-errors
     (funcall ges-comment-region-function a b 3)))
  (replace-dehighlight))


(defun ges-goto-prior ()
  (find-file 
   (ges-file-el ges-prior))
  (delete-other-windows)
  (view-mode-enter nil 'kill-buffer))  


(defun ges-kill-region (a b &rest args)
  (kill-region a b))

(defun ges-delete-region (a b &rest args)
  (delete-region a b))




;;;====================================================
(defun ges-locate-all-libraries ()
  (let* 
      ((sans (file-name-sans-extension ges-file-name))
       (fileel (concat sans "." "el"))
       (fileelc (concat sans "." "elc"))
       (strange (not 
		 (member ges-file-name (list fileel fileelc))))
       (results
	(apply 'append
	       (ges-locate-all-paths fileel load-path)
	       (ges-locate-all-paths fileelc load-path)
	       (when strange
		 (ges-locate-all-paths ges-file-name load-path))
	       (ges-locate-all-paths fileel ges-site-lisp-not)
	       (ges-locate-all-paths fileelc ges-site-lisp-not)
	       (when strange
		 (ges-locate-all-paths ges-file-name ges-site-lisp-not))
	       )))
    (querer
     nil
     (format "%S result(s):\n%s"
	     (length results)
	     (mapconcat 'identity results "\n"))
     "Press q ")))

(defun ges-locate-all-paths (file path &optional appendto)
  "All results are appended to the APPENDTO.  "
  (if path
      (let* ((dir (car path))
	     (dirfile (expand-file-name file dir))
	     restpaths 
	     (appendnew appendto))
	(if (file-exists-p dirfile)
	    (unless (member dirfile appendnew)
	      (setq appendnew (append appendnew (list dirfile)))))
	(ges-locate-all-paths file (cdr path) appendnew))
    appendto))


(defun ges-version-prior ()
  (ges-show-version ges-prior "Previous file's version: "))

(defun ges-version-current ()
  (save-window-excursion
   (let ((ges-interactivity (- ges-interactivity 69)))
     (ges-save-temp))
   (ges-show-version (expand-file-name ges-file-name
				       temporary-file-directory)
		     "Current file's version: ")))



(defcustom ges-version-search-strings 
  '(
    (ges-search-forward "^[ \t;]*Version\\s-*: ")
    (ges-search-forward "^Version\\s-*: ")
    (ges-search-forward "Revision\\s-*: ")
    (ges-search-forward ":version")
    (ges-search-forward ":revision")
    (ges-search-forward (format "def.?.?.?.?.?.?.?%s-version" 
				(file-name-sans-extension
				 (file-name-nondirectory file))))
    ;; if nowhere else, see if the last file had a Subject: line
    ;; commented out.. just return that.. that sometimes contains some
    ;; version info:
    (ges-search-forward "^[ \t;]*Subject:"))

  
  ""
  :group 'ges)

(defun ges-show-version (file msg)
  (unless msg (setq msg ""))
  (save-window-excursion
    (find-file file)
    (let ((pt nil)
	  (rem ges-version-search-strings))
      (while (and (null pt) rem)
	(setq pt (eval (car rem)))
	(setq rem (cdr rem)))
      (if (null pt)
	  (querer nil "Couldn't figure out version. ")
	(querer nil 
	       (concat msg "\n"
		       (progn
			 (goto-char pt)
			 (buffer-substring-no-properties
			  (progn (beginning-of-line) (point))
			  (progn (end-of-line) (point)))))
	       "Press q ")))))




(defun ges-recompile-maybe ()
  (interactive)
  (let* ((dir-file-el (buffer-file-name))
	 (dir-file (file-name-sans-extension dir-file-el))
	 (dir-file-elc (concat dir-file "." "elc")))
    
    (cond
     ((equal ges-byte-compile-p 'always)
      (byte-compile-file dir-file-el))
     ((file-exists-p dir-file-elc)
      (querer 
       `((?c byte-compile-file ,dir-file-el)
	 (?x ges-delete nil ,dir-file-elc))
       (concat
	(format 
	 "Warning: A compiled file also exists in the same directory: %s !!\n"
	 dir-file-elc)
	(format "c: Compile current file %s\n" dir-file-el)
	(format "x: Remove the compiled file %s\n" dir-file-elc))))
     (ges-byte-compile-p
      (querer 
       `((?c byte-compile-file ,dir-file-el)
	 (?x ges-delete nil ,dir-file-elc))
       (concat
	(format 
	 "Warning: A compiled file also exists in the same directory: %s !!\n"
	 dir-file-elc)
	(format "c: Compile current file %s\n" dir-file-el)
	(format "x: Remove the compiled file %s\n" dir-file-elc))))
     (t nil))))

      


(defun ges-buffer-local-set-keys (keybindings)
  "Odd elements of keybindings are keys and eevn elements are lists.."
  (use-local-map (copy-keymap (current-local-map)))
  (let ((foo keybindings))
    (while foo
      (local-set-key (car foo) (cadr foo))
      (setq foo (cddr foo)))))

(defun ges-kill-buffer-current-and-restore ()
  (interactive)
  (kill-buffer (current-buffer))
  (when (window-configuration-p ges-window-configuration)
    (set-window-configuration ges-window-configuration)))

(defun ges-exit ()
  (ges-message 1 "Exiting ges"))
(defun ges-noop ()
  nil)



(defun ges-save-temp-less-interactive ()
  (let ((ges-interactivity (- ges-interactivity 175)))
    (ges-save-temp)))
;;;====================================================
;; -coding- stuff..




(defun ges-coding-prefix-find-used ()
  (ges-save-temp-less-interactive)
  (let ((matches nil)
	(prefixes nil)
	(regexps
	 (eval ges-prefix-regexps))
	thisreg
	thismatchnum
	strings 
	(tmpfile 
	 (expand-file-name ges-file-name temporary-file-directory)))
    (find-file tmpfile)
    (ges-view-mode)
    (save-excursion
      (while regexps
	(setq thisreg (caar regexps))
	(setq thismatchnum (second (first regexps)))
	(goto-char (point-min))
	(while
	    (search-forward-regexp 
	     thisreg
	     nil t)
	  (add-to-list 'matches 
		     ;;(match-string 1)
		     (buffer-substring-no-properties
		      (match-beginning thismatchnum)
		      (match-end thismatchnum)))
		     )
	(setq regexps (cdr regexps))))
    (setq strings (mapcar
		   'ges-prefix-first-few
		   (reverse matches)))
    (setq prefixes
	  (remove nil
		  (apply
		   'append
		   (ges-mapcar*
		    'ges-set-union
		    strings))
		  
		  ))
    (querer
     nil
     (with-temp-buffer
       (insert "Namespaces used by this file: "
	    (mapconcat '(lambda (str) (concat str ""))
		       prefixes ", ")
	    "   ..done")
       (auto-fill-mode 1)
       (fill-region-as-paragraph (point-min) (point-max))
       (buffer-string)))))
  
(defun ges-set-union (&rest elements)
  "this functino shall preserve order.."
  (remove-duplicates elements :test 'equal))

(defun ges-mapcar* (fcn lists)
  "will NOT end until all lists run out.."
  (cond
   ((null lists)
    nil)
   ((null (first lists))
    (ges-mapcar* fcn (cdr lists)))
   (t
    (cons
     (apply fcn (mapcar 'car lists))
     (ges-mapcar* fcn 
		  (mapcar 'cdr lists))))))




(defun ges-prefix-all-possible (prefix)
  (let (pt)
    (append
     (with-temp-buffer
       (insert prefix)
       (goto-char (point-max))
       (setq pt (search-backward "-" nil t))
       (if pt
	   (ges-prefix-all-possible (buffer-substring (point-min) 
							pt))
	 nil))
     (list prefix))))
   


(defcustom ges-coding-strange-lines 
  '(

    ;; [1]
    ;; highlight top-level (require 'cl*...)
    ((search-forward-regexp "^(require[ \t'`\n]+cl" nil t)
     t  "Require cl: " )
    ;; [2]
    ;; highlight defadvices
    ((search-forward-regexp "[ \t()'`\n]defadvice[ \t()'`\n]" nil
			   t)
     t "Defadvice: ")


    ;; [3] 
    ;; highlight top-level non-defuns..
    (
     (search-forward-regexp 
      "^(" nil t)
     (not
      (or 
       ;; no defadvice because handled later
       ;; defun defmacro define defvar defcustom defalias
       ;; (regexp-opt '("defun" "defgroup" "defconst" "defmacro" "define" "defvar" "defcustom" "defalias" "defgroup" "defconst" "defface"))
       
       (looking-at 
	
	"def\\(?:alias\\|c\\(?:onst\\(?:\\)?\\|ustom\\)\\|face\\|group\\(?:\\)?\\|ine\\|macro\\|subst\\|un\\|var\\)" )
       
       ;;(looking-at "def")
       (looking-at "provide[']?")
       (looking-at "require[']?")
       (looking-at "run-hooks[']?")
       (looking-at "easy-m")
       (looking-at "eval-when-")
       ))
     "Top-level non-defuns: ")


    ;; [4]
    ;; highlight non-top-level defuns
    ((search-forward-regexp 
     "^[ \t][^;\n]*(def\\(?:alias\\|c\\(?:onst\\(?:\\)?\\|ustom\\)\\|face\\|group\\(?:\\)?\\|ine\\|macro\\|un\\|var\\)"
     ;;"^[ \t][^;\n]*(def" 
     nil t)
     t "Non-top-level Defun: "
     )
    )
  "" 
  :group 'ges)

(defun ges-coding-strange-lines ()
  (ges-save-temp-less-interactive)
  (let (
	(tmpfile 
	 (expand-file-name ges-file-name temporary-file-directory))
	(exprs ges-coding-strange-lines)
	expr
	search show msg
	)
    (find-file tmpfile)
    (ges-view-mode)
    (ges-message 1 "Highlighting strange top-level lines")
    (delete-other-windows)
    
    (while exprs
      (setq expr (first exprs))
      (setq search (first expr))
      (setq show (second expr))
      (setq msg (or (third expr) ""))
      (ges-message -20 msg)
      (sit-for 0)
      (ges-sleep-for -20 1)
      (goto-char (point-min))
      (while
	  (eval search)
	(when (eval show) (ges-highlight-line msg)))
      (setq exprs (cdr exprs)))
    (replace-dehighlight)
    (ges-message 1 "Done finding possibly strange expressions. ")))

(defun ges-highlight-line (&optional msg)
  (let ((quote (concat (if (stringp msg) (concat msg " ") "")
		       "Press SPC (other keys: e, q): ")))
    (replace-dehighlight)
    (replace-highlight
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point)))
    (querer '((?\ ges-noop)
	      (?q (lambda () (error "User quit")))
	      ) nil quote)))
	 


(defun ges-prefix-first-few (str)
  (let ((ctr 0)
	(results nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (and (not (eobp))
		  (< ctr ges-prefix-max-length))
	(unless (search-forward "-" nil t)
	  (goto-char (point-max)))
	(setq ctr (+ ctr 1))
	(push (buffer-substring-no-properties (point-min) (point))
	      results)))
    (reverse results)))

(provide 'ges)
(run-hooks 'ges-after-load-hooks)



;;; ges.el ends here
