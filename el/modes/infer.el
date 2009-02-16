;;; infer.el --- infer major and minor modes from file contents
;;
;; Author: Michelangelo Grigni <mic@mathcs.emory.edu>
;; Created: 25 January 1989
;; Keywords: modes, files
;; X-Credits: idea from file of same name by Ashwin Ram (elisp archive)
;; X-URL: ftp://ftp.mathcs.emory.edu/pub/mic/emacs/
;; Time-stamp: <97/09/21 09:18:20 mic>


;;; Commentary:
;;
;; This package provides a mechanism to infer the mode of files from
;; their contents.  By default, it only looks at files that were not
;; given a mode by the usual mechanisms (`auto-mode-alist', etc).
;; This file installs itself on `find-file-hooks'.  To use it, just
;; load it with a line like the following in your .emacs:
;;
;; (require 'infer)
;;
;; However, this file does nothing as configured.  You will need to
;; add entries to `infer-mode-alist' and `infer-minor-alist'.  Two
;; possible methods:
;;
;; (1) Many example methods appear as comments in the defvar
;; declarations of those two variables.  You could "uncomment" the
;; features that you like, and then load your modified infer.el (since
;; these are `defvar' declarations, they will have no effect after the
;; first time you load this file).
;;
;; (2)  Set the entries that you like in your .emacs file, 
;; and do not modify infer.el at all.  Something like:
;;
;; (setq infer-mode-alist
;;       '(
;;         ("\\`From \\(.+\n\\)+X-VM-" vm-mode)
;;         ("\\`#! ?/[a-z/]*bin/[a-z]*sh\\>" sh-mode)
;;         ("\\`%!\\($\\|PS-Adobe\\)" postscript-mode)
;;         ("\\`\\(This is \\)?Info file:? .*," infer-info-buffer)
;;         ("\\`\\'" infer-empty) ; see infer-empty-alist
;;        ))
;; (setq infer-minor-alist
;;       '(
;;         ("^\f" (paged-mode 0))
;;         ("^\\*[ \t]" (outline-minor-mode 1))
;;         ))

;;; Technical Commentary:
;;
;; Emacs calls normal-mode (which uses `auto-mode-alist',
;; `inhibit-first-line-modes-regexps', and `interpreter-mode-alist')
;; before this package (see files.el for details).  In particular,
;; `interpreter-mode-alist' replaces a few things that were handled by
;; earlier versions of infer.el.

;;; History:
;;
;; 07/15/97: commented out all alist entries, added Commentary, added
;;           default check for (get major-mode 'mode-class)
;; 03/07/96: support for folding.el <http://www.csd.uu.se/~andersl/emacs.shtml>
;; 07/10/95: fix some local local assumptions: default-major-mode, info-file,
;;           dired-virtual-mode.  Also, buf should be set to a buffer rather
;;           than string.  Thanks to Dima <D.Barsky@ee.surrey.ac.uk>.
;; 06/30/95: updated comments, including `interpreter-mode-alist'
;; 02/01/95: added infer-minor-* for minor modes, punted v18 support
;; 08/24/94: replaced infer-mode-overrides list by infer-mode-ok form
;; 08/04/94: renamed to infer-mode, made other functions non-commands
;; 04/22/94: updated for emacs19
;; 03/14/94: added test for trivial major-mode
;; 02/04/94: more infer-* commands, vm-visit-buffer-ask
;; 02/25/93: case sensitive, infer-info, prevent infinite loop
;; 11/08/91: retrieved, modified slightly
;; 01/25/89: version from archive, by Ashwin Ram <Ram-Ashwin@yale.ARPA>

;; Todo:
;;
;; Get rid of infer-mode-empty, it seems rarely useful.



;;; Variables

(defvar infer-mode-ok
  '(or (eq major-mode default-major-mode)
       (memq major-mode '(fundamental-mode text-mode)))
  ;; I allow text-mode for info files that typically say "-*- Text -*-".
  "If this form evaluates to nil, infer-mode ignores `infer-mode-alist'.
This is ignored when infer-mode is called interactively.")

(defvar infer-minor-ok 
  '(not (get major-mode 'mode-class))
  "If this form evaluates to nil, infer-mode ignores `infer-minor-alist'.
This is ignored when infer-mode is called interactively.")

(defvar infer-mode-alist
  ;; Call only first matching action (a buffer has a single major-mode):
  '(
    ;; vm:
    ;; Choose one:
    ;; ("\\`From \\(.+\n\\)+X-VM-" vm-mode) ; does not check inboxes
    ;; ("\\`From \\(.+\n\\)+X-VM-" (vm (buffer-file-name))) ; checks inboxes
    
    ;; rmail:
    ;; ("\\`BABYL OPTIONS:$" (rmail (buffer-file-name)))
    ;; Note: recent (> v19.22) rmail buffers have "-*-rmail-*-"

    ;; perl, sh:
    ;; These are only useful in old Emacs.  Modern X/Emacs handle this
    ;; and other common interpreters with `interpreter-mode-alist':
    ;; ("\\`#!.*perl\\>" perl-mode)
    ;; ("\\`#! ?/[a-z/]*bin/[a-z]*sh\\>" sh-mode)

    ;; hexl-mode on some common executables:
    ;; ("\\`\\(\201\^c\^a\\|L\^A\^H\\|\^?ELF\\)" hexl-mode)

    ;; Nonstandard, emacs itself as an interpreter (it can be done!):
    ;; ("\\`#!.*/.?emacs\\>" emacs-lisp-mode)

    ;; hyperbole koutline files, not tested in a long time:
    ;; ("\\`\"Kotl-3\\.[0-9]\";; file-format\n\C-_" kotl-mode)

    ;; Old fashioned sh scripts, starting with just a colon:
    ;; ("\\`:[ \n]+[#$]" sh-mode)

    ;; Postscript files:
    ;;("\\`%!\\($\\|PS-Adobe\\)" postscript-mode)

    ;; Info files, see defun below:
    ;; ("\\`\\(This is \\)?Info file:? .*," infer-info-buffer)

    ;; edb database files, not tested in a long time:
    ;; ("\\`;; Database file written by EDB" db-this-buffer)

    ;; Saved dired buffers, usually caching listing of a remote system (Dima):
    ;; ("\\`  \\(\\(/[^/ \t\n:]+\\)+:\\)?\\(/[^/ \t\n:]+\\)+:$" dired-virtual-mode)

    ;; LaTeX files with nonstandard suffixes (chapters, etcetera):
    ;; ("^\\\\\\(documentstyle\\|chapter\\|\\(sub\\)?section\\)" latex-mode)
   
    ;; An empty file!  Maybe new, see the defun and its alist below:
    ;; ("\\`\\'" infer-empty)

    ;; These remaining entries are near the end of the list, to
    ;; control their rabidity:

    ;; Incomplete outgoing mail messages (saved a *mail* buffer):
    ;; ("\\`To: " mail-mode)

    ;; A Standard Unix mail file, ask user whether to use VM:
    ;; ("\\`From .*\n[-A-Za-z]+: " infer-vm-buffer-ask)

    ;; Probably an nroff file (typically manual pages or papers):
    ;; Seen: .ds .so .OH .EH .TH .\ .\" 'sp
    ;; ("\\`\\(\\.\\\\\\|[.'][A-Za-z][A-Za-z][ \t\n]\\)" nroff-mode)

    ;; Binary encrypted PGP, but this function was never written.
    ;; Try crypt.el instead.
    ;; ("\\`\204" infer-pgp-decrypt) ; not implemented!
    )
  "Alist of (REGEXP ACTION) pairs used by infer-mode.
Calls *first* ACTION corresponding to a case-sensitive match of a
REGEXP in the first `infer-mode-limit' chars of the buffer.
ACTION may be a function symbol or a form to evaluate."
  ;; For backward compatibility, also accepts (REGEXP . FUNSYM),
  ;; like in `auto-mode-alist' and the old code by Ashwin Ram.
  )

(defvar infer-minor-alist
  ;; Call every matching action (a buffer may have multiple minor modes):
  '(
    ;; DOS line endings.  This uses my dos.el file (same FTP site):
    ;; ("\\`.*\r\n" (dos-hiding 'maybe))

    ;; A paged buffer.  This uses my paged.el minor mode (same FTP site):
    ;; ("^\f" (paged-mode 0))

    ;; Outline minor mode (standard in modern X/Emacs):
    ;; ("^\\*[ \t]" (outline-minor-mode 1)) ; this regexp is too rabid

    ;; folding minor mode (http://www.csd.uu.se/~andersl/emacs.shtml):
    ;; ("^.?.?.?{{{" (folding-mode-find-file))
    )
  "Alist of (REGEXP ACTION) pairs used by infer-mode.
Calls *every* ACTION corresponding to a case-sensitive match of a
REGEXP to the first `infer-mode-limit' chars of the buffer.
ACTION may be a function symbol or a form to evaluate.")

(defvar infer-mode-limit 2000
  "Number of characters checked by infer-mode, or nil for whole buffer.")


;;; The main command: infer-mode

(defun infer-mode nil
  "Infer major-mode and minor modes from buffer contents.
Typically called from `find-file-hooks', but also interactively.
See the various infer-* variables, in particular `infer-mode-alist'
for the major modes and `infer-minor-alist' for the minor modes."
  (interactive)
  ;; Do major modes first, since they usually reset local variables.
  (let ((old-mode major-mode)
        (todo '((infer-mode-alist  infer-mode-ok  t)
                (infer-minor-alist infer-minor-ok nil)))
        alist first-only case-fold-search)
    (while todo
      (setq alist (and (or (interactive-p) (eval (eval (nth 1 (car todo)))))
                       (eval (car (car todo))))
            first-only (nth 2 (car todo))
            todo (cdr todo))
      (while alist
        (setq alist
              (if (save-restriction
                    (widen)
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward
                       (car (car alist)) infer-mode-limit t)))
                  (let ((action (cdr (car alist)))
                        ;; Avoid a possible infinite loop?
                        infer-mode-alist infer-minor-alist)
                    ;; Be compatible with `auto-mode-alist' format
                    ;; used by A Ram: (listof (regexp . function))
                    (if (consp action) (setq action (car action)))
                    (if (symbolp action)
                        (funcall action)
                      (eval action))
                    (if first-only
                        ;; We just did a major-mode, which probably
                        ;; killed all local variables.  So reparse
                        ;; any "Local Variables".  Form swiped from
                        ;; normal-mode in files.el.
                        (progn
                          (condition-case err
                              (hack-local-variables)
                            (error (message "File local-variables error: %s"
                                            (prin1-to-string err))))
                          nil)
                      (cdr alist)))
                (cdr alist))))
      ))
  ;; Notify find-file-noselect (files.el) about possible change of
  ;; current-buffer (as done by db-this-buffer and infer-info-buffer)
  (and (boundp 'buf)
       (setq buf
             ;; emacs 18?  (buffer-name (current-buffer))
             (current-buffer)
             )))


;;; Alist entries

;; Standard in Emacs 19.28, but not autoloaded.
;; Hmm, XEmacs 20 does not have dired-x or sh-script:
;; (autoload 'dired-virtual-mode "dired-x")
;; (autoload 'sh-mode "sh-script" "Edit shell scripts." t)

;; For these two little packages, try my ftp site:
;; (autoload 'dos-hiding "dos" "Toggle hiding ^M's." t)
;; (autoload 'paged-mode "paged" "Toggle paged mode." t)

;; Mike Ernst's emacs dababase, theory.lcs.mit.edu:/pub/mernst/edb/
;; (autoload 'db-this-buffer "database" nil t)

;; Lokier/Lindgren folding minor mode:
;; (autoload 'folding-mode-find-file "folding")

;; Bob Weiner's hyperbole: ftp://cs.uiuc.edu:/pub/xemacs/infodock/
;; (autoload 'kotl-mode "hyperbole/kotl") ; wherever you put it ...

(defun infer-info-buffer nil
  ;; Alternative: I might prefer to preserve the current buffer, and
  ;; popup a second *info* buffer on the same file.
  (let ((cb (current-buffer)))
    ;; Avoid error signal on invalid info file (like etc/LNEWS)
    (condition-case err
        (progn
          (require 'info)
          (Info-find-node (buffer-file-name) "Top")
          (kill-buffer cb))
      (error nil))))

(defun infer-vm-buffer-ask nil
  (and
   ;; Make sure we are not inside vm-gobble-crash-box (vm/vm-folder.el)!
   ;; Is there a more robust way?
   (not
    (and (boundp 'enable-local-variables) (not enable-local-variables)
         (boundp 'inhibit-local-variables) inhibit-local-variables))
   (let (choice)
     (cond
      ((string-match "/inbox\\(es\\)?/$" default-directory)
       (setq choice "readonly"))
      ((while (equal
               "" (setq
                   choice
                   (completing-read
                    "Visit as a VM mail folder? (y/n/r) "
                    '(("yes") ("no") ("readonly"))
                    nil t)))
         (message "Please enter yes, no, or readonly")
         (sit-for 1))))
     (or (equal choice "no")
         ;; (vm-visit-folder (buffer-file-name) (equal choice "readonly"))
         ;; Note: vm can take a file or buffer argument!  See vm/vm-startup.el
         (vm (or buffer-file-name (current-buffer))
             (equal choice "readonly"))
         ))
   ))

(defvar infer-empty-alist
  (list
   ;; probably a new shell script:
   ;; '("/bin/[^/]+\\'" sh-mode)
   ;; probably new a mail folder:
   ;; '("/\\([Mm]ail\\|[Nn]ews\\)/[^/]+\\'" vm-mode)
   ;; a home .dotfile, probably some shell:
   ;; (list
   ;;  (concat "\\`" (regexp-quote (expand-file-name "~/")) "\\.[^/]+\\'")
   ;;  'sh-mode)
   )
  "List of (FILEPAT MODE) entries to guess major-mode of new files.

This only applies to empty files that were not already classified by
`auto-mode-alist', so the patterns probably look more at the directory
than at the filename.  For this to work at all, you need the following
entry in `infer-mode-alist':

   (\"\\`\\'\" infer-empty)")

(defun infer-empty nil
  ;; Called for a new or empty file buffer without a guessed mode.  We
  ;; just look at the path, since `auto-mode-alist' already does a
  ;; good job guessing modes from the filename and extension.
  (let ((alist infer-empty-alist) mode)
    (while alist
      (if (string-match (car (car alist)) buffer-file-name)
	  (setq mode (nth 1 (car alist)) alist nil)
	(setq alist (cdr alist))))
    (and mode (funcall mode))))


;;; Install

;; Put at end of hook, hopefully after any data conversion and mode tricks:
(add-hook 'find-file-hooks 'infer-mode t)
(provide 'infer)

;; end of infer.el
