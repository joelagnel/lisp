;;; Saved through ges-version 0.3.3dev at 2003-07-02 09:44
;;; ;;; ;;; From: Jeff Bowman <wolfjb@bigfoot.com>
;;; ;;; ;;; Subject: subversion support for emacs
;;; ;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; ;;; Date: Wed, 02 Jul 2003 03:24:33 GMT
;;; ;;; ;;; Reply-To: wolfjb@bigfoot.com
;;; ;;; ;;; Organization: Cox Communications

;;; ;;; I have written this module to include Subversion version control
;;; ;;; support in emacs. I have been using it for a while and it seems to
;;; ;;; work for what I need it to do. However, it is a minimalist
;;; ;;; implementation of only the required forms from vc.el. I would like to
;;; ;;; donate this code to GNU for (hopefully) inclusion in Emacs. I'd be
;;; ;;; happy to sign anything required to assign copyrights, etc. I did look
;;; ;;; on the gnu website but didn't find any forms, and I sent a mail to
;;; ;;; gnu.org and the reply suggested I submit this to one of the emacs
;;; ;;; maintainers for evaluation, etc. If anyone is a maintainer here, I'd
;;; ;;; appreciate you taking a look at this piece of code. Any comments
;;; ;;; anyone has toward improving/expanding it are greatly welcome. I don't
;;; ;;; claim to be a lisp expert or an expert at writing emacs modules. 

;;; ;;; TIA

;;; ;;; Jeff Bowman

;;; ---- vc-svn.el starts here
;;; vc-svn.el --- non-resident support for Subversion version-control

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author:     Jeff Bowman
;; Maintainer: Jeff Bowman <wolfjb@bigfoot.com>
;; Keywords: tools

;; vc-svn is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; vc-svn is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides Emacs VC with support for Subversion version control
;; system. Since Subversion is supposed to replace CVS, I used vc-cvs
;; as a reference while writing this code.

(eval-when-compile
  (require 'vc))

(defun vc-svn-registered (file)
  "check if FILE is registered with subversion"
  (let ((dirname (or (file-name-directory file) ""))
        (basename (file-name-nondirectory file))
        (case-fold-search nil))
    (if (file-readable-p (expand-file-name ".svn/entries" dirname))
        (with-temp-buffer
          (vc-insert-file (expand-file-name ".svn/entries" dirname))
          (goto-char (point-min))
          (cond
           ((and (re-search-forward (regexp-quote basename) nil t)
            (progn
              (beginning-of-line)
              (re-search-forward "name" (line-end-position) t)))
            t)
           (t nil)))
      nil)))

(defun vc-svn-state (file)
  "report state of FILE"
  (let ((case-fold-search nil)
        (state "")
        (end))
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-do-command t nil "svn" file "status" "-u")
      (goto-char (point-min))
      (cond ((re-search-forward (file-name-nondirectory file) nil t)
             (beginning-of-line)
             (setq end (match-beginning 0))
             (cond ((re-search-forward "^M[[:blank:]A-z0-9]*\\*" end t)
                    'needs-merge)	;modified locally and on the server
                   ((re-search-forward "^M" end t)
                    'edited)		;modified locally
                   ((re-search-forward "\*" end t)
                    'needs-patch)	;modified on the server
                   ((re-search-forward "^[[:blank:]]$" end t)
                    'up-to-date)))	;not modified anywhere
            (t (with-temp-buffer
		 (vc-do-command t nil "svn" file "status")
		 (cond
		  ((re-search-forward "^A" nil t)
		   'locally-added)	;added, but not committed
		  (t 'edited))))))))

(defun vc-svn-state-heuristic (file)
  "Subversion state heuristic"
  ;; this came from vc-cvs
  ;; If the file has not changed since checkout, consider it `up-to-date'.
  ;; Otherwise consider it `edited'.
  (let ((checkout-time (vc-file-getprop file 'vc-checkout-time))
        (lastmod (nth 5 (file-attributes file))))
    (if (equal checkout-time lastmod)
        'up-to-date
      'edited))) 

(defun vc-svn-workfile-version (file)
  "get the verion number of the FILE from subversion"
  (let ((str "")
        (beg 0)
        (end 0)
        (case-fold-search nil))
    (cd (file-name-directory file))
    (when (vc-svn-registered file)
      (with-temp-buffer
        (vc-do-command t nil "svn" file "info")
        (goto-char (point-min))
        (when (re-search-forward "Revision:" nil t)
          (beginning-of-line)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (setq str (buffer-substring beg end))
          (int-to-string (string-to-int (cadr (split-string str ":")))))))))

(defun vc-svn-checkout-model (file)
  "return checkout model for FILE"
  ;; Subversion files are always editable, so if FILE is registered,
  ;; return 'implit otherwise nil
  (cond ((vc-svn-registered file) 'implicit)
        (t nil)))

(defun vc-svn-mode-line-string (file)
  "Return string for placement into the modeline for FILE.
Compared to the default implementation, this function handles the
special case of a SVN file that is added but not yet committed."
  ;; cut and paste directly from vc-cvs.el, changed CVS to SVN
  ;; everywhere
  (let ((state   (vc-state file))
        (rev     (if (stringp (vc-workfile-version file))
		     (vc-workfile-version file)
		   (int-to-string (vc-workfile-version file)))))
    (cond ((string= rev "0")
           ;; A file that is added but not yet committed.
           "SVN @@")
          ((or (eq state 'up-to-date)
               (eq state 'needs-patch))
           (concat "SVN-" rev))
          ((stringp state)
           (concat "SVN:" state ":" rev))
          (t
           ;; Not just for the 'edited state, but also a fallback
           ;; for all other states.  Think about different symbols
           ;; for 'needs-patch and 'needs-merge.
           (concat "SVN:" rev)))))

(defun vc-svn-register (file &optional rev comment)
  "register a FILE with subversion with COMMENT if appropriate"
  ;; rev is always ignored (svn add does not support a revision for
  ;; this operation)
  (if (not (vc-svn-registered file))
      (with-temp-buffer
	(vc-do-command t nil "svn" file "add"
		       (and comment (string-match "[^\t\n ]" comment)
			    (concat "-m" comment))))))

(defun vc-svn-checkin (file rev comment)
  "checkin a file by using commit."
  ;; I'm depending on vc to Do The Right Thing if this fails, which is
  ;; probably bad. I should probably do something extra here, but I'm
  ;; not sure what based on my experience with the code. I could use
  ;; some help from a vc expert type.
  
  ;; rev is always ignored.
  (vc-do-command t nil "svn" file "commit"
		 (and comment (string-match "[^\t\n ]" comment)
		      (concat "-m" comment))))

(defun vc-svn-checkout (file &optional editable rev destfile)
  "checkout a verion REV of FILE"
  ;; all files are editable when checked out, editable will be unused.

  ;; if DESTFILE is provided, use svn copy instead of svn update since
  ;; update can't checkout a file to a different name but copy can
  (unless destfile
    (with-temp-buffer
      (get-file-buffer file)
      (if (and rev (integerp rev))
	  (setq rev (int-to-string rev)))
      (cond
       ((stringp rev)
	(vc-do-command t nil "svn" file "update" "-r" rev))
       (t
	(vc-do-command t nil "svn" file "update")))))
  (when destfile
    (let ((url "")
	  (beg 0)
	  (end 0)
	  (dir (file-name-directory file))
	  (args "")
	  (status)
	  (case-fold-search nil))
      (with-temp-buffer
	(vc-do-command t nil "svn" file "info")
	(goto-char (point-min))
	(re-search-forward "Url:" nil t)
	(beginning-of-line)
	(setq beg (point))
	(end-of-line)
	(setq end (point))
	(setq url (cadr (split-string (buffer-substring beg end)))))
      (when (integerp rev)
	(setq rev (int-to-string rev)))
      (setq args
	    (list "copy" "-r" rev url (file-name-nondirectory destfile)))
      (setq args (delq nil args))
      (with-temp-buffer
	(setq status (apply 'call-process "svn" nil 0 nil args))))))

(defun vc-svn-revert (file &optional contents-done)
  "revert changes to FILE"
  ;; svn revert FILE
  (cd (file-name-directory file))
  (vc-do-command t nil "svn" file "revert"))

(defun vc-svn-merge (file rev1 rev2)
  "merge two revisions of FILE"
  ;; svn merge -r rev1:rev2 FILE
    (when (integerp rev1)
      (setq rev1 (int-to-string rev1)))
    (when (integerp rev2)
      (setq rev2 (int-to-string rev2)))
    (cd (file-name-directory file))
    (vc-do-command t nil "svn" file "merge" "-r" rev1 ":" rev2))

(defun vc-svn-print-log (file)
  "get change log associated with FILE"
  ;; do svn log twice to get the youngest revision in order to reverse
  ;; the output and put the most current log information at the bottom
  ;; of the output, which is visibile first. The log will be in
  ;; ascending order from top to bottom
  (let ((beg)
	(end)
	(str)
	(rev))
    (with-temp-buffer
      (vc-do-command t nil "svn" file "log")
      (goto-char (point-max))
      (re-search-backward "^rev")
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (setq str (buffer-substring-no-properties beg end))
      (setq rev (string-to-int (car (cdr (split-string str))))))
    (vc-do-command nil nil "svn" file "log" "-r"
		   (concat (int-to-string rev) ":HEAD"))))

(defun vc-svn-wash-log (file)
  ;; nothing to do, leave empty to override default implementation
  )

(defun vc-svn-diff (file &optional rev1 rev2)
  "Get a difference report using Subversion between two versions of FILE"
  (if (string= (vc-svn-workfile-version file) "0")
      ;; This is added but not yet committed
      (if (or rev1 rev2)
          (error "no revisions of %s exist" file)
        (apply (vc-do-command "*vc-diff*" nil "svn" file "diff" "/dev/null"))))
  (when (integerp rev1)
    (setq rev1 (int-to-string rev1)))
  (when (integerp rev2)
    (setq rev2 (int-to-string rev2)))
  (let ((revs ""))
    (cd (file-name-directory file))
    (cond
     ((and (not rev1) (not rev2))	; rev1 and rev2 are nil so
                                        ; check against youngest
      (vc-do-command "*vc-diff*" nil "svn" file "diff"))

     ((or (not rev1) (not rev2))	; rev1 or rev2 is nil check
                                        ; against BASE 
      
      (when (and (not rev2) (> (length rev1) 0))
	(setq revs (concat rev1 ":BASE")))
      (when (and (not rev1) (> (length rev2) 0))
	(setq revs (concat "BASE:" rev2)))
      (vc-do-command "*vc-diff*" nil "svn" file "diff" "-r" revs))

     ((and rev1 rev2)                   ; both rev1 and rev2 are
                                        ; provided, use them
      
      (vc-do-command "*vc-diff*" nil "svn" file "diff" "-r"
                     (concat rev1 ":" rev2))))))

(defun vc-svn-diff-tree (dir &optional rev1 rev2)
  "Get the difference report using Subversion between two versions of DIR"
  ;; same as vc-svn-diff only pass DIR instead of FILE
  ;; need to make sure dir has ending / so that
  ;; vc-svn-workfile-version (used in vc-svn-diff) will return the
  ;; approprate thing
  (if (string= (file-name-directory dir) dir)
      (vc-svn-diff dir rev1 rev2)
    (unless (string= (file-name-directory dir) dir)
      (setq dir (concat dir "/")))
    (vc-svn-diff dir rev1 rev2)))

(defun vc-svn-create-snapshot (dir name branchp)
  "create a branch in Subversion called NAME"
  ;; branchp is irrelevant, branches are just copies in subversion
  
  ;; creates a copy of the file from repository based on the URL found
  ;; from (essentially) `svn info DIR | grep Url:` Note that
  ;; uncommitted work in this directory won't be in the repository
  ;; when the copy is made.
  (unless name
    (vc-do-command t nil "svn" dir "copy"))
  (when name
    (let ((beg 0)
	  (end 0)
	  (url "")
	  (args))
      (with-temp-buffer
	(vc-do-command "svn" dir "info")
	(goto-char (point-min))
	(re-search-forward "Url:" nil t)
	(beginning-of-line)
	(setq beg (point))
	(end-of-line)
	(setq end (point))
	(setq url
	      (cadr (split-string (buffer-substring-no-properties beg end))))
	(when (integerp rev)
	  (setq rev (int-to-string rev)))
	(setq args (list "copy" "-r" rev url
			 (file-name-nondirectory destfile)))
	(with-temp-buffer
	  (apply 'call-process "svn" nil 0 nil args))))))

(defun vc-svn-retrieve-snapshot (dir name update)
  "moves working directory DIR to branch NAME and updates files if
UPDATE is non-nil"
  (with-temp-buffer
    (cd dir)
    (vc-do-command t nil "svn" info dir)
    (let ((case-fold-search nil)
	  (beg)
	  (end)
	  (url))
      (re-search-forward "Url:")
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      ;; Url: file:///usr/local/svn (whatever) 
      (setq url
	    (cadr (split-string (buffer-substring-no-properties beg end)))))
  (cond
   (name
    (vc-do-command t nil "svn" dir "switch" "-r" name url))
   (t (vc-do-command t nil "svn" dir "switch" url)))
  (cond
   (update
    (vc-do-command t nil "svn" dir "update")))))

(defun vc-svn-rename-file (old new)
  "renames a file in Subversion from OLD to NEW"
  ;; This will copy from old to new and leave the result waiting for a
  ;; commit, but I don't know if I should do a commit here, or use the
  ;; url syntax to cause the copy to happen on the server. 
  (vc-do-command t nil "svn" old "move" new))

(provide 'vc-svn)

;;;; vc-svn ends here

