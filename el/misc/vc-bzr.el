;;; vc-bzr.el --- a VC backend for Bazaar-NG

;;; Author: Riccardo Murri <riccardo.murri@gmail.com>
;;; Version: 0.5 (2006-08-07)


;; Copyright (c) 2006 Riccardo Murri <riccardo.murri@gmail.com> (but
;; borrowed most of the code from ``vc-svn.el`` by Jim Blandy
;; <jimb@red-bean.com>)


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA


;;; Commentary:

;;  Note: This is just a quick hack to have the usual VC keybindings
;; 'C-x v v' etc work on bzr-controlled files.  VC-mode is not well
;; suited for distributed VC (like bzr is), so you should check out
;; some other mode to exploit the full power of bzr within emacs.

;; To make this file load on demand, put this file into a directory
;; in `load-path', and add this line to a startup file:
;;
;;     (add-to-list 'vc-handled-backends 'BZR)


;;; TO-DO:
;;
;; Need to add vc-bzr--delete-progress-indicators() after any invocation
;; of vc-do-command()?
;;
;; Need to distinguish between local and non-local branches?  If we
;; have a working copy which is a checkout of a remote branch, what
;; happens when we invoke commit/status/update?
;; 
;; Cache ``bzr root`` results in assoc list?
;;
;; Cache results of vc-bzr-run-status() in assoc list? (Would speed up
;; vc-bzr-state-heuristics() and vc-bzr-workfile-version())
;; 
;; Use ``bzr inventory`` instead of ``bzr unknowns`` in
;; vc-bzr--is-unknown()?
;;
;; What happens if a commit fails because of a conflict?
;;
;; *** The rest are leftover from vc-svn.el: ***
;;
;; After manual merging, need some way to run `bzr resolved'.  Perhaps
;; we should just prompt for approval when somebody tries to commit a
;; conflicted file?
;;
;; Perhaps show the "conflicted" marker in the modeline?
;;
;; If conflicted, before committing or merging, ask the user if they
;; want to mark the file as resolved.
;;
;; After merging news, need to recheck our idea of which workfile
;; version we have.  Reverting the file does this but we need to
;; force it.  Note that this can be necessary even if the file has
;; not changed.
;;
;; Does everything work properly if we're rolled back to an old
;; revision?
;;
;; Perhaps need to implement vc-bzr-latest-on-branch-p?


;;; History:
;; 
;; 0.5 (2006-08-07)
;;   - Remove the progress indicator lines from diff, log and annotate.
;;   - BUGFIX: ignore the "pending merges:" lines in "bzr status" output.
;;
;; 0.4 (2006-07-25)
;;   - vc-bzr-diff passes to ``bzr diff`` the switches as returned by
;;     `vc-diff-switches-list'.  Since "-c" is always in that list, we
;;     cannot select another `diff' format.  Better undo?
;;
;; 0.3 (2006-07-20)
;;   - vc-bzr-diff now working:
;;       - filters out progress report lines from ``bzr``
;;       - consider ``bzr diff`` command successful if exit status
;;         is 0 or 1; any other code is reported to the user as an error.
;;
;; 0.2 (2006-07-15)
;;   - correctly distinguish between unknown and unchanged files
;;   - invoke bzr through "/usr/bin/env LC_ALL=C" to get output always
;;     in the POSIX locale.
;;   - bugfix: correctly determine the revision number when opening
;;     a file in a different bzr branch


;;; Code:

(add-to-list 'vc-handled-backends 'BZR)

(defcustom vc-bzr-program-name "bzr"
  "*Name of Bazaar-NG client program, for use by Emacs's VC package."
  :type 'string
  :group 'vc
  :version "21.2.90.2")

(defcustom vc-bzr-diff-switches nil
  "*A string or list of strings specifying extra switches for `bzr diff' under VC.

Note: VC concatenates the values of `diff-switches',
`vc-diff-switches' and this variable, when invoking `bzr diff'. But
`diff' will refuse to run when passed conflicting format switches; so,
for instance, you cannot put `-u' here if the `vc-diff-switches' or
`diff-switches' hold `-c'.

See `diff-switches', `vc-diff-switches'.
"
  :type '(repeat string)
  :group 'vc
  :version "21.2.90.2")


;;; utility functions

(defun vc-bzr-append-slash-if-directory (path)
  "Return a copy of PATH, appending a slash if PATH points to a directory."
  (if (file-directory-p path) (concat path "/") path))


(defun vc-bzr-remove-trailing-newline (line)
  "Return a copy of LINE omitting the trailing newline(s)."
  (substring line 0 (or (string-match "\n+$" line) (length line))))


(defun vc-bzr-concat-with-spaces (&rest args)
  "Concatenate all ARGS, leaving one space between two consecutive ones."
  (if args
      (reduce (lambda (x y) (concat x " " y)) args)))


;;; functions

(defun vc-bzr--delete-progress-indicators (&optional buffer)
  "Remove progress indicators from `bzr' output in BUFFER.

Shell command `bzr' insists on outputting progress indicators when run
with standard output connected to a tty, such as happens when run by
Emacs function `shell-command'.  This function will remove these
progress indicators, detecting them by a very crude heuristics: within
the first line of text, everything up to the last \"^M\" is deleted.

If optional first argument BUFFER, if given, must be a buffer name
(string) or a buffer on which to operate; otherwise, remove `bzr'
progress indicators from current buffer."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (goto-char (point-min))
    (let ((eol (if (re-search-forward "$") (match-beginning 0) (point-max))))
      (goto-char (point-min))
      (while (re-search-forward "
" eol t))
      (if (match-end 0) (delete-region (point-min) (point))))))


(defun vc-bzr-command (cmd)
  "Return the command line for running bzr subcommand CMD."
  (vc-bzr-concat-with-spaces "/usr/bin/env" "LC_ALL=C" 
                             vc-bzr-program-name cmd))


(defun vc-bzr-run (cmd &rest args)
  "Run bzr subcommand CMD with ARGS and put result in current buffer."
  (apply 'call-process "/usr/bin/env" nil t nil "LC_ALL=C" 
         vc-bzr-program-name cmd args))


(defun vc-bzr-revno (root)
  "Return the current revision number of the bzr repository at ROOT as a string."
  (vc-bzr-remove-trailing-newline
   (shell-command-to-string 
    (concat
     "cd " root "; "
     (vc-bzr-command "revno")))))


(defun vc-bzr-root (file)
  "Return the root directory of the bzr repository containing FILE."
  (vc-bzr-remove-trailing-newline
   (shell-command-to-string 
    (concat
     "cd " (file-name-directory file) "; "
     (vc-bzr-command "root")))))


(defun vc-bzr-registered (file)
  "Return true if FILE is registered under Bazaar-NG."
  (not (null (vc-bzr-run-status file))))


(defun vc-bzr-pop-up-error (&rest args)
  "Pop up the Bazaar-NG output buffer, and raise an error with ARGS."
  (pop-to-buffer " *Bazaar-NG Output*")
  (goto-char (point-min))
  (shrink-window-if-larger-than-buffer)
  (apply 'error args))


(put 'vc-bzr-with-output-buffer 'lisp-indent-function 0)
(defmacro vc-bzr-with-output-buffer (&rest body)
  "Save excursion, switch to buffer ` *Bazaar-NG Output*', erase it, then execute BODY."
  `(save-excursion
     ;; Let's not delete this buffer when we're done --- leave
     ;; it around for debugging.
     (set-buffer (get-buffer-create " *Bazaar-NG Output*"))
     (erase-buffer)
     ,@body))


(defun vc-bzr-run-status (file)
  "Run `bzr status' on FILE, and return the result.

We return nil for a file not under Bazaar-NG's control,
or (STATE LOCAL CHANGED) for files that are, where:
STATE is the file's VC state (see the documentation for `vc-state'),
LOCAL is the base revision in the working copy, and
CHANGED is the last revision in which it was changed.
Both LOCAL and CHANGED are strings, not numbers.

If the file is newly added, LOCAL is \"0\" and CHANGED is nil."
  (vc-bzr-with-output-buffer
    (let ((status (vc-bzr-run "status" file)))
      (goto-char (point-min))

      ;; return code other than zero signals an error
      (if (not (equal 0 status))
          (cond
           ;; these happens when VC tries to determine if a file
           ;; is versioned - ignore and tell it is not
           ((looking-at ".*ERROR: Not a branch") nil)
           ((looking-at ".*ERROR: Path(s) do not exist") nil)
           ;; otherwise, signal error to user
           (t (vc-bzr-pop-up-error
               "Error running Bazaar-NG to check status of `%s'"
               (file-name-nondirectory file))))

        ;; remove the progress indicators
        (vc-bzr--delete-progress-indicators)

        ;; remove the "pending merges: ..." lines - they have nothing to do
        ;; with the actual file status, just signal that we need to commit
        ;; the merge before doing any other work.  Maybe message user?
        (when (re-search-forward "^pending merges:" (point-max) t)
          (delete-region (match-beginning 0) (point-max))
          (message 
           "Uncommitted merges in branch, you need to commit before any other bzr operation."))

        ;; bzr exits with code 0 but no output for "unknown" (that is,
        ;; non-VC) and "unchanged" ('up-to-date) files alike...
        (let* (
               (root (vc-bzr-root file))
               (revno (vc-bzr-revno root))
               )
          (if (equal 0 (buffer-size))
              (if (vc-bzr--is-unknown file root)
                  nil                   ; FILE is unknown to bzr
                (list 'up-to-date revno revno)) ; FILE is up-to-date
            ;; Otherwise, we've got valid status output in the buffer, so
            ;; just parse that.
            (cond
             ;; added:
             ;;     Versioned in the working copy but not in the previous revision.
             ((looking-at "added") (list 'edited revno nil))
             ;; removed:
             ;;     Versioned in the previous revision but removed or deleted
             ;;     in the working copy.
             ((looking-at "removed") nil)
             ;; renamed:
             ;;     Path of this file changed from the previous revision;
             ;;     the text may also have changed.  This includes files whose
             ;;     parent directory was renamed.
             ((looking-at "renamed") (list 'edited revno revno))
             ;; modified:
             ;;     Text has changed since the previous revision.
             ((looking-at "modified") (list 'edited revno revno))
             ;; unchanged:
             ;;     Nothing about this file has changed since the previous revision.
             ((looking-at "unchanged") (list 'up-to-date revno revno))
             ;; unknown:
             ;;     Not versioned and not matching an ignore pattern.
             ((looking-at "unknown") nil)
             ;; An ignored file
             ((looking-at "ignored") nil)
             (t (error "Couldn't parse output from `bzr status'")))))))))


(defun vc-bzr--is-unknown (file root)
  "Determine if a file was never registered with bzr.

Return t if FILE is unknown to bzr, or nil otherwise.
Second argument ROOT is the root directory of the bzr branch
hosting FILE."
    (reduce (lambda (&optional x y) (or x y))
           ;; for each unknown, return t or nil, whether it is
           ;; the initial part of file
           (mapcar 
            (lambda (initial)
              (if (string-match (concat "^" initial) file) t nil))
            ;; append "/" to all directory names
            (mapcar
             (lambda (path)
               (if (file-directory-p path) (concat path "/") path))
             ;; prepend root path to unknowns
             (mapcar
              (lambda (x) (concat root "/" x))
              ;; `bzr unknowns` returns a list of files and directories
              ;; unknown to bzr; if a directory is unknown and contains
              ;; unknown files, only the directory name is reported.
              (split-string
               (shell-command-to-string (vc-bzr-command "unknowns"))))))))


(defun vc-bzr-state (file)
  "Return the current version control state of FILE.
For a list of possible return values, see `vc-state'.

This function should do a full and reliable state computation; it is
usually called immediately after `C-x v v'.

For bzr this does *not* check for updates in the repository.  Instead,
we rely on Bazaar-NG to trap situations such as needing a merge before
commit."
  (car (vc-bzr-run-status file)))


(defun vc-bzr-state-heuristic (file)
  "Estimate the version control state of FILE at visiting time.
For a list of possible values, see the doc string of `vc-state'.

This is supposed to be considerably faster than `vc-bzr-state', but in
vc-bzr it's just the same."
  (car (vc-bzr-run-status file)))


(defun vc-bzr-workfile-version (file)
  "Return the current workfile version of FILE."
  (cadr (vc-bzr-run-status file)))


(defun vc-bzr-responsible-p (file)
  "Return non-nil if FILE belongs in a descendant directory of the ``bzr root``.

Therefore, if FILE is somewhere under the ``bzr root``, vc-bzr tells VC
it is the backend responsible for it.

This function is called by `vc-responsible-backend' to find out what
backend to use for registration of new files and for things like
change log generation."
  (string-match (concat "^" (vc-bzr-root file)) 
                (or (file-name-directory file) "")))


(defun vc-bzr-register (file &optional rev comment)
  "Register FILE with Bazaar-NG.

REV is an initial revision; Bazaar-NG ignores it.  COMMENT is an
initial description of the file; currently ignored as well."
  (vc-bzr-with-output-buffer
    (let ((status (vc-bzr-run "add" file)))
      (or (equal 0 status)         ; not zerop; status can be a string
          (vc-bzr-pop-up-error "Error running Bazaar-NG to add `%s'"
                               (file-name-nondirectory file))))))


(defun vc-bzr-checkin (file rev comment)
  (apply 'vc-do-command nil 0 vc-bzr-program-name file
         "commit" (if comment (list "-m" comment) '())))


(defun vc-bzr-checkout-model (file)
  "Indicate whether FILE needs to be `checked out' before it can be edited.

See `vc-checkout-model' for a list of possible values."
  'implicit)


(defun vc-bzr-checkout (file &optional editable rev destfile)
  "Check out revision REV of FILE into the working area.

If EDITABLE is non-nil, do a regular update, otherwise check out the
requested REV to temp file DESTFILE.  If both EDITABLE and DESTFILE
are non-nil, raise an error.

If REV is non-nil, that is the revision to check out (default is
current workfile version).  If REV is the empty string, that means to
check out the head of the trunk.  For Bazaar-NG, that's equivalent to
passing nil."
  (if editable
      (progn
        (when destfile
          (error "VC asked Bazaar-NG to check out a file under another name"))
        (when (equal rev "")
          (setq rev nil))
        (apply 'vc-do-command nil 0 vc-bzr-program-name file
               "update" (if rev (list "-r" rev) '()))
        (vc-file-setprop file 'vc-workfile-version nil))
    (with-temp-file destfile
      (apply 'vc-do-command t 0 vc-bzr-program-name file
             "cat" (if (equal rev "") '() (list "-r" rev))))))


(defun vc-bzr-revert (file &optional contents-done)
  "Revert FILE back to the current workfile version.
If optional arg CONTENTS-DONE is non-nil, then the contents of FILE
have already been reverted from a version backup, and this function
only needs to update the status of FILE within the backend.  

Presently, the CONTENTS-DONE argument is ignored, and `bzr revert
FILE' is executed anyway."
  (vc-do-command nil 0 vc-bzr-program-name file "revert"))


(defun vc-bzr-merge-news (file)
  "Merge recent changes into FILE.

This calls `bzr update'.  In the case of conflicts, Bazaar-NG puts
conflict markers into the file and leaves additional temporary files
containing the `ancestor', `mine', and `other' files.

You may need to run `bzr resolved' by hand once these conflicts have
been resolved.

Returns a vc status, which is used to determine whether conflicts need
to be merged."
  (prog1
      (vc-do-command nil 0 vc-bzr-program-name file "update")
    
    ;; This file may not have changed in the revisions which were
    ;; merged, which means that its mtime on disk will not have been
    ;; updated.  However, the workfile version may still have been
    ;; updated, and we want that to be shown correctly in the
    ;; modeline.

    ;; vc-cvs does something like this
    (vc-file-setprop file 'vc-checkout-time 0)
    (vc-file-setprop file 'vc-workfile-version
                     (vc-bzr-workfile-version file))))


(defun vc-bzr-print-log (file)
  "Insert the revision log of FILE into the *vc* buffer."
  (vc-do-command nil 0 vc-bzr-program-name file "log")
  (vc-bzr--delete-progress-indicators "*vc*"))


(defun vc-bzr-show-log-entry (version)
  "Search the log entry for VERSION in the current buffer.
Make sure it is displayed in the buffer's window."
  (when (re-search-forward 
         (concat "^\\(revno:\\) " (regexp-quote version)))
    (goto-char (match-beginning 1))
    (recenter 1)))


(defun vc-bzr-diff (file &optional rev1 rev2)
  "Insert the diff for FILE into the *vc-diff* buffer.

If REV1 and REV2 are non-nil, report differences from REV1 to REV2.
If REV1 is nil, use the current workfile version (as found in the
repository) as the older version; if REV2 is nil, use the current
workfile contents as the newer version.

This function returns a status of either 0 (no differences found), or
1 (non-empty diff).

There is no attempt to determine whether a non-local repository will
be used; therefore, the diff is always run *synchronously*."
  (let* 
         (
          (status (vc-bzr-run-status file))
          (local (elt status 1))
          (changed (elt status 2))
         
          ;; If rev1 is the default (the base revision) set it to nil.
          ;; This is nice because it lets us recognize when the diff
          ;; will run locally, and thus when we shouldn't bother to run
          ;; it asynchronously.  But it's also necessary, since a diff
          ;; for vc-default-workfile-unchanged-p *must* run
          ;; synchronously, or else you'll end up with two diffs in the
          ;; *vc-diff* buffer.  `vc-diff-workfile-unchanged-p' passes
          ;; the base revision explicitly, but this kludge lets us
          ;; recognize that we can run the diff synchronously anyway.
          ;; Fragile, no?
          (rev1 (if (and rev1 (not (equal rev1 local))) rev1))
          
          (rev-switches-list
           (cond
            ;; Given base rev against given rev.
            ((and rev1 rev2) (list "-r" (format "%s..%s" rev1 rev2)))
            ;; Given base rev against working copy.
            (rev1 (list "-r" rev1))
            ;; Working copy base against given rev.
            (rev2 (list "-r" (format "%s..%s" local rev2)))
            ;; Working copy base against working copy.
            (t '())))

          (diff-switches-list
           (apply 'vc-bzr-concat-with-spaces 
            ;; In Emacs 21.3.50 or so, the `vc-diff-switches-list' macro
            ;; started requiring its symbol argument to be quoted.
            (condition-case nil
                (vc-diff-switches-list bzr)
              (void-variable (vc-diff-switches-list 'BZR)))
            ))
          )

    ;; remove read-only status from *vc-diff*
    (save-excursion
        (set-buffer (get-buffer-create "*vc-diff*"))
        (setq buffer-read-only nil))

    ;; ``bzr diff`` exists with the same status codes as ``diff``:
    ;;    0 = files do not differ
    ;;    1 = differences found
    ;; so, let vc-do-command() report an error iff exit status is >1
    (prog1 
      (apply 'vc-do-command "*vc-diff*" 1
             vc-bzr-program-name file
             (append '("diff")
                     (if diff-switches-list
                         (append '("--diff-options") 
                                 (list diff-switches-list)))
                     rev-switches-list))
      (vc-bzr--delete-progress-indicators "*vc-diff*"))))


(defun vc-bzr-find-version (file rev buffer)
  (vc-do-command buffer 0 vc-bzr-program-name file
                 "cat" "-r" rev))


(defun vc-bzr-annotate-command (file buffer &optional version)
  "Execute \"bzr annotate\" on FILE, inserting the contents in BUFFER.
Optional arg VERSION is a revision to annotate from."
  (vc-do-command buffer 0 vc-bzr-program-name file "annotate"
                 (if version (concat "-r" version)))
  (vc-bzr--delete-progress-indicators buffer))


(defun vc-bzr-annotate-difference (point)
  "Difference between the time of the line and the current time.
Return values are as defined for `current-time'."
  nil)                                 ; don't bother with differences


(provide 'vc-bzr)

;;; vc-bzr.el ends here
