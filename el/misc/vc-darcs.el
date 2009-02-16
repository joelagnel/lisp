;;; vc-darcs.el --- a VC backend for darcs

;;; Copyright (C)  2004  Jorgen Schaefer
;;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?VcDarcs

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;; Commentary:

;; Darcs is David's Advanced Revision Control System at
;; http://www.abridgegame.org/darcs/

;; A few ideas for this file are directly taken from vc-svn.el. Thanks
;; to Jim Blandy!

;; To install, put this file into your load-path and add the following
;; to your .emacs:

;; (add-to-list 'vc-handled-backends 'DARCS)

(defvar vc-darcs-version-string "1.4"
  "The version string for vc-darcs.el.")

;;; Code:

(defgroup vc-darcs nil
  "*The darcs backend for vc."
  :prefix "vc-darcs-"
  :group 'vc)

(defcustom vc-darcs-program-name "darcs"
  "*The name of the darcs command."
  :type 'string
  :group 'vc-darcs)

(defcustom vc-darcs-program-arguments '((diff "-u"))
  "*An associative list of further arguments to pass to darcs. Each
element consists of a symbol naming the command to work on, and a list
of arguments to pass."
  :type '(alist :key-type symbol :value-type (list string))
  :group 'vc-darcs)

(defcustom vc-darcs-mail-address nil
  "*The email address to use in darcs. If this is nil, either the
environment variables DARCS_EMAIL or EMAIL are used, and if none of
them exist, the value of `user-mail-address'."
  :type '(choice string (const nil))
  :group 'vc-darcs)

(defun vc-darcs-find-root-directory (file)
  "Return the root darcs repository directory for FILE, or nil if
there is none."
  ;; We walk the tree until FILE-NAME-DIRECTORY doesn't change the
  ;; path anymore.
  (let ((dir (file-name-directory (expand-file-name file)))
        (olddir "/"))
    (while (and (not (equal dir olddir))
                (not (file-directory-p (concat dir "/_darcs"))))
      (setq olddir dir
            dir (file-name-directory (directory-file-name dir))))
    (if (equal dir olddir)
        nil
      dir)))

(defun vc-darcs-find-darcs-directory (file)
  "Return the darcs directory for FILE, or nil if there is none."
  (let ((dir (vc-darcs-find-root-directory file)))
    (if dir
        (concat dir "_darcs/")
      nil)))

(defun vc-darcs-find-darcs-current (file)
  "Return FILE in the current directory of darcs."
  (let* ((root (vc-darcs-find-root-directory file))
         (current (concat root "_darcs/current/")))
    (when (and root
               current
               (string-match (concat root "\\(.*\\)") file))
      (concat current (match-string 1 file)))))

(defun vc-darcs-do-command (command okstatus file &rest flags)
  "Run darcs COMMAND using `vc-do-command', passing OKSTATUS and FILE
along with FLAGS."
  (let ((arguments (cdr (assq command vc-darcs-program-arguments))))
    (apply #'vc-do-command nil okstatus
           vc-darcs-program-name file (symbol-name command)
           (append arguments
                   flags))))

(defun vc-darcs-registered (file)
  "Return non-nil if FILE is handled by darcs.
This is either the case if this file is in the current directory, or
if the addition of this file is in pending."
  (let ((current (vc-darcs-find-darcs-current file)))
    (if (and current
             (file-exists-p current))
        t
      (let* ((root (vc-darcs-find-root-directory file))
             (pending (concat root "_darcs/patches/pending"))
             (relative (concat "./" (substring file (length root))))
             (addfile (concat "^addfile " (regexp-quote relative))))
        (when (file-exists-p pending)
          (with-temp-buffer
            (insert-file-contents pending)
            (if (re-search-forward addfile nil t)
                t
              nil)))))))

(defun vc-darcs-state (file)
  "Return the state of FILE."
  ;; VC does update the status in `vc-after-save' to 'edited, which
  ;; might be wrong. There's no way we can fix that easily, so we have
  ;; to live with it.
  (if (and (file-newer-than-file-p file (vc-darcs-find-darcs-current file))
           ;; Or whatsnew returns no error
           (= 0 (call-process vc-darcs-program-name nil nil nil
                              "whatsnew" (file-name-nondirectory file))))
      'edited
    'up-to-date))

(defun vc-darcs-checkout-model (file)
  "Return non-nil if FILE needs to be checked out before it can be
edited.
That is never necessary with darcs."
  'implicit)

(defun vc-darcs-responsible-p (file)
  "Return non-nil if we feel responsible for FILE, which can also be a
directory."
  (and (vc-darcs-find-root-directory file) t))

(defun vc-darcs-workfile-version (file)
  "Return the current workfile version of FILE. This is terribly bogus
with darcs."
  ;; (with-temp-buffer
  ;;   (insert-file-contents
  ;;    (concat (vc-darcs-find-darcs-directory file)
  ;;            "inventory"))
  ;;   (goto-char (point-max))
  ;;   (when (re-search-backward "^\\[.*\n.*\\*\\*\\([0-9]*\\)" nil t)
  ;;     (match-string 1))))
  "23-skiddoo")

(defun vc-darcs-workfile-unchanged-p (file)
  "Return non-nil if FILE is unchanged from the repository version."
  (eq 'up-to-date (vc-darcs-state file)))

(defun vc-darcs-mode-line-string (file)
  "Return the mode line string to show for FILE."
  (format "darcs/%s" (vc-state file)))

(defun vc-darcs-register (file &optional rev comment)
 "Add FILE to the darcs repository, and record this. REV is ignored."
 (vc-darcs-do-command 'add 0 file (file-name-nondirectory file))
 (vc-darcs-checkin file rev comment))

(defun vc-darcs-checkin (file rev comment)
  "Record FILE to darcs. REV should always be nil and is ignored,
COMMENT is the new comment."
  (let ((date (format-time-string "%Y%m%d%H%M%S" nil t))
        (author (or vc-darcs-mail-address
                    (getenv "DARCS_EMAIL")
                    (getenv "EMAIL")
                    user-mail-address))
        patch-name
        log)
    (if (string-match "\n" comment)
        (setq patch-name (substring comment 0 (match-beginning 0))
              log (substring comment (match-end 0)))
      (setq patch-name comment
            log ""))
    (vc-darcs-do-command 'record 'async nil
                         "-a" "--pipe" (file-name-nondirectory file))
    (with-current-buffer (get-buffer "*vc*")
      (process-send-string nil
                           (format "%s\n%s\n%s\n%s"
                                   date author patch-name log))
      (process-send-eof))))

(defun vc-darcs-checkout (file &optional editable rev destfile)
  "This gets the latest version of FILE from the darcs repository.
This just copies the file from _darcs/current/.
EDITABLE is ignored.
REV may not be specified, since darcs is patch-based, not
revision-based."
  (when rev
    (error "VC asked darcs to use a specific revision of a file."))
  (let ((from (vc-darcs-find-darcs-current file))
        (to (or destfile
                file)))
    (copy-file from to 1)))

(defun vc-darcs-revert (file &optional contents-done)
  "This reverts FILE in the darcs repository to the current repository
version of the file."
  (unless contents-done
    (copy-file (vc-darcs-find-darcs-current file)
               file
               1)))

(defun vc-darcs-print-log (file)
  "Print the logfile for the current darcs repository to the *vc*
buffer."
  ;; FIXME! changes accepts file argument - i like to see the whole
  ;; repository log, though. Comments?
  (vc-darcs-do-command 'changes 'async nil))

(defun vc-darcs-diff (file &optional rev1 rev2)
  "Show the differences in FILE between the local version and the
current version in the repository.
See `vc-darcs-diff-switches'.
Since darcs is patch-based, not revision-based, REV1 and REV2 are
ignored."
  (let ((arguments (cdr (assq 'diff vc-darcs-program-arguments))))
    (apply #'vc-do-command "*vc-diff*" 'async
           vc-darcs-program-name (file-name-nondirectory file)
           "diff"
           arguments)))

(defun vc-darcs-rename-file (old new)
  "Rename the file OLD to NEW in the darcs repoistory."
  (call-process vc-darcs-program-name nil nil nil "mv" old new))

(provide 'vc-darcs)
;;; vc-darcs.el ends here
