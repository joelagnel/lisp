;;; shs-utils.el --- Commonly used commandline utils.
;; Time-stamp: <2005-07-11 16:53:51 deego>
;; Copyright (C) 2005 D. Goel
;; Emacs Lisp Archive entry
;; Filename: shs.el
;; Package: shs
;; Author: D. Goel <d...@gnufans.org>
;; Keywords:
;; Version:  0.0
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst shs-utils-home-page
  "http://gnufans.net/~deego/emacspub/lisp-mine/shs/")

;; Copyright (C) 2005 D. Goel

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

;; These are commonly used bash utils, to be used when shs has been started..

(require 'cl)
(require 'shs)

(defun shsu-username ()
  ;;(shs-start)
  (shsh "/usr/bin/whoami"))

(defalias 'shsu-user 'shsu-username)
(defalias 'shsu-whoami 'shsu-username)

(defun shsu-start-stop (startfcn stopfcn &optional ssarg scriptname
                                 &rest fcnargs)
  "/etc/init.d type functionality.
When callername is supplied, we use that in the error message.

ssarg is the start-stop argument.

fcnargs are any arguments to be passed to the function.
"
  (cond
   ((member ssarg '("start"))
    (apply startfcn fcnargs ))

   ((member ssarg '("stop"))
    (apply stopfcn fcnargs))

   (t
    (unless scriptname (setq scriptname "[scriptname]"))
    (error
     "Syntax: %s %s" scriptname "start/stop"))))

(defun shsu-mounted-p (string)
  "Return nil if string does not appear in mount output, else return
the matched lines. "
  (ignore-errors
    (shsh (format "%s %s" "mount | grep"
                  (shell-quote-argument (format "%s" string))))))

(defun shsu-as-root (string)
  "shsh on string.  If not root, append a sudo before the string.

Run a shell command indicated by string, but with sudo appended if
needed.
 "
  (cond
   ((string= (shsu-whoami) "root")
    (shsh string))
   (t
    (shsm "Your sudo should [1] either be password-expiry free")
    (shsm "[2] Else, make sure to reauthenticate before running this script,")
    (shsm ".. for example, by typing sudo echo hi")
    (shsm ".... else this script will fail")
    (shsh (format "%s %s" "sudo" string)))))

(defun shsu-directory-files (dir)
  "Removes . and .."
  (let ((f
         (remove
          "."
          (remove ".." (directory-files dir)))))
    (mapcar
     (lambda (f) (expand-file-name f dir)) f)))

(defun shsu-directory-files-dirs-files (dir)
  "Removes . and ..  List dirs first"
  (sort
   (shsu-directory-files dir)
   #'shsu-sort-predicate-dirs-files))

(defun shsu-sort-predicate-dirs-files (a b)
  "Dirs first, then symlinks to dirs, then files, then symlinks to files"
  (let ((ascore (shsu-sort-dirs-files-score a))
        (bscore (shsu-sort-dirs-files-score b)))
    (cond
     ((< ascore bscore)
      t)
     ((= ascore bscore) (string-lessp a b))
     (t nil))))

(defun shsu-sort-dirs-files-score (f)
  (cond
   ((file-directory-p f)
    (if (file-symlink-p f) 1 0))
   (t (if (file-symlink-p f) 3 2))))

(defun shsu-funcall-recursive (f dir &optional predf0 predr0 predf2 predr2)
  "Apply function f to each non-directory in the root of the supplied directory.

More specifically, do so only when predf is satisfied.  By default,
  predf is true only when the file is a regular file, which is not a
  symlink.

Predrecurse (predr0 and predr2) tells when to descend.  By default,
  this is true only if the subdir is a regular directory, which is not
  a symlink.  Both predr0 and predr2 must be satisfied.

predf0 and predr0 are meant to be specified by you for
situational-specific stuff in general.  Their default values is
basically absent.  Left blank for you.  If you want to just deal with
every file, just supply us nil for these.... For example, they could
be functions that check to see if the file already has a gz suffix.

If you are in the mood to overwrite our descending criteria, then use
predf2 and predr2, Predf2 and predr2 are ADDITIONAL predicates to be
satisfied.  These DO have default values which check for symlinks,
etc. . Use them if you DO want alter the default predicates, which are
predf0 and predr0.

"
  (require 'cl)
  (shsu-funcall-recursive1 f (file-truename dir) predf0 predr0 predf2 predr2))

(defun shsu-funcall-recursive1 (f dir &optional predf0 predr0 predf2 predr2)
  (unless predf0
    (setq predf0
          (lambda (arg)
            t)))

  (unless predr0
    (setq predr0
          (lambda (arg)
            t)))

  (unless predf2
    (setq predf2
          (lambda (arg)
            (and
             (file-exists-p arg)
             (not (file-symlink-p arg))
             (not (file-directory-p arg))))))
  (unless predr2
    (setq predr2
          (lambda (a)
            (and
             (file-exists-p a)
             (not (file-symlink-p a))
             (file-directory-p a)))))
  (let*
      ((allf (shsu-directory-files dir))
       (dof (remove-if-not predf0 (remove-if-not predf2 (copy-tree allf))))
       (descendf
        (remove-if-not predr0 (remove-if-not predr2 (copy-tree allf)))))
    (append
     (mapcar f dof)
     (mapcar (lambda (dir)
               (shsu-funcall-recursive1 f dir predf0 predr0 predf2 predr2))
             descendf))))

(defun shsu-gz-recursive (dir)
  "There will be ignored errors, for example, when there are hard
  links"
  (shsu-funcall-recursive
   (lambda (arg)
     (shsm "Gzipping %s..." arg)
     (shs-ignore-errors-flag (shsh (concat "/bin/gzip " arg)))
     (shsm "Gzipping %s...done\n" arg))
   dir
   (lambda (a)
     (let ((e (file-name-extension a)))
       (not (member e '("gz" "GZ" "txt")))))))

(defun shsu-sanitize-filename (file)
  "Remove all stupid MSDOS spaces from filenames in this file.
You typically receive such files from windoze users.
Replace spaces by dashes"
  (let* ((fname (file-name-nondirectory file))
         (replacep (shs-ignore-errors-flag (string-match " " fname)))
         (dir (shs-ignore-errors-flag
               (and replacep (shs-ignore-errors-flag (file-name-directory file)))))
         (fname2
          (shs-ignore-errors-flag
           (and replacep (replace-regexp-in-string " " "-" fname))))
         (file2
          (if dir
              (and replacep (shs-expand-file-name fname2
                                                  dir))
            fname2))
         (command
          (and replacep (concat "Move " (progn file)
                                " to " (progn file2)))))
    (when
        (and replacep
             (y-or-n-p
              command))
      (rename-file file file2))))

(defun shsu-sanitize-filename-recursive (file)
  "See shsu-sanitize-filename.  Do this recursively. "

  (shsu-funcall-recursive #'shsu-sanitize-filename file))

(defun shsu-hostname ()
  (shsh "hostname"))

(provide 'shs-utils)
(provide 'shsu) 