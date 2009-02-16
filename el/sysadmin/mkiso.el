;;; Saved through ges-version 0.3.3dev at 2003-04-25 07:23
;;; ;;; From: Alain Picard <apicard+die-spammer-die@optushome.com.au>
;;; ;;; Subject: mkiso.el --- make CDROM backups of some directories
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 25 Apr 2003 19:37:38 +1000
;;; ;;; Organization: Picard's Enterprise

;;; --=-=-=


;;; Dear emacs users,

;;; I wrote this small snippet because I got sick of 
;;; XCDRoast fouling things up on me.  Doesn't do much,
;;; but you may yet find it of use.  You
;;;  -- mark all your dirs in dired
;;;  -- hit one key and get a burnable ISO image of those
;;;     dirs.


;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=mkiso.el
;;; Content-Transfer-Encoding: 8bit
;;; Content-Description: mkiso.el

;;; mkiso.el --- Create CDROM images from marked directories

;; Copyright (C) 2003 Alain Picard
;; Version: 0.1
;; Author: Alain Picard <apicard+die-spammer-die@optushome.com.au>
;; Keywords: CDROM dired ISO mkisofs filesystem
;; Compatibility: Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;;  This file provides two new commands in dired mode:
;;
;;  * dired-size-of-marked-dirs
;;  * dired-mkisofs-from-marked-dirs
;;
;;  These allow you to mark a bunch of directories, find
;;  out how large they are to see if they'll fit on a CDROM,
;;  and make an ISO image which can then be burned to disc.
;;
;;  In particular, the directories are all `grafted' to the
;;  top level of the CD image (which means all directory names
;;  must be distinct).  E.g. if you mark the directories
;;  "/home/joe/foo" and "/home/sally/bar", the CD will contain
;;  the two top level directories "foo" and "bar".
;;
;;  Finally, you will be prompted for a volume label; some
;;  operating systems will use that as a pretty name to show
;;  for the CDROM icon when it is mounted.
;;
;;  WARNING:
;;  I haven't thougrouhly tested this, especially in terms
;;  of using in on directories containing unreadable files
;;  or spaghetti symlinks directories.  
;;
;;
;;  TODO:
;;   * better testing?
;;   * better documentation?
;;   * menu support?
;;   * killing child process?
;;; Code:

;;;; Utils to make backing up files on CD player a bit easily:

(eval-when-compile (require 'cl))

(defvar *dired-mkisofs-default-iso-volume-label* "CDROM"
  "*Label to apply to the ISO image.")

(defun dired-size-of-marked-dirs ()
  "Return the size, in megabytes, for all the files/dirs currently marked."
  (interactive)
  (message "Total size: %d MB" (/ (dired-dir-totals (dired-get-marked-files)) 1024 1024)))

(defun dired-mkisofs-from-marked-dirs (file)
  (interactive "FName of iso file? ")
  (let ((label (read-from-minibuffer "Label of iso? " *dired-mkisofs-default-iso-volume-label*)))
    (if (not (y-or-n-p "Really build ISO file?"))
        (message "Aborted.")
      (message "Computing size... [Hit C-g to abort]")
      (message "Okay.  Your image will be %s MB." (dired-disk-usage-of-marked-dirs))
      (dired-mkisofs-make-iso-fs file label (dired-get-marked-files)))))

(defun dired-mkisofs-make-iso-fs (file volume-label directories)
  (apply #'start-process
	 "*mkisofs*"
	 " *mkisofs*"
	 "mkisofs"
	 "-D"
	 "-graft-points"
	 "-R"
	 "-V"
	 volume-label
	 "-o"
	 file
	 (mapcar #'dired-mkisofs-graft-one-dir directories)))

(defun dired-mkisofs-graft-one-dir (file)
  (concat (file-name-nondirectory file)
	       "/="
	       file
	       "/"))

;; This function seems to be defined in only some versions
;; of emacs... hmmm must look into this
(defun dot-directory-p (file)
  (let ((name (file-name-nondirectory file)))
    (or (equal "." name)
	(equal ".." name))))

(defun map-dir-tree (root fn)
  "Apply FN to each regular file under directory ROOT."
  (cond ((file-symlink-p root)
	 ;; Ignore
	 t)

	((file-directory-p root)
	 (unless (dot-directory-p root)
	   (dolist (file (directory-files root t))
	     (map-dir-tree file fn))))

	((file-regular-p root)
	 (funcall fn root))

	(t
	 ;; What the hell is this?
	 (error "Dunno what kind of file is %s" root))))

(defun dired-dir-size (root)
  "Return the size, in bytes, of files located under ROOT.
   Symbolic links are NOT followed."
  (let ((size 0.)) ; Ugh.  Integers would overflow very quickly
    (map-dir-tree root
		  (lambda (file)
		    ;;                    \/ file-size
		    (incf size (float (nth 7 (file-attributes file))))))
    size))

(defun dired-dir-totals (dirs &optional unit)
  (let ((divisor (case unit
                   (:megabyte (* 1024 1024))
                   (:kilobyte 1024)
                   ((or nil :byte)     1))))
    (/ (reduce #'+ (mapcar #'dired-dir-size dirs))
       divisor)))

(defun dired-disk-usage-of-marked-dirs ()
  "Return the size, in megabytes, for all the files/dirs currently marked."
  (dired-dir-totals (dired-get-marked-files) :megabyte))

;; Keymap

(add-hook 'dired-load-hook
	  (lambda ()
	    (define-key dired-mode-map [?\H-t] 'dired-size-of-marked-dirs)
	    (define-key dired-mode-map [?\H-m] 'dired-mkisofs-from-marked-dirs)))

(provide 'mkiso)

;;;; mkiso.el ends here
;;; --=-=-=




;;; -- 
;;; It would be difficult to construe        Larry Wall, in  article
;;; this as a feature.			 <1995May29.062427.3640@netlabs.com>

;;; --=-=-=--

