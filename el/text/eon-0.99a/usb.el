;;; usb.el --- find and manage certain USB devices on GNU/Linux

;; Copyright (C) 2007  David O'Toole

;; Author: David O'Toole <dto@monad.lab>
;; Keywords: hardware
;; $Id: usb.el,v 0.7 2007/11/05 03:23:28 dto Exp dto $
;; Time-stamp: <2007-11-10 21:44:07 dto>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides some little utilities for working with USB
;; Mass Storage devices on GNU/Linux.
;;
;; It would probably not be hard to make this configurable so that it
;; works with other operating systems. If you use another operating
;; system and would like to help me get it working there, let me know.
;;
;; Features:
;;  - Scan for attached USB Mass Storage devices and find their UUID's
;;    (see `usb-scan-disks')
;;  - Assign unique names to devices (see `usb-disks-alist')
;;  - Automatically create and configure mount points and UUID's in /etc/fstab
;;    (see `usb-write-fstab', `usb-scan-and-configure')
;;  - Mount/unmount disks interactively, with auto-completion
;;    (see `usb-mount' and `usb-unmount')

;; Future:
;;  - TODO support configurable filesystem types
;;  - TODO Report new disks when hotplugged? (periodic scan, automount?)
;;  - TODO Configure USB Audio devices with ALSA/JACK. use jack.el!

;;; Code:

(require 'cl)
(require 'rx)

;;; Finding disks by their UUID's

(defvar usb-uuids ()
  "List of UUID's found during last scan.")

(defvar usb-new-uuids ()
  "List of newly attached UUID's, if any.")

(defvar usb-uuid-directory "/dev/disk/by-uuid"
  "Directory to read when scanning for UUID's.")

(defun* usb-current-uuids (&optional (file usb-uuid-directory))
  "Obtain a list of currently attached UUID's."
  (remove-if #'(lambda (file-name)
		 (string-match (rx ?. (* anything)) file-name))
	    (directory-files usb-uuid-directory)))

(defun usb-format-uuids (uuids &optional show-names)
  (mapconcat (lambda (uuid)
	       (if show-names
		   (let ((name (usb-mount-point-from-uuid uuid)))
		     (format "[%s %s]" (or name "???") uuid))
		 uuid))
	     uuids
	     " "))

(defun usb-scan-disks ()
  "Scan for attached disks and attempt to detect any new disks."
  (interactive)
  (message "Scanning for attached disks...")
  (let* ((uuids (usb-current-uuids))
	 (new-uuids (set-difference uuids usb-uuids :test 'equal)))
    (message "Scanning for attached disks... Done.")
    ;;
    (when uuids
      (message "Found UUIDs: %s" (usb-format-uuids uuids :show-names)))
    (setf usb-uuids uuids)
    ;;
    (when new-uuids 
      (message "New UUIDs: %s" (usb-format-uuids new-uuids :show-names)))
    (setf usb-new-uuids new-uuids)
    uuids))
  
;;; Naming your disks

(defvar usb-disks-alist ()
  "Association list mapping disk names (i.e. mount points) to
property lists whose contents describe the disks.

The list should contain an entry for each of the disks (i.e. USB
Mass Storage Class devices) that you will be using, assigning a
unique name to each. (If you set `usb-write-fstab-p' to a
non-nil value, then these names will also be used for the
filesystem mount points.)

The following keys are valid in the property lists:

 :uuid             UUID string. 
 :file-system      The file system type. The default is \"xfs\".

The use of disk UUID's makes it less likely that you will mix up
similar-looking devices (such as SD cards). In addition, we don't
have to bother figuring out which device name the disk got
assigned to, because in /etc/fstab you can specify a UUID to
mount, rather than a device file.

If you choose short names for your disks, you can label your SD
cards with one of those permanent markers that writes on dark
surfaces. (I write on the back of the card where there is no
sticker.)

You can find out your devices' UUIDs with the function
`usb-scan-disks', and use `usb-fstab-entries' to generate fstab
entries. The function `usb-write-fstab' will add appropriate
entries to /etc/fstab (via TRAMP by default; use the variable
`usb-fstab-file' to customize this behavior.)

Even simpler, you can use the interactive function
`usb-scan-and-configure' to do both jobs for you. In this case,
changes to /etc/fstab will only be made when the variable
`usb-write-fstab-p' has a non-nil value. (You should set
`usb-disks-alist' before doing this.)

Example:

  (setf usb-disks-alist
        '((\"/zoom/1\" :uuid \"3734-3937\" :file-system \"vfat\")
	  (\"/zoom/2\" :uuid \"2931-A206\" :file-system \"vfat\")
  	  (\"/red500\" :uuid \"e828ddbe-7bd8-442d-8523-a83518bed4de\" 
                       :fstype \"xfs\")))
  (setf usb-write-fstab-p t)
  (usb-scan-and-configure)

After you enter your root password for TRAMP, /etc/fstab will
be updated.

To mount and unmount the disks, use `usb-mount' and `usb-unmount'.
")

;;; Automatically configuring /etc/fstab and mount points

(defun usb-create-mount-point (mount-point)
  (message (format "Creating mount point %s..." mount-point))
  (make-directory (concat "/su::" mount-point) :parents))

(defun usb-create-mount-point-maybe (mount-point)
  (when (not (file-exists-p mount-point))
    (usb-create-mount-point mount-point)))

(defun usb-mount-point-from-uuid (uuid)
  (car-safe (find-if #'(lambda (entry)
			 (string= uuid (getf (cdr entry) :uuid)))
		     usb-disks-alist)))
		     
(defun usb-uuid-from-mount-point (mount-point)
  (getf (cdr-safe (assoc mount-point usb-disks-alist)) :uuid))

(defvar usb-write-fstab-p nil
  "When non-nil, attempt to update /etc/fstab when scanning devices.")

(defvar usb-fstab-file "/su::/etc/fstab" 
  "Tramp address for updating /etc/fstab.")

(defun* usb-fstab-entry-string (mount-point &optional &key uuid (file-system "vfat"))
  (format "UUID=%s\t%s\t%s\tuser,noauto\t0\t0\n"
	  uuid mount-point file-system))

(defun* usb-fstab-entries (&optional (disks usb-disks-alist))
  "Generate fstab entries for the disk names in the alist NAMES."
  (with-temp-buffer
    (dolist (disk disks)
      (insert (apply #'usb-fstab-entry-string disk)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun* usb-write-fstab ()
  "Add (or replace) fstab entries for the UUID's in `usb-disks-alist'."
  (with-temp-buffer
    (insert-file-contents-literally "/etc/fstab")
    (message "Backing up /etc/fstab...")
    (write-file (concat usb-fstab-file ".bak"))
    (message "Backing up /etc/fstab... Done.")
    (message "Updating /etc/fstab...")
    ;; 
    ;; remove any entries that we are updating...
    (dolist (uuid (mapcar #'(lambda (d)
			      (getf (cdr d) :uuid))
			  usb-disks-alist))
      (usb-create-mount-point-maybe (usb-mount-point-from-uuid uuid))
      (goto-char (point-min))
      (while (re-search-forward (concat "^UUID=" uuid "\\(.\\|\n\\)*$") nil :noerror)
	(replace-match "")))
    ;;
    ;; now append new entries
    (goto-char (point-max))
    (insert (usb-fstab-entries))
    (write-file usb-fstab-file)
    (message "Updating /etc/fstab... Done.")))

(defun usb-write-fstab-maybe (&optional force)
  (when (or force usb-write-fstab-p)
    (when (null usb-disks-alist)
      (error "You must set `usb-disks-alist' before attempting to update /etc/fstab."))
    (usb-write-fstab)))

;;; Choosing and mounting disks

(defvar usb-mount-command-string "mount")

(defvar usb-unmount-command-string "umount")

(defun usb-mounted-disks ()
  "Return a list of currently mounted USB disks."
  (with-temp-buffer 
    (shell-command usb-mount-command-string t)
    (let (mounted)
      (dolist (disk (mapcar #'car usb-disks-alist))
	(goto-char (point-min))
	(when (search-forward disk nil :noerror)
	  (push disk mounted)))
      mounted)))

(defvar usb-choose-disk-prompt "Choose USB Mass Storage Class disk: ")

(defun* usb-choose-disk (&optional (choices (mapcar #'car usb-disks-alist)))
  (completing-read usb-choose-disk-prompt choices nil :require-match))

(defun* usb-mount (&optional (mount-point (usb-choose-disk))
			     (command :mount))
  "Mount (or unmount) the device corresponding to MOUNT-POINT.
If no MOUNT-POINT is specified, the user is prompted for a
device (with completion.) 

Mount if COMMAND is :mount, otherwise unmount. The default is :mount."
  (interactive)
  (let ((command-string (if (eq command :mount)
			    usb-mount-command-string
			  usb-unmount-command-string)))
    (destructuring-bind (status output)
	(with-temp-buffer 
	  (list 
	   (call-process command-string nil t nil
			 mount-point)
	   (buffer-substring-no-properties (point-min) (point-max))))
      (message (format "[%s %s] %d %s: %s"
		       command-string mount-point
		       status
		       (if (= 0 status) "OK" "FAILED")
		       output)))))

(defun* usb-unmount (&optional (mount-point (usb-choose-disk)))
  (interactive)
  (usb-mount mount-point :unmount))

(defun usb-scan-and-configure ()
  (interactive)
  (usb-scan-disks)
  (usb-write-fstab-maybe))
  
(provide 'usb)
;;; usb.el ends here
