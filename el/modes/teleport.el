;;; Saved through ges-version 0.3.3dev at 2003-09-24 13:32
;;; ;;; From: Luke Gorrie <luke@bluetail.com>
;;; ;;; Subject: teleport.el 1.0
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: 22 Sep 2003 07:32:03 +0200

;;; [1. application/emacs-lisp; teleport.el]

;;; teleport.el -- explore a program by jumping to random parts.

;; Version 1.0.
;; Written by Luke Gorrie <luke@bluetail.com> in September 2003.
;;
;; This program is released under the terms of the GNU General Public
;; License.
;;
;; Compatibile with GNU Emacs 20 and 21, but requires the CL library
;; in Emacs 20. Not compatible with XEmacs -- what's their
;; `file-expand-wildcards' called?

;;; Commentary:
;;
;; We are at war with software cruft, and it is a worthy adversary.
;;
;; Cruft skillfully exploits the programmer's subconscious. He may
;; spend his life surrounded by cruft and only rarely be conscious of
;; it. He learns not to open the nasty files. When he must do so to
;; fix a bug, he keeps his eyes averted from the surrounding cruft.
;; Even the most flamboyantly ugly software cruft rests comfortably in
;; these dank and musty corners of the program, where the programmer
;; has learned not to venture.
;;
;; The subconscious mind is no match for software cruft. We must not
;; rely on its counsel. Instead we must turn to technology to arm us
;; for the battles of the future.
;;
;; This file contains an experimental new weapon for programmers:
;; teleportation. By teleporting to random lines of source throughout
;; the code base, the programmer can cast a critical eye on code he
;; has long since forgotten -- code that many others may have glanced
;; at and looked away, lest their eyeballs be damaged and their
;; stomaches turned. By depriving cruft of its principal weapon -
;; obscurity - it is exposed for all to see and made vulnerable to
;; attack.
;;
;; Take this weapon, brother, and may it serve you well.

;;; Usage:
;;
;; First use `M-x teleport-files' to select the files you want
;; consider for teleportation. The selection is made as a filename
;; which may contain wildcards.
;;
;; That command will place you at the start of a random line in one of
;; your source files. Read the code -- does it make sense? If not, fix
;; it up. When you are satisfied, use `M-x teleport' to jump to a new
;; location.
;;
;; When you're done, use `M-x teleport-close' to close the buffers
;; created during teleportation.


;;; Code:

;; Emacs20 compatibility.
(unless (and (fboundp 'dolist) (fboundp 'mapc))
  (require 'cl))

(defvar teleport-buffers '()
  "Buffers for teleportable files.")

(defvar teleport-created-buffers '()
  "Buffers created for teleportation.
This is the subset of `teleport-buffers' that teleport created itself.")

(defun teleport-files (&rest files)
  "Teleport somewhere into one of FILES.
With a prefix argument, merge FILES into the existing field of
teleportation."
  (interactive (file-expand-wildcards
                (read-file-name "Filename (wildcards allowed): ")))
  (when (null files)
    (error "No files matched!"))
  (unless current-prefix-arg (teleport-close))
  (message "Loading files..")
  (save-window-excursion (mapc 'teleport-load-file files))
  (teleport t))

(defun teleport-load-file (file)
  "Load a file for teleportation."
  (if (get-file-buffer file)
      (add-to-list 'teleport-buffers (get-file-buffer file))
    (let ((buffer (find-file-noselect file)))
      (add-to-list 'teleport-buffers buffer)
      (add-to-list 'teleport-created-buffers buffer))))

(defun teleport (&optional force)
  "Teleport to a new location.
This should be used after `teleport-files' has loaded the files."
  (interactive)
  (when (null teleport-buffers)
    (error "Nowhere to teleport! Use M-x teleport-files to load some files."))
  (let* ((total-size (apply '+ (mapcar 'teleport-buffer-size
                                       teleport-buffers))))
    (switch-to-buffer (teleport-choose-buffer (random total-size)))
    (goto-char (1+ (random (buffer-size))))
    (beginning-of-line)))

(defun teleport-choose-buffer (destination)
  "Return the buffer containing DESTINATION.
The destination is a character offset from the start of the first file."
  (catch 'found-buffer
    (let ((where 0))
      (dolist (buffer teleport-buffers)
        (setq where (+ where (teleport-buffer-size buffer)))
        (when (>= where destination)
          (throw 'found-buffer buffer))))))

(defun teleport-close ()
  "Close the buffers created by teleportation."
  (interactive)
  (mapc 'kill-buffer teleport-created-buffers)
  (setq teleport-buffers nil)
  (setq teleport-created-buffers nil))

(defun teleport-buffer-size (buffer)
  (with-current-buffer buffer (buffer-size)))

(provide 'teleport)

;; teleport.el ends here

