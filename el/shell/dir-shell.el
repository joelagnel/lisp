;;; Saved through ges-version 0.3.3dev at 2003-03-31 09:12
;;; ;;; From: Phillip Lord <p.lord@russet.org.uk>
;;; ;;; Subject: dir-shell.el
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: 31 Mar 2003 12:57:30 +0100
;;; ;;; Organization: BIOSCI/MRC Human Genome Mapping Project Resource Centre





;;; Knocked this up at the weekend. It shifts the working directory of a
;;; shell buffer to the directory of a file. 

;;; All fairly cheesy at the moment, but I find it useful. 

;;; Cheers

;;; Phil



;;; dir-shell.el --- Shifts shell buffers working directory
;; $Revision: $
;; $Date: $

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Keywords: shell, working directory

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; The package provides a variety of different ways to interact with
;; the current working directory of a shell buffer. While Emacs
;; provides a variety of commands for interacting with the shell, and
;; an array of commands for doing many of the tasks that users can do
;; from the shell (deleting, moving and copying files for instance),
;; using an interactive shell within Emacs is none the less an
;; intuitive way of augmenting Emacs' functionality, with that
;; provided by the native shell. 
;; 
;; One of the difficulties with this though, is that the user often
;; wishes to use the shell in many different working directories. This
;; is particularly true, with some languages, such as Java, which
;; require frequent traversal of many directories.
;;
;; This package provides a number of ways of helping this
;; process. More descriptions of these ways will be added as I code
;; them.

;;; Status:
;;
;; Early days yet, in fact, just chopped out of my .emacs. 

;;; Todo:
;; 1) rewrite properly.
;; 2) As well as shifting the existing *shell*, offer the ability to
;; open a new shell, named after the buffer.
;; 3) Can I cope with different shell types? "cd" should work, but
;; "pwd" might not. 
;; 4) Insinuate with ECB, speedbar.
;; 5) Custom support. 
;; 6) shift-to-current-dir offer prompt for which shell buffer. 
;; 7) eshell support?
;; 8) global minor mode for keybindings. 
;; 9) The "cd" command results in a prompt coming back which is
;; nasty. Don't know how to stop it. 


(require 'shell)

;; this needs to go...
(defvar dir-shell-install-global nil)

(if dir-shell-install-global
    (dir-shell-install-global))

(defun dir-shell-install-global()
  (global-set-key "\C-cd" 'dir-shell-show-to-current-dir))

(defvar dir-shell-pwd-command "pwd")
(defvar dir-shell-cd-command "cd")

(defun dir-shell-show-to-current-dir()
  (interactive)
  (dir-shell-to-current-dir)
  (other-window 1)
  (switch-to-buffer "*shell*"))

(defun dir-shell-to-current-dir()
  (interactive)
  (let ((file-name
         (buffer-file-name
          (current-buffer))))
    (if (not file-name)
        (error "Current buffer is not associated with a file"))
    (dir-shell-to-directory
     (file-name-directory file-name))))

(defun dir-shell-to-directory(directory)
  (let* ((shell-buffer
          (dir-shell-get-shell-buffer))
         (shell-process 
          (get-buffer-process shell-buffer)))
    (save-excursion
      (set-buffer shell-buffer)
      (process-send-string 
       shell-process       
       (concat dir-shell-cd-command " " directory "\n"))
      (cd directory)
      (process-send-string 
       shell-process 
       (concat dir-shell-pwd-command "\n")))))

(defun dir-shell-get-shell-buffer()
  (let* ((poss-shell (get-buffer "*shell*")))
    (if poss-shell
        poss-shell
      (shell))))

(provide 'dir-shell)
                         

