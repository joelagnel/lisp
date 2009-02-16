;; gse-rename.el
;;   Summary:     Bulk file renamer.
;;   Author:      Scott Evans <gse@antisleep.com>
;;   Home:        http://www.antisleep.com/elisp
;;   Time-stamp:  <2004.12.23 00:49:20 gse>
;;
;; Commentary:
;;   I always find myself having to rename a lot of files at once, and
;;   I could picture how I would do it if I had a list of the existing
;;   filenames in Emacs.  Some rectangle stuff here and there, some
;;   query-replace, some macros, whatever.  For instance, a list of
;;   mp3 files that needs a number and dash in front of every filename.
;;
;;   So I wrote a shell script that generated another shell script,
;;   full of /bin/mv commands.  Then I could edit that script.
;;   That was pretty good but I decided to cut out the middleman
;;   and do it entirely in elisp.  So now I have this.  I love it.
;;   Comments, changes are welcome.
;;
;;   Tested on XEmacs 21.4.6.  Should work on GNU Emacs thanks to
;;   a small patch from Christoph Conrad <christoph.conrad@gmx.de>.
;;
;; Installation:
;;   (require 'gse-rename)
;;
;;---------------------------------------------------------------------------
;; Change Log
;; ----------
;; 2004.01.28 Forgot to use wrapper call in one place.
;; 2004.01.27 Add Gnu Emacs implementation of directory-files.
;; 2002.05.06 Prefix means "include directories in list".
;; 2002.03.28 Add documentation.
;; 2002.03.28 Omit ".." in file list.
;; 2002.03.27 Add gse-rename-fast.
;; 2002.03.27 Created.
;;---------------------------------------------------------------------------

;;---------------------------------------------------------------------------

(defun gse-rename (directory regexp &optional prefix)
  "gse-rename makes bulk file renaming easy via
The Power Of Emacs.[tm]

DIRECTORY is the name of the directory whose files you want to rename.
REGEXP allows you to filter filenames.  With prefix, includes directories
in the list.

The gse-rename buffer shows two columns of filenames.  The first
column is the 'source' filenames, the second column is the 'target'.
C-c C-c (gse-rename-go) will rename all of the source filenames to
their respective target names.

This makes it easy to rename files with clever text-editor maneuvers
like rectangle operations, replace-string, etc."
  (interactive "DDirectory: \nsRegexp: \nP")

  (gse-rename-setup directory regexp prefix)
  (gse-rename-mode))

;;---------------------------------------------------------------------------

(defun gse-rename-fast (&optional prefix)
  "Like gse-rename, but takes the directory from the current buffer
and doesn't prompt for a regexp.  Nice for binding to a key in dired."
  (interactive "P")

  (if (not default-directory)
      (error "Buffer has no default directory."))

  (gse-rename default-directory "" prefix))

;;---------------------------------------------------------------------------


(define-derived-mode gse-rename-mode text-mode "gse-rename"
  "gse-rename makes bulk file renaming easy via
The Power Of Emacs.  See gse-rename for details."
  ;; Disable fill stuff.
  (if (functionp 'auto-fill-mode) (auto-fill-mode -1))
  (if (functionp 'turn-off-filladapt-mode) (turn-off-filladapt-mode))

  ;; Add keybinding.
  (define-key gse-rename-mode-map "\C-c\C-c" 'gse-rename-go)

  (message "Press C-c C-c to perform rename."))

;;---------------------------------------------------------------------------

(defun gse-rename-setup (directory regexp include-directories)
  "Build the contents of a gse-rename buffer, to rename
the files in DIRECTORY.  If REGEXP is nil, script is
for all files.  Otherwise it's for the files specified
by REGEXP.

If INCLUDE-DIRECTORIES is non-nil, include directories in the
list."
  (setq directory (file-name-as-directory directory))

  (let ((files (gse-rename-directory-files directory nil regexp nil (not include-directories)))
        (maxlength 0)
        (tmpfiles  nil)
        (curfile   nil)
        (buf (get-buffer-create (concat "*rename-" directory "*")))
        )
    (switch-to-buffer buf)
    (setq default-directory directory)
    (erase-buffer)

    ;; Figure out longest filename.
    (setq tmpfiles files)
    (while tmpfiles
      (setq curfile (car tmpfiles))
      (if (> (length curfile) maxlength)
          (setq maxlength (length curfile)))
      (setq tmpfiles (cdr tmpfiles)))

    (setq maxlength (+ 4 maxlength))

    ;; Generate the buffer-contents.
    (setq tmpfiles files)
    (while tmpfiles
      (setq curfile (car tmpfiles))
      (if (not (or
                (string-equal curfile "..")
                (string-equal curfile ".")))
          (insert (format
                   (concat "%-" (int-to-string maxlength) "s  %s\n")
                   (concat "\"" curfile "\"")
                   (concat "\"" curfile "\""))))
      (setq tmpfiles (cdr tmpfiles)))

    (goto-char (point-min))
    (text-mode)
    ))

;;---------------------------------------------------------------------------

;; Wrap directory-files for cross-emacs compatibility.
;; Thanks to Christoph Conrad <christoph.conrad@gmx.de> for
;; the Emacs implementation.
(defvar gse-rename-directory-files-function
  (if (featurep 'xemacs)
      'directory-files
    'gse-rename-gnu-emacs-directory-files))

(defun gse-rename-gnu-emacs-directory-files (dir &optional full match nosort files-only)
  (let ((files (directory-files dir full match nosort)))
    (if (not files-only)
        files
      (delete nil
              (mapcar
               (lambda( file )
                 (if (file-directory-p (concat dir "/" file)) nil file))
               files)))))

(defun gse-rename-directory-files (directory full match nosort files-only)
  (funcall gse-rename-directory-files-function directory full match nosort files-only))

;;---------------------------------------------------------------------------

(defun gse-rename-go ()
  "Do the renaming specified in the current buffer."
  (interactive)

  ;; Parse the contents of the buffer.  This could actually check to
  ;; see if every rename will work beforehand (are the source/dest
  ;; readable/writable?) but for now all it checks is if the source
  ;; file is writable.
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\"\\([^\"]+\\)\" +\"\\([^\"]+\\)" nil t)
        (let ((src  (match-string 1))
              (dest (match-string 2)))
          (if (not (string-equal src dest))
              (if (file-writable-p src)
                  (progn
                    ;; This slows down processing a little bit, but
                    ;; not too badly.  Might be better to make it a
                    ;; percent indicator that updates the display
                    ;; every 10%.
                    (message (concat src " -> " dest))
                    (rename-file src dest)
                    (setq count (+ 1 count)))
                (error (concat "File does not exist: \"" src "\"")))))
        ) ;; end while loop

      (if (= count 1)
          (message (concat "Renamed 1 file."))
        (message (concat "Renamed " (int-to-string count) " files."))))))

;;---------------------------------------------------------------------------

;;{{{ gse-rename-script

;; And here's the interim version that generated a shell script.
;; Not much point in keeping it around probably.
(defun gse-rename-script (directory wildcard)
  "Build a shell script to rename the files in DIRECTORY.
If WILDCARD is nil, script is for all files.  Otherwise it's
for the files specified by WILDCARD."
  (interactive "DDirectory: \nsRegexp: ")

  (setq directory (file-name-as-directory directory))

  ;; For now, set "files-only" to t ... do we want directories?
  (let ((files (gse-rename-directory-files directory nil wildcard nil t))
        (script-name (concat directory "doit"))
        (maxlength 0)
        (tmpfiles  nil)
        (curfile   nil)
        )
    (find-file script-name)
    (erase-buffer)

    (setq tmpfiles files)
    (while tmpfiles
      (setq curfile (car tmpfiles))
      (if (> (length curfile) maxlength)
          (setq maxlength (length curfile)))
      (setq tmpfiles (cdr tmpfiles)))

    (setq maxlength (+ 4 maxlength))

    ;; Generate the script.
    (insert "#!/bin/bash\n")
    (insert "\n")
    (setq tmpfiles files)
    (while tmpfiles
      (setq curfile (car tmpfiles))
      (if (not (string-equal curfile "doit"))
          (insert (format
                   (concat "mv %-" (int-to-string maxlength) "s %s\n")
                   (concat "\"" curfile "\"")
                   (concat "\"" curfile "\""))))
      (setq tmpfiles (cdr tmpfiles)))

    (insert "\n\n# this file will self-destruct.\n")
    (insert "/bin/rm doit\n")

    (goto-char (point-min))
    (shell-script-mode)
    (font-lock-mode t)
    ))
;;}}}

;;---------------------------------------------------------------------------


(provide 'gse-rename)
