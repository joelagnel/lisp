;;; file-props.el --- Add file properties to your files
;;
;; Copyright (C) 2006 Mathias Dahl
;;
;; Version: 0.1.1
;; Keywords: search, convenience, files
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; file-props.el provides a way to "tag" or "label" your files.
;; Actually, the functionality is generic and supports adding any type
;; of property to your files.  Right now "tags" and a "comment" can be
;; added.
;;
;; After having added this meta data you can use it to find files in
;; different ways.  Provided is a command to list all files having a
;; certain tag in a dired buffer.
;;
;; NOTE: This is not stable yet, it is a proof-of-concept, to see if
;; it could be useful.  Use at own your risk.
;;
;;; Installation:
;;
;; Place this file in your `load-path'.
;;
;; Put this in your .emacs file:
;;
;; (require 'file-props)
;; (file-props-load-properties)
;;
;; NOTE: If you don't load the properties file it will be overwritten
;; with only the properties you define in this session.  I'm not sure
;; if this is the best approach though.  One could make file-props.el
;; automactially load all file properties when it is loaded.
;;
;;; Usage:
;;
;; - Adding tags:
;;
;; In dired, mark a couple of files and type M-x
;; file-props-dired-add-tags RET.
;;
;; On the prompt, enter one or more tags, separated with a comma and
;; type RET.
;;
;; - Finding files:
;;
;; Type M-x file-props-find-tags-dired RET.
;;
;; Enter a tag and type RET.
;;
;; You will be presented with a dired buffer containing the files
;; having the tag you searched for.
;;
;; - Marking files:
;;
;; You can use the commad `file-props-dired-mark-from-tag' in a Dired
;; buffer to mark files that have a certain file tag.  It is
;; equivalent to how some of the other `%-commands' in dired works.
;;
;; - Edit tags and comments:
;;
;; Mark some file in Dired, either manually or using
;; `file-props-dired-mark-from-tag', then execute `file-props-edit'.
;; This will open up a new buffer where you can edit tags can comments
;; easily.
;;
;; - Other uses:
;;
;; Currently, the only other command is `file-props-dired-add-comment'
;; which will add a comment property to the file.  The idea is that
;; while tags are used to categorize or label your files, a comment is
;; more specific and can act as the description of the file.
;;
;; As explained above, tags and comments are just examples of meta
;; data you might want to add to your files, other types of data
;; should be easy to add if you want.  Look at
;; `file-props-dired-add-tags' and `file-props-dired-add-comment' for
;; examples.
;;
;;
;;; Wish-list:
;;
;; - When searching for files with tags the user should be able to
;;   specify multiple tags, not just one, when further tags are
;;   specified (with completion) only those tags are offered which
;;   have an intersection with the previous ones, (see del.icio.us)
;;
;; - Renaming and deleting tags.
;;
;;
;;; History:
;;
;;
;; Version 0.1, 2006-06-27
;;
;; First release.
;;
;; Version 0.1.1, 2006-06-28
;;
;; Removed warning when loading properties.  It wasn't very
;; useful.
;;
;; Removed question about current directory from
;; `file-props-find-tags-dired'.  I don't think this was needed
;; either.
;;
;; Replaced `read-string' with `completing-read' when reading tags to
;; search for.  I know Drew will like this because it should enabled
;; `icicles' to assimilate this functionality... :)
;;
;; Added command `file-props-dired-mark-from-tag'.
;;
;; Added command `file-props-dired-edit'.
;;
;; Fixed problems with whitespace when splitting tags string entered
;; by the user.  Two new functions was added, `file-props-trim-spaces'
;; and `file-props-split-and-trim'.
;;
;; Added a new way to enter multiple tags, using completion and made
;; this the default.  The option
;; `file-props-read-tags-comma-separated' was added if the user wants
;; to use the old method, using a comma separated string.
;;
;;; Code:


(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defgroup file-props nil
  "File properties lets you add different kinds of properties or
meta-data to files and have these properties saved to a central
data file.  This information can, for example, be used to mark
file having a certain meta-data in Dired."
  :group 'Convenience)


(defcustom file-props-data-file "~/.emacs.d/file-props"
  "File in which the file properties are saved."
  :type 'file
  :group 'file-props)


(defvar file-props-list nil
  "List containing the file properties.")


(defvar file-props-tag-history nil
  "Keeps tag history when doing `completing-read'.")


(defvar file-props-widget-list nil
  "List to keep track of meta data in edit buffer.")


(defun file-props-add-property (file property value)
  "For FILE, set PROPERTY to VALUE.
If the property does not exist, it will be created.  If it
exists, the value will be overwritten."
  (unless (assoc file file-props-list)
    (setq file-props-list (append file-props-list (list (list file nil)))))
  (setcdr (assoc file file-props-list)
          (plist-put (cdr (assoc file file-props-list)) property value)))


(defun file-props-save-properties ()
  "Save file properties.
Save file properties to file `file-props-data-file'."
  (with-temp-file (expand-file-name file-props-data-file)
    (prin1 file-props-list (current-buffer))))


(defun file-props-load-properties ()
  "Load all file properties.
Load all file properties from file `file-props-data-file'.  If
the files does not exist, no harm is done; it will be created
when file properties are added to files."
  (let ((file (expand-file-name file-props-data-file))
        buf)
    (when (file-exists-p file)
      (setq buf (find-file-noselect
                 file))
      (setq file-props-list (read buf))
      (kill-buffer buf))))


(defcustom file-props-read-tags-comma-separated nil
  "Read multiple tags using a comma separated string.
If t, the user needs to provide a comma separated string when
entering multiple tags."
  :type 'boolean
  :group 'file-props)


(defun file-props-read-tags ()
  "Read file tags.
Read file tags, either using completion or as a comma separated
string.  The variable `file-props-read-tags-comma-separated'
determines how multiple tags are read."
  (if file-props-read-tags-comma-separated
      (file-props-split-and-trim
       (read-string "Enter one or more tags.\
  Separate multiple tags by a comma: ") ",")
    (file-props-read-tag-multi)))


(defun file-props-dired-add-tags ()
  "Add file tags to current or marked files."
  (interactive)
  (mapc
   (lambda (x)
     (file-props-add-property
      x 'tags (file-props-read-tags)))
   (dired-get-marked-files))
  ;; Don't save to file if there are no properties.  This works as a
  ;; kind of security measure, making sure that the data file is not
  ;; overwritten with no data if the user by mistake did not load the
  ;; old properties before adding new ones.  If anyone can come up
  ;; with a better approach than this, please don't hesitate to
  ;; contact me.
  (if file-props-list
      (file-props-save-properties)))


(defun file-props-add-tags-to-current-file ()
  "Add file tags to currently open file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (file-props-add-property
         file 'tags
         (file-props-read-tags))
      (message "This buffer has no associated file"))
    (if file-props-list
        (file-props-save-properties))))


(defun file-props-dired-add-comment ()
  "Add file comment to current or marked files."
  (interactive)
  (let ((comment (read-string "Enter comment: ")))
    (mapc
     (lambda (x)
       (file-props-add-property x 'comment comment))
     (dired-get-marked-files)))
  (file-props-save-properties))


(defun file-props-list-all-tags ()
  "Return all unique tags for all files."
  (unless file-props-list
    (file-props-load-properties))
  (let (all-tags)
    (mapc
     (lambda (x)
       (let ((tags (plist-get (cdr x) 'tags)))
         (mapc
          (lambda (y)
            (unless (member y all-tags)
              (setq all-tags (append all-tags (list y)))))
          tags)))
     file-props-list)
    all-tags))


(defun file-props-get-property (file property)
  "Return from FILE property PROPERTY's value."
  (plist-get (cdr (assoc file file-props-list)) property))


(defun file-props-get-tags (file)
  "Return list of tags for FILE."
  (file-props-get-property file 'tags))


(defun file-props-get-comment (file)
  "Return comment property for FILE."
  (file-props-get-property file 'comment))


(defun file-props-find-files-from-tag (tag)
  "Return a list of all files having file tag TAG."
  (let (files)
    (mapc
     (lambda (x)
       (when (member tag (plist-get (cdr x) 'tags))
         (setq files (append files (list (car x))))))
     file-props-list)
    files))


(defun file-props-read-tag-multi ()
  "Read multiple tags with completion."
  (let (tags)
    (while (not (string= "" (setq tag (file-props-read-tag
                                       "Input one or more tags, \
typing RET in between. An empty value ends input): "))))
      (setq tags (append tags (list tag))))
    tags))


(defun file-props-read-tag (prompt)
  "Display PROMPT and read tag, completing from available tags."
  (completing-read
   prompt (file-props-list-all-tags) nil nil nil
   'file-props-tag-history))


(defun file-props-find-tags-dired ()
  "Search for file tag TAG to find files and list them in dired.
It generates a result like `find-dired' does."
  (interactive)
  (let* ((tag (file-props-read-tag "Tag to search for: "))
         (files (file-props-find-files-from-tag tag)))
    (if files
        (dired (cons default-directory files))
      (message "No files with tag `%s'" tag))))


(defun file-props-dired-mark-from-tag ()
  "Mark all files having a certain file tag.
In Dired, find all files that have a certain file tag and mark
them if they exist in the current directory."
  (interactive)
  (let* ((tag (file-props-read-tag "Tag to search for: "))
         (files (file-props-find-files-from-tag tag))
         (count 0))
    (when files
      (save-excursion
        (mapc
         (lambda (x)
           (goto-char (point-min))
           (when (and (string= (file-name-directory x) default-directory)
                      (search-forward-regexp
                       (format "%s$" (file-name-nondirectory x))
                       nil t))
             (setq count (1+ count))
             (dired-mark 1)))
         files)))
    (message "%d files marked" count)))


(defun file-props-dired-edit ()
  "Edit comment and tags of current or marked files.
Edit comment and tags for all marked files in an easy-to-use
form."
  (interactive)
  (setq file-props-widget-list nil)
  ;; Setup buffer.
  (let ((files (dired-get-marked-files)))
    (switch-to-buffer "*File Props Edit*")
    (kill-all-local-variables)
    (make-local-variable 'widget-example-repeat)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    ;; Some help for the user.
    (widget-insert
"\nEdit comments and tags for each file.  Separate multiple tags
with a comma.  Move forward between fields using TAB or RET.
Move to the previous field using backtab (S-TAB).  Save by
activating the Save button at the bottom of the form or cancel
the opration by activating the Cancel button.\n\n")
    ;; Here comes all file names and a comment and tag field for each
    ;; file.
    (mapc
     (lambda (file)
       (let (comment-widget tag-widget)
         (widget-insert file)
         (widget-insert "\n\nComment: ")
         (setq comment-widget
               (widget-create 'editable-field
                              :size 40
                              :format "%v "
                              :value (or (file-props-get-comment file) "")))
         (widget-insert "\nTags:    ")
         (setq tag-widget
               (widget-create 'editable-field
                              :size 40
                              :format "%v "
                              :value (or (mapconcat
                                          (lambda (tag)
                                            tag)
                                          (file-props-get-tags file)
                                          ",") "")))
         ;; Save information in all widgets so that we can use it when
         ;; the user saves the form.
         (setq file-props-widget-list
               (append file-props-widget-list
                       (list (list file comment-widget tag-widget))))
         (widget-insert "\n\n")))
     files))
  ;; Footer with Save and Cancel button.
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify
                 (lambda (&rest ignore)
                   (file-props-save-edits)
                   (bury-buffer)
                   (message "Done."))
                 "Save")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify
                 (lambda (&rest ignore)
                   (bury-buffer)
                   (message "Operation canceled."))
                 "Cancel")
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  ;; Jump to the first widget.
  (widget-forward 1))

(defun file-props-save-edits ()
  "Save information found in `file-props-widget-list'.
Use the information in `file-props-widget-list' to save comments
and tags for their respective file.  Internal function used by
`file-props-dired-edit'."
  (mapc
   (lambda (x)
     (let ((file (car x))
           (comment (widget-value (cadr x)))
           (tags (widget-value (caddr x))))
       (file-props-add-property file 'comment comment)
       (file-props-add-property
        file 'tags (file-props-split-and-trim tags ","))))
   file-props-widget-list)
  (file-props-save-properties))


(defun file-props-trim-spaces (str)
  "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
  (string-match "\\`\\s-*\\(.*?\\)\\s-*\n?\\'" str)
  (match-string 1 str))


(defun file-props-split-and-trim (str split-str)
  "Call `split-string' and trim leading and trailing spaces.
Split string STR using SPLIT-STR and trim leading and trailing
spaces from the resulting list items by calling
`file-props-trim-spaces'."
  (mapcar
   (lambda (x)
     (file-props-trim-spaces x))
   (split-string str split-str)))


(provide 'file-props)

;;; file-props.el ends here
