;;; dirtree.el --- functions for building directory-tree lists

;; Copyright (C) 1996, 1998, 2000, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1998-04-24

;; $Id: dirtree.el,v 1.6 2004/06/11 22:50:19 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I use the function `build-subdirectory-list' (and its friends) to
;; initialize my load-path, among other things.  At the root of my lisp
;; directories I have ".build-subdirs.els" files which specify
;; subdirectories which should also go into the path.

;; Updates of this file may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(defvar build-subdirectory-list-file-name ".build-subdirs.els"
  "Name of file to parse by `build-subdirectory-list'.
This variable should specify the name of the file as it would appear in a
directory, and so it should not contain any subdirectory names.
For example it could be set to \".build-subdirs.els\", but should not be set to
\"foo/.build-subdirs.els\".")

(defun build-subdirectory-list-deep (dir)
  "Like `build-subdirectory-list', but always descend recursively."
  (build-subdirectory-list dir t))

(defun build-subdirectory-list (dir &optional recurp)
  "Build a list of directory names, starting with DIR.
Optional arg RECURP means descend recursively into subdirectories of DIR to
look for candidate directories.

In directory DIR, look for a \"subdirectory-list\" file named by the value
of the variable `build-subdirectory-list-file-name'.  That file should
contain lisp expressions which compute relative directory names to be added
to the returned list.  The file may contain comments and/or any arbitrary
lisp expressions, but the top-level return value of each should either be
`nil' \(which will be ignored)\), a string representing a directory name,
or a list of directory names.

When RECURP is non-`nil', any directory names built up by scanning the
subdirectory-list file are in turn searched for subdirectory-list files.
The final list returned at the top-level will be in depth-first order
\(i.e. subdirectories will appear before their parent directories\).

For example, any or all of the expressions are appropriate entries in a
subdirectory-list file \(as are any other valid s-expressions\):

     \"foo\" \"bar\" \"baz\"

     '\(\"foo\" \"bar\" \"baz\"\)

     \(list \"foo\" \"bar\" \"baz\"\)

     \(mapcar #'\(lambda \(s\)
                 \(upcase \(concat s \".DIR;1\"\)\)\)
             '\(\"foo\" \"bar\" \"baz\"\)\)"
  (let* ((bufname " *Init Directories*")
         (buf (get-buffer-create bufname))
         (list nil))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (setq buffer-read-only nil)
          (buffer-disable-undo buf)
          (widen)
          (erase-buffer)
          (setq list
                (cons dir
                      (build-subdirectory-list-internal dir recurp list buf))))
      (kill-buffer buf))
    (nreverse list)))

(defun build-subdirectory-list-internal (dir recurp list buf)
  (setq dir (file-name-as-directory dir))
  (let ((file (concat dir build-subdirectory-list-file-name))
        (objlist nil)
        obj)
    (cond
     ((file-readable-p file)
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))

      (condition-case err
          (while (setq obj (eval (read buf)))
            (setq objlist (cons obj objlist)))
        (end-of-file nil)
        (error (apply 'signal err)))

      (setq objlist (nreverse objlist))

      (while objlist
        (setq obj (car objlist))
        (setq objlist (cdr objlist))

        (if (stringp obj)
            (setq obj (list obj))
          (setq obj (nreverse obj)))

        (while obj
          (if (null obj)
              nil
            (setq list (cons (concat dir (car obj)) list))
            (and recurp
                 (file-directory-p (car obj))
                 (setq list (build-subdirectory-list-internal
                             (car obj) recurp list buf)))
            (setq obj (cdr obj)))))))
    list))


(defun directory-tree (dir &optional depth-first predicate filter depth-limit)
  "Return a nested list representing the tree of directories beneath DIR.
Each member of the returned list is a string representing the name of a
subdirectory; or if the subdirectory has children of its own, a list of the
subdirectory and its children.  See examples at the end.

Optional arg DEPTH-FIRST non-nil means subdirectories of a given parent
are listed first.  Normally the parent is listed first.

Optional arg PREDICATE is a function of one string argument: a directory
name.  If it returns nil, the directory name is not included in the
resulting list and no subdirectory of that directory will be searched.

Optional arg FILTER is a function of one string argument: a directory name.
Whatever value is returned by this function is what will occur in the
return list, instead of the original directory name.

Optional arg DEPTH-LIMIT is an integer specifying the maximum recursion
depth of the search.  For example a value of 1 means return a list of only
the directories immediately in DIR.  The default is to descend all the way
to the bottom of the directory hierarchy.

Examples:

        (directory-tree \"/foo/\")
        => (\"/foo/a/\"
            (\"/foo/b/1/\"
             \"/foo/b/2/\"
             (\"/foo/b/3/bar/\"
              \"/foo/b/3/\")
             \"/foo/b/\")
            \"/foo/c/\"
            \"/foo/\")

        (directory-tree \"/foo/\" t)
        => (\"/foo/\"
            \"/foo/a/\"
            (\"/foo/b/\"
             \"/foo/b/1/\"
             \"/foo/b/2/\"
             (\"/foo/b/3/\"
              \"/foo/b/3/bar/\"))
            \"/foo/c/\")"
  (and (file-directory-p dir)
       (let* ((default-directory dir)
              (lst (sort (file-name-all-completions "" dir) 'string-lessp))
              (l lst))
         (while l
           (if (and (file-directory-p (car l))
                    (not (member (car l) '("./" "../")))
                    (or (null predicate)
                        (funcall predicate (car l))))
               (setcar l (concat (file-name-as-directory dir) (car l)))
             (setq lst (delq (car l) lst)))
           (setq l (cdr l)))
         (and filter
              (setq dir (funcall filter dir)))
         (cond ((and lst
                     (or (not (integerp depth-limit))
                         (>= (setq depth-limit (1- depth-limit)) 0)))
                (setq l (mapcar
                         (lambda (dir)
                           (directory-tree dir depth-first predicate filter
                                           depth-limit))
                         lst))
                (if depth-first
                    (nconc l (cons dir nil))
                  (cons dir l)))
               (t dir)))))


(defun walk-filesystem (dir action &optional no-error)
  "Traverse filesystem starting with directory DIR and call ACTION.
Starting with the directory specified by DIR, walk down the directory tree
recursively and call ACTION for each file or directory found.
Optional argument NO-ERROR means do not abort if a subdirectory cannot be
traversed for permission reasons, but it will not prevent exceptions in the
ACTION function.

ACTION function receives two arguments: a directory name and a file in that
directory.  Note that the file argument may in fact be a directory itself.
If the argument is a directory and ACTION returns nil, that subdirectory
will not be traversed.  For non-directory entries, ACTION is only called
for side effect.

Files are not traversed in any particular order; if ACTION builds a list of
results, they may need to be sorted afterward."
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir (1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (if no-error
                        (condition-case err
                            (directory-files dir nil nil t)
                          (error nil))
                      (directory-files dir nil nil t)))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (walk-filesystem fullname action no-error)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))

(defun existing-directory-root (dir)
  "Return the leading portion of DIR which actually exists in the filesystem.
If the entire directory hierarchy exists, just return DIR."
  (while (not (file-exists-p dir))
    (setq dir (file-name-directory (directory-file-name dir))))
  dir)

(provide 'dirtree)

;;; dirtree.el ends here
