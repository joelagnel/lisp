;;; branch.el --- working with multiple branches of projects

;; Copyright (C) 2004, 2005 Rob Walker <rob@tenfoot.org.uk>

;; Author: Rob Walker <rob@tenfoot.org.uk>
;; Version: 0.3
;; Keywords: branch ediff compare
;; X-URL: http://www.tenfoot.org.uk/emacs/

;; This file is NOT part of GNU Emacs.

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:
;; This minor mode assists working with multiple branches of a project.
;; It provides a menu (Branch) which has entries to find or compare against the
;; version of the current file in other branches.  Multiple projects are
;; supported, each with different branch structures
;;
;; To use, first define the branches of the projects you want to use.
;; This can be done using customize (M-x customize-group RET branch RET) or by
;; setting the variable branch-index directly within your .emacs.
;; Each project is a list containing name, a root path and branches.
;; Each branch is a cons cell of name and a path relative to the root. E.g:
;;  "Project A" in ~/work/project-A with branches:
;;    "Development" in head,
;;    "Version 1" in v1
;;  "Project B" in /somedir/devel/projB with branches:
;;    "Branch 2" in branches/2,
;;    "Branch 1" in branches/1
;;    "Version 1.1" in releases/1.1
;; would be set with:
;; (setq branch-index '(("Project A" "~/work/project-A"
;;                       (("Development" . "head")
;;                        ("Version 1"   . "v1")))
;;                      ("Project B" "/somedir/devel/projB"
;;                       (("Branch 2"    . "branches/2")
;;                        ("Branch 1"    . "branches/1")
;;                        ("Version 1.1" . "releases/1.1")))))
;;
;; To enable branch mode when opening a file, add branch-mode-find-file-hook
;; to find-file-hook by placing the following in your .emacs:
;;   (add-hook 'find-file-hooks 'branch-mode-find-file-hook)
;; or call branch-mode to toggle it on or off.

;;; History:
;; 03 Nov 2004 - 0.1 - Initial release
;; 23 Nov 2004 - 0.2 - Fix interactive use of branch-find-file, branch-ediff-file etc
;;                     (Allow branch other branch to take a branch name)
;; 01 Jul 2005 - 0.3 - XEmacs compatibility. Use easy-mmode.

;;TODO: projects with branches not all under one root (use absolute paths)

;;; Code:

;;
;; Requires
;;
(require 'easymenu)
(require 'easy-mmode)

;;
;; Custom options
;;
(defgroup branch nil
  "Support for multiple project branches"
  :group 'tools
  :prefix "branch-"
  :link '(url-link "http://www.tenfoot.org.uk/emacs")
  )

(defcustom branch-index nil
  "Branches of projects. See `branch-mode'"
  :type '(repeat (list :tag "Project"
                       (string    :tag "Name")
                       (directory :tag "Root path")
                       (repeat    :tag "Branches"
                                  (cons :tag "Branch"
                                        (string :tag "Name")
                                        (string :tag "Relative path")
                                        ))))
  :group 'branch)

;;
;; Buffer variables
;;
(defvar branch-current-root nil
  "The root for the current file")
(make-variable-buffer-local 'branch-current-root)

(defvar branch-current-branch nil
  "The branch for the current file")
(make-variable-buffer-local 'branch-current-branch)

;;
;; Functions
;;
(defsubst branch-root-name (&optional root)
  "Return the name of ROOT"
  (car (or root branch-current-root)))

(defsubst branch-root-path (&optional root)
  "Return the path of ROOT"
  (expand-file-name (car (cdr (or root branch-current-root)))))

(defsubst branch-root-branches (&optional root)
  "Return the branches of ROOT"
  (car (cdr (cdr (or root branch-current-root)))))

(defsubst branch-name (&optional branch)
  "Return the name of BRANCH"
  (car (or branch branch-current-branch)))

(defsubst branch-path (&optional branch)
  "Return the path of BRANCH"
  (cdr (or branch branch-current-branch)))

(defsubst branch-full-path (&optional root branch)
  "Return the full path of BRANCH under ROOT"
  (concat (branch-root-path (or root branch-current-root)) "/"
          (branch-path (or branch branch-current-branch))))

(defsubst branch-root-path-exp (&optional root)
  "Return a regexp that matches files under ROOT"
  (concat "^\\(" (branch-root-path (or root branch-current-root)) "\\)/"))

(defsubst branch-full-path-exp (&optional root branch)
  "Return a regexp that matches files under BRANCH of ROOT"
  (concat "^\\(" (branch-root-path (or root branch-current-root)) "\\)/\\("
          (branch-path (or branch branch-current-branch)) "\\)/"))

(defun branch-root-from-filename (filename)
  "Return the root that FILENAME is under, or nil if not under any root"
  ;; find the longest matching root
  (setq filename (expand-file-name filename))
  (let (best-root)
    (dolist (root branch-index best-root)
      (if (and (string-match (branch-root-path-exp root) filename)
               (or (not best-root)
                   (< (length (branch-root-path best-root))
                      (length (branch-root-path root)))))
          (setq best-root root)))))

(defun branch-from-filename (filename)
  "Returns the root and branch that FILENAME is under, or nil if not under
any root or branch.  Return value is a cons cell (root . branch)"
  (setq filename (expand-file-name filename))
  (let ((best-root (branch-root-from-filename filename))
        best-branch)
    (when best-root
        ;; find longest matching branch
        (dolist (branch (branch-root-branches best-root) best-branch)
          (if (and (string-match (branch-full-path-exp best-root branch) filename)
                   (or (not best-branch)
                       (< (length (branch-path best-branch))
                          (length (branch-path branch)))))
                (setq best-branch branch)))
        (when best-branch
          (cons best-root best-branch))
        )))

(defun branch-other-branch (filename branch)
  "Returns the equivalent of FILENAME under BRANCH.
Does not check to see if it really exists."
  (let* ((cur (or (branch-from-filename filename)
                  (error "%s is not in a valid branch" filename)))
         (root (car cur))
         (cur-branch (cdr cur)))
    (if (stringp branch)
        (setq branch (or (assoc branch (branch-root-branches root))
                         (error "%s is not a branch of %s" branch (branch-root-name root)))))
    (if (string-match (branch-full-path-exp root cur-branch) filename)
        (replace-match (concat "\\1" "/" (branch-path branch) "/") t nil filename)
      )))

(defun branch-find-file (filename branch)
  "Finds the equivalent of FILENAME under BRANCH"
  (interactive "fEnter filename: \nsEnter branch: ")
  (let ((other-filename (branch-other-branch filename branch)))
    (find-file other-filename)))

(defun branch-ediff-file (filename branch)
  "Ediff FILENAME against the equivalent of FILENAME under BRANCH"
  (interactive "fEnter filename: \nsEnter branch: ")
  (let ((other-filename (branch-other-branch filename branch)))
    (ediff-files other-filename filename)))

(defun branch-ediff-dir (dir branch)
  "Ediff DIR against the equivalent of DIR under BRANCH"
  (interactive "fEnter dir: \nsEnter branch: ")
  (let ((other-dir (branch-other-branch dir branch)))
    (ediff-directories other-dir dir nil)))

;;
;; Menu
;;
(defun branch-mode-menu-func (m)
  (append
   (mapcar '(lambda (x)
              (vector (concat "Open file in " (branch-name x) " branch")
                      `(branch-find-file buffer-file-name ',x)
                      t)) (branch-root-branches branch-current-root))
   '("----")
   (mapcar '(lambda (x)
              (vector (concat "Ediff file against " (branch-name x) " branch")
                      `(branch-ediff-file buffer-file-name ',x)
                      t)) (branch-root-branches branch-current-root))
   '("----")
   (mapcar '(lambda (x)
              (vector (concat "Ediff directory against " (branch-name x) " branch")
                      `(branch-ediff-dir (file-name-directory buffer-file-name) ',x)
                      t)) (branch-root-branches branch-current-root))
   ))

(defvar branch-mode-menu-bar
  '("Branch"
    :filter branch-mode-menu-func
    ))

(defvar branch-mode-menu nil
  "Menu for Branch utilities")

(defvar branch-mode-map
  (let ((m (make-sparse-keymap)))
    (easy-menu-define branch-mode-menu m "Branch" branch-mode-menu-bar)
    m)
  )

;;
;; Minor mode
;;
(define-minor-mode branch-mode
  "This minor mode assists working with multiple branches of a project.
It provides a menu (Branch) which has entries to find or compare against the
version of the current file in other branches.  Multiple projects are
supported, each with different branch structures

To use, first define the branches of the projects you want to use.
This can be done using customize (M-x customize-group RET branch RET) or by
setting the variable `branch-index' directly within your .emacs.
Each project is a list containing name, a root path and branches.
Each branch is a cons cell of name and a path relative to the root. E.g:
 \"Project A\" in ~/work/project-A with branches:
   \"Development\" in head,
   \"Version 1\" in v1
 \"Project B\" in /somedir/devel/projB with branches:
   \"Branch 2\" in branches/2,
   \"Branch 1\" in branches/1
   \"Version 1.1\" in releases/1.1
would be set with:
 (setq branch-index '((\"Project A\" \"~/work/project-A\"
                       ((\"Development\" . \"head\")
                        (\"Version 1\"   . \"v1\")))
                      (\"Project B\" \"/somedir/devel/projB\"
                       ((\"Branch 2\"    . \"branches/2\")
                        (\"Branch 1\"    . \"branches/1\")
                        (\"Version 1.1\" . \"releases/1.1\")))))

To enable branch mode when opening a file, add branch-mode-find-file-hook
to find-file-hook by placing the following in your .emacs:
  (add-hook 'find-file-hooks 'branch-mode-find-file-hook)
or call `branch-mode' to toggle it on or off.
"
  nil " BR" branch-mode-map)

(defun branch-mode-init ()
  (when branch-mode
    (let ((res (branch-from-filename buffer-file-name)))
      (when res
        (setq branch-current-root   (car res))
        (setq branch-current-branch (cdr res))
        (easy-menu-add branch-mode-menu branch-mode-map)))))

(add-hook 'branch-mode-hook 'branch-mode-init) 

;;
;; Find file hook
;;
(defun branch-mode-find-file-hook ()
  (if (branch-root-from-filename buffer-file-name)
      (branch-mode 1)))

(provide 'branch)

