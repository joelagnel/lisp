                                                                         
;;;    From: Steve Kemp (skx@tardis.ed.ac.uk)
;;;    Subject: tree.el -- Create + manipulate expanding/collapsing trees.
;;;    Newsgroups: gnu.emacs.sources
   

;;;   Recently there has been talk of a CPAN-like arrangement for
;;;  Emacs Lisp, I think that this is a good idea, which would need
;;;  some form of package management.

;;;   I've been writing some code to this end, the three biggest pieces
;;;  of work I see are:

;;;   1.  Comparing versions of lisp files,  (I've got a "require-version"
;;;     function which replaces "require" nicely).

;;;   2.  Having a browsable, tree-like, navigation system for navigating
;;;     the package hierarchy.

;;;   3.  Downloading + configureing the lisp.

;;;   Here I present an early version of a _generalized_ tree
;;;  "widget" that may be used by any lisp programmer; along with
;;;  a small dired test case.

;;;   Enjoy.

;;; Steve
;;; ---
;;; http://GNUSoftware.com/ -- GNU Software for Windows Users
;;; http://steve.org.uk/    -- All about Steve

;; Tree.El -- Simple expandable/collapsable tree control

;; Copyright (C) 2000 by Steve Kemp

;; COPYRIGHT NOTICE

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

;;; Version 0.2

;;; Description:

;;  tree.el allows lisp coders to display and control
;; the use of tree structures within buffers.
;;
;;  To use the tree you need only define the original nodes
;; that are to be inserted upon startup, and define a function
;; which will be called to insert children when branches are
;; expanded.
;;
;;  For example a simple directory browser can be written
;; using the following function, and sample code:
;;
;;  ; Function to be called when a node is expanded.
;; (defun dired-root-expand (node)
;;   (let ((dirs nil))
;;     (if (file-directory-p (car node))
;;      (progn
;;        (setq dirs (directory-files (car node)))
;;        (setq dirs (delete "." dirs))
;;        (setq dirs (delete ".." dirs))
;;        )
;;    dirs)))
;;
;;  ; Test code
;;  (progn
;;   (pop-to-buffer (get-buffer-create "*Poor Mans Dired*"))
;;   (erase-buffer)
;;   (setq tree-root-nodes (list "/"))
;;   (tree-create tree-root-nodes 'dired-root-expand t)
;;   (local-set-key " " 'tree-toggle-branch)
;;  )
;;
;;
;;; Steve Kemp
;;; --
;;; http://GNUSoftware.com/

(defvar tree-indent-value 2
  "Number of spaces to indent sub-levels of branches.")

(defvar tree-expand-function nil
  "Function to call to get subkeys.
This function should return a list containing the text
to insert.")

(defvar tree-seperator "/"
 "This is the character that will be used to build
a path of the current branch, or node, which is used
in the call to `tree-expand-function'")



(defun tree-insert-and-pad (string width)
  "Insert `string', and pad with spaces so that it takes up width characters."
  (let ((strlen 0))
    (setq strlen (length string))
    (if string
        (insert string))
    (while (< strlen width)
      (insert " ")
      (setq width (- width 1)))))



(defun tree-create( ROOT_NODES EXPAND_FUNCTION &optional SORT_NODES)
  "Create an expandable tree.
The tree is inserted into the current buffer.

ROOT_NODES is a list of names that may be used for
the top level branches - these are displayed in a closed
manner.

If SORT_NODES is non-nil then the root nodes are sorted
before insertion.

EXPAND_FUNCTION is the function which is called to find
a list of children to insert beneath a branch when it is
expanded.  This function takes a single argument, which is
the path of the branch being expanded.  It should return a
list of items to insert, or nil to insert no entries."
  (setq tree-expand-function EXPAND_FUNCTION)
  (let ((root-keys  ROOT_NODES))
    (if SORT_NODES
        (setq root-keys (sort root-keys 'string<)))
    (while (car root-keys)
      (tree-insert-and-pad "" tree-indent-value)
      (insert (concat "[+] " (car root-keys) "\n"))
      (setq root-keys (cdr root-keys))))
  )

(defun tree-get-indent-level ()
  "Get the level of indentation of the value under the point."
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "^\\([ ]*\\)\\[" nil t)
    (length (match-string 1))))


(defun tree-get-current-path()
  "Return the path from the root to the current node."
  (interactive)
  (let ((path "")
        (temp "")
        (working 1)
        (position (point))
        (length (tree-get-indent-level))
        (finished nil))
    (save-excursion
      (end-of-line)
      (if (eq 0 (tree-get-indent-level))
          (progn
            ;;  On the first line.
            (re-search-backward "^\\[[+\\|-]\\][ ]*\\(.*\\)$" nil t)
            (setq path (concat (match-string 1) tree-seperator)))
        (progn
          ;; On a subkey
          (while (and (not finished)
                      (re-search-backward "^\\([ ]*\\[\\)[+\\|-]\\][ ]*\\(.*\\)
$" nil t))
            (setq temp  (match-string 2))
            (if (equal length (tree-get-indent-level))
                (progn
                  (setq path (concat temp tree-seperator path))
                  (setq length (- length tree-indent-value))
                  ))
            (if (eq 0 (tree-get-indent-level))
                (setq finished t))))))
    (save-excursion
      (let ((end nil)
            (current (point)))
        (setq end)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (if (re-search-forward "^[ ]*\\(.*\\) "  end t)
            (setq path (cons path (match-string 1)))
          )))
    path))

(defun tree-toggle-branch()
  "Called to interactively expand, or contract the current branch."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((eol (point)))
      (beginning-of-line)
      (re-search-forward "\\[" eol t)
      (if (looking-at "+")
          (tree-expand-region)
        (if (looking-at "-")
            (tree-contract-region)
          (progn
            (end-of-line)
            (re-search-backward "\\[\\+\\|-\\]" nil t)
            (tree-toggle-key)))))))

(defun tree-contract-region ()
  "Contract a branch."
  (interactive)
  (delete-char 1)
  (insert "+")
  (end-of-line)
  (let ((pos (point))
        (end nil)
        (level (tree-get-indent-level))
        (finished nil))
    (forward-line 1)
    (beginning-of-line)
    (while (not finished)
      (setq end (re-search-forward "^\\([ ]*\\[\\)" nil t))
      (if (eq end nil)
          (setq finished t))
      (if (<= (tree-get-indent-level) level)
          (setq finished t)))
    (if end
        (progn
          (beginning-of-line)
          (delete-region pos (- (point) 1))
          )
      (delete-region pos (point-max)))))

(defun tree-expand-region( )
  "Calls the user defined function to insert new entries.
The user defined function is specified in the call to
`tree-create', and is assumed to return a list of items
to insert."

  (delete-char 1)
  (insert "-")
  (end-of-line)

  (let ((keys nil)
        (depth (tree-get-indent-level)))
    ;; Get the subkeys to insert.
    (setq keys (funcall tree-expand-function (tree-get-current-path)))

    (if keys
        (progn

          ;; Some branches were returned.
          (setq depth (+ depth tree-indent-value))

          (setq keys (sort keys 'string<))
          (while keys
            (insert "\n")
            (setq temp depth)
            (while (> temp 0)
              (insert " ")
              (setq temp (- temp 1)))
            (insert "[+] ")
            (insert (car keys))
            (setq keys (cdr keys)))))))


(provide 'tree)

