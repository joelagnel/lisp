;;; infobook.el --- keep info node "hot list"

;; Copyright (C) 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Created: 1995-02-20

;; LCD Archive Entry:
;; infobook|Noah Friedman|friedman@prep.ai.mit.edu|
;; keep info node "hot list"|
;; $Date: 1995/02/24 23:14:34 $|$Revision: 1.1 $|~/misc/infobook.el.gz|

;; $Id: infobook.el,v 1.1 1995/02/24 23:14:34 friedman Exp $

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
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; TODO: Add keymaps?

;;; Code:

(require 'info)

(defvar infobook-file "~/.info-bookmarks"
  "*Name of file containing bookmark list for Info nodes.")

(defvar infobook-alist '()
  "*Info node hot list.")

(defvar infobook-delay-save t
  "If non-nil, don't update bookmark file until just before exiting emacs.
Otherwise, update bookmark file whenever an entry is added or removed from
the hotlist.")

(defvar infobook-version-control (default-value 'version-control)
  "*Like `version-control', but applied specifically to info bookmark file.")

;; Flag used to decide whether saving bookmark file is necessary.
(defvar infobook-alist-modified-p nil)


(defun infobook-load-hotlist ()
  (interactive)
  (load infobook-file t)
  (setq infobook-alist-modified-p nil))

(defun infobook-save-hotlist ()
  (interactive)
  (let ((buff (generate-new-buffer " *Infobook*"))
        (orig-buff (current-buffer)))
    (if (null infobook-alist)
        (message "Info bookmark list is empty; nothing to save.")
      (unwind-protect
          (progn
            (set-buffer buff)
            (if (featurep 'pp)
                (insert (pp-to-string (list 'setq 'infobook-alist
                                            (list 'quote infobook-alist))))
              (insert "(setq infobook-alist '")
              (let ((standard-output buff)
                    (print-escape-newlines t))
                (prin1 infobook-alist))
              (insert ")\n"))
            (let ((version-control infobook-version-control))
              (write-file infobook-file))
            (setq infobook-alist-modified-p nil))
        (set-buffer orig-buff)
        (kill-buffer buff)))))

(defun infobook-new-hotlist-entry (name node &optional cell)
  (let ((enode (or cell (assoc name infobook-alist))))
    (cond ((or (eq enode t)
               (null enode))
           (setq infobook-alist (cons (cons name node) infobook-alist))
           (setq infobook-alist-modified-p t))
          ((string= node (cdr enode)))
          (t
           (setcdr enode node)
           (setq infobook-alist-modified-p t))))
  (and infobook-alist-modified-p
       (not infobook-delay-save)
       (progn
         (infobook-save-hotlist)
         (setq infobook-alist-modified-p nil)))
  infobook-alist)

(defun infobook-add-current-node (&optional name)
  (interactive (list (read-string
                      (format "Entry name (default \"%s\"): "
                              Info-current-node))))
  (and (or (null name)
           (string= name ""))
       (setq name Info-current-node))
  (let ((node (concat "(" Info-current-file ")" Info-current-node))
        (enode (assoc name infobook-alist)))
    (cond ((null enode)
           (setq enode (rassoc node infobook-alist))
           (cond ((null enode)
                  (infobook-new-hotlist-entry name node t))
                 ((yes-or-no-p
                   (format "%s \"%s\".  %s"
                           "This node in hotlist already by name"
                           (car enode)
                           "Add anyway? "))
                  (infobook-new-hotlist-entry name node t))))
          ((string= node (cdr enode))
           (message "This node already in hotlist"))
          ((yes-or-no-p
            (format "\"%s\" already in use.  Replace referenced node? "
                    name))
           (infobook-new-hotlist-entry name node enode)))))

(defun infobook-goto-node (&optional name)
  (interactive (list (completing-read "Bookmark name: "
                                      infobook-alist nil t)))
  (Info-goto-node (cdr (assoc name infobook-alist))))


(add-hook 'kill-emacs-hook 'infobook-save-hotlist)
(eval-after-load "infobook" '(infobook-load-hotlist))

(provide 'infobook)

;;; infobook.el ends here
