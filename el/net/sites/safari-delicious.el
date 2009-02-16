;;; safari-delicious.el --- import safari bookmarks to del.icio.us

;; Copyright (C) 2005  Edward O'Connor

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; utilities for importing your Safari bookmarks into del.icio.us.
;; beware the lack of documentation. MUHAHAAHAHAHAA.

;;; Code:

(require 'esh-util) ; for `eshell-flatten-list'
(require 'osx-plist)
(require 'delicioapi)

(defvar safari-bookmarks-plist "~/Library/Safari/Bookmarks.plist"
  "Plist file in which Safari keeps your bookmarks.")

(defun safari-bookmarks-parse (hash)
  (let ((web-bookmark-type (gethash "WebBookmarkType" hash "unknown"))
        bookmarks)
    (cond
     ((equal web-bookmark-type "WebBookmarkTypeProxy")) ; ignore
     ((equal web-bookmark-type "WebBookmarkTypeList")
      (let ((children (gethash "Children" hash)))
        (setq bookmarks
              (eshell-flatten-list
               (apply 'append
                      (mapcar 'safari-bookmarks-parse children)
                      bookmarks)))))
     ((equal web-bookmark-type "WebBookmarkTypeLeaf")
      (let ((url (gethash "URLString" hash))
            (title (gethash "title" (gethash "URIDictionary" hash))))
        (push (cons url title) bookmarks))))
    bookmarks))

;;;###autoload
(defun safari-bookmarks-get (&optional bookmarks-file)
  "Read your Safari bookmarks out of BOOKMARKS-FILE.
Returns your bookmarks as an alist of (URL . TITLE) pairs. Uses the
value of `safari-bookmarks-plist' if BOOKMARKS-FILE is unspecified."
  (let ((file-name (make-temp-file "safbkmrk-")))
    (copy-file (or bookmarks-file safari-bookmarks-plist) file-name t)
    (shell-command (format "plutil -convert xml1 %s" file-name))
    (safari-bookmarks-parse (osx-plist-parse-file file-name))))

(defun safari-bookmarks-to-delicious (bookmarks)
  (mapc
   (lambda (cons)
     (sit-for 1)
     (ignore-errors (delicious-api-post (car cons) (cdr cons) "from-safari")))
   bookrmarks))

(provide 'safari-delicious)
;;; safari-delicious.el ends here
