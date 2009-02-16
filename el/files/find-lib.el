;;; find-lib.el --- Find files in Emacs' `load-path' with completion.
;; $Id: find-lib.el,v 1.14 2004/04/27 22:51:56 wence Exp $

;; This file is NOT part of Emacs.

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>
;; Filename: find-lib.el
;; Version: $Revision: 1.14 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-07-24
;; Keywords: finding files

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;;; Commentary:
;; Have you ever spent time searching for a .el file you know you have
;; somewhere?  Or have you ever tried to remember exactly what a file
;; was called.  Then this might be what you want, it is basically a
;; wrapper around (find-file (locate-library "foo.el")), but with
;; filename completion.

;; This provides one user function `find-lib-locate-library', which
;; you can bind to a key if you want.


;;; History:
;; 2002-07-27  Posted to gnu.emacs.sources
;; After which I find out that this is already provided by lib-complete.el
;; and ilocate-library.el :). AND fff.el (with fff-elisp.el)

;;; TODO:
;; Maybe generalise for all file types, instead of just .el files?
;; 2002-07-28  Functionality exists, `find-lib-load-path' and
;; `find-lib-locate-file'.

;;; Code:
(defvar find-lib-elisp-file-list (make-vector 29 nil)
  "Cache for elisp version of `find-lib-find-files'.")

(defvar find-lib-elisp-path load-path
  "*Default path to search for elisp files in.")

(defvar find-lib-use-cache nil
  "*Whether find-lib should use a file cache.
If this is nil, then
the obarray holding filename completions will be refilled each time
you call `find-lib-{locate,load,find}-file'.")

(defun find-lib-find-files (file-list &optional ext path)
  "Fill an obarray (FILE-LIST) with all files of type (extension) EXT in PATH.

If EXT is nil, we assume a value of \"el\".  If PATH is nil, we use
`find-lib-elisp-path' which defaults to the value of `load-path'."
  (let* ((suffix (concat "\\." (or ext "el") "$"))
         (path (or path find-lib-elisp-path))
         (files (mapcar #'file-name-sans-extension
                        (apply #'nconc
                               (mapcar #'(lambda (dir)
                                           (and (file-readable-p dir)
                                                (directory-files dir nil suffix)))
                                       path)))))
    (mapc #'(lambda (file)
              (intern file file-list))
          files)))

(defun find-lib-locate-file (file file-list &optional ext path)
  "Locate FILE completing from FILE-LIST with extension EXT in PATH."
  (catch 'answer
    (let* ((ext (or ext ".el"))
           (path (or path find-lib-elisp-path))
           (file (concat file ext))
           result)
      (mapc
       #'(lambda (dir)
           (let ((try (expand-file-name file dir)))
             (and (file-readable-p try)
                  (throw 'answer try))))
       path))))

;;;###autoload
(defun find-lib-load-file (file)
  "Load FILE."
  (interactive
   (list
    (progn (or find-lib-use-cache
               (find-lib-find-files find-lib-elisp-file-list))
           (completing-read "Load file: " find-lib-elisp-file-list))))
  (setq file (file-name-sans-extension
              (find-lib-locate-file file find-lib-elisp-file-list)))
  (load file))

(defun find-lib-find-file (file file-list &optional ext path)
  "Find FILE completing from FILE-LIST with EXT."
  (find-file (find-lib-locate-file file file-list ext path)))

;;;###autoload
(defun find-lib-find-elisp-file (file)
  (interactive
   (list
    (progn (or find-lib-use-cache
               (find-lib-find-files find-lib-elisp-file-list))
           (completing-read "Find file: " find-lib-elisp-file-list))))
  (find-lib-find-file file find-lib-elisp-file-list))

(provide 'find-lib)

;;; find-lib.el ends here
