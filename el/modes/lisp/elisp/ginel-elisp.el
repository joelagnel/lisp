;;; ginel-elisp.el --- Locate Emacs Lisp source files.

;; Copyright (C) 1998 Stefan Hornburg

;; Author: Stefan Hornburg <racke@gundel.han.de>
;; Maintainer: Stefan Hornburg <racke@gundel.han.de>
;; Version: 0.0.0
;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Change log:
;; Fri Jan 30 19:03:20 1998 MET	Stefan Hornburg	<racke@gundel.han.de>
;;  * ginel-elisp.el v0.0.0 released.

;;; Code:

(defvar ginel-locate-library-source-verbose t
  "Whether `ginel-locate-library-source' should print log messages.")

;;;###autoload
(defun ginel-locate-library-source (library &optional nosuffix)
  "Returns full path name of the source file for Emacs library LIBRARY.
This command searches the directories in `load-path'.
 Optional second arg NOSUFFIX non-nil means don't add suffixes `.el'
to the specified name LIBRARY."
  (interactive "sLocate library source: ")
  (catch 'answer
    (mapcar
     '(lambda (dir)
	(mapcar
	 '(lambda (suf)
	    (let ((try (expand-file-name (concat library suf) dir)))
	      (and (file-readable-p try)
		   (null (file-directory-p try))
		   (progn
		     (and ginel-locate-library-source-verbose
				  (message "Source is file %s" try))
		     (throw 'answer try)))))
	 (if nosuffix '("") '(".el" ""))))
     load-path)
	(and ginel-locate-library-source-verbose
		 (message "No source for library %s in search path" library))
    nil))

;;;###autoload
(defun ginel-find-file-library-source (library)
  "Edit library LIBRARY.
Switch to a buffer visiting LIBRARY, creating one if none already exists.
LIBRARY is located by `ginel-locate-library-source'."
  (interactive "sFind library source: ")
  (let ((ginel-locate-library-source-verbose nil) file)
	(setq file (ginel-locate-library-source library))
	(if file
		(find-file file)
	  (signal 'file-error
			  (list (format "Source file for library %s not found"
							library))))))

(provide 'ginel-elisp)

;;; Local Variables:
;;; generated-autoload-file: "ginel.el"
;;; End:

;;; ginel-elisp.el ends here