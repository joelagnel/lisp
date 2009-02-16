;;; which.el --- search program in paths of variable `exec-path'

;; Copyright (c) 1999 Christoph Conrad

;; Maintainer: Christoph Conrad

;; Author: Christoph Conrad <christoph.conrad@gmx.de>
;; Created: 08 December 1999
;; Version: 0.44
;; Keywords: matching

;; $Revision: 1.2 $

;; This file is not yet part of any Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; `which' is an interactive function which takes a program name (string)
;; as argument and searches all executables with this name in all paths of
;; the variable `exec-path'. Almost every Unix system (especially our
;; beloved GNU/Linux) has this command 'on board', some operating systems
;; don't have. On operating systems like Windows NT where there are
;; several meaningful extensions (e.g. "com", "exe") which can be a suffix
;; of an executable you can define a variable whose name is build from the
;; concatenation of "which-system-", the variable `system-type' and
;; "extensions". It is a list of extensions (strings) to append to the
;; program name given to the function `which'. If the environment variable
;; "PATHEXT" is available it is used instead!

;; `which-elisp' is an interactive function which takes a file name
;; (string) as argument and searches the files with this name and the
;; extension ".el" in all paths of the variable `load-path'.

;; If `which' or `which-elisp' are called interactively, they display the
;; found paths in the modeline. If called non-interactively, they return
;; the list of found paths.

;; The code does something usable, though not perfect.  Improvement
;; suggestions from Emacs experts are welcome.

;;; Credits:

;; People which made contributions or suggestions. Ordered by surname.

;; - David Biesack <sasdjb@unx.sas.com>
;; - Peter Breton <pbreton@bluewin.ch> (send me his own `which' code)
;; - Mickey Ferguson <MFerguson@peinc.com> (`file-executable-p' on Windows
;;   NT is curious)
;; - Kai Groﬂjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>. He published
;;   under the subject "Re: where-is" a snippet of code with the basis
;;   functionality.
;; - Bruce Ingalls <bingalls@panix.com>
;; - Jonas Steverud <d4jonas@dtek.chalmers.se>

;;; Installation:

;; e.g. in file .emacs: (require 'which)
;; Put a copy of which.el/.elc into some path of `load-path'. To show
;; `load-path': "C-h v" load-path RET

;;; ToDo:

;; - `file-name-handler-alist', some paths seem to be preceeded
;; - showing symbolic/hard links and doubles
;; - better documentation in header/code
;; - customizable which predicate to test file, e.g. `file-executable-p'
;;   or `file-exists-p' or user defined function
;; - From: Noah Friedman <friedman@splode.com>:
;;   http://www.splode.com/~friedman/software/emacs-lisp/, in particular,
;;   see fff.el and file-fns.el
;; - Better customization for extension variable
;; - `with-output-to-temp-buffer', (suggestion by ??)

;;; Change log: see at end of file. Search for "Change log".


;;; Code:

;; variables

(defconst which-cvs-revision "$Revision: 1.2 $")

(defgroup which nil
  "Find files in paths."
  :group 'matching)

(defcustom which-system-windows-nt-extensions
  (if (getenv "PATHEXT")
      (split-string (getenv "PATHEXT") ";")
    '(".com" ".exe" ".bat" ".cmd"))
  "*Extensions for executables on Windows NT (function `which')."
  :type '(repeat string)
  :group 'which)

(defcustom which-result-separator
  path-separator
  "*Separator between paths in result string."
  :type '(string)
  :group 'which)

;; functions

(defun which-out( path-list interactive )
  "Process all paths found (PATH-LIST).
If INTERACTIVE is true, a string is displayed in the modeline, where all
paths from the PATH-LIST are concatenated with which-result-separator.
Otherwise the PATH-LIST is returned."
  (if (not interactive)
      path-list
    (let ((paths (mapconcat 'identity
                            path-list
                            which-result-separator)))
      (message (if (not (equal paths ""))
                   "Paths: %s"
                 "No File found.")
               paths))))

(defun which-intern( filename search-paths extensions test-func-p )
  "Search files in SEARCH-PATHS with EXTENSIONS and test them with TEST-FUNC-P."
  (let* (
         ;; list of file names with extensions
         (progs
          (mapcar #'(lambda( suffix )
                      (concat filename suffix ))
                  extensions))

         ;; list of lists which contains the result of the executable
         ;; testing.
         (found-path-list
          (mapcar
           #'(lambda( path )
               (mapcar
                ;; test if a file is executable
                #'(lambda( progname )
                    (let ((filename (expand-file-name progname path)))
                      (when (funcall test-func-p filename)
                        filename)))
                progs))
           search-paths)))

    (delete nil (apply 'append found-path-list))))



(defun which( programname )
  "Search executable PROGRAMNAME in `exec-path'."
  (interactive
   (list (read-from-minibuffer (if (eq system-type 'windows-nt)
                                   "Program/batch name without extension: "
                                 "Program name: "))))
  (let ((extensions
         (or (symbol-value (intern-soft
                            (concat "which-system-"
                                    (format "%s" system-type)
                                    "-extensions")))
             '(""))))

    ;; debugging code
    ;; (extensions '(".com" ".exe" ".bat" ".cmd"))

    (which-out
     (which-intern programname exec-path extensions 'file-executable-p)
     (interactive-p))))


(defun which-elisp( filename )
  "Search lisp file FILENAME in `load-path'."
  (interactive "sFilename (without .el): ")
  (let ((extensions '(".el")))
    (which-out (which-intern filename load-path extensions 'file-exists-p)
               (interactive-p))))

(provide 'which)


;; Time-stamp: <08.07.2000 18:45:11>

;; $Log: which.el,v $
;; Revision 1.2  2000/07/10 20:20:08  cc
;; UPD: Version: 0.44
;; FIX: Mailadress corrected to bingalls@panix.com
;; UPD: Change log moved to end of file
;; UPD: `which-cvs-revision' defvar -> defconst
;; UPD: Explicit loading of dos-w32 deleted
;;
;; Revision 1.1.1.1  2000/07/03 05:13:25  cc
;; Initial entry
;;

;; 02 Jul 2000 - v0.43:
;; - if called non-interactively, `which'/`which-elisp' don't display a
;;   string in the modeline, they just return the list of paths.
;; - if system-type is 'windows-nt or 'ms-dos, dos-w32 is `required' to be
;;   sure `path-separator' is defined on these systems.
;; - documentation update

;; 15 Jun 2000 - v0.42:
;; - changed email address to "christoph.conrad@gmx.de"

;; 15 Jun 2000 - v0.41:
;; - `which-result-separator' defaults to `path-separator'
;; - some `defcustom' keyword changes

;; 24 Dec 1999 - v0.4:
;; - copyright notice, minor change (no file COPYING)

;; 24 Dec 1999 - v0.3:
;; - `checkdoc'-ed, `lm-verify'-ed (lisp-mnt.el), `elint'-ed
;; - simple interface function `which-intern' for user defined functions
;;   with other searchpaths, predefined function `which-elisp' (David
;;   Biesack 13 Dec 1999)
;; - use PATHEXT under Windows NT if available (David Biesack 13 Dec 1999)
;; - do all assignments in let* (Jonas Steverud 11 Dec 1999)
;; - changed order of extensions for Windows NT when there is no
;;   PATHEXT-environment variable to the NT default value
;; - better documentation in header/code
;; - code reorganization
;; - Variable `which-separator' renamed to  `which-result-separator'
;; - Variable `which-<system-type>' renamed to
;;   `which-system-<system-type>-extensions'
;; - Fixed some bugs and typos.
;;
;; 11 Dec 1999 - v0.2:
;; - First public released version as package.
;;
;; 08 Dec 1999 - v0.1:
;; - First public released version, one function.

;;; which.el ends here
