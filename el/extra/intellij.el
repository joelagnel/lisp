;;; Saved through ges-version 0.3.3dev at 2003-09-21 13:27
;;; ;;; ;;; ;;; From: sandipchitale@yahoo.com (Sandip Chitale)
;;; ;;; ;;; ;;; Subject: Two way integration between emacs and IntelliJ IDEA
;;; ;;; ;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; ;;; ;;; Date: 3 Sep 2003 14:08:21 -0700
;;; ;;; ;;; ;;; Organization: http://groups.google.com/

;;; ;;; ;;; Two way integration (opening files) between emacs and IntelliJ

;;; ;;; ;;; To open file of current emacs buffer in IntelliJ use the following:

;;; ;;; ;;; intellij.el --- edit current file in intellij

;;; ;;; ;; Author: Sandip V. Chitale
;;; ;;; ;; Created: Sep 03 2003
;;; ;;; ;; Keywords: open file intellij

;;; ;;; ;; This file is not part of GNU Emacs yet.

;;; ;;; Commentary:
;;; ;;
;;; ;; This package enables opening file in IntelliJ from emacs buffer.
;;; ;; 
;;; ;; Background:
;;; ;; IntellJ (http://www.jetbrains.com) has an easy mechanism to edit
;;; ;; files in external programs. For example, defining an external tool
;;; ;; (using Options:External Tools) with:
;;; ;;
;;; ;; Program: C:\emacs\bin\gnuclientw.exe
;;; ;; Parameters: +$LineNumber$  "$FilePath$"
;;; ;;
;;; ;; However there is no easy way to open a file in IntelliJ from emacs.
;;; ;;
;;; ;; Alexey Efimov has written an IntelliJ plugin called FileAssociation
;;; ;; The FileAssociations plugin provide special file server.
;;; ;; It load files into IntelliJ from outside, using a TCP/IP socket
;;; based
;;; ;; server. Get it from:
;;; ;; http://www.intellij.org/twiki/bin/view/Main/FileAssociations
;;; ;; 
;;; ;; To install and use, put this file on your Emacs-Lisp load path and
;;; add the
;;; ;; following into your ~/.emacs startup file:
;;; ;;
;;; ;;  (require 'intellij)
;;; ;;
;;; ;; You may set the following key binding: ;; ;; (global-set-key
;;; [(control e)] 'open-buffer-file-in-intellij)
;;; ;;
;;; ;; You may have to customize the `intellij-install-dir' variable to
;;; specify IntelliJ installation
;;; ;; directory.
;;; ;;
;;; Code:
(defgroup IntelliJ nil
  "Integration with IntelliJ"
  :group 'tools
  :prefix "intellij-")

(defcustom intellij-install-dir "c:\\IntelliJ-IDEA-3.0.4"
  "IntelliJ installation directory"
  :type 'directory
  :group 'IntelliJ
  )

(defun current-line ()
"Return current line number. Based on code from `what-line'."
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(forward-line 0)
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(count-lines 1 (point))))))

(defun open-buffer-file-in-intellij ()
  "Open current file in Intellij."
  (interactive)
  (open-file-in-intellij (buffer-file-name) (current-line)))

(defun open-file-in-intellij (file &optional line)
  "Open the specified file in Intellij. If specified go to that line."
  (shell-command (format "%s\\jre\\bin\\javaw -classpath
\"%s\\plugins\\FileAssociations-bin.jar\" loader \"%s\" -l %s"
			 intellij-install-dir
			 intellij-install-dir
			 file
			 (or line
			     1))))
(provide 'intellij)
;;; ;;; end of intellij.el
;;; --------------------
;;; To open current open file of IntelliJ in emacs define an external tool
;;; using Options:External Tools

;;; Program: c:\emacs\bin\gnuclientw.exe
;;; Parameters: +$LineNumber$  &quot;$FilePath$&quot;
;;; Working Directory=C:/emacs/bin

;;; You may set the CTRL-e as a shortcut for this external tool.

;;; Requires gnuserv package.
;;; ---------------------

