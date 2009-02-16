;;; Saved through ges-version 0.3.3dev at 2003-04-16 17:53
;;; ;;; From: Phillip Lord <p.lord@russet.org.uk>
;;; ;;; Subject: sa-learn:- spamassassin bayesian interface
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: 16 Apr 2003 16:15:56 +0100
;;; ;;; Organization: Dept of Computer Science, University of Manchester, U.K.



;;; I proudly present a somewhat unfinished, and poorly documented
;;; interface between RMAIL, Gnus, and sa-learn, the front end to
;;; spamassassin's bayessian learner. 


;;; Cheers

;;; Phil



;;; ;; This file is not part of Emacs

;;; ;; Author: Phillip Lord <p.lord@russet.org.uk>
;;; ;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;;; ;; Keywords: shell, working directory
;;; ;; Latest Version: http://www.russet.org.uk/emacs

;;; ;; COPYRIGHT NOTICE
;;; ;;
;;; ;; This program is free software; you can redistribute it and/or modify
;;; ;; it under the terms of the GNU General Public License as published by
;;; ;; the Free Software Foundation; either version 2, or (at your option)
;;; ;; any later version.

;;; ;; This program is distributed in the hope that it will be useful,
;;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; ;; GNU General Public License for more details.

;;; ;; You should have received a copy of the GNU General Public License
;;; ;; along with this program; see the file COPYING.  If not, write to the
;;; ;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; ;; Boston, MA 02111-1307, USA.

;;; Commentary
;; 
;; Provides an interface to the "sa-learn" command from spamassassin. 



(defvar sa-learn-invoke-spam "sa-learn --no-rebuild --single --spam")
(defvar sa-learn-invoke-ham "sa-learn --no-rebuild --single --ham")
(defvar sa-learn-invoke-forget "sa-learn --no-rebuild --single --forget")
(defvar sa-learn-invoke-rebuild "sa-learn --rebuild")

(defvar sa-learn-invoke-rebuild-threshold 20)
(defvar sa-learn-invoke-messages-since-rebuild 0)

(defun sa-learn-invoke-spam()
  (interactive)
  (unless (sa-learn-invoke-check-learnt)
    (rmail-set-attribute "spam" t)
    (sa-learn-invoke sa-learn-invoke-spam)))

(defun sa-learn-invoke-ham()
  (interactive)
  (unless (sa-learn-invoke-check-learnt)
    (rmail-set-attribute "ham" t)
    (sa-learn-invoke sa-learn-invoke-ham)
    (rmail-undelete-previous-message)))

(defun sa-learn-invoke-forget()
  (interactive)
  (save-excursion
    (set-buffer rmail-buffer)
    (if (rmail-message-labels-p rmail-current-message "ham")
        (rmail-set-attribute "ham" nil))
    (if (rmail-message-labels-p rmail-current-message "spam")
        (rmail-set-attribute "spam" nil)))
  (message "Forgetting message...")
  (sa-learn-invoke sa-learn-invoke-forget))

(defun sa-learn-invoke-check-learnt()
  (save-excursion
    (set-buffer rmail-buffer)
    (or
     (if (rmail-message-labels-p rmail-current-message "spam")
         (progn (message "Message already learnt as spam")
                t))
     (if (rmail-message-labels-p rmail-current-message "ham")
         (progn (message "Message already learnt as ham")
                t)))))
  
  

(defun sa-learn-invoke(command)
  (let ((file (make-temp-file "sa-learn")))
    (message "Learning message...")
    (sa-learn-invoke-write-message file)
    (shell-command (format "%s < %s" command file))
    (sa-learn-invoke-rebuild-maybe)))

(defun sa-learn-invoke-rebuild-maybe()
  (if (> sa-learn-invoke-messages-since-rebuild
         sa-learn-invoke-rebuild-threshold)
      (progn
        (sa-learn-invoke-rebuild)
        (setq sa-learn-invoke-messages-since-rebuild 0)))
  (incf sa-learn-invoke-messages-since-rebuild))

(defun sa-learn-invoke-rebuild()
  (interactive)
  (message "Rebuilding database...")
  (shell-command sa-learn-invoke-rebuild)
  (message "Rebuilding database...done"))

(defun sa-learn-invoke-write-message(file)
  (interactive)
  (rmail-output file 1 t))

(eval-after-load "rmail"
  '(progn
     (define-key rmail-summary-mode-map "\C-cs" 'sa-learn-invoke-spam)
     (define-key rmail-mode-map "\C-cs" 'sa-learn-invoke-spam)
     (define-key rmail-summary-mode-map "\C-ch" 'sa-learn-invoke-ham)
     (define-key rmail-mode-map "\C-ch" 'sa-learn-invoke-ham)))


(defun sa-learn-invoke-gnus-spam()
  (interactive)
  (gnus-summary-save-in-pipe sa-learn-invoke-spam)
  (sa-learn-invoke-rebuild-maybe))

(defun sa-learn-invoke-gnus-ham()
  (interactive)
  (gnus-summary-save-in-pipe sa-learn-invoke-ham)
  (sa-learn-invoke-rebuild-maybe))

(defun sa-learn-invoke-gnus-forget()
  (interactive)
  (gnus-summary-save-in-pipe sa-learn-invoke-forget)
  (sa-learn-invoke-rebuild-maybe))

(eval-after-load "gnus"
  '(progn
     (define-key gnus-summary-mode-map "\C-cs" 'sa-learn-invoke-gnus-spam)
     (define-key gnus-article-mode-map "\C-cs" 'sa-learn-invoke-gnus-spam)
     (define-key gnus-summary-mode-map "\C-ch" 'sa-learn-invoke-gnus-ham)
     (define-key gnus-article-mode-map "\C-ch" 'sa-learn-invoke-gnus-ham)))

(provide 'sa-learn-invoke)



