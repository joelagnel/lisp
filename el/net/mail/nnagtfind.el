;;; Saved through ges-version 0.3.3dev at 2004-05-14 12:06
;;; ;;; From: Michael Schierl <schierlm-usenet@gmx.de>
;;; ;;; Subject: nnagtfind 0.1 -- Find articles by message ID from the Gnus agent
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Followup-To: gnu.emacs.gnus
;;; ;;; Date: Tue, 13 Apr 2004 22:56:22 +0200
;;; ;;; Reply-To: schierlm@gmx.de

;;; [1. text/plain]

;;; (Automatically generated with ges-post.el, version 0.6)

;;; Feedback appreciated.

;;; [2. application/emacs-lisp; nnagtfind.el]

;;; nnagtfind.el --- Find articles by message ID from the Gnus agent

;; Copyright (C) 2004 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 14 March 2004
;; Keywords: gnus, agent, message-id, unplugged
;; Version: 0.1

(defconst nnagtfind-version "0.1"
  "Version of nnagtfind.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; When you are reading news offline with Gnus agent (e.g. in a
;; train...) you might find someone refering to a Message-ID in a post
;; which you thing you should have inside your Agent.  However, Gnus
;; does not provide any (simple) way to search the complete Gnus Agent
;; for a message (CMIIW).

;;; Installation:

;; (Auto)load this file and add (nnagtfind) somewhere into your
;; `gnus-refer-article-method' variable. This will search all
;; newsgroups there iff Gnus is unplugged. (Since nntp Message-ID
;; searches will be quicker, nnagtfind will only search the Agent when
;; really necessary - so when Gnus is unplugged.)

;;; Bugs:

;; Creating the list of all agentized groups is quite slow.

;;; History:

;; 0.1   initial version

;;; Code:

(require 'gnus)

(defun nnagtfind-search-for-article (id to-buffer)
  "Search for Article ID (internal use).
Argument TO-BUFFER will be passed to `nnagent-request-article'."
  (if gnus-plugged
      nil ;; online methods will be faster
    (let (result)
      (mapc
       (lambda (x)
	 (unless result
	   (setq result
		 (nnagtfind-search-for-article-0
		  id to-buffer (intern (car x)) (nth 1 x) (nth 2 x)))))
       (nnagtfind-find-all))
      (message "Searching agent groups...done")
    result)))

(defun nnagtfind-search-for-article-0 (id to-buffer method server group)
  "Internal function.
ID is the Message-ID, SERVER, GROUP and TO-BUFFER will be passed
to `nnagent-request-article', METHOD is the select method (or
something like that, this is based on trial'n'error...)."
  (message "Searching in %S:%S (%S)" method server group)
  (let ((gnus-command-method (cons method server)))
    (nnagent-request-article
     id group
     server to-buffer)))


(defun nnagtfind-find-all-servers ()
  "Find all agentized servers."
  (apply 'append
	 (mapcar
	  (lambda (method)
	    (mapcar
	     (lambda (server)
	       (cons method server))
	     (nnagtfind-find-subdirs method)))
	  (nnagtfind-find-subdirs "" '("lib")))))

(defun nnagtfind-find-all-groups (server)
  "Find all agentized groups on SERVER."
  (mapcar
   (lambda (g)
     (list (car server) (cdr server) g))
   (nnagtfind-find-all-groups-0
    (expand-file-name (cdr server)
		      (expand-file-name (car server)
					gnus-agent-directory)) "")))

(defun nnagtfind-find-all-groups-0 (dir prefix)
  "Internal method.
DIR is the dir to search in (recursively), PREFIX is the part of
the group name already known."
  (apply 'append
	 (if (file-exists-p (expand-file-name ".overview" dir))
	     (list prefix)
	   nil)
	 (mapcar
	  (lambda (d)
	    (nnagtfind-find-all-groups-0 (expand-file-name d dir)
				       (concat prefix (if (string= prefix "")
							  ""
							".")
					       d)))
	  (nnagtfind-find-subdirs dir))))

(defvar nnagtfind-all-group-cache nil
  "Used to cache the list of all agentized groups.")

(defun nnagtfind-find-all ()
  "Find all agentized groups on all servers.
If the value is already cached, it is used instead of
recalculating it."
  (unless nnagtfind-all-group-cache
    (setq nnagtfind-all-group-cache
	  (apply 'append
		 (mapcar 'nnagtfind-find-all-groups
			 (nnagtfind-find-all-servers)))))
  nnagtfind-all-group-cache)

(defun nnagtfind-find-subdirs (path &optional except)
  "Find all subdirs of PATH except EXCEPT."
  (let* ((fpath (expand-file-name path gnus-agent-directory))
	 (fls (directory-files fpath)))
    (delete nil
	    (mapcar (lambda (x)
		      (cond
		       ((not (file-directory-p (expand-file-name x fpath)))
			nil)
		       ((string= "." (substring x 0 1))
			nil)
		       ((member x except)
			nil)
		       (t
			x)))
		    fls))))

;;; backend stuff:

(gnus-declare-backend "nnagtfind" 'none)

(defun nnagtfind-request-article (article &optional group server to-buffer)
  (nnagtfind-search-for-article article to-buffer))

(defun nnagtfind-retrieve-headers (articles &optional group server fetch-old)
  (error "Not implemented"))

(defun nnagtfind-status-message (&optional server)
  "")


(defun nnagtfind-ok (&rest args)
  t)

(defun nnagtfind-undefined (&rest args)
  (error "Not implemented"))

(defalias 'nnagtfind-open-server   'nnagtfind-ok)
(defalias 'nnagtfind-close-server  'nnagtfind-ok)
(defalias 'nnagtfind-request-close 'nnagtfind-ok)
(defalias 'nnagtfind-server-opened 'nnagtfind-ok)
(defalias 'nnagtfind-request-group 'nnagtfind-undefined)
(defalias 'nnagtfind-close-group   'nnagtfind-undefined)
(defalias 'nnagtfind-request-list  'nnagtfind-undefined)
(defalias 'nnagtfind-request-post  'nnagtfind-undefined)

(provide 'nnagtfind)

(provide 'nnagtfind)

;;; nnagtfind.el ends here

