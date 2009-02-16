;;;; majordomo.el
;;; Time-stamp: <2005-01-05 09:36:22 john>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; By John Sturdy <john@cb1.com>

(provide 'majordomo)
(require 'cl)
(require 'backquote)

;; This package is meant for people who manage several majordomo lists.
;; It composes and sends majordomo commands, parses the replies, and
;; maintains a cache of the membership of each list.

;;;; Handle a directory of majordomo data

(defvar majordomo-directory "~/lists"
  "The directory containing majordomo-related data;
the main files in it are lists of members, in the format returned by the
majordomo \"who\" command. The name of each file is the whole name of the
list, eg samplelist@sampleserver.org")

(defun majordomo-file (list-name file-kind)
  "Return the filename for a given LIST-NAME and FILE-KIND."
  (let ((directory (expand-file-name list-name
				     majordomo-directory)))
    (unless (and (file-exists-p directory)
		 (file-directory-p directory))
      (make-directory directory t))
    (expand-file-name (symbol-name file-kind) directory)))

;;;; Basic sending of commands

(defvar majordomo-send-carefully t
  "*Whether to check with the user before sending a majordomo command message.")

(defvar majordomo-message-initialized nil
  "Whether we currently have a message header set up.
Actually holds the buffer in which it is set up.")

(defvar majordomo-sending-suspended nil
  "Whether we are holding the current message to have more added to it.")

(defun majordomo-send (server &rest strings)
  "Send to majordomo at SERVER a message made from the remaining args."
  (save-window-excursion
    (if majordomo-message-initialized
	(set-buffer majordomo-message-initialized)
      (compose-mail (concat "majordomo@" server)
		    (concat "message to "
			    (if (string-match "approve" (car strings))
				(cadr strings)
			      (car strings))
			    "..."))
      (setq majordomo-message-initialized (current-buffer)))
    (goto-char (point-max))
    (mapcar 'insert strings)
    (insert "\n")
    (unless majordomo-sending-suspended
      (when (or (not majordomo-send-carefully)
		(save-window-excursion
		  (display-buffer (current-buffer))
		  (yes-or-no-p "Send this majordomo message? ")))
	(mail-send-and-exit nil))
      (setq majordomo-message-initialized nil))))

(defmacro majordomo-in-one-message (&rest forms)
  "Evaluate body FORMS but any majordomo message they construct
are bundled up into one message."
  `(progn
     (setq majordomo-message-initialized nil)
     (let ((majordomo-sending-suspended t))
       ,@(butlast forms))
     ,@(last forms)))

(defmacro majordomo-outgoing-command (&rest forms)
  "Execute &body FORMS in a context set up for sending a majordomo message."
  `(progn
     (setq majordomo-message-initialized nil)
     ,@forms))

(defun majordomo-send-who (server listname)
  "Send a message to SERVER to ask for the subscribers to LISTNAME."
  (interactive "sServer: 
sList: ")
  (majordomo-send server (majordomo-op server listname 'who)))

(defun majordomo-send-getconfig (server listname)
  "Get the config file from SERVER for LISTNAME"
  (interactive "sServer: 
sList: ")
  (majordomo-send server (majordomo-op server listname 'config)))

(defun majordomo-send-getinfo (server listname)
  "Get the info file from SERVER for LISTNAME"
  (interactive "sServer: 
sList: ")
  (majordomo-send server (majordomo-op server listname 'info)))

(defun majordomo-send-getintro (server listname)
  "Get the intro file from SERVER for LISTNAME"
  (interactive "sServer: 
sList: ")
  (majordomo-send server (majordomo-op server listname 'intro)))

(defun majordomo-order-update (server listname)
  "Order an update from SERVER about LISTNAME.
This gets members, config, info and intro."
  (interactive "sServer name: 
sList name: ")
  (majordomo-in-one-message
   (majordomo-send-who server listname)
   (majordomo-send-getconfig server listname)
   (majordomo-send-getinfo server listname)
   (majordomo-send-getintro server listname)))

;;;; Server and list names

(defun majordomo-server (combined)
  "Return the server part of COMBINED."
  (let ((match (string-match "@" combined)))
    (if match
	(substring combined (1+ match))
      nil)))

(defun majordomo-list (combined)
  "Return the list part of COMBINED."
  (let ((match (string-match "@" combined)))
    (if match
	(substring combined 0 match)
      nil)))


(defun majordomo-lists@servers ()
  "Return the list of known list@server combinations."
  (directory-files majordomo-directory
		   nil
		   "^[^.]"))

(defun majordomo-prompt-for-list (prompt)
  "Prompt the user for a list name."
  (completing-read prompt
		   (mapcar 'list
			   (majordomo-lists@servers))))

(defun majordomo-servers ()
  "Return a list of the servers listed in majordomo-directory."
  (mapcar 'majordomo-server (majordomo-lists@servers)))

(defun majordomo-lists ()
  "Return a list of the lists listed in majordomo-directory."
  (mapcar 'majordomo-list (majordomo-lists@servers)))

(defvar majordomo-servers-history nil
  "A history list for reading server names.")

(defun majordomo-prompt-for-server (prompt)
  "Prompt for a server name, rendering what help we can."
  (completing-read prompt
		   (mapcar 'list (majordomo-servers))
		   nil
		   nil
		   nil
		   'majordomo-servers-history))

(defvar majordomo-lists-history nil
  "A history list for reading list names.")

(defun majordomo-prompt-for-list (prompt)
  "Prompt for a list name, rendering what help we can."
  (completing-read prompt
		   (mapcar 'list (majordomo-lists@servers))
		   nil
		   nil
		   nil
		   'majordomo-lists-history))

(defun majordomo-regexp-matching-lists ()
  "Return a regexp matching the name of any list that you manage.
This is used for searching messages for mentions of any such list."
  (concat "\\("
	  (mapconcat (function
		      (lambda (wholename)
			(let ((at (string-match "@" wholename)))
			  (if at
			      (substring wholename 0 at)
			    wholename))))
		     (majordomo-lists@servers)
		     ;; "\\)\\|\\("
		     "\\|"
		     )
	  "\\)"))

(defun majordomo-full-name-of-list (partname)
  "Given part of a list name, return the full name."
  (let ((lists (directory-files majordomo-directory nil partname t)))
    (if lists
	(car lists)
      nil)))

;;;; Parse replies from majordomo, storing results in our majordomo directory

;; todo: replace these by a single parser that gets multiple things from a message and stores them intelligently (including allowing for commands for several lists in one message)

(defun majordomo-receive-who ()
  "Parse a message for majordomo \"who\" results, saving the data in the
file for the list concerned -- see documentation for majordomo-directory."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^From: majordomo@\\([-_.a-zA-Z0-9]+\\)" (point-max) t)
	(let ((server-name (buffer-substring (match-beginning 1) (match-end 1))))
	  (while (re-search-forward "^Members of list '\\(.+\\)':" (point-max) t)
	    (let* ((list-name (buffer-substring (match-beginning 1) (match-end 1)))
		   (full-name (format "%s@%s" list-name server-name)))
	      (beginning-of-line 3)
	      (let ((start (point)))
		(re-search-forward "^[0-9]+ subscribers")
		(end-of-line 0)
		(write-region start (point)
			      (majordomo-file full-name 'members)))))))))

(defun majordomo-receive-getconfig ()
  "Try to parse a config result from a message."
  (interactive)
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward "^From: majordomo@\\([-_.a-zA-Z0-9]+\\)" (point-max) t)
	  (let ((server-name (buffer-substring (match-beginning 1) (match-end 1))))
	    (when (re-search-forward "^>+ config \\([^ ]+\\) .+" (point-max) t)
	      (let* ((list-name (buffer-substring (match-beginning 1) (match-end 1)))
		     (full-name (format "%s@%s" list-name server-name)))
		(beginning-of-line 2)
		(write-region (point) (point-max)
			      (majordomo-file full-name 'config)))))))))

;; also do info and intro

;;;; Member (people) names

(defun majordomo-members (listname)
  "Make an alist of name+email to email for the given list."
  (let ((filename (majordomo-file listname 'members)))
    (if (file-readable-p filename)
	(save-window-excursion
	  (find-file filename)
	  (save-window-excursion
	    (goto-char (point-min))
	    (let ((members nil))
	      (while (not (eobp))
		(cond
		 ((looking-at "^[^ \n]+@[^ \n]+$")
		  (push
		   (cons (match-string 0)
			 (match-string 0))
		   members))
		 ((looking-at "^\"\\([^<]+\\)\" <\\(.+\\)>$")
		  (push
		   (cons (concat (match-string 1) " " (match-string 2))
			 (match-string 2))
		   members))
		 ((looking-at "^\"?[^<]*\"? ?<\\(.+\\)>$")
		  (push
		   (cons (match-string 0)
			 (match-string 1))
		   members)))
		(beginning-of-line 2))
	      (nreverse members))))
      nil)))

(defun majordomo-email-regexp-pair (email-pair)
  "Make a regexp pair for an email pair."
  (let* ((email (cdr email-pair))
	 (match (string-match "@" email)))
    (if match
	(cons (concat
	       (substring email 0 (1+ match))
	       "[^ ,]*"
	       (substring email (1+ match)))
	      email)
      nil)))

(defun majordomo-member-email-regexps (listname)
  "Make for LISTNAME an alist of regexps to email addresses.
The regexp in each entry matches addresses that majordomo should
consider equivalent to the given address."
  (mapcar 'majordomo-email-regexp-pair (majordomo-members listname)))

(defun majordomo-subscribed-form-of-address (address listname)
  "Return the subscribed form of ADDRESS used on LISTNAME."
  (let ((regexp-pairs (majordomo-member-email-regexps listname)))
    (catch 'matched
      (dolist (pair regexp-pairs)
	(when (string-match (car pair) address)
	  (throw 'matched (cdr pair))))
      nil)))

(defun majordomo-prompt-for-member (prompt listname)
  "Prompt for a member of the given list."
  (let ((completion-ignore-case t)
	(members (majordomo-members listname)))
    (if members
	(completing-read prompt members)
      (read-from-minibuffer prompt))))

;;;; Data-mining messages from people

(defun majordomo-find-list-mentioned ()
  "Return the name of the first of yours lists mentioned in this buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (majordomo-regexp-matching-lists) (point-max) t)
       (majordomo-full-name-of-list (match-string 0))
      nil)))

(defun majordomo-find-member-sending (listname &optional allow-non-members)
  "Look in this message for which member of LISTNAME sent the message.
Unless optional ALLOW-NON-MEMBERS is given, they must be a member.
Returns a their email address in the form in which they are subscribed
to LISTNAME (unless in the case of ALLOW-NON-MEMBERS they are not a member,
in which case it is given as found)."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^From: \\(.+\\)$" (point-max) t)
	(let* ((fullname (match-string 1))
	       (email (if (string-match "<\\(.+\\)>" fullname)
			  (substring fullname (match-beginning 1) (match-end 1))
			fullname)))
	  (let ((subbed-form (majordomo-subscribed-form-of-address email listname)))
	    (if subbed-form
		subbed-form
	      (if allow-non-members
		  email
		nil)))))))

(defun majordomo-find-realname-sending ()
  "Find the real name of the person sending this message."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^From: \\(.+\\)$" (point-max) t)
	(let* ((fullname (match-string 1))
	       (realname (if (string-match "\\(.+\\) <\\(.+\\)>" fullname)
			  (substring fullname (match-beginning 1) (match-end 1))
			fullname)))
	  (when (string-match "[\" ]\\(.+\\)[\" ]" realname)
	    (setq realname (substring realname (match-beginning 1) (match-end 1))))
	  (setq realname (read-from-minibuffer "Edit real name: " realname))
	  realname)
      (read-from-minibuffer "Real name: "))))

;;;; Handle majordomo commands, adding approval if necessary

(defvar majordomo-session-passwords nil
  "Known passwords")

(defun majordomo-password (server listname)
  "Get the password for SERVER LISTNAME, with caching."
  (let* ((key (concat listname "@" server))
	 (value (cdr (assoc key majordomo-session-passwords))))
    (when (null value)
      (setq value (read-from-minibuffer
		   (format "Password for %s: " key))
	    majordomo-session-passwords
	    (cons (cons key value) majordomo-session-passwords)))
    value))

(defun majordomo-op (server listname op)
  "Return an op string with password if needed"
  (cond
   ((memq op '(subscribe unsubscribe))
    (format "approve %s %s %s " (majordomo-password server listname) op listname))
   ((memq op '(config newconfig))
    (format "%s %s %s" op listname (majordomo-password server listname)))
   ((memq op '(info intro who))
    (format "%s %s" op listname))
   (t (symbol-name op))))

;;;; Subscription commands
  
(defun majordomo-subscribe (server listname name address)
  "Subscribe"
  (interactive
   (let* ((server (majordomo-prompt-for-server "Server: "))
	  (list (majordomo-prompt-for-list "List: " server))
	  (name (read-from-minibuffer "Name: "))
	  (address (read-from-minibuffer "Address: ")))
     (list server list name address)))
  (majordomo-outgoing-command
   (majordomo-send server
		   (majordomo-op server listname 'subscribe)
		   name " <" address ">")))

(defun majordomo-unsubscribe (server listname address)
  "Unsubscribe"
  (interactive
   (let* ((full-listname (majordomo-prompt-for-list "List to unsubscribe someone from: "))
	  (person (majordomo-prompt-for-member "Unsubscribe person: " full-listname)))
     (list (majordomo-server full-listname) (majordomo-list full-listname) person)))
  (majordomo-outgoing-command
   (majordomo-send server
		   (majordomo-op server listname 'unsubscribe)
		   listname address)))

(defun majordomo-mail-unsubscribe()
  "Look in the message you are reading and try to construct an unsubscribe command from it."
  (interactive)
  (let* ((listname (majordomo-find-list-mentioned))
	 (person (majordomo-find-member-sending listname)))
    (when (yes-or-no-p (format "Unsubscribe %s from %s? " person listname))
      (majordomo-unsubscribe (majordomo-server listname)
			     (majordomo-list listname)
			     person))))

(defun majordomo-mail-subscribe()
  "Look in the message you are reading and try to construct a subscribe command from it."
  (interactive)
  (let* ((listname (majordomo-find-list-mentioned))
	 (address (majordomo-find-member-sending listname t))
	 (person (majordomo-find-realname-sending)))
    (when (yes-or-no-p (format "Subscribe %s <%s> to %s? " person address listname))
      (majordomo-subscribe (majordomo-server listname)
			     (majordomo-list listname)
			     person
			     address))))

(defun majordomo-bbdb-subscribe (server list)
  "Subscribe the person whose entry is being shown, to a majordomo list."
  (interactive
   (let* ((server (majordomo-prompt-for-server "Server: "))
	  (list (majordomo-prompt-for-list "List: " server)))
     (list server list)))
  (let* ((record (bbdb-current-record t))
	 (email (car (bbdb-record-net record)))
	 (override (bbdb-record-getprop record 'mail-name))
	 (name (or override (bbdb-record-name record))))
    (majordomo-subscribe server list name email)))

(defun majordomo-bbdb-unsubscribe (server list)
  "Unsubscribe the person whose entry is being shown, from a majordomo list."
  (interactive
   ;; should be able to make this depend intelligently on which lists they're on
   (let* ((server (majordomo-prompt-for-server "Server: "))
	  (list (majordomo-prompt-for-list "List: " server)))
     (list server list)))
  (let* ((record (bbdb-current-record t))
	 (email (car (bbdb-record-net record))))
    (majordomo-unsubscribe server list email)))

;;;; Menu entries

(define-key menu-bar-tools-menu [majordomo]
  (cons "Majordomo" (make-sparse-keymap "Majordomo")))

(define-key menu-bar-tools-menu [majordomo majordomo-subscribe]
  '("Subscribe" . majordomo-subscribe))

(define-key menu-bar-tools-menu [majordomo majordomo-unsubscribe]
  '("Unsubscribe" . majordomo-unsubscribe))

(define-key menu-bar-tools-menu [majordomo majordomo-order-update]
  '("Fetch majordomo data" . majordomo-order-update))

(define-key menu-bar-tools-menu [majordomo majordomo-mail-subscribe]
  '("Subscribe Sender" . majordomo-mail-subscribe))

(define-key menu-bar-tools-menu [majordomo majordomo-mail-unsubscribe]
  '("Unsubscribe Sender" . majordomo-mail-unsubscribe))

;;; end of majordomo.el
