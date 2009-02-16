;;; mew-imap.el for reading

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Feb 21, 2002

;;; Code:

(require 'mew)

(defvar mew-imap-msgid-file ".mew-msgid")
(defvar mew-imap-folder-alist-file ".mew-folder-alist")
(defvar mew-imap-folder-alist nil)
(defvar mew-imap-friend-folder-list-file ".mew-friend-folder-list")
(defvar mew-imap-friend-folder-list nil)

(defconst mew-imap-inbox-string "inbox")
(defsubst mew-imap-inbox-str-p (mailbox)
  (mew-case-equal mailbox mew-imap-inbox-string))

(defvar mew-imap-skip-uidl t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; IMAP info
;;;

(defvar mew-imap-info-list
  '("server" "port" "process" "ssh-process" "ssl-process" "status"
    "directive" "bnm" "mdb"
    "rmvs" "kils" "refs" "movs"
    "rtrs" "dels" "uidl" "range"
    "rttl" "rcnt" "dttl" "rgttl" "rgcnt" "dgttl" "dgcnt" "jcnt" "rfl" "hlds"
    "user" "auth" "auth-list" "passwd" "account"
    "size" "truncated" "get-body"
    "flush" "no-msg" "msgdb" "done" "dispatched" "error"
    "delete"
    "case" "mailbox" "msgid" "max" "tag"
    "wrk" "jobs" "namespace" "authl"
    "spam" "spam-field" "spam-word" "new-mailbox"
    "my-prefix" "specials" "bytes"))

(mew-info-defun "mew-imap-" mew-imap-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FSM
;;;

(defvar mew-imap-fsm
  '(("greeting"      ("OK" . "capability"))
    ("capability"    ("OK" . "post-capability"))
    ("auth-cram-md5" ("OK" . "pwd-cram-md5") ("NO" . "wpwd"))
    ("pwd-cram-md5"  ("OK" . "next") ("NO" . "wpwd"))
    ("auth-login"    ("OK" . "user-login") ("NO" . "wpwd"))
    ("user-login"    ("OK" . "pwd-login") ("NO" . "wpwd"))
    ("pwd-login"     ("OK" . "next") ("NO" . "wpwd"))
    ("login"         ("OK" . "next") ("NO" . "wpwd"))
    ("select"        ("OK" . "uid"))
    ("select2"       ("OK" . "search"))
    ("uid"           ("OK" . "umsg"))
    ("copy"          ("OK" . "copy") ("NO \\[TRYCREATE\\]" . "create") ("NO" . "wmbx"))
    ("create"        ("OK" . "copy") ("NO" . "wmbx"))
    ("dels"	     ("OK" . "dels"))
    ("expunge"       ("OK" . "post-expunge"))
    ;;
    ("search"        ("OK" . "post-search"))
    ;;
    ("fetch"         ("OK" . "post-fetch"))
    ;;
    ("namespace"     ("OK" . "post-namespace") ("NO\\|BAD" . "list"))
    ("list"          ("OK" . "post-list"))
    ;;
    ("delete"        ("OK" . "logout") ("NO" . "logout3"))
    ("rename"        ("OK" . "logout") ("NO" . "logout3"))
    ;;
    ("logout"        ("OK" . "noop"))
    ("logout2"       ("OK" . "noop"))
    ("logout3"       ("OK" . "noop"))))

(defsubst mew-imap-fsm-by-status (status)
  (assoc status mew-imap-fsm))

(defsubst mew-imap-fsm-next (status code)
  (cdr (mew-assoc-match2 code (nthcdr 1 (mew-imap-fsm-by-status status)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filters
;;;

(defun mew-imap-secure-p (pnm)
  (or (mew-imap-get-ssh-process pnm) (mew-imap-get-ssl-process pnm)))
 
(defun mew-imap-command-capability (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Auth'ing"
		  nil
		  (mew-imap-secure-p pnm))
  (mew-imap-process-send-string pro pnm "CAPABILITY"))

(defun mew-imap-command-post-capability (pro pnm)
  (let ((case-fold-search t)
	authl)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "AUTH=\\([^ \r\n]+\\)" nil t)
	(setq authl (cons (mew-match-string 1) authl)))
      (goto-char (point-min))
      (mew-imap-set-authl pnm authl)
      (mew-imap-set-status pnm "authentication")
      (mew-imap-command-authentication pro pnm))))

(defun mew-imap-command-authentication (pro pnm)
  (cond
   ((eq (mew-imap-get-auth pnm) t) ;; t means SASL
    (let* ((auth-list (mew-imap-get-auth-list pnm))
	   (authl (mew-imap-get-authl pnm))
	   auth func)
      (if (and authl
	       (setq auth (mew-auth-select2 authl auth-list))
	       (setq func (mew-imap-auth-get-func auth))
	       (fboundp func))
	  (progn
	    (mew-imap-set-auth pnm auth)
	    (funcall func pro pnm))
	(mew-imap-debug "<AUTH>" "No preferred IMAP AUTH.\n")
	(mew-imap-set-status pnm "login")
	(mew-imap-command-login pro pnm))))
   (t
    (mew-imap-set-status pnm "login")
    (mew-imap-command-login pro pnm))))

(defun mew-imap-command-login (pro pnm)
  (let* ((user (mew-imap-get-user pnm))
         (prompt (format "IMAP password (%s): " (mew-imap-get-account pnm)))
         (pass (mew-input-passwd prompt (mew-imap-passtag pnm))))
    (setq user (mew-quote-string user ?\\ '(?\\ ?\")))
    (setq pass (mew-quote-string pass ?\\ '(?\\ ?\")))
    (mew-imap-process-send-string pro pnm "LOGIN \"%s\" \"%s\"" user pass)))

(defun mew-imap-command-wpwd (pro pnm)
  (mew-imap-set-error pnm "IMAP password is wrong!")
  (mew-passwd-set-passwd (mew-imap-passtag pnm) nil)
  (mew-imap-set-status pnm "logout2")
  (mew-imap-command-logout2 pro pnm))

(defun mew-imap-command-next (pro pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(jobs (mew-imap-get-jobs pnm)))
    (when (and (eq directive 'jobs) jobs)
      (let* ((case (mew-imap-get-case pnm))
	     (bnm (mew-imap-get-bnm pnm))
	     (file (mew-expand-folder bnm (car jobs)))
	     (work (concat file mew-queue-work-suffix))
	     (data (concat file mew-imapq-info-suffix))
	     (data-work (concat data mew-queue-work-suffix))
	     info mailbox)
	(rename-file file work 'override)
	(setq info (mew-lisp-load data))
	(rename-file data data-work 'override)
	(setq mailbox (car info))
	(setq mailbox (mew-imap-utf-7-encode-string mailbox))
	(setq mailbox (mew-imap-expand-mailbox case mailbox))
	(mew-imap-set-mailbox pnm mailbox)
	(setq info (cdr info))
	(mew-imap-set-rmvs pnm (nth 0 info))
	(mew-imap-set-kils pnm (nth 1 info))
	(mew-imap-set-refs pnm (nth 2 info))
	(mew-imap-set-movs pnm (nth 3 info))
	(mew-imap-set-rttl pnm (nth 4 info))
	(mew-imap-set-dttl pnm (nth 5 info))
	(mew-imap-set-rcnt pnm 1)
	(mew-imap-set-rgcnt pnm 1)
	(mew-imap-set-dgcnt pnm 1)
	(mew-imap-set-wrk  pnm work)
	(mew-imap-set-jobs pnm (cdr jobs))))
    (mew-imap-set-status pnm "select")
    (mew-imap-command-select pro pnm)))

(defun mew-imap-command-select (pro pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(mailbox (mew-imap-get-mailbox pnm)))
    (cond
     ((eq directive 'list)
      (mew-imap-set-status pnm "namespace")
      (mew-imap-command-namespace pro pnm))
     ((eq directive 'delete)
      (mew-imap-set-status pnm "delete")
      (mew-imap-command-delete pro pnm))
     ((eq directive 'rename)
      (mew-imap-set-status pnm "rename")
      (mew-imap-command-rename pro pnm))
     (t
      (if (mew-imap-get-spam pnm)
	  (mew-imap-set-status pnm "select2"))
      (mew-imap-process-send-string pro pnm "SELECT \"%s\"" mailbox)))))

(defun mew-imap-command-uid (pro pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(refs (mew-imap-get-refs pnm)) ;; (uid siz del (+fld msg))
	(rmvs (mew-imap-get-rmvs pnm))
	(bnm (mew-imap-get-bnm pnm))
	(range (mew-imap-get-range pnm))
	max)
    (if (and mew-imap-skip-uidl
	     (or (eq directive 'exec) (eq directive 'jobs) (eq directive 'get)))
	(mew-imap-command-dispatch pro pnm directive refs rmvs nil)
      (mew-net-status bnm "Checking" nil (mew-imap-secure-p pnm))
      (cond
       ((eq directive 'biff)
	(setq max (mew-lisp-load (mew-expand-folder bnm mew-imap-msgid-file))))
       ((eq directive 'scan)
	(if (eq range nil) ;; update
	    (setq max (mew-lisp-load
		       (mew-expand-folder bnm mew-imap-msgid-file))))))
      (if max
	  (setq max (string-to-int max))
	(setq max 0))
      (mew-imap-set-max pnm max)
      (mew-imap-process-send-string
       pro pnm "UID FETCH %d:* (UID RFC822.SIZE)" (1+ max)))))

(defun mew-imap-command-umsg (pro pnm)
  (let* ((directive (mew-imap-get-directive pnm))
	 (max (mew-imap-get-max pnm))
	 (refs (mew-imap-get-refs pnm)) ;; (uid siz del (+fld msg))
	 (rmvs (mew-imap-get-rmvs pnm))
	 (del-time (mew-imap-get-delete pnm))
	 (range (mew-imap-get-range pnm))
	 (ctime (current-time))
	 rtr rtrs dels uid siz uidl old-uidl uid-time hlds)
    (if (eq directive 'inc)
	(setq old-uidl (mew-net-uidl-db-get (mew-imap-passtag pnm))))
    (goto-char (point-min))
    (while (re-search-forward "^\\* [0-9]+ FETCH (UID \\([0-9]+\\) RFC822.SIZE \\([0-9]+\\))" nil t)
      (setq uid (mew-match-string 1))
      (setq siz (mew-match-string 2))
      (cond
       ((or (eq directive 'exec) (eq directive 'jobs) (eq directive 'get))
	(cond
	 ((setq rtr (assoc uid refs))
	  (setq rtrs (cons rtr rtrs)))
	 ((member uid rmvs)
	  (setq dels (cons uid dels)))))
       ((eq directive 'biff)
	(if (and uid (> (string-to-int uid) max))
	    (setq rtrs (cons (list uid siz) rtrs))))
       ((eq directive 'scan)
	(if (and uid (or range ;; all, last:n
			 (> (string-to-int uid) max))) ;; update
	    (setq rtrs (cons (list uid siz) rtrs))))
       ((eq directive 'sync)
	(if uid (setq hlds (cons uid hlds))))
       ((eq directive 'inc)
	(if uid (setq uid-time (cdr (assoc uid old-uidl))))
	(cond
	 (uid-time
	  (setq uidl (cons (cons uid uid-time) uidl))
	  (if (mew-expired-p uid-time del-time)
	      (setq dels (cons uid dels))))
	 (t
	  (setq uidl (cons (cons uid ctime) uidl))
	  (if (eq del-time t)
	      (setq dels (cons uid dels)))
	  (setq rtr (list uid siz))
	  (setq rtrs (cons rtr rtrs)))))))
    (mew-imap-set-msgid pnm (or (nth 0 (car rtrs)) max)) ;; 'scan
    (mew-imap-set-uidl pnm uidl) ;; 'inc
    ;; last:n
    (when (and (eq directive 'scan) (numberp range))
      (if (> (length rtrs) range)
	  (setcdr (nthcdr (1- range) rtrs) nil)))
    (setq rtrs (nreverse rtrs))
    (setq dels (nreverse dels))
    (setq hlds (nreverse hlds))
    (mew-imap-command-dispatch pro pnm directive rtrs dels hlds)))

(defun mew-imap-command-dispatch (pro pnm directive rtrs dels hlds)
  (cond
   ((or (eq directive 'exec) (eq directive 'jobs))
    ;; '((uid1 siz1 t   dst1 dst2)
    ;;   (uid2 siz2 t   dst1)
    ;;   (uid3 siz3 nil dst1 dst2)
    ;;   (uid4 siz4 nil dst3))
    ;; =>
    ;; '((dst1 uid1 uid2)
    ;;   (dst2 uid1 uid2 uid3)
    ;;   (dst3 uid4))
    ;; dels2 = (uid1 uid2)
    ;; =>
    ;; '((dst1 uid1,uid2)
    ;;   (dst2 uid1,uid2)
    ;;   (dst2 uid3)
    ;;   (dst3 uid4))
    (let (uid del fld flds ent ents dels2 fld-uids uids)
      (setq ents rtrs)
      (setq rtrs nil)
      (while ents
	(setq ent  (car ents))
	(setq ents (cdr ents))
	(mew-set '(uid nil del) ent)
	(setq flds (nthcdr 3 ent))
	(if del (setq dels2 (cons uid dels2)))
	(while flds
	  (setq fld  (car flds))
	  (setq flds (cdr flds))
	  (if (setq fld-uids (assoc fld rtrs))
	      (nconc fld-uids (list uid))
	    (setq rtrs (cons (list fld uid) rtrs)))))
      ;;
      (setq ents rtrs)
      (setq rtrs nil)
      (while ents
	(setq ent  (car ents))
	(setq ents (cdr ents))
	(setq fld  (car ent))
	(setq uids (cdr ent))
	(setq uids (mew-net-msg-group uids))
	(setq uids (mapcar (lambda (arg) (list fld arg)) uids))
	(setq rtrs (nconc rtrs uids)))
      (mew-imap-set-rtrs pnm rtrs)
      (mew-imap-set-rgttl pnm (length rtrs))
      ;;
      (setq dels (mew-net-msg-group dels))
      (setq dels2 (nreverse dels2))
      (setq dels2 (mew-net-msg-group dels2))
      (setq dels (nconc dels dels2))))
   (t
    (mew-imap-set-rttl pnm (length rtrs))
    (mew-imap-set-rtrs pnm rtrs)
    (mew-imap-set-dttl pnm (length dels))
    (setq dels (mew-net-msg-group dels))))
  ;;
  (mew-imap-set-dels pnm dels)
  (mew-imap-set-dgttl pnm (length dels))
  (mew-imap-set-hlds pnm hlds)
  (mew-imap-set-dispatched pnm t)
  (cond
   ((or (eq directive 'sync) (eq directive 'biff))
    (mew-imap-set-status pnm "logout")
    (mew-imap-command-logout pro pnm))
   ((or (eq directive 'exec) (eq directive 'jobs))
    (if rtrs
	(mew-imap-command-pre-copy pro pnm)
      (mew-imap-command-pre-dels pro pnm)))
   (t ;; 'get and 'inc
    (mew-imap-command-pre-fetch pro pnm))))

(defun mew-imap-command-pre-copy (pro pnm)
  (let ((rtt (length (mew-imap-get-rtrs pnm)))
	(rttl (mew-imap-get-rttl pnm))
	(dttl (mew-imap-get-dttl pnm))
	(jcnt (mew-imap-get-jcnt pnm)))
    (cond
     ((= rtt 0) ;; should not occur
      (mew-imap-set-status pnm "dels") ;; logout
      (mew-imap-command-dels pro pnm))
     (t
      (cond
       (jcnt
	(cond
	 ((= jcnt 1)
	  (mew-imap-message pnm "Processing 1 job in background..."))
	 (t
	  (mew-imap-message pnm "Processing %d jobs in background..." jcnt))))
       ((= rttl 0)
	(cond
	 ((= dttl 1)
	  (mew-imap-message pnm "Deleting 1 message in background..."))
	 (t
	  (mew-imap-message pnm "Deleting %d messages in background..." dttl))))
       ((= rttl 1)
	(mew-imap-message pnm "Refiling 1 message in background..."))
       (t
	(mew-imap-message pnm "Refiling %d messages in background..." rttl)))
      (mew-imap-set-status pnm "copy")
      (mew-imap-command-copy pro pnm)))))

(defun mew-imap-command-copy (pro pnm)
  (mew-net-status1
   (mew-imap-get-bnm pnm) "Refiling"
   (mew-imap-get-rgttl pnm) (mew-imap-get-rgcnt pnm) (mew-imap-secure-p pnm))
  (let* ((case (mew-imap-get-case pnm))
	 (spam (mew-imap-get-spam pnm))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (rtr (car rtrs))
	 (dst (nth 0 rtr))
	 (uid (nth 1 rtr)))
    (if (null rtr)
	(cond
	 ((mew-imap-get-dels pnm)
	  (mew-imap-set-status pnm "dels")
	  (mew-imap-command-dels pro pnm))
	 (t
	  (mew-imap-set-status pnm "logout")
	  (mew-imap-command-logout pro pnm)))
      (mew-imap-set-rgcnt pnm (1+ (mew-imap-get-rgcnt pnm)))
      (mew-imap-set-rtrs pnm (cdr rtrs))
      (setq dst (mew-imap-expand-mailbox
		 case (mew-imap-utf-7-encode-string dst)))
      (mew-imap-set-rfl pnm rtr)
      (if spam
	  (mew-imap-process-send-string pro pnm "COPY %s \"%s\"" uid dst)
	(mew-imap-process-send-string pro pnm "UID COPY %s \"%s\"" uid dst)))))

(defun mew-imap-command-create (pro pnm)
  (let* ((case (mew-imap-get-case pnm))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (rtr (mew-imap-get-rfl pnm))
	 (dst (nth 0 rtr))) ;; 'exec or 'jobs
    (mew-imap-set-rgcnt pnm (1- (mew-imap-get-rgcnt pnm)))
    (mew-imap-set-rtrs pnm (cons rtr rtrs))
    (setq dst (mew-imap-expand-mailbox
	       case (mew-imap-utf-7-encode-string dst)))
    (mew-imap-process-send-string pro pnm "CREATE \"%s\"" dst)))

(defun mew-imap-command-wmbx (pro pnm)
  (mew-imap-set-error pnm "Mailbox name is wrong!")
  (mew-imap-set-status pnm "logout2")
  (mew-imap-command-logout2 pro pnm))

(defun mew-imap-command-pre-dels (pro pnm)
  (let ((dtt (length (mew-imap-get-dels pnm)))
	(dttl (mew-imap-get-dttl pnm))
	(jcnt (mew-imap-get-jcnt pnm)))
    (cond
     ((= dtt 0) ;; should not occur
      (mew-imap-set-status pnm "logout")
      (mew-imap-command-logout pro pnm))
     (t
      (cond
       (jcnt
	(cond
	 ((= jcnt 1)
	  (mew-imap-message pnm "Processing 1 job in background..."))
	 (t
	  (mew-imap-message pnm "Processing %d jobs in background..." jcnt))))
       ((= dttl -1)
	(mew-imap-message pnm "Deleting all messages in background..."))
       ((= dttl 1)
	(mew-imap-message pnm "Deleting 1 message in background..."))
       (t
	(mew-imap-message pnm "Deleting %d messages in background..." dttl)))
      (mew-imap-set-status pnm "dels")
      (mew-imap-command-dels pro pnm)))))

(defun mew-imap-command-dels (pro pnm)
  (mew-net-status1
   (mew-imap-get-bnm pnm) "Deleting"
   (mew-imap-get-dgttl pnm) (mew-imap-get-dgcnt pnm) (mew-imap-secure-p pnm))
  (let ((dels (mew-imap-get-dels pnm))
	(spam (mew-imap-get-spam pnm))
	num)
    (cond
     (dels
      (mew-imap-set-dgcnt pnm (1+ (mew-imap-get-dgcnt pnm)))
      (setq num (car dels))
      (mew-imap-set-dels pnm (cdr dels))
      (if spam
	  (mew-imap-process-send-string
	   pro pnm "STORE %s +FLAGS (\\Deleted)" num)
	(mew-imap-process-send-string
	 pro pnm "UID STORE %s +FLAGS (\\Deleted)" num)))
     (t
      (mew-imap-set-status pnm "expunge")
      (mew-imap-command-expunge pro pnm)))))

;;

(defun mew-imap-command-search (pro pnm)
  (mew-net-status
   (mew-imap-get-bnm pnm) "Filtering" nil (mew-imap-secure-p pnm))
  (mew-imap-process-send-string
   pro pnm "SEARCH HEADER %s %s"
   (mew-imap-get-spam-field pnm)
   (mew-imap-get-spam-word pnm)))

(defun mew-imap-command-post-search (pro pnm)
  (let* ((case (mew-imap-get-case pnm))
	 (trash (or (mew-imap-spam-folder case) (mew-imap-trash-folder case)))
	 dels rtrs beg)
    (goto-char (point-min))
    (mew-crlf-to-lf)
    (goto-char (point-min))
    (when (looking-at "^\\* SEARCH ")
      (setq beg (match-end 0))
      (goto-char (point-max))
      (forward-line -1)
      (if (bolp) (forward-char -1))
      (setq dels (mew-split (mew-buffer-substring beg (point)) mew-sp))
      (when trash
	(setq rtrs (copy-sequence dels))
	(mew-imap-set-rttl pnm (length rtrs))
	(setq rtrs (mew-net-msg-group rtrs))
	(setq rtrs (mapcar (lambda (arg) (list trash arg)) rtrs))
	(mew-imap-set-rtrs pnm rtrs)
	(mew-imap-set-rgttl pnm (length rtrs)))
      (mew-imap-set-dttl pnm (length dels))
      (setq dels (mew-net-msg-group dels))
      (mew-imap-set-dels pnm dels)
      (mew-imap-set-dgttl pnm (length dels)))
    (cond
     (rtrs
      (mew-imap-command-pre-copy pro pnm))
     (dels
      (mew-imap-command-pre-dels pro pnm))
     (t
      (mew-imap-set-status pnm "uid")
      (mew-imap-command-uid pro pnm)))))

;;

(defun mew-imap-command-pre-fetch (pro pnm)
  (let* ((rtt (length (mew-imap-get-rtrs pnm))))
    (cond
     ((= rtt 0)
      (mew-imap-message pnm "No new messages")
      (mew-imap-set-status pnm "logout")
      (mew-imap-command-logout pro pnm))
     ((= rtt 1)
      (mew-imap-message pnm "Retrieving 1 message in background...")
      (mew-imap-set-status pnm "fetch")
      (mew-imap-command-fetch pro pnm))
     (t
      (mew-imap-message pnm "Retrieving %d messages in background..." rtt)
      (mew-imap-set-status pnm "fetch")
      (mew-imap-command-fetch pro pnm)))))

(defun mew-imap-command-fetch (pro pnm)
  (mew-net-status2 (mew-imap-get-bnm pnm)
		   (mew-imap-get-rttl pnm)
		   (mew-imap-get-rcnt pnm)
		   (nth 1 (car (mew-imap-get-rtrs pnm)))
		   'zero
		   (mew-imap-secure-p pnm))
  (let* ((directive (mew-imap-get-directive pnm))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (dels (mew-imap-get-dels pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (lim (mew-imap-get-size pnm))
	 (get-body (mew-imap-get-get-body pnm)))
    (cond
     ((or (null rtr) (eq directive 'biff))
      (mew-imap-set-truncated pnm nil)
      (if dels
	  (progn
	    (mew-imap-set-status pnm "dels")
	    (mew-imap-command-dels pro pnm))
	(mew-imap-set-status pnm "logout")
	(mew-imap-command-logout pro pnm)))
     ((or (eq directive 'exec) (eq directive 'jobs) (eq directive 'get))
      (mew-imap-set-truncated pnm nil)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822" uid))
     ((and (eq directive 'scan) (not get-body))
      (mew-imap-set-truncated pnm t)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822.HEADER" uid))
     ((or (= lim 0) (< (string-to-int siz) lim))
      (mew-imap-set-truncated pnm nil)
      (mew-imap-process-send-string pro pnm "UID FETCH %s RFC822" uid))
     (t
      (mew-imap-set-truncated pnm t)
      (mew-imap-process-send-string
       pro pnm "UID FETCH %s RFC822.HEADER" uid)))))

(defun mew-imap-command-post-fetch (pro pnm)
  (let* ((directive (mew-imap-get-directive pnm))
	 (width (1- (mew-scan-width)))
	 (rtrs (mew-imap-get-rtrs pnm))
	 (rtr (car rtrs))
	 (uid (nth 0 rtr))
	 (siz (nth 1 rtr))
	 (fld-msg (nth 3 rtr)) ;; 'exec, 'jobs, 'scan, ...
	 (truncated (mew-imap-get-truncated pnm))
	 fld msg vec file msg-file lmsg mark)
    (cond
     ((null fld-msg)
      (setq fld (mew-imap-get-bnm pnm)))
     ((stringp fld-msg)
      (setq fld fld-msg))
     ((listp fld-msg)
      (mew-set '(fld msg) fld-msg)
      (setq lmsg msg)))
    ;; line delimiters
    (goto-char (point-min))
    (mew-crlf-to-lf)
    (cond
     ((eq directive 'scan)
      (setq msg uid) 
      (setq file (mew-expand-folder fld msg)))
     (t
      (setq msg-file (mew-net-get-new-message
		      pnm fld msg 'mew-imap-get-msgdb 'mew-imap-set-msgdb))
      (setq msg (car msg-file) file (cdr msg-file))))
    (goto-char (point-min))
    (cond
     (truncated
      (mew-header-insert-xmu uid siz t))
     ((eq directive 'scan)
      (mew-header-insert-xmu uid siz nil))
     ((and (eq directive 'get) (mew-folder-imapp (mew-case:folder-folder fld)))
      (mew-header-insert-xmu uid siz nil)))
    (catch 'write-error
      (condition-case nil
	  (mew-frwlet 
	   mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) file nil 'no-msg))
	(error
	 (mew-imap-set-status pnm "logout")
	 (mew-imap-command-logout pro pnm)
	 (throw 'write-error nil)))
      (mew-set-file-modes file)
      (mew-imap-set-rcnt pnm (1+ (mew-imap-get-rcnt pnm)))
      ;;
      (mew-set-buffer-multibyte t)
      (setq vec (mew-scan-header))
      (mew-scan-set-folder vec fld)
      (mew-scan-set-message vec msg)
      (if (and (eq directive 'scan)
	       (string= (mew-case:folder-folder fld) mew-imap-inbox-folder))
	  (setq mark (mew-scan-inbox-action vec)))
      (mew-scan-body vec)
      (mew-set-buffer-multibyte nil)
      (mew-scan-insert-line fld vec width lmsg mark)
      (mew-imap-set-rtrs pnm (cdr rtrs))
      (mew-imap-set-status pnm "fetch")
      (mew-imap-set-bytes pnm nil)
      (mew-imap-command-fetch pro pnm))))

(defun mew-imap-command-expunge (pro pnm)
  (mew-imap-process-send-string pro pnm "EXPUNGE"))

(defun mew-imap-command-post-expunge (pro pnm)
  (if (mew-imap-get-spam pnm)
      (progn
	(mew-imap-set-status pnm "uid")
	(mew-imap-command-uid pro pnm ))
    (mew-imap-set-status pnm "logout")
    (mew-imap-command-logout pro pnm)))

(defun mew-imap-command-namespace (pro pnm)
  (mew-imap-process-send-string pro pnm "NAMESPACE"))

(defun mew-imap-read-sexp ()
  (condition-case nil
      (read (current-buffer))
    (error ())))

(defun mew-imap-command-post-namespace (pro pnm)
  (when (looking-at "^\\* NAMESPACE ")
    (let* ((beg (match-end 0))
	   (case (mew-imap-get-case pnm))
	   (prefix-list (mew-imap-prefix-list case))
	   (my-prefix "")
	   user-prefix sexp sep sharp namespace ent my sp specials)
      (goto-char (point-max))
      (forward-line -1)
      (save-restriction
	(narrow-to-region beg (point)) ;; xxx (1- (point))?
	(goto-char (point-min))
	(while (search-forward "NIL" nil t)
	  (replace-match "nil" t t))
	(goto-char (point-min))
	(setq sexp (mew-imap-read-sexp))
	(cond
	 ((null prefix-list)
	  (setq ent (car sexp))
	  (setq my-prefix (nth 0 ent))
	  (setq sep (nth 1 ent)))
	 (t
	  (while sexp
	    (setq ent (car sexp))
	    (setq sexp (cdr sexp))
	    (mew-set '(my sp) ent)
	    (when (member my prefix-list)
	      (cond
	       (sp
		(setq my-prefix my)
		(setq sep sp))
	       (t
		(setq specials (cons (cons (concat mew-folder-imap my) my) specials))))))))
	(mew-imap-set-my-prefix pnm my-prefix)
	(mew-imap-set-specials pnm specials)
	(setq sexp (mew-imap-read-sexp))
	(setq user-prefix (or (nth 0 (car sexp)) ""))
	(setq sexp (mew-imap-read-sexp))
	(setq sharp (mew-assoc-match "^#" sexp 0)))
      (setq namespace (mew-imap-namespace-create sep my-prefix user-prefix sharp))
      (mew-imap-set-namespace pnm namespace)))
  (mew-imap-set-status pnm "list")
  (mew-imap-command-list pro pnm))

(defun mew-imap-command-list (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Listing"
		  nil
		  (mew-imap-secure-p pnm))
  (let ((my-prefix (mew-imap-get-my-prefix pnm)))
    (mew-imap-message pnm "Collecting mailbox list...")
    (mew-imap-process-send-string pro pnm "LIST \"%s\" *" (or my-prefix ""))))

(defun mew-imap-command-post-list (pro pnm)
  (let* ((case (mew-imap-get-case pnm))
	 (queue (mew-imap-queue-folder case))
	 (namespace (mew-imap-get-namespace pnm))
	 (specials (mew-imap-get-specials pnm))
	 ret mailboxes friends)
    (unless namespace
      (setq namespace (mew-imap-namespace-create nil "" "" nil)) ;; xxx separator?
      (mew-imap-set-namespace pnm namespace))
    (setq ret (mew-imap-command-post-list-tree namespace case))
    (mew-set '(mailboxes friends) ret)
    (unless case (setq case mew-case-default))
    (if specials (setq mailboxes (nconc specials mailboxes)))
    (if queue (mew-folder-insert queue mailboxes nil))
    (mew-imap-folder-set case mailboxes friends)
    (mew-imap-set-status pnm "logout")
    (mew-imap-command-logout pro pnm)))

(defun mew-imap-command-post-list-tree (namespace case)
  (let* ((case-fold-search t)
	 (my   (format "^%s" (regexp-quote
			      (or (mew-imap-namespace-my-prefix namespace) ""))))
	 (user (format "^%s" (regexp-quote
			      (or (mew-imap-namespace-user-prefix namespace) ""))))
	 (sep (mew-imap-namespace-sep namespace))
	 subfolder friendp mbx mailbox mailboxes subnm friends)
    (goto-char (point-min))
    (mew-crlf-to-lf)
    (goto-char (point-min))
    (when sep
      (setq subfolder (format "\\([^%s\n\r]+\\)$" sep))
      (setq friendp (mew-imap-friend-regex sep case)))
    (while (not (eobp))
      (catch 'break
	(when (looking-at "^\\* LIST ([^)\n]*) \"?\\([^\"\n]*\\)\"? \"?\\([^\"\n]*\\)\"?")
	  (unless sep
	    ;; NAMESPACE may return NIL NIL NIL
	    (setq sep (mew-match-string 1))
	    (mew-imap-namespace-change-sep namespace sep)
	    (setq subfolder (format "\\([^%s\n\r]+\\)$" sep))
	    (setq friendp (mew-imap-friend-regex sep case)))
	  (setq mbx (mew-match-string 2))
	  (setq mbx (mew-imap-utf-7-decode-string mbx))
	  (cond
	   ((mew-imap-inbox-str-p mbx)
	    (throw 'break nil))
	   ((string-match my mbx)
	    (setq mbx (substring mbx (match-end 0)))
	    (setq mailbox (concat mew-folder-imap mbx))
	    (cond
	     ((string-match friendp mailbox)
	      (setq friends (cons mailbox friends))
	      (setq mailboxes (cons (mew-folder-func mailbox) mailboxes)))
	     ((string-match subfolder mbx)
	      (setq subnm (match-string 1 mbx))
	      (setq mailboxes (cons (mew-folder-func mailbox subnm) mailboxes)))))
	   ((string-match user mbx)
	    (setq mbx (substring mbx (match-end 0)))
	    (setq mailbox (concat mew-folder-imap "~" mbx))
	    (when (string-match subfolder mbx)
	      (setq subnm (match-string 1 mbx))
	      (setq mailboxes (cons (mew-folder-func mailbox subnm) mailboxes))))
	   (t
	    (setq mailbox (concat mew-folder-imap "#" mbx))
	    (when (string-match subfolder mbx)
	      (setq subnm (match-string 1 mbx))
	      (setq mailboxes (cons (mew-folder-func mailbox subnm) mailboxes)))))))
      (forward-line))
    (setq mailboxes (sort mailboxes (lambda (x y) (string< (car x) (car y)))))
    (setq mailboxes (cons namespace mailboxes))
    (mew-imap-mailbox-arrange mailboxes sep)
    (setq friends (sort friends 'string<))
    (list mailboxes friends)))

(defun mew-imap-mailbox-arrange (lst sep)
  (let ((sep-char (string-to-char sep))
	prv fst nxt len1 len2)
    (setq prv lst)
    (setq fst (car (car lst)))
    (setq len1 (length fst))
    (setq lst (cdr lst))
    (while lst
      (setq nxt (car (car lst)))
      (setq len2 (length nxt))
      (when (and (< len1 len2)
		 (char-equal (aref nxt len1) sep-char)
		 (string= fst (substring nxt 0 len1)))
	(setcar (car prv) (concat fst sep)))
      (setq fst nxt)
      (setq prv lst)
      (setq lst (cdr lst))
      (setq len1 len2))))

(defun mew-imap-command-delete (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Deleting"
		  nil
		  (mew-imap-secure-p pnm))
  (mew-imap-message pnm "Deleting this folder...")
  (let ((mailbox (mew-imap-get-mailbox pnm)))
    (mew-imap-process-send-string pro pnm "DELETE \"%s\"" mailbox)))

(defun mew-imap-command-rename (pro pnm)
  (mew-net-status (mew-imap-get-bnm pnm)
		  "Renaming"
		  nil
		  (mew-imap-secure-p pnm))
  (mew-imap-message pnm "Deleting this folder...")
  (mew-imap-message pnm "Renaming this folder...")
  (let ((old (mew-imap-get-mailbox pnm))
	(new (mew-imap-get-new-mailbox pnm)))
    (mew-imap-process-send-string pro pnm "RENAME \"%s\" \"%s\"" old new)))

(defun mew-imap-command-logout (pro pnm)
  (let ((directive (mew-imap-get-directive pnm)))
    (cond
     ((eq directive 'jobs)
      (mew-queue-backup (mew-imap-get-wrk pnm) mew-imapq-info-suffix)
      (if (mew-imap-get-jobs pnm)
	  (progn
	    (mew-imap-set-status pnm "next")
	    (mew-imap-command-next pro pnm))
	(mew-imap-set-done pnm t)
	(mew-imap-process-send-string pro pnm "LOGOUT")))
     (t
      (mew-imap-set-done pnm t)
      (mew-imap-process-send-string pro pnm "LOGOUT")))))

(defun mew-imap-command-logout2 (pro pnm)
  (let ((wrk (mew-imap-get-wrk pnm)))
    (if wrk (mew-queue-enqueue2 wrk)))
  (mew-imap-process-send-string pro pnm "LOGOUT"))

(defun mew-imap-command-logout3 (pro pnm)
  (mew-imap-process-send-string pro pnm "LOGOUT"))

(defun mew-imap-command-noop (pro pnm)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUTH
;;;

(defvar mew-imap-auth-alist
  '(("CRAM-MD5" mew-imap-command-auth-cram-md5)
    ("LOGIN"    mew-imap-command-auth-login)))

(defsubst mew-imap-auth-get-func (auth)
  (nth 1 (mew-assoc-case-equal auth mew-imap-auth-alist 0)))

(defun mew-imap-command-auth-cram-md5 (pro pnm)
  (mew-imap-process-send-string pro pnm "AUTHENTICATE CRAM-MD5")
  (mew-imap-set-status pnm "auth-cram-md5"))

(defun mew-imap-command-pwd-cram-md5 (pro pnm)
  (let ((user (mew-imap-get-user pnm))
        (prompt (format "IMAP CRAM-MD5 password (%s): "
                        (mew-imap-get-account pnm)))
	challenge passwd cram-md5)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward " \\([a-zA-Z0-9+/]+=*\\)" nil t)
	  (setq challenge (mew-match-string 1))))
    (setq passwd (mew-imap-input-passwd prompt pnm))
    (setq cram-md5 (mew-cram-md5 user passwd challenge))
    (mew-imap-process-send-string2 pro cram-md5)))

(defun mew-imap-command-auth-login (pro pnm)
  (mew-imap-process-send-string pro pnm "AUTHENTICATE LOGIN")
  (mew-imap-set-status pnm "auth-login"))

(defun mew-imap-command-user-login (pro pnm)
  (let* ((user (mew-imap-get-user pnm))
	 (euser (mew-base64-encode-string user)))
    (mew-imap-process-send-string2 pro euser)))

(defun mew-imap-command-pwd-login (pro pnm)
  (let* ((prompt (format "IMAP LOGIN password (%s): "
                         (mew-imap-get-account pnm)))
         (passwd (mew-imap-input-passwd prompt pnm))
	 (epasswd (mew-base64-encode-string passwd)))
    (mew-imap-process-send-string2 pro epasswd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub functions
;;;

(defconst mew-imap-info-prefix "mew-imap-info-")

(defsubst mew-imap-info-name (case mailbox)
  (let ((server (mew-imap-server case))
	(port (mew-imap-port case))
	(sshsrv (mew-imap-ssh-server case))
	(name mew-imap-info-prefix))
    (setq name (concat name server "/" mailbox))
    (unless (string= port mew-imap-port)
      (setq name (concat name ":" port)))
    (if sshsrv
	(concat name "%" sshsrv)
      name)))

(defsubst mew-imap-buffer-name (pnm)
  (concat mew-buffer-prefix pnm))

(defun mew-imap-process-send-string (pro pnm &rest args)
  (let ((str (apply 'format args))
	(tag (mew-imap-tag)))
    (mew-imap-debug "=SEND=" (concat tag " " str))
    (mew-imap-set-tag pnm tag)
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat tag " " str mew-cs-eol))
      (message "IMAP time out"))))

(defun mew-imap-process-send-string2 (pro &rest args)
  (let ((str (apply 'format args)))
    (if (and (processp pro) (eq (process-status pro) 'open))
	(process-send-string pro (concat str mew-cs-eol))
      (message "IMAP time out"))))

(defsubst mew-imap-message (pnm &rest args)
  (or (mew-imap-get-no-msg pnm) (apply 'message args)))

(defun mew-imap-tag ()
  (format "%s%04d" (mew-random-string 4 nil) (% (mew-random) 10000)))

(defun mew-imap-passtag (pnm)
  (let ((server (mew-imap-get-server pnm))
	(port (mew-imap-get-port pnm))
	(user (mew-imap-get-user pnm)))
    (concat user "@" server ":" port)))

(defun mew-imap-passtag2 (case)
  (let ((server (mew-imap-server case))
	(port (mew-imap-port case))
	(user (mew-imap-user case)))
    (concat user "@" server ":" port)))

(defun mew-imap-input-passwd (prompt pnm)
  (let ((directive (mew-imap-get-directive pnm))
	(pro (mew-imap-get-process pnm))
	(tag (mew-imap-passtag pnm))
	pass)
    (if (eq directive 'biff)
	(or (mew-imap-get-passwd pnm)      ;; mew-imap-biff
	    (mew-input-passwd prompt tag)) ;; mew-imap-check
      (setq pass (mew-input-passwd prompt tag))
      (unless (and (processp pro) (eq (process-status pro) 'open))
	(mew-passwd-set-passwd tag nil))
      pass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Namespace
;;;

(defvar mew-imap-namespace-length 5)

(defun mew-imap-namespace-create (sep my-prefix user-prefix sharp)
  (mew-folder-func mew-imap-inbox-folder (list sep my-prefix user-prefix sharp)))

(defun mew-imap-namespace (case)
  (let* ((alist (mew-imap-folder-alist case))
	 (namespace (assoc mew-imap-inbox-folder alist)))
    (if (= (length namespace) mew-imap-namespace-length)
	namespace
      (error (mew-substitute-for-summary "Type '3\\[mew-status-update]' to collect IMAP folders!")))))

(defsubst mew-imap-namespace-sep (namespace)
  (nth 1 namespace))

(defsubst mew-imap-namespace-change-sep (namespace sep)
  (setcar (nthcdr 1 namespace) sep))

(defsubst mew-imap-namespace-my-prefix (namespace)
  (nth 2 namespace))

(defsubst mew-imap-namespace-user-prefix (namespace)
  (nth 3 namespace))

(defsubst mew-imap-namespace-sharp (namespace)
  (nth 4 namespace))

(defun mew-imap-separator (case)
  (mew-imap-namespace-sep (mew-imap-namespace case)))

(defun mew-imap-directory-file-name (dir case)
  ;; inbox.foo. => inbox.foo
  (let* ((sep (mew-imap-separator case))
	 (sep-char (string-to-char sep))
	 (len (1- (length dir))))
    (if (char-equal sep-char (aref dir len))
	(substring dir 0 len)
      dir)))
	   
(defun mew-imap-file-name-as-directory (dir case)
  ;; inbox.foo => inbox.foo.
  (let* ((sep (mew-imap-separator case))
	 (sep-char (string-to-char sep))
	 (len (1- (length dir))))
    (if (char-equal sep-char (aref dir len))
	dir
      (concat dir sep))))

;;;

(defsubst mew-imap-bnm-to-mailbox (bnm)
  "
   case:%inbox -> inbox
   case:%foo   -> inbox.foo
   case:%~foo  -> user.foo
   case:%#foo  -> foo"
  (let ((case (mew-case:folder-case bnm))
	(folder (mew-case:folder-folder bnm)))
    (mew-imap-expand-mailbox case folder)))

(defun mew-imap-expand-mailbox (case folder)
  "%inbox -> inbox
   %foo   -> inbox.foo
   %~foo  -> user.foo
   %#foo  -> foo"
  (when (stringp folder)
    (setq folder (mew-imap-directory-file-name folder case))
    (let* ((str (mew-folder-string folder))
	   (c (aref str 0))
	   (substr (substring str 1))
	   (namespace (mew-imap-namespace case))
	   (my-prefix (mew-imap-namespace-my-prefix namespace))
	   (user-prefix (mew-imap-namespace-user-prefix namespace))
	   (sharp (mew-imap-namespace-sharp namespace)))
      (cond
       ((char-equal c ?~)
	(if user-prefix (concat user-prefix substr) substr))
       ((char-equal c ?#)
	(if sharp str substr))
       ((string= str mew-imap-inbox-string)
	mew-imap-inbox-string)
       (t
	(if my-prefix (concat my-prefix str) str))))))

(defun mew-imap-expand-folder (case str)
  "
   foo   -> foo
   ~foo  -> user.foo
   #foo  -> #foo
"
  (when (and (stringp str) (not (string= str "")))
    (if (char-equal (aref str 0) ?~)
	(let* ((namespace (mew-imap-namespace case))
	       (user-prefix (mew-imap-namespace-user-prefix namespace)))
	  (concat "#" user-prefix (substring str 1)))
      str)))

;;		Mew		cache		IMAP
;;		folder		directory	mailbox
;; 
;; WU
;;		%foo		foo		foo
;;		%foo/bar	foo/bar		foo/bar
;;		%~alice/foo	#~alice/foo	~alice/foo   "~" -> "~"
;;		%#shared.foo	#shared.foo	#shared.foo
;; 
;; Cyrus
;;		%foo		foo		inbox.foo
;;		%foo.bar	foo.bar		inbox.foo.bar
;;		%~alice.foo	#user.alice.foo	user.alice.foo
;;		%#shared.foo	#shared.foo	shared.foo
;; 
;; Courier
;;		%foo		foo		inbox.foo
;;		%foo.bar	foo.bar		inbox.foo.bar
;;		%~alice.foo	N/A		N/A
;;		%#shared.foo	#shared.foo	shared.foo

(defun mew-imap-local-to-imap (case str)
  (let* ((sep (mew-imap-separator case))
	 (fld (copy-sequence str)))
    (aset fld 0 ?%)
    (unless (string= sep mew-path-separator)
      (mew-replace-character fld ?/ (string-to-char sep)))
    fld))

(defun mew-imap-fcc-to-mailbox (case fld)
  (let* ((namespace (mew-imap-namespace case))
	 (my-prefix (mew-imap-namespace-my-prefix namespace))
	 (ret (substring fld (1+ (length mew-folder-imap-fcc-prefix)))))
    (if (string= ret mew-imap-inbox-string)
	mew-imap-inbox-string
      (if my-prefix (concat my-prefix ret) ret))))

(defun mew-imap-friend-regex (sep case)
  (format "^%s%s"
	  (regexp-quote (mew-imap-friend-folder case))
	  (regexp-quote sep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opening IMAP
;;;

(defun mew-imap-open (pnm server port no-msg)
  (let ((sprt (mew-port-sanity-check port))
	pro tm)
    (condition-case emsg
	(progn
	  (setq tm (mew-timer mew-imap-timeout-time 'mew-imap-timeout))
	  (or no-msg (message "Connecting to the IMAP server..."))
	  (setq pro (open-network-stream pnm nil server sprt))
	  (process-kill-without-query pro)
	  (mew-set-process-cs pro mew-cs-binary mew-cs-text-for-net)
	  (or no-msg (message "Connecting to the IMAP server...done")))
      (quit
       (or no-msg (message "Cannot connect to the IMAP server"))
       (setq pro nil))
      (error
       (or no-msg (message "%s, %s" (nth 1 emsg) (nth 2 emsg)))
       (setq pro nil)))
    (if tm (cancel-timer tm))
    pro))

(defun mew-imap-timeout ()
  (signal 'quit nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Launcher
;;;

(defun mew-imap-retrieve (case directive bnm &rest args)
  (let* ((server (mew-imap-server case))
         (user (mew-imap-user case))
	 (port (mew-imap-port case))
	 (sshsrv (mew-imap-ssh-server case))
	 (sslp (mew-imap-ssl case))
	 (sslport (mew-imap-ssl-port case))
	 ;; dirty but necessary for migration
	 (mailbox (unless (eq directive 'list)
		    (mew-imap-utf-7-encode-string
		     (mew-imap-bnm-to-mailbox bnm))))
	 (pnm (mew-imap-info-name case mailbox))
	 (buf (get-buffer-create (mew-imap-buffer-name pnm)))
	 (no-msg (eq directive 'biff))
	 process sshname sshpro sslname sslpro lport info jobs tls)
    (if (mew-imap-get-process pnm)
	(message "Another IMAP process is running. Try later")
      (cond
       (sshsrv
	(setq sshpro (mew-open-ssh-stream case server port sshsrv))
	(when sshpro
	  (setq sshname (process-name sshpro))
	  (setq lport (mew-ssh-pnm-to-lport sshname))
	  (when lport
	    (setq process (mew-imap-open pnm "localhost" lport no-msg)))))
       (sslp
	(if (string= port sslport) (setq tls mew-tls-imap))
 	(setq sslpro (mew-open-ssl-stream case server sslport tls))
 	(when sslpro
 	  (setq sslname (process-name sslpro))
	  (setq lport (mew-ssl-pnm-to-lport sslname))
 	  (when lport
 	    (setq process (mew-imap-open pnm "localhost" lport no-msg)))))
       (t
	(setq process (mew-imap-open pnm server port no-msg))))
      (if (null process)
	  (when (eq directive 'exec)
	    (mew-imap-exec-recover bnm))
	(mew-summary-lock process "IMAPing" (or sshpro sslpro))
	(mew-sinfo-set-summary-form (mew-get-summary-form bnm))
	(mew-sinfo-set-unread-mark nil)
	(mew-sinfo-set-scan-id nil)
	(mew-info-clean-up pnm)
	(mew-imap-set-no-msg pnm no-msg) ;; must come here
	(mew-imap-message pnm "Communicating with the IMAP server...")
	(mew-imap-set-process pnm process)
	(mew-imap-set-ssh-process pnm sshpro)
	(mew-imap-set-ssl-process pnm sslpro)
	(mew-imap-set-server pnm server)
	(mew-imap-set-port pnm port)
	(mew-imap-set-user pnm user)
        (mew-imap-set-account pnm (format "%s@%s" user server))
	(mew-imap-set-auth pnm (mew-imap-auth case))
	(mew-imap-set-auth-list pnm (mew-imap-auth-list case))
	(mew-imap-set-status pnm "greeting")
	(mew-imap-set-directive pnm directive)
	(mew-imap-set-bnm pnm bnm)
	(mew-imap-set-rcnt pnm 1)
	(mew-imap-set-rgcnt pnm 1)
	(mew-imap-set-dgcnt pnm 1)
	(mew-imap-set-jcnt pnm nil)
	(mew-imap-set-rttl pnm 0)
	(mew-imap-set-dttl pnm 0)
	(mew-imap-set-size pnm (mew-imap-size case))
	(mew-imap-set-mailbox pnm mailbox)
	(mew-imap-set-case pnm case)
	(mew-imap-set-bytes pnm nil)
	(cond
	 ((eq directive 'biff)
	  (mew-imap-set-passwd pnm (nth 0 args))) ;; password
	 ((eq directive 'inc)
	  (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))
	  (mew-imap-set-flush pnm (nth 0 args)) ;; no-flush
	  (mew-imap-set-delete pnm (mew-imap-delete case)))
	 ((eq directive 'get)
	  (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))
	  (mew-imap-set-refs pnm (nth 0 args)))
	 ((eq directive 'scan)
	  (mew-imap-set-range pnm (nth 0 args))
	  (mew-imap-set-get-body pnm (nth 1 args))
	  (if (mew-imap-get-range pnm)
	      (progn
		(mew-imap-set-mdb pnm (mew-summary-mark-collect4))
		(mew-net-folder-clean))
	    (mew-sinfo-set-unread-mark (mew-get-unread-mark bnm))
	    (mew-imap-set-mdb pnm (mew-summary-mark-collect5))
	    (mew-net-invalid-cache-invisible))
	  (when (and (string= mailbox mew-imap-inbox-string)
		     (mew-imap-spam-field case) (mew-imap-spam-word case))
	    (mew-imap-set-spam pnm t)
	    (mew-imap-set-spam-field pnm (mew-imap-spam-field case))
	    (mew-imap-set-spam-word pnm (mew-imap-spam-word case))))
	 ((eq directive 'sync)
	  )
	 ((eq directive 'exec)
	  (setq info (nth 0 args))
	  (mew-imap-set-rmvs pnm (nth 0 info)) ;; (uid siz del dst1 dst2 ...)
	  (mew-imap-set-kils pnm (nth 1 info))
	  (mew-imap-set-refs pnm (nth 2 info))
	  (mew-imap-set-movs pnm (nth 3 info))
	  (mew-imap-set-rttl pnm (nth 4 info))
	  (mew-imap-set-dttl pnm (nth 5 info)))
	 ((eq directive 'jobs)
	  (mew-imap-set-jcnt pnm 0)
	  (setq jobs (nth 0 args))
	  (mew-imap-set-jobs pnm jobs)
	  (mew-imap-set-jcnt pnm (length jobs)))
	 ((eq directive 'delete)
	  )
	 ((eq directive 'rename)
	  (mew-imap-set-new-mailbox pnm (mew-imap-bnm-to-mailbox (nth 0 args)))))
	(mew-sinfo-set-start-point (point)) ;; after erase-buffer
	;;
	(set-process-sentinel process 'mew-imap-sentinel)
	(set-process-filter process 'mew-imap-filter)
	(set-process-buffer process buf)))))

(defun mew-imap-exec-recover (bnm)
  (mew-summary-visible-buffer bnm)
  (mew-sinfo-set-refile
   (nconc (mew-sinfo-get-refile) (mew-sinfo-get-refile-back)))
  (mew-sinfo-set-refile-back nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-imap-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (let ((start (point)))
	(insert (format "\n<%s>\n%s\n" label string))
	(goto-char start)
	(mew-crlf-to-lf))
      (goto-char (point-max)))))

(defun mew-imap-filter (process string)
  (let* ((pnm (process-name process))
	 (status (mew-imap-get-status pnm))
	 (tag (mew-imap-get-tag pnm))
	 (eos (format "^%s \\(OK\\|NO \\[TRYCREATE\\]\\|NO\\|BAD\\)" tag))
	 (bytes (mew-imap-get-bytes pnm))
	 stay next func code)
    (mew-imap-debug (upcase status) string)
    (mew-filter
     ;; Process's buffer
     (goto-char (point-max))
     (mew-set-buffer-multibyte nil)
     (insert string)
     (cond
      ((string= status "fetch")
       (mew-net-status2 (mew-imap-get-bnm pnm)
			(mew-imap-get-rttl pnm)
			(mew-imap-get-rcnt pnm)
			(nth 1 (car (mew-imap-get-rtrs pnm)))
			nil
			(mew-imap-secure-p pnm))
       (goto-char (point-min))
       (when (and (null bytes)
		  (looking-at "^\\*[^{]*{\\([0-9]+\\)}"))
	 (setq bytes (string-to-int (mew-match-string 1)))
	 (mew-imap-set-bytes pnm bytes))
       (when bytes
	 (goto-char (point-min))
	 (while (and (looking-at "^\\*") (= (forward-line) 0))
	   (delete-region (point-min) (point))))
       (if (or (null bytes) (< (point-max) bytes)
	       (save-excursion (goto-char (point-max)) (not (bolp))))
	   (setq stay t)
	 (goto-char (point-max))
	 (forward-line -1)
	 (while (looking-at "^\\*") (forward-line -1))
	 (if (not (looking-at eos))
	     (setq stay t)
	   (delete-region (1+ bytes) (point-max))
	   (setq next (mew-imap-fsm-next status "OK")))))
      ((and (goto-char (point-max)) (= (forward-line -1) 0) (looking-at "^\\*"))
       ;; untagged message
       (if (string= status "greeting")
	   (setq next (mew-imap-fsm-next "greeting" "OK"))
	 (setq stay t)))
      ((and (goto-char (point-min)) (looking-at "\\+"))
       (setq next (mew-imap-fsm-next status "OK")))
      ((and (goto-char (point-max)) (= (forward-line -1) 0) (looking-at eos))
       (mew-imap-set-tag pnm nil)
       (setq code (mew-match-string 1))
       (setq next (mew-imap-fsm-next status code)))
      (t
       ;; xxx what about in this case?
       ;; * untag
       ;; * untag
       ;; tag
       ;; * untag
       (setq stay t)))
     (unless stay
       (unless next (setq next "logout"))
       (mew-imap-set-status pnm next)
       (setq func (intern-soft (concat "mew-imap-command-" next)))
       (goto-char (point-min))
       (if (fboundp func)
	   (funcall func process pnm)
	 (error "No function called %s" (symbol-name func)))
       (if (and process (equal (process-buffer process) (current-buffer)))
	   (mew-erase-buffer))))))

(defun mew-imap-sentinel (process event)
  (let* ((pnm (process-name process))
	 (directive (mew-imap-get-directive pnm))
	 (case (mew-imap-get-case pnm))
	 (mdb (mew-imap-get-mdb pnm))
	 (sshpro (mew-imap-get-ssh-process pnm))
	 (sslpro (mew-imap-get-ssl-process pnm))
	 (rttl (mew-imap-get-rttl pnm))
	 (dttl (mew-imap-get-dttl pnm))
	 (jcnt (mew-imap-get-jcnt pnm))
	 (bnm (or (mew-imap-get-bnm pnm) (current-buffer))) ;; C-c C-k
	 (flush (mew-imap-get-flush pnm))
	 (kils (mew-imap-get-kils pnm))
	 (movs (mew-imap-get-movs pnm))
	 (hlds (mew-imap-get-hlds pnm))
	 (uidl (mew-imap-get-uidl pnm))
	 (msgid (mew-imap-get-msgid pnm))
	 (done (mew-imap-get-done pnm))
	 (error (mew-imap-get-error pnm))
	 (file (mew-expand-folder bnm mew-imap-msgid-file))
	 (buf (process-buffer process)))
    (mew-imap-debug "IMAP SENTINEL" event)
    (set-process-buffer process nil)
    (set-buffer bnm)
    (mew-summary-mark-recover mdb)
    (mew-remove-buffer buf)
    (if (not done)
	(let* ((rtrs (mew-imap-get-rtrs pnm))
	       (lefts (length rtrs))
	       (msgid (nth 0 (car rtrs)))
	       leftp)
	  (if error
	      (mew-imap-message pnm error)
	    (mew-imap-message pnm "IMAP connection is lost"))
	  (cond
	   ((eq directive 'exec)
	    (mew-imap-exec-recover bnm))
	   ((eq directive 'scan)
	    (mew-summary-visible-buffer bnm))
	   ((or (eq directive 'delete) (eq directive 'rename))
	    (setq mew-summary-buffer-process-error t)))
	  (when (mew-imap-get-dispatched pnm)
	    (cond
	     ((eq directive 'scan)
	      (when msgid
		(setq msgid (int-to-string (1- (string-to-int msgid))))
		(mew-lisp-save file msgid nil 'unlimit))
	      (setq leftp t))
	     ((eq directive 'inc)
	      ;; uidl is reversed.
	      (setq msgid (assoc msgid uidl))
	      (setq uidl (cdr (member msgid uidl)))
	      (mew-net-uidl-db-set (mew-imap-passtag pnm) uidl)
	      (setq leftp t)))
	    (when leftp
	      (mew-imap-message
	       pnm
	       "%d message retrieved. %d messages are left due to an error"
	       (- rttl lefts) lefts)
	      (mew-summary-folder-cache-save))))
      (cond
       ((eq directive 'list)
	(mew-imap-message pnm "Collecting mailbox list...done"))
       ((eq directive 'delete)
	(mew-imap-message pnm "Deleting this folder...done"))
       ((eq directive 'rename)
	(mew-imap-message pnm "Renaming this folder...done"))
       ((eq directive 'biff)
	(funcall mew-biff-function rttl))
       ((eq directive 'sync)
	(mew-imap-message pnm "Synchronizing messages...")
	(mew-net-folder-sync bnm hlds)
	(mew-imap-message pnm "Synchronizing messages...done")
	(mew-summary-folder-cache-save))
       ((eq directive 'inc)
	(mew-biff-clear)
	(mew-net-uidl-db-set (mew-imap-passtag pnm) uidl)
	(cond
	 ((= rttl 0)
	  (mew-imap-message pnm "No new messages"))
	 ((= rttl 1)
	  (mew-imap-message pnm "1 message retrieved")
	  (mew-summary-folder-cache-save))
	 (t
	  (mew-imap-message pnm "%s messages retrieved" rttl)
	  (mew-summary-folder-cache-save))))
       ((eq directive 'get)
	(cond
	 ((= rttl 0)
	  (mew-imap-message pnm "No new messages"))
	 ((= rttl 1)
	  (mew-imap-message pnm "1 message retrieved")
	  (mew-summary-folder-cache-save))
	 (t
	  (mew-imap-message pnm "%s messages retrieved" rttl)
	  (mew-summary-folder-cache-save))))
       ((eq directive 'scan)
	(mew-biff-clear)
	(cond
	 ((= rttl 0)
	  (mew-imap-message pnm "No new messages"))
	 ((= rttl 1)
	  (mew-imap-message pnm "1 message retrieved")
	  (mew-lisp-save file msgid nil 'unlimit)
	  (mew-net-invalid-cache-clean))
	 (t
	  (mew-imap-message pnm "%s messages retrieved" rttl)
	  (mew-lisp-save file msgid nil 'unlimit)
	  (mew-net-invalid-cache-clean))))
       ((eq directive 'exec)
	(if kils (mew-mark-exec-unlink bnm kils))
	(when movs
	  (let ((mew-inherit-exec-case case)
		(mew-inherit-offline t))
	    (mew-mark-exec-refile bnm movs)))
	(when (or kils movs)
	  (mew-mark-kill-invisible)
	  (mew-summary-folder-cache-save))
	(cond
	 ((= rttl 1)
	  (mew-imap-message pnm "1 message refiled"))
	 ((> rttl 1)
	  (mew-imap-message pnm "%d messages refiled" rttl))
	 ((= dttl -1)
	  (mew-imap-message pnm "All messages deleted"))
	 ((= dttl 1)
	  (mew-imap-message pnm "1 message deleted"))
	 (t
	  (mew-imap-message pnm "%d messages deleted" dttl))))
       ((eq directive 'jobs)
	(if (= jcnt 1)
	    (mew-imap-message pnm "1 job processed")
	  (mew-imap-message pnm "%d jobs processed" jcnt)))))
    ;;
    (mew-net-status-clear bnm)
    (mew-info-clean-up pnm)
    (set-buffer-modified-p nil)
    (mew-summary-unlock)
    (if (and (processp sshpro) (not mew-ssh-keep-connection))
	(process-send-string sshpro "exit\n"))
    (if (and (processp sslpro) (not mew-ssl-keep-connection))
 	 (delete-process sslpro))
    (unless (eq directive 'biff)
      (run-hooks 'mew-imap-sentinel-non-biff-hook))
    (run-hooks 'mew-imap-sentinel-hook)
     (when (and mew-auto-flush-queue flush)
       (mew-smtp-flush-queue mew-case-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biff
;;;

(defun mew-imap-biff ()
  (let* ((case mew-case-input)
	 (inbox (mew-proto-inbox-folder nil case))
	 (case:inbox (mew-case-folder case inbox))
	 (tag (mew-imap-passtag2 case))
	 passwd)
    (when (get-buffer case:inbox)
      (save-excursion
	(set-buffer case:inbox)
	(when (and (mew-summary-exclusive-p 'no-msg)
		   (and mew-use-cached-passwd
			(setq passwd (mew-passwd-get-passwd tag))))
	  (mew-imap-retrieve case 'biff case:inbox passwd))))))

(defun mew-imap-check ()
  "See if messages are arrived by IMAP."
  (interactive)
  (let* ((case mew-case-input)
	 (inbox (mew-proto-inbox-folder nil case))
	 (case:inbox (mew-case-folder case inbox)))
    (when (get-buffer case:inbox)
      (save-excursion
	(set-buffer case:inbox)
	(when (mew-summary-exclusive-p)
	  (mew-imap-retrieve case 'biff case:inbox))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mailbox alist
;;;

(defun mew-imap-folder-clean-up ()
  (setq mew-imap-folder-alist nil)
  (setq mew-imap-friend-folder-list nil))

(defun mew-imap-folder-alist (&optional case no-load)
  (unless case (setq case mew-case-default))
  (let* ((ent (assoc case mew-imap-folder-alist))
	 (t2 (nth 1 ent)) ;; may be nil
	 (alist (nth 2 ent))
	 (fld (mew-imap-folder case))
	 (file (mew-expand-folder fld mew-imap-folder-alist-file))
	 (t1 (mew-file-get-time file)))
    (if (or no-load (and t1 (not (mew-compare-times t1 t2))))
	alist
      (mew-imap-folder-load case)
      (setq ent (assoc case mew-imap-folder-alist))
      (setcar (nthcdr 1 ent) t1)
      (setq alist (nth 2 ent))
      (if alist
	  alist
	(delete nil
		(list
		 (mew-folder-func mew-imap-inbox-folder)
		 (mew-folder-func (mew-imap-trash-folder case))
		 (mew-folder-func (mew-imap-queue-folder case))))))))

(defun mew-imap-friend-folder-list (&optional case no-load)
  (unless case (setq case mew-case-default))
  (unless no-load (mew-imap-folder-alist case)) ;; do update
  (cdr (assoc case mew-imap-friend-folder-list)))

(defun mew-imap-folder-load (case)
  (let* ((fld (mew-imap-folder case))
	 (file (mew-expand-folder fld mew-imap-folder-alist-file))
	 (mailboxes (mew-lisp-load file))
	 (file2 (mew-expand-folder fld mew-imap-friend-folder-list-file))
	 (friends (mew-lisp-load file2)))
    (mew-imap-folder-set case mailboxes friends 'no-save)))

(defun mew-imap-folder-save (case)
  (let* ((fld (mew-imap-folder case))
	 (dir (mew-expand-folder fld))
	 (file (expand-file-name mew-imap-folder-alist-file dir))
	 (file2 (expand-file-name mew-imap-friend-folder-list-file dir))
	 (mailboxes (mew-imap-folder-alist case 'no-load))
	 (friends (mew-imap-friend-folder-list case 'no-load)))
    (mew-check-directory dir)
    (mew-lisp-save file mailboxes 'nobackup 'unlimit)
    (mew-lisp-save file2 friends 'nobackup 'unlimit)
    (mew-file-get-time file)))

(defun mew-imap-folder-set (case mailboxes friends &optional no-save)
  (unless case (setq case mew-case-default))
  (setq mew-imap-folder-alist
	(cons (list case nil mailboxes)
	      (delete (assoc case mew-imap-folder-alist)
		      mew-imap-folder-alist)))
  (setq mew-imap-friend-folder-list
	(cons (cons case friends)
	      (delete (assoc case mew-imap-friend-folder-list)
		      mew-imap-friend-folder-list)))
  (unless no-save
    (let ((t1 (mew-imap-folder-save case))
	  (ent (assoc case mew-imap-folder-alist)))
      (setcar (nthcdr 1 ent) t1))))

(defun mew-imap-update (&optional case)
  (let ((bnm (mew-summary-folder-name 'ext)))
    (unless case 
      (setq case (mew-sinfo-get-case))
      (if mew-config-cases
	  (setq case (mew-input-case case "IMAP"))))
    (mew-imap-retrieve case 'list bnm)))

(defun mew-imap-folder-insert (case folder)
  (let* ((sep (mew-imap-separator case))
	 (regex (format "\\([^%s%s\n\r]+\\)$" sep mew-folder-imap))
	 (mailboxes (mew-imap-folder-alist case))
	 (friends (mew-imap-friend-folder-list case))
	 (friendp (mew-imap-friend-regex sep case))
	 sub)
    (when (string-match regex folder)
      (cond
       ((string-match friendp folder)
	(mew-folder-insert folder mailboxes nil)
	(setq friends (cons folder friends)))
       (t
	(setq sub (match-string 1 folder))
	(mew-folder-insert folder mailboxes sub)))
      (mew-imap-folder-set case mailboxes friends)
      t)))

(defun mew-imap-folder-delete (case folder)
  (let ((mailboxes (mew-imap-folder-alist case))
	(friends (mew-imap-friend-folder-list case)))
    (mew-folder-delete folder mailboxes)
    (setq friends (delete folder friends))
    (mew-imap-folder-set case mailboxes friends)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flush the job queue
;;;

(defun mew-imap-flush-queue ()
  (let* ((case (mew-sinfo-get-case))
	 (qfld (mew-summary-folder-name 'ext))
	 (jobs (mew-folder-messages qfld)))
    (if (null jobs)
	(message "No IMAP jobs")
      (mew-summary-folder-cache-clean qfld)
      (mew-imap-retrieve case 'jobs qfld jobs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UTF-7 IMAP
;;;

(defun mew-imap-utf-7-decode-string (str)
  (if (or (null mew-cs-utf-16be) (not (string-match "&" str)))
      str ;; fast path
    (let ((regex "&[A-Za-z0-9+,]+-")
	  padn)
      (with-temp-buffer
	(mew-set-buffer-multibyte t)
	(insert str)
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-min))
	  (delete-char 1)
	  (while (search-forward "," nil t)
	    (replace-match "/" nil t))
	  (goto-char (point-max))
	  (delete-char -1)
	  (setq padn (- 4 (% (- (point-max) (point-min)) 4)))
	  (unless (= padn 4)
	    (insert (make-string padn ?=)))
	  (base64-decode-region (point-min) (point-max))
	  (decode-coding-region (point-min) (point-max) mew-cs-utf-16be)
	  (goto-char (point-max))
	  (widen))
	(buffer-substring-no-properties (point-min) (point-max))))))

(defun mew-imap-utf-7-encode-string (str)
  (if (or (null mew-cs-utf-16be) (string-match "^[ -%'-~]+$" str))
      str ;; fast path
    (let ((regex "[^ -%'-~]+"))
      (with-temp-buffer
	(mew-set-buffer-multibyte t)
	(insert str)
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (encode-coding-region (point-min) (point-max) mew-cs-utf-16be)
	  (base64-encode-region (point-min) (point-max))
	  (goto-char (point-min))
	  (insert "&")
	  (when (re-search-forward "=+" nil t)
	    (delete-region (match-beginning 0) (match-end 0)))
	  (goto-char (point-min))
	  (while (search-forward "/" nil t)
	    (replace-match "," nil t))
	  (goto-char (point-max))
	  (insert "-")
	  (widen))
	(buffer-substring-no-properties (point-min) (point-max))))))

(provide 'mew-imap)

;;; Copyright Notice:

;; Copyright (C) 2002-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-imap.el ends here
