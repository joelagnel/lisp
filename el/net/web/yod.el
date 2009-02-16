;;; yod.el - a yahoo messenger client

;; yod.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; yod.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even  the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright 2005,2006 Dennis Marti

(defvar yodel-version "0.27"
  "last updated on 2006/06/11")

(defvar yodel-home-page
  "http://marti.textdriven.com/yodel/")

(require 'cl)
(require 'thingatpt)
(require 'timer)
(require 'xml)
(require 'custom)

(defgroup yodel ()
  "yod.el chat client group"
  :group 'applications)

(defcustom yodel-data-directory "~/yodel"
  "Directory where we can store account data between sessions."
  :type 'string
  :group 'yodel)

(defcustom yodel-crypt-program "~/yodel/ycrypt3"
  "The path to the password encryption program."
  :type 'string
  :group 'yodel)

(defcustom yodel-debug-flag nil
  "when true, the packet processing functions will dump packet
contents, http requests, and times to the yodel-debug-buffer"
  :type 'boolean
  :group 'yodel)
;;(setf yodel-debug-flag t)

(defcustom yodel-supress-beep nil
  "When true, don't ring the bell when displaying alerts. The bell
might be visual (instead of a beep) depending on your settings."
  :type 'boolean
  :group 'yodel)
;(setf yodel-supress-beep t)

(defcustom yodel-chatroom-notices t
  "print informtional messages about users entering and leaving"
  :type 'boolean
  :group 'yodel)
;(setq yodel-chatroom-notices nil)

(defcustom yodel-http-get-images nil
  "Set to true to retrieve advertising images in chatrooms.
Retrieving advertising images may influence whether or not Yahoo
shuts down your connectionn periodically."
  :type 'boolean
  :group 'yodel)
;(setf yodel-http-get-images t)

(defcustom yodel-tattoo ""
  "A silly feature available in some Yahoo clients."
  :type 'string
  :group 'yodel)

(defcustom yodel-wrap-message-text nil
  "Set this value to true to wrap message text. This defaults to false
as of 2004/0/4/22 because it May cause problems for newer versions of
emacs."
  :type 'boolean
  :group 'yodel)
;(setq yodel-wrap-message-text nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable some simple unit testing

(defvar yodel-unit-tests (make-hash-table))

(defmacro def-yodel-test (name args &rest body)
  `(setf (gethash ',name yodel-unit-tests) (lambda ,args ,@body)))

(defun yodel-fail-test (name &rest reason)
  (error (format "failed '%s %s" name (or reason ""))))

(defun yodel-run-test (name &rest args)
  "run the named test with args"
  (let ((test (gethash name yodel-unit-tests)))
    (if test
	(apply test args)
      (yodel-fail-test name "test does not exist"))))

(defun yodel-run-all-tests ()
  "run all the unit tests"
  (maphash (lambda (k v) (yodel-run-test k)) yodel-unit-tests))

(def-yodel-test unit-test-test ()
  (assert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables that were causing warnings because they weren't defined early enough

(defvar yodel-chatroom-sent-init nil
  "true after we send a YODEL_SERVICE_CHAT_INIT")

(defvar yodel-chatroom-need-splash t)

(defvar *yodel-buddies* (make-hash-table :test 'equal)
  "store buddies by name")

(defvar yodel-current-chatroom nil
  "the name and lobby of the current chatroom. has the form 'Room Name:x',
where x is the lobby number.")

(defvar yodel-pending-text ""
  "incoming text that hasn't been processed yet.")

(defvar yodel-chat-buddy-name nil
  "buffer local variable that holds the name of the buddy you're
chatting with in this buffer.")

(defvar yodel-chat-categories nil)

;; some functions yod.el uses may not be defined depending on how
;; your emacs was built. if these functions don't exist, they aren't
;; needed. unfortunately, the reverse isn't true.

(defun yodel-set-process-coding-system (&rest args)
  (if (fboundp 'set-process-coding-system)
      (apply 'set-process-coding-system args)))

(defun yodel-set-buffer-multibyte (&rest args)
  (if (fboundp 'set-buffer-multibyte)
      (apply 'set-buffer-multibyte args)))

;; xemacs compatiblity

(defmacro yodel-xemacs ()
  (search "XEmacs" emacs-version))

(defmacro yodel-replace-in-string-macro (regex repl string)
  (if (yodel-xemacs)
      `(replace-in-string ,string ,regex ,repl)
    `(replace-regexp-in-string ,regex ,repl ,string)))

(defun yodel-replace-in-string (regex repl string &rest ignored)
  (yodel-replace-in-string-macro regex repl string))
;(yodel-replace-in-string "[0-9]" "x" "123")

(defun yodel-match-string (num &optional string)
  (if (yodel-xemacs)
      (match-string num string)
    (match-string-no-properties num string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yodel-32 (n)
  "convert an integer into a four character (32 bit) string. This has
to be available before the constants get defined. Numbers greater than
most-positive-fixnum need to be converted by hand."
  (let ((acc ()))
    (dotimes (i 4 (apply 'concat acc))
      (push (vector (logand n 255)) acc)
      (setq n (ash n -8)))))
;;(yodel-32 most-positive-fixnum)

(defun yodel-version ()
  "Display yod.el version information."
  (interactive)
  (message yodel-version))

;;; yahoo constants
;;; Grabbed many of the constants from GAIM on 2003/05/24.

(defconst yodel-signature "YMSG")

(defconst yodel-available "Available")

;; packet types
(defconst YODEL_YMSG 12)		;ymsg version
(defconst YODEL_YCHT 101)
(defconst YODEL_UNKNOWN 0)

(defun yodel-packet-header-length (type)
  (cond ((= type YODEL_YMSG) (+ 4 2 2 2 2 4 4))
	((= type YODEL_YCHT) 14)
	(t 0)))

(defconst yodel-delim (format "%c%c" #xc0 #x80))
;(multibyte-string-p yodel-delim)
;(equal yodel-delim (yodel-put16 (yodel-get16 yodel-delim 0)))

(defvar YODEL_PROTO_VER YODEL_YCHT)
(setq YODEL_PROTO_VER YODEL_YMSG)

(defvar YODEL_FLAGS 0)

(defconst YODEL_SERVICE_LOGON  1)
(defconst YODEL_SERVICE_LOGOFF 2)
(defconst YODEL_SERVICE_ISAWAY 3)
(defconst YODEL_SERVICE_ISBACK 4)
(defconst YODEL_SERVICE_IDLE 5)
(defconst YODEL_SERVICE_MESSAGE 6)
(defconst YODEL_SERVICE_IDACT 7)
(defconst YODEL_SERVICE_IDDEACT 8)
(defconst YODEL_SERVICE_MAILSTAT 9)

(defconst YODEL_SERVICE_USERSTAT 10)

(defconst YODEL_SERVICE_NEWMAIL 11)
(defconst YODEL_SERVICE_CHATINVITE 12)
(defconst YODEL_SERVICE_CALENDAR 13)
(defconst YODEL_SERVICE_NEWPERSONALMAIL 14)
(defconst YODEL_SERVICE_NEWCONTACT 15)
(defconst YODEL_SERVICE_ADDIDENT 16)
(defconst YODEL_SERVICE_ADDIGNORE 17)
(defconst YODEL_SERVICE_PING 18)
(defconst YODEL_SERVICE_GROUPRENAME 19)
(defconst YODEL_SERVICE_SYSMESSAGE 20)

(defconst YODEL_SERVICE_IMV_DATA 21)
(defconst YODEL_SERVICE_PASSTHROUGH2 22)

(defconst YODEL_SERVICE_CONFINVITE 24)
(defconst YODEL_SERVICE_CONFLOGON 25)
(defconst YODEL_SERVICE_CONFDECLINE 26)
(defconst YODEL_SERVICE_CONFLOGOFF 27)
(defconst YODEL_SERVICE_CONFADDINVITE 28)
(defconst YODEL_SERVICE_CONFMSG 29)

;; old chat stuff?
(defconst YODEL_SERVICE_CHATLOGON 30)
(defconst YODEL_SERVICE_CHATLOGOFF 31)
(defconst YODEL_SERVICE_CHATMSG 32)

(defconst YODEL_SERVICE_GAMELOGON 40)
(defconst YODEL_SERVICE_GAMELOGOFF 41)
(defconst YODEL_SERVICE_GAMEMSG 42)

(defconst YODEL_SERVICE_FILETRANSFER 70)
(defconst YODEL_SERVICE_NOTIFY 75)	;when someone is typing to you

(defconst yodel-service-p2p-request 77)	;2003/06/23 (not sure)
(defconst yodel-service-client-running-but-not-logged-in 76) ;2003/06/20
(defconst YODEL_SERVICE_EMPTY_PACKET 76)

;; sent one of these (service 79) to YahooHelper requesting
;; peer-to-peer.  the bot sent one back, with a status of all
;; 1's (like the deny you get from the chatroom logon)
(defconst YODEL_SERVICE_P2P 79)

(defconst YODEL_SERVICE_AUTHRESP 84)
;; service_list requests the list from the server
(defconst YODEL_SERVICE_LIST 85)

;; you send an 87 to the server, then it sends one back, then you
;; send an 84, then it sends an 85 if you get logged in.
;; you also get an 85 when you change your ignore list
(defconst YODEL_SERVICE_AUTH 87)

;; we get an 88 as part of the ignore process

(defconst YODEL_SERVICE_ADDBUDDY 131)
(defconst YODEL_SERVICE_REMBUDDY 132)

(defconst YODEL_SERVICE_IGNORE 133)

;; 138 - a keep alive the client sends to the server?
;; followed by something that looks like a junk packet.
;; XXX because we do get these junk packets, skip YMSG 
;; packets that don't have defined service numbers.

(defconst YODEL_SERVICE_HEARTBEAT 138)

(defconst YODEL_SERVICE_CHAT_INIT 150)
(defconst YODEL_SERVICE_CHAT_JOIN 152)

(defconst YODEL_SERVICE_CHAT_LEAVE 155)	;a user leaves

(defconst YODEL_SERVICE_CHAT_EXIT 160)	;you get logged out (?)

;;sent every half hour to prevent being logged out of chat.
(defconst YODEL_SERVICE_CHAT_HEARTBEAT 161)

(defconst YODEL_SERVICE_CHAT_MESSAGE 168)

(defconst YODEL_SERVICE_WEBLOGIN #x226)

;; these status messages are sent in the header status field
(defconst YODEL_STATUS_AVAILABLE (yodel-32 0))
(defconst YODEL_STATUS_TYPING (yodel-32 #x16))

;; these status messages are part of the data
;(defconst YODEL_STATUS_AVAILABLE_IM "0")
(defconst YODEL_STATUS_BRB "1")
(defconst YODEL_STATUS_BUSY "2")
(defconst YODEL_STATUS_NOTATHOME "3")
(defconst YODEL_STATUS_NOTATDESK "4")
(defconst YODEL_STATUS_NOTINOFFICE "5")
(defconst YODEL_STATUS_ONPHONE "6")
(defconst YODEL_STATUS_ONVACATION "7")
(defconst YODEL_STATUS_OUTTOLUNCH "8")
(defconst YODEL_STATUS_STEPPEDOUT "9")
(defconst YODEL_STATUS_INVISIBLE "12")
(defconst YODEL_STATUS_CUSTOM "99")
(defconst YODEL_STATUS_IDLE "999")

(defconst YODEL_YCHT_LOGON 1)
(defconst YODEL_YCHT_JOIN_ROOM 17)
(defconst YODEL_YCHT_EXIT_ROOM 18)
(defconst YODEL_YCHT_UNKNOWN 33)
(defconst YODEL_YCHT_MESSAGE 65)
(defconst YODEL_YCHT_THOUGHT 67)
(defconst YODEL_YCHT_BUDDY_STATUS 104)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; these next two are functions now because as strings they were
;;; being interpreted as multibyte and screwing up the login.

;; XXX fix this misleading name
(defun yodel-status-offline-im ()
  (concat [90] [85] [170] [86]))

(defun yodel-status-offline ()		;also used for web login
  (concat [90] [85] [170] [85]))
;(multibyte-string-p (yodel-status-offline))
;(multibyte-string-p (concat [90] [0] [170] [85]))

; /* Games don't fit into the regular status model */
(defconst YODEL_STATUS_GAME (yodel-32 #x2)) 
;(multibyte-string-p YODEL_STATUS_GAME)

(defconst YODEL_STATUS_OFFLINE_MSG (yodel-32 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string utilities

(defun yodel-get16 (buf pos)
  "convert a binary string to a number."
  (let ((buf (string-make-unibyte buf)))
    (logior (lsh (elt buf pos) 8) (elt buf (1+ pos)))))

(defun yodel-put16 (num)
  ;; for some reason, concatenating two vectors doesn't result in
  ;; a multibyte string, where format "%c%c" does
  (let ((a (logand 255 (lsh num -8)))
	(b (logand 255 num)))
    (concat (vector a) (vector b))))

;; (when nil
;;   ;; in emacs 21.1.1 and 21.2.1 these expressions return nil.
;;   ;; turning off multibyte characters doesn't fix this.
;;   (equal (concat (format "%c%c" 0 99) (format "%c%c" 99 230))
;; 	 (format "%c%c%c%c" 0 99 99 230))
;;   (equal (concat (format "%c" 0) (format "%c" 200))
;; 	 (concat [0] [200]))
;;   (yodel-get16 (concat (format "%c" 0) (format "%c" 200)) 0)
;;   ;;these are okay
;;   (yodel-get16 (concat [0] [200]) 0)
;;   (equal (concat (format "%c" 0) (format "%c" 100))
;; 	 (concat [0] [100]))
;;   (yodel-get16 (yodel-put16 231) 0)
;;   )

(defun yodel-get32 (buf pos)
  "a simpler idea. just use the string. no need to convert it."
  (substring buf pos (+ pos 4)))

(defun yodel-put32 (num)
  (cond ((stringp num) num)
	((integerp 4) (yodel-32 num))
	(t (error "unknown value in yodel-put32"))))

(def-yodel-test put/get ()
  (dolist (n '(0 1 2 3 10 20 100 200 168 254 1000 2000 10000 #xff01 65535))
    (assert (equal n (yodel-get16 (yodel-put16 n) 0))))
  (dolist (n '("zzzz" "~~~~"))
    (assert (equal n (yodel-put32 (yodel-get32 n 0))))))
;(yodel-run-test 'put/get)

(defun yodel-string-trim (string)
  "trim trailing whitespace"
  (yodel-replace-in-string "[ \t\r\n]+$" "" string))

(defun yodel-message-clean (msg)
  "the function that is called to clean up text before inserting it
into a chat buffer."
  ;; remove annoying yahelite tattoos
  ;;(setf msg (yodel-remove-tattoo msg))
  ;; remove some html tags
  (setf msg (yodel-dehtml msg))
  ;; remove escape sequences
  (setf msg (yodel-replace-in-string (format "%c[^m]*m" #o33) "" msg))
  ;; remove bogus colors from the spam
  (setf msg (yodel-replace-in-string "<#[a-fA-F0-9]\\{6,6\\}>" "" msg))
  ;; remove carriage returns
  (setf msg (delete* ?\r msg))
  (setf msg (yodel-string-trim msg))
  msg)

(defun yodel-incoming-message-clean (&rest foo)
  "called by message receieve hook"
  (yodel-packet-update "14" (yodel-message-clean (yodel-packet-ref "14"))))

;; not quite random HTML tags to remove from incoming messages. I have
;; no idea if this list is complete, but removing all <tags> doesn't
;; work.
(defvar yodel-dehtml-tag-regex)
(setq yodel-dehtml-tag-regex
  (concat "</?\\("
	  (regexp-opt
	   '("i" "b" "u" "br" "alt" "font" "face" "fade"
	     "black" "gray" "yellow" "pink"
	     "cyan" "blue" "orange" "red" "green" "purple"))
	  "\\)\\>[^<>]*>" ))

(defun yodel-dehtml (string)
  "remove some html formatting"
  (yodel-replace-in-string yodel-dehtml-tag-regex "" string ))

(defun yodel-dehtml-entities (string)
  (let* ((entities `(("&amp;" . "&") ("&apos;" . "'")
		     ("&gt;" . ">") ("&lt;" . "<")))
	 (regexp (regexp-opt (mapcar 'car entities) t)))
    (yodel-replace-in-string regexp
			     (lambda (s) (or (cdr (assoc s entities)) s))
			     string)))

(defun yodel-dehtml-entities-old (string)
  "convert some html entities to characters"
  (when string
    (yodel-replace-in-string "&amp;" "&"
      (yodel-replace-in-string "&apos;" "'"
	(yodel-replace-in-string "&gt;" ">"
	  (yodel-replace-in-string "&lt;" "<" string))))))
;(yodel-dehtml-entities "foo&apos;s bar &amp; grill")

(when (yodel-xemacs)
  (defalias 'yodel-dehtml-entities 'yodel-dehtml-entities-old))

(defun yodel-remove-whitespace (string)
  (yodel-replace-in-string "[ \t\r\n]+" "" string))

(defun yodel-pairs (&rest data)
  (do ((data data (cddr data))
       (acc () (acons (first data) (second data) acc)))
      ((null data) (nreverse acc))))

(defun yodel-hash-table-keys (ht)
  "return a list of keys for ht"
  (let ((acc ()))
    (maphash (lambda (k v) (push k acc)) ht)
    acc))

(defun yodel-version-message ()
  (let ((e-ver (if (yodel-xemacs)
		  (format "XEmacs %s" (car (split-string emacs-version)))
		(format "GNU Emacs %s" emacs-version))))
    (format "is using yod.el %s and %s \t%s"
	    (yodel-version) e-ver yodel-home-page)))
;(yodel-version-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; creating and accessing timers

(defvar yodel-timers nil)

(defvar yodel-timer-minimum-interval 30)

(defun yodel-timer-exists (symbol)
  ;; XXX check that it's actually running?
  (member symbol yodel-timers))

(defun yodel-make-timer (start interval symbol)
 "create a timer to run the function named SYMBOL every INTERVAL
seconds. stores the timer in the alist yodel-timers so it can be
canceled later."
 (yodel-kill-timer symbol)
 (if (< interval yodel-timer-minimum-interval)
     (error "yodel-make-timer: interval is too short")
   (let ((timer (run-at-time start interval symbol)))
     (push (cons symbol timer) yodel-timers))))

(defun yodel-kill-timer (symbol)
  "find the timer associated with SYMBOL and cancel it."
  (let ((timer (cdr (assoc symbol yodel-timers))))
    (when timer
      ;; stop the timer, then remove it from the list
      (cancel-timer timer)
      (setq yodel-timers
	    (remove-if (lambda (p) (eq symbol (car p)))
		       yodel-timers)))))

(defun yodel-pop-timer ()
  "kill the first timer. (that is the one added most recently.)"
  (yodel-kill-timer (caar yodel-timers)))
;(yodel-pop-timer)

(defun yodel-kill-all-timers ()
  (dolist (timer yodel-timers t)
    (yodel-kill-timer (car timer))))

(defun yodel-time-since (time)
  "return the number of seconds since TIME"
  (when time
    (let ((now (current-time)))
      (+ (ash (- (car now) (car time)) 16)
	 (- (cadr now) (cadr time))))))

(defun yodel-current-clock-minutes ()
  ;;Thu Sep 25 22:07:36 2003
  (let ((s (current-time-string)))
    (let ((pos (string-match ":[0-9][0-9]:" s)))
      (if pos
	  (car (read-from-string (substring s (+ 1 pos) (+ 3 pos))))
	(error "time string didn't match in yodel-current-clock-mintes")
	))))
;(yodel-current-clock-minutes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reading tcpdump output (development tools)

(defvar yodel-current-packet nil)

(defun yodel-packet-at-point ()
  "parse the packet at point and stuff it in the debug buffer"
  (interactive)
  (let* ((string (buffer-substring-no-properties (point) (+ (point) 20)))
	 (type (yodel-packet-type string))
	 (headlen (if (= type YODEL_YMSG) 20 16))
	 (datalen (if (= type YODEL_YMSG)
		      (yodel-packet-length-ymsg string)
		    (yodel-packet-length-ycht string))))
    ;;(y-or-n-p (format "%s %s %s" type headlen datalen))
    (setf yodel-current-packet
	  (yodel-packet-parse
	   (buffer-substring-no-properties
	    (point)
	    (+ (point) headlen datalen))))))
;(global-set-key "\C-cyp" 'yodel-packet-at-point)
;yodel-current-packet

(defun yodel-packet-at-point-ycht ()
  (let ((p (yodel-packet-at-point)))
    (and p
	 (or (equal 256 (yodel-packet-ver p))
	     (equal 174 (yodel-packet-ver p)))
	 (zerop (yodel-packet-flags p))
	 p)))

(defun yodel-dump-data (buffer-name)
  "debugging tool"
  (interactive "Bselect a buffer that will be overwritten: ")
  (let ((buffer (get-buffer-create buffer-name))
	(case-fold-search nil))
    (with-current-buffer buffer (erase-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (posix-search-forward "\\(\\(GET \\|YCHT\\|YMSG\\).*\\)" nil t)
	(let ((string (yodel-match-string 1))
	      (thing nil)
	      (limit 400))
	  (cond ((equal "GET" (substring string 0 3))
		 (setf thing string))
		(t
		 (goto-char (match-beginning 1))
		 (setf thing (format "%s" (yodel-packet-at-point)))
		 (forward-char 1)))
	  (when (< limit (length thing))
	    (setf thing (substring thing 0 limit)))
	  (with-current-buffer buffer
	    (insert "\n\n" (format "%s" thing))))))))

(defun yodel-log-ymsg (buffer)
  (interactive "BBuffer: ")
  (let ((buffer (get-buffer-create buffer))
	(case-fold-search nil))
    (with-current-buffer buffer (erase-buffer))
    (while (posix-search-forward "\\(YMSG\\|YCHT\\)" nil t)
      (backward-char 4)
      (let ((p (if (equal "YCHT" (yodel-match-string 1))
		   (yodel-packet-at-point-ycht)
		 (yodel-packet-at-point))))
	(with-current-buffer buffer
	  (let ((s (prin1-to-string p)))
	    (insert "\n\n"
		    (if (> 250 (length s))
			s
		      (concat (substring s 0 200) "...")))))
	(forward-char 1)))
    (switch-to-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authentication

(defun yodel-auth-tokens (name pass seed)
  "return the cryptographic tokens to send in the login."
  (let ((pairs (yodel-crypt name pass seed)))
    (list (cdr (assoc "6" pairs))
	  (cdr (assoc "96" pairs)))))

(defun yodel-crypt (name pass seed)
  "interface to the ycrypt C program"
  (with-temp-buffer
    (insert (format "%s\n%s\n%s\n" name pass seed))
    (shell-command-on-region (point-min)
			     (point-max)
			     yodel-crypt-program
			     t)
    ;; (message (buffer-substring (point-min) (point-max)))
    (goto-char (point-min))
    (apply 'yodel-pairs (read (current-buffer)))))

(def-yodel-test yodel-crypt ()
  (yodel-crypt "foo" "bar" "baz"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the connection

(defvar yodel-session-id (yodel-32 0)
  "the server assigned session id")

(defvar yodel-incoming-packet nil
  "a reference to the current packet being processed")

(defvar yodel-pager-hosts nil)
(setf yodel-pager-hosts '("scs.msg.yahoo.com"
			  "scsa.msg.yahoo.com"
			  "scsb.msg.yahoo.com"
			  "scsc.msg.yahoo.com"))

(defvar yodel-pager-hosts-webmsg nil)
(setf yodel-pager-hosts-webmsg  (list "wcs2.msg.dcn.yahoo.com"))
;(setf yodel-pager-hosts-webmsg (list "jcs.chat.dcn.yahoo.com"))

;; what's the prefered port? does it matter?
(defvar YODEL_PAGER_PORT 5050) ;23)

(defvar yodel-http-port 80)

(defvar yodel-http-get-header nil
  "the last http header we retrieved, or nil on a new request")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web login - added in January 2004

(defun yodel-read-base16 (string start finish)
  (car (read-from-string (concat "#x" (substring string start finish)))))

(defun yodel-md5-array (string)
  "returns an 16 element array that is the binary representation of
the input string md5 hash. this was originally to help with testing."
  (let ((str (md5 string))
	(acc ()))
    ;; coerce to any array to help with testing
    (dotimes (i 16 (coerce (reverse acc) 'array))
      (push (yodel-read-base16 str (* i 2) (+ (* i 2) 2)) acc))))

(defun yodel-md5 (string)
  "returns a 16 character long string that is the md5 hash of string"
  ;; coerce doesn't seem to be affected by the concat bug
  (coerce (yodel-md5-array string) 'string))

(defun yodel-md5-hex (string)
  (mapconcat (lambda (c) (format "%02x" c))
	     (yodel-md5 string)
	     ""))

(defun yodel-md5-test (in out)
  ;; we test against -array to make it easier to test the binary strings
  (unless (equal (yodel-md5-array in) out)
    (error "%s != (yodel-md5 '%s')\n" out in)))

(when nil
  (progn
    (yodel-md5-test "" [212 29 140 217 143 0 178 4 233 128 9 152 236 248 66 126])
    (yodel-md5-test "a" [12 193 117 185 192 241 182 168 49 195 153 226 105 119 38 97])
    (yodel-md5-test "ab" [24 126 244 67 97 34 209 204 47 64 220 43 146 240 235 160])
    (yodel-md5-test "abc" [144 1 80 152 60 210 79 176 214 150 63 125 40 225 127 114])
    (yodel-md5-test "abcd" [226 252 113 76 71 39 238 147 149 243 36 205 46 127 51 31])
    (yodel-md5-test "abcde" [171 86 180 217 43 64 113 58 204 90 248 153 133 212 183 134])
    (yodel-md5-test "abcdefghijklmnopqrstuvwxyz"
		    [195 252 211 215 97 146 228 0 125 251 73 108 202 103 225 59])
    t))

(defun yodel-url-escape (string)
  (flet ((replacement (s) (format "%%%02x" (aref s 0))))
    (yodel-replace-in-string "[^A-Za-z0-9._-]" 'replacement string)))


(defun yodel-login-query-string (alist)
  (let ((passwd (cdr (assoc "passwd" alist))))
    (assert passwd)
    (let* ((hash1 (yodel-md5-hex passwd))
	   (challenge (cdr (assoc ".challenge" alist)))
	   (hash2 (concat hash1 challenge))
	   (hash (yodel-md5-hex hash2))
	   (url "?.md5=1&.js=1&.hash=1"))
      (dolist (pair alist url)
	(destructuring-bind (name . value) pair
	  (setf value (cond ((equal name "passwd") hash)
			    ((equal name "hasMsgr") "1")
			    ((equal name ".js") "")
			    ((equal name ".md5") "")
			    ((equal name ".hash") "")
			    (t value)))
	  (unless (equal "" value)
	    (setf url (concat url "&" name "=" (yodel-url-escape value)))))))))

(defun yodel-login-form-pairs ()
  "search the login page for key/value pairs to send to the login
server. returns an alist."
  (interactive)
  (and (posix-search-forward
	"<input type=hidden name=\"\\([^\"]*\\)\" value=\"\\([^\"]*\\)\" >"
	nil t)
       (acons (yodel-match-string 1)
	      (yodel-match-string 2)
	      (yodel-login-form-pairs))))

(defvar yodel-account nil
  "the account name we're connecting with. set in yodel-account.")

(defvar yodel-http-login-step nil)

(defun yodel-logon-webmsg ()
  (interactive)
  (setf yodel-account nil)
  (setf yodel-http-login-step 'yodel-http-login-step2)
  (message "requesting the login page")
  (yodel-http-login-connection "login.yahoo.com" "/config/login?.src=pg"))

(defun yodel-http-login-step2 ()
  ;; get form values from the login page and the user's name and password
  (let ((buffer (yodel-connection-buffer)))
    (setf yodel-http-login-step 'yodel-http-login-step3)
    (with-current-buffer buffer
      (goto-char (point-min))
      (let* ((alist (yodel-login-form-pairs))
	     (passwd (read-passwd "password: "))
	     (query (yodel-login-query-string
		     (append `(("login" . ,(yodel-account))
			       ("passwd" . ,passwd))
			     alist))))
	(message "sending login info")
	(yodel-http-login-connection "login.yahoo.com"
			       (concat "/config/login" query))
	))))

(defun yodel-http-login-step3 ()
  (let ((buffer (yodel-connection-buffer)))
    (with-current-buffer buffer
      (setf yodel-http-login-step nil)
      (yodel-extract-cookies)
      (let ((yodel-pager-hosts yodel-pager-hosts-webmsg))
	(yodel-connect)
	(yodel-send-weblogin)
	(yodel-set-packet-handler 'yodel-list-pending-handler)
	))))

(defun yodel-http-login-location ()
  (goto-char (point-min))
  (if (posix-search-forward "^Location: http://\\([^/]+\\)\\(/.*\\)\r$")
      (list (yodel-match-string 1) (yodel-match-string 2))
    (error "didn't find a server location")))

(defun yodel-http-login-connection (host page)
  "open a connection that is initially an http connection, but
that turns into an instant message session."
  (let ((buffer (yodel-connection-buffer)))
    (let ((proc (or (yodel-connection-process)
		    (open-network-stream "yodel-network-connection"
					 buffer
					 host
					 yodel-http-port))))
      (assert proc nil "connecting to host: %s" host)
      (with-current-buffer buffer
	(erase-buffer)
	(setq yodel-http-get-header nil)
	(yodel-set-process-coding-system proc 'binary 'binary)
	;;(set-process-filter proc 'yodel-http-login-filter)
	(set-process-sentinel proc 'yodel-http-login-sentinel)
	(process-send-string proc (yodel-http-get-request host page))
	buffer))))

(defun yodel-http-login-sentinel (proc desc)
  (when (functionp yodel-http-login-step)
    (funcall yodel-http-login-step)))

(defun yodel-extract-cookies ()
  ;; specific to the yahoo setup
  (interactive)
  (goto-char (point-min))
  (while (posix-search-forward "^Set-Cookie:\\(.*\\)$" nil t)
    (let* ((h (yodel-match-string 1))
	   (c (car (split-string h "[ :;]+")))
	   (p (and c (search "=" c))))
      ;;(print c)
      (and p
	   (yodel-update-cookie (substring c 0 p) (substring c (1+ p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; account values

(defvar yodel-account-defaults nil)

(defvar yodel-ycontent-filename "ycontent-values")

(defun yodel-account ()
  "return the value of yodel-account or, if it's nil, prompt for it"
  ;;(error "yodel-account")
  (or yodel-account
      (progn
	(setq yodel-account (read-string "Account Name: "))
	(yodel-account-load-values)
	yodel-account)))
;(yodel-account)

(defun yodel-login-info ()
  "prompt the user for login information"
  ;; XXX read-passwd is GNUemacs specific
  (interactive)
  (list (yodel-account) (read-passwd "Password: "))
  )
;(yodel-login-info)

(defun yodel-account-symbol ()
  "return a symbol for this yodel-account"
  (if yodel-account
      (intern (concat "yodel-account-" yodel-account))
    'yodel-account-defaults))

(defun yodel-account-value (symbol)
  "return the current account value for symbol"
  (if (not yodel-account)
      nil
    (or (get (yodel-account-symbol) symbol)
	(get 'yodel-account-defaults symbol))))

(defun yodel-account-value-set (symbol value)
  "store a value for this account"
  (when yodel-account
    (unless (equal value (yodel-account-value symbol))
      (put (yodel-account-symbol) symbol value)
      (yodel-account-save-values))))

(defsetf yodel-account-value yodel-account-value-set)

(defun yodel-account-save-values ()
  "save the account property values (ycontent times)"
  (when yodel-account
    (yodel-account-save yodel-ycontent-filename
			(symbol-plist (yodel-account-symbol))
			)))
;(yodel-account-save-values)

(defun yodel-account-load-values ()
  "load the account settings from the file. this should be called
before we send a ycontent request."
  (setf (symbol-plist (yodel-account-symbol))
	(yodel-account-load yodel-ycontent-filename)))
;(yodel-account-load-values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; saving and loading data

(defun yodel-account-path (filename)
  "return the full path to FILENAME for the current account"
  (when yodel-account
    (concat (yodel-account-data-dir) "/" filename)))
;(yodel-account-path "foo")

(defun yodel-account-save (filename data)
  "save lisp data DATA to a file named FILENAME in the user's data dir.
returns DATA so it can be used by the caller."
  (let ((buffer (find-file-noselect (yodel-account-path filename))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format ";; last update: %s\n" (current-time-string)))
      (pp data buffer)
      (save-buffer)
      (kill-buffer buffer)
      data)))

(defun yodel-account-load (filename)
  "load values from file named FILENAME in the account dir"
  (when (file-exists-p (yodel-account-path filename))
    (let ((buffer (find-file-noselect (yodel-account-path filename))))
      (prog1 (read buffer)
	(kill-buffer buffer)))))

(defun yodel-directory-p (path)
  "return true if PATH is a directory"
  (let ((attr (file-attributes path)))
    (and attr (eq t (car attr)))))

(defun yodel-create-dir (dir)
  "try to create director DIR. return DIR if successful."
  (cond ((null dir) nil)		;not signed in
	((yodel-directory-p dir) dir)	;directory exists
	((file-exists-p dir)
	 (error "file exists (in yodel-create-account-xml-dir)"))
	(t (make-directory dir t)
	   dir)))

(defun yodel-account-data-dir ()
  "the directory for the current accounts data storage.
creates it if necessary."
  (assert (yodel-directory-p yodel-data-directory))
  (when yodel-account
    (let ((name (yodel-replace-in-string "[/:\\ ]" "-" (yodel-account))))
      (yodel-create-dir
       (concat (directory-file-name yodel-data-directory) "/" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; event hooks

;;; closing a connection

(defvar yodel-disconnect-hook nil
  "hook to run when a connection is being shutdown")
(setq yodel-disconnect-hook nil)

;; a hook is a LIFO so order is important in this one
(add-hook 'yodel-disconnect-hook
	  (lambda () (yodel-status-message "disconnected")))

(add-hook 'yodel-disconnect-hook
	  (lambda ()
	    (unless yodel-debug-flag
	      (yodel-connection-buffer 'kill))))

(add-hook 'yodel-disconnect-hook 'yodel-delete-cookies)
(add-hook 'yodel-disconnect-hook 'yodel-kill-all-timers)

;;; entering chat mode

(defvar yodel-chat-mode-hook nil)
(setf  yodel-chat-mode-hook nil)

(add-hook 'yodel-chat-mode-hook 'yodel-chat-font-lock)

;;; making a new connection

(defvar yodel-connect-hook nil
  "hook to run after a connection is made, but before we logon.")
(setq yodel-connect-hook nil)

;; reset the session id
(add-hook 'yodel-connect-hook
	  (lambda () (setq yodel-session-id (yodel-32 0))))

;; force chatroom init
(add-hook 'yodel-connect-hook
	  (lambda () (setf yodel-chatroom-sent-init nil)))

;; tell us we're connected
(add-hook 'yodel-connect-hook
	  (lambda () (yodel-status-message "connected")))


;;; logon

(defvar yodel-logon-hook nil
  "hook to run after we successfully logon.")
(setq yodel-logon-hook nil)

;; create the instant message buffer
(add-hook 'yodel-logon-hook
	  (lambda () (switch-to-buffer (yodel-message-buddy-buffer nil))))

;; the splash ad gets downloaded once per session (or so it seems)
(add-hook 'yodel-logon-hook
	  (lambda () (setq yodel-chatroom-need-splash t)))

(add-hook 'yodel-logon-hook
	  (lambda () (yodel-status-message "logged on")))

(add-hook 'yodel-logon-hook 'yodel-http-get-ycontent)

(add-hook 'yodel-logon-hook
	  (lambda ()
	    (setq *yodel-buddies* (make-hash-table :test 'equal))))

;; change the font lock to use the latest id
(add-hook 'yodel-logon-hook 'yodel-chat-font-lock)

;;; sending and receiving IMs

(defvar yodel-message-send-hook nil)
(setq yodel-message-send-hook nil)

;; added 2003/11/13
(defvar yodel-im-receive-hook)
(setf yodel-im-receive-hook '(yodel-message-record-sender
			      yodel-incoming-message-notify
			      ;yodel-silly-unknown-sender
			      ))

;; added 2005/04/21
(defvar yodel-message-insert-hook ()
  "run when text is inserted into the instant message buffer
so you can add things like time stamps.")

;;; hook for various chatroom events

(defvar yodel-chatroom-join-hook)
(setq yodel-chatroom-join-hook nil)

(defvar yodel-chatroom-user-ignore-hook ()
  "called with a single arg -- the name of the message sender.")
;(setq yodel-chatroom-user-ignore-hook nil)
  
(defvar yodel-chatroom-message-receive-hook ()
  "called with a single arg -- the name of the message sender.")
(setq yodel-chatroom-message-receive-hook nil)

;; make sure the user is in the chatroom
(add-hook 'yodel-chatroom-message-receive-hook
	  'yodel-chatroom-add-occupant)

;; XXX this is now just 'yodel-message-send-hook
(defvar yodel-chatroom-message-send-hook)
(setq  yodel-chatroom-message-send-hook nil)

(defvar yodel-chatroom-user-join-functions)
(setf yodel-chatroom-user-join-functions nil)

(add-hook 'yodel-chatroom-user-join-functions
	  'yodel-chatroom-add-occupant)

(defvar yodel-chatroom-user-leave-hook)
(setq yodel-chatroom-user-leave-hook nil)

(defvar yodel-chatroom-exit-hook)
(setq yodel-chatroom-exit-hook nil)

(add-hook 'yodel-chatroom-exit-hook 'yodel-chatroom-stop-heartbeat)

(add-hook 'yodel-chatroom-exit-hook
	  (lambda ()
	    (yodel-kill-timer 'yodel-chatroom-subsequent-ads)))

(add-hook 'yodel-chatroom-exit-hook
	  (lambda ()
	    (setq yodel-chatroom-sent-init nil
		  yodel-current-chatroom nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packet sending and parsing

(defstruct yodel-packet sig ver flags length service status id data)

(defun yodel-packet-string-length (p)
  (+ (yodel-packet-length p)
     (if (equal "YCHT" (yodel-packet-sig p)) 16 20)))

(defun yodel-send-data (string)
  (let ((process (yodel-connection-process)))
    (cond ((null process)
	   (run-hooks 'yodel-disconnect-hook)
	   (error "no yodel process"))
	  ;((multibyte-string-p string)
	   ;(error "sending a multibyte string: %s" string))
	  (t (condition-case nil
		 (process-send-string process string)
	       (error (yodel-alert "error sending packet")))
	     (let ((p (yodel-packet-parse string)))
	       (yodel-packet-debug string)
	       (when p
		 (yodel-packet-debug p)))))))

;; (flet ((yodel-send-data (string) string)
;;        (yodel-packet-parse (string) (mapcar* 'identity string))
;;        (yodel-account () "testing"))
;;   (let ((yodel-current-chatroom "room:1"))
;;     (multibyte-string-p (yodel-chatroom-message-send "foo"))))

(defun yodel-send (&rest data)
  "send the packet data to yahoo."
  (with-current-buffer (yodel-connection-buffer)
    (yodel-send-data
     (yodel-packet-create-message
      (apply 'make-yodel-packet data)))))

(defun yodel-send-ycht (type &rest data)
  (with-current-buffer (yodel-connection-buffer)
    (yodel-send-data (yodel-ycht-string type data))))

(defun yodel-ycht-string (type data)
  (let ((data (with-output-to-string
		(do ((data data (cdr data)))
		    ((null data))
		  (princ (car data))
		  (when (cdr data)
		    (princ yodel-delim))))))
    (with-current-buffer (yodel-connection-buffer)
      (concat "YCHT"
	      (yodel-put32 174)
	      (yodel-put32 type)
	      (yodel-put16 0)
	      (yodel-put16 (length data))
	      data))))
;;(yodel-ycht-string 16 '("programming:1::122222"))

(defun yodel-split-data (string)
  "split incoming data into a list"
  ;; split-string doesn't work with yodel-delim
  (let ((acc ())
	(start 0)
	(end 0))
    (while (setf end (search yodel-delim string :start2 start))
      (push (substring string start end) acc)
      (setf start (+ end (length yodel-delim))))
    ;; ycht messages don't have a trailing delimiter
    (when (< start (length string))
      (push (substring string start) acc))
    (nreverse acc)))

(defun yodel-packet-type (string)
  "identify the packet type"
  (let ((case-fold-search nil))
    (cond ((posix-string-match "^YCHT\\(.\\|\n\\)\\{12\\}" string)
	   YODEL_YCHT)
	  ((posix-string-match "^YMSG\\(.\\|\n\\)\\{16\\}" string)
	   YODEL_YMSG)
	  (t YODEL_UNKNOWN))))

(defun yodel-packet-parse (string)
  "parse incoming data and return a yodel-packet structure."
  (let ((type (yodel-packet-type string)))
    (cond ((= type YODEL_YCHT) (yodel-packet-parse-ycht string))
	  ((= type YODEL_YMSG) (yodel-packet-parse-ymsg string))
	  (t (message "bad packet in yodel-packet-parse: |%s|" string) nil))))

(defun yodel-packet-length-ymsg (string)
  (yodel-get16 string 8))

(defun yodel-packet-parse-ymsg (string)
  (let ((pktlen (yodel-packet-length-ymsg string))
	(hdrlen (yodel-packet-header-length YODEL_YMSG)))
    (cond ((< (length string) (+ pktlen hdrlen)) nil)
	  (t
	   (make-yodel-packet
	    :sig     (substring string 0 4)
	    :ver     (yodel-get16 string 4)
	    :flags   (yodel-get16 string 6)
	    :length  pktlen
	    :service (yodel-get16 string 10)
	    :status  (yodel-get32 string 12)
	    :id      (yodel-get32 string 16)
	    :data    (apply 'yodel-pairs
			    (yodel-split-data
			     (substring string hdrlen (+ hdrlen pktlen))))
	    )))))

(defun yodel-packet-length-ycht (string)
  (yodel-get16 string 14))

(defun yodel-packet-parse-ycht (string)
  (let ((pktlen (yodel-packet-length-ycht string))
	(hdrlen (yodel-packet-header-length YODEL_YCHT))
	(ver (+ (yodel-get16 string 4) (yodel-get16 string 6)))
	(service (+ (yodel-get16 string 8) (yodel-get16 string 10))))
    (if (< (length string) (+ pktlen hdrlen))
	nil
      (make-yodel-packet
       :sig     (substring string 0 4)
       :ver     ver
       :service service
       :flags   (yodel-get16 string 12)
       :length  (yodel-get16 string 14)
       ;; ignore YCHT logons that we send.
       ;; the length has another meaning
       :data    (unless (and (equal 174 ver) (equal 1 service))
		  ;;(mapcar (lambda (s) (split-string s ""))
		  (yodel-split-data (substring string 16)))
       ))))

(defun yodel-packet-debug (p)
  "format the packet for viewing in the yodel process buffer"
  (when yodel-debug-flag
    (yodel-connection-buffer-insert
     ";; " (current-time-string) "\n" (prin1-to-string p))))

(defun yodel-packet-data-to-string (p)
  (with-output-to-string
    (dolist (p (yodel-packet-data p))
      (princ (car p))
      (princ yodel-delim)
      (princ (cdr p))
      (princ yodel-delim))))

(defun yodel-packet-create-message (p)
  "create a message suitable to send to the server"
  (assert (yodel-packet-service p))
  (let ((contents (yodel-packet-data-to-string p)))
    (with-output-to-string
      (princ (or (yodel-packet-sig p) yodel-signature))
      (princ (yodel-put16 (or (yodel-packet-ver p) YODEL_PROTO_VER)))
      (princ (yodel-put16 (or (yodel-packet-flags p) YODEL_FLAGS)))
      (princ (yodel-put16 (length contents)))
      (princ (yodel-put16 (yodel-packet-service p)))
      (princ (yodel-put32 (or (yodel-packet-status p) YODEL_STATUS_AVAILABLE)))
      (princ (yodel-put32 (or (yodel-packet-id p) yodel-session-id)))
      (princ contents))))

(defun yodel-packet-status-error-p (pkt)
  (not (equal (yodel-32 1) (yodel-packet-status pkt))))

(defun yodel-packet-data-assoc (num pkt)
  (and (yodel-packet-p pkt)
       (assoc num (yodel-packet-data pkt))))

(defun yodel-packet-fetch (pkt num)
  "fetch a data element from a packet"
  (cdr (yodel-packet-data-assoc num pkt)))

(defun yodel-packet-ref (num &optional packet)
  "another way to retrieve packet data. if packet is nil, defaults
to using the packet in yodel-incoming-packet.
this should be combined with yodel-packet-fetch."
  (yodel-packet-fetch (or packet yodel-incoming-packet) num))

(defun yodel-packet-update (fieldnum value &optional packet)
  (let ((pair (yodel-packet-data-assoc
	       fieldnum (or packet yodel-incoming-packet))))
    (and pair (setf (cdr pair) value))))

(defun yodel-packet-hooker (fieldnum function &optional packet)
  (yodel-packet-update
   fieldnum
   (funcall function (yodel-packet-ref fieldnum packet))
   packet))

(defun yodel-packet-fetch* (pkt &rest numbers)
  "fetch a list of data elements from a packet"
  (loop for n in numbers
	collect (yodel-packet-fetch pkt n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buddy stuff

;;; operations for buddies:
;;;  - add a buddy
;;;  - remove a buddy
;;;  - update buddy group (later)
;;;  - view buddy status
;;;  - establish a session with a buddy
;;;  - send a buddy some sort of invitation (chat room or game)

;; store buddies by name.
;(setq *yodel-buddies* (ake-hash-table :test 'equal))
;(yodel-hash-table-keys *yodel-buddies*)

(defstruct yodel-buddy name group alias age location status status-string)

(defun yodel-change-buddy (name change)
  "either add or delete a buddy"
  (let ((buddy (yodel-buddy-find-or-make name)))
    (when buddy
      (yodel-send :service change
		  :data (yodel-pairs "1" (yodel-account)
				     "7" name
				     "65" (yodel-buddy-group buddy)))
      (yodel-status-message "buddy update sent"))))

(defun yodel-buddy-add ()
  "add a buddy to your buddy list"
  ;(interactive "senter a buddy name to add: ")
  (interactive)
  (let ((name (yodel-message-prompt-for-user "New buddy: ")))
    (and name (yodel-change-buddy name YODEL_SERVICE_ADDBUDDY))))

(defun yodel-buddy-del (&optional name)
  "remove a buddy from the buddy list"
  (interactive)
  (unless name
    (setf name (yodel-buddy-select "select a buddy to remove: ")))
  (yodel-change-buddy name YODEL_SERVICE_REMBUDDY))

(defun yodel-buddy-process-list (text)
  "convert the buddy list from the server's text format to objects"
  (when text
    (dolist (line (split-string text "\n"))
      (setq line (yodel-remove-whitespace line))
      (let ((buddies (split-string line "[:,]")))
	(destructuring-bind (group &rest buddies) buddies
	  (dolist (name buddies buddies)
	    (setf (gethash name *yodel-buddies*)
		  (make-yodel-buddy :name name
				    :group group
				    :status 'offline
				    ))))))))
;;(gethash "elisp_messenger" *yodel-buddies*)

(defun yodel-buddy-find (name)
  "look up a buddy by name."
  (cond ((yodel-buddy-p name) name)
	((stringp name) (gethash name *yodel-buddies*))))
;(yodel-buddy-find "flaxal")

(defun yodel-buddy-find-or-make (name)
  "returns an existing buddy object named 'name' or creates a new  one"
  (or (yodel-buddy-find name)
      (yodel-edit-buddy (make-yodel-buddy :name name :group "Buddies"))))

(defun yodel-edit-buddy (buddy)
  "TBD: edit a buddy and return the buddy object"
  (message "TBD")
  buddy)

(defun yodel-buddy-available-p (buddy)
  "return the availability of a buddy. input can be a buddy structure
or a string."
  (equal 'online (yodel-buddy-status (yodel-buddy-find buddy))))

(defun yodel-buddy-update-status (pkt status)
  "update buddy's status according to the info in PKT"
  ;; XXX this was screwing up packet processing
  (condition-case nil
      (yodel-buddy-update-status-orig pkt status)
    (error (progn
	     (message yodel-pending-text)
	     (message "error in yodel-buddy-update-status")
	     (setf yodel-pending-text "")))))

(defun yodel-buddy-update-status-orig (pkt status)
  "update buddy's status according to the info in PKT"
  (when (yodel-packet-data pkt)
    (let* ((name (yodel-packet-fetch pkt "7"))
	   (string (yodel-message-status-string pkt status))
	   (buddy (yodel-buddy-find name)))
      (setf (yodel-buddy-status buddy)
	    ;; the initial buddy update you get when you logon makes
	    ;; this cond necessary. maybe we should just go with
	    ;; strings for buddy status.
	    (cond ((equal "Idle" string) 'idle)
		  ((not (equal yodel-available string)) 'away)
		  (t status)))
      (setf (yodel-buddy-status-string buddy) string))))

(defun yodel-buddy-name-or-alias (buddy)
  (or (yodel-buddy-alias buddy)
      (yodel-buddy-name buddy)))

(defun yodel-online-buddies ()
  (interactive)
  (mapcan (lambda (b) (and (yodel-buddy-available-p b) (list b)))
	  (yodel-hash-table-keys *yodel-buddies*)))
;(yodel-buddy-online-buddies)
;(global-set-key "\C-cyo" 'yodel-buddy-online-buddies)

(defun yodel-buddy-status-list ()
  "display a list of buddies and their status"
  (let ((acc ()))
    (maphash (lambda (k v)
	       (push (concat k
			     "  "
			     (or (yodel-buddy-status-string v) "Offline"))
		     acc))
	     *yodel-buddies*)
    acc))
;(yodel-buddy-status-list)

(defun yodel-buddy-show-status-list ()
  (interactive)
  (yodel-info-list 'yodel-buddy-status-list "Buddies"))

(defun yodel-buddy-show-available-buddies ()
  (yodel-info-list 'yodel-online-buddies "Online Buddies"))

(defun yodel-buddy-list (&optional online)
  (if online
      (yodel-online-buddies)
    (yodel-hash-table-keys *yodel-buddies*)))

(defun yodel-buddy-select (prompt &optional online)
  (interactive)
  (completing-read prompt
		   (mapcar 'list (yodel-buddy-list online))
		   nil
		   t))

(defun yodel-name-p (name)
  "return true if NAME is a name looks like a valid username"
  (and name (not (equal "" name))))

(defun yodel-name-at-point ()
  (let ((name (thing-at-point 'symbol)))
    (and (yodel-name-p name) name)))

(defun yodel-buddy-at-point ()
  (let ((name (yodel-name-at-point)))
    (and (yodel-buddy-find name) name)))

(defvar yodel-message-senders nil
  "list of people that sent us messages.")

(defun yodel-message-record-sender (user &rest stuff)
  "maintains the list of people that sent us messages. we keep the
most recent sender at the head of the list for replies."
  (unless (equal user (car yodel-message-senders))
    (setf yodel-message-senders
	  (cons user (remove user yodel-message-senders)))))

;; all messages all get jamed up together, but you can create
;; buffers to chat with individuals. i just have a hard time
;; switching around when there are a lot.
(defvar yodel-message-buffer-name "*yodel-instant-messages*")

(defvar yodel-away-messages
  `(("Away from Emacs" ,YODEL_STATUS_CUSTOM)
    ("Be Right Back" ,YODEL_STATUS_BRB)
    ("Busy" ,YODEL_STATUS_BUSY)
    ("Coding" ,YODEL_STATUS_CUSTOM)
    ("Hacking yod.el" ,YODEL_STATUS_CUSTOM)
    ("Not At Home" ,YODEL_STATUS_NOTATHOME)
    ("Not At Desk" ,YODEL_STATUS_NOTATDESK)
    ("Not In Office" ,YODEL_STATUS_NOTINOFFICE)
    ("On The Phone" ,YODEL_STATUS_ONPHONE)
    ("On Vacation" ,YODEL_STATUS_ONVACATION)
    ("Out To Lunch" ,YODEL_STATUS_OUTTOLUNCH)
    ("Stepped Out" ,YODEL_STATUS_STEPPEDOUT)
    ("Invisible" ,YODEL_STATUS_INVISIBLE)
    ("Idle" ,YODEL_STATUS_IDLE )
    ))

(defun yodel-choose-away-message ()
  (let* ((message (completing-read "away message: "
				   yodel-away-messages nil))
	 (data (or (assoc message yodel-away-messages)
		   (list message YODEL_STATUS_CUSTOM)))
	 (code (cadr data)))
    (cond ((equal message "Invisible")
	   (yodel-pairs "10" code))
	  ((equal code YODEL_STATUS_CUSTOM)
	   (yodel-pairs "10" YODEL_STATUS_CUSTOM "19" message "47" "1"))
	  (t (yodel-pairs "10" code "47" "1")))))
;(yodel-choose-away-message)
	
(defun yodel-custom-away-message (message)
  ;; XXX make these persistent
  (interactive "sMessage: ")
  (unless (assoc message yodel-away-messages)
    (setf yodel-away-messages
	  (cons (list message YODEL_STATUS_CUSTOM)
		yodel-away-messages))))
;(yodel-custom-away-message "foo")

(defvar yodel-status-messages
  (mapcar 'reverse (cons `(,yodel-available "0") yodel-away-messages)))

(defun yodel-message-status-string (packet status)
  (case status
    (offline "Offline")
    (idle "Idle")
    (t (let ((code (yodel-packet-fetch packet "10")))
	 (cond ((equal code YODEL_STATUS_CUSTOM)
		(yodel-packet-fetch packet "19"))
	       (t (let ((status (assoc code yodel-status-messages)))
		    (if status
			(cadr status)
		      "Unknown"))))))))

(defun yodel-chat-buffer (buffer-name)
  (or (get-buffer buffer-name)
      (let ((buffer (generate-new-buffer buffer-name)))
	(with-current-buffer buffer
	  (yodel-chat-mode))
	buffer)))

(defun yodel-message-buffer-name (buddy-name)
  (if buddy-name
      (format "*yodel-messages* (%s)" buddy-name)
    yodel-message-buffer-name))

(defun yodel-message-buffer-p (buffer)
  (posix-string-match "^\\*yodel-" (if (bufferp buffer)
				       (buffer-name buffer)
				     buffer)))

(defun yodel-make-buddy-buffer (&optional buddy)
  "create a new buffer for messages to and from BUDDY.
if BUDDY is NIL, create the group IM buffer."
  (interactive)
  (unless (yodel-chatroom-buffer-p)
    (let ((buddy (or buddy
		     (yodel-name-at-point)
		     (yodel-message-prompt-for-user "Username: "))))
      (when buddy
	(with-current-buffer
	    (yodel-chat-buffer (yodel-message-buffer-name buddy))
	  (setf yodel-chat-buddy-name buddy)
	  (switch-to-buffer (current-buffer)))))))

(defun yodel-individual-chat-buffer-p ()
  "if the buffer has a buddy-name, it's an individual buffer"
  yodel-chat-buddy-name)

(defun yodel-info-make-buddy-buffer ()
  "let an info list create a new individual chat buffer"
  (interactive)
  (let ((name (yodel-buddy-at-point)))
    (cond (name
	   (yodel-make-buddy-buffer name)
	   (switch-to-buffer (yodel-message-buffer-name name))
	   (delete-other-windows))
	  (t (message "Did not find a buddy name.")))))

(defun yodel-message-buddy-buffer (buddy)
  "if buddy has a buffer, return it, or return the generic chat
buffer, or create and return the generic chat buffer."
  (or (get-buffer (yodel-message-buffer-name buddy))
      (get-buffer (yodel-message-buffer-name nil))
      (yodel-chat-buffer (yodel-message-buffer-name nil))))
;;(yodel-message-buddy-buffer "mac")

(defun yodel-alert (fmt &rest args)
  "try to ring the bell, but respect customizations."
  (unless yodel-supress-beep
    (beep))
  (apply 'message fmt args))

(defun yodel-incoming-message-notify (user &rest stuff)
  "run when an incoming instant message is received."
  (unless (or (minibuffer-window-active-p (minibuffer-window))
	      (equal (car (buffer-list)) (yodel-message-buddy-buffer user)))
    (yodel-alert "received IM from %s" user)))

(defun yodel-message-fill-paragraph (text)
  "wrap message (using 'fill-paragraph) text depending on the
content and the value of yodel-wrap-message-text"
  ;; added 2004/04/23 to handle fill problems
  (cond ((find ?\n text :test 'eql) (insert "\n")) ;possibly formatted, leave it alone
	(yodel-wrap-message-text
	 (fill-paragraph nil)
	 (unless (= ?\n (char-before)) (insert "\n")))
	(t (insert "\n"))))
;(setq yodel-wrap-message-text t)

(defun yodel-message-insert-text (user text &optional from-me)
  ;; XXX decouple this from the user you're sending stuff to
  (let ((buffer (yodel-message-buddy-buffer user)))
    (with-current-buffer buffer
      (let ((follow (equal (point) (point-max))))
	(save-excursion
	  (goto-char (point-max))
	  (run-hook-with-args 'yodel-message-insert-hook user text)
	  (insert "\n"
		  (cond ((and from-me (yodel-individual-chat-buffer-p))
			 (yodel-account))
			(from-me (format "(to %s)" user))
			(t user))
		  ": " text)
	  (yodel-message-fill-paragraph text))
	(if t ;follow			;always following now
	    (goto-char (point-max)))))))

(defvar yodel-message-last-recipient nil
  "the last person we sent an IM too")

(defun yodel-message-set-last-recipient (name)
 (setf yodel-message-last-recipient (list name (current-time))))

(defun yodel-message-last-recipient ()
 "i've been sending a lot of messages to the wrong people. this
function is an effort to fix that by limiting the lifetime of the
yodel-message-last-recipient variable."
 (when yodel-message-last-recipient
   (destructuring-bind (name time) yodel-message-last-recipient
       (if (> (yodel-time-since time) (* 5 60))
           ()
         name))))

(defun yodel-message-send-im (user text &optional do-not-record-recipient)
  (when (yodel-name-p user)
    (let ((yodel-current-message text))
      (unless (or (yodel-individual-chat-buffer-p) do-not-record-recipient)
	(yodel-message-set-last-recipient user))
      (run-hooks 'yodel-message-send-hook)
      ;;(yodel-packet-debug yodel-message-send-hooks)
      (yodel-message-insert-text user yodel-current-message t)
      (let ((data `(("1" . ,(yodel-account))
		    ("5" . ,user)
		    ("97" . "1")
		    ("14" . ,yodel-current-message))))
	(when (equal YODEL_YMSG YODEL_PROTO_VER)
	  (setf data (append data `(("63" . "")
				    ("64" . "2")
				    ("1002" . "1")))))
	(yodel-send :service YODEL_SERVICE_MESSAGE
		    :id (and (equal 101 YODEL_PROTO_VER) (yodel-32 0))
		    :status (yodel-status-offline-im)
		    :data data)
	;; XXX try to remove this
	(when (equal 101 YODEL_PROTO_VER)
	  (yodel-send-typing user))
	))))

(defun yodel-message-send-im-to-user (name)
  "prompt for text when we know the username."
  (yodel-message-send-im
   name
   (yodel-message-prompt-for-text (format "Message (to %s) " name))))

(defun yodel-send-typing (user)
  "send the typing notice"
  (yodel-send :service YODEL_SERVICE_NOTIFY
	      :status YODEL_STATUS_TYPING
	      :id (and (equal 101 YODEL_PROTO_VER) (yodel-32 0))
	      :data `(("49" . "TYPING")
		      ("1" . ,(yodel-account))
		      ("14" . " ")
		      ("13" . "1")
		      ("5" . ,user)
		      ("1002" . "1"))))
;(let ((yodel-account "foo")) (yodel-send-typing "bar"))

(defvar yodel-message-history nil
  "history of the messages we've sent")

(defun yodel-message-prompt-for-text (&optional prompt)
  (let ((msg (read-string (or prompt "Message: ")
			  nil 'yodel-message-history)))
    (and msg
	 (not (equal "" msg))
	 msg)))

(defvar yodel-message-user-prompt-history nil
  "holds names of users we've IMed for 'completing-read")
;(setq yodel-message-user-prompt-history nil)

(defun yodel-message-prompt-for-user (&optional prompt)
  (let* ((prompt (or prompt "User: "))
	 (history
	  (append (mapcar 'list yodel-message-user-prompt-history)
		  (mapcar 'list yodel-message-senders)
		  (mapcar 'list (yodel-online-buddies))))
	 (user (completing-read prompt history)))
    (pushnew user yodel-message-user-prompt-history :test 'equal)
    user))
;(yodel-message-prompt-for-user)

(defun yodel-sent-message-to-user-p (name)
  "return true if we've sent an instant message to name in this session."
  (member name yodel-message-user-prompt-history))
;(setq  yodel-message-user-prompt-history nil)
;(pop yodel-message-user-prompt-history)

(defun yodel-message-prompt-and-send ()
  (interactive)
  (let ((user (or yodel-chat-buddy-name (yodel-message-prompt-for-user))))
    (yodel-send-typing user)
    (let ((text (yodel-message-prompt-for-text)))
      (yodel-message-send-im user text)
      ;; sending a message to someone, switches the buffer on you
      (switch-to-buffer (yodel-message-buddy-buffer user)))))

(defun yodel-status-message (message)
  "display status in the main IM buffer"
  (let ((msg-and-time (format "%s  (%s)" message (current-time-string))))
    (yodel-message-insert-text "** status" msg-and-time)
    (yodel-connection-buffer-insert msg-and-time)
    (message message)))

(defun yodel-send-heartbeat ()
  (yodel-send :service YODEL_SERVICE_HEARTBEAT
	      :data `(("0" . ,(yodel-account)))))
;(yodel-send-heartbeat)

(defun yodel-send-logoff ()
  (with-current-buffer (yodel-connection-buffer)
    (yodel-send :service YODEL_SERVICE_LOGOFF
		:data (yodel-pairs "1" (yodel-account)))
    (yodel-status-message "logged off")))

(defun yodel-send-isaway ()
  (interactive)
  (if (equal 101 YODEL_PROTO_VER)
      (message "away messages are broken in the web messenger protocol")
    (let ((data (yodel-choose-away-message)))
      (yodel-send :service YODEL_SERVICE_ISAWAY
		  ;;:id (and (equal 101 YODEL_PROTO_VER) (yodel-32 0))
		  :data data)
      (yodel-status-message
       (yodel-message-status-string
	(make-yodel-packet :data data) 'away)))))

(defun yodel-send-isback ()
  (interactive)
  (yodel-send :service YODEL_SERVICE_ISBACK)
  (yodel-status-message "Active"))
;(yodel-send-isback)


(defun yodel-send-auth-request ()
  (yodel-send :service YODEL_SERVICE_AUTH
	      :status (yodel-32 0) ;YODEL_STATUS_OFFLINE
	      :id (yodel-32 0)
	      :data `(("1" . ,(yodel-account)))))

(defun yodel-send-empty-packet ()
  (yodel-send :service YODEL_SERVICE_EMPTY_PACKET
	      :status (yodel-32 0)
	      :id (yodel-32 0)))
;(yodel-send-empty-packet)

(defun yodel-send-ping ()
  ;; proto 101 ping
  (interactive)
  (yodel-send :service YODEL_SERVICE_PING
	      :status (yodel-32 0)
	      :id (yodel-32 0)
	      :data nil))

(defun yodel-send-username/password (seed)
  "send username and password information"
  (destructuring-bind (username password) (yodel-login-info)
    (destructuring-bind (passhash crypthash)
	(yodel-auth-tokens username password seed)
      (yodel-send :service YODEL_SERVICE_AUTHRESP ;84
		  :status (yodel-status-offline)
		  :id (yodel-32 0)
		  :data  (yodel-pairs
			  "0" username
			  "6" passhash
			  "96" crypthash
			  ;;"2" "1"
			  "1" username
			  ;; some stuff from version 11
			  ;"135" "5, 6, 0, 1347"
			  ;"148" "300"
			  ;; XXX if we know these, we should send them
			  ;;"59" Y-Cookie
			  ;;"59" T-Cookie
			  )))))

(defun yodel-send-weblogin ()
  (yodel-send :service YODEL_SERVICE_WEBLOGIN
	      :status (yodel-status-offline)
	      :id (yodel-32 0)
	      :data (yodel-pairs "0" (yodel-account)
				 "1" (yodel-account)
				 "6" (yodel-make-cookie-partial "Y" "T"))))

(defun yodel-send-ping-response (&rest args)
  ;; XXX these things may not be related to a ping
  (yodel-send-passthrough)
  (yodel-send-imv-data)
  ;; this is where we get a bunch of stuff
  ;(yodel-http-get-ycontent)
  ;(yodel-http-get-feed-quotes)
  )

(defun yodel-send-passthrough ()
  ;; seems like the version 10 protocol didn't do this
  (yodel-send :service YODEL_SERVICE_PASSTHROUGH2
	      :data `(("1" . ,(yodel-account))
		      ;; XXX do we really need to send this stuff?
		      ("25" . "C=0F=1,P=0,C=0,H=0,W=0,B=0,O=0,G=0M=1,P=0,C=0,S=0,L=2,D=0,N=0,G=0,F=0,T=0")
		      ;; let's try skipping these...
		      ;;("146" . "V2luZG93cyBYUA==")
		      ;;("145" . "SW50ZWwgUGVudA==")
		      ;; this one seems to change. the others don't.
		      ;;("147" . "RWFzdGVybiBTdGFuZGFyZCBUaW1l")
		      )))

(defun yodel-send-imv-data ()
  (yodel-send :service YODEL_SERVICE_IMV_DATA
	      :data '(("26" . "IMV,[none]=2,0,0,0,0,0,0;cingular=0,0,0,0,0,0,1;colgate=0,0,0,0,0,0,1;fifa=0,0,0,0,0,0,1;holes=0,0,0,0,0,0,1;polo=0,0,0,0,0,0,1;"))
	      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server side ignores

(defvar yodel-ignore-list ())

(defun yodel-process-ignore-list (string)
  "process the list of ignored names the server sends when we log in"
  ;; thanks to r_muthuswamy for finding this one. 2004/03/15.
  (when string				
    (setf yodel-ignore-list (split-string string ","))))

(defun yodel-server-ignore-list ()
  "display a list of user names that we are ignoring"
  (interactive)
  (yodel-info-list yodel-ignore-list "Ignore List"))

(defun yodel-send-ignore (user type)
  "add or remove USER from the ignore list (depending on TYPE)"
  (when (yodel-name-p user)
    (yodel-send :service YODEL_SERVICE_IGNORE
		:data (yodel-pairs "1" (yodel-account)
				   "7" user
				   "13" type))))

(defun yodel-send-add-ignore ()
  "tell the server you want to ignore a user"
  (interactive)
  (let ((user (yodel-message-prompt-for-user "Ignore User: ")))
    (if (yodel-buddy-find user)
	(message "can't ignore buddies")
      (yodel-send-ignore user "1"))))

(defun yodel-send-del-ignore ()
  "remove someone from your ignore list."
  (if (not yodel-ignore-list)
      (message "no one to unignore")
    (let ((user (completing-read "Unignore user: "
				 (mapcar 'list yodel-ignore-list)
				 nil t)))
      (yodel-send-ignore user "2"))))

(defun yodel-request-service-list (&rest ignore)
  "request a service list. includes your buddy and ignore lists."
  (yodel-send :service YODEL_SERVICE_LIST
	      :data `(("1" . ,(yodel-account)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yodel-unibyte-buffer (name &optional kill)
  "return a buffer named NAME suitable for a yodel connection,
creating it if necessary. if KILL is true, kill the buffer."
  (let ((buffer (get-buffer name)))
    (cond (kill (and buffer (kill-buffer name)))
	  (buffer buffer)
	  (t (let ((buffer (generate-new-buffer name)))
	       (with-current-buffer buffer
		 (yodel-set-buffer-multibyte nil))
	       buffer)))))

(defun yodel-connection-buffer-insert (&rest text)
  "insert TEXT into the connection buffer."
  (with-current-buffer (yodel-connection-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert "\n\n")
      (map nil 'insert text))))

(defun yodel-connection-buffer (&optional kill)
  "return the connection buffer"
  (yodel-unibyte-buffer "<yodel-connection-buffer>" kill))

(defun yodel-connection-process ()
  (get-buffer-process (yodel-connection-buffer)))

(defun yodel-open-connection ()
  "try opening a network connection to a yodel host.
try each host in turn. return the connection, or nil."
  (let ((conn nil))
    (dolist (host yodel-pager-hosts conn)
      (unless conn
	(message "trying %s" host)
	(condition-case nil
	    (setf conn (open-network-stream "yahoo-connection"
					    (yodel-connection-buffer)
					    host
					    YODEL_PAGER_PORT))
	  (error nil))))))

(defun yodel-connect ()
  "connect to the yodel server and send a request to authenticate"
  (let ((conn (yodel-open-connection)))
    (unless conn
      (error "could not establish a yodel connection"))
    ;; aha! coding systems! found on 2003/09/24.
    (yodel-set-process-coding-system conn 'binary 'binary)
    (set-process-filter conn 'yodel-connection-filter)
    (set-process-sentinel conn 'yodel-connection-sentinel)
    (run-hooks 'yodel-connect-hook)))

(defun yodel-connection-filter (process string)
  "process incoming data from the yodel connection"
  (with-current-buffer (yodel-connection-buffer)
    ;(if yodel-debug-flag (yodel-connection-buffer-insert string))
    (setq yodel-pending-text
	  (yodel-message-start (concat yodel-pending-text string)))
    (if yodel-debug-flag (yodel-packet-debug yodel-pending-text))
    (let ((pkt (yodel-packet-parse yodel-pending-text)))
      ;; yodel-packet-parse returns null when string is too short
      (when pkt
	(yodel-packet-handler pkt)
	(setq yodel-pending-text
	      (substring yodel-pending-text (yodel-packet-string-length pkt)))
	;; process any remaining input
	(unless (string= "" yodel-pending-text)
	  (yodel-connection-filter process ""))
	))))
;(length yodel-pending-text)

(defun yodel-advance-packet-processing ()
  "advance the pending buffer by one character so we can find the next message"
  (unless (equal "" yodel-pending-text)
    (setq yodel-pending-text (substring yodel-pending-text 1))))

(defun yodel-message-start (string)
  "find the Yodel signature ('YMSG') in string and return the
substring starting there."
  (let* ((case-fold-search nil)
	 (start (posix-string-match "\\bY\\(CHT\\|MSG\\)" string)))
    (cond ((not start) "")
	  ((zerop start) string)
	  (t (substring string start)))))

(defun yodel-connection-sentinel (proc desc)
  "watch for changes in the yodel process state."
  ;; if we can determine that we've been cut off, try to reestablish
  ;; the connection.
  ;(message desc)
  (run-hooks 'yodel-disconnect-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yodel-logon-ymsg ()
  (when (yodel-connection-process)
    ;; if there's a connection, kill it, then try again
    (yodel-disconnect))
  (setq yodel-account nil)	;force account prompt
  (yodel-connect)
  (yodel-set-packet-handler 'yodel-auth-pending-handler)
  (yodel-send-empty-packet)
  (yodel-send-auth-request)	;asks for a seed
  ;; 2003/10/25, we don't need to check for updates
  ;;(yodel-http-get-updates)
  )

(defvar yodel-heartbeat-interval 60
  "number of seconds between heartbeat sends")
(setq yodel-heartbeat-interval (* 1 60))

(defun yodel-stop-heartbeat ()
  (yodel-kill-timer 'yodel-send-heartbeat))
;(yodel-stop-heartbeat)

(defun yodel-start-heartbeat-ymsg ()
  (yodel-make-timer yodel-heartbeat-interval
		    yodel-heartbeat-interval
		    'yodel-send-heartbeat))

(defun yodel-start-heartbeat-webmsg ()
  (yodel-make-timer 300 300 'yodel-send-ping))

(defun yodel-start-heartbeat ()
  (unless (yodel-timer-exists 'yodel-send-heartbeat)
    (if (equal YODEL_YMSG YODEL_PROTO_VER)
	(yodel-start-heartbeat-ymsg)
      (yodel-start-heartbeat-webmsg))))

;(yodel-start-heartbeat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yodel packet handlers
;;; we handle packets differently depending on the logon state

(defvar yodel-packet-handler nil
  "the current packet handler")

(defun yodel-packet-handler (pkt)
  "sende the incoming packet to the current packet handler"
  (setq yodel-incoming-packet pkt)
  (yodel-packet-debug pkt)
  (run-hooks 'yodel-incoming-packet-hook)
  (when yodel-packet-handler
    (funcall yodel-packet-handler pkt)))

(defun yodel-set-packet-handler (func)
  "change the function that handles incoming packets"
  (setf yodel-packet-handler func))

(defun yodel-auth-pending-handler (pkt)
  "this handler is active while we're waiting for the auth seed"
  (cond ((equal YODEL_SERVICE_AUTH (yodel-packet-service pkt))
	 (if (yodel-process-auth pkt)
	     (yodel-set-packet-handler 'yodel-list-pending-handler)
	   (error "unsuccessful logon (1)")))
	;(t (yodel-logon)) 
	(t ;(yodel-alert "unexpected packet (1)")
	   )))

(defun yodel-list-pending-handler (pkt)
  "we've sent our auth tokens and are waiting for confirmation"
  (cond ((equal YODEL_SERVICE_LIST (yodel-packet-service pkt))
	 ;; we're in
	 (yodel-set-packet-handler 'yodel-online-handler)
	 (run-hooks 'yodel-logon-hook)
	 (yodel-packet-handler pkt))
	(t
	 (error "unsuccessful logon (2)"))))

(defun yodel-online-handler (pkt)
  "the most common packet handler. runs after successful login."
  (let ((proc
	 (cond ((equal "YCHT" (yodel-packet-sig pkt))
		(yodel-get-proc-ycht (yodel-packet-service pkt)))
	       (t (yodel-get-proc (yodel-packet-service pkt))))))
    (when proc
      (funcall proc pkt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packet processors

(defun yodel-process-message (pkt)
  "check incoming messages for errors, if clean, send them on."
  (cond ((equal (yodel-packet-status pkt) YODEL_STATUS_OFFLINE_MSG)
	 (yodel-process-message-noerr pkt))
	((yodel-packet-status-error-p pkt)
	 (yodel-process-message-error pkt))
	(t (yodel-process-message-noerr pkt))))

(defun yodel-process-message-noerr (pkt)
  "add an incoming message to the chat buffer"
  ;;XXX what did yodel-serv-got-im do?
  ;;XXX check the field number for from here
  (let ((sender (yodel-packet-fetch pkt "4"))
	(text (yodel-packet-fetch pkt "14")))
    (yodel-message-insert-text sender (yodel-message-clean text))
    (run-hook-with-args 'yodel-im-receive-hook sender text)))

(defvar yodel-auto-response-receivers nil)

(defun yodel-auto-response (sender text)
  "since those yahoo's broke ymsg 11, we do our own away message."
  (unless (member sender yodel-auto-response-receivers)
    (push sender yodel-auto-response-receivers)
    (yodel-message-send-im
     sender
     (format "Sorry, %s, I'm not available right now, but those Yahoos won't let me change my status."
	     sender)
     t)))

(defun yodel-toggle-auto-response ()
  (interactive)
  (cond ((member 'yodel-auto-response yodel-im-receive-hook)
	 (setf yodel-im-receive-hook
	       (remove 'yodel-auto-response yodel-im-receive-hook)))
	(t (setf yodel-auto-response-receivers nil)
	   (add-hook 'yodel-im-receive-hook 'yodel-auto-response))))
;;(yodel-toggle-auto-response)

(defun yodel-process-message-error (pkt)
  ;; a message packet with an error status means one of your messages
  ;; couldn't be sent.
  (yodel-message-insert-text
   "(error sending message to "
   (format "%s)" (yodel-packet-fetch pkt "4"))))

(defun yodel-process-auth (pkt)
  "process a login request"
  (let ((seed (yodel-packet-fetch pkt "94")))
    (cond ((not seed) nil)
	  ;; some kind of failure?
	  ((yodel-packet-fetch pkt "66") nil)
	  (t (yodel-send-username/password seed)
	     t))))

(defun yodel-process-service-list (pkt)
  ;; receiving this means we got a successful logon
  (setq yodel-session-id (yodel-packet-id pkt))
  (yodel-start-heartbeat)
  (yodel-buddy-process-list (yodel-packet-fetch pkt "87"))
  (yodel-process-ignore-list (yodel-packet-fetch pkt "88"))
  (yodel-process-cookies pkt))

(defun yodel-process-cookies (pkt)
  "process the cookies from a service list packet"
  (dolist (p (yodel-packet-data pkt))
    (when (equal "59" (car p))
      (let ((cookie (cdr p)))
	(destructuring-bind (letter value &rest stuff)
	    (split-string cookie "[\t;]")
	  (yodel-update-cookie letter value))))))

(defun yodel-process-update-status (pkt status)
  "update a buddy's status"
  (yodel-buddy-update-status pkt status))

(defun yodel-process-update-online (pkt)
  "packets with a service setting of online are handled differently
because we get a special one that includes a list of our buddies that
are currenlty online when we first logon."
  (do ((data (yodel-packet-data pkt) (cdr data)))
      ((null data) nil)
    (when (equal "7" (caar data))		;the buddy name
      (yodel-process-update-status
       (make-yodel-packet :data data)
       'online))))

(defun yodel-process-status (pkt)
  "process incoming status messages"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process incoming messages

(defvar yodel-process-functions (make-hash-table))

(defun define-yodel-proc (service-id function &optional comment)
  "associate FUNCTION with SERVICE-ID"
  (setf (gethash service-id yodel-process-functions) function))

(defun yodel-get-proc (service-id)
  "return the processor for SERVICE-ID"
  (gethash service-id yodel-process-functions))

;; authentication
(define-yodel-proc YODEL_SERVICE_AUTH 'yodel-process-auth)
(define-yodel-proc YODEL_SERVICE_LIST 'yodel-process-service-list)

;; ping seems to be part of logging in
(define-yodel-proc YODEL_SERVICE_PING 'yodel-send-ping-response)

;; users coming and going
(define-yodel-proc YODEL_SERVICE_LOGON
  (lambda (pkt) (yodel-process-update-online pkt)))

(define-yodel-proc YODEL_SERVICE_LOGOFF
  (lambda (pkt) (yodel-process-update-status pkt 'offline)))

(define-yodel-proc YODEL_SERVICE_ISAWAY
  (lambda (pkt) (yodel-process-update-status pkt 'away)))

(define-yodel-proc YODEL_SERVICE_ISBACK
  (lambda (pkt) (yodel-process-update-status pkt 'online)))

(define-yodel-proc YODEL_SERVICE_IDLE
  (lambda (pkt) (yodel-process-update-status pkt 'idle)))

;(define-yodel-proc YODEL_SERVICE_USERSTAT 'yodel-proces-status)

;; instant messages
(define-yodel-proc YODEL_SERVICE_MESSAGE 'yodel-process-message)

;; 15 - server sends after an add or delete buddy
(define-yodel-proc YODEL_SERVICE_NEWCONTACT 'yodel-request-service-list)

;; 17 - server sends after an add or delete ignore
(define-yodel-proc YODEL_SERVICE_ADDIGNORE 'yodel-request-service-list)

;; chatroom messages (definitions in yodel-chatroom.el)
;; 150
(define-yodel-proc YODEL_SERVICE_CHAT_INIT 'yodel-chatroom-select-and-join)
;; 152
(define-yodel-proc YODEL_SERVICE_CHAT_JOIN 'yodel-chatroom-user-join)
;; 155
(define-yodel-proc YODEL_SERVICE_CHAT_LEAVE 'yodel-chatroom-user-leave)
;; 160
(define-yodel-proc YODEL_SERVICE_CHAT_EXIT 'yodel-chatroom-got-exit)
;; 168 - a message in the chatroom
(define-yodel-proc YODEL_SERVICE_CHAT_MESSAGE 'yodel-chatroom-message-receive)

;; 32 - an instant (private) message from a chat room
(define-yodel-proc YODEL_SERVICE_CHATMSG 'yodel-process-message)

(define-yodel-proc YODEL_SERVICE_NEWMAIL
  (lambda (pkt)
    (let ((from (yodel-packet-fetch pkt "42")))
      (when from
	(message "new mail from <%s>" from)))))


(defun yodel-get-proc-ycht (service-id)
  ;;(case service-id
  ;;)
  'yodel-chatroom-insert-ycht)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XML tools

;;; Yahoo provides some data to the user in xml format. It is tagged
;;; with a content time. The latest times are sent to the server as
;;; part of the initial ycontent request. The data is retrieved as
;;; updates are available. That is, on the first request, we get
;;; everything, but on subsequent requests, only those nodes that
;;; have changed since our last connection are sent.

;;; We only really care about chat categories. Filters, IMV, System,
;;; etc. are all specific to Yahoo's application.

(defun yodel-xml-parse-file (file)
  "make sure there are not carriage returns in FILE (which seems to
mess up xml.el, then parse it and return the data."
  (let ((buffer (find-file-noselect file)))
    (yodel-xml-parse-buffer buffer 'save 'close)))
;(yodel-xml-parse-file (yodel-account-path "chatroom_1600047754"))

(defun yodel-replace-string (from to)
  (while (search-forward from nil t)
    (replace-match to nil t)))

(defun yodel-xml-parse-buffer (buffer &optional savep closep)
  (with-current-buffer buffer
    ;; work arounds for some issues with xml.el
    (goto-char (point-min))
    (yodel-replace-string "\r" "")
    (goto-char (point-min))
    (yodel-replace-string "=\"\"" "=\".\"")
    (yodel-xml-delete-comments buffer)
    (let ((xml (xml-parse-region (point-min) (point-max))))
      (when savep (save-buffer))
      (when closep (kill-buffer buffer))
      (yodel-fix-xml xml)
      )))

(defun yodel-xml-delete-comments (buffer)
  ;; 20050525 - yahoo has started putting comments after the XML.
  ;; xml.el thinks that's invalid. as long as it is just comments,
  ;; we'll just delete them
  (goto-char (point-min))
  (while (posix-search-forward "<!--" nil t)
    (backward-char 4)
    (let ((start (point)))
      (if (re-search-forward "-->")
	  (delete-region start (point))
	(error "unterminated xml comment")))))
  
(defun yodel-xml-node-name (xml)
  "a function to wrap around the xml-node-name macro (for mapping)."
  (cond ((not (consp xml)) ())
	(t (xml-node-name xml))))

(defun yodel-xml-content (xml)
  "the xml seems to get an extra list layer. this just removes it and
returns the data."
  (cond ((not (consp xml)) ())
	((consp (xml-node-name xml))
	 (yodel-xml-content (car xml)))
  	((eq 'content (xml-node-name xml)) xml)
	(t nil)))
;(yodel-xml-content yodel-chatrooms)

(defun yodel-xml-content-names (xml)
  "return the content categories in this update.
for now, we only care about chatCategories."
  (mapcar 'yodel-xml-node-name
	  (xml-node-children (yodel-xml-content xml))))
;(yodel-xml-content-names yodel-chatrooms)

(defun yodel-xml-content-time (xml)
  "return the content time for this update."
  (xml-get-attribute (yodel-xml-content xml) 'time))
;(yodel-xml-content-time yodel-chatrooms)

(defun yodel-update-account-values (xml)
  "update all the content times in this xml. this is called by
yodel-xml-process whenever we download ycontent."
  (let ((time (yodel-xml-content-time xml)))
    (mapcar (lambda (c)
	      (setf (yodel-account-value c) time)
	      (cons c time))
	    (yodel-xml-content-names xml))))
;(yodel-update-account-values yodel-chatrooms)

(defun yodel-fix-xml (xml)
  ;; added 2004/02/22 - newer versions of xml.el seem to cause problems
  "convert extra whitespace to nils so they don't break the xml tools"
  (cond ((null xml) ())
	((stringp xml) ())
	((yodel-list-p xml) (mapcar 'yodel-fix-xml xml))
	(t xml)))

(defun yodel-list-p (thing)
  "because the built-in listp returns true on '(1 . 2)"
  (cond ((null thing) t)
	((not (consp thing)) nil)
	(t (let ((c (cdr thing)))
	     (while (consp c)
	       (setq c (cdr c)))
	     (null c)))))
;(yodel-list-p '(1 . 2))
;(yodel-list-p '(1 2))
;(yodel-list-p ())

(defun yodel-xml-content-node (xml node-name)
  ;; return the content node(s) in XML named NODE-NAME.
  (xml-get-children (yodel-xml-content xml) node-name))
;(yodel-xml-content-node yodel-chatrooms 'chatCategories)
;(xml-get-children (yodel-xml-content yodel-chatrooms) 'chatRooms)
;(yodel-xml-content-node yodel-chatrooms 'chatRooms)

(defun yodel-xml-chat-categories (xml)
  "returns the list of chat categories if they exist in XML."
  (let ((categories (yodel-xml-content-node xml 'chatCategories)))
    (when categories
      (xml-node-children
       (if (consp (xml-node-name categories))
	   (car categories)
	 categories)))))
;(yodel-xml-chat-categories yodel-chatrooms)

(defun yodel-update-chat-categories (xml)
  "save new chat categories from XML to a file, or load existing
categories"
  (let ((categories (yodel-xml-chat-categories xml))
	(category-file "chat-categories"))
    (setq yodel-chat-categories
	  (if (not categories)
	      (yodel-account-load category-file)
	    (yodel-account-save category-file categories)))))
;(yodel-update-chat-categories yodel-chatrooms)

(defun yodel-xml-process-ycontent (buffer &rest flags)
  "called when ycontent is downloaded from the server. for now,
it just extracts and saves chat categories. flags get passed to
yodel-xml-parse-buffer."
  ;; 1. parse the XML
  (let ((xml (apply 'yodel-xml-parse-buffer buffer flags)))
    ;; 2. update ycontent node values
    (yodel-update-account-values xml)
    ;; 3. update chat categories
    (yodel-update-chat-categories xml)))
;(yodel-xml-process-ycontent "ycontent")


;; yodel-cookies is updated when we get a successful login.  these
;; appear in the order they were sent to the server  because i'm not
;; sure if that matters.

(defvar yodel-messenger-update-host "update.messenger.yahoo.com")

(defvar yodel-insider-host "insider.messenger.yahoo.com")

;; this is also where the ads come from
(defvar yodel-chat-host "chat.yahoo.com")

(defvar yodel-http-user-agent
  (format "Mozilla/4.01 [en] (emacs/%s)" emacs-version))
;; we'll go back to the "emacs" agent when we know it's not causing
;; the disconnect.
(setq yodel-http-user-agent
      "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; .NET CLR 1.0.3705)")

(defvar yodel-embedded-user-agent
  "Mozilla Compatible/2.0 (WinNT; I; NCC/2.0)")

(defvar yodel-http-cache-control "no-cache")

(defvar yodel-http-connection-close "close")

(defvar yodel-http-get-request)
(setq yodel-http-get-request "GET %s HTTP/1.1\r
Accept: */*\r
Accept-Language: en-us\r
User-Agent: %s\r
Host: %s\r
Cache-Control: %s\r
Connection: %s\r
%s\r\n"
      )

(defvar yodel-cookie-ingredients
;;(setq yodel-cookie-ingredients
  `(
    ;; trying with only Y and T
    ;("B" . "3ves0rcvffpgo&b=2")
    ;("Q" . "q1=AACAAAAAAAAAeg--&q2=Pvcimg--")
    ("Y")
    ("T")
    ("C" . "mg=1")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ugh. all this http crap needs to migrate to url.el

(defun yodel-http-get-request (host page)
  "returns the http GET header."
  (let ((cookie (yodel-make-cookie host)))
    (unless (string= "" cookie)
      (setq cookie (concat "Cookie: " cookie "\r\n")))
    (format yodel-http-get-request page
	    yodel-http-user-agent
	    host
	    yodel-http-cache-control
	    yodel-http-connection-close
	    cookie
	    )))
;(insert (yodel-http-get-request "yahoo.com" "/a/ne/bar"))

(defvar yodel-http-get-finish (make-hash-table :test 'equal)
  "holds clean up forms to be called when yodel-http-get finishes")

(defun yodel-http-get-sentinel (proc event)
  "a process sentinel for the web page retrieval. to keep it from
inserting the exit status (which always seems to be 256.)"
  ;; this doesn't actually close the process correctly,
  ;; it just prevents the error from being stored in the buffer,
  ;; but now we can process the page.
  ;(message "%s %s" (process-name proc) event)

  ;; XXX should check for redirects and content length.
  (let* ((buffer (process-buffer proc))
	 (cleanup (gethash buffer yodel-http-get-finish)))
    (remhash buffer yodel-http-get-finish)
    (unwind-protect
	(dolist (func cleanup) (funcall func buffer))
      (delete-process proc))))

(defun yodel-http-insert (proc string)
  (with-current-buffer (process-buffer proc)
    (if (plusp (buffer-size))
	(insert string)
      (let ((pos (search "\r\n\r\n" string)))
	(cond ((not pos) (insert string))
	      (t (setq yodel-http-get-header (substring string 0 pos))
		 (insert (substring string (+ 4 pos)))))))))

(defun yodel-http-get (buffname host page &rest cleanup-funcs)
  "Make an http connection to HOST and retrieve PAGE into a buffer
named BUFFNAME. If BUFFNAME is a buffer, use that. Clears the buffer
before retrieving the page. Returns the buffer."
  (let ((buffer (get-buffer-create buffname)))
    (yodel-packet-debug (list 'yodel-http-get host page))
    (let ((proc (open-network-stream (concat "http-get-" host)
				     buffer
				     host
				     yodel-http-port)))
      (assert proc nil "connecting to host: %s" host)
      (with-current-buffer buffer
	(setf (gethash buffer yodel-http-get-finish) cleanup-funcs)
	(erase-buffer)
	(setq yodel-http-get-header nil)
	(yodel-set-process-coding-system proc 'binary 'binary)
	(set-process-filter proc 'yodel-http-insert)
	(set-process-sentinel proc 'yodel-http-get-sentinel)
	(process-send-string proc (yodel-http-get-request host page))
	;(accept-process-output)
	buffer))))
;(yodel-http-get "google" "www.google.com" "/")
;yodel-http-get-header
;(list-processes)

(defun yodel-get-yodel ()
  "get the latest yod.el"
  (interactive)
  (destructuring-bind (host page) (yodel-parse-url yodel-home-page)
    (let ((page (yodel-replace-in-string "[^/]+$" "yod.el"  page)))
      (yodel-http-get "yod.el" host page))))

(defun yodel-make-cookie-partial (&rest ingredients)
  (let ((yodel-cookie-ingredients
	 (mapcar (lambda (c) (assoc c yodel-cookie-ingredients))
		 (reverse ingredients))))
    (yodel-make-cookie)))

(defun yodel-make-cookie (&optional host)
  ;; XXX changed on 2003/10/01 to check for host
  (if (and host (not (string-match "\\.yahoo\\.com$" host)))
      ""
    (with-output-to-string
      ;; we push onto a list to store cookies, and we try to maintain
      ;; that order when we send them back. not sure if it matters.
      (do ((list (reverse yodel-cookie-ingredients) (cdr list)))
	  ((null list))
	(when (cdar list)
	  (princ (format "%s=%s" (caar list) (cdar list)))
	  (princ (if (cdr list) "; " "")))))))
;(yodel-make-cookie)

(defun yodel-update-cookie (cookie value)
  (let ((c (assoc cookie yodel-cookie-ingredients)))
    (if c
	(setf (cdr c) value)
      (push (cons cookie value) yodel-cookie-ingredients)
      ;;(message "cookie does not exist: %s %s" cookie value)
      )))
;(yodel-update-cookie "Y" "2")
;(yodel-update-cookie "X" "2")
;yodel-cookie-ingredients

(defun yodel-delete-cookies ()
  ;(setq yodel-cookie-ingredients `(("Y") ("T") ("C")))
  (setq yodel-cookie-ingredients nil))

;(yodel-delete-cookies)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; requests sent as part of logging on

(defun yodel-embedded-http-get (&rest args)
  "retrieve a page with the embedded browser and reduced cookies"
  (let ((yodel-http-user-agent yodel-embedded-user-agent))
    (apply 'yodel-http-get args)))

(defun yodel-http-get-updates ()
  "send the two requests to the update host"
  (yodel-http-get-msgrcli)
  (yodel-http-get-components))

(defun yodel-http-get-msgrcli ()
  ;; the first page requested as we logon.
  ;; this is sent before we have the "Y" and "T" cookies from YMSG.
  (yodel-embedded-http-get "yodel-update-msgrcli.html"
			   yodel-messenger-update-host
			   "/msgrcli.html"
			   'kill-buffer))

(defun yodel-http-get-components ()
  ;; the second page requested when we logon.
  ;; the contents looks like an INI file.
  ;; this is sent before we have the "Y" and "T" cookies from YMSG.
  (yodel-embedded-http-get "yodel-update-components.html"
			   yodel-messenger-update-host
			   "/pgdownload/components.html"
			   ;'kill-buffer
			   ))
;(yodel-http-get-components)

;; account specific, persistent values
(put 'yodel-account-defaults 'filters "0")
(put 'yodel-account-defaults 'imvironments "0")
(put 'yodel-account-defaults 'system "0")
(put 'yodel-account-defaults 'sms "0")
(put 'yodel-account-defaults 'chatCategories "0")
(put 'yodel-account-defaults 'addressbook "0")
;;(yodel-account-value 'ycontent-filter)

(defun yodel-http-get-ycontent ()
  ;; this is the third page we retrieve.
  ;; this request is made after successful logon.

  ;; the numeric values in the URL seem to represent the time of the
  ;; latest version of the parameter they're associated with.  the
  ;; client is notified of updates, downloads them, and changes the
  ;; value.
  (let ((buffer (find-file-noselect (yodel-account-path "ycontent"))))
    (yodel-http-get
     buffer
     yodel-insider-host
     (format "/ycontent/?&filter=%s&imv=%s&system=%s&sms=%s&chatcat=%s&ab2=%s&intl=us&os=win"
	     (yodel-account-value 'filters)
	     (yodel-account-value 'imvironments)
	     (yodel-account-value 'system)
	     (yodel-account-value 'sms)
	     (yodel-account-value 'chatCategories)
	     (yodel-account-value 'addressbook))
     'yodel-xml-process-ycontent
     (lambda (b) (with-current-buffer b (save-buffer)))
     'kill-buffer)
    ))
;(yodel-http-get-ycontent)

(defun yodel-http-get-feed-quotes ()
  ;; i doubt this one is necessary (that's why i included it :)
  (yodel-http-get "yodel-stock-quotes"
		  "pgq.yahoo.com"
		  "/feed/pg4?s=quotes"
		  'kill-buffer))

(put 'yodel-account-defaults 'chatroom-id "0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trying to support yahoo advertising

(defvar yodel-yahoo-needs-our-support t
  "if true, we pull ads. if not, we don't")
(setq  yodel-yahoo-needs-our-support t)

(defvar yodel-chatroom-ad-interval 240
  "how often we request ads")

(defun yodel-chatroom-selection-ad ()
  "retrieve the ad from the chatroom selection box"
  (yodel-http-get "yodel-chat-html"
		  yodel-chat-host
		  "/c/msg/chat.html"
		  'yodel-http-get-images))
;;does this need to happen periodically?
;(yodel-chatroom-selection-ad)

(defun yodel-chatroom-splash-ad ()
  (when yodel-chatroom-need-splash
    (yodel-http-get "yodel-splash-ad"
		    yodel-chat-host
		    (format "/c2/msg/trans.html?id=%s&intl=us"
			    (yodel-chatroom-current-room-id))
		    'yodel-http-get-images)
    (setq yodel-chatroom-need-splash nil)))


(defun yodel-chatroom-ad-link (&optional params)
  "return the link to the first advertisement."
  (concat "http://" yodel-chat-host "/c/admsg.html?"
	  (if params (concat params "&") "")
	  ".rmspace=" (yodel-account-value 'chatroom-id) "&"
	  ".rmcat=" (yodel-chatroom-current-category-id)))
;(yodel-chatroom-ad-link ".rf=1")

(defun yodel-chatroom-first-ad ()
  ;; another attempt to pull ads from yodel as a sort of keep alive.
  ;; this one gets the big ad when you first join a room.
  ;; this one probably needs to display for a couple seconds before
  ;; you join the room.
  (yodel-http-get "yodel-chatroom-admsg.html"
		  yodel-chat-host
		  (concat "/c/admsg.html?.rmspace="
			  (yodel-chatroom-current-room-id)
			  "&.rmcat="
			  (yodel-chatroom-current-category-id)
			  )))
;yodel-timers
;(yodel-make-timer 0 300 'yodel-chatroom-subsequent-ads)

(defun yodel-chatroom-subsequent-ads ()
  "another attempt to pull ads from yodel as a sort of keep alive.
this one gets the ads at the bottom of the chatroom window, and
runs from a timer."
  ;; XXX for some reason, canceling this timer doesn't seem to work
  ;(message "in yodel-chatroom-subsequent-ads")
  (when (yodel-chatroom-current-room-id)
    (yodel-http-get
     "yodel-chatroom-admsg.html"
     yodel-chat-host
     (concat "/c/admsg.html?.rf=1&.rmspace="
	     (yodel-chatroom-current-room-id)
	     "&.rmcat="
	     (yodel-chatroom-current-category-id))
     ;; 2003/11/06 - see if we stay connected without getting images
     ;; 2003/12/21 - turned it back on to see if we can make it past 1:00
     'yodel-http-get-images
     )))

(defun yodel-http-src-urls ()
  "return a list of src urls in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((acc ()))
      ;; XXX watch this, it was updated on 2003/10/25
      (while (posix-search-forward
	      "\\(<[ \t\r\n]?\\(img\\|script\\)[ \t\r\n][^<>]*>\\)"
	      nil t)
	(let ((url (yodel-http-img-src 
		    (buffer-substring (match-beginning 1) (match-end 1)))))
	  (and url (push url acc))))
      (reverse acc))))

(defun yodel-http-img-src (img-tag)
  ;; could be an "img" or a "script" tag
  ;; returns a list for mapcan
  (when (string-match "\\<src[ \t\r\n]*=[ \t\r\n]*\"?\\([^\"<> \t\r\n]*\\)"
		      img-tag)
    (substring img-tag (match-beginning 1) (match-end 1))
    ;;(yodel-match-string 1)
    ))
;;(yodel-http-img-src "<img src=\"foo/bar.gif\">")

(defvar yodel-http-image-urls (make-hash-table :test 'equal))

(add-hook 'yodel-chatroom-exit-hook
	  (lambda ()
	    (setf yodel-http-image-urls (make-hash-table :test 'equal))))

(defun yodel-image-needed-p (url)
  ;; we don't really want images
  (cond ((posix-string-match "\\.js$" url) t)
	((not yodel-http-get-images) nil)
	((equal "" url) nil)
	((and (gethash url yodel-http-image-urls)
	      (> (* 60 60)		;get images once an hour
		 (yodel-time-since (gethash url yodel-http-image-urls))))
	 nil)
	(t (setf (gethash url yodel-http-image-urls) (current-time))
	   t)))

(defun yodel-http-get-images (buffer)
  "get each of the images with a src attribute in the current buffer"
  (with-current-buffer buffer
    (let ((urls (yodel-http-src-urls)))
      (dolist (u urls)
	(when (yodel-image-needed-p u)
	  (let ((host/page (yodel-parse-url u)))
	    (when (cdr host/page)
	      (message u)
	      (apply 'yodel-http-get "yodel-image-buffer" host/page))))))))

(defun yodel-parse-url (url)
  "extract hostname and page from url"
  ;; time to get url.el
  (when (string-match "http://\\([^/]+\\)\\(/.*\\)" url)
    (list (substring url (match-beginning 1) (match-end 1))
	  (substring url (match-beginning 2) (match-end 2)))))
;(yodel-parse-url "http://foo.com/bar/baz/quux.html")
;(yodel-parse-url "")

(defun yodel-chatroom-advertising ()
  (when yodel-yahoo-needs-our-support
    ;; XXX put some info about the ad (like a title or link)
    ;; XXX info into the chatroom buffer.
    (yodel-chatroom-splash-ad)
    (yodel-chatroom-first-ad)
    (yodel-make-timer yodel-chatroom-ad-interval
		      yodel-chatroom-ad-interval
		      'yodel-chatroom-subsequent-ads)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chatroom support

(defvar yodel-xml-host "insider.msg.yahoo.com"
  "the web host")

;; is this one used?
(defvar yodel-message-max-size 256)	;XXX what's the real limit?

;; these should probably be in yodel-defs (and use defconst)
(defvar yodel-chatroom-message-sender "109")
(defvar yodel-chatroom-join-error "114")
(defvar yodel-chatroom-message-text "117")
(defvar yodel-chatroom-message-room-name "104")

(defvar yodel-chatroom-buffer-name "*yodel-chat*")

(defvar yodel-chatroom-occupants nil
  "denizens of the current chatroom.
value is nil, or a hash table of name.")

(defun yodel-chatroom-buffer ()
  (yodel-chat-buffer yodel-chatroom-buffer-name))

(defun yodel-chatroom-buffer-p ()
  "returns true if the current buffer is a chatroom buffer.
used to tell an instant message buffer from a chatroom buffer."
  (interactive)
  (equal (buffer-name) yodel-chatroom-buffer-name))

(defun yodel-chatroom-fix-name (name)
  (cond ((equal "" name) "")
	((equal ?* (aref name 0)) name)
	(t (yodel-replace-in-string " " "-" name))))
;;(yodel-chatroom-fix-name "* fo")
;;(yodel-chatroom-fix-name " fo")

(defun yodel-chatroom-insert (user message &optional status-message)
  "insert the MESSAGE from USER into the current chatroom buffer"
  ;; insert at the end of the buffer, but don't follow the
  ;; text unless point is at the end (unless we're sending)
  (with-current-buffer (yodel-chatroom-buffer)
    (let ((follow (or (= (point-max) (point))
		      (equal user yodel-account)))
	  (message (yodel-message-clean message)))
      (save-excursion
	(goto-char (point-max))
	(cond ((and user status-message)
	       (insert (format "\n%s %s" user message)))
	      (user (insert (format "\n%s: %s"
				    (yodel-chatroom-fix-name user)
				    message)))
	      (t (insert (format "\n\nNOTICE - %s\n"  message))))
	(yodel-message-fill-paragraph message))
      (when follow
	(goto-char (point-max))))))
;(global-set-key "\C-cym" (i-lambda () (message "%s %s" (point) (point-max))))

(defun yodel-chatroom-insert-ycht (pkt)
  (with-current-buffer (yodel-chatroom-buffer)
    (insert "\n\n" (format "%s" pkt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sending a message

(defvar yodel-current-message "")

(defun yodel-update-current-message (function)
  (setq yodel-current-message (funcall function yodel-current-message)))

(defun yodel-chatroom-message-limit-length (string)
  "make sure the current message isn't longer than max size"
  (if (> (length string) yodel-message-max-size)
       (substring string 0 yodel-message-max-size)
    string))

(defun yodel-chatroom-message-send (message &optional status-message)
  "send MESAGE to the current chatroom.
MESSAGE may be modified by a hook function."
  ;; this MESSAGE variable is free
  (when yodel-current-chatroom
    (let ((yodel-current-message message))
      (run-hooks 'yodel-message-send-hook)
      ;; insert after run-hooks so we can see what we're sending
      (when (and yodel-current-message
		 (not (string= "" yodel-current-message)))
	(unless status-message
	  (setq yodel-current-message
		(concat yodel-tattoo yodel-current-message)))
	(yodel-chatroom-insert (yodel-account) message status-message)
	(yodel-send :service YODEL_SERVICE_CHAT_MESSAGE
		    :data (yodel-pairs "1" (yodel-account)
				       "104" yodel-current-chatroom
				       "117" yodel-current-message
				       ;; message type 1 = normal, 2 = ?
				       "124" (if status-message "2" "1")))
	))))
;yodel-message-send-hook

(defun yodel-chatroom-message-receive (pkt)
  "process an incoming chatroom message. the message text can be
modified by a hook."
  ;; there are a couple different types of message
  ;; ("124" . "1") - a normal message
  ;; ("124" . "2") - a status message (or thought?)
  (let ((name (yodel-packet-fetch pkt yodel-chatroom-message-sender))
	(message (yodel-packet-fetch pkt yodel-chatroom-message-text)))
    (setf message (yodel-message-clean message))
    (let ((yodel-current-message message))
      (cond ((yodel-chatroom-ignore-p name message)
	     (when (yodel-chatroom-ignore-user-p name)
	       (run-hook-with-args 'yodel-chatroom-user-ignore-hook name)))
	    (t
	     (run-hook-with-args 'yodel-chatroom-message-receive-hook name)
	     (yodel-chatroom-insert name
				    yodel-current-message
				    (yodel-chatroom-status-message-p pkt)))))))

(defun yodel-chatroom-status-message-p (pkt)
  (equal "2" (yodel-packet-fetch pkt "124")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; entering and leaving a chatroom

;; joining a chatroom has a number of steps

;; 1. send a YODEL_SERVICE_CHAT_INIT (150)
;; 2. receive a chat init from the server
;; 3. choose a chat category (and a subcategory if present)
;; 4. retrieve rooms for category
;; 5. choose a chat room
;; 6. send a YODEL_SERVICE_CHAT_JOIN (152)
;; 7. receive a YODEL_SERVICE_CHAT_JOIN (152) with list of roomies

(defvar yodel-chatroom-first-join t
  "true when we join the first chatroom for this session.
triggers the splash ad and heartbeat timer.")

(defun yodel-join-chatroom ()
  "the command that starts off the whole process of joining a room."
  (interactive)
  ;; we keep track if an init was sent, because we don't need to
  ;; send another one to switch rooms.
  (if yodel-chatroom-sent-init
      (yodel-chatroom-select-and-join)
    (yodel-chatroom-send-init)))

(defun yodel-chatroom-send-init ()
  "prep the server for accessing chatrooms.
we send the room selection after receiving a YODEL_SERVICE_CHAT_INIT
from the server. seems like this only has to happen once."
  (yodel-send :service YODEL_SERVICE_CHAT_INIT
	      :data (yodel-pairs
		     yodel-chatroom-message-sender (yodel-account)
		     "1" (yodel-account)
		     "6"  "abcde"
		     "98" "us"
		     ;"135" "ym5, 6, 0, 1347"
		     "135" "ym6,0,0,1922" ;2005/03/21
		     ))
  (setq yodel-chatroom-sent-init t
	yodel-chatroom-first-join t))

(defun yodel-chatroom-current-room-id ()
  (yodel-account-value 'chatroom-id))

(defvar yodel-chatroom-advertising-delay 5
  "seconds to wait for ads to display before joining room.")

;; 2004/06/09
;; trying without the advertising delay.
;; their new version doesn't seem to wait.
(defun yodel-chatroom-join-chatroom-ymsg (id name lobby room-id)
  ;; start ads here because they need room and category info
  (cond (yodel-chatroom-first-join
	 ;;(message (format "%d second advertising delay..."
	 ;;  yodel-chatroom-advertising-delay))
	 (yodel-chatroom-advertising)
	 (yodel-chatroom-start-heartbeat)
	 (setf yodel-chatroom-first-join nil)
	 ;;(run-at-time yodel-chatroom-advertising-delay nil
	 ;;  'yodel-chatroom-finish-join id name lobby room-id))
	 ;;(t
	 ))
  (yodel-chatroom-finish-join id name lobby room-id))

(defun yodel-chatroom-join-chatroom-ycht (id name lobby room-id)
  (when yodel-chatroom-first-join
    (yodel-send-ycht YODEL_YCHT_LOGON)
    (setf yodel-chatroom-first-join nil))
  ;; may need to send a /chat/cmd?cmd=join;args=:::
  (yodel-send-ycht YODEL_YCHT_JOIN_ROOM name lobby "" room-id))

(defun yodel-chatroom-join-chatroom (id name lobby room-id)
  ;; set yodel-current-chatroom to nil until you get the list
  (setq yodel-current-chatroom nil
	yodel-chatroom-occupants (make-hash-table :test 'equal))
  (setf (yodel-account-value 'chatroom-id) room-id)
  (switch-to-buffer (yodel-chatroom-buffer))
  (cond ((= YODEL_PROTO_VER 101)
	 (yodel-chatroom-join-chatroom-ycht id name lobby room-id))
	(t
	 (yodel-chatroom-join-chatroom-ymsg id name lobby room-id))))
 
(defun yodel-chatroom-finish-join (id name lobby room-id)
  (yodel-send :service YODEL_SERVICE_CHAT_JOIN
	      :data (yodel-pairs
		     "1" (yodel-account)
		     "62" "2"
		     "104" (concat name ":" lobby)
		     "129" id
		     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chatroom heartbeat
;;; 2005/04/08 happens at the top and bottom of the hour
;;; old - happens twice an hour on  a minute divisible by 15.

;;; XXX got right of the need to track join-time -- remove this var if
;;; it works.
(defvar yodel-chatroom-join-time nil
  "the time we started chatting. heartbeats get sent based on this.")

(defun yodel-chatroom-start-heartbeat ()
  "start the timer to send chatroom heartbeats (161)"
  (setq yodel-chatroom-join-time (current-time))
  (yodel-make-timer 30 30 'yodel-chatroom-pre-heartbeat))
;yodel-timers
;(yodel-pop-timer)

(defun yodel-chatroom-pre-heartbeat ()
  "runs every minute until we hit either the bottom or top or the hour
then runs every half hour (or so.)"
  (let ((min (yodel-current-clock-minutes)))
    (when (or (= min 0) (= min 30))
      (yodel-kill-timer 'yodel-chatroom-pre-heartbeat)
      (yodel-make-timer 0 (* 15 60) 'yodel-chatroom-send-heartbeat))))

(defun yodel-chatroom-send-heartbeat ()
  "send the chatroom heartbeat"
  (interactive)
  (yodel-send :service YODEL_SERVICE_CHAT_HEARTBEAT
	      :data `(("109" . ,(yodel-account))))
  ;; we used to send a few other things, just in case.
  ;;(yodel-retrieve-chatrooms yodel-chat-current-category 'identity)
  ;;(yodel-chatroom-selection-ad)
  )
;(yodel-chatroom-send-heartbeat)

(defun yodel-chatroom-stop-heartbeat ()
  ;; this should only happen when we exit the chatroom system
  (setq yodel-chatroom-join-time nil)
  (yodel-kill-timer 'yodel-chatroom-send-heartbeat))
;yodel-timers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yodel-packet-extract-occupants (pkt)
  "extract the list of chatroom occupants from packet"
  ;; XXX for now it's just the names
  (mapcan (lambda (p)
	    (and (equal (car p) yodel-chatroom-message-sender)
		 (list (cdr p))))
	  (yodel-packet-data pkt)))

(defun yodel-chatroom-clear-occupants ()
  (setf yodel-chatroom-occupants (make-hash-table :test 'equal)))

(defun yodel-chatroom-add-occupant (name &rest message)
  "add NAME to the list of chatroom occupants"
  (unless (gethash name yodel-chatroom-occupants)
    (setf (gethash name yodel-chatroom-occupants) (current-time))
    (unless (yodel-chatroom-ignore-user-p name)
      (when yodel-chatroom-notices
	(yodel-chatroom-insert "** User Entered" name))
      )))

(defun yodel-chatroom-del-occupant (name)
  (remhash name yodel-chatroom-occupants))

(defun yodel-chatroom-user-join (pkt)
  "called when a user joins a room.
when you join a room, it contains the list of everyone in the room.
when someone else joins, it contains their information."
  ;; process the new user info
  ;; XXX there be bugs here
  (let ((room (yodel-packet-fetch pkt yodel-chatroom-message-room-name)))
    (cond ((yodel-packet-fetch pkt yodel-chatroom-join-error)
	   (message "could not join room: %s" room))
	  ((equal room yodel-current-chatroom)
	   (run-hook-with-args
	    'yodel-chatroom-user-join-functions
	    (car (yodel-packet-extract-occupants pkt))))
	  ((null room)
	   ;; added this case on 2004/05/08
	   ;; seems that large room lists get sent in two packets, and
	   ;; the second doesn't have a room name.
	   (mapcar 'yodel-chatroom-add-occupant
		   (yodel-packet-extract-occupants pkt)))
	  (t				; entered a new or different room
	   (message "entering new chatroom")
;	   (yodel-chatroom-insert (format "** %s" room))
;	   (and (yodel-packet-etch pkt "105")
;		(yodel-chatroom-insert
;		 (format "** %s" (yodel-packet-fetch pkt "105"))))
	   (yodel-chatroom-clear-occupants)
	   (setq yodel-current-chatroom room)
	   (run-hooks 'yodel-chatroom-join-hook)
	   (mapcar 'yodel-chatroom-add-occupant
		   (yodel-packet-extract-occupants pkt))
	   ))))

(defun yodel-chatroom-send-exit ()
  "exit the chatroom system"
  ;; after sending this, seems like you get a "160"
  ;; XXX we get a 160 at the top of the hour anyway.
  (interactive)
  (when (y-or-n-p "really send chatroom exit? ")
    (when yodel-current-chatroom
      (run-hooks 'yodel-chatroom-exit-hook)
      (yodel-send :service YODEL_SERVICE_CHAT_EXIT
		  :data `(("1" . ,(yodel-account))
			  ;; XXX do we need this one? it changes.
			  ;;("1005" . "33984552") 
			  )))))

(defun yodel-chatroom-user-leave (pkt)
  (run-hooks 'yodel-chatroom-user-leave-hook)
  (let ((user (yodel-packet-fetch pkt yodel-chatroom-message-sender)))
    (when yodel-chatroom-notices
      (unless (yodel-chatroom-ignore-user-p user)
	(yodel-chatroom-insert "** User Left" user)))
    (yodel-chatroom-del-occupant user)))

(defun yodel-chatroom-got-exit (pkt)
  "the server sent us an exit."
  ;; XXX this is happening a lot. what are we missing?
  (yodel-chatroom-insert nil "received an exit message")
  (run-hooks 'yodel-chatroom-exit-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yodel-chatroom-occupants ()
  (yodel-hash-table-keys yodel-chatroom-occupants))
;(length (yodel-chatroom-occupants))

(defun yodel-chatroom-send-count ()
  (interactive)
  (yodel-chatroom-message-send
   (format "this emacs thinks there are %d users in %s."
	   (length (yodel-chatroom-occupants))
	   yodel-current-chatroom)))

(defun yodel-chatroom-show-occupants ()
  (yodel-info-list 'yodel-chatroom-occupants
		   (concat yodel-current-chatroom " Occupants")))

(defvar yodel-chatroom-message-history nil)

(defun yodel-chatroom-user-prompt (&optional prompt)
  "prompt for the name of a chatroom user"
  (let* ((name (yodel-name-at-point))
	 (users (yodel-chatroom-occupants)))
    (completing-read
     (or prompt "User (enter for none): ")
     (mapcar 'list (if name (cons name users) users))
     nil nil name)))
;(yodel-chatroom-user-prompt)

(defun yodel-chatroom-message-prompt (&optional no-prompt)
  "chatroom prompt and completing read"
  ;; experimenting with prompts. what i really need to do is define a
  ;; prompt function that will let me end the prompting at any point
  ;; and add spaces.
  (let ((user ""))
    (unless no-prompt
      (setf user (yodel-chatroom-user-prompt)))
    (read-string "Message: "
		 (if (equal "" user) "" (format "%s, " user))
		 yodel-chatroom-message-history)))

(defun yodel-chatroom-user-info ()
  (interactive)
  (let ((user (yodel-chatroom-user-prompt)))
    (message "%s" (gethash user yodel-chatroom-occupants))))
;(yodel-chatroom-user-info)

(defun yodel-chatroom-browse-profile ()
  (interactive)
  (let ((name (yodel-chatroom-user-prompt "Who's Profile: ")))
    (browse-url (concat "http://profiles.yahoo.com/" name))))
;(global-set-key "\C-cyp" 'yodel-chatroom-browse-profile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chatroom ignores (they don't get saved right now)

(defvar yodel-chatroom-user-messages)
(setq yodel-chatroom-user-messages (make-hash-table :test 'equal))

;; ignore-duplicates isn't used right now. we just ignore them.
;(defvar yodel-chatroom-ignore-duplicates t)

;; used to use a hashtable, but changing to REGEX to more - we need both
;; effectively ignore people.
(defvar yodel-chatroom-ignores)
(setq yodel-chatroom-ignores
  (make-hash-table :test 'equal))

(defun yodel-chatroom-add-ignore-hashtable (user)
  (when user
    (setf (gethash user yodel-chatroom-ignores) t)))

;; trying a regex ignore
(defvar yodel-chatroom-ignore-regex nil)
(defvar yodel-chatroom-ignore-regex-pieces ())

(defun yodel-chatroom-add-ignore-regex (user)
  (when user
    (push user yodel-chatroom-ignore-regex-pieces)
    (setf yodel-chatroom-ignore-regex
	  (yodel-chatroom-make-ignore-regex))))

(defun yodel-chatroom-make-ignore-regex ()
  (let ((list yodel-chatroom-ignore-regex-pieces))
    (cond ((null list) nil)		;no ignores
	  (t (let ((re ""))
	       (dolist (n list)
		 (setq re (concat re
				  (if (string= "" re) "" "\\|")
				  n)))
	       (concat "^\\(" re "\\)$"))))))

(defun yodel-chat-ignore-bot ()
  (yodel-chatroom-add-ignore-hashtable (yodel-chatroom-user-prompt "bot name: ")))

(defun yodel-chatroom-add-ignore ()
  ;; normal deletes use the regex. bot deletes use the hashtable
  ;; so we can dump it to a web page
  (interactive)
  (yodel-chatroom-add-ignore-regex (yodel-chatroom-user-prompt))
  )

(defun yodel-chatroom-ignore-p (name message)
  ;; thanks to acksponies in programming:1 for making me realize that
  ;; i might want to repeat myself, but that doesn't mean i want to
  ;; see all the repeated spew in the chatroom.
  (unless (equal name (yodel-account))
    (or (yodel-chatroom-repeated-p name message)
	(yodel-chatroom-ignore-user-p name)
	(yodel-chatroom-ignore-content-p message)
	)))
  
(defun yodel-chatroom-ignore-user-p (name)
  (or (gethash name yodel-chatroom-ignores)
      (and yodel-chatroom-ignore-regex
	   (posix-string-match yodel-chatroom-ignore-regex name)
	   ;(yodel-chatroom-ignore-debug name)
	   )))

(defun yodel-chatroom-ignore-debug (name)
  (message (concat "ignored: " name))
  t)

(defun yodel-chatroom-del-ignore-ht ()
  (interactive)
  (let ((ignores
	 (mapcar 'list (yodel-hash-table-keys yodel-chatroom-ignores))))
    (if (null ignores)
	(message "nobody is being ignored")
      (let ((choice (completing-read "restore user: " ignores)))
	(when choice
	  (remhash choice yodel-chatroom-ignores))))))
;(yodel-chatroom-del-ignore)

(defun yodel-chatroom-del-ignore-re ()
  (let ((ignores (mapcar 'list yodel-chatroom-ignore-regex-pieces)))
    (if (null ignores)
	(message "nobody is being ignored")
      (let ((choice (completing-read "restore user: " ignores)))
	(when choice
	  (setf yodel-chatroom-ignore-regex-pieces
		(remove choice yodel-chatroom-ignore-regex-pieces)
		yodel-chatroom-ignore-regex
		(yodel-chatroom-make-ignore-regex)))))))

(defun yodel-chatroom-del-ignore ()
  (yodel-chatroom-del-ignore-ht))

(defun yodel-chatroom-clean-message (message)
  "for comparing messages without considering case or weird characters"
  (upcase (yodel-replace-in-string "[^A-Za-z0-9]" "" message)))
;(yodel-chatroom-clean-message "Foo Bar Baz")

(defun yodel-chatroom-repeated-p (user message)
  (let ((clean (yodel-chatroom-clean-message message)))
    (prog1 (equal clean (gethash user yodel-chatroom-user-messages))
      (setf (gethash user yodel-chatroom-user-messages) clean))))

(defun yodel-chatroom-ignore-content-p (message)
  ;; this is defined elsewhere
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chat categories

(defvar yodel-chat-current-category nil)
(defvar yodel-chat-current-room nil)

(defvar yodel-chatrooms nil
  "the list of chatrooms in the current category")

(defun yodel-chat-categories ()
  "return the value in yodel-chat-categories"
  ;; yodel-chat-categories should be populated when we first make
  ;; the yodel connection. if this value is nil, there is a problem.
  (assert yodel-chat-categories)
  yodel-chat-categories)
;(yodel-chat-categories)

(defstruct yodel-chatcat name id children)

(defun yodel-chat-category-hash (xml)
  "make chat categories easier to handle"
  ;(print xml)
  (cond ((null xml) nil)
	((equal '(()) xml) nil)
	(t (let ((hash (make-hash-table :test 'equal)))
	     (dolist (cat xml hash)
	       (let ((name (yodel-dehtml-entities
			    (xml-get-attribute cat 'name))))
		 (setf (gethash name hash)
		       (make-yodel-chatcat :name (xml-get-attribute cat 'name)
					   :id (xml-get-attribute cat 'id)
					   :children (yodel-chat-category-hash
						      (xml-node-children cat))
					   ))))))))
;(yodel-hash-table-keys (yodel-chat-category-hash yodel-chat-categories))

(defun yodel-select-category-helper (hashtable)
  (let ((selected
	 (completing-read
	  "select category: "
	  (mapcar 'list (yodel-hash-table-keys hashtable))
	  nil
	  t)))
    (message "retrieving rooms...")
    (let ((struct (gethash selected hashtable)))
      (if (null (yodel-chatcat-children struct))
	  struct
	(yodel-select-category-helper (yodel-chatcat-children struct))))))

(defun yodel-select-category ()
  (yodel-select-category-helper
   (yodel-chat-category-hash (yodel-chat-categories))))
;(yodel-select-category)

(defun yodel-retrieve-chatrooms (category continuation)
  "retrieve and return a list of yodel chatrooms for CATEGORY.
CONTINUATION is a function to call (with the chatroom XML buffer as an
argument) when the HTTP GET request is complete."
  ;; XXX should we even bother to save chatrooms to disk?
  ;; 2003/10/02 - started using a single file for chatrooms.
  (let* ((buffer (find-file-noselect
		  (yodel-account-path "yodel-chatroom"))))
    (setf yodel-chat-current-category category)
    (yodel-http-get buffer
		    yodel-xml-host
		    (format "/ycontent/?chatroom_%s=0&intl=us"
			    (yodel-chatcat-id category))
		    continuation)))

(defun yodel-process-chatroom-xml (buffer)
  "process the chatCategories (xml) in buffer and return a list."
  (with-current-buffer buffer
    (setf yodel-chatrooms (yodel-xml-parse-buffer buffer 'save))))
;;yodel-chatrooms
;(yodel-retrieve-chatrooms)
;(yodel-retrieve-chatrooms yodel-chat-current-category 'identity)

(defun yodel-chatroom-current-category-id ()
  (yodel-chatcat-id yodel-chat-current-category))
;(yodel-chatroom-current-category-id)

(defun yodel-room-lobbies (room)
  "pull the lobbies out of the room xml"
  (mapcar (lambda (lobby) (xml-get-attribute lobby 'count))
	  (xml-node-children room)))

(defun yodel-room-alist (xml)
  "XML is the parsed XML that resulted from yodel-chatroom-retrieve"
  (let ((rooms (mapcan
		(lambda (node) (xml-node-children node))
		(yodel-xml-content-node xml 'chatRooms))))
    (let ((acc ()))
      (dolist (room rooms acc)
	(let ((name (xml-get-attribute room 'name)))
	  (unless (equal "" name)
	    (push (list name
			(xml-get-attribute room 'id)
			(yodel-room-lobbies room))
		  acc)))))))

(defun yodel-select-lobby (lobbies)
  "select a lobby from lobbies until you get it right"
  ;(message (prin1-to-string lobbies))
  (let ((lobbies (remove "" lobbies)))
    (if (null (cdr lobbies))
	"1"
      (let ((lobby (read-string
		    (format "lobby [1-%d]? " (length lobbies)))))
	(cond ((equal "" lobby) "1")
	      ((member lobby lobbies) lobby)
	      (t (yodel-select-lobby lobbies)))))))
;(yodel-select-lobby '("1" "2" "3"))
;(yodel-select-lobby '())

(defun yodel-select-room (xml &optional room-name lobby-number)
  "select a chatroom from XML"
  (let* ((rooms (yodel-room-alist xml))
	 (name (or room-name (completing-read "select room: " rooms)))
	 (room (assoc name rooms))
	 (room-id (second room))
	 (lobbies (third room))
	 (lobby (or lobby-number (yodel-select-lobby lobbies))))
    (mapcar 'yodel-dehtml-entities (list name lobby room-id))))
;(setq yodel-xxx (yodel-xml-parse-buffer "test-rooms"))
;(yodel-select-room yodel-xxx)

(defun yodel-chatroom-select-and-join (&optional packet)
  "prompts for a category. retrieves rooms. prompts for a room and lobby.
returns a list of category-id, room-name and lobby-number."
  (yodel-retrieve-chatrooms
   (yodel-select-category)
   'yodel-select-chatroom-from-xml-and-join))

(defun yodel-select-chatroom-from-xml (xml-buffer)
  "this is passed to yodel-retrieve-chatrooms and is funcall'ed
after the chatroom XML is retrieved by yodel-http-get."
  (let ((rooms (yodel-process-chatroom-xml xml-buffer)))
    (cons (yodel-chatroom-current-category-id)
	  (mapcar 'yodel-dehtml (yodel-select-room rooms)))))

(defun yodel-select-chatroom-from-xml-and-join (xml-buffer)
  (destructuring-bind (id name lobby room-id)
      (yodel-select-chatroom-from-xml xml-buffer)
    (yodel-chatroom-join-chatroom id name lobby room-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yodel-conf.el - a first crack at yodel conferencing
;;; started on 2003/10/26. not very well tested.

(defvar yodel-conference-room nil
  "buffer local conference room ID")

(defvar yodel-conference-attendees nil
  "buffer local list of conference participants (excluding you)")

(defvar yodel-conf-address-field "3"
  "the packet field used to address messages to conference attendees")

(defun yodel-conference-p ()
  "if yodel-conference-room has a value in this buffer, and there are
attendees (both are buffer local vars), then it's a conference
buffer."
  (and yodel-conference-room yodel-conference-attendees))

(defun yodel-conf-invitee (pkt)
  (yodel-packet-fetch pkt "50"))

(defun yodel-conf-room-id (pkt)
  (yodel-packet-fetch pkt "57"))

(defun yodel-conf-message-text (pkt)
  (yodel-packet-fetch pkt "14"))

(defun yodel-conf-message-sender (pkt)
  ;; this is different than a plain message
  (yodel-packet-fetch pkt "3"))

(defun yodel-conf-buffer (room-id)
  (or (get-buffer room-id)
      (error "no such room: %s" room-id)))

(defun yodel-conf-make-buffer (pkt)
  (let ((buffer (get-buffer-create (yodel-conf-room-id pkt))))
    (switch-to-buffer buffer)
    (yodel-chat-mode)
    (make-local-variable 'yodel-conference-room)
    (make-local-variable 'yodel-conference-attendees)
    (setf yodel-conference-room (yodel-conf-room-id pkt)
	  yodel-conference-attendees (yodel-conf-other-attendees pkt))
    buffer))

(defun yodel-conf-other-attendees (pkt)
  (let ((attendees nil))
    (loop for (key . val) in (yodel-packet-data pkt)
	  when (and (or (equal "52" key) (equal "50" key))
		    (not (equal (yodel-account) val)))
	  do (pushnew val attendees :test 'equal))
    attendees))
;;(yodel-conf-other-attendees yodel-test-packet)

;;; sending conference messages

(defun yodel-conf-list-attendees ()
  "display the list of conference attendees"
  (yodel-info-list yodel-conference-attendees "Attendees"))

(defun yodel-conf-prompt-and-send (&optional msg)
  (let ((text (or msg (read-string "Message: "))))
    (when text
      (let ((yodel-conf-address-field "53"))
	(yodel-conf-send YODEL_SERVICE_CONFMSG
			 `(("14" . ,text)
			   ("97" . "1")))
	(yodel-conf-insert yodel-conference-room
			   (format "(%s): %s" (yodel-account) text))))))

(defun yodel-conf-send (service &optional rest)
  (yodel-send :service service
	      :data `(("1" . ,(yodel-account))
		      ("57" . ,yodel-conference-room)
		      ,@(mapcar (lambda (name)
				  (cons yodel-conf-address-field name))
				yodel-conference-attendees)
		      ,@rest)))

(defun yodel-conf-join (pkt)
  (interactive)
  (with-current-buffer (yodel-conf-make-buffer pkt)
    (yodel-conf-send YODEL_SERVICE_CONFLOGON)))
;;(yodel-conf-join yodel-incoming-packet)

(defun yodel-conf-decline (pkt)
  ;; XXX verify
  (let ((yodel-conference-room (yodel-conf-room-id pkt))
	(yodel-conference-attendees (yodel-conf-other-attendees pkt)))
  (yodel-conf-send YODEL_SERVICE_CONFDECLINE )))

(defun yodel-conf-quit ()
  (when (y-or-n-p "really quit? ")
    (yodel-conf-send YODEL_SERVICE_CONFLOGOFF)
    ;; run-hooks...
    (kill-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; incoming message handlers

(defun yodel-conf-process-message (pkt)
  ;; XXX refactor - seems like we should be able to use the other
  ;; insert-text functions here.
  (yodel-conf-insert
   (yodel-conf-room-id pkt)
   (concat (yodel-conf-message-sender pkt)
	   ": "
	   (yodel-conf-message-text pkt))))

(defun yodel-conf-insert (room-id text)
  (with-current-buffer (yodel-conf-buffer room-id)
    (let ((followp (equal (point) (point-max))))
      (save-excursion
	(goto-char (point-max))
	(insert "\n\n" text))
      (and followp (goto-char (point-max))))))

(defvar yodel-conf-invites (make-hash-table :test 'equal))
;(setq yodel-conf-invites (make-hash-table :test 'equal))

(defun yodel-conf-process-invite-p (invitee)
  "Returns true if we will process a conference invitation from INVITEE."
  ;; A flood of conference invitations seems to be an effective way to
  ;; boot, so we limit the number of invitiations we will process from
  ;; any single user. (Tested the with notorious Yahoo! hacker, UK Leo
  ;; on 2004/04/24.)
  (let ((last (gethash invitee yodel-conf-invites)))
    (when (or (not last)
	      ;; process one invitation every 30 seconds
	      (< 30 (yodel-time-since last)))
      (setf (gethash invitee yodel-conf-invites) (current-time)))))

(defun yodel-conf-process-invite (pkt)
  "called when we receive a conference invitation"
  (let ((invitee (yodel-conf-invitee pkt)))
    (when (yodel-conf-process-invite-p invitee)
      (if (yes-or-no-p (format "join %s's conference? " invitee))
	  (yodel-conf-join pkt)
	(yodel-conf-decline pkt)))))

(defun yodel-conf-process-add-invite (pkt)
  "called when we receive a conference invitation"
  (message "TBD: add-invite"))

(defun yodel-conf-process-join (pkt)
  ;; tells when other people joined, or you errored (i guess)
  (message "conf-process-join: tbd")
  )

(defun yodel-conf-process-quit (pkt)
  ;; tells when other people joined, or you errored (i guess)
  (let ((conf (yodel-packet-fetch pkt "57"))
	(quitter (yodel-packet-fetch pkt "56")))
    (when (get-buffer conf)
      (with-current-buffer (get-buffer conf)
	(yodel-conf-delete-user quitter)))
    (yodel-conf-insert conf (format "*** %s left" quitter))))

(defun yodel-conf-add-user (name)
  (push name yodel-conference-attendees))

(defun yodel-conf-delete-user (name)
  (setf yodel-conference-attendees
	(remove name yodel-conference-attendees)))

(defun yodel-conf-process-decline (pkt)
  ;; tells when other people joined, or you errored (i guess)
  (message "conf-process-quit: tbd")
  )

(define-yodel-proc YODEL_SERVICE_CONFINVITE 'yodel-conf-process-invite)

(define-yodel-proc YODEL_SERVICE_CONFADDINVITE
  'yodel-conf-process-add-invite)

(define-yodel-proc YODEL_SERVICE_CONFLOGON 'yodel-conf-process-join)

(define-yodel-proc YODEL_SERVICE_CONFLOGOFF 'yodel-conf-process-quit)

(define-yodel-proc YODEL_SERVICE_CONFDECLINE 'yodel-conf-process-decline)

(define-yodel-proc YODEL_SERVICE_CONFMSG 'yodel-conf-process-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yodel-info-mode
;;; an attempt to reduce our abuse of the minibuffer.

(defvar yodel-info-temp-window " yodel-info")

(defvar yodel-info-mode-map nil)

(defvar yodel-info-mode-hook nil)

(defvar yodel-info-title "yodel info")

(defvar yodel-info-function nil)

(defun yodel-info-mode-map ()
  (setq yodel-info-mode-map (make-keymap))
  (suppress-keymap yodel-info-mode-map)
  (define-key yodel-info-mode-map "?" 'Helper-help)
  (define-key yodel-info-mode-map "h" 'Helper-help)
  (define-key yodel-info-mode-map "P" 'yodel-chatroom-browse-profile)
  (define-key yodel-info-mode-map "q" 'delete-window)
  (define-key yodel-info-mode-map "Q" 'delete-window)
  (define-key yodel-info-mode-map "r" 'yodel-info-refresh)
  (define-key yodel-info-mode-map "R" 'yodel-info-refresh)
  (define-key yodel-info-mode-map [enter] 'yodel-info-make-buddy-buffer)
  (define-key yodel-info-mode-map [return] 'yodel-info-make-buddy-buffer)
  )

(if yodel-info-mode-map
    t
  (yodel-info-mode-map))

(defun yodel-info-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'yodel-info-mode)
  (setq mode-name "yodel-info")
  (use-local-map yodel-info-mode-map)
  (make-local-variable 'yodel-info-title)
  (make-local-variable 'yodel-info-function)
  (run-hooks 'yodel-info-mode-hook))

(defun yodel-info-title ()
  (let ((s (concat yodel-info-title
		   "    (press 'q' to quit"
		   (if (functionp yodel-info-function)
		       ", 'r' to refresh)"
		     ")"))))
    (insert s "\n")
    (dotimes (i (length s))
      (insert "-"))
    (insert "\n")))

(defun yodel-info-data ()
  (cond ((functionp yodel-info-function)
	 (funcall yodel-info-function))
	((consp yodel-info-function) yodel-info-function)))

(defun yodel-info-list (list-function &optional title)
  "display LIST in a separate window. like completion results."
  ;; help already does this. how do we call that stuff?
  (interactive)
  ;; XXX shouldn't split if we don't need to
  (split-window)
  (switch-to-buffer (get-buffer-create yodel-info-temp-window))
  (yodel-info-mode)
  (setf yodel-info-function list-function
	yodel-info-title (or title yodel-info-title))
  (yodel-info-refresh))

(defun yodel-info-refresh ()
  (interactive)
  (erase-buffer)
  (yodel-info-title)
  (yodel-info-insert-list (yodel-info-data))
  (goto-char (point-min))		;start at the top
  (next-line 1))

(defun yodel-info-column-width (width &optional min-padding)
  (if (zerop width)
      1
    (let ((cols (/ (frame-width) width)))
      (if (<= cols 1)
	  1
	(let ((pads (/ (- (frame-width) (* width cols)) (1- cols))))
	  (if (> pads min-padding)
	      cols
	    (1- cols)))))))
;(yodel-info-column-width 60 3)

(defun yodel-info-insert-list (list)
  ;; list is a list of strings
  (when list
    (let* ((padding 3)
	   (biggest (apply 'max (mapcar 'length list)))
	   (num-cols (yodel-info-column-width biggest padding))
	   (template (format "%%-%ds" (+ biggest padding)))
	   (elms (sort (copy-list list) 'string-lessp)))
      (while elms
	(insert "\n")
	(dotimes (i num-cols)
	  (when elms
	    (insert (if (= i (1- num-cols))
			(pop elms)
		      (format template (pop elms))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yodel-chat-mode

(defvar yodel-buffer-history nil)

(defvar yodel-chat-mode-map nil)

;(global-set-key "\C-cy" 'yodel-logon)
;(yodel-disconnect)

(defun yodel-chat-mode-map ()
  "Key map for yodel-chat major mode."
  (setq yodel-chat-mode-map (make-keymap))
  (suppress-keymap yodel-chat-mode-map)

  ;; help with yodel-chat-mode
  (define-key yodel-chat-mode-map "?" 'Helper-help)
  (define-key yodel-chat-mode-map "h" 'Helper-help)
  ;; make a new buffer to chat with a buddy
  (define-key yodel-chat-mode-map "n" 'yodel-make-buddy-buffer)
  (define-key yodel-chat-mode-map "/" 'yodel-chat-command)

  ;; insert a new message (in the current chatroom or IM)
  (define-key yodel-chat-mode-map [return] 'yodel-chat-message)
  (define-key yodel-chat-mode-map [enter] 'yodel-chat-message)
  (define-key yodel-chat-mode-map "\r" 'yodel-chat-message)
  (define-key yodel-chat-mode-map "m" 'yodel-chat-message-fewer-prompts)
  (define-key yodel-chat-mode-map "t" 'yodel-chat-message-thought)
  ;;(define-key yodel-chat-mode-map "M" 'yodel-chat-message-window)
  (define-key yodel-chat-mode-map "r" 'yodel-chat-message-reply)

  ;; some automatic messages
  (define-key yodel-chat-mode-map "V" 'yodel-chat-send-version)
  ;;(define-key yodel-chat-mode-map "C" 'yodel-chatroom-send-count)

  ;; quit a particular chat window
  (define-key yodel-chat-mode-map "q" 'yodel-chat-quit)

  ;; join a chat-room
  (define-key yodel-chat-mode-map "j" 'yodel-join-chatroom)
  (define-key yodel-chat-mode-map "c" 'yodel-chat-cycle-buffers)

  ;; close the current chat window, but stay connected
  (define-key yodel-chat-mode-map "k" 'yodel-chat-kill-buffer)

  ;; buddy stuff
  ;; add a buddy
  (define-key yodel-chat-mode-map "a" 'yodel-buddy-add)
  (define-key yodel-chat-mode-map "d" 'yodel-buddy-del)
  (define-key yodel-chat-mode-map "l" 'yodel-chat-show-buddies)
  (define-key yodel-chat-mode-map "F" 'yodel-chat-font-lock)
  (define-key yodel-chat-mode-map "L" 'yodel-buddy-show-status-list)
  (define-key yodel-chat-mode-map "P" 'yodel-chatroom-browse-profile)

  (define-key yodel-chat-mode-map "u" 'browse-url-at-point)

  ;; status
  (define-key yodel-chat-mode-map "A" 'yodel-send-isaway)
  (define-key yodel-chat-mode-map "B" 'yodel-send-isback)

  ;; ignores
  (define-key yodel-chat-mode-map "i" 'yodel-chat-add-ignore)
  (define-key yodel-chat-mode-map "I" 'yodel-chat-del-ignore)

  ;; clear the buffer
  (define-key yodel-chat-mode-map "X" 'yodel-chat-erase-buffer)

  ;; stuff from yodel-silly
;;   (define-key yodel-chat-mode-map "E" 'yodel-eval-and-send)
;;   (define-key yodel-chat-mode-map "Y" 'yodel-chat-yow)
;;   (define-key yodel-chat-mode-map "R" 'yodel-chat-reverse-and-send)
;;   (define-key yodel-chat-mode-map "V" 'yodel-silly-version)
;;   (define-key yodel-chat-mode-map "C" 'yodel-caesar-message)
;;   (define-key yodel-chat-mode-map "3" 'yodel-silly-31337-message)
;;   (define-key yodel-chat-mode-map "D"
;;     (lambda ()
;;       (interactive)
;;       (yodel-doctor (yodel-chatroom-user-prompt "patient: "))))

  ;; (define-key yodel-chat-mode-map [mouse-1] 'browse-url-at-mouse)
  t)

(if yodel-chat-mode-map
    t
  (yodel-chat-mode-map))

(defun yodel-chat-mode ()
  "Major mode for chatting with Yodel!Messenger users.

Type `m' to insert a message,
     `q' to quit yodel-chat-mode,
     `h' for more help."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'yodel-chat-mode)
  (setq mode-name "yodel-chat")
  (use-local-map yodel-chat-mode-map)
  (make-local-variable 'yodel-chat-buddy-name)
  (make-local-variable 'yodel-buffer-history)
  (run-hooks 'yodel-chat-mode-hook))

(defun yodel-chat-font-lock ()
  (interactive)
  (when (or (yodel-xemacs) global-font-lock-mode)
    (font-lock-mode t)
    (setf font-lock-keywords
	  `((,(concat "^\\(" (yodel-account) "\\|" "(to[^:\n]+)\\):[ \n]")
	     . font-lock-function-name-face)
	    ("^[A-Za-z0-9_.@-]+:[ \n]" . font-lock-comment-face)
	    ("^\\*\\*[^:\n]*:" . font-lock-keyword-face)))
    (set (make-local-variable 'font-lock-string-face) nil)))
;(yodel-chat-font-lock)

(defun yodel-chat-help ()
  (interactive)
  (Helper-help))

(defun yodel-chat-command ()
  (interactive)
  (message "tbd"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XXX refactor these

(defun yodel-logon ()
  "command to logon to a yodel server"
  (interactive)
  (if (equal YODEL_YMSG YODEL_PROTO_VER)
      (yodel-logon-ymsg)
    (yodel-logon-webmsg)))

(defun yodel-disconnect ()
  (interactive)
  (when (yodel-connection-process)
    (when (y-or-n-p "kill the yod.el connection? ")
      (yodel-send-logoff)
      (run-hooks 'yodel-disconnect-hook))))
;(yodel-disconnect)

(defun yodel-chat-message ()
  "insert a message into the current yodel-chat-mode buffer.
yodel-chat-message maintains a history so you don't have to retype
messages."
  (interactive)
  (cond ((yodel-conference-p) (yodel-conf-prompt-and-send))
	((yodel-chatroom-buffer-p)
	 (yodel-chatroom-message-send (yodel-chatroom-message-prompt)))
	((yodel-individual-chat-buffer-p)
	 ;; trying to make it easier to select a buddy
	 (yodel-message-send-im
	  yodel-chat-buddy-name (yodel-message-prompt-for-text)))
	(t (let ((name (yodel-name-at-point)))
	     (if (yodel-buddy-find name)
		 (yodel-message-send-im
		  name (yodel-message-prompt-for-text
			(format "Message (to %s) " name)))
	       (yodel-message-prompt-and-send))))))

(defun yodel-chat-message-fewer-prompts ()
  "send a message using previously entered data if possible"
  (interactive)
  (cond ((yodel-conference-p) (yodel-chat-message))
	((yodel-chatroom-buffer-p)
	 (yodel-chatroom-message-send
	  (read-string "Message: " "" yodel-chatroom-message-history)))
	((yodel-individual-chat-buffer-p) (yodel-chat-message))
	((yodel-message-last-recipient)
	 (yodel-message-send-im-to-user (yodel-message-last-recipient)))
	(t (yodel-chat-message))))

(defun yodel-chat-message-thought ()
  "send a status message to a chatroom, or a normal message in other buffers."
  (interactive)
  (cond ((yodel-chatroom-buffer-p)
	 (yodel-chatroom-message-send
	  (read-string "Message: " "" yodel-chatroom-message-history) t))
	(t (yodel-chat-message-fewer-prompts))))

(defun yodel-chat-message-reply ()
  "send a message to the last person who sent you one."
  (interactive)
  (cond ((yodel-chatroom-buffer-p) (yodel-chat-message))
	((yodel-conference-p) (yodel-chat-message))
	((yodel-individual-chat-buffer-p) (yodel-chat-message))
	(t (yodel-message-send-im-to-user (car yodel-message-senders)))))

(defun yodel-chat-show-buddies ()
  "displays a list of online buddies, or chatroom occupants."
  (interactive)
  (cond ((yodel-conference-p) (yodel-conf-list-attendees))
	((yodel-chatroom-buffer-p) (yodel-chatroom-show-occupants))
	(t (yodel-buddy-show-available-buddies))))

(defun yodel-chat-quit ()
  "quit the current chat buffer, or the whole chat session."
  (interactive)
  (cond ((yodel-conference-p) (yodel-conf-quit))
	((yodel-chatroom-buffer-p) (yodel-chatroom-send-exit))
	((yodel-individual-chat-buffer-p)
	 (when (y-or-n-p "Kill this buffer? (not the connection) ")
	   (kill-buffer (current-buffer))))
	(t (yodel-disconnect))))

(defun yodel-chat-add-ignore ()
  "ignore a user"
  (interactive)
  (cond ((yodel-chatroom-buffer-p) (yodel-chatroom-add-ignore))
	(t (yodel-send-add-ignore))))

(defun yodel-chat-del-ignore ()
  "stop ignoring a user"
  (interactive)
  (cond ((yodel-chatroom-buffer-p) (yodel-chatroom-del-ignore))
	(t (yodel-send-del-ignore))))

(defun yodel-chat-erase-buffer ()
  "erase the current buffer."
  (interactive)
  (erase-buffer))

(defun yodel-chat-cycle-buffers ()
  "cycle through the yodel message buffers"
  (interactive)
  (let ((buffers (sort (remove-if-not 'yodel-message-buffer-p
				      (mapcar 'buffer-name (buffer-list)))
		       'string<))
	(current (buffer-name (current-buffer))))
    (unless (null buffers)
      (let ((first (car buffers)))
	(while buffers
	  (when (equal current (pop buffers))
	    (switch-to-buffer (if buffers (car buffers) first) 'no-record)))
	(goto-char (point-max))))))

(defun yodel-chat-send-version ()
  "send yod.el (and emacs) version information"
  (interactive)
  (let ((msg (yodel-version-message)))
    (cond ((yodel-chatroom-buffer-p)
	   (yodel-chatroom-message-send msg t))
	  ((yodel-conference-p) (yodel-conf-send msg))
	  ;; XXX bleah. this needs to be easier.
	  (t (message "version is tbd in chat buffers"))
	  )))

;;; end of yod.el
