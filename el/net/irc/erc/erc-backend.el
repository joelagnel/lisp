;;; erc-backend.el --- Backend network communication for ERC

;; Copyright (C) 2004,2005 Free Software Foundation, Inc.

;; Filename: erc-backend.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-05-7
;; Keywords: IRC chat client internet

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This file defines backend network communication handlers for ERC.
;;
;; How things work:
;;
;; You define a new handler with `define-erc-response-handler'.  This
;; defines a function, a corresponding hook variable, and populates a
;; global hash table `erc-server-responses' with a map from response
;; to hook variable.  See the function documentation for more
;; information.
;;
;; Upon receiving a line from the server, `erc-parse-server-response'
;; is called on it.
;;
;; A line generally looks like:
;;
;; LINE := ':' SENDER ' ' COMMAND ' ' (COMMAND-ARGS ' ')* ':' CONTENTS
;; SENDER := Not ':' | ' '
;; COMMAND := Not ':' | ' '
;; COMMAND-ARGS := Not ':' | ' '
;;
;; This gets parsed and stuffed into an `erc-response' struct.  You
;; can access the fields of the struct with:
;;
;; COMMAND --- `erc-response.command'
;; COMMAND-ARGS --- `erc-response.command-args'
;; CONTENTS --- `erc-response.contents'
;; SENDER --- `erc-response.sender'
;; LINE --- `erc-response.unparsed'
;;
;; WARNING, WARNING!!
;; It's probably not a good idea to destructively modify the list
;; of command-args in your handlers, since other functions down the
;; line may well need to access the arguments too.
;;
;; That is, unless you're /absolutely/ sure that your handler doesn't
;; invoke some other function that needs to use COMMAND-ARGS, don't do
;; something like
;;
;; (while (erc-response.command-args parsed)
;;   (let ((a (pop (erc-response.command-args parsed))))
;;     ...))
;;
;; The parsed response is handed over to
;; `erc-handle-parsed-server-response', which checks whether it should
;; carry out duplicate suppression, and then runs `erc-call-hooks'.
;; `erc-call-hooks' retrieves the relevant hook variable from
;; `erc-server-responses' and runs it.
;;
;; Most handlers then destructure the parsed response in some way
;; (depending on what the handler is, the arguments have different
;; meanings), and generally display something, usually using
;; `erc-display-message'.

;;; TODO;
;; o Rip out the sending commands stuff from erc.el and rewrite it in
;;   a slightly more abstracted fashion.  That should probably go in
;;   here too.
;;
;; o Generalise the display-line code so that we can use it to
;;   display the stuff we send, as well as the stuff we receive.
;;   Then, move all display-related code into another backend-like
;;   file, erc-display.el, say.
;;
;; o Clean up the handlers using new display code (has to be written
;;   first).

;;; History:
;; 2004/05/10 --- Handler bodies taken out of erc.el and ported to
;; new interface.

;;; Code:
(require 'cl)
(autoload 'erc-with-buffer "erc" nil nil 'macro)

(defconst erc-backend-version "$Revision: 1.18.2.7 $")

(defvar erc-server-responses (make-hash-table :test #'equal)
  "Hashtable mapping server responses to their handler hooks.")

(defstruct (erc-response (:conc-name erc-response.))
  (unparsed "" :type string)
  (sender "" :type string)
  (command "" :type string)
  (command-args '() :type list)
  (contents "" :type string))

(defun erc-parse-server-response (proc string)
  "Parse and act upon a complete line from an IRC server.
PROC is the process (connection) from which STRING was received.
PROCs `process-buffer' is `current-buffer' when this function is called."
  (unless (string= string "") ;; Ignore empty strings
    (save-match-data
      (let ((posn (if (eq (aref string 0) ?:)
		      (string-match " " string)
		    0))
	    (msg (make-erc-response :unparsed string)))

	(setf (erc-response.sender msg)
              (if (eq posn 0)
                  erc-session-server
                (substring string 1 posn)))

	(setf (erc-response.command msg)
              (let* ((bposn (string-match "[^ ]" string posn))
                     (eposn (string-match " " string bposn)))
                (setq posn (and eposn
                                (string-match "[^ ]" string eposn)))
                (substring string bposn eposn)))

	(while (and posn
		    (not (eq (aref string posn) ?:)))
	  (push (let* ((bposn posn)
                       (eposn (string-match " " string bposn)))
                  (setq posn (and eposn
                                  (string-match "[^ ]" string eposn)))
                  (substring string bposn eposn))
                (erc-response.command-args msg)))
	(when posn
      (let ((str (substring string (1+ posn))))
        (push str (erc-response.command-args msg))))

    (setf (erc-response.contents msg)
          (first (erc-response.command-args msg)))

    (setf (erc-response.command-args msg)
          (nreverse (erc-response.command-args msg)))

    (erc-decode-parsed-server-response msg)

    (erc-handle-parsed-server-response proc msg)))))

(defun erc-decode-parsed-server-response (parsed-response)
  "Decode a pre-parsed PARSED-RESPONSE before it can be handled.

If there is a channel name in `erc-response.command-args', decode
`erc-response' acroding this channel name and
`erc-encoding-coding-alist', or use `erc-default-coding-system'
for decoding."
  (let ((args (erc-response.command-args parsed-response))
	(decode-target nil)
	(decoded-args ()))
    (dolist (arg args nil)
      (when (string-match "^[#&].*" arg)
	(setq decode-target arg)))
    (when (stringp decode-target)
      (setq decode-target (erc-decode-string-from-target decode-target nil)))
    (setf (erc-response.unparsed parsed-response)
	  (erc-decode-string-from-target
	   (erc-response.unparsed parsed-response)
	   decode-target))
    (setf (erc-response.sender parsed-response)
	  (erc-decode-string-from-target
	   (erc-response.sender parsed-response)
	   decode-target))
    (setf (erc-response.command parsed-response)
	  (erc-decode-string-from-target
	   (erc-response.command parsed-response)
	   decode-target))
    (dolist (arg (nreverse args) nil)
      (push (erc-decode-string-from-target arg decode-target)
	    decoded-args))
    (setf (erc-response.command-args parsed-response) decoded-args)
    (setf (erc-response.contents parsed-response)
	  (erc-decode-string-from-target
	   (erc-response.contents parsed-response)
	   decode-target))))

;; (defun erc-parse-server-response (process response)
;;   "Parse a server PROCESS's IRC RESPONSE."
;;   ;; FIXME: this function doesn't do the same as our original
;;   ;; `erc-parse-line-from-server' on responses not starting with a
;;   ;; colon -- Lawrence 2004/05/10
;;   ;; FIXME: we currently don't deal correctly with IPv6 hosts in the
;;   ;; hostmask, they contain colons which messes up our parsing.
;;   ;; There's nothing in the IRC RFCs which says how to deal with
;;   ;; this, so at the moment we just ignore it.  However, it does mean
;;   ;; that anything sent by people with IPv6 hostmasks doesn't get
;;   ;; shown. -- Lawrence 2004/05/15
;;   (unless (string= response "")      ; don't care about empty strings.
;;     (save-match-data
;;       (let ((message (make-erc-response)))
;;         (setf (erc-response.unparsed message) response)
;;         ;; Everything after the second colon is the contents of the
;;         ;; message.
;;         (when (string-match "\\([^:]*:[^:]*:\\)\\(.*\\)" response)
;;           (setf (erc-response.contents message) (match-string 2 response))
;;           ;; XEmacs' 5th argument to `replace-match' behaves unlike
;;           ;; Emacs' when dealing with strings (rather than buffers).
;;           (setq response (match-string 1 response)))
;;         ;; If the message starts with a colon, then everything up to
;;         ;; the first space is the sender of the message (be it nick
;;         ;; or server).  If it doesn't, just use the current server.
;;         (if (string-match "^:\\([^ ]*\\) " response)
;;             (progn
;;               (setf (erc-response.sender message) (match-string 1 response))
;;               (setq response (replace-match "" nil t response)))
;;           (setf (erc-response.sender message) erc-session-server))

;;         ;; The next part of the message, up to the space, is the
;;         ;; command. (e.g. PRIVMSG).
;;         (when (string-match "\\([^ ]+\\) " response)
;;           (setf (erc-response.command message) (match-string 1 response))
;;           (setq response (replace-match "" nil t response)))

;;         ;; Everything up to the next colon are arguments to the
;;         ;; command.
;;         (while (not (string-match "^\\(:?:\\|$\\)" response))
;;           (when (string-match "\\([^ ]+\\)\\(:? \\|$\\)" response)
;;             (push (match-string 1 response)
;;                   (erc-response.command-args message))
;;             (setq response (replace-match "" nil t response))))
;;         (setf (erc-response.command-args message)
;;               (reverse (erc-response.command-args message)))
;;         (erc-handle-parsed-server-response process message)))))

(defun erc-handle-parsed-server-response (process parsed-response)
  "Handle a pre-parsed PARSED-RESPONSE from PROCESS.

Hands off to helper functions via `erc-call-hooks'."
  (if (member (erc-response.command parsed-response) erc-prevent-duplicates)
      (let ((m (erc-response.unparsed parsed-response)))
        ;; duplicate supression
        (if (< (or (gethash m erc-duplicates) 0)
               (- (erc-current-time) erc-duplicate-timeout))
            (erc-call-hooks process parsed-response))
        (puthash m (erc-current-time) erc-duplicates))
    ;; Hand off to the relevant handler.
    (erc-call-hooks process parsed-response)))

(defun erc-get-hook (command)
  "Return the hook variable associated with COMMAND.

See also `erc-server-responses'."
  (gethash (format (if (numberp command) "%03i" "%s") command)
           erc-server-responses))

(defun erc-call-hooks (process message)
  "Call hooks associated with MESSAGE in PROCESS.

Finds hooks by looking in the `erc-server-responses' hashtable."
  (let ((hook (or (erc-get-hook (erc-response.command message))
                  'erc-default-server-functions)))
    (run-hook-with-args-until-success hook process message)
    (with-current-buffer (erc-server-buffer)
      (run-hook-with-args 'erc-timer-hook (erc-current-time)))))

(add-hook 'erc-default-server-functions 'erc-handle-unknown-server-response)

(defun erc-handle-unknown-server-response (proc parsed)
  "Display unknown server response's message."
  (let ((line (concat (erc-response.sender parsed)
		      " "
		      (erc-response.command parsed)
		      " "
		      (mapconcat 'identity (erc-response.command-args parsed) " "))))
    (erc-display-message parsed 'notice proc line)))


(put 'define-erc-response-handler 'edebug-form-spec
     '(&define :name erc-response-handler
	       (name &rest name)
	       &optional sexp sexp def-body))

(defmacro* define-erc-response-handler ((name &rest aliases)
                                        &optional extra-fn-doc extra-var-doc
                                        &rest fn-body)
  "Define an ERC handler hook/function pair.
NAME is the reponse name as sent by the server (see the IRC RFC for
meanings).

This creates:
 o a hook variable `erc-server-NAME-functions' initialised to 'erc-server-NAME.
 o a function `erc-server-NAME' with body FN-BODY.

If ALIASES is non-nil, each alias in ALIASES is `defalias'ed to
`erc-server-NAME'.
Alias hook variables are created as `erc-server-ALIAS-functions' and
initialised to the same default value as `erc-server-NAME-functions'.

FN-BODY is the body of `erc-server-NAME' it may refer to the two
function arguments PROC and PARSED.

If EXTRA-FN-DOC is non-nil, it is inserted at the beginning of the
defined function's docstring.

If EXTRA-VAR-DOC is non-nil, it is inserted at the beginning of the
defined variable's docstring.

As an example:

  (define-erc-response-handler (311 WHOIS WI)
    \"Some non-generic function documentation.\"
    \"Some non-generic variable documentation.\"
    (do-stuff-with-whois proc parsed))

Would expand to:

  (prog2
      (defvar erc-server-311-functions 'erc-server-311
        \"Some non-generic variable documentation.

  Hook called upon receiving a 311 server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.
  See also `erc-server-311'.\")

      (defun erc-server-311 (proc parsed)
        \"Some non-generic function documentation.

  Handler for a 311 server response.
  PROC is the server process which returned the response.
  PARSED is the actual response as an `erc-response' struct.
  If you want to add responses don't modify this function, but rather
  add things to `erc-server-311-functions' instead.\"
        (do-stuff-with-whois proc parsed))

    (puthash \"311\" 'erc-server-311-functions erc-server-responses)
    (puthash \"WHOIS\" 'erc-server-WHOIS-functions erc-server-responses)
    (puthash \"WI\" 'erc-server-WI-functions erc-server-responses)

    (defalias 'erc-server-WHOIS 'erc-server-311)
    (defvar erc-server-WHOIS-functions 'erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WHOIS server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.
  See also `erc-server-311'.\")

    (defalias 'erc-server-WI 'erc-server-311)
    (defvar erc-server-WI-functions 'erc-server-311
      \"Some non-generic variable documentation.

  Hook called upon receiving a WI server response.
  Each function is called with two arguments, the process associated
  with the response and the parsed response.
  See also `erc-server-311'.\"))

\(fn (NAME &rest ALIASES) &optional EXTRA-FN-DOC EXTRA-VAR-DOC &rest FN-BODY)"
  (if (numberp name) (setq name (intern (format "%03i" name))))
  (setq aliases (mapcar (lambda (a)
                          (if (numberp a)
                              (format "%03i" a)
                            a))
                        aliases))
  (let* ((hook-name (intern (format "erc-server-%s-functions" name)))
         (fn-name (intern (format "erc-server-%s" name)))
         (hook-doc (format "%sHook called upon receiving a %%s server response.
Each function is called with two arguments, the process associated
with the response and the parsed response.
See also `%s'."
                           (if extra-var-doc
                               (concat extra-var-doc "\n\n")
                             "")
                           fn-name))
         (fn-doc (format "%sHandler for a %s server response.
PROC is the server process which returned the response.
PARSED is the actual response as an `erc-response' struct.
If you want to add responses don't modify this function, but rather
add things to `%s' instead."
                         (if extra-fn-doc
                             (concat extra-fn-doc "\n\n")
                           "")
                         name hook-name))
         (fn-alternates
          (loop for alias in aliases
                collect (intern (format "erc-server-%s" alias))))
         (var-alternates
          (loop for alias in aliases
                collect (intern (format "erc-server-%s-functions" alias)))))
    `(prog2
         ;; Normal hook variable.
         (defvar ,hook-name ',fn-name ,(format hook-doc name))
         ;; Handler function
         (defun ,fn-name (proc parsed)
           ,fn-doc
           ,@fn-body)

       ;; Make find-function and find-variable find them
       (put ',fn-name 'definition-name ',name)
       (put ',hook-name 'definition-name ',name)

       ;; Hashtable map of responses to hook variables
       ,@(loop for response in (cons name aliases)
               for var in (cons hook-name var-alternates)
               collect `(puthash ,(format "%s" response) ',var
                                 erc-server-responses))
       ;; Alternates.
       ;; Functions are defaliased, hook variables are defvared so we
       ;; can add hooks to one alias, but not another.
       ,@(loop for fn in fn-alternates
               for var in var-alternates
               for a in aliases
               nconc (list `(defalias ',fn ',fn-name)
                           `(defvar ,var ',fn-name ,(format hook-doc a))
			   `(put ',var 'definition-name ',hook-name))))))

(define-erc-response-handler (ERROR)
  "Handle an ERROR command from the server." nil
  (erc-display-message
   parsed 'error nil 'ERROR
   ?s (erc-response.sender parsed) ?c (erc-response.contents parsed)))

(define-erc-response-handler (INVITE)
  "Handle invitation messages."
  nil
  (let ((target (first (erc-response.command-args parsed)))
        (chnl (erc-response.contents parsed)))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (setq invitation chnl)
      (when (string= target (erc-current-nick))
        (erc-display-message
         parsed 'notice 'active
         'INVITE ?n nick ?u login ?h host ?c chnl)))))


(define-erc-response-handler (JOIN)
  "Handle join messages."
  nil
  (let ((chnl (erc-response.contents parsed))
        (buffer nil))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      ;; strip the stupid combined JOIN facility (IRC 2.9)
      (if (string-match "^\\(.*\\)?\^g.*$" chnl)
          (setq chnl (match-string 1 chnl)))
      (save-excursion
	(let* ((str (cond
		     ;; If I have joined a channel
		     ((erc-current-nick-p nick)
		      (setq buffer (erc erc-session-server erc-session-port
					nick erc-session-user-full-name
					nil nil
					erc-default-recipients chnl erc-process))
		      (when buffer
			(set-buffer buffer)
			(erc-add-default-channel chnl)
			(erc-send-command (format "MODE %s" chnl)))
		      (erc-with-buffer (chnl proc)
				       (erc-channel-begin-receiving-names))
		      (erc-update-mode-line)
		      (run-hooks 'erc-join-hook)
		      (erc-make-notice
		       (erc-format-message 'JOIN-you ?c chnl)))
		     (t
		      (setq buffer (erc-get-buffer chnl proc))
		      (erc-make-notice
		       (erc-format-message
			'JOIN ?n nick ?u login ?h host ?c chnl))))))
	  (when buffer (set-buffer buffer))
	  (erc-update-channel-member chnl nick nick t nil nil host login)
	  ;; on join, we want to stay in the new channel buffer
	  ;;(set-buffer ob)
	  (erc-display-message parsed nil buffer str))))))

(define-erc-response-handler (KICK)
  "Handle kick messages received from the server." nil
  (let* ((ch (first (erc-response.command-args parsed)))
         (tgt (second (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer ch proc)))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (erc-remove-channel-member buffer tgt)
      (cond
       ((string= tgt (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-you ?n nick ?u login ?h host ?c ch ?r reason)
        (run-hook-with-args 'erc-kick-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc-remove-channel-users))
        (erc-delete-default-channel ch buffer)
        (erc-update-mode-line buffer))
       ((string= nick (erc-current-nick))
        (erc-display-message
         parsed 'notice buffer
         'KICK-by-you ?k tgt ?c ch ?r reason))
       (t (erc-display-message
             parsed 'notice buffer
	   'KICK ?k tgt ?n nick ?u login ?h host ?c ch ?r reason))))))

(define-erc-response-handler (MODE)
  "Handle server mode changes." nil
  (let ((tgt (first (erc-response.command-args parsed)))
        (mode (mapconcat 'identity (cdr (erc-response.command-args parsed))
                         " ")))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (erc-log (format "MODE: %s -> %s: %s" nick tgt mode))
      ;; dirty hack
      (let ((buf (cond ((erc-channel-p tgt)
                        (erc-get-buffer tgt proc))
                       ((string= tgt (erc-current-nick)) nil)
                       ((erc-active-buffer) (erc-active-buffer))
                       (t (erc-get-buffer tgt)))))
        (with-current-buffer (or buf
                                 (current-buffer))
          (erc-update-modes tgt mode nick host login))
          (if (or (string= login "") (string= host ""))
	    (erc-display-message parsed 'notice buf
				 'MODE-nick ?n nick
				 ?t tgt ?m mode)
	  (erc-display-message parsed 'notice buf
			       'MODE ?n nick ?u login
			       ?h host ?t tgt ?m mode)))
      (erc-banlist-update proc parsed))))

(define-erc-response-handler (NICK)
  "Handle nick change messages." nil
  (let ((nn (erc-response.contents parsed))
        bufs)
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (erc-log (format "NICK: %s -> %s" nick nn))
      ;; if we had a query with this user, make sure future messages will be
      ;; sent to the correct nick. also add to bufs, since the user will want
      ;; to see the nick change in the query, and if it's a newly begun query,
      ;; erc-channel-users won't contain it
      (erc-buffer-filter
       (lambda ()
         (when (equal (erc-default-target) nick)
           (setq erc-default-recipients
                 (cons nn (cdr erc-default-recipients)))
           (rename-buffer nn)
           (erc-update-mode-line)
           (add-to-list 'bufs (current-buffer)))))
      (erc-update-user-nick nick nn host nil nil login)
      (cond
       ((string= nick (erc-current-nick))
        (add-to-list 'bufs (erc-server-buffer))
        (erc-set-current-nick nn)
        (erc-update-mode-line)
        (setq erc-nick-change-attempt-count 0)
        (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
        (erc-display-message
         parsed 'notice bufs
         'NICK-you ?n nick ?N nn)
        (run-hook-with-args 'erc-nick-changed-functions nn nick))
       (t
        (erc-handle-user-status-change 'nick (list nick login host) (list nn))
	(erc-display-message parsed 'notice bufs 'NICK ?n nick
			     ?u login ?h host ?N nn))))))

(define-erc-response-handler (PART)
  "Handle part messages." nil
  (let* ((chnl (first (erc-response.command-args parsed)))
         (reason (erc-trim-string (erc-response.contents parsed)))
         (buffer (erc-get-buffer chnl proc)))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (erc-remove-channel-member buffer nick)
      (erc-display-message parsed 'notice buffer
			   'PART ?n nick ?u login
			   ?h host ?c chnl ?r (or reason ""))
      (when (string= nick (erc-current-nick))
        (run-hook-with-args 'erc-part-hook buffer)
        (erc-with-buffer
            (buffer)
          (erc-remove-channel-users))
        (erc-delete-default-channel chnl buffer)
        (erc-update-mode-line buffer)
        (when erc-kill-buffer-on-part
          (kill-buffer buffer))))))

(define-erc-response-handler (PING)
  "Handle ping messages." nil
  (let ((pinger (first (erc-response.command-args parsed))))
    (erc-log (format "PING: %s" pinger))
    ;; ping response to the server MUST be forced, or you can lose big
    (erc-send-command (format "PONG :%s" pinger) t)
    (when erc-verbose-server-ping
      (erc-display-message
       parsed 'error proc
       'PING ?s (erc-time-diff last-ping-time (erc-current-time))))
    (setq last-ping-time (erc-current-time))))

(define-erc-response-handler (PONG)
  "Handle pong messages." nil
  (let ((time (string-to-number (erc-response.contents parsed))))
    (when (> time 0)
      (setq erc-lag (erc-time-diff time (erc-current-time)))
      (when erc-verbose-server-ping
	(erc-display-message
	 parsed 'notice proc 'PONG
         ?h (first (erc-response.command-args parsed)) ?i erc-lag
         ?s (if (/= erc-lag 1) "s" "")))
      (erc-update-mode-line))))

(define-erc-response-handler (PRIVMSG NOTICE)
  nil nil
  (let ((sender-spec (erc-response.sender parsed))
	(cmd (erc-response.command parsed))
	(tgt (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))
    (if (or (erc-ignored-user-p sender-spec)
	    (erc-ignored-reply-p msg tgt proc))
	(when erc-minibuffer-ignored
	  (message "Ignored %s from %s to %s" cmd sender-spec tgt))
      (let* ((sndr (erc-parse-user sender-spec))
	     (nick (nth 0 sndr))
	     (login (nth 1 sndr))
	     (host (nth 2 sndr))
	     (msgp (string= cmd "PRIVMSG"))
	     (noticep (string= cmd "NOTICE"))
	     ;; S.B. downcase *both* tgt and current nick
	     (privp (erc-current-nick-p tgt))
	     s buffer
	     fnick)
	(setf (erc-response.contents parsed) msg)
	(setq buffer (erc-get-buffer (if privp nick tgt) proc))
	(when buffer
	  (with-current-buffer buffer
	    ;; update the chat partner info.  Add to the list if private
	    ;; message.	 We will accumulate private identities indefinitely
	    ;; at this point.
	    (erc-update-channel-member (if privp nick tgt) nick nick
                                       privp nil nil host login nil nil t)
	    (let ((cdata (erc-get-channel-user nick)))
	      (setq fnick (funcall erc-format-nick-function (car cdata) (cdr cdata))))))
	(cond
	 ((erc-is-message-ctcp-p msg)
	  (setq s (if msgp
		      (erc-process-ctcp-query proc parsed nick login host)
		    (erc-process-ctcp-reply proc parsed nick login host
					    (match-string 1 msg)))))
	 (t
	  (setcar last-peers nick)
	  (setq s (erc-format-privmessage (or fnick nick) msg
					  ;; If buffer is a query buffer,
					  ;; format the nick as for a channel.
					  (and (not
						(and
						 buffer
						 (erc-query-buffer-p buffer)
						 erc-format-query-as-channel-p))
                                               privp)
					  msgp))))
	(when s
	  (if (and noticep privp)
	      (progn
		(run-hook-with-args 'erc-echo-notice-always-hook
				    s parsed buffer nick)
		(run-hook-with-args-until-success
		 'erc-echo-notice-hook s parsed buffer nick))
            (erc-display-message parsed nil buffer s)))
        (when (string= cmd "PRIVMSG")
          (erc-auto-query proc parsed))))))

;; FIXME: need clean way of specifiying extra hooks in
;; define-erc-response-handler.
(add-hook 'erc-server-PRIVMSG-functions 'erc-auto-query)

(define-erc-response-handler (QUIT)
  nil nil
  (let ((reason (erc-response.contents parsed))
        bufs)
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (setq bufs (erc-buffer-list-with-nick nick proc))
      (erc-remove-user nick)
      (setq reason (erc-wash-quit-reason reason nick login host))
      (erc-display-message parsed 'notice bufs
			   'QUIT ?n nick ?u login
			   ?h host ?r reason))))

(define-erc-response-handler (TOPIC)
  nil nil
  (let* ((ch (first (erc-response.command-args parsed)))
         (topic (erc-trim-string (erc-response.contents parsed)))
         (time (format-time-string "%T %m/%d/%y" (current-time))))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (erc-update-channel-member ch nick nick nil nil nil host login)
      (erc-update-channel-topic ch (format "%s\C-o (%s, %s)" topic nick time))
      (erc-display-message parsed 'notice (erc-get-buffer ch proc)
			   'TOPIC ?n nick ?u login ?h host
			   ?c ch ?T topic))))

(define-erc-response-handler (WALLOPS)
  nil nil
  (let ((message (erc-response.contents parsed)))
    (multiple-value-bind (nick login host)
        (erc-parse-user (erc-response.sender parsed))
      (erc-display-message
       parsed 'notice nil
       'WALLOPS ?n nick ?m message))))

(define-erc-response-handler (001)
  "Set current-nick to reflect server settings and display the welcome message."
  nil
  (erc-set-current-nick (first (erc-response.command-args parsed)))
  (erc-update-mode-line)                ; needed here?
  (setq erc-nick-change-attempt-count 0)
  (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
  (erc-display-message
   parsed 'notice 'active (erc-response.contents parsed)))

(define-erc-response-handler (MOTD 002 003 371 372 374 375)
  "Display the server's message of the day." nil
  (erc-handle-login)
  (erc-display-message
   parsed 'notice (if erc-connected 'active proc)
   (erc-response.contents parsed)))

(define-erc-response-handler (376 422)
  nil nil
  (erc-server-MOTD proc parsed)
  (erc-connection-established proc parsed))

(define-erc-response-handler (004)
  nil nil
  (multiple-value-bind (server-name server-version)
      (cdr (erc-response.command-args parsed))
    (setq erc-server-version server-version)
    (setq erc-announced-server-name server-name)
    (erc-update-mode-line-buffer (process-buffer proc))
    (erc-display-message
     parsed 'notice proc
     's004 ?s server-name ?v server-version
     ?U (fourth (erc-response.command-args parsed))
     ?C (fifth (erc-response.command-args parsed)))))

(define-erc-response-handler (005)
  "Set the variable `erc-server-parameters' and display the received message.

According to RFC 2812, suggests alternate servers on the network.
Many servers, however, use this code to show which parameters they have set,
for example, the network identifier, maximum allowed topic length, whether
certain commands are accepted and more.	 See documentation for
`erc-server-parameters' for more information on the parameters sent.

A server may send more than one 005 message."
  nil
  (let ((line (mapconcat 'identity
			 (setf (erc-response.command-args parsed)
			       (cdr (erc-response.command-args parsed)))
			 " ")))
    (while (erc-response.command-args parsed)
      (let ((section (pop (erc-response.command-args parsed))))
        ;; fill erc-server-parameters
        (when (string-match "^\\([A-Z]+\\)\=\\(.*\\)$\\|^\\([A-Z]+\\)$"
                            section)
          (add-to-list 'erc-server-parameters
                       `(,(or (match-string 1 section)
                              (match-string 3 section))
                         .
                         ,(match-string 2 section))))))
    (erc-display-message parsed 'notice proc line)))

(define-erc-response-handler (221)
  nil nil
  (let* ((nick (first (erc-response.command-args parsed)))
         (modes (mapconcat 'identity
			   (cdr (erc-response.command-args parsed)) " ")))
    (erc-set-modes nick modes)
    (erc-display-message parsed 'notice 'active 's221 ?n nick ?m modes)))
                         

(define-erc-response-handler (252)
  "Display the number of IRC operators online." nil
  (erc-display-message parsed 'notice 'active 's252
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (253)
  "Display the number of unknown connections." nil
  (erc-display-message parsed 'notice 'active 's253
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (254)
  "Display the number of channels formed." nil
  (erc-display-message parsed 'notice 'active 's254
                       ?i (second (erc-response.command-args parsed))))

(define-erc-response-handler (250 251 255 256 257 258 259 265 266 377 378)
  "Generic display of server messages as notices.

See `erc-display-server-message'." nil
  (erc-display-server-message proc parsed))

(define-erc-response-handler (301)
  "AWAY notice." nil
  (erc-display-message parsed 'notice 'active 's301
                       ?n (second (erc-response.command-args parsed))
                       ?r (erc-response.contents parsed)))

(define-erc-response-handler (303)
  "ISON reply" nil
  (erc-display-message parsed 'notice 'active 's303
                       ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (305)
  "Return from AWAYness." nil
  (erc-process-away proc nil)
  (erc-display-message parsed 'notice 'active
                       's305 ?m (erc-response.contents parsed)))

(define-erc-response-handler (306)
  "Set AWAYness." nil
  (erc-process-away proc t)
  (erc-display-message parsed 'notice 'active
                       's306 ?m (erc-response.contents parsed)))

(define-erc-response-handler (311 314)
  "WHOIS/WHOWAS notices." nil
  (let ((fname (erc-response.contents parsed))
        (catalog-entry (intern (format "s%s" (erc-response.command parsed)))))
    (multiple-value-bind (nick user host)
        (cdr (erc-response.command-args parsed))
      (erc-update-user-nick nick nick host nil fname user)
      (erc-display-message
       parsed 'notice 'active catalog-entry
       ?n nick ?f fname ?u user ?h host))))

(define-erc-response-handler (312)
  nil nil
  (multiple-value-bind (nick server-host)
      (cdr (erc-response.command-args parsed))
    (erc-display-message
     parsed 'notice 'active 's312
     ?n nick ?s server-host ?c (erc-response.contents parsed))))

(define-erc-response-handler (313)
  "IRC Operator response in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's313
   ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (315 318 323 369)
  ;; 315 - End of WHO
  ;; 318 - End of WHOIS list
  ;; 323 - End of channel LIST
  ;; 369 - End of WHOWAS
  nil nil
  (ignore proc parsed))

(define-erc-response-handler (317)
  "IDLE notice." nil
  (multiple-value-bind (nick seconds-idle on-since time)
      (cdr (erc-response.command-args parsed))
    (setq time (when on-since
                 (format-time-string "%T %Y/%m/%d"
                                     (erc-string-to-emacs-time on-since))))
    (erc-update-user-nick nick nick nil nil nil
                          (and time (format "on since %s" time)))
    (if time
        (erc-display-message
         parsed 'notice 'active 's317-on-since
         ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle)) ?t time)
      (erc-display-message
       parsed 'notice 'active 's317
       ?n nick ?i (erc-sec-to-time (string-to-number seconds-idle))))))

(define-erc-response-handler (319)
  nil nil
  (erc-display-message
   parsed 'notice 'active 's319
   ?n (second (erc-response.command-args parsed))
   ?c (erc-response.contents parsed)))

(define-erc-response-handler (320)
  "Identified user in WHOIS." nil
  (erc-display-message
   parsed 'notice 'active 's320
   ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (321)
  "LIST header." nil
  ;; FIXME: Need to change this reference to an erc-prefixed one.
  ;; -- Lawrence 2004/05/10
  (setq channel-list nil)
  (erc-display-message parsed 'notice 'active 's321))

(define-erc-response-handler (322)
  "LIST notice." nil
  (let ((topic (erc-response.contents parsed)))
    (multiple-value-bind (channel num-users)
        (cdr (erc-response.command-args parsed))
      (add-to-list 'channel-list (list channel))
      (erc-update-channel-topic channel topic)
      (erc-display-message
       parsed 'notice 'active 's322
       ?c channel ?u num-users ?t (or topic "")))))

(define-erc-response-handler (324)
  "Channel or nick modes." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (modes (mapconcat 'identity (cddr (erc-response.command-args parsed))
                          " ")))
    (erc-set-modes channel modes)
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's324 ?c channel ?m modes)))

(define-erc-response-handler (329)
  "Channel creation date." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (time (erc-string-to-emacs-time
               (third (erc-response.command-args parsed)))))
    (erc-display-message
     parsed 'notice (erc-get-buffer channel proc)
     's329 ?c channel ?t (format-time-string "%A %Y/%m/%d %X" time))))

(define-erc-response-handler (330)
  nil nil
  ;; FIXME: I don't know what the magic numbers mean.  Mummy, make
  ;; the magic numbers go away.
  ;; No seriously, I have no clue about the format of this command,
  ;; and don't sit on Quakenet, so can't test.  Originally we had:
  ;; nick == (aref parsed 3)
  ;; authaccount == (aref parsed 4)
  ;; authmsg == (aref parsed 5)
  ;; The guesses below are, well, just that. -- Lawrence 2004/05/10
  (let ((nick (second (erc-response.command-args parsed)))
        (authaccount (third (erc-response.command-args parsed)))
        (authmsg (erc-response.contents parsed)))
    (erc-display-message parsed 'notice 'active 's330
                         ?n nick ?a authmsg ?i authaccount)))

(define-erc-response-handler (331)
  "Channel topic." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (topic (erc-response.contents parsed)))
    ;; FIXME: why don't we do anything with the topic? -- Lawrence 2004/05/10
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's331 ?c channel)))

(define-erc-response-handler (332)
  "TOPIC notice." nil
  (let ((channel (second (erc-response.command-args parsed)))
        (topic (erc-response.contents parsed)))
    (erc-update-channel-topic channel topic)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's332 ?c channel ?T topic)))

(define-erc-response-handler (333)
  ;; Who set the topic, and when
  nil nil
  (multiple-value-bind (channel nick time)
      (cdr (erc-response.command-args parsed))
    (setq time (format-time-string "%T %Y/%m/%d"
                                   (erc-string-to-emacs-time time)))
    (erc-update-channel-topic channel
                              (format "\C-o (%s, %s)" nick time)
                              'append)
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's333 ?c channel ?n nick ?t time)))

(define-erc-response-handler (341)
  "Let user know when an INVITE attempt has been sent successfully."
  nil
  (multiple-value-bind (nick channel)
      (cdr (erc-response.command-args parsed))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         's341 ?n nick ?c channel)))

(define-erc-response-handler (352)
  "WHO notice." nil
  (multiple-value-bind (channel user host server nick away-flag)
      (cdr (erc-response.command-args parsed))
    (let ((full-name (erc-response.contents parsed))
          hopcount)
      (when (string-match "\\(^[0-9]+ \\)\\(.*\\)$" full-name)
        (setq hopcount (match-string 1 full-name))
        (setq full-name (match-string 2 full-name)))
      (erc-update-channel-member channel nick nick nil nil nil host user full-name)
      (erc-display-message parsed 'notice 'active 's352
                           ?c channel ?n nick ?a away-flag
                           ?u user ?h host ?f full-name))))

(define-erc-response-handler (353)
  "NAMES notice." nil
  (let ((channel (third (erc-response.command-args parsed)))
        (users (erc-response.contents parsed)))
    (erc-with-buffer (channel proc)
      (erc-channel-receive-names users))
    (erc-display-message parsed 'notice (or (erc-get-buffer channel proc)
                                            'active)
                         's353 ?c channel ?u users)))

(define-erc-response-handler (366)
  "End of NAMES." nil
  (erc-with-buffer ((second (erc-response.command-args parsed)) proc)
    (erc-channel-end-receiving-names)))

(define-erc-response-handler (367)
  "Channel ban list entries" nil
  (multiple-value-bind (channel banmask setter time)
      (cdr (erc-response.command-args parsed))
    (erc-display-message parsed 'notice 'active 's367
                         ?c channel
                         ?b banmask
                         ?s setter
                         ?t time)))

(define-erc-response-handler (368)
  "End of channel ban list" nil
  (let ((channel (second (erc-response.command-args parsed))))
    (erc-display-message parsed 'notice 'active 's368
                         ?c channel)))

(define-erc-response-handler (379)
  "Forwarding to another channel." nil
  ;; FIXME: Yet more magic numbers in original code, I'm guessing this
  ;; command takes two arguments, and doesn't have any "contents". --
  ;; Lawrence 2004/05/10
  (multiple-value-bind (from to)
      (cdr (erc-response.command-args parsed))
    (erc-display-message parsed 'notice 'active
                         's379 ?c from ?f to)))

(define-erc-response-handler (391)
  "Server's time string" nil
  (erc-display-message
   parsed 'notice 'active
   's391 ?s (second (erc-response.command-args parsed))
   ?t (third (erc-response.command-args parsed))))

(define-erc-response-handler (401)
  "No such nick/channel." nil
  (let ((nick/channel (second (erc-response.command-args parsed))))
    (when erc-whowas-on-nosuchnick
      (erc-log (format "cmd: WHOWAS: %s" nick/channel))
      (erc-send-command (format "WHOWAS %s 1" nick/channel)))
    (erc-display-message parsed '(notice error) 'active
                         's401 ?n nick/channel)))

(define-erc-response-handler (403)
  "No such channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's403 ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (404)
  "Cannot send to channel." nil
  (erc-display-message parsed '(notice error) 'active
                       's404 ?c (second (erc-response.command-args parsed))))


(define-erc-response-handler (405)
  ;; Can't join that many channels.
  nil nil
  (erc-display-message parsed '(notice error) 'active
                       's405 ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (406)
  ;; No such nick
  nil nil
  (erc-display-message parsed '(notice error) 'active
                       's406 ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (412)
  ;; No text to send
  nil nil
  (erc-display-message parsed '(notice error) 'active 's412))

(define-erc-response-handler (421)
  ;; Unknown command
  nil nil
  (erc-display-message parsed '(notice error) 'active 's421
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (432)
  ;; Bad nick.
  nil nil
  (erc-display-message parsed '(notice error) 'active 's432
                       ?n (second (erc-response.command-args parsed))))

(define-erc-response-handler (433)
  ;; Login-time "nick in use"
  nil nil
  (erc-nickname-in-use (second (erc-response.command-args parsed))
                       "already in use"))

(define-erc-response-handler (437)
  ;; Nick temporarily unavailable (IRCnet)
  nil nil
  (let ((nick/channel (second (erc-response.command-args parsed))))
    (unless (erc-channel-p nick/channel)
      (erc-nickname-in-use nick/channel "temporarily unavailable"))))

(define-erc-response-handler (442)
  ;; Not on channel
  nil nil
  (erc-display-message parsed '(notice error) 'active 's442
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (461)
  ;; Not enough params for command.
  nil nil
  (erc-display-message parsed '(notice error)  'active 's461
                       ?c (second (erc-response.command-args parsed))
                       ?m (erc-response.contents parsed)))

(define-erc-response-handler (474)
  "Banned from channel errors" nil
  (erc-display-message parsed '(notice error) nil
                       (intern (format "s%s"
                                       (erc-response.command parsed)))
                       ?c (second (erc-response.command-args parsed))))

(define-erc-response-handler (475)
  "Channel key needed." nil
  (erc-display-message parsed '(notice error) nil 's475
                       ?c (second (erc-response.command-args parsed)))
  (when erc-prompt-for-channel-key
    (let ((channel (second (erc-response.command-args parsed)))
          (key (read-from-minibuffer
                (format "Channel %s is mode +k.  Enter key (RET to cancel): "
                        (second (erc-response.command-args parsed))))))
      (when (and key (> (length key) 0))
          (erc-cmd-JOIN channel key)))))

(define-erc-response-handler (477)
  nil nil
  (let ((channel (second (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed 'notice (erc-get-buffer channel proc)
                         (format "%s: %s" channel message))))                       

(define-erc-response-handler (482)
  nil nil
  (let ((channel (second (erc-response.command-args parsed)))
        (message (erc-response.contents parsed)))
    (erc-display-message parsed '(error notice) 'active 's482
                         ?c channel ?m message)))

(define-erc-response-handler (431 445 446 451 462 463 464 465 481 483 484 485
				  491 501 502)
  ;; 431 - No nickname given
  ;; 445 - SUMMON has been disabled
  ;; 446 - USERS has been disabled
  ;; 451 - You have not registered
  ;; 462 - Unauthorized command (already registered)
  ;; 463 - Your host isn't among the privileged
  ;; 464 - Password incorrect
  ;; 465 - You are banned from this server
  ;; 481 - Need IRCop privileges
  ;; 483 - You can't kill a server!
  ;; 484 - Your connection is restricted!
  ;; 485 - You're not the original channel operator
  ;; 491 - No O-lines for your host
  ;; 501 - Unknown MODE flag
  ;; 502 - Cannot change mode for other users
  nil nil
  (erc-display-error-notice
   parsed
   (intern (format "s%s" (erc-response.command parsed)))))

;; FIXME: These are yet to be implemented, they're just stubs for now
;; -- Lawrence 2004/05/12

;; response numbers left here for reference

;; (define-erc-response-handler (323 364 365 381 382 392 393 394 395
;;                               200 201 202 203 204 205 206 208 209 211 212 213
;;                               214 215 216 217 218 219 241 242 243 244 249 261
;;                               262 302 342 351 402 407 409 411 413 414 415
;;                               423 424 436 441 443 444 467 471 472 473 KILL)
;;   nil nil
;;   (ignore proc parsed))
(provide 'erc-backend)

;;; erc-backend.el ends here
