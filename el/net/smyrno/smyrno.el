;;; smyrno.el --- a Jabber client for Emacs

;; Copyright (C) 2002 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: comm, tools, processes

;; This file is part of Smyrno, a Jabber client for Emacs.

;; Smyrno is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Smyrno is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Look Ma, no comments!
;; [several months later: i'm such a bastard.]

;;; Code:

(require 'format-spec)
(require 'xml)

(require 'xmpp)

(define-xmpp-client smyrno (cadr (split-string "$Revision: 1.9 $"))
  "Smyrno is an extensible Jabber client modeled after ERC.")

(defgroup smyrno nil
  "a Jabber client for Emacs."
  :link '(url-link "http://oconnor.cx/smyrno/")
  :link '(url-link "http://www.nongnu.org/smyrno/")
  :prefix "smyrno-"
  :group 'processes)

(defcustom smyrno-startup-file "~/.smyrno.el"
  "The startup file Smyrno should load."
  :type '(file :tag "Startup file")
  :group 'smyrno)

(defun smyrno-ensure-startup-file-loaded ()
  "Ensure that the Smyrno starup file has been loaded."
  ;; I wonder if this check is portable.
  (unless (assoc smyrno-startup-file load-history)
    (load smyrno-startup-file t)))

(defcustom smyrno-server "talk.google.com"
  "The Jabber server you'd like to connect to."
  :type '(string :tag "Hostname")
  :group 'smyrno)

(defcustom smyrno-port 5222
  "The port you'd like to connect to."
  :type '(number :tag "Port")
  :group 'smyrno)

(defcustom smyrno-username "abhijithg"
  "The Jabber user name you'd like to use."
  :type '(string :tag "Username")
  :group 'smyrno)

(defcustom smyrno-password nil
  "Your password. Yeah, this sucks, and yeah, it'll be better."
  :type '(string :tag "Password")
  :group 'smyrno)

(defcustom smyrno-resource "smyrno"
  "The resouce you'd like to be online as."
  :type '(string :tag "Resource")
  :group 'smyrno)

(defcustom smyrno-show 'chat
  "Your default presence."
  :type '(choice (const 'chat   :tag "Available for chat")
                 (const 'normal :tag "Normal")
                 (const 'away   :tag "Away")
                 (const 'dnd    :tag "Do Not Disturb")
                 (const 'xa     :tag "Extended Away"))
  :group 'smyrno)

(defcustom smyrno-status "Trying out Smryno"
  "Your default presence reason string."
  :type 'string
  :group 'smyrno)

(defcustom smyrno-prompt ">"
  "The prompt that you'd like Smyrno to use.
You don't need to have a trailing space."
  :type '(string :tag "Prompt")
  :group 'smyrno)

(defcustom smyrno-paranoia-flag nil
  "How paranoid should Smyrno be?
This is used to determine how we should behave when other people
ask us for our client version, the time, and other such
unimportant things."
  :type '(choice (const t :tag "Ignore requests for information.")
                 (const 'ask :tag "Ask before giving out information.")
                 (const nil :tag "Freely give out information."))
  :group 'smyrno)

(defcustom smyrno-paranoia-timeout 30
  "Number of seconds Smyrno should wait before respnding when
`smyrno-paranoia-flag' is `ask'."
  :type '(integer)
  :group 'smyrno)

;; These aren't actually being used yet.

(defgroup smyrno-faces nil
  "Faces used by Smryno."
  :group 'smyrno)

;; Yes, the face default values are inspired by ERC (and Exodus).

(defface smyrno-prompt-face '((t (:foreground "Black"
                                  :background "lightblue2"
                                  :bold t)))
  "The face with which the Smyrno prompt is highlighted in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-notice-face '((t (:foreground "SlateBlue" :bold t)))
  "The face with which notices should be displayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-nick-face '((t (:foreground "Red" :bold t)))
  "The face with which nicks should be displayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-self-face '((t (:foreground "Blue" :bold t)))
  "The face with which your own nick should be displayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-time-face '((t (:foreground "Green" :bold t)))
  "The face with which timestamps should be displayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-action-face '((t (:bold t)))
  "The face with which action messages should be displayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-input-face '((t (:foreground "brown")))
  "The face with which your messages should be dispayed in chat buffers."
  :group 'smyrno-faces)

(defface smyrno-default-face '((t ()))
  ""
  :group 'smyrno-faces)

(defvar smyrno-init nil
  "Have we already initialized Smyrno?")

(defun smyrno-init ()
  "Insinuate Smyrno functionality into xmpp.el."
  (unless smyrno-init
    (add-hook 'xmpp-received-normal-message-hook 'smyrno-receive-normal-message)
    (add-hook 'xmpp-received-chat-message-hook 'smyrno-receive-chat-message)
    (add-hook 'xmpp-received-groupchat-message-hook 'smyrno-receive-groupchat-message)
    (add-hook 'xmpp-received-headline-message-hook 'smyrno-receive-headline-message)
    (add-hook 'xmpp-received-error-message-hook 'smyrno-receive-error-message)
    (add-hook 'xmpp-received-unknown-message-hook 'smyrno-receive-unknown-message))
  (setq smryno-init t))

(defun smyrno-select (server port username password resource)
  ""
  (interactive "sJabber server: \nnPort: \nsUsername: \nsPassword: \nsResource: \n")
  (let ((server   (if (string-equal server   "") smyrno-server   server))
        (port     (if (string-equal port     "") smyrno-port     port))
        (username (if (string-equal username "") smyrno-username username))
        (password (if (string-equal password "") smyrno-password password))
        (resource (if (string-equal resource "") smyrno-resource resource)))
    (smyrno-connect server port username password resource)))

(defun smyrno-connect (server port username password resource)
  "Connect to SERVER on PORT as USERNAME with PASSWORD on RESOURCE."
  (smyrno-init)
  (interactive (list smyrno-server smyrno-port
                     smyrno-username smyrno-password
                     smyrno-resource))
  (xmpp-connect server port)
  (xmpp-login username password resource)
  (xmpp-set-presence smyrno-show smyrno-status))

(defun smyrno-close ()
  (interactive)
  (xmpp-close))



(defcustom smyrno-announce-format
  "Message from %f in %B!"
  ""
  :type '(string)
  :group 'smyrno)

(defun smyrno-announce-message (message)
  ""
  (message "%s"
    (format-spec smyrno-announce-format
                 `((?f . ,(jabber-jid-to-string (jabber-message-from message)))
                   (?B . ,(buffer-name))))))

(defun smyrno-receive-message (message)
  ""
  (let ((old-buffer (current-buffer)))
    (with-current-buffer (smyrno-get-buffer message)
      (unless (eq old-buffer (current-buffer))
        (smyrno-announce-message message))
      (smyrno-add-message message))))

(defalias 'smyrno-receive-normal-message 'smyrno-receive-message)
(defalias 'smyrno-receive-groupchat-message 'smyrno-receive-message)
(defalias 'smyrno-receive-chat-message 'smyrno-receive-message)
(defalias 'smyrno-receive-error-message 'smyrno-receive-message)
(defalias 'smyrno-receive-unknown-message 'smyrno-receive-message)

(defun smyrno-receive-headline-message (message)
  (smyrno-display-headline message))

(defcustom smyrno-headline-format
  "%b"
  ""
  :type '(string)
  :group 'smyrno)

(defun smyrno-display-headline (message)
  "Display MESSSAGE as a headline."
  (message "%s"
    (format-spec smyrno-headline-format
                 `((?b . ,(jabber-message-body message))))))



;;; Chat support

(defgroup smyrno-chat nil
  ""
  :group 'smyrno)

(defcustom smyrno-chat-header-line-format
  "[Jabber] Chatting with %j"
  ""
  :type '(string)
  :group 'smyrno-chat)

(defvar smyrno-chat-mode-map (make-sparse-keymap)
  "Keymap for `smyrno-chat-mode'.")

(define-key smyrno-chat-mode-map (kbd "RET")
  'smyrno-chat-send-line)

(defun smyrno-chat-mode ()
  "Major mode for Jabber chat and groupchat sessions."
  (kill-all-local-variables)
  (setq major-mode 'smyrno-chat-mode
        mode-name "Jabber Chat")
  (use-local-map smyrno-chat-mode-map))

(defcustom smyrno-chat-action-prefix
  "***"
  ""
  :type 'string
  :group 'smyrno-chat)

(defcustom smyrno-chat-action-format
  "[%t] %N %u %b"
  ""
  :type 'string
  :group 'smyrno-chat)

(defcustom smyrno-chat-message-format
  "[%t] <%u> %b"
  ""
  :type 'string
  :group 'smyrno-chat)

(defun smyrno-chat-message-format (message selfp)
  (jabber-message-bind message
    (let ((actionp (string-match "^[/]me[ ]" body)))
      (when actionp
        (setq body (substring body 4)))
      (format-spec
       (if actionp
           smyrno-chat-action-format
         smyrno-chat-message-format)
       `((?N . ,(propertize smyrno-chat-action-prefix
                            'face (cond (actionp 'smyrno-action-face)
                                        (t 'smyrno-default-face))))
         (?u . ,(propertize (jabber-jid-user from)
                            'face (cond (actionp 'smyrno-action-face)
                                        (selfp 'smyrno-self-face)
                                        (t 'smyrno-nick-face))
                            'rear-nonsticky t))
         (?h . ,(jabber-jid-host from))
         (?r . ,(let ((r (jabber-jid-resource from)))
                  (if r
                      (concat "/" r)
                    "")))
         (?j . ,(propertize (jabber-jid-to-string from)
                            'face (cond (actionp 'smyrno-action-face)
                                        (selfp 'smyrno-self-face)
                                        (t 'smyrno-nick-face))
                            'rear-nonsticky t))
         (?b . ,(propertize body
                            'face (cond (actionp 'smyrno-action-face)
                                        (selfp 'smyrno-input-face)
                                        (t 'smyrno-default-face))))
         (?t . ,(propertize (format-time-string "%X" date)
                            'face 'smyrno-time-face
                            'rear-nonsticky t)))))))

(defun smyrno-add-message (message)
  "Add MESSAGE to the current Smyrno chat buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-quote smyrno-prompt))
    (beginning-of-line)
    (let ((start (point-marker))
          (end nil))
      (insert (smyrno-chat-message-format message nil) "\n")
    (setq end (point-marker))
    (save-restriction
      (narrow-to-region start end)
      (run-hooks 'smyrno-chat-insert-modify-hook)))))

(defcustom smyrno-chat-insert-modify-hook '()
  "Hook to be run when adding content to a smyrno chat buffer."
  :type 'hook
  :group 'smyrno-chat)

(defun smyrno-chat-send-line ()
  (interactive)
  (goto-char (point-max))
  (search-backward smyrno-prompt (point-min) t 1)
  (let ((beginning-of-prompt (point))
        (before-whitespace nil))
    (condition-case nil
        (backward-char 1)
      (error nil))
    (search-forward smyrno-prompt (point-max) t 1)
    (let ((inhibit-read-only t))
      (delete-region beginning-of-prompt (point)))
    (setq before-whitespace (point))
    ;; Skip over whitespace after the prompt. I'm not entirely
    ;; happy about this behavior, but it seems like Exodus nukes
    ;; your leading whitespace too, so whatever.
    ;; (skip-chars-forward " ")
    (when (looking-at " ")
      (forward-char 1))
    (let ((msg (jabber-message-new))
          (xml-sexp nil))
      (jabber-message-type-set   msg 'chat)
      (jabber-message-thread-set msg smyrno-thread)
      (jabber-message-to-set     msg smyrno-chat-other)
      (jabber-message-from-set   msg (jabber-jid-new smyrno-username
                                                     smyrno-server
                                                     smyrno-resource))
      (jabber-message-body-set   msg
                                 (buffer-substring-no-properties
                                  (point)
                                  (point-max)))
      (let ((inhibit-read-only t))
        (delete-region before-whitespace (point-max)))
      (let ((start (point-marker))
            (end nil))
        (insert (smyrno-chat-message-format msg t) "\n")
        (setq end (point-marker))
        (save-restriction
          (narrow-to-region start end)
          (run-hooks 'smyrno-chat-insert-modify-hook)))
      (setq xml-sexp (jabber-message-to-xml-sexp msg))
      (xmpp-send xml-sexp))
    (smyrno-chat-insert-prompt)))

(defun smyrno-chat-insert-prompt ()
  (goto-char (point-max))
  (insert (propertize (concat (propertize smyrno-prompt
                                          'face 'smyrno-prompt-face)
                              " ")
                      'read-only t
                      'front-nonsticky t
                      'rear-nonsticky t)))

(defun smyrno-get-buffer (message)
  "Fetch or create a chat buffer for MESSAGE."
  (let ((thread (jabber-message-thread message))
        (from (jabber-message-from message))
        (the-buffer nil))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (and (boundp 'smyrno-thread)
                         (string-equal smyrno-thread thread))
                (setq the-buffer buffer))))
          (buffer-list))

    (setq thread (or thread (smyrno-generate-thread-id message)))

    (or the-buffer
        (let ((the-buffer (generate-new-buffer (concat "*jabber-chat-"
                                                       thread "*"))))
          (with-current-buffer the-buffer
            (goto-char (point-min))
            (smyrno-chat-mode)
            (smyrno-chat-insert-prompt)
            (setq header-line-format
                  (format-spec smyrno-chat-header-line-format
                               (list (cons ?j (jabber-jid-to-string from)))))
            (set (make-local-variable 'smyrno-chat-other) from)
            (set (make-local-variable 'smyrno-thread) thread))
          the-buffer))))

(defun smyrno-generate-thread-id (message)
  "Generate a thread ID based on MESSAGE."
  (if (string-equal
       (car (split-string (jabber-jid-host (jabber-message-from message))
                          "[.]"))
       "aim")
      ;; Work around the AIM transport's threading issues.
      (downcase jid-string)
    ;; Do we want to expose `xmpp-unique-id'?
    (xmpp-unique-id)))

(defun smyrno-chat (jid-string)
  "Start a chat session with JID-STRING."
  (interactive "sChat with: ")
  (let ((dummy (jabber-message-new))
        (other (jabber-jid-from-string jid-string))
        (thread nil))
    (jabber-message-from-set dummy other)
    (jabber-message-thread-set dummy (smyrno-generate-thread-id message))
    (switch-to-buffer (smyrno-chat-get-buffer dummy))))



;;; Status handling.

(defun smyrno-set-status-away (reason)
  "Set status to away, with REASON."
  (interactive "sReason: ")
  (xmpp-set-presence smyrno-connection 'away reason))

(defun smyrno-set-status-chat (reason)
  "Set status to chat, with REASON."
  (interactive "sReason: ")
  (xmpp-set-presence smyrno-connection 'chat reason))

(defun smyrno-set-status-dnd (reason)
  "Set status to dnd, with REASON."
  (interactive "sReason: ")
  (xmpp-set-presence smyrno-connection 'dnd reason))

(defun smyrno-set-status-normal (reason)
  "Set status to normal, with REASON."
  (interactive "sReason: ")
  (xmpp-set-presence smyrno-connection 'normal reason))

(defun smyrno-set-status-xa (reason)
  "Set status to xa, with REASON."
  (interactive "sReason: ")
  (xmpp-set-presence smyrno-connection 'xa reason))

;; Keymap

(defvar smyrno-global-map (make-sparse-keymap)
  "Keymap for interacting with Smyrno.")

(suppress-keymap smyrno-global-map)
(define-key smyrno-global-map (kbd "a") 'smyrno-set-status-away)
(define-key smyrno-global-map (kbd "c") 'smyrno-set-status-chat)
(define-key smyrno-global-map (kbd "d") 'smyrno-set-status-dnd)
(define-key smyrno-global-map (kbd "n") 'smyrno-set-status-normal)
(define-key smyrno-global-map (kbd "x") 'smyrno-set-status-xa)
(define-key smyrno-global-map (kbd "q") 'smyrno-close)
(define-key smyrno-global-map (kbd "l") 'smyrno-connect)
(define-key smyrno-global-map (kbd "r") 'smyrno-view-roster)
(define-key smyrno-global-map (kbd "m") 'smyrno-chat)

;; Suggested customization
;; (global-set-key (kbd "C-c s") smyrno-global-map)

(defun smyrno-view-roster ()
  ""
  (interactive)
  )



;;; Roster view

(defgroup smyrno-roster nil
  ""
  :group 'smyrno)

(defcustom smyrno-roster-entry-format
  "%t %n (%j)"
  "*Format string to be used when viewing your roster.
Valid format directives are:

        t - subscription type indicator, which is one of these characters:
                + - both
                > - You're subscribed to them, but not the other way
                < - They're subscribed to you, but not the other way
                - - Not subscribed
        n - nick
        j - jid
        s - current status (short)
        S - current status (long)"
  :type 'string
  :group 'smyrno-roster)

(defvar smyrno-roster-view-mode-map (make-sparse-keymap)
  "Keymap for `smyrno-roster-view-mode'.")

(define-key smyrno-roster-view-mode-map (kbd "g") 'smyrno-roster-refresh)

(defun smyrno-roster-view-mode ()
  "Major mode for viewing your Jabber roster."
  (kill-all-local-variables)
  (setq major-mode 'smyrno-roster-view-mode
        mode-name "Jabber Roster"))

(defun smyrno-roster-refresh ()
  ""
  (interactive)
  )



;;; Bug reporting.

(if (fboundp 'rfc822-goto-eoh)
    (defalias 'smyrno-rfc822-goto-eoh 'rfc822-goto-eoh)
  ;; From GNU Emacs 21.2's simple.el.
  (defun smyrno-rfc822-goto-eoh ()
    ;; Go to header delimiter line in a mail message, following RFC822 rules
    (goto-char (point-min))
    (while (looking-at "^[^: \n]+:\\|^[ \t]")
      (forward-line 1))
    (point)))

(if (fboundp 'mail-header-end)
    (defalias 'smyrno-mail-header-end 'mail-header-end)
  ;; From GNU Emacs 21.2's sendmail.el.
  (defun smyrno-mail-header-end ()
    "Return the buffer location of the end of headers, as a number."
    (save-restriction
      (widen)
      (save-excursion
        (smyrno-rfc822-goto-eoh)
        (point)))))

(defconst smyrno-bug-report-email-address
  "ted@oconnor.cx"
  "Address to which to send Smyrno bugs.")

;;;###autoload
(defun smyrno-report-bug (subject)
  "Report a bug in Smyrno, with SUBJECT."
  (interactive "sBug report subject: ")
  (compose-mail smyrno-bug-report-email-address subject)
  (goto-char (smyrno-mail-header-end))
  (forward-line 1)
  (insert
   "Please write in English.\n"
   "\n"
   "Your bug report will be posted to the smyrno-bug mailing list:\n"
   "\t" smyrno-bug-report-email-address "\n"
   "\n"
   "In " (emacs-version) "\n"
   "Smyrno: " xmpp-client-version "\n"

   (let ((messages '()))
     (with-current-buffer (get-buffer "*Messages*")
       (goto-char (point-min))
       (while (re-search-forward "^error in process filter\\(.*\\)$" nil t)
         (add-to-list 'messages (match-string 1)))
       (goto-char (point-max)))
     (mapc (lambda (m) (insert "PFE: " m "\n"))
           messages))

   "\n"
   "Please describe exactly what actions triggered the bug\n"
   "and the precise symptoms of the bug:\n\n")
  ;; This is so the user has to type something in order to send
  ;; easily.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map))
  nil)



;;; Misc.

(smyrno-ensure-startup-file-loaded)



(provide 'smyrno)
;;; smyrno.el ends here
