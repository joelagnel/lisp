;;; xmpp.el --- XMPP (Jabber) protocol support for Emacs

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

;; This file, xmpp.el, provides an Emacs Lisp implementation of
;; the eXtensible Messaging and Presence Protocol (XMPP),
;; otherwise known as the Jabber protocol. Using this file, you
;; should be able to easily build a specialized Jabber client to
;; suit your needs.

;;; Code:

(require 'xml)
(require 'timer)

(require 'xml-stream)
(require 'jabber-objs)

(condition-case nil
    (require 'smyrno-hack)
  (error
   (defun xmpp-log (&rest args)
     nil)))

(defvar xmpp-process nil
  "The process via which we're connected to the Jabber server.")

(defvar xmpp-nick-hash (makehash 'equal))
(defvar xmpp-jid-hash (makehash 'equal))

(defun xmpp-hash-keys-alist (hash)
  (let ((retval '()))
    (maphash (lambda (key ignored-value)
               (add-to-list 'retval (cons key key)))
             hash)
    (nreverse retval)))

(defvar xmpp-received-normal-message-hook '())
(defvar xmpp-received-chat-message-hook '())
(defvar xmpp-received-groupchat-message-hook '())
(defvar xmpp-received-headline-message-hook '())
(defvar xmpp-received-error-message-hook '())
(defvar xmpp-received-unknown-message-hook '())

(defun xmpp-message-handler (message)
  "Handle the Jabber MESSAGE we just received."
  (jabber-message-bind message
    (cond ((eq type 'normal)
           (run-hook-with-args 'xmpp-received-normal-message-hook message))
          ((eq type 'chat)
           (run-hook-with-args 'xmpp-received-chat-message-hook message))
          ((eq type 'groupchat)
           (run-hook-with-args 'xmpp-received-groupchat-message-hook message))
          ((eq type 'headline)
           (run-hook-with-args 'xmpp-received-headline-message-hook message))
          ((eq type 'error)
           (run-hook-with-args 'xmpp-received-error-message-hook message))
          (t
           (run-hook-with-args 'xmpp-received-unknown-message-hook message)))))

;;     (if (and (eq type 'chat) thread)
;;         (with-current-buffer (smyrno-chat-get-buffer thread from)
;;           (smyrno-chat-add-message message))
;;       (with-current-buffer (generate-new-buffer "*xmpp-message*")
;;         (smyrno-message-view-mode)
;;         (set (make-local-variable 'smyrno-from) from)
;;         (run-hooks 'smyrno-message-view-display-hook)
;;         (delete-region (point-min) (point-max))
;;         (insert (format "Jabber message from %s:\n\n" smyrno-from)
;;                 (format "%s\n" body))
;;         (pop-to-buffer (current-buffer))))))

(defun xmpp-expand-show (show)
  (cond ((eq show 'chat) "wants to chat")
        ((eq show 'normal) "normal")
        ((eq show 'away) "away")
        ((eq show 'dnd) "do not disturb")
        ((eq show 'xa) "extended away")
        (t (format "%s" show))))

(defun xmpp-presence-handler (presence)
  (jabber-presence-bind presence
    (if (eq type 'error)
        (message "Error presence packet from %s" (jabber-jid-to-string from))
      (message "%s is now %s (%s with reason %S)"
               (jabber-jid-to-string from)
               type
               (xmpp-expand-show show)
               ;; Handle AIM message format strings. w00t.
               (format-spec status
                            `((?n . ,(jabber-jid-user to))
                              (?d . ,(format-time-string "%x"))
                              (?t . ,(format-time-string "%X"))))))))

(defun xmpp-get-client-version (jid-string)
  (interactive "sWho: ")
  (xmpp-send `(iq ((type . get)
                   (to . ,jid-string)
                   (id . ,(xmpp-unique-id)))
                  (query ((xmlns . "jabber:iq:version"))))))

(defun xmpp-iq-result-jabber:iq:version-handler (iq)
  (jabber-iq-bind iq
    (let ((children (xml-node-children query))
          (name "")
          (version "")
          (os ""))
      (mapc (lambda (child)
              (when (listp child)
                (let ((node-name (xml-node-name child))
                      (node-children (xml-node-children child)))
                  (when (memq node-name '(name version os))
                    (set node-name
                         (mapconcat 'identity node-children ""))))))
            children)
      (message
       (format "%s is using %s version %s on %s"
               (jabber-jid-to-string from)
               name
               version
               os)))))

(defun xmpp-iq-get-jabber:iq:version-handler (iq)
  "Report our Jabber client version to the requesting agent."
  (jabber-iq-bind iq
    (when (or (not smyrno-paranoia-flag)
              (and (eq smyrno-paranoia-flag 'ask)
                   (y-or-n-p-with-timeout
                    (format "Tell %s your client version? "
                            (jabber-jid-to-string from))
                    smyrno-paranoia-timeout
                    t)))
      (xmpp-send `(iq ((type . result)
                       (to . ,(jabber-jid-to-string from))
                       (id . ,id))
                      (query ((xmlns . "jabber:iq:version"))
                             (name nil ,(symbol-name xmpp-client-name))
                             (version nil ,xmpp-client-version)
                             (os nil ,(emacs-version))))))))

(defun xmpp-iq-get-jabber:iq:time-handler (iq)
  "Report our local time to the requesting agent."
  (jabber-iq-bind iq
    (when (or (not smyrno-paranoia-flag)
              (and (eq smyrno-paranoia-flag 'ask)
                   (y-or-n-p-with-timeout
                    (format "Tell %s your time? "
                            (jabber-jid-to-string from))
                    smyrno-paranoia-timeout
                    t)))
      (xmpp-send `(iq ((type . result)
                       (to . ,(jabber-jid-to-string from))
                       (id . ,id))
                      (query ((xmlns . "jabber:iq:time"))
                             (utc nil ,(format-time-string "%Y%m%dT%H:%M:%S"
                                                           (current-time) t))
                             (tz nil ,(format-time-string "%Z"))
                             (display nil ,(format-time-string "%c"))))))))

(defun xmpp-iq-result-jabber:iq:roster-handler (iq)
  "Update our roster from the server's information."
  ;; (clrhash xmpp-jid-hash)
  ;; (clrhash xmpp-nick-hash)
  (let ((roster-items (xml-node-children (jabber-iq-query iq))))
    (mapc (lambda (roster-item)
            (let* ((attrs (xml-node-attributes roster-item))
                  (children (xml-node-children roster-item))
                  (subscription (cdr (assq 'subscription attrs)))
                  (name (cdr (assq 'name attrs)))
                  (jidstr (cdr (assq 'jid attrs)))
                  (jid (jabber-jid-from-string jidstr))
                  (groups '()))
              (mapc (lambda (child)
                      (when (eq (xml-node-name child) 'group)
                        (add-to-list 'groups
                                     (apply 'concat
                                            (xml-node-children child)))))
                    children)
              (let ((entry (jabber-roster-entry-new jid name subscription
                                                    groups)))
                (puthash name entry xmpp-nick-hash)
                (puthash jidstr entry xmpp-jid-hash))))
          roster-items)))

(defun xmpp-iq-default-handler (iq)
  (message "Received IQ %S" iq))

(defun xmpp-iq-handler (iq)
  "Handle the IQ packet we just received."
  (jabber-iq-bind iq
    (cond ((member type '(result get set result error))
           (let* ((attrs (xml-node-attributes query))
                  (xmlns (cdr (assq 'xmlns attrs)))
                  (default-sym (intern (concat "xmpp-iq-"
                                               (symbol-name type)
                                               "-default-handler")))
                  (handler-sym (intern (concat "xmpp-iq-"
                                               (symbol-name type)
                                               "-"
                                               xmlns
                                               "-handler"))))
             (if (fboundp handler-sym)
                 (funcall handler-sym iq)
                 (xmpp-log "No handler defined: %s" default-sym)
               (if (fboundp default-sym)
                   (funcall default-sym iq)
                 (xmpp-log "No default handler defined: %s" default-sym)
                 (xmpp-iq-default-handler iq)))))
          (t
           (xmpp-log "Strange IQ type: %S" iq)))))

(defun xmpp-error-handler (error)
  (xmpp-log "Received errpr %S" error))

(defun xmpp-default-handler (data)
  (message "Received unknown data %S" data))

(defun xmpp-xs-callback (data &rest ignore)
  "Handle incoming DATA."
  (cond ((eq (car data) 'message)
         (xmpp-message-handler (jabber-message-from-xml-sexp data)))
        ((eq (car data) 'presence)
         (xmpp-presence-handler (jabber-presence-from-xml-sexp data)))
        ((eq (car data) 'iq)
         (xmpp-iq-handler (jabber-iq-from-xml-sexp data)))
        ((eq (car data) 'stream:error)
         (xmpp-error-handler (jabber-error-from-xml-sexp data)))
        (t
         (xmpp-default-handler data))))

(defmacro xmpp-send (object)
  `(xml-stream-send xmpp-process ,object))

;;;###autoload
(defun xmpp-connect (hostname &optional port)
  "Connect to the XMPP server on HOSTNAME's PORT."
  (setq xmpp-process
        (xml-stream-open-network-stream
         "XMPP"
         (generate-new-buffer " *xmpp*")
         hostname
         (or port 5222)
         '((xmlns . "jabber:client"))
         'xmpp-xs-callback)))

;;;###autoload
(defun xmpp-close ()
  (xml-stream-close xmpp-process)
  (setq xmpp-process nil))

(defvar xmpp-unique-id 0)

(defun xmpp-unique-id ()
  ;; Not that we expect anything to go wrong with a simple call to
  ;; `format', it's just quite essential that the ID be unique.
  ;; Yay paranoia.
  (unwind-protect (format "xmpp-el-unique-id-%d" xmpp-unique-id)
    (setq xmpp-unique-id (+ xmpp-unique-id 1))))

;;;###autoload
(defun xmpp-login (username password resource)
  (setq xmpp-last-login-unique-id (xmpp-unique-id))
  (xmpp-send `(iq ((type . "set")
                   (id . ,xmpp-last-login-unique-id))
                  (query ((xmlns . "jabber:iq:auth"))
                         (username nil ,username)
                         (password nil ,password)
                         (resource nil ,resource)))))

(defun xmpp-set-presence (show status)
  (xmpp-send `(presence ((id . ,(xmpp-unique-id)))
                        (show nil ,show)
                        (status nil ,status))))

(defun xmpp-message (message)
  (jabber-message-id-set message (xmpp-unique-id))
  (xmpp-send (jabber-message-to-xml-sexp message)))

(defun xmpp-get-roster ()
  (xmpp-send `(iq ((type . "get")
                   (id . ,(xmpp-unique-id)))
                  (query ((xmlns . "jabber:iq:roster"))))))

(defun xmpp-jid-no-resource (jid-string)
  ""
  (car (split-string jid-string "/")))

(defmacro define-xmpp-client (name version &optional docstring)
  `(progn
     (setq xmpp-client-name ',name
           xmpp-client-version ,version)
     (put ',name 'xmpp-client-documentation ,docstring)))

(provide 'xmpp)
;;; xmpp.el ends here
