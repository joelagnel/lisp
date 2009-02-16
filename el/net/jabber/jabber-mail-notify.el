;;; jabber-mail-notify.el --- jabber extension for gmail notification

;; Copyright (C) 2008 Joel Agnel Fernandes
;; Author: Joel Agnel Fernandes <joel@hackerbliss.org>
;; Maintainer: Joel Agnel Fernandes <joel@hackerbliss.org>
;; Created:  Nov 2008
;; Version: 1.1
;; Keywords: jabber gmail
;; URL: http://atlantisbangalore.selfip.com/~joel/projects/jabber-mail-notify.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: Google's XMPP implementation has a nice extension to notify new mail which isn't ofcourse a part of the XMPP standard.
;;; This little plugin extends the Emacs jabber client to support this.
;;; Installation/Usage Instructions- http://atlantisbangalore.selfip.com/~joel/projects/jabber-mail-notify.html

;;; Code:

(defun jabber-set-mode-line-notification (str)
  (interactive)
  (setq jabber-mode-line-newmail str))

(defun jabber-get-jid ()
  (concat jabber-username "@" jabber-server))

(defun jabber-enable-debug ()
  (interactive)
  (setq jabber-debug-log-xml t))

(defun jabber-xml-get-child(node child-name)
     (car (jabber-xml-get-children node child-name)))

(defun jabber-get-mailbox (iq-data)
  (jabber-xml-get-child iq-data 'mailbox))

(defun jabber-get-mailthreads (iq-data)
  (jabber-xml-get-children (jabber-get-mailbox iq-data) 'mail-thread-info))

(defun jabber-get-noof-mailthreads (iq-data)
  (length (jabber-get-mailthreads iq-data)))

(defun jabber-get-mailthread-tid (mailthread)
  (jabber-xml-get-attribute mailthread 'tid))

(defun jabber-get-mailthread-date (mailthread)
  (jabber-xml-get-attribute mailthread 'date))

(defun jabber-get-mailthread-subject (mailthread)
  (caddar (jabber-xml-get-children mailthread 'subject)))

(defun jabber-get-mailthread-snippet (mailthread)
  (caddr (jabber-xml-get-child mailthread 'snippet)))

(defun jabber-get-mailthread-senders (mailthread)
  (jabber-xml-node-children (jabber-xml-get-child mailthread 'senders)))

(defun jabber-build-senders-string(senders)
  (let ((senders-string ""))
    (mapcar (lambda (sender)
              (setq senders-string (concat senders-string (jabber-xml-get-attribute sender 'name) "")))
            senders)
    senders-string))

(defun jabber-build-sender-list(iq-data)
  (let* ((mailthreads (jabber-get-mailthreads iq-data))
         (senders-combined ""))
    (string-join ", "
                 (mapcar (lambda (mailthread)
                           (let* ((senders (jabber-get-mailthread-senders mailthread)))
                             (concat senders-combined (jabber-build-senders-string senders))))
                         mailthreads))))

(defun jabber-build-mailthread-string(mailthread)
  (let* ((subject (jabber-get-mailthread-subject mailthread))
         (snippet (jabber-get-mailthread-snippet mailthread))
         (senders (jabber-get-mailthread-senders mailthread))
         (sender-string (jabber-build-senders-string senders)))
    (concat sender-string " says " subject ", " snippet "..")))

;; Called when google sends us a message of type "result"  sending us a list of messages
;; We take the list, display it in the minbuffer, and update tid and time variables
(defun jabber-build-mailbox-string (iq-data)
  (let* ((mailthreads (jabber-get-mailthreads iq-data))
         (no-of-mailthreads (length mailthreads))
         (final-message (if (equal no-of-mailthreads 1)
                            "New e-mail "
                          (format "%d new email messages\n" no-of-mailthreads)))
         (count 0))
    (mapcar (lambda (mailthread)
              (let* ((tid (jabber-get-mailthread-tid mailthread))
                     (date (jabber-get-mailthread-date mailthread))
                     (subject (jabber-get-mailthread-subject mailthread))
                     (senders (jabber-get-mailthread-senders mailthread)))
                (and (< count 10)
                     (setq final-message (concat final-message "(" (number-to-string (+ count 1))
                                                 ") "  (jabber-build-mailthread-string mailthread) "\n"))
                     (setq count (+ count 1)))
                (if (> (string-to-number tid) (string-to-number jabber-latest-mail-tid))
                    (setq jabber-latest-mail-tid tid))
                (if (> (string-to-number date) (string-to-number jabber-latest-mail-date))
                    (setq jabber-latest-mail-date date))))
            mailthreads)
    final-message))


(defun jabber-display-new-mail (iq-data context)
  (and context
       ;; Don't display if no new messages
       (jabber-get-mailthreads iq-data)
       ;; Display in minibuffer
       (princ (jabber-build-mailbox-string iq-data))
       (jabber-xosd-display-message (jabber-build-mailbox-string iq-data))
       ;; Run mail notification hooks
       (run-hook-with-args 'jabber-mail-notification-hook iq-data)))

;;;###autoload
(defun jabber-check-new-mail ()
  (let ((query-children '((xmlns . "google:mail:notify")))
        (tid jabber-latest-mail-tid)
        (date jabber-latest-mail-date))
    ;; Make sure we get only new mail that's never been notified.
    (or (equal jabber-latest-mail-tid "0")
        (equal jabber-latest-mail-date "0")
        (and (setq query-children (append query-children `((newer-than-tid . ,tid))))
             (setq query-children (append query-children `((newer-than-time . ,date))))))
    ;; Send request - callback from google will be of type "get"
    (jabber-send-iq (jabber-get-jid) "get"
                    `(query ,query-children)
                    #'jabber-display-new-mail t
                    #'jabber-display-new-mail nil)))


(defun jabber-receive-new-mail-notification (iq-data)
  (jabber-check-new-mail))

;;;###autoload
(defun jabber-initialize-mail ()
  (sit-for 15)
  (setq jabber-latest-mail-tid "0")
  (setq jabber-latest-mail-date "0")
  (jabber-check-new-mail))

;; Listen for new mail notification from google (type "result")
(add-to-list 'jabber-iq-set-xmlns-alist
             (cons "google:mail:notify" 'jabber-receive-new-mail-notification))

;; Check for new mail on connect
(add-hook 'jabber-post-connect-hook 'jabber-initialize-mail t)

;; Uncomment to enable debug log window
;; (jabber-enable-debug)

(provide 'jabber-mail-notify)

;;; jabber-mail-notify.el ends here
