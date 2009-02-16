;; JABBER

(load "de-vars")
(setq load-path (cons (concat root-path "net/jabber") load-path))

(require 'jabber)

(require 'jabber-festival)
(require 'jabber-alert)

(setq jabber-show-resources t)

(require 'jabber-ft-server)
(require 'jabber-ft-client)
(require 'jabber-socks5)

(setq
 special-display-regexps
 '(("jabber-chat"
    (width . 80)
    (scroll-bar-width . 16)
    (height . 15)
    (tool-bar-lines . 0)
    (menu-bar-lines 0)
    (left . 80))))


(defun my-jabber-chat-delete-or-bury ()
  (interactive)
  (if (eq 'jabber-chat-mode major-mode)
      (condition-case e
          (delete-frame)
        (error
         (if (string= "Attempt to delete the sole visible or iconified frame"
                      (cadr e))
             (bury-buffer))))))

;; (add-hook 'jabber-chat-mode-hook 'pretty-greek)
;; (add-hook 'jabber-roster-mode-hook 'pretty-greek)

(defun jabber ()
  (interactive)
  (require 'jabber)
  (define-key jabber-chat-mode-map [escape]
    'my-jabber-chat-delete-or-bury)

  (define-key mode-specific-map "jr"
    (lambda ()
      (interactive)
      (switch-to-buffer "*-jabber-*")))

  (define-key mode-specific-map "jc"
    '(lambda ()
      (interactive)
      (call-interactively 'jabber-connect)))

  (define-key mode-specific-map "jd"
    '(lambda ()
      (interactive)
      (call-interactively 'jabber-disconnect)))

  (define-key mode-specific-map "jj"
    '(lambda ()
      (interactive)
      (call-interactively 'jabber-chat-with)))

  (define-key mode-specific-map "ja"
    '(lambda ()
      (interactive)
      (jabber-send-presence "away" "" 10)))

  (define-key mode-specific-map "jo"
    '(lambda ()
      (interactive)
      (jabber-send-presence "" "" 10)))

  (define-key mode-specific-map "jx"
    '(lambda ()
      (interactive)
      (jabber-send-presence "xa" "" 10))))


(add-to-list 'jabber-alert-message-hooks
             (lambda (from buffer text proposed-alert)
               (goto-address)))





(setq jabber-chat-header-line-format
      '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
        " " (:eval (jabber-jid-resource jabber-chatting-with)) "\t" ;
        (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                 (propertize
                  (or
                   (cdr (assoc (get buddy 'show) jabber-presence-strings))
                   (get buddy 'show))
                  'face
                  (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                      'jabber-roster-user-online))))
        "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
        (:eval (unless (equal "" *jabber-current-show*)
                 (concat "\t You're " *jabber-current-show*
                         " (" *jabber-current-status* ")")))))



                                        ; (require 'autosmiley)
                                        ;(add-hook 'jabber-chat-mode-hook 'autosmiley-mode)






;; (defun jabber-switch-account ()
;;   (interactive)
;;   (require 'jabber)
;;   (let* ((accounts
;;           '(("deeproot" ("helpdesk.deeproot.co.in" nil ""))
;;             ("google" ("talk.google.com" 5222 ""))))
;;          (acc (cadr (assoc (completing-read "Account: " accounts nil t) accounts)))
;;          (server (nth 0 acc))
;;          (user (nth 2 acc))
;;          (port (nth 1 acc)))
;;     (when server
;;       (message "Switching to %s..." server)
;;       (jabber-disconnect)
;;       (setq jabber-server server
;;             jabber-port port
;;          jabber-username user)
;;       (jabber-connect))))



;;talk to googletalk

(setq jabber-username "agnel.joel")
(setq jabber-password "seemahegde")
(setq jabber-nickname "joel")
(setq jabber-connection-type (quote ssl))
(setq jabber-network-server "talk.google.com")
(setq jabber-server "gmail.com")

(setq jabber-xosd-display-time 5)

(defun jabber-xosd-display-message (message)
  "Displays MESSAGE through the xosd"
  (let ((process-connection-type nil))
    (start-process "jabber-xosd" nil "osd_cat" "-p" "bottom" "-A" "right" "-f" "-*-courier-*-*-*-*-25" "-d" (number-to-string jabber-xosd-display-time))
    (process-send-string "jabber-xosd" message)
    (process-send-eof "jabber-xosd")))


(defun jabber-message-xosd (from buffer text propsed-alert)
  (jabber-xosd-display-message (jabber-jid-displayname jabber-chatting-with)))


(add-to-list 'jabber-alert-message-hooks
             'jabber-message-xosd)

;; individual window for chats

(setq
 special-display-regexps
 '(("jabber-chat"
    (width . 80)
    (scroll-bar-width . 16)
    (height . 15)
    (tool-bar-lines . 0)
    (menu-bar-lines 0)
    (font . "-GURSoutline-Courier New-normal-r-normal-normal-11-82-96-96-c-70-iso8859-1")
    (left . 80))))

;; (if (not (string-match "Jabber-ID" mail-default-headers))
;;     (setq mail-default-headers
;;           (concat "Jabber-ID: your-jid-here\n"
;;                   mail-default-headers)))



;; ---------------- Joel --------------
;; Stumpwm new message mode-line integration
(defun jabber-stumpwm-activity-modeline-update ()
  (let ((text jabber-activity-mode-string))
    (if (not (eq (length text) 0))
        (setf text (concat " [Message from " text "]")))
;;;     (write-string-to-file "~/emacs-jabber.temp" text)
;;;     (princ "Done.")
    ))
(add-hook 'jabber-activity-update-hook 'jabber-stumpwm-activity-modeline-update)

;; Stumpwm new mail mode-line integration
(defun jabber-mail-stumpwm-mode-line-update(iq-data)
  (let* ((text (jabber-build-mailbox-string iq-data)))
;;;     (write-string-to-file "~/emacs-jabber-mail.temp" text)
;;;     text)
;;;   (princ "Done.")
  ))
(add-hook 'jabber-mail-notification-hook 'jabber-mail-stumpwm-mode-line-update)
;; -------------------------------------------
