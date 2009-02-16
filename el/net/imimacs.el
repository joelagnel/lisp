;; imimacs.el -- Instant Messaging in Emacs
;; Written Dec. 6th 2006 by Markus Triska (tri...@gmx.at)
;; Public domain code.

;; Copy this file to your load-path and add to your .emacs:
;;
;; (require 'imimacs)
;; ; interface that the server listens on:
;; (setq imimacs-listen-interface "eth0")
;; ; predefined peers available via M-n and M-p in minibuffer:
;; (setq imimacs-peers '("192.168.123.12" "my.peer.com"))
;; ; arbitrary (nick-)name, prepended to outgoing messages if non-nil
;; (setq imimacs-user-name "my_nick")
;; (imimacs-init)
;; (global-set-key [f5] 'imimacs-send)

;; Restart Emacs and press F5 to send something to a peer; M-p and M-n
;; cycle through addresses predefined via `imimacs-peers'. C-0 F5
;; sends a message to the peer you got the most recent message from.

;; The server listens on port `imimacs-port'. All sent and received
;; strings are displayed in the buffer `imimacs-buffer-name'. Adjust
;; `imimacs-receive-face' and `imimacs-send-face' to change text
;; properties of received and sent messages. For example, try:
;; (setq imimacs-receive-face '((:background "LightCyan2")))

;; Requires GNU Emacs >= 22.0.90.

(defvar imimacs-port 8947
  "Port the server listens on and client connects to.")

(defvar imimacs-listen-interface nil
  "String of interface name the server should listen on; nil for none.")

(defvar imimacs-peers nil
  "List of IP or host address strings available via M-n and M-p in minibuffer.")

(defvar imimacs-receive-face 'default
  "Face or properties for showing received messages.

For example, try: '((:inverse-video t)) or '((:background \"LightCyan2\")).")

(defvar imimacs-send-face 'default
  "Face or properties for showing sent messages.

For example, try: '((:foreground \"blue\")).")

(defvar imimacs-pop-up t
  "Whether to display the imimacs message buffer when a message arrives.")

(defvar imimacs-user-name nil
  "Your identification string, prepended to outgoing messages if non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst imimacs-version "0.8")

(defvar imimacs-buffer-name "*imimacs*")

(defvar imimacs-server-process nil)

(defvar imimacs-client-process nil)

(defvar imimacs-most-recent-sender nil
  "Address of sender of most recently received message.")

(defun imimacs-toggle-pop-up ()
  "Toggle whether the chat buffer pops up when a message is sent or received."
  (interactive)
  (if (setq imimacs-pop-up (not imimacs-pop-up))
      (message "Enabled pop-up buffer.")
    (message "Disabled pop-up buffer.")))

(defun imimacs-server-running ()
  (and imimacs-server-process
       (eq (process-status imimacs-server-process) 'listen)))

(defun imimacs-client-connection ()
  (and imimacs-client-process
       (eq (process-status imimacs-client-process) 'open)))

(defun imimacs-off ()
  "Stop server and client, if running."
  (interactive)
  (if (imimacs-server-running)
      (delete-process imimacs-server-process))
  (if (imimacs-client-connection)
      (delete-process imimacs-client-process)))

(defun imimacs-init (&optional remote-host)
  "Start the server if not running, and connect to REMOTE-HOST.

If REMOTE-HOST is nil or omitted, only start the server, if it is
not running. `imimacs-listen-interface' is the interface the
server uses for listening, and it will only accept connections to
that interface's address."
  (if (and imimacs-listen-interface (not (imimacs-server-running)))
      (let (addr)
        (dolist (i (network-interface-list))
          (if (string= (car i) imimacs-listen-interface)
              (setq addr (format-network-address (cdr i) t))))
        (when addr
          (setq imimacs-server-process
                (condition-case nil
                    (make-network-process  :name "imimacs-server"
                                           :filter 'imimacs-receive-filter
                                           :host addr
                                           :server t
                                           :service imimacs-port)
                  (error nil)))
          (if (processp imimacs-server-process)
              (set-process-query-on-exit-flag imimacs-server-process nil)))))
  (if remote-host (imimacs-start-client remote-host)))

(defun imimacs-start-client (remote-host)
  (setq imimacs-client-process
        (condition-case nil
            (make-network-process :name "imimacs-client"
                                  :host remote-host
                                  :service imimacs-port)
          (error nil)))
  (if (imimacs-client-connection)
      (set-process-query-on-exit-flag imimacs-client-process nil)))

(defun imimacs-show (string)
  (save-selected-window
    (let* ((buffer (get-buffer-create imimacs-buffer-name))
           (window (select-window (display-buffer buffer))))
      (goto-char (point-max))
      (insert (format "%s\n" string))
      (unless imimacs-pop-up (delete-window window)))))

(defun imimacs-receive-filter (proc string)
  (setq imimacs-most-recent-sender (car (process-contact proc)))
  (imimacs-show
   (propertize (decode-coding-string string 'utf-8)
               'face imimacs-receive-face)))

(defun imimacs-send (arg)
  "Ask for a message string and send it to the current receiver.
If no connection is currently established, ask for the receiver's
address and connect. With prefix argument, terminate any current
connection and ask for the receiver's address. With prefix
argument zero, automatically establish the new connection to the
peer that the most recent message was received from."
  (interactive "P")
  (if (and arg (imimacs-client-connection))
      (delete-process imimacs-client-process))
  (unless (imimacs-client-connection)
    (imimacs-start-client
     (if (and (eq arg 0) imimacs-most-recent-sender)
         imimacs-most-recent-sender
       (read-string "Receiver: " nil 'imimacs-peers))))
  (if (imimacs-client-connection)
      (let ((str (read-string "Send: ")))
        (unless (string= str "")
          (if (imimacs-client-connection)
              (progn
                (process-send-string
                 imimacs-client-process
                 (encode-coding-string (concat imimacs-user-name
                                               (if imimacs-user-name ": ")
                                               str) 'utf-8))
                (imimacs-show (propertize str 'face imimacs-send-face)))
            (message "Receiver not available."))))
    (message "Couldn't connect to server.")))

(provide 'imimacs)
