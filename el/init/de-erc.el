;;;ERC


(load "de-vars")
(setq load-path (cons (concat root-path "net/irc/erc") load-path))


;;(require 'erc-speak)
(require 'erc)
(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "abhi"
      erc-email-userid "abhijith"    
      erc-prompt-for-password nil) 

(require 'erc-show)
(require 'erc-dcc)


(require 'erc-fill)
(require 'erc-match)
(require 'erc-nickserv)
(require 'erc-stamp)
(require 'erc-notify)
(require 'erc-pcomplete)
(require 'erc-track)

;; (defvar erc-start-list
;;   '(("irc.freebnode.net" 6666  "icy-lisper"))
;;   "List in the form of (server port nick) to open at startup.")

;; (defcustom erc-autojoin-channels-alist nil
;;   "List of channels to autojoin on IRC networks, in the form
;;  ((server-substring . '(chana chanb)))")

;; (setq erc-autojoin-channels-alist
;;       '((".freenode.net" . ("#emacs"))
;; 	("irc.vensnews.com" . ("#interchange"))
;; 	(".oftc.net" . ("#kernelnewbies"))))

;; (defun erc-start ()
;;   "Start an ERC session using as a list of servers `erc-start-list'."
;;   (interactive)
;;   (mapcar (lambda (x)
;; 	    (erc (nth 0 x) (nth 1 x) (nth 2 x) erc-user-full-name t))
;; 	  erc-start-list))

;; (defun erc-autojoin-channels (server nick)
;;   (dolist (l erc-autojoin-channels-alist)
;;     (when (string-match (car l) server)
;;       (dolist (chan (cdr l))
;;         (erc-send-command (concat "join " chan))))))

;; (defadvice save-buffers-kill-emacs 
;;   (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; (erc-pcomplete-enable)
;; (erc-stamp-enable)
;; (erc-match-mode 1)
;; (erc-track-modified-channels-mode 1)
;; (setq erc-auto-query t)
;; (setq erc-hide-timestamps nil)
;; (setq erc-echo-timestamps t)
;; (setq erc-insert-timestamp-function (quote erc-insert-timestamp-left))
;; (setq erc-insert-away-timestamp-function (quote erc-insert-timestamp-left))
;; (setq erc-timestamp-only-if-changed-flag nil)
;; (setq erc-away-nickname "IcyAway")
;; (setq erc-log-channels t)
;; (setq erc-log-channels-directory "~/irc-logs")
;; (setq erc-log-insert-log-on-open nil)
;; (setq erc-minibuffer-privmsg nil)
;; (setq erc-notice-highlight-type nil)
;; (setq erc-current-nick-highlight-type 'nick)
;; ;(setq erc-play-command "esdplay")
;; (setq erc-save-buffer-on-part t)
;; (setq erc-server "irc.freenode.net")
;; (setq erc-timestamp-format "%H:%M")

;; (setq erc-echo-timestamps t)
;; (setq erc-user-full-name "Isaac")
;; (setq erc-keywords '("Lathi"))
;; (setq erc-pals '("gregkh" "delYsid" "kensanata"))
;; (setq erc-notify-list '("gregkh" "resolve" "delysid" "kensanata"))
;; ;(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; (setq erc-hide-list nil)
;; (setq erc-minibuffer-privmsg t)
;; (add-hook 'erc-insert-hook 'erc-truncate-buffer)
;; (setq erc-max-buffer-size 30000)
;; (setq erc-join-buffer 'buffer           ;No more extra frames and buffers
;;       erc-join-into-buffer 'disable)
;; (setq erc-message-english-QUIT "%n (%u@%h) quit")
;; (setq erc-message-english-JOIN "%n (%u@%h) joined")
;; (setq erc-message-english-PART "%n (%u@%h) left")
;; (setq erc-message-english-NICK "%n is now known as %N")



;; (add-hook 'erc-insert-modify-hook 'erc-fill)
;; (add-hook 'erc-send-modify-hook 'erc-fill)
;; (add-hook 'erc-after-connect 'erc-autojoin-channels)

;; ;; try and get erc buffers to be ignored when doing compiles
;; (add-hook 'erc-insert-post-hook 
;; 	  '(lambda () (set-buffer-modified-p nil)))
;; (add-hook 'erc-send-post-hook 
;; 	  '(lambda () (set-buffer-modified-p nil)))


;; ;; ;; nicserv stuff
;; ;; (setq erc-prompt-for-password nil)
;; ;; (setq erc-prompt-for-nickserv-password nil)
;; ;; (setq erc-nickserv-passwords 
;; ;;       '((openprojects (("Lathi" . "foobar")))
;; ;; 	(freenode (("Lathi" . "foobar")))))

;; ;; my /commands

;; (defun erc-cmd-XMMS (&optional line  force)
;;   "Say the current xmms mp3 song title to the current ERC channel"
;;   (erc-send-message 
;;    (concat "I'm listening to "
;; 	   (if (= (string-to-int 
;; 		   (shell-command-to-string "xmmsctrl playing; echo $?"))
;; 		  0)
;; 	       (shell-command-to-string "xmmsctrl title")
;; 	     "nothing")))
;;   t)

;; (defun erc-cmd-COUNT (&optional line force)
;;   "Say the count of names in the current channel"
;;   (let ((tgt (erc-default-target)))
;;     (message "There are %d users in %s" 
;; 	     (length (erc-get-channel-members tgt)) tgt)))

;; (defun erc-cmd-RP (&optional line  force)
;;   "Say the current song playing on http://www.radioparadise.com"
;;   (let ((song (radioparadise-now-playing)))
;;     (if song
;; 	(erc-send-message (concat "http://www.radioparadise.com is now playing: " song))
;;       (erc-send-message "I can't tell what song is playing on http://www.radioparadise.com"))))


;; (defun erc-cmd-YOW ()
;;    (let ((yow (replace-regexp-in-string "\n" "" (yow))))
;;      (erc-send-message yow)))


;; ;; (add-hook 'erc-send-pre-hook '(lambda (string) (setq str (dka-expand-wiki-in-string string))))
;; ;; (defun erc-notify-sign-on-message (nick)
;; ;;   (message "%s has just signed on" nick))

;; ;; (defun erc-notify-sign-off-message (nick)
;; ;;   (message "%s has just signed off" nick))

;; ;; ;(remove-hook 'erc-notify-signoff-hook 'erc-notify-sign-off-message)
;; ;; ;(remove-hook 'erc-notify-signon-hook 'erc-notify-sign-on-message)

;; ;; (defun switch-to-erc ()
;; ;;       "Switch to an IRC buffer, or run `erc-select'.
;; ;;     When called repeatedly, cycle through the buffers."
;; ;;       (interactive)
;; ;;       (let ((buffers (and (fboundp 'erc-buffer-list)
;; ;;                           (erc-buffer-list))))
;; ;;         (when (eq (current-buffer) (car buffers))
;; ;;           (bury-buffer)
;; ;;           (setq buffers (cdr buffers)))
;; ;;         (if buffers
;; ;;             (switch-to-buffer (car buffers))
;; ;;           (erc-select))))


;;Hereâs a way to play a sound whenever someone uses your name, or uses certain keywords (this also assumes youâre using ErcHighlighting):

    (add-hook 'erc-text-matched-hook
              (lambda (match-type nickuserhost message)
                (cond
                  ((eq match-type 'current-nick)
                    (play-sound-file "~/pub/TR2070/TR-Mail.wav"))
                  ((eq match-type 'keyword)
                    (play-sound-file "~/pub/TR2070/TR-Command.wav")))))

