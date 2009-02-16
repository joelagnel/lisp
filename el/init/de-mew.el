
;; IMAP

;; (setq new-imap-prefix-list '("#mh/" "mhinbox"))
;; (setq mew-use-cached-passwd t)

;; uncomment nbelow line if you dont want to get mail when startup
;(setq mew-auto-get nil)
;; (setq mew-config-alist
;;       '(
;; 	("default"
;; 	 ("name"        . "Isaac Praveen")
;; 	 ("mail-domain" . "deeproot.co.in")
;; 	 ("proto"       . "%")
;; 	 ("imap-server" . "9.182.1.5")
;; 	 ("imap-ssl"    . nil)
;; 	 ("imap-size"   . 0)
;; 	 ("imap-delete" . nil))
;; 	("local"
;; 	 ("local"
;; 	  ("proto"  . "+")
;; 	  ("mailbox-type" . mbox)
;; 	  ("mbox-command" . "incm")
;; 	  ("mbox-command-arg" . "-c -d /var/mail/ike")))))


;;smtp

(setq mew-imap-prefix-list '("#mh/" "#mhinbox"))
(setq mew-use-cached-passwd t)
(setq mew-auto-get t)
(setq mew-config-alist
      '(
	("default"
	("name"     . "Isaac Praveen")
	 ("user"    . "isaac") 
 	 ("mail-domain" . "deeproot.co.in")
	 ("proto" . "%")
	 ("smtp-auth-list" . ("PLAIN"))
	 ("smtp-server" . "192.168.1.5")
	 ("smtp-port" . "25")
	 ("smtp-ssl" . nil)
	("smtp-ssl-port" . "25")
	("smtp-user" . "isaac")
	("imap-server" . "192.168.1.5")
	("imap-ssl" . nil)
	("imap-user" . "isaac")
	("imap-size" . 0)
	("mew-imap-inbox-folder". "%inbox") 
	("imap-delete" . t))))


(setq mew-imap-header-only t)
(setq mew-use-unread-mark t)

(setq mew-passwd-alist '(("isaac@deeproot.co.in" "isaac" 0)))


(setq mew-imap-user "isaac")  ;; (user-login-name)
(setq mew-imap-server "192.168.1.5")	  
(setq mew-imap-delete t)
(setq mew-use-thread-separator t)

(setq mew-mail-path "~/Mail")
(setq mew-conf-path mew-mail-path)

(setq mew-window-use-full t)
(setq mew-underline-lines-use t)
(setq mew-use-fancy-thread t)
(setq mew-use-fancy-highlight-body t)
;;(Setq Mew-fancy-highlight-body-prefix-width 10)
(setq mew-highlight-body-regex-comment "^[;#?%]+.*")
(setq mew-use-highlight-mouse-line t)


(setq mew-use-highlight-cursor-line t)
(setq mew-highlight-cursor-line-face 'underline)

(setq mew-summary-form
	  '(type (5 date) " " (14 from) " " t (30 subj) "|" (0 body)))


;;browser
(setq mew-summary-form
	  '(type (5 date) " " (14 from) " " t (30 subj) "|" (0 body)))




;;biff

(setq mew-use-biff t)	

(setq mew-biff-interval 10)



(require 'boxquote)
(defun boxquote-region-message ()
  "boxquote a reply region in a message"
  (interactive)
  (fill-region (region-beginning) (region-end))
  (boxquote-region (region-beginning) (region-end)))
  ;;(boxquote-title (message-fetch-field "To")))
  ;;(boxquote-title (mail-header-from message-reply-headers)))


(defvar ab:boxquote-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b")   'boxquote-paragraph)
    (define-key m (kbd "i")   'boxquote-insert-file)
    (define-key m (kbd "p")   'boxquote-paragraph)
    (define-key m (kbd "r")   'boxquote-region)
    (define-key m (kbd "t")   'boxquote-title)
    (define-key m (kbd "u")   'boxquote-unbox)
    (define-key m (kbd "m")   'boxquote-region-message)
    (define-key m (kbd "C-y") 'boxquote-yank)
    (define-key m (kbd "C-k") 'boxquote-kill)
    m)
  "Keymap for boxquote functions.")
(global-set-key  (kbd "C-c q")    ab:boxquote-map)




(setq mew-prog-text/html         'mew-mime-text/html-w3m) ;; See w3m.el
(setq mew-prog-text/html-ext     "/usr/bin/firefox")

(setq mew-prog-postscript      '("evince" ("") t))

(defconst mew-x-mailer "Mew(GNU Emacs) on GNU/linux")
(setq mew-msg-rm-policy 'always)


;; ;; Flyspell
;; (require 'flyspell)
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; (display-time)
;; ;;WORD WRAP AUTOFILL
;; (flyspell-mode 1)
;; (setq default-major-mode 'text-mode)
;; (setq default-major-mode 'mew-mode)
(setq message-mode-hook
      '(lambda nil
	(setq fill-column 72)
 	 (auto-fill-mode 1)
	))

;; (setq ispell-skip-sgml t)
;; (set-default 'ispell-local-dictionary "american")

