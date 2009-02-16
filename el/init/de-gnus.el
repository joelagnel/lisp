;;gnus

(load "de-vars")
(setq load-path (cons (concat root-path "net/mail/gnus/lisp") load-path))


(require 'gnus-load)


(setq load-path (cons (concat root-path "net/mail") load-path))

(require 'smtpmail)


(load-library "gnus-group")

;;if gnus-nntp-server is set, it will override
;;gnus-select-method.

(setq gnus-nntp-server nil)


(setq
 gnus-select-method '(nnimap "www.gmail.com") ;; server
 gnus-gcc-mark-as-read t
 mail-envelope-from "agnel.joel@gmail.com"
 user-mail-address "agnel.joel@gmail.com"
 user-full-name "agnel.joel"
 smtpmail-auth-login-username "agnel.joel"
 smtpmail-default-smtp-server "mail.google.com"
 smtpmail-smtp-server "mail.google.com"
 mail-host-address "mail.google.com"
 gnus-local-domain ""
 smtpmail-sendto-domain "gmail.com"
 smtpmail-smtp-service nil)

(setq smtpmail-auth-credentials
      '(("www.gmail.com" nil "agnel.joel" ""))) ;; server

;;; Say how Gnus is to store the mail.  We use nnimap groups.
(setq imap-ssl-program "openssl s_client -tls1 -connect %s:%p")

(setq gnus-message-archive-method '(nnimap "gmail.com"))
(setq gnus-message-archive-group "INBOX.Sent")

(setq mail-sources '((imap
                      :server "gmail.com"
                      :mailbox "INBOX"
                      :user    "agnel.joel"
                      :password ""
                      :predicate nil ; Get all mail in "INBOX".
                      :fetchflag "\\Seen"
                      :dontexpunge t )

                     ))




;; (add-to-list 'gnus-secondary-select-methods
;; '(nnmaildir "name-here" (directory "~/Maildir/")))

;(setq gnus-secondary-select-methods '((nnml "")))


;------------------------------ Summary -------------------

(global-set-key "\C-p" 'gnus-summary-delete-article)

(define-key gnus-summary-mode-map [delete] 'gnus-summary-delete-article)
;; (define-key gnus-summary-mode-map "d" 'gnus-summary-mark-as-expirable)
;; (define-key gnus-summary-mode-map "D" 'gnus-summary-expire-articles-now)
;; (define-key gnus-summary-mode-map "s" 'gnus-summary-move-article)

(setq gnus-show-threads nil) ;; Turn off threading
(gnus-group-list-all-groups) ;; Show all the groups, even empty ones
(setq gnus-message-archive-group "INBOX.Sent") ;; Save sent mail

(require 'highline)
(add-hook 'gnus-summary-mode-hook 'highline-on)

;;I dont like the graphics smileys
(setq gnus-treat-display-smileys nil)

(setq  gnus-summary-line-format "%U%R%I%(%[%-8,8d%]%) %(%[%-20,20n%]%)%e%s\n")

(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)


;;(setq gnus-use-trees t)

;------------------------------ Message --------


(setq
 message-send-mail-function 'smtpmail-send-it
 message-syntax-checks '((sender . disabled))
 message-yank-prefix "|"
 gnus-novice-user nil
 mail-from-style 'angles
 message-mode-hook '(flyspell-mode))

(setq gnus-posting-styles
      '((".*"
         ("From" "Joel")
             ("Sender" "agnel.joel@gmail.com")
             ("Organization" "Atlantis Computing")
             (signature-file "~/.signature"))))

(setq message-signature-file "~/.signature")



(add-to-list 'load-path "~/repository/lisp/el/text/")
(require 'boxquote)
(defun boxquote-region-message ()
  "boxquote a reply region in a message"
  (interactive)
  (fill-region (region-beginning) (region-end))
  (boxquote-region (region-beginning) (region-end))
  ;;(boxquote-title (message-fetch-field "To")))
  (boxquote-title (mail-header-from message-reply-headers)))

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

(setq gnus-verbose 10)

(setq message-user-organization "Church of Emacs")

(add-hook 'message-mode-hook
          (lambda ()
            (setq fill-column 72)
            (turn-on-auto-fill)))


(setq gnus-ignored-headers "^References:\\|^Xref:") ;; Hide boring headers
;;visible headers
(setq gnus-visible-headers
      "^To:\\|^Cc:\\|^From:\\|^Date:\\|^Subject:\\|^X-Mailer:\\|^User-Agent:\\|^X-Newsreader:")
(setq gnus-treat-highlight-signature 'last)
;; Uses w3m to show html pages within gnus
(setq mm-text-html-renderer 'w3m)
(setq gnus-check-new-newsgroups nil)


;Tell the world that you use Jabber by including a "Jabber-ID" header in your outgoing email.

(if (not (string-match "Jabber-ID" message-default-headers))
    (setq message-default-headers
          (concat "Jabber-ID: your-jid-here\n"
                  message-default-headers))
)



;; group-highlighting

(cond (window-system
       (setq custom-background-mode 'light)
       (defface my-group-face-1
         '((t (:foreground "PaleGreen" :bold t))) "First group face")
       (defface my-group-face-2
         '((t (:foreground "SteelBlue" :bold t)))
         "Second group face")
       (defface my-group-face-3
         '((t (:foreground "Orange" :bold t))) "Third group face")
       (defface my-group-face-4
         '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
       (defface my-group-face-5
         '((t (:foreground "yellow" :bold t))) "Fifth group face")))

(setq gnus-group-highlight
      '(((> unread 200) . my-group-face-1)
        ((and (< level 3) (zerop unread)) . my-group-face-2)
        ((< level 3) . my-group-face-3)
        ((zerop unread) . my-group-face-4)
        (t . my-group-face-5)))




(custom-set-faces
 '(gnus-cite-attribution-face ((t (:foreground "darkorange" :slant italic))))
 '(gnus-cite-face-7 ((((class color) (background dark)) (:foreground "lightskyblue1"))))
 '(gnus-summary-normal-ancient-face ((((class color) (background dark)) (:foreground "#a9a9a9"))))
 '(gnus-summary-normal-read-face ((((class color) (background dark)) (:foreground "Orange"))))
 '(gnus-summary-normal-ticked-face ((((class color) (background dark)) (:foreground "#bce8ff"))))
 '(gnus-summary-normal-undownloaded-face ((t (:foreground "LightGray" :strike-through "#d9d9d9" :weight normal))))
 '(gnus-summary-selected-face ((t (:background "#4d4d4d" :foreground "#fcffa8"))))
 '(message-cited-text-face ((((class color) (background dark)) (:foreground "sandy brown"))))
 '(message-header-cc-face ((t (:foreground "medium aquamarine" :weight bold))))
 '(message-header-name-face ((((class color) (background dark)) (:foreground "aquamarine"))))
 '(message-header-other-face ((((class color) (background dark)) (:foreground "aquamarine"))))
 '(message-header-subject-face ((((class color) (background dark)) (:foreground "green2" :weight bold))))
 '(message-header-xheader-face ((((class color) (background dark)) (:foreground "khaki"))))
 '(message-mml-face ((((class color) (background dark)) (:foreground "orchid"))))
 '(message-separator-face ((((class color) (background dark)) (:foreground "khaki")))))



;; ;;; BBDB
;; (require 'bbdb)
;; (bbdb-initialize 'gnus 'message 'w3 'sc)

;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

;; (setq gnus-Startup-hook 'bbdb-insinuate-gnus)

;; (global-set-key "\C-c\C-t" 'bbdb-complete-name)

;;; view mime parts
(setq gnus-mime-view-all-parts t)
