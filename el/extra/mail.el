
;; smtp

(setq user-full-name "Isaac Praveen")
(setq user-mail-address "isaac@deeproot.co.in")

(setq smtpmail-default-smtp-server "192.168.1.5:25")
(setq smtpmail-local-domain nil)
(setq send-mail-function 'smtpmail-send-it)

(load-library "smtpmail")

