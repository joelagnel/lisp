;; SVN

(load "de-vars")
(setq load-path (cons (concat root-path "net") load-path))


;;proxy settings

(setq url-proxy-services
      '(("http://atlantisbangalore.selfip.com" . "192.168.1.2:2222")))

(require 'psvn)

;;tramp

(setq tramp-syntax 'url)
(require 'tramp)
(setq tramp-default-method "ssh")


(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))


;;     C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)

;; Also,

;; <fsbot> [5] emacs syntax: /<user>@<host>/path/to/file or
;;      /<protocol>:<user>@<host>/path/to/file,
;; <fsbot> [7] /su::/etc/hosts to edit /etc/hosts as root,
;; <fsbot> [8] note double : when using su to be root


;;ssh
(setq load-path (cons (concat root-path "net/ssh") load-path))

(require 'ssh)

