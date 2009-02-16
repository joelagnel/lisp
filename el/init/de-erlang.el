;; erlang initialization


;; (load "de-vars")
;; (setq load-path (cons (concat root-path "modes/erlang") load-path))

;; (setq load-path (cons  "/usr/local/otp/lib/tools-<ToolsVer>/emacs"   load-path))
;; (setq erlang-root-dir "/usr/local/otp")
;; (setq exec-path (cons "/usr/local/otp/bin" exec-path))
;; (require 'erlang)
;; (require 'erlang-start)



(load "de-vars")
(setq load-path (cons (concat root-path "modes/erlang") load-path))
(setq load-path (cons (concat root-path "modes/erlang/esense-1.12") load-path))
;(setq load-path (cons (concat root-path "modes/erlang/distel/elisp") load-path))
(require 'esense-start)
(setq esense-indexer-program "/home/abhi/repository/lisp/el/modes/erlang/esense-1.12/esense.sh")
(setq load-path (cons  "/usr/local/otp/lib/tools-<ToolsVer>/emacs"   load-path))
(setq erlang-root-dir "/usr/local/otp")
(setq exec-path (cons "/usr/local/otp/bin" exec-path))
(load "erlang")
(load "erlang_appwiz")
(require 'erlang-start)

(add-to-list 'load-path "/home/abhijithg/repository/lisp/el/modes/distel/elisp")
;; (require 'distel)
;; (distel-setup)

(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(defvar inferior-erlang-prompt-timeout t)
