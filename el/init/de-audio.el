
(load "de-vars")
(setq load-path (cons (concat root-path "audio") load-path))



;(require 'festival)

(autoload 'say-minor-mode "festival" "Menu for using Festival." t)
(say-minor-mode t)

(require 'erec)


;;instead of beeping or flashing, Emacs could play a cool sound file, whenever an error occurs:


;;  (setq ring-bell-function (lambda ()
;; 			   (call-process "audioplay" nil 0 nil 
;; 				 "/this/is/my/errorsound.au")))

;;Disable!!
;; (setq ring-bell-function 'ignore)