

;;; pretty-lambda.el -- Lambda character syntactic-sugar hack for Common Lisp
;;; Written for Emacs 21 by Luke Gorrie <luke@bluetail.com>
;;; $Id: pretty-lambda.el,v 1.2 2002/08/14 19:17:50 luke Exp luke $
;;
;; A simple hack to use pretty lambda characters in Emacs for Common
;; Lisp programming, antisocially implemented by using greek character
;; encoding for Lisp source. Best explained with a screenshot:
;; http://www.bluetail.com/~luke/misc/lambda.jpg
;;
;; Here's how it works:
;;
;; * Emacs already supports entering and displaying fancy
;;   characters. You can enter a lambda symbol either with
;;   set-input-method to "greek" (then press l), or with the
;;   `pretty-lambda-insert' function provided here.
;;
;; * We tell Emacs to load and save lisp files using the
;;   `greek-iso-8bit' character encoding instead of Latin-1. You'll
;;   want to do this differently if you really use Latin-1!
;;
;;   We tell ilisp to do the same conversion when it talks to an
;;   inferior lisp.
;;
;; * We install a reader macro in Common Lisp to expand the character
;;   that represents lambda into the symbol LAMBDA.
;;
;; You'll also need to have a font in your X server that can display
;; greek characters (otherwise you just see a hollow box). On my
;; Redhat 7.1 machine I already had this (though it's a bit ugly). On
;; my Debian woody machine, I apt-get'd all the international fonts
;; packages and then restarted the X server, and found myself with
;; very nice greek characters.
;;
;; Almost forgot! Here's the Common Lisp side:
;;
;;   (defun greek-lambda-reader (stream char)
;;     (declare (ignore stream char))
;;     'lambda)
;;
;;   (set-macro-character (code-char 235) #'greek-lambda-reader)
;;
;; There is probably a much better way to do all this!

(defvar pretty-lambda-coding-system 'greek-iso-8bit)

;; Hopefully this number is portable - I don't know if it's unicode,
;; or what..
(defvar pretty-lambda-character 2923)

(defun pretty-lambda-insert ()
  "Insert a lambda character."
  (interactive)
  (insert pretty-lambda-character))

(defun pretty-lambda-unpretty-region (start end)
  "Replace lambda characters with \"lambda\"."
  (interactive "r")
  (replace-string (string pretty-lambda-character) "lambda" nil start end))

;; Integration (automatically adds hooks - there is no escape!!)

(defun pretty-lambda-ilisp-init-hook ()
  (set-process-coding-system (ilisp-process) pretty-lambda-coding-system))

(add-hook 'ilisp-init-hook 'pretty-lambda-ilisp-init-hook)
(modify-coding-system-alist 'file "\\.lisp$" pretty-lambda-coding-system)

(provide 'pretty-lambda)

