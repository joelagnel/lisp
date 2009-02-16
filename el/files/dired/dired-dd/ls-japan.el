;;; ls-japan.el --- Japanese filename display switcher.

;; Mostly for emacs-20.x, *WITHOUT* 'ls-lisp package.

;; Tue Oct 24 16:32:36 2000 s.n.

;; Install:
;;   Save this sort of euc output phoney ls ~/bin/ls-euc
;;     #!/bin/ksh
;;     exec ls -N "$@" | exec nkf -e

;; Caveats:
;;   1) ls-lisp.el seems not working in emacs-20.7 at all (?).
;;   2) This program can't go well with ls-lisp.el on emacs-19.34.
;;      (ls-lisp works on mule-2.3@19.34, but this program can't display
;;       the euc output once ls-lisp is loaded)
;;   A file with a certain name encoded in SJIS may not handled correctly.
;;   even with ls-lisp...

(require 'dired)

(defvar my-insert-directory-program "ls-euc"
  "/bin/ls substitution generates euc output.")

(define-key dired-mode-map "\M-G" 'dired-show-japanese-listing)

(defun dired-show-japanese-listing ()
  "Shows corresponding Japanese directory listing in `dired' buffer.
Bound to \\[dired-show-japanese-listing].
Do \\[revert-buffer] to get the original octal `dired' display."
  (interactive)
  (let ((insert-directory-program my-insert-directory-program))
    (revert-buffer)
    (message (format "Hit `g' to return to octal display"))))

(provide 'ls-japan)
