;; 2003-03-03 T10:05:35-0500 (Monday)    D. Goel
;; modified to a different name because this version is used by
;; runshell-shortcuts.el

;;; Debugging info for self: Saved through ges-version 1.5dev
;;; ;;; ;;; From: lawrence mitchell <wence@gmx.li>
;;; ;;; ;;; Subject: rfind-lib.el --- Find a file in Emacs' load-path with completion.
;;; ;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; ;;; Date: Sat, 27 Jul 2002 21:33:21 +0100
;;; ;;; ;;; Organization: funfunfun
;;; ;;; ;;; Mail-Copies-To: nobody

;;; ;;; --=-=-=


;;; ;;; Commentary:
;;; ;; Have you ever spent time searching for a .el file you know you have
;;; ;; somewhere?  Or have you ever tried to remember exactly what a file
;;; ;; was called.  Then this might be what you want, it is basically a
;;; ;; wrapper around (find-file (locate-library "foo.el")), but with
;;; ;; filename completion.

;;; I'm not sure how useful this will be to people, I use it quite a
;;; lot, as I'm generally too lazy to type TAB a few times to find a
;;; file.

;;; This could probably be extended quite easily to deal with other
;;; types of files, you'd just have to provide an alternate
;;; `locate-library' function which completed the filename extension
;;; you wanted.  Hmmm...

;;; Suggestions/comments welcome, especially any ways of filling the
;;; obarray of files more efficiently.


;;; ;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=rfind-lib.el
;;; Content-Transfer-Encoding: 8bit
;;; Content-Description: rfind-lib.el

;;; rfind-lib.el --- Find a file in Emacs' `load-path'.

;;; Commentary:
;; Have you ever spent time searching for a .el file you know you have
;; somewhere?  Or have you ever tried to remember exactly what a file
;; was called.  Then this might be what you want, it is basically a
;; wrapper around (find-file (locate-library "foo.el")), but with
;; filename completion.

;; This provides one user function `rfind-lib-locate-library', which
;; you can bind to a key if you want.

;;; History:
;; 

;;; Code:


;;; Internal variables.
(defvar rfind-lib-file-list nil
  "Obarray holding a list of all *.el files in Emacs' `load-path'.

See also `rfind-lib-fill-file-obarray' to see how this obarray is
filled.")

;;; Internal functions.
;; Support functions we need.
(if (not (fboundp 'primep))
    (defsubst rfind-lib-prime-p (num)
      "Return t if NUM is a prime number."
      (let ((lim (sqrt num))
            (nu 2)
            (prime t))
        (while (and prime (< nu lim))
          (setq prime (/= 0 (mod num nu))
                nu (1+ nu)))
        prime))
  (defalias 'rfind-lib-prime-p 'primep))

;; create an obarray holding all .el files in Emacs' `load-path'.  Any
;; additions making it faster would be very welcome.
(defun rfind-lib-fill-file-obarray ()
  "Fill `rfind-lib-file-list' with all .el files in `load-path'."
  (let ((files (mapcar 'file-expand-wildcards
                       (mapcar #'(lambda (dir)
                                   (concat dir "/*.el"))
                               load-path)))
        file-list vector-length)
    (setq files (delete nil files))
    (while files
      (setq file-list (append (car files) file-list)
            files (cdr files)))
    (setq file-list (mapcar 'file-name-nondirectory
                            file-list)
          vector-length (length file-list))
    (while (not (rfind-lib-prime-p vector-length))
      (setq vector-length (1+ vector-length)))
    (setq rfind-lib-file-list (make-vector vector-length nil))
    (mapcar #'(lambda (file)
                (intern file rfind-lib-file-list))
            file-list)))

;;; User functions
;;;###autoload
(defun rfind-lib-locate-library ()
  "Find a file in Emacs' `load-path'.

This is equivalent to: \(find-file (locate-library \"file-name.el\")).
The filename completion table is created by
`rfind-lib-fill-file-obarray', which see."
  (interactive)
  (unless rfind-lib-file-list
    (rfind-lib-fill-file-obarray))
  ;; We have an `or' here in case we have added files to `load-path'
  ;; after initialising the obarray.
  (let ((library (or (completing-read "Library: " rfind-lib-file-list)
                     (read-string "Library: "))))
    (if (not (string-match "\\.el$" library))
        (setq library (concat library ".el")))
    (condition-case err
        (find-file (locate-library library))
      (wrong-type-argument
       (message "The file %s does not exist in this Emacs' load-path" library)))))

(provide 'rfind-lib)

;;; rfind-lib.el ends here

;;; --=-=-=


;;; -- 
;;; lawrence mitchell <wence@gmx.li>

;;; --=-=-=--

