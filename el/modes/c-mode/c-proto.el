;; SYNOPSIS: Extract function protypes from C source code
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh
;; Notes:
;;   Original code from internet news was slow.
;;   Made fast by ignoring comments, 15-Feb-95 Wed.

;; Caution: Inside comments, put a ':' so we won't match a false
;; a function header reg-exp:
;;     /* Now we call  h(x). */  Not ok.
;;     /* Now we call: h(x). */  OK.

(defun c-prototype ()
  "Extract only the function definitions out of a C source file."
  (interactive)
  (save-excursion
    (setq comment-start "/*" comment-end "*/")
    (goto-char (point-min))
    (let (found start)
      ;; this should match sth like "int[\n ]something(args,...)"
      ;; with or without return-type.
      (while (re-search-forward
          ;; Old function-header reg-exp.
              ;; "^\\(\\(\\s_\\|\\w\\)+[\n ]*\\)*\\*?\\(\\s_\\|\\w\\)+\\s-*("
          ;; New:
              "^[ \ta-zA-Z_*]+("
              nil t)
        (setq start (match-beginning 0))
        (goto-char (1- (match-end 0)))
        (forward-sexp 1)
        (setq found (concat found (buffer-substring start (point)) ";\n"))
        (message "Found: %s" (buffer-substring start (point)) ";")
        ;; New
        (re-search-forward "{" nil t)
        (goto-char (1- (match-end 0)))
    (forward-sexp 1)
        )
      (if found
          (progn
            ;; insert the found items
            (pop-to-buffer "*prototypes*")
            ; (c-mode) ; Was a mistake to comment out, by Mosh 19-Jan-95.
        ; Now we don't need this to define /* and */.
            (erase-buffer)
            (insert found)

            ))
      )))

;;; EOF
