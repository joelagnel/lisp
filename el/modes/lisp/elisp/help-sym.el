;; SYNOPSIS: Show all matching symbols in emacs, [C-h S] regexp.
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh
;; From elisp.info file.

(defun describe-symbols (pattern)

  "Describe the Emacs Lisp symbols matching PATTERN.
   All symbols that have PATTERN in their name are described
   in the `*Help*' buffer."

  (interactive "sDescribe symbols matching: ")
  (let ((describe-func
         (function
          (lambda (s)

            ;; Print description of symbol.
            (if (fboundp s)             ; It is a function.
                (princ
                 (format
                  "===============\nFUNC: (%s)....... %s\n\nDOC: %s\n\n" s
                  (if (commandp s)
                      (let ((keys (where-is-internal s)))
                        (if keys
                            (concat
                             "Keys: "
                             (mapconcat 'key-description keys ", "))
                          "Keys: none"))
                    "Function")
                  (or (documentation s) "not documented"))))

            (if (boundp s)              ; It is a variable.
                (princ
                 (format (concat
                  "===============\nVAR: %s ....... %s\n\nDOC: %s\n\n"
                  "VALUE: %s.\n\n")
                  s
                  (if (user-variable-p s) "Option " "Variable")
                  (or (documentation-property s 'variable-documentation)
                      "not documented")
                  (eval s)
        ))))))
        sym-list)

    ;; Build a list of symbols that match pattern.
    (mapatoms (function
               (lambda (sym)
                 (if (string-match pattern (symbol-name sym))
                     (setq sym-list (cons sym sym-list))))))

    ;; Display the data.
    (with-output-to-temp-buffer "*Help*"
      (mapcar describe-func (sort sym-list 'string<))
      (print-help-return-message)
)))

;; EOF
