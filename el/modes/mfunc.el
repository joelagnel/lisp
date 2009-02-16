;; SYNOPSIS: find/jump to C/C++/Perl/Asm/etc Functions.
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh

(defvar mosh-re-asm-func (concat
  "\\(^\\(BeginProc\\|EndProc\\)[ \t]+\\(\\w+\\)\\)" ; _NSPStartup@8 ENDP
  "\\|"
  "\\(\\(^\\S-+\\) \\(PROC\\|ENDP\\)\\b\\)"     ; _NSPCleanup@4 PROC NEAR
  )
  "Regexp that matched a function boundary and name, at boln."
)

(defvar mosh-re-elisp-func (concat
  "^(\\S-+[ \t()]+"                  ;; defun    $1
  "\\([^ \t\n]+\\)\\>"               ;;    fname $2
 )
  "Regexp that matches start of a lisp sexp/func and its name."
)

;; Why not use imenu-example--function-name-regexp-perl

(defvar mosh-re-perl "^[a-zA-Z].* \\([::a-zA-Z_0-9]+\\) .*[{}]\\|^[{}]"
  "Regexp that matches perl sub-routine name"
)

(defvar mosh-re-text "^\\(\\[?[0-9][0-9.]+\\) *\\(.*\\)"
  "Regexp that matches section titles, eg. 2.1.1."
)

(defvar mosh-func-name "none" "Name of function for cut/paste.")

(defun mosh-which-func (&optional moveit)
  "show and color the current function name in which the point is located.
   move point if moveit."
  (interactive)
  (let (mark1 mark2)
     (save-excursion
       (cond
        ((eq major-mode 'emacs-lisp-mode)
         (beginning-of-defun)
         (re-search-forward mosh-re-elisp-func)
         (setq mark1 (match-beginning 1))
         (setq mark2 (match-end 1))
        )

        ((eq major-mode 'c++-mode)
         ;; Example: int foo( params ) const { body <point> body }.
         ;; We arrived at this thru experimentation with c++/c code.
         (beginning-of-defun)             ; over body, at open paren.
         (repeat                          ; over const..
          (backward-sexp)                 ; over params, after foo.
          'until (or (bobp)
                     (looking-at "[()]")
                     (looking-at mosh-re-stl)))
         ;; usual c/c++ function.
         (skip-chars-backward " \t\n")
         (setq mark2 (point))
         (if (bolp) (error "No C++ function found."))
         (skip-chars-backward "a-zA-Z0-9_:~+-=*")
         (setq mark1 (point))
        )
        ((eq major-mode 'asm-mode)
           (end-of-line)
           (re-search-backward mosh-re-asm-func)
           (cond
            ((match-string 3)
             (setq mark1 (match-beginning 3))
             (setq mark2 (match-end       3)))
            ((match-string 5)
             (setq mark1 (match-beginning 5))
             (setq mark2 (match-end       5))))
        )
        ((eq major-mode 'perl-mode)
           (end-of-line)
           (re-search-backward imenu-example--function-name-regexp-perl)
           (setq mark1 (match-beginning 3))
           (setq mark2 (match-end       3))
        )
        ((eq major-mode 'text-mode)
         (end-of-line)
         (re-search-backward mosh-re-text)
         (setq mark1 (match-beginning 0))
         (setq mark2 (match-end       0))
        )
        ((equal mode-name 'hapimode)
           (end-of-line)
           (re-search-backward mosh-re-hapi-vari)
           (setq mark1 (match-beginning 1))
           (setq mark2 (match-end       2))
        )
        ((eq major-mode 'pascal-mode)
         (re-search-backward mosh-pascal-func-re)
         (setq mark1 (match-beginning 2))
           (setq mark2 (match-end       2))
        )
        ((eq major-mode 'mverilog-mode)
         (end-of-line)
         (re-search-backward mverilog-module-re)
         (setq mark1 (match-beginning 2))
         (setq mark2 (match-end       2))
         )
        ((eq major-mode 'vhdl-mode)
         (end-of-line)
         (re-search-backward mvhdl-proc-re)
         (setq mark1 (match-beginning 1))
         (setq mark2 (match-end       1))
        )
       )
     )
     ;; Now return the function name if mark1 and mark2 are set.
     ;; else return an empty string.
     (if (and mark1 mark2)
         (progn
          (setq mosh-func-name (buffer-substring mark1 mark2)) ; Save "foo"
          (message "Currently in: %s()" mosh-func-name)        ; Print foo()
          (mosh-color-region mark1 mark2 'bold)                ; Color foo
          (if moveit (goto-char mark1))                        ; goto to start.
          mosh-func-name                                       ; Return name.
         )
       ""
     )
))

;; Moving cursor over C functions, Only for C++. 15-Dec-95.

(defun mosh-prev-c-defun ()                  "Goto start of function."
  (interactive)
  (beginning-of-line)
  (mosh-which-func 'moveit)
)

(defun mosh-next-c-defun ()                      "Goto end of function."
  (interactive)
  (end-of-defun 2)
  (mosh-which-func 'moveit)
)

(defun mosh-prev-perl ()
  (interactive)
  (beginning-of-line)
  (cond
   ((re-search-backward mosh-re-perl (point-min) 'NOERROR))
   (t (goto-char (point-min)))
))

(defun mosh-next-perl ()
  (interactive)
  (end-of-line)
  (cond
   ((re-search-forward mosh-re-perl (point-max) 'NOERROR))
   (t (goto-char (point-max)))
))

(defun mosh-prev-section ()                  "Goto start of section."
  (interactive)
  (beginning-of-line)
  (cond
   ((re-search-backward mosh-re-text (point-min) 'NOERROR)
      (mosh-which-func 'moveit))
   (t (goto-char (point-min)))
))

(defun mosh-next-section ()                  "Goto end of section."
  (interactive)
  (end-of-line)
  (cond
   ((re-search-forward mosh-re-text (point-max) 'NOERROR)
    (mosh-which-func 'moveit))
   (t (goto-char (point-max)))
))

(defun mosh-prev-lisp-defun ()
  (interactive)
  (beginning-of-line)
  (cond
   ((re-search-backward mosh-re-elisp-func (point-min) 'NOERROR)
    (mosh-color-match 'bold 1)
    (goto-char (match-beginning 1)))
   (t (goto-char (point-min)))
))

(defun mosh-next-lisp-defun ()
  (interactive)
  (end-of-line)
  (cond
   ((re-search-forward mosh-re-elisp-func (point-max) 'NOERROR)
    (mosh-color-match 'bold 1)
    (goto-char (match-beginning 1)))
   (t (goto-char (point-max)))
))

(defun mosh-prev-asm-defun ()                  "Goto start of function."
  (interactive)
  (beginning-of-line)
  (cond
   ((re-search-backward mosh-re-asm-func)
    (cond
     ((match-string 3)
      (goto-char (match-beginning 3))
      (mosh-color-match 'green 3))
     ((match-string 5)
      (goto-char (match-beginning 5))
      (mosh-color-match 'green 5))))
   (t (goto-char (point-min)))
))

(defun mosh-next-asm-defun ()                      "Goto end of function."
  (interactive)
  (end-of-line)
  (cond
   ((re-search-forward mosh-re-asm-func)
    (cond
     ((match-string 3)
      (goto-char (match-beginning 3))
      (mosh-color-match 'green 3))
     ((match-string 5)
      (goto-char (match-beginning 5))
      (mosh-color-match 'green 5))))
   (t (goto-char (point-max)))
))

;; ws2_32:  (format "%s(%s,(\"%s:\" ));" DEBUGF    DBG_TRACE FNAME)
;; wsock32: (format "%s(   (\"%s:\" ));" DLL_PRINT           FNAME)
;; wsock32: (format "%s(%s, \"%s:\"  );" WS_TRACE  STARTUP   FNAME)

(setq

 ;; For ws2_32.dll,  DEBUGF(DBG_TRACE,("WSAStartup:%d\n",i));
 ;; For wsock32.dll, WS_TRACE(STARTUP,"WSAStartup:",0,0,0);
 ;; sapnsp2.dll,     DLL_PRINT(  ("NSPStartup:\n"));
 ;; For utils,       DEBUG_TRACE(("WSAStartup:%d\n", GetLastError() ));
 ;; For utils,       DEBUG_PRINT(("NSPStartup:\n"));
 ;; For Coffee,      printf("TCoffeeMachine::TCoffeeMachine\n");

 mosh-insert-trace-prefix    "DEBUGF(DBG_TRACE,(\""
 mosh-insert-trace-prefix    "DEBUG_TRACE((\""
 mosh-insert-trace-prefix    "DEBUG_PRINT((\""
 mosh-insert-trace-prefix    "printf(\""

 mosh-insert-trace-suffix    ":%d\\n\",i));\n"
 mosh-insert-trace-suffix    "%d:\", GetLastError() ));\n"
 mosh-insert-trace-suffix    ":\\n\"));\n"
 mosh-insert-trace-suffix    ":\\n\");\n"
)

(defun mosh-insert-trace ()      "Insert trace statements in c program."
  (interactive)
  (insert
      mosh-insert-trace-prefix
      (mosh-which-func)
      mosh-insert-trace-suffix
  )
  (c++-indent-command)
  (search-backward ":")
)

(setq mosh-main-debug (concat
     "DEBUG_PRINT((\"main:\" __FILE__ "
     "\" built \" __DATE__ \" \" __TIME__ \"\\n\" ));\n"
))

;; 11-Jan-98.

(defun mosh-fix-open-parens ()
  (interactive)
  "Make all functions open paren start on a new line"
  (save-excursion
    (while (re-search-forward "^}" (point-max) t)
      (backward-list 1)
      (if (bolp) t
        (message "y to insert newline, n to skip, C-g to quit:")
        (if (eq (read-char) ?y) (insert "\n")))
      (re-search-forward "^}")
    ))
  (message "done")
)

;; patch ispell-complete-word to keep case of file.

(defun mosh-ispell-expand (ispell-complete-word-dict)

  "Expand current word by matching it with words in a given file.
   Case munging is turned off by modifying:
   ispell.el:ispell-complete-word(interior-flag) see patches.el"

  (interactive (list (read-file-name "expand words from file:")))
  (ispell-complete-word)
)

(provide 'mfunc)
;; EOF
