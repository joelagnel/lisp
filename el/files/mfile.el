;; SYNOPSIS: Visit the file mentioned at cursor.
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh
;; USAGE with:  grep -n -B 7 "FAILED" *.log > all
;; ToDo: C-u should get you next match, as in tags.
;;       too much to ask for case insensitive match.

(defun mosh-file-here (&optional begin end)
  "Pick file from point, standalone."
  (interactive)
  (or begin (setq begin (point)))
  (or end   (save-excursion
              (forward-char 3)  ;; Skip over initial c:/ /win95
              (re-search-forward "[\n\t ;:\"'(){}]")
              (backward-char)
              (setq end (point))))
  (mosh-color-region begin end 'red)
  (let ( (name (buffer-substring begin end)) )
    (if (file-readable-p name)
        (find-file-other-window name)
      ;; Else (error "file '%s' unreadable" name)
      (mosh-find-new-file name 1)
      ))
)

;;; Goto to a file and line mentioned on the current line.
;;; Assume filename.ext is nearby (20 chars).

(defvar mosh-err-file-re (concat
   "\\("
         "[/\\][/\\]"                 "\\|"      ; \\ or // or \/ or /\
         "~?[a-zA-Z0-9_-]*[/\\]"      "\\|"      ; ~/ or ~name/ or /
         "\\b[a-z]:[/\\]"                        ; c:/
   "\\)?"                                        ; = $1.
     "\\(\\([_a-zA-Z0-9-.]+[/\\]\\)*\\)"         ; ((name/)*) path = $2.
     "\\([._a-zA-Z0-9-]+\\.[_a-zA-Z0-9-]+\\)"     ; file.ext = $4
     "[^0-9]?"                                   ; :?
     "\\([0-9]*\\)"                              ; line = $5.
                             )
  "Regexp that matches path/file.ext:linenumber, used by mosh-err-file."
)

(defun mosh-err-file ()
  "If point is on the line: 'PATH/FILE:NUMBER:',
   visit PATH/FILE at line NUMBER."

  (interactive)
  (let (full path name line eol dirlist)
    (save-excursion
      (end-of-line) (setq eol (point)) (beginning-of-line)
      (if (re-search-forward mosh-err-file-re eol t) ;; ~/file.txt:21
          (progn
            (setq path (concat (match-string 1) (match-string 2)
                               ;; (match-string 3)
            ))
            (setq name (match-string 4))   ;; d.x
            (if (match-string 5)           ;; 212
                  (setq line (string-to-number (match-string 5)))) ;; 212
            (if (match-string 1) (mosh-color-match 'blue  1))      ;; ~/
            (if (match-string 2) (mosh-color-match 'red   2))      ;; a/b/c/
            (if (match-string 3) (mosh-color-match 'green 3))      ;; c/
            (if (match-string 4) (mosh-color-match 'blue  4))      ;; d.x
            (if (match-string 5) (mosh-color-match 'green 5))      ;; 212
          )
        (error "mosh-err-file(): No file name found")
      )

      ;; (match-string 0) (match-string 1) (match-string 2)
      ;; (match-string 3) (match-string 4) (match-string 5)
      ;; ~/a/b/c/d.x:212 g:/mosh/x.c       (setq eol (point))

      (or path (setq path ""))
      (or (> line 0)
          (re-search-forward "[0-9]+" (+ (point) 1000) t)
          (setq line (string-to-number (match-string 0))))
      (message "mosh-err-file: path=%s, name=%s, line=%s" path name line)
    )
    (setq dirlist (cons (file-name-directory buffer-file-name)
                        mosh-cdpath))
    (mosh-c-check-file name line path dirlist)
))

;;; ./conrb1.log-419
;;; mosh.el: 11
;;; g:\mosh\emacs\mosh.el         12
;;;   \mosh\emacs\mosh.el         13
;;;              \mosh.el         14
;;;             ./mosh.el         15
;;; \\mohsina_p90\winsock2\readme.ws2 16

(defvar mosh-cdpath (list default-directory "./" "~/" "../")
  "*List of prefix paths, for find files; like cdpath.
   Keeps growing as it finds out about new dirs."
)

(defun mosh-c-check-file (name line path dirlist)
  "Find the file (dirlist|mosh-cdpath)(/path/)name and goto line.
   Used to find files flagged by c-check.
    ~/a/b/c/d.x:212
   name=x.c, line=212, path=~/a/b/c/, dirlist=( /usr/ /etc/)"

  (message "mosh-c-check-file: name=%s, line=%d, path=%s, dirlist=%s"
           name line path dirlist)
  (let (newpath full)
    (setq newpath path)
    (setq full (concat newpath name))
    (if (file-exists-p full)                          ;; ~/a/b/c/x.c?
        (mosh-find-old-file full newpath line)
      ;; Else
      (if (null dirlist)
          (mosh-find-new-file name line)              ;; x.c
        ;; Else
        (setq newpath (concat (car dirlist) path))    ;; /usr/~a/b/c/
        (setq full (concat newpath name))             ;; /usr/~a/b/c/x.c
        (if (file-exists-p full)
          (mosh-find-old-file full newpath line)
          ;; Else
          (setq full (concat (car dirlist) name))     ;; /usr/x.c
          (if (file-exists-p full)
              (mosh-find-old-file full nil line)
            ;; Else
            (mosh-c-check-file name line path (cdr dirlist)))
)))))

(defun mosh-find-new-file (name line)
  "Find file=name in a (ask user for new dir), and go to line."
  (let (newdir full)
    (setq newdir
          (read-file-name
           (concat  "Search " name ":" line ", in dir=")
           "" "*none*" 'mustmatch))
    (setq newdir (expand-file-name newdir))
    (or (file-directory-p newdir)
        (error "dir=%s unreadble." newdir))
    (setq full (concat newdir "/" name))
    (cond ((file-readable-p full)
           (mosh-find-old-file full newdir line))
          (t
           (message "File '%s/%s' not found." newdir name)
           (mosh-find-new-file name line))
  ))
)

(defun mosh-find-old-file (full newdir line)
  "Open existing file in other window and goto line,
   add newdir to mosh-cdpath."
  (message "mosh-find-old-file: full=%s, newdir=%s" full newdir)
  (if (member newdir mosh-cdpath) nil
    (setq mosh-cdpath (cons newdir mosh-cdpath)))
  (find-file-read-only-other-window full)
  (set-buffer (file-name-nondirectory full))
  (goto-line (or line 1))
  (mosh-color-line 'green)
  (other-window 1)
)

(provide 'mfile)
;; EOF
