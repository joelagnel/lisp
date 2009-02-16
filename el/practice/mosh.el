;; SYNOPSIS: gnu/x/nt emacs customisation functions
;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh

;; REGION/COPY/CUT/PASTE/MARK/SEARCH

(if xemacs
    (defun transient-mark-mode (show)
      (setq zmacs-regions  (not (eq show 0))))
)

(defun mosh-show-region ()                    "Flash region, several uses."
    (interactive)
    (if (window-minibuffer-p (selected-window)) nil
      (count-lines-region (region-beginning) (region-end))
      (transient-mark-mode 1)
      (sit-for 0.1)
      (transient-mark-mode 0)
    )
)

(defun mosh-exchange-mark-and-point ()
  "Flashy Exchange point and mark, builtin mosh-show-region."
  (interactive)
    (if (window-minibuffer-p (selected-window)) nil
      (transient-mark-mode 1)
      (exchange-dot-and-mark)
      (count-lines-region (point) (mark))
      (sit-for 0.1)
      (transient-mark-mode 0)
))

(defun mosh-mark-key (arg)              "Push/Pop marks."
   (interactive "P")
   (set-mark-command arg)
   (message "%s %s." (if arg "Jumped to" "Marked") (what-line))
   (transient-mark-mode 1)
)


(defun mosh-copy-region ()                         "COPY region [M-w]"
  (interactive)
  (if (eq last-command this-command) nil
    (copy-region-as-kill (region-beginning) (region-end)))
  (if (window-minibuffer-p (selected-window)) nil
    (mosh-exchange-mark-and-point)
    (message "Region copied"))
)


(defvar mosh-copy-thing nil "Saved thing for pasting.")

(defun mosh-copy-thing (&optional what)
  "Copy what to CLIPBOARD and variable.
   what can be word/line/region/file-name/slm-file-name.
   Note we already have mosh-copy-region."

  (interactive "S word/line/region/file-name/slm-filename: ")
  (cond
   ((eq what 'region)
    (setq mosh-copy-thing
          (buffer-substring (point) (mark))))
   ((eq what 'line)
    (save-excursion
      (setq mosh-copy-thing
            (buffer-substring
             (progn (beginning-of-line)(point))
             (progn (forward-line 1)   (point))))))
   ((eq what 'word)
    (setq mosh-copy-thing (current-word)))
   ((eq what 'file-name)
    (setq mosh-copy-thing (buffer-file-name)))
   ((eq what 'slm-file-name)
    (setq mosh-copy-thing (mosh-find-slm-file nil 'getname)))
  )

  ;; See /usr/local/share/emacs/19.34/lisp/select.el
  (if w32emacs
      (win32-set-clipboard-data mosh-copy-thing)
    (x-set-selection 'PRIMARY mosh-copy-thing))

  (if (window-minibuffer-p (selected-window)) nil
    (message "Copied <%s>, paste with [f12 p]" mosh-copy-thing))
)

;; Was: (insert-register ?a) (message "Reg a pasted.")

(defun mosh-yank ()                           "Paste and show region."
    (interactive)
    (yank)
    (mosh-exchange-mark-and-point)
)

(defun mosh-yank-pop () "Simpified yank-pop with show region. [M-Paste]"
  (interactive)
  (delete-region (point) (mark t))
  (insert (current-kill '1))
  (mosh-exchange-mark-and-point)
)


(defun mosh-delete-whitespace ()
  "Delete whitespace and delete-blank-lines bidirectionally"
  (interactive)
  (cond
   ((looking-at "[ \t]")
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point))))
   ((looking-at "\n\n")
    (delete-blank-lines))
   ((eolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t\n") (point))))
   ((bolp)
    (delete-region (point)
                   (progn (skip-chars-backward " \t\n") (point))))
   (t
    (delete-horizontal-space))
))

(defun mosh-trim-blanks ()
   "Delete trailing blanks, C-m, untabify, from all lines in file.
    was called by 'write-file-hook, now on \\[mosh-trim-blanks]"
   (interactive)
   (untabify (point-min) (point-max))
   (save-excursion
     (goto-char (point-min))
     (replace-regexp "[ \t\C-m]+$" "" ))
   (message "Trimmed trailing blanks")
   nil      ; Must return nil for write-file-hook to continue.
)

(defun mosh-pad-blanks ()       "Pad blanks to each line upto fill column."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (move-to-column fill-column 'extend)
      (forward-line)
  ))
  (message "Padded all lines with spaces upto column %s" fill-column)
)

(defun mosh-no-tabs () "Remove all tabs"
  (interactive)
  (untabify (point-min) (point-max))
)

(defun mosh-no-cr () "Remove all pesky DOS Carriage-Return"
  (interactive)
   (save-excursion
     (goto-char (point-min))
     (replace-regexp "\C-m$" "" ))
)

(defun mosh-tab-one ()   "Like vi >> adds a tabspace."
  (interactive)
  (insert "    ")
)

(defun mosh-untab-one ()   "Like vi << del a tabspace."
  (interactive)
  (save-excursion
    ; (beginning-of-line)
    (skip-chars-backward " ")
    (if (looking-at "    ") (replace-match ""))
))

(defvar mosh-fill-column 60 "fill a line with char upto this char.")
(defun mosh-fill-to-column () "Repeat last char upto mosh-fill-column."
  (interactive)
  (if (eq (preceding-char) ?\n) (insert "="))
  (insert-char (preceding-char) (- mosh-fill-column (current-column)))
)

;; ===============================================
;; Vertical Motion

(defun mosh-vert-up (&optional lines)
  "Move cursor vertically up by 1 lines, add spaces when needed.
   Use negative lines to move down. Keys: [H-up] and [H-down]"
  (interactive "P")
  (let (i)
    (setq i (current-column))
    (previous-line (or lines 1))
    (move-to-column i "Insert spaces if needed"))
)


;; MATCH-PAREN

(defun mosh-match-paren-other-window () "Match-Paren or Other-Window. kp5"
   (interactive)
   ;; (setq zmacs-regions t)
   (cond
     ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
     ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
     ((eq (count-windows) 1)                    ; Single window?
      ;; (exchange-point-and-mark)
      (mosh-exchange-mark-and-point)
      )
     (t (other-window '1)))
)

(defun mosh-close-paren ()
  "Find last open paren, and close it and indent, key: \\[mosh-close-paren]."
  (interactive)
  (insert
   (matching-paren
    (save-excursion
      (backward-up-list 1)
      (following-char)))
  )
  (indent-for-tab-command)
  (newline-and-indent)
)

(defun mosh-kill-junk-buffer (BUFNAME)
   (if (member BUFNAME (mapcar 'buffer-name (buffer-list)))
    (kill-buffer BUFNAME)
))

(defun mosh-kill-junk () "kill tmp buffers"
  (interactive)
  (let (bufs abuf)
    (setq bufs (mapcar 'buffer-name (buffer-list)))
    (while (setq abuf (car bufs))
      (setq bufs (cdr bufs))
      (and (numberp (string-match "^ *\\*" abuf))
           (mosh-kill-junk-buffer abuf)
           ;; (message "killed %s" abuf)
      )
  ))
  (garbage-collect)
)

(defun mosh-stop ()
   "Stop: Zap buffer or come out of minibuffer."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (abort-recursive-edit)
    (kill-buffer (current-buffer))
))

(defun mosh-name-date ()
  "Return MohsinA, 02-Jan-96."
  (concat
   (format-time-string "%Y-%m-%d ")
   ; user-full-name "mailto:"
   user-mail-address
))

(defun mosh-insert-header ()
  "Insert a long descriptive header for new src files."
  (interactive)
  (let (cs ce bn)
    (setq cs (concat (or comment-start ";") "  " ))
    (setq ce (concat (or comment-end "") "\n"))
    (setq bn (or buffer-file-name  (buffer-name)))
    (insert cs "Name:   " user-full-name        ce
            cs            user-url              ce
          ; cs "Date:   " (current-time-string) ce
            cs "Date:   " (format-time-string
                          "%Y-%m-%d %H:%M %b %a") ce
                        ; "%Y-%m-%d %H:%M (%Y-%b-%d %a)")
            cs "MailTo: " user-mail-address     ce
            cs "File:   " hostname ":" bn         ce
)))


(defun mosh-toggle-case ()                      "Like vi ~ command."
    (interactive)
    (let (s e c (case-fold-search nil))
      (setq s (point))
      (setq c (looking-at "[A-Z]"))
      (forward-char)
      (setq e (point))
      (if c (downcase-region s e) (upcase-region   s e)))
)

(defvar mosh-case-word-up nil "toggle case of word")
(defun mosh-case-word ()  "flip the case of this word"
  (interactive)
  (if mosh-case-word-up
      (capitalize-word -1)
    (downcase-word -1))
  (setq mosh-case-word-up (not mosh-case-word-up))
)
;; PRINT

;;  KILLERS
;; They only kill one type of chars at a time, jump over newlines.

(defun mosh-kill-words (count)              "Safer C-Backspace/Delete"
  (interactive)
  (cond
   ((and (> count 0) (eolp) (not (bolp)))  ; Avoid killing ...<>\n
    (forward-char))
   ((and (< count 0) (bolp) (not (eolp)))  ; Avoid killing \n<>...
    (backward-char))
   (t (kill-region (point) (progn (mosh-move-word count) (point))))
))

(defun mosh-kill-line (count)
  "-1 => kill line to bolp or backspace."
  (interactive)
  (cond
   ((< count 0)
      (if (bolp) (backward-delete-char-untabify 1) (kill-line 0))
)))

(defun mosh-frame-title ()              "Give a title to this frame."
  (interactive)
  (if window-system
      (modify-frame-parameters
       (selected-frame)
       (list
        (cons 'name
              (concat
               "GnuEmacs:" emacs-version ", "
               ;; (format-time-string "%y-%m-%d") ", "
               ;; user-login-name "@" hostname
               user-mail-address
        ))
        ; (cons 'top   30)
        ; (cons 'left -10)
))))


(defun mosh-tag-dir (dir)
  "Make DIR the default for everything.
   DIR/FILE acceptable from history, only DIR part is used."
  (interactive "DDefault Dir (for etags): ")
  (setq default-directory (file-name-directory dir))
  (visit-tags-table  "etags")
)

(setq-default find-tag-hook 'mosh-color-match)

(defun swap-bs-and-del ()
  "Actually swaps C-h and C-\?, for non windowing systems."
  (let ((new-xlat-table (make-string 128 0) ) (index 0))
                                        ; Generate the identity map.
    (while (< index 128)
      (aset new-xlat-table index index)
      (setq index (1+ index) ))
                                        ; Swap backspace and del.
    (aset new-xlat-table 8 127)
    (aset new-xlat-table 127 8)
    (setq keyboard-translate-table new-xlat-table)
))


(defun mosh-dira (&rest list)
  "Find the first readable file or dir from the list
   of dirs and env variables of form $var."
  (if (null list) nil
    (let ((head (car list)) (tail (cdr list)) envval)
      ; (message "list=%s, head=%s, tail=%s" list head tail)
      (cond
       ((null head) (apply 'mosh-dira tail))
       ((file-readable-p head) head)
       ((eq (elt head 0) ?$)
        (if (setq envval (getenv (substring head 1)))
            (apply 'mosh-dira (cons envval tail))
          (message "mosh-dira getenv(%s) failed" head)
          (apply 'mosh-dira tail)
       ))
       (t (apply 'mosh-dira tail))))
))

(defun mosh-dirl (&rest list)
  "Returns the sub list of readable files/dirs from the list
   of dirs and env variables of form $var.
   A list of Lists is flattened."
  (if (null list) nil
    (let ((head (car list)) (tail (cdr list)) envval)
      ; (message "list=%s, head=%s, tail=%s" list head tail)
      (cond
       ((null head) (apply 'mosh-dirl tail))
       ((listp head)
        (append (apply 'mosh-dirl head)
                (apply 'mosh-dirl tail)))
       ((file-readable-p head)
        (cons head (apply 'mosh-dirl tail)))
       ((eq (elt head 0) ?$)
        (if (setq envval (getenv (substring head 1)))
            (apply 'mosh-dirl (cons envval tail))
          ;; Else
          (message "mosh-dirl getenv(%s) failed" head)
          (apply 'mosh-dirl tail)
       ))
       (t (apply 'mosh-dirl tail))))
))


(defun mosh-line-numbers ()
  "insert line numbers into file."
  (interactive)
  (beginning-of-buffer)
  (let ((num 1)) ; (setq num 1)
    (while (not (eobp))
      (forward-line)
      (insert (number-to-string num) "| ")
      (setq num (+ num 1))
    ))
)

(provide 'mosh)

;; EOF
