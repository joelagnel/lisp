(defun open-init-dot-el-file () 
  (interactive)
  (find-file "~/.emacs"))

(defun load-dot-emacs-file () 
(interactive)
(load-file "~/.emacs"))

;;Moves point/cursor to matching parens. C-c]
(defun match-paren () 
  "Go to the matching parenthesis if on parenthesis otherwise insert %." 
  (interactive) 
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1)) 
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1)) 
        ((progn (backward-char 1) (looking-at "\\s\)")) (forward-char 1) (backward-list 1)) 
        (t (forward-char) (message "NÃ£o estÃ¡ sobre ( ou )"))))


(defun open-shell-window (&optional n)
  (interactive)
  (split-window-vertically -10)
  (other-window 1)
  (shell))

(defun list-buffers-other-win ()
  "Opens list-buffers and put focus on it"
  (interactive)
  (list-buffers)
  (other-window 1)
  (goto-char (+ 4 (point))))


;; UTF8 input method

(setq read-quoted-char-radix 16)

(defun unicode-insert (char)
  "Read a unicode code point and insert said character.
Input uses `read-quoted-char-radix'.  If you want to copy
the values from the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (ucs-insert char))

(defun my-perl-find-error ()
  "Parse error message at point and go there."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward ": \\(.* at \\(.*\\) line \\([0-9]+\\)\\)" nil t)
    (let ((msg (match-string 1))
	  (file (match-string 2))
	  (line (string-to-number (match-string 3))))
      (message msg)
      (find-file file)
      (goto-line line))))


