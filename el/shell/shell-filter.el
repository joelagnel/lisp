(provide 'shell-filter)

;;Date:         Fri, 15 Dec 89 23:22:36 GMT
;;Reply-To:     gaynor@topaz.rutgers.edu
;;From:         Silver <paul.rutgers.edu!gaynor%RUTGERS.EDU>
;;Subject:      Giving shell filters one last kick...

;; When logging in remotely, you might want to give the command "stty -echo" to
;; suppress the echoing of commands as they're executed.  For tcsh, you may
;; also want to unset editmode and/or filec.  Up to you, though...

;; The regexp is a little convoluted, but I think it covers most of the bases.
(defvar shell-filter-password-prompt "\\<passw\\(or\\)?d[\ \t]*[:>][\ \t]*\\'"
"Regular expression used to determine whether shell output contains a request
for a password.  The successful candidate will match the end of the buffer,
handle whitespace appropriately, and various convolutions of `password'.  The
ambient value of case-fold-search is non-nil during matching.")

(defun read-string-no-echo (&optional prompt)
"Read and return a string without echoing it.  Newline and return characters
terminate input.  If optional PROMPT is non-nil, it is displayed and the cursor
placed in the minibuffer while reading.  \(Warning: view-lossage/recent-keys
can access the last 100 characters typed.\)"
  (interactive)
  (save-window-excursion
    (let ((echo-keystrokes 0)
          (string "")
          char)
      (if prompt
        (progn (select-window (minibuffer-window))
               (set-window-buffer (selected-window)
                                  (get-buffer-create " *Temporary*"))
               (erase-buffer)
               (insert prompt)))
      ;; Grossly inefficient.  BFD.
      (while (not (memq (setq char (read-char)) '(?\r ?\n)))
        (setq string (concat string (char-to-string char))))
      string)))

(defun shell-filter-read-password ()
"Read a password in-line (without display, of course) and return it."
  (read-string-no-echo))

(defun shell-filter-nuke-1    () (delete-char 1))
(defun shell-filter-ding      () (delete-char 1) (ding 'continue))
(defun shell-filter-backspace () (delete-char 1) (delete-char (if (eq ?_
 (preceding-char)) -1 1)))

;; I would have done this by regexp instead of character, but I think that this
;; would be putting more computation and effort than the task warrants.
(defvar shell-filter-specials-alist
  '((?\C-m . shell-filter-nuke-1)
    (?\C-l . shell-filter-nuke-1)
    (?\C-g . shell-filter-ding)
    (?\C-h . shell-filter-backspace))
"Alist of (CHARACTER . ACTION).  When CHARACTER is encountered in shell output,
call ACTION with no parameters.")

(defun shell-filter (process string)
"Output filter for shell-mode buffers.  See shell-filter-specials-alist for
information about special character handling.  See shell-filter-password-prompt
and shell-filter-read-password for information about password handling."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (marker-position (process-mark process)))
    (let ((begin (point))
          (end (progn (insert-before-markers string) (point)))
          (case-fold-search t)
          (specials (concat "^" (mapconcat (function (lambda (el)
                                                       (char-to-string (car
 el))))
                                           shell-filter-specials-alist ""))))
      (goto-char begin)
      ;; Alternatively, things could be based around re-search-foward.  There's
      ;; no need for the added overhead, imho.
      (while (progn (skip-chars-forward specials end) (< (point) end))
        (funcall (cdr (assoc (following-char) shell-filter-specials-alist))))
      ;; It might be more `correct' to match against string instead of the buffer.
      (if (re-search-backward shell-filter-password-prompt begin t)
        (progn (goto-char (match-end 0))
               (process-send-string process (concat (shell-filter-read-password)
 "\n")))))))

;; RU's site-init contains a function named add-hook.  I don't know if it's
;; standard, but its intent is fairly obvious.
(setq shell-mode-hook
      (function (lambda ()
		  ;; Uncomment this if you want shells to die easily.
		  ;; (process-kill-without-query (get-buffer-process (current-buffer)))
		  (set-process-filter (get-buffer-process (current-buffer))
                                          (function shell-filter)))))
