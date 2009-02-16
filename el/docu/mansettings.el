(defmacro foreach (var list1 body)
  (` (let ((, var) (list_to_traverse (, list1)))
       (while list_to_traverse
  (setq (, var) (car list_to_traverse))
  (setq list_to_traverse (cdr list_to_traverse))
  ((,@ body))))))


(require 'man)

;; Appearance of the hyperlinks, e.g. foreground color red
(defvar link_face (facemenu-get-face (intern "fg:Red")))
;; Location of the man-glimpse directory
(defvar man_glimpse_directory "/.tkman_glimpse_database")
;; Options for glimpse
(defvar man_glimpse_options "-nWyi -L 0:50:10 ")
;; Keys for the two commands
(define-key global-map [f11] 'man_if_necessary)
(define-key global-map [S-f11] 'man_glimpse_search)
(add-hook 'Man-mode-hook
   (function (lambda ()
        (local-set-key [mouse-2] 'man_on_current_link))))


(defun Man-fontify-manpage ()
  "Convert overstriking and underlining to the correct fonts.
Same for the ANSI bold and normal escape sequences."
  (interactive)
  (message "Please wait: making up the %s man page..." Man-arguments)
  (goto-char (point-min))
  (while (search-forward "\e[1m" nil t)
    (delete-backward-char 4)
    (put-text-property (point)
         (progn (if (search-forward "\e[0m" nil 'move)
      (delete-backward-char 4))
         (point))
         'face Man-overstrike-face))
  (goto-char (point-min))
  (while (search-forward "_\b" nil t)
    (backward-delete-char 2)
    (put-text-property (point) (1+ (point)) 'face Man-underline-face))
  (goto-char (point-min))
  (while (search-forward "\b_" nil t)
    (backward-delete-char 2)
    (put-text-property (1- (point)) (point) 'face Man-underline-face))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\)\\(\b\\1\\)+" nil t)
    (replace-match "\\1")
    (put-text-property (1- (point)) (point) 'face Man-overstrike-face))
  (goto-char (point-min))
  (while (re-search-forward "o\b\\+\\|\\+\bo" nil t)
    (replace-match "o")
    (put-text-property (1- (point)) (point) 'face 'bold))
  (goto-char (point-min))
  (while (re-search-forward "[-|]\\(\b[-|]\\)+" nil t)
    (replace-match "+")
    (put-text-property (1- (point)) (point) 'face 'bold))
  ;; \255 is some kind of dash in Latin-1.
  (goto-char (point-min))
  (while (search-forward "\255" nil t) (replace-match "-"))
  (goto-char (point-min))
  ;; CHANGE: Here the hyperlinks are detected and tinted
  (while (re-search-forward "\\([a-zA-Z0-9._]+([1-9lonpxD]\.?[^)]?)\\)" nil t)
    (overlay-put (make-overlay (match-beginning 1) (match-end 1))
          'face link_face))
  (message "%s man page made up" Man-arguments))


;; This is a new function similar to greprec (published some time ago) and
;; the built-in function grep
(defun man_glimpse_search (options_search_regexp)
  (interactive
   ;; Ask for the options, the regular expression and the toplevel directory
   (list (read-from-minibuffer "glimpse (opt - expr): "
          "" nil nil 'grep-history)))
  (let (buf
 (old_compilation_window_height compilation-window-height))
    ;; Use a larger window for displaying the grep hits
    (setq compilation-window-height 16)

    ;; Use a compilation buffer to execute the search command;
    (setq buf (compile-internal
        (concat "glimpse " man_glimpse_options
         " -H " (expand-file-name man_glimpse_directory)
         " " options_search_regexp)
        "No more glimpse hits" "glimpse"
        ;; Give it a simpler regexp to match.
        nil '(("^\\([^:( \t]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))))
    (save-excursion
      (set-buffer buf)
      (set (make-local-variable 'compilation-exit-message-function)
    (lambda (status code msg)
      (if (eq status 'exit)
   (cond ((zerop code)
   '("finished (matches found)\n" . "matched"))
         ((= code 1)
   '("finished with no matches found\n" . "no match"))
         (t
   (cons msg code)))
        (cons msg code)))))
  (setq compilation-window-height old_compilation_window_height)))



(defun Man-bgproc-sentinel (process msg)
  "Manpage background process sentinel."
  (let ((Man-buffer (process-buffer process))
 (delete-buff nil)
 (err-mess nil))
    (if (null (buffer-name Man-buffer)) ;; deleted buffer
 (set-process-buffer process nil)
      (save-excursion
 (set-buffer Man-buffer)
 (goto-char (point-min))
 (if (not (and (eq (process-status process) 'exit)
         (= (process-exit-status process) 0)))
      (progn
        (setq err-mess
       (concat (buffer-name Man-buffer)
        ": process "
        (let ((eos (1- (length msg))))
          (if (= (aref msg eos) ?\n)
       (substring msg 0 eos) msg))))
        (goto-char (point-max))
        (insert (format "\nprocess %s" msg))
        (sleep-for 0 1))
          ;; CHANGED: fontify and Man-mode only after the process has exited
   (Man-fontify-manpage)
   (run-hooks 'Man-cooked-hook)
   (Man-mode)
   (set-buffer-modified-p nil))
 ;; Restore case-fold-search before calling
 ;; Man-notify-when-ready because it may switch buffers.

 (if (not delete-buff)
     (Man-notify-when-ready Man-buffer))

 (if err-mess
     (error err-mess))
 ))))


;; What has to be done after the middle mouse button clicked onto a link
(defun man_on_current_link (event)
  (interactive "e")
  (let ((compilation-buffer (window-buffer (posn-window (event-end event))))
 current_overlay)
    (save-excursion
      (set-buffer compilation-buffer)
      (when (setq current_overlay
    (car (overlays-at (posn-point (event-end event)))))
 (man (buffer-substring (overlay-start current_overlay)
          (overlay-end current_overlay)))
 (sleep-for 0 200)
 ))))


;; Load a man page. If the volume isn't specified and a man page of the same
;; name has already been loaded, just switch to the corresponding buffer.
(defun man_if_necessary (man-args)
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
  (input (read-string
   (format "Manual entry%s: "
    (if (string= default-entry "")
        ""
      (format " (default %s)" default-entry))))))
    (if (string= input "")
        (if (string= default-entry "")
     (error "No man args given")
   default-entry)
      input))))
  (if (string-match "(" man-args)
      (man man-args)
    (let ((buffer_to_choose nil) buffer_iterator)
      (foreach buffer_iterator (buffer-list)
        (when (string-match (concat "^\*Man [^ \t]*[ \t]*" man-args "$")
       (buffer-name buffer_iterator))
   (setq buffer_to_choose buffer_iterator)))
      (if buffer_to_choose
   (switch-to-buffer buffer_to_choose)
 (man man-args)))))


(require 'compile)

(defun compile-mouse-goto-error (event)
  (interactive "e")
  (let ((compilation_buffer (window-buffer (posn-window (event-end event)))))
    (save-excursion
      (set-buffer compilation_buffer)
      (goto-char (posn-point (event-end event)))
      (or (compilation-buffer-p (current-buffer))
          (error "Not in a compilation buffer."))
      (setq compilation-last-buffer (current-buffer))
      ;; CHANGE: I think here was an error, because with the standard function,
      ;;   sometimes the message "no error to go" appeared without reason
      (compile-reinitialize-errors nil (point))

      ;; Move to bol; the marker for the error on this line will point there.
      (beginning-of-line)

      ;; Move compilation-error-list to the elt of compilation-old-error-list
      ;; we want.
      (setq compilation-error-list compilation-old-error-list)
      (while (and compilation-error-list
                  (> (point) (car (car compilation-error-list))))
        (setq compilation-error-list (cdr compilation-error-list))))
    (push-mark)
    ;; CHANGE: for glimpse
    (if (string-equal (buffer-name compilation_buffer) "*glimpse*")
 (progn
   (require 'man)
          (let ((old_manual-program manual-program)
  (old_process-connection-type process-connection-type))
     (setq process-connection-type nil)
            (setq manual-program "nroff -man")
            (man (buffer-file-name
    (marker-buffer (cdr (compilation-next-error-locus nil nil)))))
            (setq manual-program old_manual-program)
     (setq process-connection-type old_process-connection-type)))
      (next-error 1))))
