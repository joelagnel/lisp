
;;C - mode


(require 'compile)


(defvar compile-guess-command-table
  '((c-mode       . "gcc -Wall -g %s -o %s -lm"); Doesn't work for ".h" files.
    (c++-mode     . "g++ -g %s -o %s -lm")      ; Doesn't work for ".h" files.
    ))


;; This code guesses the right compilation command when Emacs is asked
;; to compile the contents of a buffer.  It bases this guess upon the
;; filename extension of the file in the buffer.


(defun compile-guess-command ()

  (let ((command-for-mode (cdr (assq major-mode
                                     compile-guess-command-table))))
    (if (and command-for-mode
             (stringp buffer-file-name))
        (let* ((file-name (file-name-nondirectory buffer-file-name))
               (file-name-sans-suffix (if (and (string-match "\\.[^.]*\\'"
                                                             file-name)
                                               (> (match-beginning 0) 0))
                                          (substring file-name
                                                     0 (match-beginning 0))
                                        nil)))
          (if file-name-sans-suffix
              (progn
                (make-local-variable 'compile-command)
                (setq compile-command
                      (if (stringp command-for-mode)
                          ;; Optimize the common case.
                          (format command-for-mode
                                  file-name file-name-sans-suffix)
                        (funcall command-for-mode
                                 file-name file-name-sans-suffix)))
                compile-command)
            nil))
      nil)))


;; Add the appropriate mode hooks.

(add-hook 'c-mode-hook       (function compile-guess-command) (lambda () ((c-set-style "stroustrup"))))
(add-hook 'c++-mode-hook     (function compile-guess-command))

;;; Here are the new commands that are invoked by the "Compile" menu.

(defun previous-compilation-error ()
  (interactive)
  (next-error -1))

(defun first-compilation-error ()
  (interactive)
  (next-error '(4)))

(defvar check-history nil)

(defun check-file ()
  "Run ftnchek on the file contained in the current buffer"
  (interactive)
  (let* ((file-name (file-name-nondirectory buffer-file-name))
         (check-command (read-from-minibuffer
                         "Check command: "
                         (format "ftnchek %s" file-name) nil nil
                         '(check-history . 1))))
    (save-some-buffers nil nil)
    (compile-internal check-command "Can't find next/previous error"
                      "Checking" nil nil nil)))

(defun make ()
  (interactive)
  (save-some-buffers nil nil)
  (compile-internal (read-from-minibuffer "Make command: " "make ")
                    "Can't find next/previous error" "Make"
                    nil nil nil))


;; include-at-point

(defun c-include-at-point ()
  "return the filename in the next #include statement."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[\"<]")
    (let ((start (point)))
      (re-search-forward "[\">]")
      (let ((end (point))
            ;; It might be better to search a global and local include path list.
            ;; This is a temporary hack.  Please send patches :-)
            (prefix (if (string-equal (buffer-substring (1- start) start) "\"")
                        "" ;; local include path
                        "/usr/include/")))  ;; global include path
          (concat prefix (format "%s" (buffer-substring start (1- end))))))))

(defun c-find-include ()
  "Visit the include file at point."
  (interactive)
  (find-file (c-include-at-point)))


(define-key global-map "\C-c\C-j" 'c-find-include)


(load "de-vars")
(setq load-path (cons (concat root-path "modes/c-mode") load-path))

(require 'ctypes)

; let emacs put in a return for you after left curly braces,
; right curly braces, and semi-colons.
(setq c-auto-newline 1)


(require 'c-includes)

;;Manual entry for word at point

(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))

(setq c-hungry-delete-key t)

;; will let emacs put in a "carriage-return" for you automatically
;; after left curly braces, right curly braces, and semi-colons in "C mode"

(setq c-auto-newline 1)

(global-set-key [f12]         'dabbrev-expand)
(define-key esc-map [f12]     'dabbrev-completion)

;;etags

(add-hook 'c-mode-common-hook
        (lambda ()
                (define-key c-mode-map [(ctrl tab)] 'complete-tag)))




;; hide and show blocks

(add-hook 'c-mode-hook 'hs-minor-mode)

(define-key global-map "\C-c\}" 'hs-hide-all)
(define-key global-map "\C-c\{" 'hs-show-all)
(define-key global-map "\C-c\]" 'hs-hide-block)
(define-key global-map "\C-c\[" 'hs-show-block)



;; Templates
;; This is a way to hook tempo into cc-mode
(defvar c-tempo-tags nil
  "Tempo tags for C mode")
(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")


(require 'hyper-compile)

(provide 'de-c)

