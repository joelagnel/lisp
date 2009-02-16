
(load "de-vars")
(setq load-path (cons (concat root-path "buffer") load-path))

;;escreen
(load "escreen")
(escreen-install)
(global-set-key (kbd "M-1") 'escreen-goto-screen-0)
(global-set-key (kbd "M-2") 'escreen-goto-screen-1)
(global-set-key (kbd "M-3") 'escreen-goto-screen-2)
(global-set-key (kbd "M-4") 'escreen-goto-screen-3)
(global-set-key (kbd "M-5") 'escreen-goto-screen-4)
(global-set-key [S-right] 'escreen-goto-next-screen)
(global-set-key [S-left]  'escreen-goto-prev-screen)

(require 'setnu)
(require 'rotate-split)
(require 'icomplete+)
(require 'aok)

;; understand ~ and / in minibuff
(load "minibuf-electric-gnuemacs")

;;set the frame-title to escreen buffer name!
(setq frame-title-format
      '("%S: " (buffer-file-name "%f"
                (dired-directory dired-directory "%b"))))


(setq load-path (cons (concat root-path "buffer/icicles") load-path))

(require 'icicles)
;(icicle-mode 1)


(defun list-buffers-other-win ()
  "Opens list-buffers and put focus on it"
  (interactive)
  (list-buffers)
  (other-window 1)
  (goto-char (+ 4 (point))))


;; Access buffers like dired
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'list-buffers-other-win)
(autoload 'ibuffer "ibuffer" "List buffers." t)


;;enlarge window
(define-key global-map (kbd "M-=") 'enlarge-window)
(define-key global-map (kbd "M-+")
  (lambda () (interactive) (enlarge-window -1)))


;;muliple-buffers
;(put 'dired-find-alternate-file 'disabled nil)
;;  (defun dired-follow-file ()
;;   "In dired, visit the file or directory on this line.
;; If a directory is on the current line, replace the current Dired buffer
;; with one containing the contents of the directory. Otherwise, invoke
;; `dired-find-file' on the file."
;;   (interactive)
;;   (let ((filename (dired-get-filename)))
;;     (if (file-directory-p filename)
;;         (find-alternate-file filename)
;;       (dired-find-file))))

;; (defun dired-setup-follow-file ()
;;   (substitute-key-definition
;;    'dired-find-file 'dired-follow-file dired-mode-map)
;;   (substitute-key-definition
;;    'dired-advertised-find-file 'dired-follow-file dired-mode-map))

;; (add-hook 'dired-mode-hook 'dired-setup-follow-file)

