;; -*- emacs-lisp -*-
;;; ftp-el.el --- FTP downloads from within emacs

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is some barebones code to provide an emacs interface to downloading via
;; FTP. The basic idea is that you browse an FTP using ange-ftp (c-x c-f
;; /ftp:myuser@myhost:), move the point over a file or directory and do M-x
;; eftp-ftp-queue-current (or SPC) to add the file to the queue.

;; M-x eftp-start-queue will start downloading files from the queue, M-x
;; eftp-stop-queue will pause the queue (although anything downloading will
;; continue to do so).

;; M-x eftp-display-downloads (or ?) will show the current contents of the
;; queue and provide buttons covering various functionality.

;;; Code:

(defvar eftp-waiting-queue '() "The queue of urls awaiting download")
(defvar eftp-downloading-urls (make-hash-table :test 'equal)
  "a hash table mapping urls currently being downloaded to processes")
(defvar eftp-make-download-cmdline 'eftp-lftp-download-cmdline
  "A function which takes a URL struct as an argument and returns a list of
the form (command <arg1> .. <argn>)")
(defvar eftp-max-downloads 1
  "the number of downloads that can be run in parallel")
(defvar eftp-queue-active nil "is the queue currently downloading?")
(defvar eftp-download-dir (getenv "HOME")
  "directory in whicn to place downloads")

(defstruct eftp-url username password hostname path isdirectory port)

(defun eftp-dump-queues ()
  "Dump the queues to a file"
  (interactive)
  (let ((acc '()))
    (maphash (lambda (k v)
               (push k acc))
             eftp-downloading-urls)
    (with-temp-buffer
      (insert (format "(setq eftp-waiting-queue %S)"
                      (cons 'list (append eftp-waiting-queue acc))))
      (write-file "~/.ftp-dumped-queue.el" nil))))

(defun eftp-lftp-download-cmdline (url)
  (let ((dir (file-name-nondirectory
              (eftp-string-trim-right (eftp-url-path url) ?/)))
        (path (file-name-directory
               (eftp-string-trim-right (eftp-url-path url) ?/))))
    (if (eftp-url-isdirectory url)
        (list "lftp" "-c"
              (format "open ftp://%s:%s@%s:%s; cd \"%s\"; mirror -c \"%s\""
                      (eftp-url-username url) (eftp-url-password url)
                      (eftp-url-hostname url) (eftp-url-port url)
                      path dir))
      (list "lftp" "-c"
            (format "open ftp://%s:%s@%s:%s; cd \"%s\"; get -c \"%s\""
                    (eftp-url-username url) (eftp-url-password url)
                    (eftp-url-hostname url) (eftp-url-port url)
                    path dir)))))

(defun eftp-start-queue ()
  "start downloading the contents of the queue"
  (interactive)
  (make-directory eftp-download-dir t)
  (setq eftp-queue-active t)
  (eftp-download-next))

(defun eftp-stop-queue ()
  "inhibit any further items from being downloaded"
  (interactive)
  (setq eftp-queue-active nil))

(defun eftp-add-to-queue (url)
  "Queue URL for download"
  (push url eftp-waiting-queue)
  (eftp-dump-queues)
  (eftp-refresh-downloads)
  (unless (member (get-buffer "*downloads*") (visible-buffers))
    (eftp-display-downloads))
  (eftp-download-next))

(defun eftp-next-in-queue ()
  "pop the next process from the queue"
  (prog1
      (car (last eftp-waiting-queue))
    (setq eftp-waiting-queue (butlast eftp-waiting-queue))))

(defun eftp-remove-from-queue (url)
  "Remove URL from the download queue and kill its process if it was running"
  (setq eftp-waiting-queue
        (remove url eftp-waiting-queue))
  (eftp-dump-queues)
  (eftp-refresh-downloads))

(defun eftp-kill-download (process)
  "Kill a process and remove its associated buffer"
  (when (process-buffer process)
    (when (get-buffer-window (process-buffer process))
      (delete-window (get-buffer-window (process-buffer process))))
    (kill-buffer (process-buffer process)))
  (kill-process process))

(defun eftp-running-downloads-count ()
  (hash-table-count eftp-downloading-urls))

(defvar *eftp-timer* nil "Used for updating the display periodically")

(defun eftp-download-next ()
  "Send the next queued item for download"
  (eftp-dump-queues)
  (when (and eftp-queue-active
             (< (eftp-running-downloads-count) eftp-max-downloads))
    (lexical-let ((url (eftp-next-in-queue)))
      (cond (url
             (cd eftp-download-dir)
             (let* ((cmdline (funcall eftp-make-download-cmdline url))
                    (process (apply 'start-process "ftp-download" nil
                                    cmdline)))
               (when (zerop (eftp-running-downloads-count))
                 (when (timerp *eftp-timer*)
                   (cancel-timer *eftp-timer*))
                 (setq *eftp-timer*
                       (run-with-idle-timer 1 t 'eftp-refresh-downloads)))
               (puthash url (cons process nil) eftp-downloading-urls)
               (eftp-refresh-downloads)
               (set-process-sentinel process
                                     (lambda (proc change)
                                       (remhash url eftp-downloading-urls)
                                       (eftp-refresh-downloads)
                                       (eftp-download-next)))
               (set-process-filter process
                                   `(lambda (proc output)
                                      (let ((output (remove
                                                     (string-to-char "")
                                                     output)))
                                        (puthash ,url (cons ,process output)
                                                 eftp-downloading-urls))))))
            (t (when (and (timerp *eftp-timer*)
                          (zerop (eftp-running-downloads-count)))
                 (cancel-timer *eftp-timer*)))))))

(defun eftp-dired-to-url (dired-entry isdirectory)
  "Return a structure representing a dired-entry (ie. the return value
  of (dired-get-filename))"
  (let ((split (split-string dired-entry "[:@]")))
    (destructuring-bind (protocol user host path)
        (if (= (length split) 4)
            split
          (list* (car split) ange-ftp-default-user (cdr split)))
      (let ((host (car (split-string host "#")))
            (port (cadr (split-string host "#"))))
        (make-eftp-url :username user :password (ange-ftp-get-passwd host user)
                       :hostname host
                       :path path
                       :port (or (ignore-errors (string-to-number port)) 21)
                       :isdirectory isdirectory)))))

(defun eftp-ftp-queue-current ()
  (interactive)
  (mapc #'(lambda (file)
            (eftp-add-to-queue
             (eftp-dired-to-url file (not (dired-nondirectory-p file)))))
        (or (dired-get-marked-files) (list (dired-get-filename))))
  (dired-unmark-all-marks))


(defun eftp-display-downloads ()
  (interactive)
  (pop-to-buffer "*downloads*")
  (eftp-refresh-downloads)

  (let ((map (copy-keymap (or (current-local-map) (make-sparse-keymap)))))
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map [backtab] 'backward-button)
    (define-key map (kbd "q")
      (lambda ()
        (interactive)
        (when (and (timerp *eftp-timer*)
                   (zerop (eftp-running-downloads-count)))
          (cancel-timer *eftp-timer*))
        (kill-buffer-and-window)))
    (use-local-map map)))

(defun eftp-insert-heading (text)
  (let ((p (point)))
    (insert text)
    (add-text-properties
     p (point)
     '(face (:height 1.1 :weight bold :inherit variable-pitch)))))

(defun eftp-refresh-downloads ()
  "Display the current queue in a buffer called *downloads*"
  (let ((pt (point)))
    (when (get-buffer "*downloads*")
      (with-current-buffer (get-buffer "*downloads*")
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (goto-char (point-min))
          (insert "\n")
          (setq header-line-format
                (format "[eftp] %s   %s   %s"
                        (if eftp-queue-active
                            "queue active"
                          "queue disabled")
                        (format "Maximum downloads: %s" eftp-max-downloads)
                        (format "Download dir: %s" eftp-download-dir)))

          (insert (concat (make-string (or fill-column 70) ?=) "\n\n"))
          ;; show downloading items
          (eftp-insert-heading "Downloads\n\n")
          (maphash
           #'(lambda (url entry)
               (destructuring-bind (id . last-output) entry
                 (insert "  ")
                 (eftp-create-button "kill"
                                     `(lambda ()
                                        (interactive)
                                        (eftp-kill-download ,id)))
                 (insert (string-pad (format " %s%s"
                                             (eftp-url-hostname url)
                                             (eftp-url-path url))
                                     (- (frame-width) 20)))
                 (insert "\n")

                 (when last-output
                   (insert "  ")
                   (insert (string-pad
                            (car (last (split-string last-output "[\n\r]")))
                            (- (frame-width) 4)))
                   (insert "\n"))
                 (insert "\n")))
           eftp-downloading-urls)

          (eftp-insert-heading "Waiting\n\n")
          ;; show waiting items
          (mapc #'(lambda (url)
                    (insert "  ")
                    (eftp-create-button "remove"
                                        `(lambda ()
                                           (interactive)
                                           (eftp-remove-from-queue ,url)))
                    (insert (string-pad (format " %s%s"
                                                (eftp-url-hostname url)
                                                (eftp-url-path url))
                                        (- (frame-width) 20)))
                    (insert "\n"))
                (reverse eftp-waiting-queue))
          (delete-blank-lines)
          (insert "\n")
          (insert (concat (make-string (or fill-column 70) ?=) "\n\n"))

          (if eftp-queue-active
              (eftp-create-button "stop queue"
                                  (lambda () (interactive) (eftp-stop-queue)
                                    (eftp-refresh-downloads)))
            (eftp-create-button "start queue"
                                (lambda () (interactive) (eftp-start-queue)
                                  (eftp-refresh-downloads))))
          (insert " ")
          (eftp-create-button "flush queue"
                              (lambda () (interactive)
                                (when (y-or-n-p "Really flush the queue? ")
                                  (eftp-stop-queue)
                                  (maphash (lambda (url proc)
                                             (eftp-kill-download (car proc)))
                                           eftp-downloading-urls)
                                  (setq eftp-downloading-urls
                                        (make-hash-table :test 'equal))
                                  (setq eftp-waiting-queue '())
                                  (eftp-refresh-downloads))))
          (insert " ")
          (eftp-create-button "set maximum downloads"
                              (lambda () (interactive)
                                (let ((input (string-to-number
                                              (read-from-minibuffer
                                               "Maximum downloads?: "))))
                                  (when (> input 0)
                                    (setq eftp-max-downloads input))
                                  (eftp-download-next)
                                  (eftp-refresh-downloads))))
          (insert " ")
          (eftp-create-button "set download dir"
                              (lambda () (interactive)
                                (let ((input ))
                                  (setq eftp-download-dir
                                        (expand-file-name
                                         (read-directory-name "Download dir?: "
                                                              nil nil t)))
                                  (eftp-refresh-downloads))))
          (center-line)
          (insert "\n")
          (goto-char pt)
          (setq buffer-read-only t))))))

;; there must be a predefined way of doing this, but I can't see it. To top
;; things off, it's also a lazy hack..
(defun string-pad (s width)
  "Pad a string with spaces to the specified width, truncating the string if
  it will not fit"
  (format (concat "%-" (number-to-string width) "s")
          (reverse-string
           (format (concat "%." (number-to-string width) "s")
                   (reverse-string s)))))


(defun reverse-string (s)
  (coerce (reverse (loop for b across s
                         collect b))
          'string))

(defun eftp-create-button (label callback)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") callback)
    (let ((start (point)))
      (insert label)
      (make-button
       start (point)
       'keymap map
       'face '(:background "gray90"
                           :box (:line-width 1 :style released-button)))
      (set-text-properties start (point) '(read-only t)))))

(add-hook 'dired-mode-hook
          (lambda ()
            (let ((path (car (rassoc (current-buffer) dired-buffers))))
              (when (and path (string-match "^/ftp" path))
                (let ((map (copy-keymap (current-local-map))))
                  (define-key map (kbd "SPC")
                    (lambda ()
                      (interactive)
                      (eftp-ftp-queue-current)
                      (dired-next-line 1)))
                  (define-key map (kbd "?") 'eftp-display-downloads)
                  (use-local-map map))))))

(defun visible-buffers ()
  (let ((buffers '()))
    (walk-windows
     (lambda (w) (push (window-buffer w) buffers)))
    buffers))

(defun eftp-string-trim (s &rest chars)
  "Trim CHARS from the ends of S"
  (apply 'eftp-string-trim-right
         (apply 'eftp-string-trim-left s chars)
         chars))

(defun eftp-string-trim-left (s &rest chars)
  (let ((idx (dotimes (i (length s))
               (unless (member (elt s i) chars)
                 (return i)))))
    (if idx
        (subseq s idx)
      "")))

(defun eftp-string-trim-right (s &rest chars)
  (reverse-string (apply 'eftp-string-trim-left (reverse-string s) chars)))

(provide 'ftp-el)
