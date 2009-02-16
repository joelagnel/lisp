(if (not (boundp 'inferior-xlispstat-program))
    (setq-default inferior-xlispstat-program "xlispstat"))

(defun xlisp-quit-sentinel (proc reason)
  (if (and (not (memq reason '(run stop))))
      (save-buffers-kill-emacs)))

(defun run-xlispstat-exit () 
  (run-xlispstat)
  (let ((process (get-process "inferior-lisp")))
    (set-process-sentinel process 'xlisp-quit-sentinel)
    (process-kill-without-query process)))

(defun run-xlispstat ()
  "Run an inferior xlispstat process."
  (interactive)
  (run-lisp inferior-xlispstat-program))

