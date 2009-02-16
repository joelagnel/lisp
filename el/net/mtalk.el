(defun mtalk (address)
  (interactive "sTalk to (user@host:n.n): ")
  (save-match-data
    (let* ((template "*%s@%s*")
           (lname (user-login-name))
           (lhost (system-name))
           lbuf rbuf rname rhost rdisp
           (cframe (selected-frame))
           nframe)

      (or (string-match "\\([^@]+\\)@\\([^:]+\\):\\(.*\\)" address)
          (error "mtalk: invalid address format \"%s\"" address))
      (setq rname (substring address (match-beginning 1) (match-end 1)))
      (setq rhost (substring address (match-beginning 2) (match-end 2)))
      (setq rdisp (substring address (match-beginning 3) (match-end 3)))

      (setq lbuf (get-buffer-create (format template lname lhost)))
      (setq rbuf (get-buffer-create (format template rname rhost)))

      (delete-other-windows)
      (switch-to-buffer rbuf)
      (erase-buffer)
      (split-window)
      (switch-to-buffer lbuf)
      (erase-buffer)

      (setq nframe (make-frame-on-display 
                    (concat rhost ":" rdisp)
                    (list (cons 'name (format "talk %s@%s" lname lhost)))))
      (unwind-protect
          (progn
            (select-frame nframe)
            (delete-other-windows)
            (switch-to-buffer lbuf)
            (erase-buffer)
            (split-window)
            (switch-to-buffer rbuf)
            (erase-buffer))
        (select-frame cframe)))))
