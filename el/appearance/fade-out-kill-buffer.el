;;; fade-out-kill-buffer.el -- fade to black when killing buffer
;; Ryan Yeske 20051013

(defun fade-out-kill-buffer (buffer)
  ;;need a way to find a way to disable confirmation
  (interactive "bKill buffer: ")
  (with-current-buffer buffer
    (let ((str (buffer-substring
		(progn (move-to-window-line 0)
		       (point))
		(progn (move-to-window-line -1)
		       (point-at-eol)))))
      (when (kill-buffer buffer)
	(with-temp-buffer
	  (insert str)
	  (switch-to-buffer (current-buffer))
	  (goto-char (point-min))
	  (setq cursor-type nil)
	  (dotimes (i 10)
	    (put-text-property (point-min) (point-max)
			       'face (list :foreground 
					   (format "gray%d"
                                                 (* 5  (1+ i))))) ; for white background
;;						   (- 100 (* 5 (1+ i))))))
	    (sit-for 0)
	    (sleep-for .01)))))))


(global-set-key (kbd "C-x k") 'fade-out-kill-buffer)



;; (defun switch-screen-fade ()
;;   ;;need a way to find a way to disable confirmation
;;   (interactive)
;;   (with-current-buffer buffer
;;     (let ((str (buffer-substring
;; 		(progn (move-to-window-line 0)
;; 		       (point))
;; 		(progn (move-to-window-line -1)
;; 		       (point-at-eol)))))
;;       (when (kill-buffer buffer)
;; 	(with-temp-buffer
;; 	  (insert str)
;; 	  (escreen-goto-next-screen)
;; 	  (goto-char (point-min))
;; 	  (setq cursor-type nil)
;; 	  (dotimes (i 20)
;; 	    (put-text-property (point-min) (point-max)
;; 			       'face (list :foreground 
;; 					   (format "gray%d"
;;                                                   ;(* 5  (1+ i))))) ; for white background
;; 						   (- 100 (* 5 (1+ i))))))
;; 	    (sit-for 0)
;; 	    (sleep-for .01)))))))

(provide 'fade-out-kill-buffer)