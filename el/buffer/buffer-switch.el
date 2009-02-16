;; Window shifting. C-x-o lets us go forward a window (or several). This
;; one lets us go back one or more windows. From Glickstein.
(defun other-window-backward (&optional n)
  "Select previous Nth window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; now bind it to C-x p
(global-set-key "\C-x\p" 'other-window-backward)
;; end window shifting.