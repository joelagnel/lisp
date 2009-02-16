;;; Need to resolve where to put these.

(define-keys empi-map
  [?p]			'empi-player-gui-prefs
  [?l]			'empi-player-launch
  [f4]			'empi-player-exit)

(defun empi-player-gui-prefs ()
  (interactive)
  (empi-simple-action :gui-prefs))

(defun empi-player-exit ()
  (interactive)
  (empi-simple-action :exit))

(defun empi-player-launch ()
  (interactive)
  (empi-simple-action :launch))
