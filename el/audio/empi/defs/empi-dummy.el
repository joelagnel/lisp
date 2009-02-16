;;; EMPI-DUMMY.EL --- Dummy backend for EMPI

(require 'empi-elisp)

(defvar empi-dummy 'empi-elisp-command)
(defvar dummy-balance 0)
(defvar dummy-volume 100)

(setplist 'empi-dummy
 `(:qbalance (lambda () dummy-balance)
   :balance (lambda (val) (setq dummy-balance val))
   :qvolume (lambda () dummy-volume)
   :volume (lambda (val) (and (> val 0) (setq dummy-volume val)))))

(provide 'empi-dummy)

;;; EMPI-DUMMY.EL ends here
