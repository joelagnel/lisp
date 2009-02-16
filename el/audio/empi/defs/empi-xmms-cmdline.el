;;; EMPI-XMMS-CMDLINE.EL --- Backend to XMMS for EMPI using the command line

(require 'empi-proc)

(defvar empi-xmms-cmdline 'empi-proc-command)
(setplist 'empi-xmms-cmdline
'(:ihandle "xmms" :version ("-v") :plback ("-r") :play ("-p") :pause ("-u")
  :plnext ("-f") :guishow ("-m") :launch nil))

(provide 'empi-xmms-cmdline)

;;; EMPI-XMMS-CMDLINE.EL ends here
