;;; cus-load.el --- automatically extracted custom dependencies
;;
;;; Code:

(put 'w3-menus 'custom-loads '("w3-cus" "w3-menu"))
(put 'w3-java 'custom-loads '("w3-java"))
(put 'w3-images 'custom-loads '("w3-cus" "w3-display"))
(put 'w3-hooks 'custom-loads '("w3-cus"))
(put 'w3-parsing 'custom-loads '("w3-cus"))
(put 'w3-advanced 'custom-loads '("w3-cus"))
(put 'ssl 'custom-loads '("ssl"))
(put 'w3 'custom-loads '("w3-cus" "w3-java"))
(put 'comm 'custom-loads '("ssl"))
(put 'hypermedia 'custom-loads '("w3-cus"))
(put 'socks 'custom-loads '("socks"))
(put 'faces 'custom-loads '("font"))
(put 'w3-files 'custom-loads '("w3-cus"))
(put 'w3-display 'custom-loads '("w3-cus" "w3-parse"))
(put 'processes 'custom-loads '("socks"))
;;; These are for handling :version.  We need to have a minimum of
;;; information so `custom-changed-variables' could do its job.  
;;; For both groups and variables we have to set `custom-version'.
;;; For variables we also set the `standard-value' and for groups
;;; `group-documentation' (which is shown in the customize buffer), so
;;; we don't have to load the file containing the group.

;;; `custom-versions-load-alist' is an alist that has as car a version
;;; number and as elts the files that have variables that contain that
;;; version. These files should be loaded before showing the
;;; customization buffer that `customize-changed-options' generates.


;;; This macro is used so we don't modify the information about
;;; variables and groups if it's already set. (We don't know when
;;; cus-load.el is going to be loaded and at that time some of the
;;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))


(defvar custom-versions-load-alist nil
 "For internal use by custom.")

(provide 'cus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; cus-load.el ends here
