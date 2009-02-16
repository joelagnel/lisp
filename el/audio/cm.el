;; Common Music

(defvar cm-directory "e:/lisp/apps/cm")

(defvar cm-startfile "cm.bat")

(let* ((els (concat cm-directory "/etc/xemacs"))
       (bin (concat cm-directory "/bin/"
                    cm-startfile))
       (load-path (cons els load-path)))
  (load "listener")
  (load "cm")
  (setq inferior-lisp-program bin))