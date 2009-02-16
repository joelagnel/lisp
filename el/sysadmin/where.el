;;
;; $Id: where.el,v 1.2 2003/09/19 14:22:20 yamauchi Exp $
;; where-is-in
;;	Copyright (C) 2000 YAMAUCHI Hitoshi »³Æâ ÀÆ
;;
(defun where-is-in-function (command)
  "Search command (*.el) on load path and return find directory list."
  (let ((searchpath load-path) (findpath nil))
    (while searchpath
      (if (file-exists-p (concat (car searchpath) "/" command))
	  (progn 
	    (setq findpath (append findpath (list (car searchpath))))
	    (message (car searchpath))
	    (sit-for 0.5)))
      (setq searchpath (cdr searchpath)))
    findpath))

(defun where-is-in (command)
  "Where directory is the command in. 
This function shows all command founded directories."
  (interactive "sFilename:")
  (let ((ret (where-is-in-function command)))
    (if ret
	(message (format "find at %s" ret))
      (message (format "Can not find %s on load-path" command)))))

