;;; $Id: animals.el,v 1.1 2000/12/31 04:33:15 sds Exp $
;;; $Source: /usr/local/cvs/sds/lisp/animals.el,v $

(setq animals-debug-output t)
(setq animals-file-name (or (getenv "ANIMALS")
			    (concat (getenv "HOME") "/.animals")))
(setq animals-data '("Is it an insect" ("Can it sting" "an ant" . "a bee")
		     "Can it fly" "a duck" . "a penguin"))
;; (setq animals-data (read (find-file-noselect animals-file-name)))

(defun anml-ask-q (root)
  (y-or-n-p (concat (car root) "? ")))

(defun anml-finish (tail)
  (if (y-or-n-p (concat "Is it a " tail "? "))
      (message "I won!") (message "I lost...")))

(setq anml-root animals-data)

(while (consp anml-root)
  (setq anml-root (if (anml-ask-q anml-root)
		      (car (cdr anml-root)) (cdr (cdr anml-root)))))
(anml-finish anml-root)

(defun read-from-file (fl)
  (let ((buf (get-buffer-create " *read-from-file*")))
    (prog1
        (save-excursion
          (set-buffer buf)
          (insert-file-contents-literally fl nil nil nil t)
          (goto-char 0) (read buf))
      (kill-buffer buf))))

(defun comp (z0 z1)
  (cond ((and (consp z0) (consp z1))
         (unless (equalp (car z0) (car z1))
           (insert (format "0: %S\n1: %S\n\n" (car z0) (car z1))))
         (comp (cadr z0) (cadr z1))
         (comp (cddr z0) (cddr z1)))
        ((equalp z0 z1))
        (t (insert (format "0: %S\n1: %S\n\n" z0 z1)))))

(defun add-questions (z0)
  (when (consp z0)
    (setf (car z0) (concat (car z0) "?"))
    (add-questions (cadr z0))
    (add-questions (cddr z0))))

(defun write-to-file (obj fl)
  (let ((buf (find-file-noselect fl t t)))
    (save-excursion
      (set-buffer buf) (buffer-disable-undo buf) (erase-buffer)
      (insert (format "%S" obj)) (save-buffer))
    (kill-buffer buf)))


(setq z0 (read-from-file "/home/sds/.animals"))
(setq z1 (read-from-file "/home/sds/lisp/animals"))
(comp z0 z1)
(add-questions z0)
(write-to-file z0 "/home/sds/lisp/animals")
