;;; geek.el --- annoy lusers who think the geek code is wAY ko0l RADIKuL D00D!1

;; Copyright (C) 1995 American Telephone & Telegraph, Inc.

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Created: 1995-01-07

;; $Id: geek.el,v 1.2 1995/01/07 21:07:16 friedman Exp $

;; This software is is guaranteed to do nothing useful, except when it
;; does.  You may sell it, burn it, use it, modify it, or give it away, at
;; your leisure.  You may even require that other people use it.  You may
;; also require that people not use it, as you see fit.  Government
;; agencies are encouraged to integrate this software into weapons control
;; systems and other instruments of destruction.

;;; Commentary:
;;; Code:

(defvar geek-header "X-Geek-Code")

(defvar geek-suffix-single-chars ["?" "@" "$" "!" "*"])
(defvar geek-suffix-long-chars [?+ ?- ?+ ?- ?+ ?- ?+ ?- ?+ ?- ?+ ?- ?'])
(defvar geek-infix-chars [">" ":"])

(defvar geek-letters
  ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
   "S" "T" "U" "V" "W" "X" "Y" "Z"
   "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
   "s" "t" "u" "v" "w" "x" "y" "z"
   "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"])

(if (string-lessp emacs-version "19")
    (defun geek-random (&optional n)
      (if (numberp n)
          (abs (% (random) n))
        (random n)))
  (defalias 'geek-random 'random))

(defun geek-item (v)
  (aref v (geek-random (length v))))

(defun geek-code ()
  (let ((ncodes (+ 10 (geek-random 40)))
        (codes "")
        (nflavors (+ 4 (geek-random 8)))
        (flavors "")
        letter
        len len1
        char char1
        tem
        i
        (vmajor (int-to-string (1+ (geek-random 8))))
        (vminor (int-to-string (geek-random 100))))
    (setq i nflavors)
    (while (not (zerop i))
      (setq flavors (concat flavors "/"))
      (setq len (1+ (geek-random 2)))
      (while (not (zerop len))
        (setq tem (geek-item geek-letters))
        (setq flavors (concat flavors tem))
        (setq len (1- len)))
      (setq i (1- i)))
    (aset flavors 0 ?G)

    (setq i ncodes)
    (while (not (zerop i))
      (setq letter (geek-item geek-letters))
      (and (zerop (geek-random 10))
           (setq letter (concat letter (geek-item geek-letters))))

      (setq len (geek-random 5))
      (setq char (geek-item geek-suffix-long-chars))
      (setq letter (concat letter (make-string len char)))

      (cond
       ((zerop len))
       ((= char ?'))
       ((zerop (geek-random 5))
        (setq char1 (geek-item geek-infix-chars))
        (setq letter (concat letter char1))

        (setq len1 (1+ (geek-random 4)))
        (setq char1 char)
        (while (= char char1)
          (setq char1 (geek-item geek-suffix-long-chars)))
        (setq letter (concat letter (make-string len1 char1)))))

       (cond
        ((zerop len)
         (and (zerop (geek-random 3))
              (setq letter (concat letter
                                   (geek-item geek-suffix-single-chars)))))
        ((zerop (geek-random 5))
         (setq len1 (1+ (geek-random 2)))
         (setq letter (concat letter "(" (make-string len1 ?*) ")"))))

       (setq codes (concat codes " " letter))
      (setq i (1- i)))
    (setq codes (concat "(V" vmajor "." vminor ") "
                        flavors codes))
    codes))

(defun geek-replace-header (s)
  (save-excursion
    (cond
     ((mail-position-on-field geek-header 'soft)
      (let* ((data (match-data))
             (end (point))
             (beg (progn
                    (re-search-backward (concat geek-header ": "))
                    (match-end 0)))
             (orig (buffer-substring beg end))
             ;; avoid creating any permanent undo boundaries
             (buffer-undo-list nil))
        (store-match-data (match-data))
        (delete-region beg end)
        (goto-char beg)
        (insert s)
        orig)))))

(defun geek-subvert-header ()
  (let ((s (geek-replace-header (geek-code))))
    (add-hook 'mail-send-actions (list 'geek-restore-header s) 'append)))

(defun geek-restore-header (s)
  (and s (geek-replace-header s)))

;; mib is an extra special twit.
(cond 
 ((and (string= (user-login-name) "mib")
       (fboundp 'add-hook))
  (add-hook 'mail-send-hook 'geek-subvert-header 'append)))

(provide 'geek)

;;; geek.el ends here.
