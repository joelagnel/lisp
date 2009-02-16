;; -*- emacs-lisp -*-
;;; typist.el --- measure typing speed and accuracy

;; Author: Mark Triggs <mst@dishevelled.net>, Tim Raupach
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:

;; A friend and I hacked this up in half an hour for the fun of it.

;;; Code:

(defun tp-file-contents (filename)
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file filename)
        (buffer-string))
    nil))

(defun tp-wrap-string (s)
  (with-temp-buffer
    (insert s)
    (fill-region (point-min)
                 (point-max))
    (let ((s (buffer-string)))
      (subseq s 0 (1- (length s))))))


(defun tp-string-wordcount (s)
  (1+ (count (string-to-char " ") (tp-wrap-string s))))

(defun play-typist ()
  (interactive)
  (labels ((get-data ()
                     (let ((text
                            (tp-file-contents (read-file-name
                                               "Filename to use: "))))
                       (or text (get-data)))))
    (let ((errors 0)
          (text (get-data)))
      (save-excursion
        (select-window (split-window-vertically))
        (switch-to-buffer "*Typist*")
        (toggle-input-method t)
        (mapc (lambda (line)
                (insert (format ">%s\n\n\n" line)))
              (split-string (tp-wrap-string text) "\n"))

        (goto-char (point-min))
        (next-line 1)
        (insert " ")

        (let* ((start-time nil))
          (map nil
               #'(lambda (c)
                   (let ((input (read-char nil t)))
                     (unless start-time
                       (setq start-time (float-time)))
                     (cond
                      ((= c (string-to-char "\n"))
                       (beginning-of-line)
                       (next-line 3)
                       (insert " "))
                      ((= c input) (insert (string input)))
                      (t
                       (incf errors)
                       (while (not (eql (setq input (read-char nil t)) c))
                         (incf errors))
                       (insert (string input))))))
               (tp-wrap-string text))


          (let ((end-time (float-time)))
            (flet ((yes-or-no-p (&rest ignored) t))
              (kill-buffer-and-window))
            (message "Speed: %.0f WPM. Accuracy: %d%%\n"
                     (* (/ (tp-string-wordcount text)
                           (- end-time start-time))
                        60)
                     (let ((result (- 100
                                      (* (/ errors
                                            (float (length text)))
                                         100))))
                       (if (< result 0)
                           0
                         result)))))))))

(provide 'typist)
;;; typist.el ends here
