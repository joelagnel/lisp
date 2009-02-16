;;;  erc-show.el --- Show the results of evaluating lisp expressions in ERC.

;; This file is NOT part of Emacs.

;; Copyright (C) 2003,2005,2005 Lawrence Mitchell <wence@gmx.li>
;; Filename: erc-show.el
;; Version: 1.1
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2003-09-22
;; URL: http://purl.org/NET/wence/erc-show.el
;; Keywords: chat ERC

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This defines a /show command for ERC, the Emacs IRC Client.  It
;; allows you to type a Lisp form into the ERC prompt.  The form, and
;; the result of its evaluation are then sent to the current channel:
;; For example:

;; >>> /show (let ((x 'a) (y 'b)) (list x y))
;; <lawrence> (let ((x 'a) (y 'b)) (list x y)) => (a b)

;;; Installation:
;; Put this file somewhere in your `load-path', then put
;; (require 'erc-show) in your ~/.emacs or your ERC startup file.

;;; Code:

(defun erc-cmd-SHOW (&rest form)
  "Eval FORM and send the result and the original form as:

FORM => (eval FORM)."
  (let ((string 
         (with-temp-buffer
           (mapc #'(lambda (f) (insert f " ")) form)
           (goto-char (point-min))
           (setq form (read (current-buffer)))
           (let ((res (condition-case err
                          (eval form)
                        (error
                         (format "Error: %s" err)))))
             (insert (format " => %s" res)))
           (buffer-substring-no-properties
            (point-min) (1- (point-max))))))
    (erc-send-message string)))

(add-to-list 'erc-noncommands-list 'erc-cmd-SHOW)

(provide 'erc-show)

;;; erc-show.el ends here

