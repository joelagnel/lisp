;; -*- emacs-lisp -*-
;;; ret-while.el --- a while loop which returns the value of its last iteration

;; Author: Mark Triggs <mst@dishevelled.net>
;; $Id: ret-while.el,v 1.6 2003/10/23 12:10:41 mst Exp $
;; Keywords: lisp

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

;;; Code:

(defmacro ret-while (test &rest body)
  "A while loop who returns the value of the last iteration"
  (let ((exit (gensym))
        (ret (gensym)))
    `(let ((,exit nil)
           (,ret nil))
       (while (not ,exit)
         (if ,test
             (setq ,ret (progn ,@body))
           (setq ,exit t)))
       ,ret)))

;; use appropriate indenting and highlighting
(put 'ret-while 'lisp-indent-function 1)
(font-lock-add-keywords nil '(("ret-while" . font-lock-keyword-face)))

(provide 'ret-while)
;;; ret-while.el ends here
