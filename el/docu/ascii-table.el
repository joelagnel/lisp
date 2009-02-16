;;; ascii-table.el --- Print an ASCII Table.

;; This file is NOT part of Emacs.

;; Copyright (C) 2004, 2005 Lawrence Mitchell <wence@gmx.li>
;; Filename: ascii-table.el
;; Version: $Revision: 1.7 $
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-01-15

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
;; Vital for any coder :)
;; The function `ascii-table' switches to a new buffer and then prints
;; the ascii-table showing Emacs' internal representations of the
;; files, and their respective decimal, octal and hexadecimal codes.


;;; History:
;;

;;; Code:
(require 'cl)
(defconst ascii-version
  "$Id: ascii-table.el,v 1.7 2004/02/27 21:30:21 wence Exp $"
  "ascii-table's version number.")

(defun ascii-pad-to-column (column string)
  "Pad to COLUMN with spaces, then insert STRING."
  (loop while (< (current-column) column)
        do (insert " ")
        finally (insert string)))

(defun ascii-table ()
  "Display an ASCII Table."
  (interactive)
  (switch-to-buffer (get-buffer-create "*ASCII Table*"))
  (erase-buffer)
  (insert " Cha | Dec | Oct  | Hex  || Cha | Dec | Oct  | Hex  || Cha | Dec | Oct  | Hex\n"
          "-----+-----+------+------++-----+-----+------+------++-----+-----+------+------\n")
  (loop for i from 0 to 42
        for j = (+ i 43)
        for k = (+ j 43)
        do
        (ascii-insert-char i)
        (ascii-insert-char j)
        (ascii-insert-char k)))

(defun ascii-insert-char (char)
  (if (= char 128)
      (insert "\n")
    (let ((col (cond ((< char 43)
                      5)
                     ((< char 86)
                      32)
                     (t
                      59))))
      (insert (format " %2s" (single-key-description char)))
      (ascii-pad-to-column col "|")
      (insert (format " %2d" char))
      (ascii-pad-to-column (setq col (+ col 6)) "|")
      (insert (format " %04o" char))
      (ascii-pad-to-column (setq col (+ col 7)) "|")
      (insert (format " 0x%02x" char))
      (if (> char 85)
          (insert "\n")
        (ascii-pad-to-column (setq col (+ col 7)) "||")))))

(provide 'ascii-table)

;;; ascii-table.el ends here
