;;; banner.el -- make banners from strings
;;; Old-time-stamp: <95/09/25 10:25:06 john>
;;; Time-stamp: <2006-05-05 17:20:05 john>
;;; Written by john@harlqn.co.uk, 6th October 1993

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;
;  \ ^   /\    ^   |  ^   |   /\   +-\      /\   |
;   Y )    |   |\  |  |\  |  |  '  |  \    |  '  |
;   +<    -+   | \ |  | \ |  +-    |  /    +-    |
;   | )  ( |   |  \|  |  \|  |  '  +-<     |  '  |
;   \_/   V \  |   Y  |   Y   \/   |  \  *  \/   +--
;

(provide 'banner)

(makunbound 'uncial-banner-font-heavy)
(makunbound 'uncial-banner-font-light)

(defvar uncial-banner-font-heavy
  '((?a
     "      "
     "      "
     " ##   "
     "   #  "
     "  ##  "
     " # #  "
     "  # # "
     )
    (?b
     "      "
     "      "
     " # #  "
     "  # # "
     "  ##  "
     "  # # "
     "  ### "
     )
    (?c
     "      "
     "      "
     "  ##  "
     " #  # "
     " #    "
     " #  # "
     "  ##  "
     )
    (?d
     "     "
     "     "
     "  #  "
     "   # "
     "  #  "
     " # # "
     "  #  "
     )
    (?e
     "      "
     "      "
     "  ##  "
     " #  # "
     " ##   "
     " #  # "
     "  ##  "
     )
    (?f
     "      "
     "      "
     "  ##  "
     " #  # "
     " ##   "
     " #    "
     " #    "
     )
    (?g
     "       "
     "       "
     "  ##   "
     " #     "
     " # ### "
     "  ## # "
     "    ## "
     )
    (?h
     "      "
     "      "
     " # #  "
     " ## # "
     " #  # "
     " #  # "
     " # #  "
     )
    (?i
     "   "
     "   "
     " # "
     " # "
     " # "
     " # "
     " # "
     )
    (?j
     "     "
     "     "
     "   # "
     "   # "
     "   # "
     " # # "
     "  #  "
     )
    (?K
     "      "
     " #    "
     " #  # "
     " # #  "
     " ##   "
     " # #  "
     " #  # "
     )
    (?k
     "      "
     "      "
     " #    "
     " #  # "
     " # #  "
     " ##   "
     " # #  "
     )
    (?l
     "     "
     "     "
     " #   "
     " #   "
     " #   "
     " #   "
     " ### "
     )
    (?m
     "       "
     "       "
     "  # #  "
     " # # # "
     " # # # "
     " # # # "
     " # # # "
     )
    (?n
     "       "
     "       "
     " #   # "
     " ##  # "
     " # # # "
     " #  ## "
     " #   # "
     )
    (?o
     "       "
     "       "
     "   ##  "
     "  #  # "
     " #   # "
     " #  #  "
     "  ##   "
     )
    (?P
     "      "
     " ###  "
     " #  # "
     " #  # "
     " ###  "
     " #    "
     " #    "
     )
    (?p
     "      "
     "      "
     " ###  "
     " #  # "
     " #  # "
     " ###  "
     " #    "
     )
    (?q
     "       "
     "       "
     "  ##   "
     " #  #  "
     " # ##  "
     "  ###  "
     "     # "
     )
    (?R
     "      "
     " ###  "
     " #  # "
     " #  # "
     " ###  "
     " # #  "
     " #  # "
     )
    (?r
     "      "
     "      "
     " ###  "
     " #  # "
     " #  # "
     " ###  "
     " # #  "
     )
    (?s
     "     "
     "     "
     "  ## "
     " #   "
     "  #  "
     "   # "
     " ##  "
     )
    (?t
     "     "
     "     "
     " ### "
     "  #  "
     " #   "
     " #   "
     "  #  "
     )
    (?u
     "       "
     "       "
     "  # #  "
     " #  #  "
     " #  #  "
     " #  #  "
     "  ## # "
     )
    (?v
     "       "
     "       "
     " #   # "
     " #   # "
     " #   # "
     "  # #  "
     "   #   "
     )
    (?w
     "       "
     "       "
     " # # # "
     " # # # "
     " # # # "
     " # # # "
     "  ###  "
     )
    (?x
     "     "
     "     "
     " # # "
     " # # "
     "  #  "
     " # # "
     " # # "
     )
    (?y
     "       "
     "       "
     " #   # "
     "  # #  "
     "   #   "
     "   #   "
     "   #   "
     )
    (?z
     "       "
     "       "
     " ##### "
     "    #  "
     " ##### "
     "  #    "
     " ##### "
     )
    (? 
     "     "
     "     "
     "     "
     "     "
     "     "
     )
    (?/
     "       "
     "       "
     "     # "
     "    #  "
     "   #   "
     "  #    "
     " #     "
     )
    (?-
     "     "
     "     "
     "     "
     "     "
     " === "
     "     "
     "     "
     )
    (?!
     "     "
     "  #  "
     "  #  "
     "  #  "
     "  #  "
     "     "
     "  *  "
     )
    (?.
     "     "
     "     "
     "     "
     "     "
     "     "
     "     "
     " #   "
     ))
  "Alist matching characters of banner input to rectangles of banner output.")

(defvar uncial-banner-font-light
  '((?a
     "      "
     "      "
     " /\\   "
     "   |  "
     "  -+  "
     " ( |  "
     "  V \\ "
     )
    (?b
     "      "
     "      "
     " \\ ^  "
     "  Y ) "
     "  +<  "
     "  | ) "
     "  \\_/ "
     )
    (?c
     "      "
     "      "
     "  /\\  "
     " /  ' "
     " |    "
     " \\  ` "
     "  \\/  "
     )
    (?d
     "     "
     "     "
     "  \\  "
     "   ) "
     "  ^  "
     " ( ) "
     "  V  "
     )
    (?e
     "      "
     "      "
     "  /\\  "
     " |  ' "
     " +-   "
     " |  ' "
     "  \\/  "
     )
    (?f
     "      "
     "      "
     "  /\\  "
     " /  ' "
     " L_   "
     " |    "
     " |    "
     )
    (?g
     "       "
     "       "
     "  /~\\  "
     " /     "
     " \\ ___ "
     "  V  | "
     "   ~~+ "
     )
    (?h
     "       "
     "       "
     " \\  ^  "
     "  Y  \\ "
     "  |  | "
     "  |  | "
     "  |  / "
     )
    (?i
     "   "
     "   "
     " | "
     " | "
     " | "
     " | "
     " | "
     )
    (?j
     "     "
     "     "
     "   | "
     "   | "
     "   | "
     " \\ / "
     "  V  "
     )
    (?K
     "      "
     " |    "
     " |  / "
     " | /  "
     " +<   "
     " | \\  "
     " |  \\ "
     )
    (?k
     "      "
     "      "
     " |    "
     " |  / "
     " | /  "
     " +<   "
     " | \\  "
     )
    (?l
     "     "
     "     "
     " |   "
     " |   "
     " |   "
     " |   "
     " +-- "
     )
    (?m
     "       "
     "       "
     "  ^ ^  "
     " / Y \\ "
     " [ | ] "
     " [ | ] "
     " \\ | / "
     )
    (?n
     "       "
     "       "
     " ^   | "
     " |\\  | "
     " | \\ | "
     " |  \\| "
     " |   Y "
     )
    (?o
     "       "
     "       "
     "   _^  "
     "  /  ) "
     " /   / "
     " (  /  "
     "  V-   "
     )
    (?P
     "      "
     " /-\\  "
     " |  \\ "
     " |  / "
     " L_/  "
     " |    "
     " |    "
     )
    (?p
     "      "
     "      "
     " /-\\  "
     " |  \\ "
     " |  / "
     " L_/  "
     " |    "
     )
    (?q
     "       "
     "       "
     "  /-\\  "
     " |   \\ "
     " | \\ / "
     "  \\_X  "
     "     \\ "
     )
    (?R
     "      "
     " +-\\  "
     " |  \\ "
     " |  / "
     " +-<  "
     " | \\  "
     " |  \\ "
     )
    (?r
     "      "
     "      "
     " +-\\  "
     " |  \\ "
     " |  / "
     " +-<  "
     " |  \\ "
     )
    (?s
     "     "
     "     "
     "  ~\\ "
     " (   "
     "  \\  "
     "   ) "
     " \\~  "
     )
    (?t
     "     "
     "     "
     " -+- "
     "  /  "
     " /   "
     " \\   "
     "  \\  "
     )
    (?u
     "       "
     "       "
     "  /  / "
     " /  |  "
     " |  |  "
     " \\  ^  "
     "  \\/ \\ "
     )
    (?v
     "       "
     "       "
     " |   | "
     " |   | "
     " \\   / "
     "  \\ /  "
     "   V   "
     )
    (?w
     "       "
     "       "
     " | | | "
     " | | | "
     " | | | "
     " \\ ^ / "
     "  V V  "
     )
    (?x
     "     "
     "     "
     " | | "
     " \\ / "
     "  X  "
     " / \\ "
     " | | "
     )
    (?y
     "       "
     "       "
     " \\   / "
     "  \\ /  "
     "   Y   "
     "   |   "
     "   |   "
     )
    (?z
     "       "
     "       "
     " ----> "
     "    /  "
     " __X-- "
     "  /    "
     " <____ "
     )
    (? 
     "     "
     "     "
     "     "
     "     "
     "     "
     "     "
     "     "
     )
    (?/
     "       "
     "       "
     "     / "
     "    /  "
     "   /   "
     "  /    "
     " /     "
     )
    (?!
     "     "
     "  !  "
     "  !  "
     "  !  "
     "  !  "
     "  !  "
     "  .  "
     )
    (?-
     "     "
     "     "
     "     "
     " ___ "
     "     "
     "     "
     "     "
     )
    (?.
     "     "
     "     "
     "     "
     "     "
     "     "
     "     "
     " *   "
     ))
  "Alist matching characters of banner input to rectangles of banner output.")

(defvar uncial-banner-block
  '((?A
     "  #   "
     " # #  "
     "#   # "
     "#   # "
     "##### "
     "#   # "
     "#   # ")
    (?B
     )
    "Alist matching characters of banner input to rectangles of banner output."))

(defvar uncial-gothic
  '((?m
     "      "
     "      "
     "\\/\\/\\ "
     "| | | "
     "| | | "
     "      "
     "      "
     )
    (?n
     "    "
     "    "
     "\\/\\ "
     "| | "
     "| | "
     "    "
     "    "
     )
    (?i
     "  "
     "  "
     "\\ "
     "| "
     "| "
     "  "
     "  "
     )
    (?u
     "    "
     "    "
     "| | "
     "| | "
     "\\/\\ "
     "    "
     "    "
     ))
  "Alist matching characters of banner input to rectangles of banner output.")

(defvar uncial-banner-font nil
  "An uncial banner font; either light or heavy might be here.")

(setq uncial-banner-font uncial-banner-font-heavy)
(setq uncial-banner-font uncial-banner-font-light)

(defvar banner-font-alist (list (cons "Uncial heavy" uncial-banner-font-heavy)
				(cons "Uncial light" uncial-banner-font-light)
				;; (cons "Gothic light" uncial-gothic)
				)
  "aList of banner fonts.")

(defun banner-findfont (fontname)
  "Return the banner font called FONTNAME."
  (cdr (assoc fontname banner-font-alist)))

;;;###autoload
(defun banner-string (str font)
  "Make an banner from STR using FONT."
  (interactive
   (let* ((string (read-from-minibuffer "Banner string: "))
	  (completion-ignore-case t)
	  (fontname (completing-read "Banner font: "
				     banner-font-alist
				     nil t nil)))
     (list string fontname)))
  (setq font (banner-findfont font))
  (switch-to-buffer-other-window "*Banner*")
  (erase-buffer)
  (let ((i 0)
	(l (length str)))
    (while (< i l)
      (let* ((letter (aref str i))
	     (expansion (or (assoc letter font)
			    ;; try the other cases, in case of
			    ;; monocase fonts (either way)
			    (assoc (downcase letter) font)
			    (assoc (upcase letter) font))))
	(if expansion
	    (progn
	      (goto-char (point-min))
	      (end-of-line 1)
	      (insert-rectangle (cdr expansion)))))
      (setq i (1+ i))))
  (buffer-string))

;;;###autoload
(defun insert-banner-string (str font)
  "Insert a banner of STR using FONT."
  (interactive
   (let* ((string (read-from-minibuffer "Banner string: "))
	  (completion-ignore-case t)
	  (fontname (completing-read "Banner font: "
				     banner-font-alist
				     nil t nil)))
     (list string fontname)))
  (insert
   (save-window-excursion
     (banner-string str font))))

;;;###autoload
(defun banner-message (msg font)
  "Display MSG as a banner in FONT, with caching."
 (let* ((msg-buffer-name (concat " *" font "-banner-" msg))
	(msg-buffer (get-buffer msg-buffer-name)))
   (if (null msg-buffer)
       (progn
	(setq msg-buffer (get-buffer-create msg-buffer-name))
	(set-buffer msg-buffer)
	(insert-banner-string msg font)))
   (switch-to-buffer msg-buffer)))

;;; end of banner.el
