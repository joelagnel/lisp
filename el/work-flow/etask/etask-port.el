;;; etask-port.el --- part of etask

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum <rene (at) reneweichselbaum (dot) com>

;; $Id: etask-port.el,v 1.5 2004/06/21 22:17:17 rene Exp $

;; Keywords: calendar 

;; Human-Keywords: task management, bar chart, Gantt chart, project
;; management, todo list, personal information management

;; See `etask.el' to find out which software components you need to
;; run etask.

;; URL: http://www.reneweichselbaum.com/etask.html


;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


;;; Commentary:

;; This software component hides portability issues from etask.el.


;; _________________


;;; Code:


;;(when (featurep 'xemacs)
;;   (require 'overlay))

(if (featurep 'xemacs)
    (defalias 'etask-adjust-frame-height nil)
  (defalias 'etask-adjust-frame-height 'etask-adjust-frame-height-gnuemacs))

(cond ((fboundp 'propertize)
       (defalias 'etask-propertize 'propertize))
      ((fboundp 'ibuffer-propertize)
       (defalias 'etask-propertize 'ibuffer-propertize))
      (t
       (defun etask-propertize (string &rest properties)
         "Return a copy of STRING with text properties added.

First argument is the string to copy.  Remaining arguments form a
sequence of PROPERTY VALUE pairs for text properties to add to the
result."
         (let ((str (copy-sequence string)))
           (add-text-properties 0 (length str)
                                properties
                                str)
           str))))

(if (fboundp 'fit-window-to-buffer)
    (defalias 'etask-fit-window-port 'fit-window-to-buffer)
  (defun etask-fit-window-port (&optional window max-height min-height)
    (setq min-height (or min-height window-min-height))
    (setq max-height (or max-height (- (frame-height) (window-height) 1)))
    (let* ((window-min-height min-height)
           (windows (count-windows))
           (config (current-window-configuration)))
      (enlarge-window (- max-height (window-height)))
      (when (> windows (count-windows))
        (set-window-configuration config))
      (if (/= (point-min) (point-max))
          (shrink-window-if-larger-than-buffer window)
        (shrink-window (- (window-height) window-min-height))))))

(if (fboundp 'display-multi-frame-p)
    (defalias 'etask-multi-frame-p 'display-multi-frame-p)
  (defun etask-multi-frame-p()
    (featurep 'xemacs)))


;;; Initialization

(setq etask-port-loaded t)
(provide 'etask-port)


;;; etask-port.el  end of file