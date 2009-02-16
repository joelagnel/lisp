;;;
;;; rhtml-dirty-font.el - more liberal fontification support for `rhtml-mode'
;;;

;; ***** BEGIN LICENSE BLOCK *****
;; Version: MPL 1.1/GPL 2.0/LGPL 2.1

;; The contents of this file are subject to the Mozilla Public License Version 
;; 1.1 (the "License"); you may not use this file except in compliance with 
;; the License. You may obtain a copy of the License at 
;; http://www.mozilla.org/MPL/

;; Software distributed under the License is distributed on an "AS IS" basis,
;; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;; for the specific language governing rights and limitations under the
;; License.

;; The Original Code is Dirty Fontification Support for RHTML-MODE.

;; The Initial Developer of the Original Code is
;; Paul Nathan Stickney <pstickne@gmail.com>.
;; Portions created by the Initial Developer are Copyright (C) 2006
;; the Initial Developer. All Rights Reserved.

;; Contributor(s):

;; Alternatively, the contents of this file may be used under the terms of
;; either the GNU General Public License Version 2 or later (the "GPL"), or
;; the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
;; in which case the provisions of the GPL or the LGPL are applicable instead
;; of those above. If you wish to allow use of your version of this file only
;; under the terms of either the GPL or the LGPL, and not to allow others to
;; use your version of this file under the terms of the MPL, indicate your
;; decision by deleting the provisions above and replace them with the notice
;; and other provisions required by the GPL or the LGPL. If you do not delete
;; the provisions above, a recipient may use your version of this file under
;; the terms of any one of the MPL, the GPL or the LGPL.

;; ***** END LICENSE BLOCK *****


;; Provides more liberal fontification support for cases when it is
;; suspected the colorizing may be invalid.
;; THIS SHOULD PROBABLY BE REPLACE WITH INTERNAL FONT-LOCK SUPPORT/JIT.


(require 'rhtml-erb)

(defun rhtml-activate-dirty-font ()
  "Activate dirty-font support."
  (make-local-variable 'rhtml-dirty-delete)
  (make-local-hook 'before-change-functions)
  (add-hook 'before-change-functions 'rhtml-before-change t t)
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'rhtml-after-change t t))

(defmacro rhtml-expanded-region-has-erb-tag-p (start end)
  "Like `rhtml-region-as-erb-tag-p' but will expand the region to
cover the width of the delimeters which is needed to catch some
cases."
  `(rhtml-region-has-erb-tag-p (- ,start rhtml-erb-open-delim-len)
                               (+ ,end rhtml-erb-close-delim-len)))

(defun rhtml-before-change (start end)
  "Before change hook function.
Marks a fontification re-scan if any ERB tags might be removed."
  (save-match-data
    (setq rhtml-dirty-delete
          (rhtml-expanded-region-has-erb-tag-p start end))))

(defun rhtml-after-change (start end old-len)
  "After change hook function.
Causes a fontification re-scan if ERB tags were added or a
re-scan was request."
  (save-match-data
    (when (or rhtml-dirty-delete
              (rhtml-expanded-region-has-erb-tag-p start end))
      (font-lock-fontify-buffer)
      (setq rhtml-dirty-delete nil))))

(defvar rhtml-dirty-delete nil
  "Non-nil indicates a large part of the buffer needs to be updated.
This will generally be handled be performing a complete
refresh.")


;;
(provide 'rhtml-dirty-font)