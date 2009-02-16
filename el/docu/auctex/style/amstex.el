;;; amstex.el --- AMS-LaTeX support.

;;; This file is only needed when using AMS-LaTeX 1.1 and LaTeX 2.09.
;;; In later versions of LaTeX and AMS-LaTeX this file is never used,
;;; because there is no longer a class or package name amstex.
;;;
;;; As far as AUCTeX is concerned, the old amstex style is fairly
;;; similar to the new amsmath package. So we will just run that hook
;;; here.

;;; Code:

(TeX-add-style-hook "amstex"
 (function
  (lambda ()
    ;; Run the amsmath hook instead.
    (TeX-run-style-hooks "amsmath")
    )))

;;; amstex.el ends here.
