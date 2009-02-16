;;; -*- emacs-lisp -*-
;;; scrartcl.el -- AUC TeX style for scrartcl.cls

;; Copyright (C) 2002 Mark Trettin
;; License: GPL, see the file COPYING in the base directory of AUC TeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
;; Version: $Id: scrartcl.el,v 1.2 2002/10/08 13:24:33 dakas Exp $
;; Keywords: tex

;;; Commentary:

;; This file adds support for `scrartcl.cls'. This file needs
;; `scrbase.el'.

;; This file is intended to be used with the AUC TeX-Package by Per
;; Abrahamsen. Put this File into your TeX-style-path. You may also
;; byte-compile this file.

;;; Code:
(TeX-add-style-hook "scrartcl"
   (lambda ()
     (setq LaTeX-largest-level (LaTeX-section-level "section"))
     ;; load basic definitons
     (TeX-run-style-hooks "scrbase")))

;;; scrartcl.el ends here
