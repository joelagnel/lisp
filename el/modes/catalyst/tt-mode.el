
(require 'font-lock)

(defvar tt-mode-hook nil
  "List of functions to call when entering TT mode")

(defvar tt-keywords "\\bGET\\b\\|\\bCALL\\b\\|\\bSET\\b\\|\\bDEFAULT\\b\\|\\bINSERT\\b\\|\\bINCLUDE\\b\\|\\bBLOCK\\b\\|\\bEND\\b\\|\\bPROCESS\\b\\|\\bWRAPPER\\b\\|\\bIF\\b\\|\\bUNLESS\\b\\|\\bELSIF\\b\\|\\bELSE\\b\\|\\bSWITCH\\b\\|\\bCASE\\b\\|\\bFOREACH\\b\\|\\bWHILE\\b\\|\\bFILTER\\b\\|\\bUSE\\b\\|\\bMACRO\\b\\|\\bPERL\\b\\|\\bRAWPERL\\b\\|\\bTRY\\b\\|\\bTHROW\\b\\|\\bCATCH\\b\\|\\bFINAL\\b\\|\\bLAST\\b\\|\\bRETURN\\b\\|\\bSTOP\\b\\|\\bCLEAR\\b\\|\\bMETA\\b\\|\\bTAGS")

(defvar tt-font-lock-keywords 
   (list
    ;; Fontify [& ... &] expressions
    '("\\(\\[%[-+]?\\)\\(.+?\\)\\([-+]?%\\]\\)"  
      (1 font-lock-string-face t)
      (2 font-lock-variable-name-face t)
      (3 font-lock-string-face t))
    ;; Look for keywords within those expressions
    (list (concat
	   "\\[%[-+]? *\\("
	   tt-keywords 
	   "\\)") 
	  1 font-lock-keyword-face t)
    )
  "Expressions to font-lock in tt-mode.")

(defun tt-mode ()
  "Major mode for editing Template Toolkit files"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'tt-mode)
  (setq mode-name "TT")
  (if (string-match "Xemacs" emacs-version)
      (progn
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords tt-font-lock-keywords))
    ;; Emacs
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(tt-font-lock-keywords nil t))
    )
  (font-lock-mode)
  (setq indent t)
  (run-hooks tt-mode-hook))

(provide 'tt-mode)

;; tt-mode.el ends here