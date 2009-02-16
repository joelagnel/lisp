(setq auto-mode-alist
      (append
       '(
	 ("\\.mail$" . mail-mode)
;	 ("\\.rm$" . rmail-mode)
	 ("\\.ol$" . outline-mode)
;	 ("/root\\.tex$" . outline-mode)	; Must precede .tex line
	 ("\\.rol$" . latex-mode)
	 ("\\.ivr$" . latex-mode)
	 ("\\.tex$" . latex-mode)
	 ("\\.s?html?$" . html-helper-mode)
	 )
       auto-mode-alist
       '(
	 ("/bin/[^\\.]+$" . sh-mode)
	 ("/cgi-bin/[^\\.]+$" . sh-mode)
	 )
       ))

(setq completion-ignored-extensions
      (append
       '(".txi" ".auxhold" ".tochold" ".glohold" ".idxhold" ".lofhold"
	 ".lothold" ".index" ".indexhold" ".fig" ".spell" ".fxi" ".ps" "-soln.tex" "-inclass.tex")
        completion-ignored-extensions))

(setq find-file-run-dired nil)

(setq find-file-existing-other-name t)
 ; should be irrelevant with next setting, but good in case that's undone.

(setq find-file-visit-truename t)

;; Following section and/or evdirabbrevs.el to be punted or updated for xenon.
;; (load "~rig/emacs/lisp/evdirabbrevs.el")
(setq directory-abbrev-alist
      (append
       '(
	 ("^~/public_html/" . "~/.www/")
	 ("^~/.www/home/" . "~/")
        )
;;        evdirabbrevs
nil
       ))

(provide 'filename-hackery)
