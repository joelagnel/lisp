
(load "de-vars")
(setq load-path (cons (concat root-path "modes/xml/nxml-mode") load-path))

(load "rng-auto.el")

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))