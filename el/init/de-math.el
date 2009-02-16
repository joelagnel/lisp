(load "de-vars")
(setq load-path (cons (concat root-path "math/maxima") load-path))

(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))

(autoload 'maxima "maxima" "Running Maxima interactively" t)
(autoload 'maxima-mode "maxima" "Maxima editing mode" t)

(setq load-path (cons  "math/calc" load-path ))

(require 'calc)
(require 'calc-ext)

(setq load-path (cons  "math/gnuplot" load-path ))

(require 'gnuplot)

 (autoload 'imaxima "imaxima" "Image support for Maxima." t)