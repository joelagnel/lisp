(load "de-vars")
(setq load-path (cons (concat root-path "docu/trac") load-path))

(require 'xml-rpc)


(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)