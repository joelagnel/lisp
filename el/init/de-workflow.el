;;howm


(load "de-vars")
(setq load-path (cons (concat root-path "workflow/howm") load-path))


(add-to-list 'load-path "~/repository/lisp/el/work-flow/howm/")

(require 'howm)


(setq load-path (cons (concat root-path "work-flow/sidebrain") load-path))

(require 'sidebrain)


