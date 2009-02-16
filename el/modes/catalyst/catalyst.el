(load "de-vars")

(setq load-path (cons (concat root-path "modes/catalyst") load-path))

(require 'dbix-class)
(require 'model)
(require 'controller)
(require 'view)
(require 'catalyst-server)
(require 'config)
(require 'helper)
(require 'plugin)
(require 'debug)
(require 'chains)

(provide 'catalyst)