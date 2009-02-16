
(add-hook 'erlang-mode-hook 'esense-start-function)

(defun esense-start-function ()
  "Hook ESense functionality to Erlang mode."
  (require 'esense)
  (esense-mode 1))

(autoload 'esense-trace-mode "esense-trace" "Autoload for ESense trace mode." t)
(autoload 'esense-trace-minor-mode "esense-trace" "Autoload for ESense trace minor mode." t)

(provide 'esense-start)
