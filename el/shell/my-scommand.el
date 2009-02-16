;;
;; (A)synch shell command execution
(defun my-scommand (command &optional flag)
  "Runs shell command asynchrounous, or synchrounous if terminated
with \"&\""
  (interactive "sShell command: \nP")
  (if (string-match "&$" command)
      (scommand (substring command 0 -1) flag)
    (shell-command command flag)
))
(autoload 'scommand "scommand" "Enhanced shell-command" t)
(global-set-key "!" 'my-scommand)
