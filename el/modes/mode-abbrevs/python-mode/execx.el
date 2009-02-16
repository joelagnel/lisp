(progn
  (save-excursion
    (goto-char (point-min))
    (when (not (re-search-forward "^import.*\\<commands\\>" nil t))
      (forward-line 1)
      (insert "import commands\n")))
  (insert "commands.getoutput(\"\")")
  (backward-char 2))
