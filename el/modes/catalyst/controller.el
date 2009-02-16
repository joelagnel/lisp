;;actions

(defun catalyst-complete-action ()
  "Complete action name"
  (interactive))


(defun catalyst-complete-action-attribute ()
  "complete action attributes"
  (interactive))


(defun catalyst-list-actions ()
  "list actions which are browseable. 
    Should be of the format controller/action--path-attribute-referredby
    list actions from current controller(default) or specify."
  (interactive))


(defun catalyst-jump-to-action  ()
  "Jump to a catalyst action. this should take URL actions!"
  (interactive))


(defun catalyst-run-current-action ()
  "Test if view file is present and run current action in browser
    with correct port"
  (interactive))



;;controllers

(defun catalyst-complete-controller ()
  "Complete action name"
  (interactive))


(defun catalyst-list-controllers ()
  "list actions which are browseable. 
    Should be of the format controller/action--path-attribute-referredby"
  (interactive))


(defun catalyst-jump-to-controller  ()
  "Jump to a catalyst action"
  (interactive))


(defun catalyst-run-current-controller ()
  "run controller/index"
  (interactive))


;;methods $c->method

(defun catalyst-complete-method ()
  "Somehow identify methods from POD/source"
  (interactive))



;;variables

(defun catalyst-stash-variable ()
  "insert text to stash a variable"
  (interactive))




(provide 'controller)

  
   
  