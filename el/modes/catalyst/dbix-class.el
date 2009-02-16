
(defun dbic-generate-sql ()
  "Generate sql from defined schema"
  (interactive))


(defun dbic-generate-schema ()
  "Generate schema from sql"
  (interactive))


(defun  dbic-generate-schema-diagram ()
  "Generates schema.png"
  (interactive))

(defun dbic-generate-uml ()
  "Generate UML"
  (interactive))


(defun dbic-load-sql-file ()
  "Load given sql file. Take connection params from model"
  (interactive))


(defun dbic-list-columns-in-schema ()
  "List columns in given schema"
  (interactive))

(defun dbic-setup ()
  "Setup Model and schema. Will ask for static/dynamic loading. Helper!"
  (interactive))



;;components

(defun dbic-list-loaded-components ()
  "List loaded components"
  (interactive))

(defun dbic-list-components ()
  "List components"
  (interactive))

(defun dbic-list-inflated-columns ()
  (interactive))


;; relationships

(defun dbic-complete-related-columns ()
  (interactive))



(provide 'dbix-class)