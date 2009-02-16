(defun find-region-translation (start end)
  "Find the region in the EDICT file."
  (interactive "r")
  (let ((str (buffer-substring-no-properties start end))
	(conf (current-window-configuration)))
    (if (= (length str) 1)
	(find-file  "/home/alex/elisp/kdic/kanjidic")
      (find-file "/usr/share/edict/edict"))
    (goto-char (point-min))
    (when (occur str)
      (set-window-configuration conf)
      (switch-to-buffer "*Occur*")
      (local-set-key (kbd "q") 'bury-buffer))))

(global-set-key (kbd "C-c j") 'find-region-translation)

(add-to-list 'auto-coding-alist '("edict\\'" . euc-jp))
(add-to-list 'auto-coding-alist '("kanjidic\\'" . euc-jp))

;;; KanjiDictionary

(autoload 'kdic "kdic" "Practice some kanji or vocab." t)
(setq kdic-cache "/home/alex/kdic.cache"
      kdic-filter '(1 2)
      kdic-missed-ratio 80
      kdic-encoding 'iso-2022-jp
      kdic-dictionary "/home/alex/elisp/kdic/kanjidic")

(add-to-list 'file-coding-system-alist
	     (cons "kdic" 'iso-2022-jp))

(defun my-kdic-correct-answer ()
  "Get related information from the kanjidic file."
  (let ((buf (get-file-buffer kdic-dictionary))
	(coding-system-for-read kdic-encoding)
	(str (kdic-key kdic-correct-answer))
	(inhibit-read-only t))
    (get-other-frame); make sure there is at least one
    (other-frame 1)
    (if buf
	(set-buffer buf)
      (setq buf (find-file kdic-dictionary)))
    (occur str)
    (set-buffer (get-buffer "*Occur*"))
    (while (re-search-forward "\\<IN\\([0-9]+\\)" nil t)
      (put-text-property (match-beginning 1)
			 (match-end 1)
			 'face
			 'bold))
    (goto-char (point-min))
    (other-frame -1)))

(add-hook 'kdic-correct-hook 'my-kdic-correct-answer)

;;; KanjiDictionary

(autoload 'kdic "kdic" "Practice some kanji or vocab." t)
(setq kdic-cache "/home/alex/kdic.cache"
      kdic-filter '(1 2)
      kdic-missed-ratio 80
      kdic-encoding 'iso-2022-jp
      kdic-dictionary "/home/alex/elisp/kdic/kanjidic")

(add-to-list 'file-coding-system-alist
	     (cons "kdic" 'iso-2022-jp))

(defun my-kdic-correct-answer ()
  "Get related information from the kanjidic file."
  (let ((buf (get-file-buffer kdic-dictionary))
	(coding-system-for-read kdic-encoding)
	(str (kdic-key kdic-correct-answer))
	(inhibit-read-only t))
    (get-other-frame); make sure there is at least one
    (other-frame 1)
    (if buf
	(set-buffer buf)
      (setq buf (find-file kdic-dictionary)))
    (occur str)
    (set-buffer (get-buffer "*Occur*"))
    (while (re-search-forward "\\<IN\\([0-9]+\\)" nil t)
      (put-text-property (match-beginning 1)
			 (match-end 1)
			 'face
			 'bold))
    (goto-char (point-min))
    (other-frame -1)))

(add-hook 'kdic-correct-hook 'my-kdic-correct-answer)

;;; KanjiDictionary

(autoload 'kdic "kdic" "Practice some kanji or vocab." t)
(setq kdic-cache "/home/alex/kdic.cache"
      kdic-filter '(1 2)
      kdic-missed-ratio 80
      kdic-encoding 'iso-2022-jp
      kdic-dictionary "/home/alex/elisp/kdic/kanjidic")

(add-to-list 'file-coding-system-alist
	     (cons "kdic" 'iso-2022-jp))

(defun my-kdic-correct-answer ()
  "Get related information from the kanjidic file."
  (let ((buf (get-file-buffer kdic-dictionary))
	(coding-system-for-read kdic-encoding)
	(str (kdic-key kdic-correct-answer))
	(inhibit-read-only t))
    (get-other-frame); make sure there is at least one
    (other-frame 1)
    (if buf
	(set-buffer buf)
      (setq buf (find-file kdic-dictionary)))
    (occur str)
    (set-buffer (get-buffer "*Occur*"))
    (while (re-search-forward "\\<IN\\([0-9]+\\)" nil t)
      (put-text-property (match-beginning 1)
			 (match-end 1)
			 'face
			 'bold))
    (goto-char (point-min))
    (other-frame -1)))

(add-hook 'kdic-correct-hook 'my-kdic-correct-answer)

;;; TextProperties

(defalias 'text-properties-remove-all 'facemenu-remove-all)

