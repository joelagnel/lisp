
;;emms

(load "de-vars")
(setq load-path (cons (concat root-path "audio/emms") load-path))


(require 'emms-setup)
;(emms-standard)
(emms-devel)
(emms-default-players)


(require 'emms-playlist-sort)

;; Show the current track each time EMMS
(add-hook 'emms-player-started-hook 'emms-show)
  

;(setq emms-source-file-default-directory "~/music/")

(require 'emms-player-mplayer)        

(setq emms-player-mpg321-parameters '("-o" "alsa"))
(setq emms-player-mplayer-parameters (list "-slave" "-nortc" "-quiet" "-really-quiet"))
;; (setq emms-player-mplayer-parameters '("-af" "equalizer=1:1:5:4:5:1:0:8:8:8"))


(require 'emms-info-libtag)
(require 'emms-player-mpg321-remote)



(push 'emms-player-mpg321-remote emms-player-list)
(push 'emms-player-mplayer emms-player-list)
(push 'emms-player-mplayer-playlist emms-player-list)


(setq emms-playlist-buffer-name "*Music*")

(setq
 emms-info-asynchronously t
 later-do-interval 0.0001
 emms-info-functions '(emms-info-libtag)
 emms-mode-line-format " emms "
 emms-show-format "playing: %s")

(global-set-key (kbd "<kp-subtract>") 'emms-previous)
(global-set-key (kbd "<kp-add>") 'emms-next)
(global-set-key (kbd "<insert>") 'de-add-dir)
(global-set-key (kbd "<f2>") 'emms-smart-browse)
(global-set-key (kbd "<kp-right>") 'emms-seek-forward)
(global-set-key (kbd "<kp-left>") 'emms-seek-backward)

(define-key emms-playlist-mode-map (kbd "/") 'de-search)
(define-key emms-browser-mode-map (kbd "W W") 'emms-browser-lookup-multi)

(add-hook 'emms-playlist-selection-changed-hook 'de-focus-on-track)
(add-hook 'emms-player-started-hook 'emms-show)


;; (setq emms-browser-default-covers
;;       (list "/home/ike/Tofs/dirac.jpg" nil nil))


(defun de-focus-on-track ()
  (let ((w (get-buffer-window emms-playlist-buffer t)))
    (when w
      (with-selected-window w
        (emms-playlist-mode-center-current)
        (recenter '(4))))))

(defun de-toggle-playing ()
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(defun de-add-dir ()
  (interactive)
  (call-interactively 'emms-add-directory-tree)
  (emms-playlist-mode-go))

(defun de-search ()
  (interactive)
  (goto-char (point-min))
  (call-interactively 'isearch-forward))

(defun emms-browser-lookup-multi ()
  (interactive)
  (emms-browser-lookup-wikipedia 'info-artist)
  (emms-browser-lookup-wikipedia 'info-album)
  (emms-browser-lookup-pitchfork 'info-artist))

      

(require 'emms-mode-line)
(emms-mode-line 1)

(require 'emms-playing-time)
(emms-playing-time 1)

(require 'emms-volume)

(require 'emms-mode-line-icon)

(require 'emms-metaplaylist-mode)

(setq emms-mode-line-titlebar-function 'emms-mode-line-playlist-current)


(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

(defun emms-google-for-lyrics ()
  (interactive)
  (browse-url
   (concat "http://www.google.com/search?q="
           (replace-regexp-in-string " +" "+"
                                     (concat "lyrics "
                                             (delete ?- (emms-track-description
                             (emms-playlist-current-selected-track))))))))


