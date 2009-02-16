;; This is -*- emacs-lisp -*-
;; -------------------------------------------------------------------------- ;;
;;   EMMS configuration script                                                ;;
;; .......................................................................... ;;
;;   Author:             Lucas Bonnet                                         ;;
;;   Sources:            EmacsWiki, #emacs, emms-help@gnu.org                 ;;
;; -------------------------------------------------------------------------- ;;

(require 'emms-setup)
(load-file "~/repository/lisp/el/text/string.el")
(require 'string)

(emms-devel)

(setq emms-mode-line-titlebar-function 'emms-mode-line-playlist-current)

(add-hook 'emms-player-started-hook 'my-emms-current 'emms-get-current-track-name)

(setq emms-player-mpg321-parameters '("-o" "alsa"))

(setq emms-show-format "NP: %s")

;; get track name
;; Abhi


(defun emms-get-current-track-name ()
         (car (reverse (string-split "/" (emms-track-description (emms-playlist-current-selected-track))))))

(defun emms-google-for-lyrics ()
  (interactive)
  (browse-url
   (concat "http://www.google.com/search?q="
           (replace-regexp-in-string " +" "+"
                                     (concat "lyrics "  (emms-get-current-track-name))
;;;                                              (delete ?- (emms-track-description
;;;                              (emms-playlist-current-selected-track))))))))

;; play what i mean
(defun emms-pwim (truc &optional rien)
  "Plays the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will play the
tree."
  (interactive
   (find-file-read-args "Play what ? " t))
  (cond
   ((file-exists-p truc)    (emms-play-file truc))
   ((file-directory-p truc) (emms-play-directory truc)))
  )

;; add what i mean
(defun emms-awim (truc &optional rien)
  "Adds the TRUC specified, whatever it is. The function tries to
guess the type of TRUC, between playlist, directory containing
playable tracks, and files. If the directory does not contain
playable tracks, but some sub-directories, it will add the
tree."
  (interactive
   (find-file-read-args "Add what ? " t))
  (cond
   ((file-exists-p truc)    (emms-add-file truc))
   ((file-directory-p truc) (emms-add-directory truc)))

  ;; test playlist
  )

(defun emms-osd-message (string)
  (shell-command-to-string
   (concat "osdctl" " -s " "\"" string "\"" ","))
  nil)

(defun my-emms-current ()
  (interactive)
  (emms-track-description (emms-playlist-current-selected-track)))

(defun emms-osd-np ()
  (interactive)
  (emms-osd-message (my-emms-current))
  nil)

(add-hook 'emms-player-started-hook 'emms-osd-np)


(setq emms-stream-info-format-string "NS: %s"
      emms-stream-default-action "play"
      emms-stream-popup-default-height 120)

(define-emms-simple-player spc '(file) (regexp-opt '(".spc" ".SPC")) "ospc" "-l" "-t " "3:00")

(setq emms-player-list
      '(
        emms-player-spc
        emms-player-mplayer-playlist
        emms-player-mplayer
        emms-player-gstreamer))

(setq emms-player-mplayer-parameters (list "-slave" "-nortc" "-quiet" "-really-quiet"))

;; Icon setup.
(setq emms-mode-line-icon-before-format "["
      emms-mode-line-format " %s]"
      emms-mode-line-icon-color "lightgrey")

(require 'emms-mode-line-icon)

(defun circe-command-NP (arg)
  "test."
  (lui-replace-input (emms-show)))

(setq emms-lyric-display-p nil)

(defun emms-mode-line-icon-function ()
  (concat " "
          emms-mode-line-icon-before-format
          (propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (format emms-mode-line-format (emms-track-get
                                         (emms-playlist-current-selected-track)
                                         'info-title))))

(setq emms-playlist-buffer-name "*EMMS Playlist*"
      emms-playlist-mode-open-playlists t)

;; Libtag support
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

;; Stolen and adapted from TWB
(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (and (emms-track-get track 'info-artist)
           (emms-track-get track 'info-title))
      (let ((pmin (emms-track-get track 'info-playing-time-min))
            (psec (emms-track-get track 'info-playing-time-sec))
            (ptot (emms-track-get track 'info-playing-time))
            (art  (emms-track-get track 'info-artist))
            (tit  (emms-track-get track 'info-title)))
        (cond ((and pmin psec) (format "%s - %s [%02d:%02d]" art tit pmin psec))
              (ptot (format  "%s - %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
              (t (emms-track-simple-description track))))))

(setq emms-track-description-function 'my-emms-info-track-description)

;; caching stuff
(setq later-do-interval 0.001
      emms-info-asynchronously nil)

(global-set-key (kbd "<f1>")    'emms-google-for-lyrics)
(global-set-key (kbd "<f2>")    'emms-smart-browse)
(global-set-key (kbd "<f3>")    'emms-playlist-mode-go)
(global-set-key (kbd "<S-f3>")  'emms-stream-popup)
(global-set-key (kbd "C-c <right>") 'emms-next)
(global-set-key (kbd "C-c <left>") 'emms-previous)

(provide 'emms-lukhas-setup)
