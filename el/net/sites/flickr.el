;;; flickr.el --- Upload photos to Flickr with Emacs

;; Copyright (C) 2004  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use, set `flickr-email' and `flickr-password' appropriately.

;; Use `flickr-upload' to upload a file.

;; This code binds `f' to something useful in `thumbs-mode' if you have
;; `thumbs-mode' installed.

;;; Code:

(require 'http-post)

(defvar flickr-email nil)
(defvar flickr-password nil)

;; Main (non-thumbs-mode) user entry point.

(defun flickr-upload (filename tags)
  "Upload FILENAME to flickr with TAGS."
  (interactive "fImage: \nsTags: ")

  (let ((image-data (with-temp-buffer
                      (insert-file-contents-literally filename)
                      (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
    (flickr-post-multipart "www.flickr.com" 80 "/tools/uploader_go.gne"
                           `(("email" ,flickr-email)
                             ("password" ,flickr-password)
                             ("photo" ,filename)
                             ("tags" ,tags))
                           `(("photo" ,filename ,image-data)))))

;; Interfacing with thumbs-mode.

(when (condition-case nil
          (require 'thumbs)
        (error nil))
  (define-key thumbs-mode-map (kbd "f") 'flickr-images)
  (define-key thumbs-view-image-mode-map (kbd "f")'flickr-image)

  (defun flickr-image (tags)
    "Upload the current image to flickr."
    (interactive "sTags: ")
    (flickr-upload thumbs-current-image-filename tags))

  (defun flickr-images (tags)
    "Upload the marked images to flickr.
Uploads the current image if none are marked."
    (interactive "sTags: ")
    (mapcar (lambda (image)
              (flickr-upload image tags))
            thumbs-markedL)))

;; No user-serviceable parts inside.

(defmacro flickr-push (x list-sym)
  "Insert X at the head of the list named by LIST-SYM."
  `(setq ,list-sym (cons ,x ,list-sym)))

(defvar flickr-boundary "=-=-=")

(defun flickr-encode-multipart-form-data (fields files)
  "Returns crazy shit."
  (let ((boundary flickr-boundary)
        (retval ()))
    (mapcar (lambda (field)
              (flickr-push (concat "--" boundary) retval)
              (flickr-push (concat "Content-Disposition: form-data; name=\""
                                   (car field)
                                   "\"")
                           retval)
              (flickr-push "" retval)
              (flickr-push (cadr field) retval))
            fields)
    (mapcar (lambda (file)
              (flickr-push (concat "--" boundary) retval)
              (flickr-push (concat "Content-Disposition: form-data; name=\""
                                   (car file)
                                   "\"; filename=\""
                                   (cadr file)
                                   "\"")
                           retval)
              (flickr-push (concat "Content-type: image/jpeg"
                                   ;;(guess-content-type (cadr file))
                                   )
                           retval)
              (flickr-push "" retval)
              (flickr-push (nth 2 file) retval)
              (flickr-push (cdr file) retval))
            files)
    (flickr-push (concat "--" boundary "--") retval)
    (flickr-push "" retval)
    (mapconcat (lambda (something)
                 (format "%s" something))
               (nreverse retval)
               "\r\n")))

(defun flickr-post-multipart (host port path fields files)
  "This is so ghetto, it's stupid."
  (let* ((proc (open-network-stream "Flickr" "*Flickr*" host port))
         (buf (process-buffer proc))
         (data (flickr-encode-multipart-form-data fields files)))
    (process-send-string
     proc
     (concat
      "POST " path " HTTP/1.0\r\n"
      "Host: " host "\r\n"
      "Content-type: multipart/form-data; boundary=\"" flickr-boundary "\"\r\n"
      "Content-length: " (format "%d" (length data)) "\r\n\r\n"
      data))
    (while (eq (process-status proc) 'open)
      (when (not (accept-process-output proc 180))
        (delete-process proc)
        (error "Some kind of network error occured.")))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (looking-at "HTTP/1.0 200 OK")
          (let ((ids ()))
            (while (re-search-forward "<photoid>\\([0-9]+\\)</photoid>")
              (flickr-push (match-string 1) ids))
            (message "Success! Photo id %s" (mapconcat 'identity ids " ")))
        (error "Failed to upload image to flickr. Sorry!")))))

(provide 'flickr)
;;; flickr.el ends here

