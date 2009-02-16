;;; Saved through ges-version 0.3.3dev at 2004-11-20 18:04
;;; From: Ian Zimmerman <itz@buug.org>
;;; Subject: obqueue.el --- handle queues of finalizable objects
;;; Newsgroups: gmane.emacs.sources
;;; Date: 11 Jul 2004 09:01:08 -0700

;;; obqueue.el --- handle queues of finalizable objects

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

;; Copyright (C) 2004 Ian Zimmerman

;; $Id: obqueue.el,v 1.2 2004/07/06 03:52:16 itz Exp $

(defun obqueue-make (init-size trail enlarge-fun finalizer)
  (let ((vec (make-vector init-size nil)))
    (vector vec init-size 0 -1 trail enlarge-fun finalizer)))

(defsubst obqueue-vector (obq)
  (aref obq 0))

(defsubst obqueue-set-vector (obq vec)
  (aset obq 0 vec))

(defsubst obqueue-allocated-size (obq)
  (aref obq 1))

(defsubst obqueue-set-allocated-size (obq size)
  (aset obq 1 size))

(defsubst obqueue-length (obq)
  (aref obq 2))

(defsubst obqueue-set-length (obq len)
  (aset obq 2 len))

(defsubst obqueue-cursor (obq)
  (aref obq 3))

(defsubst obqueue-set-cursor (obq cur)
  (aset obq 3 cur))

(defsubst obqueue-trail (obq)
  (aref obq 4))

(defsubst obqueue-enlarge-fun (obq)
  (aref obq 5))

(defsubst obqueue-finalizer (obq)
  (aref obq 6))

(defun obqueue-empty-p (obq)
  (zerop (obqueue-length obq)))

(defun obqueue-ref (obq)
  (when (obqueue-empty-p obq)
    (error "Referencing an empty obqueue"))
  (aref (obqueue-vector obq) (obqueue-cursor obq)))

(defun obqueue-push (obq datum)
  (let ((len (obqueue-length obq))
        (vec (obqueue-vector obq)))
    (when (= len (obqueue-allocated-size obq))
      (if (> (obqueue-cursor obq) (obqueue-trail obq))
          (let ((shift (- (obqueue-cursor obq) (obqueue-trail obq)))
                (i 0))
            (while (< i shift)
              (funcall (obqueue-finalizer obq) (aref vec i))
              (aset vec i (aref vec (+ i shift)))
              (setq i (1+ i)))
            (obqueue-set-length obq (- (obqueue-length obq) shift))
            (obqueue-set-cursor obq (- (obqueue-cursor obq) shift))
            (setq len (obqueue-length obq)))
        (let* ((new-size (funcall (obqueue-enlarge-fun obq) len))
               (new-vec (make-vector new-size nil))
               (i 0))
          (when (<= new-size len)
            (error "New obqueue size smaller than before enlargement"))
          (while (< i len)
            (aset new-vec i (aref vec i))
            (setq i (1+ i)))
          (obqueue-set-allocated-size obq new-size)
          (obqueue-set-vector obq new-vec)
          (setq vec new-vec))))
    (aset vec len datum)
    (obqueue-set-length obq (1+ len))))

(defun obqueue-move (obq n)
  (let ((new-cur (+ (obqueue-cursor obq) n)))
    (when (or (< new-cur 0) (<= (obqueue-length obq) new-cur))
      (error "New obqueue cursor out of bounds"))
    (obqueue-set-cursor obq new-cur)))

(defun obqueue-start (obq)
  (when (obqueue-empty-p obq)
    (error "Restarting an empty obqueue"))
  (obqueue-set-cursor obq 0))

(defun obqueue-end (obq)
  (when (obqueue-empty-p obq)
    (error "Going to the end of an empty obqueue"))
  (obqueue-set-cursor obq (1- (obqueue-length obq))))

(provide 'obqueue)

;;; obqueue.el ends here

