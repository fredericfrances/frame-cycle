;;; frame-cycle.el --- frame switching helper        -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  ffrances
;;
;; Author: ffrances
;; Version: 0.1
;; Keywords: frames
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is not part of gnu Emacs.
;;
;; Cycle frame position but keep selected frame at top position.
;;
;;; Usage:
;;
;;  frame-cycle-next moves frame as follow:
;;      (list frame1 frame2 frame3 frame4) -> (list frame2 frame3 frame4 frame1)
;;
;;  frame-cycle-prev moves frame as follow:
;;      (list frame1 frame2 frame3 frame4) -> (list frame4 frame1 frame2 frame3)
;;
;;; Configuration:
;;
;; (require 'frame-cycle)
;; (global-set-key (kbd "C-<left>") 'frame-cycle-next)
;; (global-set-key (kbd "C-<right>") 'frame-cycle-prev)
;;
;;; Code:
;;(require 'cl)

(defvar frame-cycle-hooks nil
  "Hook called after `frame-cycle-apply'.")

(defun frame-cycle-rotate-next (list)
  "Rotate LIST (list 1 2 3 4) becomes (list 2 3 4 1)."
  (let ((head (car list))
        (tail (cdr list)))
    (reverse (cons head (reverse tail)))))

(defun frame-cycle-rotate-prev (list)
  "Rotate LIST (list 1 2 3 4) becomes (list 4 1 2 3)."
  (reverse (frame-cycle-rotate-next (reverse list))))

(defun frame-cycle-list-position (l-frames)
  "Return positon 'top and 'left for each frame in L-FRAMES."
  (mapcar (lambda (frame)
            (list
             (cons 'top (frame-parameter frame 'top))
             (cons 'left (frame-parameter frame 'left))
             ))
          l-frames))

(defun frame-cycle-rotate-list (rotation list)
  "Apply ROTATION 'next or 'prev to LIST."
  (cond ((equal rotation 'next)
         (frame-cycle-rotate-next list))
        ((equal rotation 'prev)
         (frame-cycle-rotate-prev list))
        (t list)))

(defun frame-cycle-apply (direction)
  "Rotate frame in DIRECTION 'prev or 'next.
Call `frame-cycle-hook' after that."
  (let* ((old-frames (reverse (x-frame-list-z-order)))
         (new-frames (frame-cycle-rotate-list direction old-frames))
         (positions (frame-cycle-list-position old-frames)))

    (mapcar* (lambda (new-frame position)
               (modify-frame-parameters new-frame position)
               ;; raise frame in reverse z-stack order
               ;; so new top frame is on top.
               (raise-frame new-frame))
             new-frames positions))
  (run-hooks 'frame-cycle-hooks))

(defun frame-cycle-next ()
  "Cycle frame using `frame-cycle-rotate-next'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame2 frame3 frame4 frame1
"
  (interactive)
  (frame-cycle-apply 'next))

(defun frame-cycle-prev ()
  "Cycle frame using `frame-cycle-rotate-prev'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame4 frame1 frame2 frame3 frame4
"
  (interactive)
  (frame-cycle-apply 'prev))

(global-set-key (kbd "C-<left>") 'frame-cycle-next)
(global-set-key (kbd "C-<right>") 'frame-cycle-prev)

(provide 'frame-cycle)
;;; frame-cycle.el ends here
