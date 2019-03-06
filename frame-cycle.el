;;; frame-cycle.el --- frame switching helper        -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  ffrances
;;
;; Author: ffrances
;; Version: 0.2
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
;;  frame-cycle-select-frame:
;;       select a frame to swap position with `selected-frame'
;;
;;; Configuration:
;;
;; (require 'frame-cycle)
;; (global-set-key (kbd "<f1> n") 'frame-cycle-next)
;; (global-set-key (kbd "<f1> p") 'frame-cycle-prev)
;; (global-set-key (kbd "<f1> s") 'frame-cycle-select-frame)
;;; Code:
(defun frame-cycle-rotate-next (list)
  "Rotate LIST (list 1 2 3 4) becomes (list 2 3 4 1)."
  (let ((head (car list))
        (tail (cdr list)))
    (reverse (cons head (reverse tail)))))

(defun frame-cycle-rotate-prev (list)
  "Rotate LIST (list 1 2 3 4) becomes (list 4 1 2 3)."
  (reverse (frame-cycle-rotate-next (reverse list))))

(defun frame-cycle-frame-position (frame)
  (list
   (cons 'top (frame-parameter frame 'top))
   (cons 'left (frame-parameter frame 'left))
   (cons 'fullscreen (frame-parameter frame 'fullscreen))
   ))

(defun frame-cycle-list-position (l-frames)
  "Return positon 'top and 'left for each frame in L-FRAMES."
  (mapcar (lambda (frame)
            (frame-cycle-frame-position frame))
          l-frames))

(defun frame-cycle-rotate-list (rotation list)
  "Apply ROTATION 'next or 'prev to LIST."
  (cond ((equal rotation 'next)
         (frame-cycle-rotate-next list))
        ((equal rotation 'prev)
         (frame-cycle-rotate-prev list))
        (t list)))

(defun frame-cycle-remove-fullscreen-apply-parameters( frame parameters )
  "Remove 'fullscreen parameter from FRAME and apply PARAMETERS."
  (when (frame-parameter frame 'fullscreen)
    (set-frame-parameter frame 'fullscreen nil)
    ;; Manipulating a frame without waiting for the fullscreen
    ;; animation to complete can cause a crash, or other unexpected
    ;; behavior, on macOS (bug#28496).
    (when (featurep 'cocoa) (sleep-for 0.5)))

  (modify-frame-parameters frame parameters)

  (when (frame-parameter frame 'fullscreen)
    ;; Manipulating a frame without waiting for the fullscreen
    ;; animation to complete can cause a crash, or other unexpected
    ;; behavior, on macOS (bug#28496).
    (when (featurep 'cocoa) (sleep-for 0.5))))

(defun frame-cycle-apply (direction)
  "Rotate frame in DIRECTION 'prev or 'next.
Call `frame-cycle-hook' after that."
  (let* ((old-frames (reverse (x-frame-list-z-order)))
         (new-frames (frame-cycle-rotate-list direction old-frames))
         (parameters (frame-cycle-list-position old-frames))
;;         (ret (list))
         )

    (while parameters
      (let* ((parameter (car parameters))
             (new-frame (car new-frames)))
;;        (setq ret (cons new-frame ret))
        (setq new-frames (cdr new-frames))
        (setq parameters (cdr parameters))

        (frame-cycle-remove-fullscreen-apply-parameters new-frame parameter)

        ;; raise frame in reverse z-stack order
        ;; so new top frame is on top.
        (raise-frame new-frame)
        (select-frame new-frame))
      )
;;    ret
    ))

(defun format-frame-info (frame)
  (format "%s @ %sx%s"
          (frame-parameter frame 'name)
          (frame-parameter frame 'top)
          (frame-parameter frame 'left)))


(defun frame-cycle-swap (frame1 frame2)
  "Swap FRAME1 and FRAME2 position."
  (unless (equal frame1 frame2)
    (let ((frame1-info     (frame-cycle-frame-position frame1))
          (frame2-info     (frame-cycle-frame-position frame2)))
      (frame-cycle-remove-fullscreen-apply-parameters frame1 frame2-info)
      (frame-cycle-remove-fullscreen-apply-parameters frame2 frame1-info))))

(defun frame-cycle-make-frame-name-alist (f-list)
  "Return assoc list of positions from F-LIST."
  (mapcar
   (lambda (frame)
     (cons (format-frame-info frame) frame))
   f-list))

(defun frame-cycle-select-frame-by-name (frame-names-alist)
  "Select and return frame from FRAME-NAMES-ALIST."
  (let ((default (car (car frame-names-alist))))
    (cdr (assoc
          (completing-read (format "Select Frame (current %s): "
                                       default)
                               frame-names-alist nil t nil)
          frame-names-alist))))

(defun frame-cycle-select-frame ()
  "Select the frame by name and swap it with current frame.

use `frame-cycle-swap' for this operation."

  (interactive)
  (let* ((frame-names-alist (frame-cycle-make-frame-name-alist (x-frame-list-z-order)))
         (new-frame (frame-cycle-select-frame-by-name frame-names-alist)))

    (frame-cycle-swap new-frame (selected-frame))

    (raise-frame new-frame)
    (select-frame new-frame)))

(defun frame-cycle-next ()
  "Cycle frame using `frame-cycle-apply'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame2 frame3 frame4 frame1
"
  (interactive)
  (frame-cycle-apply 'next))

(defun frame-cycle-prev ()
  "Cycle frame using `frame-cycle-apply'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame4 frame1 frame2 frame3 frame4
"
  (interactive)
  (frame-cycle-apply 'prev))

(provide 'frame-cycle)
;;; frame-cycle.el ends here
