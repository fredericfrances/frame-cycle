;;; frame-cycle.el --- frame switching helper        -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  ffrances
;;
;; Author: ffrances
;;
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
;; This is not part of gnu emacs.
;;
;; Can be usefull on multi-frame environment or when emacs is run on multiple screen.
;;
;; This lisp part try to cycle frame content instead of switching between frames.
;; it is a bit slow for the moment.
;;
;;
;;; Usage:
;;
;;  frame-cycle-head-to-tail moves frame as follow:
;;      (list frame1 frame2 frame3 frame4) -> (list frame2 frame3 frame4 frame1)
;;
;;  frame-cycle-head-to-tail moves frame as follow:
;;      (list frame1 frame2 frame3 frame4) -> (list frame4 frame1 frame2 frame3)
;;
;;; Configuration:
;;
;; (require 'frame-cycle)
;; (global-set-key (kbd "C-<left>") 'frame-cycle-head-to-tail)
;; (global-set-key (kbd "C-<right>") 'frame-cycle-tail-to-head)
;;
;;; Code:
(require 'cl)

(defvar frame-cycle-hooks nil
  "Hook called after `frame-cycle-apply'.")

(defun frame-cycle-rotate-head-to-tail (list)
  "Rotate LIST (list 1 2 3 4) -> (list 2 3 4 1)."
  (let ((head (car list))
        (tail (cdr list)))
    (reverse (cons head (reverse tail)))))

(defun frame-cycle-rotate-tail-to-head (list)
  "Rotate LIST (list 1 2 3 4) -> (list 4 1 2 3)."
  (reverse (frame-cycle-rotate-head-to-tail (reverse list))))

(defun frame-cycle-create-frame-configuration (frame-list
                                               config-list)
  "Create a frame configuration using FRAME-LIST and CONFIG-LIST."
  (cons 'frame-configuration
           (mapcar* (lambda (frame configuration)
                      (let ((frame-param   (car (cdr configuration))))
                        (list frame frame-param
                              (current-window-configuration frame))))
                    frame-list  config-list)))


(defun frame-cycle-apply  (frame-list)
  "Create a new frame configuration and call `set-frame-configuration'.
Call `frame-cycle-hook' after that."
  (let ((focus (selected-frame))
        (config-list (cdr (current-frame-configuration))))
    (set-frame-configuration
     (frame-cycle-create-frame-configuration frame-list config-list))

    (if (fboundp 'frame-restack) ;; emacs 26 feature
        (mapc (lambda (frame)
                (frame-restack focus frame))
              frame-list))
    (select-frame focus))
  (run-hooks 'frame-cycle-hooks))

(defun frame-cycle-head-to-tail ()
  "Cycle frame using `frame-cycle-rotate-head-to-tail'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame2 frame3 frame4 frame1
"
  (interactive)
  (frame-cycle-apply (frame-cycle-rotate-head-to-tail (frame-list))))

(defun frame-cycle-tail-to-head ()
  "Cycle frame using `frame-cycle-rotate-tail-to-head'.

Moves frame content as follow:
     frame1 frame2 frame3 frame4 ->  frame4 frame1 frame2 frame3 frame4
"
  (interactive)
  (frame-cycle-apply (frame-cycle-rotate-tail-to-head (frame-list))))

(provide 'frame-cycle)
;;; frame-cycle.el ends here
