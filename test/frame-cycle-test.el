;;; frame-cycle-test.el ---
;;
;; Filename: frame-cycle-test.el
;; Description:
;; Author: ffrances
;; Maintainer:
;; Created: sam. mars  2 22:45:07 2019 (+0100)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'ert)
(require 'cl)

(ert-deftest rotate-next()
  (let* ((expect "(2 3 4 1)")
         (result (format "%s" (frame-cycle-rotate-next (list 1 2 3 4))))
         )
    (should (string-equal expect result))))

(ert-deftest rotate-prev()
  (let* ((expect "(4 1 2 3)")
         (result (format "%s" (frame-cycle-rotate-prev (list 1 2 3 4)))))
    (should (string-equal expect result))))

(ert-deftest list-position()
  (let* ((frames (frame-list))
         (positions (frame-cycle-list-position frames)))
    (should (equal (length frames) (length positions)))
    ))

(ert-deftest rotate-list-next()
  (let ((expect "(2 3 4 1)")
        (result (format "%s" (frame-cycle-rotate-list 'next (list 1 2 3 4)))))
    (should (string-equal expect result))))

(ert-deftest rotate-list-prev()
  (let ((expect "(4 1 2 3)")
        (result (format "%s" (frame-cycle-rotate-list 'prev (list 1 2 3 4)))))
    (should (string-equal expect result))))

(ert-deftest rotate-list-xxx()
  (let ((expect "(1 2 3 4)")
        (result (format "%s" (frame-cycle-rotate-list 'xxx (list 1 2 3 4)))))
    (should (string-equal expect result))))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; frame-cycle-test.el ends here
