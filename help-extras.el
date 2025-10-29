;;; help-extras.el --- Common function for help-extras  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;;; Customize
(defgroup help nil
  "This adds to built-in help-mode so we put all defcustoms in same group."
  :prefix "helper-"
  :group 'help)


;;; Private

(defvar help-display-source-mode) ;; forward declaration

(defvar helper--post-insert-hook)

(defmacro with-help-buffer (&rest body)
  "Execuate BODY in the context of `help-buffer'."
  (declare (indent 0) (debug t))
  `(let ((help-buffer (help-buffer)))
     (when (get-buffer-window help-buffer)
       (with-current-buffer help-buffer
         (with-silent-modifications
           (save-excursion ,@body))))))


(provide 'help-extras)
;;; help-extras.el ends here
