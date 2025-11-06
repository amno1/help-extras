;;; help-remote.el --- Remote control for help window  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: tools

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
(require 'pcase)
(require 'help-mode)
(eval-and-compile
  (require 'buffer-remote (expand-file-name "buffer-remote.el")))


;;; Customize
(defgroup help nil
  "This adds to built-in help-mode so we put all defcustoms in same group."
  :prefix "helper-"
  :group 'help)

(defvar helper--remote-advised nil
  "List of advised functiqons.")

(defvar help-remote--from-window nil
  "Holds the window to return to when we jump from *Help* buffer.")

;; with a little help from Helm
(defcustom help-remote-help-prefix-key "C-h h"
  "The key HELP-REMOTE-HELP-PREFIX-MAP is bound to in the global map."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :set (lambda (var key)
         (when (and (boundp var) (symbol-value var))
           (define-key (current-global-map)
                       (read-kbd-macro (symbol-value var)) nil))
         (when key
           (define-key (current-global-map)
                       (read-kbd-macro key)
                       (remote--gen-map
                        help-mode-map "help-" "*Help*"
                        'helper--remote-advised)))
         (set var key))
  :group 'help-remote)

(defun help-push-button ()
  "Help-specific version of `push-button'"
  (interactive)
  (let ((help-buffer (help-buffer)))
    (unless (window-live-p (get-buffer-window help-buffer))
      (display-buffer help-buffer))
    (with-selected-window (get-buffer-window help-buffer)
      (with-current-buffer help-buffer
        (call-interactively #'push-button)))))

(define-minor-mode helper--remote-internal
  "A remote control for the `*Help*' buffer."
  :global nil :lighter " help-remote"
  (cond
   (helper--remote-internal
    (define-key help-mode-map [?\r] #'help-push-button)
    (define-key (current-global-map)
                (kbd help-remote-help-prefix-key)
                (remote--gen-map
                 help-mode-map "help-" "*Help*" 'helper--remote-advised)))
   (t
    (while helper--remote-advised
      (let ((advice (pop helper--remote-advised)))
        (advice-remove (car advice) (cdr advice))))
    (define-key help-mode-map [?\r] nil)
    (define-key global-map (kbd help-remote-help-prefix-key) nil))))

;;;###autoload
(define-globalized-minor-mode help-remote
  helper--remote-internal (lambda () (helper--remote-internal)))

(provide 'help-remote)
;;; help-remote.el ends here
