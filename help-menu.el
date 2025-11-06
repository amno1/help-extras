;;; help-menu.el --- Display context menu for help-mode in a buffer  -*- lexical-binding: t; -*-

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

(defvar context-menu-mode-map)
(defvar help-menu "*help-menu*")
(defvar-local menu-mode--working-frame)
(defvar-local menu-mode--working-window)
(defvar-local menu-mode--parent-menu nil)

;; hack to get a key to auto-bind keys on each screen
(defun make-free-keys ()
  (let ((keys
         (append "wertasdfgzxcvbyuiophjklnmWERTASDFGZXCVBYUIOPHJKLNM0123456789,.-+;:!%&/(){[]}" nil)))
    (lambda () (string (pop keys)))))

(defsubst next-free-key (free-keys-list)
  (funcall free-keys-list))

(defvar-local free-keys (make-free-keys))

(defun find-free-key (keymap)
  (let ((key (next-free-key free-keys)))
    (while (lookup-key keymap (kbd key))
      (setf key (next-free-key free-keys)))
    key))

(defun lookup-command-binding (command keymap)
  "Find a key to which COMMAND is bound if any in KEYMAPS."
  (let ((bindings (where-is-internal command keymap)))
    (when bindings
      (let ((shortest (key-description (car bindings))))
        (dolist (binding (cdr bindings))
          (let ((description (key-description binding)))
            (when (< (length description) (length shortest))
              (setf shortest description))))
        (or
         (cond ((string-prefix-p "<menu-bar>" shortest)
                (find-free-key keymap))
               ((string-empty-p shortest)
                (find-free-key keymap))
               (t shortest))
         ""))))) 

(defun make-menu-mode-map ()
  "Generate base keymap for `context-menu-mode'"
  (append (make-sparse-keymap)
          (cons 17 'buffer-menu-quit-cmd)
          (cons 113 'buffer-menu-quit-menu-cmd)
          (list '(remap keymap (self-insert-command . undefined)))))

(defun make-menu-cmd (command)
  "Generate a wrapper for COMMAND"
  (lambda ()
    (interactive)
    (with-selected-frame menu-mode--working-frame
      (with-selected-window menu-mode--working-window
        (call-interactively command)))))

(defun make-submenu-cmd (submenu original-keymap)
  "Generate a wrapper to render a submenu"
  (lambda ()
    (interactive)
    (push original-keymap menu-mode--parent-menu)
    (display-menu-in-buffer submenu original-keymap)))

(defun buffer-menu-quit-cmd ()
  (interactive)
  (call-interactively 'quit-window))

(defun buffer-menu-quit-menu-cmd ()
  (let ((parent (pop menu-mode--parent-menu)))
    (if menu-mode--parent-menu
        (display-menu-in-buffer parent (car menu-mode--parent-menu))
      (call-interactively 'buffer-menu-quit-cmd))))

(defun insert-menu-in-buffer (menu original-keymap)
  "Insert MENU into `current-buffer'"
  (let ((map (make-menu-mode-map))
        (free-keys (make-free-keys)))
    (dolist (item (cddr menu))
      (let* ((cmd (nth 3 item))
             (help (nth 5 item))
             (label (nth 2 item))
             (key (lookup-command-binding cmd original-keymap)))

        (cond
         ((keymapp cmd)
          (define-key map (kbd key) (make-submenu-cmd cmd original-keymap))
          (setf label (format "[%s]" label)))
         (t
          (define-key map (kbd key) (make-menu-cmd cmd))))

        (when label
          (insert
           key
           "\t"
           (propertize label 'help-echo help 'mouse-face 'highlight)
           "\n"))))
    (use-local-map map)))

(defun display-menu-in-buffer (menu original-keymap)
  "Display MENU keymap in `current-buffer'"
  (let ((label (or (keymap-prompt menu) (symbol-name major-mode)))
        (inhibit-read-only t))
    (erase-buffer)
    (insert label ":\n\n")
    (insert-menu-in-buffer menu original-keymap)))

(defun keymap-menu-bar (keymap)
  "Return menubar items for KEYMAP if any"
  (let ((menubar
         (catch 'menubar
           (when (keymapp keymap)
             (dolist (elt keymap)
               (and (consp elt) (eq (car elt) 'menu-bar)
                    (throw 'menubar elt)))))))
    menubar))

(define-derived-mode buffer-menu-mode special-mode "Context-menu mode"
  "Help mode for context-menu buffer."
  :global nil :lighter " ctxt-menu")

(cl-defun display-keymap-menus (&optional (keymap (current-local-map))
                                          (menu-label (symbol-name major-mode)))
  (let ((frame (selected-frame))
        (window (selected-window)))
    (with-current-buffer (get-buffer-create menu-label)
      (let ((inhibit-read-only t)
            (menu-bar (cddr (keymap-menu-bar keymap))))
        (buffer-menu-mode)
        (setf menu-mode--working-frame frame
              menu-mode--working-window window)
        (while menu-bar
          (let ((menu
                 (cl-find-if (lambda (x)
                               (and (consp x)
                                    (eq (cadr x) 'menu-item)
                                    (nth 3 x)))
                             menu-bar)))
            (when menu
              (display-menu-in-buffer (nth 3 menu) keymap)))
          (pop menu-bar)))
      (switch-to-buffer-other-window (current-buffer)))))

(defun display-global-menu-map ()
  (interactive)
  (display-keymap-menus (current-global-map) "*global-menu*"))

(defun display-major-mode-menu ()
  (interactive)
  (display-keymap-menus (symbol-value (derived-mode-map-name major-mode))
                        (format "*%s-menu*" major-mode)))

(provide 'help-menu)
;;; help-menu.el ends here
