;;; buffer-remote.el --- Functions for controlling buffers from other buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Arthur Miller

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
(defgroup buffer-remote nil
  "This adds to built-in help-mode so we put all defcustoms in same group."
  :prefix "remote-"
  :group 'convenience)

(defun remote--make-advice-body (buffer-name)
  "Create lambda function for buffer with name BUFFER-NAME."
  (lambda (fn &rest args)
    (let ((previous (selected-window))
          (buffer (get-buffer buffer-name))
          (window (get-buffer-window buffer-name)))
      (unwind-protect
          (when (get-buffer buffer)
            (unless (window-live-p window)
              (display-buffer buffer))
            (select-window window)
            (apply fn args))
        (select-window previous)))))

(defun remote--make-advice-function (buffer-name)
  "Create advice function for buffer with name BUFFER-NAME."
  (let ((advice-name
         (intern (format "help-remote--%s-advice-fn" buffer-name))))
    (unless (fboundp advice-name)
      (defalias advice-name (remote--make-advice-body buffer-name)))
    advice-name))

(defun remote--advise (fn buffer-name advised)
  "Advice function FN to run in buffer with name BUFFER-NAME."
  (let ((advice (remote--make-advice-function buffer-name)))
    (add-to-list advised (cons fn advice))
    (advice-add fn :around advice)))

(defun remote--gen-cons (binding prefix buffer-name advised)
  "Ensure BINDING in buffer named BUFFER-NAME uses symbol prefixed with PREFIX."
  (if (consp binding)
      (let ((name (symbol-name (cdr binding)))
            (symb (cdr binding)))
        (unless (string-match-p prefix name)
          (setq symb (intern (concat prefix name)))
          (defalias symb (symbol-function (cdr binding))))
        (remote--advise symb buffer-name advised)
        (cons (car binding) symb))
    (error "I don't know what am I doing")))

(defun remote--char-table (keym prefix buffer-name advised)
  "Copy char-table KEYM to new keymap with symbols with PREFIX."
  (let ((new (list 'keymap)))
    (dotimes (i (length keym))
      (let ((tbl (cadr new))
            (sym (aref keym i))
            prefixed-sym)
        (pcase sym
          ('nil)
          ((pred symbolp)
           (unless (string-match-p prefix (symbol-name sym))
             (setq prefixed-sym (intern (concat prefix (symbol-name sym))))
             (defalias prefixed-sym (symbol-function sym)))
           (remote--advise (or prefixed-sym sym) buffer-name advised)
           (aset tbl i (or prefixed-sym sym)))
          ((pred char-table-p)
           (aset tbl i (remote--char-table sym prefix buffer-name advised)))
          ((pred proper-list-p)
           (aset tbl i (remote--gen-map sym prefix buffer-name advised))))))
    (nreverse new)))

(defsubst menu-bar-p (object)
  (and (proper-list-p object) (eq (car object) 'menu-bar)))

(defsubst remapp (object)
  (and (proper-list-p object) (eq (car object) 'remap)))

(defsubst prefix-map-p (object)
  (and (proper-list-p object) (eq (cadr object) 'keymap)))

(defun remote--gen-map (keym prefix buffer-name advised)
  "Return a copy of KEYM keymap.

Prefix symbol names with PREFIX if they don't already start with PREFIX.
Argument BUFFER-NAME Buffer to be remotely controlled.
Argument ADVISED is a list on which to add advised function symbols."
  (let ((kmp (list 'keymap)))
    (dolist (elt keym)
      (cond
       ((char-table-p elt)
        (push (remote--char-table elt prefix buffer-name advised) kmp))
       ((keymapp elt)
        (push (remote--gen-map elt prefix buffer-name advised) kmp))
       ((menu-bar-p elt)
        (push elt kmp)) ;; pass through
       ((remapp elt)
        (push elt kmp))  ;; pass through
       ((prefix-map-p elt) 
        (let (newkmap)
          (dolist (e elt)
            (if (consp e)
                (push (remote--gen-cons e prefix buffer-name advised) newkmap)
              (push e newkmap)))
          (push (nreverse newkmap) kmp)))
       ((consp elt)
        (push (remote--gen-cons elt prefix buffer-name advised) kmp))
       (t (push elt kmp))))
    (nreverse kmp)))

(provide 'buffer-remote)
;;; buffer-remote.el ends here
