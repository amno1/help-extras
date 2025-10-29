;;; help-display-sources.el --- Display source code for items in help buffer  -*- lexical-binding: t; -*-

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

;;; Code:

;; silence byte-compiler
(defgroup help nil
  "This adds to built-in help-mode so we put all defcustoms in same group."
  :prefix "helper-"
  :group 'help)

(eval-when-compile
  (require 'help-fns)
  (require 'help-mode)
  (require 'find-func))

(eval-when-compile
  (require 'help-extras "help-extras.el"))

;;; Advised functions

(defun helper--describe-symbol (fn &rest args)
  (apply fn args)
  (plist-put help-mode--current-data :caller 'describe-symbol)
  (when (bound-and-true-p help-display-source-mode)
    (goto-char (point-max))
    (let ((src (helper--source-fn)))
      (when src (insert "\n" src)))))

(defun helper--describe-function (fn &rest args)
  (apply fn args)
  (with-help-buffer
    (when (bound-and-true-p help-display-source-mode)
      (plist-put help-mode--current-data :caller 'describe-function)
      (goto-char (point-max))
      (let ((src (helper--source-fn)))
        (when src (insert "\n" src))))))

(defun helper--describe-variable (fn &rest args)
  (apply fn args)
  (with-help-buffer
   (when (bound-and-true-p help-display-source-mode)
     (plist-put help-mode--current-data :caller 'describe-variable)
     (goto-char (point-max))
      (let ((src (helper--source-fn)))
        (when src (insert "\n" src))))))

;;; Fetchers
(defun helper--fetch-c-src (symbol type file)
  (let (src beg)
    (setq file (expand-file-name file source-directory))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (delay-mode-hooks (funcall 'c-mode))
        (goto-char (point-min))
        (unless type
          ;; Either or both an alias and its target might be advised.
          (setq symbol (find-function-advised-original
                        (indirect-function
                         (find-function-advised-original symbol)))))
        (when (re-search-forward
               (if type
                   (concat "DEFVAR[A-Z_]*[ \t\n]*([ \t\n]*\""
                           (regexp-quote (symbol-name symbol))
                           "\"")
                 (concat "DEFUN[ \t\n]*([ \t\n]*\""
                         (regexp-quote (subr-name (advice--cd*r symbol)))
                         "\""))
               nil t)
          (if type
              (and (re-search-backward "DEFVAR" nil t)
                   (setq beg (point))
                   (re-search-forward ");$" nil t))
            (and (re-search-backward "DEFUN" nil t)
                 (setq beg (point))
                 (re-search-forward ")[\n\s\t\r]*{")
                 (re-search-forward "^}[\n\s\t\r]+")))
          (narrow-to-region beg (point))
          (with-no-warnings (font-lock-fontify-buffer))
          (setq src (buffer-string)))))
    src))

(defun helper--fetch-lisp-src (symbol type file)
  (let (src pos)
    (when file
      (setq file (or file (find-lisp-object-file-name symbol type)))
      (with-temp-buffer
        (insert-file-contents file)
        (delay-mode-hooks (funcall 'emacs-lisp-mode))
        (setq pos (cdr (find-function-search-for-symbol symbol type file)))
        (when pos
          (goto-char pos)
          (forward-sexp)
          (narrow-to-region pos (point))
          (with-no-warnings (font-lock-fontify-buffer))
          (setq src (buffer-string)))))
    src))

(defun helper--source-fn ()
  (let* ((file (plist-get help-mode--current-data :file))
         (type (plist-get help-mode--current-data :type))
         (sym (plist-get help-mode--current-data :symbol))
         (src (cond
               ((eq file 'C-source)
                (let ((src (find-function-library sym)))
                  (when src
                    (helper--fetch-c-src sym type (cdr src)))))
               ((or (string-suffix-p ".el" file)
                    (string-suffix-p ".elc" file))
                (helper--fetch-lisp-src sym type file))
               ((string-suffix-p ".c" file)
                (helper--fetch-c-src sym type file))
               (t
                "Source code not available."))))
    (if src
        (format "\n%s\n" src)
      "")))

;;; Minor mode

(define-minor-mode helper--internal-mode
  "Extensions for built-in help-mode."
  :global nil :lighter nil
  (cond
   (helper--internal-mode
    (advice-add 'describe-symbol :around #'helper--describe-symbol)
    (advice-add 'describe-function :around #'helper--describe-function)
    (advice-add 'describe-variable :around #'helper--describe-variable))
   (t (advice-remove 'describe-symbol #'helper--describe-symbol)
      (advice-remove 'describe-function #'helper--describe-function)
      (advice-remove 'describe-variable #'helper--describe-variable))))

;;;###autoload
(define-globalized-minor-mode help-display-source-mode
  helper--internal-mode (lambda () (helper--internal-mode)))

(provide 'help-display-sources)
;;; help-display-sources.el ends here
