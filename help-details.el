;;; help-details.el --- Display source code for items in help buffer  -*- lexical-binding: t; -*-

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


;;; Private

(defgroup help nil
  "This adds to built-in help-mode so we put all defcustoms in same group."
  :prefix "helper-"
  :group 'help)

(require 'disass)
(require 'help-fns)
(require 'help-mode)
(require 'find-func)
(require 'text-property-search)

(defvar helper--post-insert-function)

(defmacro with-help-buffer (&rest body)
  "Execuate BODY in the context of `help-buffer'."
  (declare (indent 0) (debug t))
  `(let ((help-buffer (help-buffer)))
     (when (get-buffer-window help-buffer)
       (with-current-buffer help-buffer
         (with-silent-modifications
           (save-excursion ,@body))))))

;;; Advised functions
(defun helper--describe-symbol (fn &rest args)
  "Advice for `describe-symbol'"
  (apply fn args)
  (plist-put help-mode--current-data :caller 'describe-symbol)
  (funcall helper--post-insert-function))

(defun helper--describe-function (fn &rest args)
  "Advice for `describe-function'"
  (apply fn args)
  (plist-put help-mode--current-data :caller 'describe-function)
  (funcall helper--post-insert-function))

(defun helper--describe-variable (fn &rest args)
  "Advice for `describe-variable'"
  (apply fn args)
  (plist-put help-mode--current-data :caller 'describe-variable)
  (funcall helper--post-insert-function))

;;; Fetchers
(defun helper--fetch-c-src (symbol type file)
  "Fetch source code for SYMBOL from a C file FILE.

TEYPE is either \\\\='function or \\\\='variable"
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
  "Fetch source code for SYMBOL from a Lisp file FILE.

TEYPE is either \\\\='function or \\\\='variable"
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

(defun helper--source-function ()
  "Fetch sources for a current symbol in `help-buffer'"
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
                (helper--fetch-c-src sym type file)))))
    (if src
        (format "\n%s\n" src)
      "Source code not available.")))

(defun helper--display-source-function ()
  "Inserts fetched source code for current symbol into `help-buffer'"
  (with-help-buffer
    (let ((src (helper--source-function)))
      (when src
        (goto-char (point-min))
        (if (text-property-search-forward 'helper-data)
            (delete-region (1- (point)) (point-max))
          (goto-char (point-max)))
        (insert (propertize "\n" 'helper-data (point)) src)))))

(defun helper--display-properties-function ()
  "Fetch and insert symbol properties for the currrent symbol in `help-buffer'"
  (with-help-buffer
    (let ((props (symbol-plist (plist-get help-mode--current-data :symbol))))
      (when props
        (goto-char (point-min))
        (if (text-property-search-forward 'helper-data)
            (delete-region (1- (point)) (point-max))
          (goto-char (point-max)))
        (insert (propertize "\n" 'helper-data (point)))
        (while props
          (insert (format "%s\n  %S\n"
                          (propertize (symbol-name (car props))
                                      'face 'font-lock-constant-face)
                          (cadr props)))
          (setq props (cddr props)))))))

(defun helper--dissassembly-function ()
  "Dissasemble the current symbol in `help-buffer'"
  (with-temp-buffer
    (let ((sym (with-help-buffer
                 (plist-get help-mode--current-data :symbol))))
      (if (and (consp sym) (not (functionp sym)))
          (setq sym `(lambda () ,sym)))
      (disassemble-internal sym 0 nil)
      (delay-mode-hooks (funcall 'asm-mode))
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-string)))

(defun helper--display-dissassembly-function ()
  "Display dissasembly for the current symbol in `help-buffer'"
  (with-help-buffer
    (let ((src (helper--dissassembly-function)))
      (when src
        (goto-char (point-min))
        (if (text-property-search-forward 'helper-data)
            (delete-region (1- (point)) (point-max))
          (goto-char (point-max)))
        (insert (propertize "\n" 'helper-data (point)) src)))))

;;; Minor mode
(define-minor-mode helper--internal-mode
  "Extensions for built-in help-mode."
  :global nil :lighter nil
  (cond
   (helper--internal-mode
    (advice-add 'describe-symbol :around #'helper--describe-symbol)
    (advice-add 'describe-function :around #'helper--describe-function)
    (advice-add 'describe-variable :around #'helper--describe-variable)
    (setf helper--post-insert-function #'helper--display-source-function))
   (t (advice-remove 'describe-symbol #'helper--describe-symbol)
      (advice-remove 'describe-function #'helper--describe-function)
      (advice-remove 'describe-variable #'helper--describe-variable)
      (makunbound 'helper--post-insert-function))))

;;;###autoload
(define-globalized-minor-mode help-details-mode
  helper--internal-mode (lambda () (helper--internal-mode)))


;; Commands

;;;###autoload
(defun help-display-properties ()
  "Display property list for the symbol currently visible in `help-mode'"
  (interactive)
  (helper--display-properties-function))

;;;###autoload
(defun help-display-dissassembly ()
  "Display dissassembly for the symbol currently visible in `help-mode'"
  (interactive)
  (helper--display-dissassembly-function))

;;;###autoload
(defun help-display-source ()
  "Display source-code for the symbol currently visible in `help-mode'"
  (interactive)
  (helper--display-source-function))

(provide 'help-details)
;;; help-details.el ends here
