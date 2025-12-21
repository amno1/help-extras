;;; help-extras.el --- A small collection of helpfull utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Keywords: tools, help, convenience, abbrev
;; URL: https://github.com/amno1/help-extras
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4"))

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

;; A small suite of minimalistic utilites, each usable on its own. Together
;; they add few small convenience tools to improve quality of life when using
;; built-in help-mode:

;; help-remote  - A "remote control" for help buffer
;; help-details - Provides more details about symbols (properties, source and
;;                dissassembly), inspired by Helpful
;; buffer-faces - Display list of all faces used in a buffer

;; This file does nothing on its own. Exists to make package manager happy.

;;; Code:

(provide 'help-extras)
;;; help-extras.el ends here
