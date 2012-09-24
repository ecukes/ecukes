;;; ecukes --- Cucumber for Emacs

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>, Dave Paroulek <upgradingdave@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.2.0
;; Keywords: testing, cucumber
;; URL: http://github.com/rejeep/ecukes

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README

;;; Code:

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(defvar ecukes-vendor-path
  (expand-file-name "vendor" ecukes-path)
  "Path to ecukes vendor.")

(add-to-list 'load-path ecukes-path)
(add-to-list 'load-path ecukes-vendor-path)

(require 'ecukes-setup)
(require 'ansi-color)

(defun ecukes ()
  (interactive)
  (progn
    (setq *ecukes-message-log* (list ""))
    (ecukes-run-default)
    (switch-to-buffer (get-buffer-create "*ecukes-output-buffer*"))
    (erase-buffer)
    (mapcar (lambda (line)
              (insert (ansi-color-apply line))
              (insert "\n"))
            *ecukes-message-log*)
    (font-lock-mode t)))

(provide 'ecukes)
