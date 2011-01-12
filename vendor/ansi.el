;;; ansi.el --- Turn string into ansi strings

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: color, ansi
;; URL: http://github.com/rejeep/ansi

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

;; This package defines functions that turn string into ansi colored
;; strings.

;; You can paint strings (see `ansi-colors' for all possible text
;; colors).
;;
;;   (ansi-red "foo")
;;   (ansi-black "bar")
;;
;;
;; You can paint a string backgrounds (see `ansi-on-colors' for all
;; possible background colors).
;;
;;   (ansi-on-blue "foo")
;;   (ansi-on-green "bar")
;;
;;
;; You can add styles to a string (see `ansi-styles' for all possible
;; styles).
;;
;;   (ansi-bold "foo")
;;   (ansi-blink "bar")
;;
;;
;; You can use `with-ansi', which allows for a simplified DSL.
;;
;;   (with-ansi
;;    (red "foo")
;;    (black "bar"))
;;
;;   (with-ansi
;;    (on-blue "foo")
;;    (on-green "bar"))
;;
;;   (with-ansi
;;    (bold "foo")
;;    (blink "bar")
;;    ;; Note that reverse is not available within `with-ansi' because
;;    ;; it conflicts with the builtin function for reversing a
;;    ;; list. However, `ansi-reverse' is still available.
;;    (contrary "foobar"))
;;
;;
;; If you want to add multiple effects on a single string, you can use
;; nesting:
;;
;;   (ansi-bold
;;    (ansi-red "foo"))
;;
;;   (with-ansi
;;    (bold
;;     (red "foo")))


;;; Code:

(eval-when-compile
  (require 'cl))


(defconst ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst ansi-on-colors
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defconst ansi-reset 0
  "Ansi code for reset.")


(defmacro with-ansi (&rest body)
  "Allows using shortcut names of coloring functions."
  `(flet
       ,(mapcar
         (lambda (alias)
           (let ((fn (intern (concat "ansi-" (symbol-name alias)))))
             `(,alias (string) (,fn string))))
         (append
          (mapcar 'car ansi-colors)
          (mapcar 'car ansi-on-colors)
          (mapcar 'car ansi-styles)))
     ,(cons 'ansi-concat body)))

(defun ansi-concat (&rest sequences)
  "Like `concat' but concats only the string values from SEQUENCES."
  (let ((strings (remove-if-not 'stringp sequences)))
    (apply 'concat strings)))

(defun ansi-color (string color)
  "Paint STRING with COLOR."
  (ansi-effect ansi-colors string color))

(defun ansi-on-color (string color)
  "Paint STRING on COLOR."
  (ansi-effect ansi-on-colors string color))

(defun ansi-style (string style)
  "Style STRING with STYLE."
  (ansi-effect ansi-styles string style))

(defun ansi-effect (list string effect)
  "Add EFFECT to string."
  (let ((code (cdr (assoc effect list))))
    (format "\e[%sm%s\e[%sm" code string ansi-reset)))


;; COLORS

(defun ansi-black (string)
  (ansi-color string 'black))

(defun ansi-red (string)
  (ansi-color string 'red))

(defun ansi-green (string)
  (ansi-color string 'green))

(defun ansi-yellow (string)
  (ansi-color string 'yellow))

(defun ansi-blue (string)
  (ansi-color string 'blue))

(defun ansi-magenta (string)
  (ansi-color string 'magenta))

(defun ansi-cyan (string)
  (ansi-color string 'cyan))

(defun ansi-white (string)
  (ansi-color string 'white))


;; ON COLORS

(defun ansi-on-black (string)
  (ansi-on-color string 'on-black))

(defun ansi-on-black (string)
  (ansi-on-color string 'on-black))

(defun ansi-on-red (string)
  (ansi-on-color string 'on-red))

(defun ansi-on-green (string)
  (ansi-on-color string 'on-green))

(defun ansi-on-yellow (string)
  (ansi-on-color string 'on-yellow))

(defun ansi-on-blue (string)
  (ansi-on-color string 'on-blue))

(defun ansi-on-magenta (string)
  (ansi-on-color string 'on-magenta))

(defun ansi-on-cyan (string)
  (ansi-on-color string 'on-cyan))

(defun ansi-on-white (string)
  (ansi-on-color string 'on-white))


;; STYLES

(defun ansi-bold (string)
  (ansi-style string 'bold))

(defun ansi-dark (string)
  (ansi-style string 'dark))

(defun ansi-italic (string)
  (ansi-style string 'italic))

(defun ansi-underscore (string)
  (ansi-style string 'underscore))

(defun ansi-blink (string)
  (ansi-style string 'blink))

(defun ansi-rapid (string)
  (ansi-style string 'rapid))

(defun ansi-contrary (string)
  (ansi-style string 'contrary))
(defalias 'ansi-reverse 'ansi-contrary)

(defun ansi-concealed (string)
  (ansi-style string 'concealed))

(defun ansi-strike (string)
  (ansi-style string 'strike))


(provide 'ansi)

;;; ansi.el ends here
