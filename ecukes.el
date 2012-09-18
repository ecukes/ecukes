#!/usr/bin/env emacs --script
;;; ecukes --- Cucumber for Emacs

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.2.1
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

(setq debug-on-error t)
(setq debug-on-entry t)

(defvar *ecukes-message-log* (list ""))

(defadvice message  (after ecukes-log-messages-to-buffer
                           activate)
  (when ad-return-value
    (setf (cdr (last *ecukes-message-log*))
          (cons ad-return-value nil))))


(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(defvar ecukes-vendor-path
  (expand-file-name "vendor" ecukes-path)
  "Path to ecukes vendor.")

(add-to-list 'load-path ecukes-path)
(add-to-list 'load-path ecukes-vendor-path)

(eval-when-compile
  (require 'cl))

(require 'ansi)
(require 'ecukes-template)
(require 'ecukes-new)
(require 'ecukes-def)
(require 'ecukes-startup)
(require 'ecukes-parse)
(require 'ecukes-steps)
(require 'ecukes-run)
(require 'ecukes-print)
(require 'ecukes-hooks)
(require 'ecukes-stats)

(add-to-list 'command-switch-alist '("--new" . ecukes-new-handler))

(when (ecukes-startup-run-p)

  (defun run-step (step &optional print background)
    "Run STEP, including increasing counters."
    (if previous-step-success
        (cond
         ((ecukes-run-step step)
          (unless background (ecukes-stats-step-pass))
          (if print
              (ecukes-print-step-success step)))
         (t
          (unless background (ecukes-stats-step-fail))
          (setq step-has-failed t)
          (if print
              (ecukes-print-step-failure step))
          (setq previous-step-success nil)))
      (ecukes-stats-step-skip)
      (if print
          (ecukes-print-step-pending step))))

  (let ((feature-files (ecukes-startup-features argv)))
    (cond (feature-files

           (ecukes-startup-load)

           (ecukes-hooks-run-setup)

           (dolist (feature-file feature-files)
             (let* ((feature (ecukes-parse-feature feature-file))
                    (background (ecukes-feature-background feature))
                    (scenarios (ecukes-feature-scenarios feature))
                    (steps
                     (apply
                      'append
                      (if background (ecukes-background-steps background))
                      (mapcar 'ecukes-scenario-steps scenarios)))
                    (undefined (ecukes-steps-undefined steps)))

               (setq step-has-failed nil)
               (setq previous-step-success t)
               (setq background-runned nil)

               (cond (undefined
                      (ecukes-print-undefined-steps undefined))
                     ((let ((intro (ecukes-feature-intro feature)))
                        (ecukes-print-intro intro)

                        (when background
                          (ecukes-print-background-header)
                          (dolist (step (ecukes-background-steps background))
                            (run-step step t))
                          (setq background-runned t))

                        (dolist (scenario scenarios)
                          (ecukes-hooks-run-before)

                          (when background
                            (dolist (step (ecukes-background-steps background))
                              (run-step step nil t)))

                          (ecukes-print-newline)
                          (ecukes-print-scenario-header scenario)
                          (dolist (step (ecukes-scenario-steps scenario))
                            (run-step step t))

                          (if step-has-failed
                              (ecukes-stats-scenario-fail)
                            (ecukes-stats-scenario-pass))

                          (setq previous-step-success t)
                          (setq step-has-failed nil)

                          (ecukes-hooks-run-after)))))))

           (ecukes-hooks-run-teardown)

           (ecukes-stats-print-summary))
          (t
           (ecukes-print-message
            (ansi-red "You did not provide any features to run"))))))

(when (getenv "ECUKES_OUTFILE")
  (with-temp-buffer
    (mapcar (lambda (line)
              (insert line) (insert "\n"))
            *ecukes-message-log*)
    ;; ecukes-tmp-file-target needs to get set from somewhere else
    (write-file (getenv "ECUKES_OUTFILE"))))

(kill-emacs (if (> ecukes-stats-scenarios-failed 0) 1 0))

;;; ecukes.el ends here
