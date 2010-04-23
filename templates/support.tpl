;; This file is loaded once before any feature is run.


;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.
(let ((current-directory (file-name-directory load-file-name)))
  (setq {PROJECT-NAME}-root-path (expand-file-name ".." current-directory))
  (setq {PROJECT-NAME}-util-path (expand-file-name "util" {PROJECT-NAME}-root-path)))

(add-to-list 'load-path {PROJECT-NAME}-root-path)
(add-to-list 'load-path (expand-file-name "espuds" {PROJECT-NAME}-util-path))
(add-to-list 'load-path (expand-file-name "ert" {PROJECT-NAME}-util-path))

(require '{PROJECT-NAME})
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
