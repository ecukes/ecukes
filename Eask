;; -*- mode: eask; lexical-binding: t -*-

(package "ecukes"
         "0.7.0"
         "Cucumber for Emacs")

(website-url "http://github.com/ecukes/ecukes")
(keywords "test")

(package-file "ecukes.el")
(package-descriptor "ecukes-pkg.el")

(files "ecukes-*.el" "templates" "bin" "reporters")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)    ; for cl-lib
(source 'melpa)

(depends-on "emacs" "25")
(depends-on "f" "0.11.0")
(depends-on "s" "1.8.0")
(depends-on "dash" "2.2.0")
(depends-on "ansi" "0.3.0")
(depends-on "espuds" "0.2.2")
(depends-on "commander" "0.6.1")

(development
 (depends-on "el-mock")
 (depends-on "ert-runner")
 (depends-on "ecukes"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
