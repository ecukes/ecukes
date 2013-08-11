(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'ansi)

(defvar ecukes-test/test-path
  (f-dirname load-file-name))

(defvar ecukes-test/root-path
  (f-parent ecukes-test/test-path))

(defvar ecukes-test/fixtures-path
  (f-expand "fixtures" ecukes-test/test-path))

(defvar ecukes-test/vendor-path
  (f-expand "vendor" ecukes-test/root-path))

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" ecukes-test/vendor-path)))

(setq debug-on-entry t)
(setq debug-on-error t)
(setq ecukes-include-tags nil)

(add-to-list 'load-path ecukes-test/root-path)
