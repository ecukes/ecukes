(require 'f)

(defvar ecukes-support-path
  (f-dirname load-file-name))

(defvar ecukes-features-path
  (f-parent ecukes-support-path))

(defvar ecukes-root-path
  (f-parent ecukes-features-path))

(add-to-list 'load-path ecukes-root-path)

(require 'espuds)
(require 'ert)
