(require 'f)

(defvar ecukes-support-path
  (f-dirname load-file-name))

(defvar ecukes-features-path
  (f-parent ecukes-support-path))

(defvar ecukes-root-path
  (f-parent ecukes-features-path))

(defvar ecukes-project-path
  (f-expand "super-project" ecukes-features-path))

(defvar ecukes-project-features-path
  (f-expand "features" ecukes-project-path))

(defvar ecukes-project-step-definitions-path
  (f-expand "step-definitions" ecukes-project-features-path))

(defvar ecukes-bin-path
  (f-expand "bin" ecukes-root-path))

(defvar ecukes-executable
  (f-expand "ecukes" ecukes-bin-path))

(add-to-list 'load-path ecukes-root-path)

(require 'espuds)
(require 'ert)
(require 's)
(require 'ansi)
(require 'ansi-color)

(Fail
 (unless (s-blank? ecukes-stdout)
   (princ "==================== ECUKES OUTPUT ====================\n")
   (princ ecukes-stdout))
 (unless (s-blank? ecukes-stderr)
   (princ "==================== ECUKES ERROR ====================\n")
   (princ (ansi-red "%s" ecukes-stderr))))
