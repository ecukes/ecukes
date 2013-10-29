(require 'f)

(defvar ecukes-support-path
  (f-dirname load-file-name))

(defvar ecukes-features-path
  (f-parent ecukes-support-path))

(defvar ecukes-root-path
  (f-parent ecukes-features-path))

(defvar ecukes-vendor-path
  (f-expand "vendor" ecukes-root-path))

(defvar ecukes-projects-path)

(defvar ecukes-project-path)

(defvar ecukes-project-features-path)

(defvar ecukes-project-step-definitions-path)

(defvar ecukes-bin-path
  (f-expand "bin" ecukes-root-path))

(defvar ecukes-executable
  (f-expand "ecukes" ecukes-bin-path))

(defvar ecukes-stderr)
(defvar ecukes-stdout)

(add-to-list 'load-path ecukes-root-path)

(require 'espuds)
(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" ecukes-vendor-path)))
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

(Before
 (setq ecukes-stderr "")
 (setq ecukes-stdout "")

 (setq ecukes-projects-path (f-expand "projects" ecukes-features-path))
 (setq ecukes-project-path (f-expand "super-project" ecukes-projects-path))
 (setq ecukes-project-features-path (f-expand "features" ecukes-project-path))
 (setq ecukes-project-step-definitions-path (f-expand "step-definitions" ecukes-project-features-path))
 
 (let ((new-path (f-expand "new" ecukes-projects-path)))
   (when (f-dir? new-path)
     (f-delete new-path 'force)))

 (-each
  (f-files
   ecukes-project-features-path
   (lambda (file)
     (s-matches? ".feature$" file))
   :recursive)
  'f-delete)

 (-each
  (f-files
   ecukes-project-features-path
   (lambda (file)
     (equal (f-ext file) "elc"))
   'recursive)
  'f-delete)

 (-each
  '(".ecukes" ".ecukes-failing-scenarios")
  (lambda (file)
    (let ((path (f-expand file ecukes-project-path)))
      (when (f-file? path) (f-delete path))))))
