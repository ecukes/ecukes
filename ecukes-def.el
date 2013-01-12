;;; ecukes-def.el --- Data structure definitions


(defstruct ecukes-feature
  "A feature is the top level structure for a feature file."
  intro background outlines scenarios)

(defstruct ecukes-intro
  "A feature introduction is a description of a feature. It is
optional, but is conventionally included."
  header description)

(defstruct ecukes-background
  "A feature background is a few steps that are run before each scenario."
  steps)

(defstruct ecukes-outline
  "A scenario outline contains examples that are used to generate concrete scenarios."
  name steps tags table)

(defstruct ecukes-scenario
  "A feature scenario is a scenario that is built up by steps."
  name steps tags)

(defstruct ecukes-step
  "A step is some kind of action."
  name head body arg type err)

(defstruct ecukes-step-def
  "A step definition."
  regex fn)


(provide 'ecukes-def)

;;; ecukes-def.el ends here
