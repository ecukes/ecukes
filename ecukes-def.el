;;; ecukes-def.el --- Data structure definitions


;; Complete feature. Each feature file is described with this struct.
;;
;;   intro      - Feature intro
;;   background - Feature background
;;   scenarios  - All feature scenarios
(defstruct ecukes-feature intro background scenarios)


;; Feature intro. Intro is optional, but is conventionally included.
;;
;; header      - One line description of feature
;; description - Conventionally consists of three lines, like this:
;;                 In order ...
;;                 As a ...
;;                 I want ...
;;               This could however consist of fewer or more lines. For example:
;;                 In order ...
;;                 A ... wants ...
(defstruct ecukes-intro header description)


;; Feature background.
;;
;; steps - All steps in background
(defstruct ecukes-background steps)


;; A feature scenario. Each feature has many scenarios.
;;
;; name  - The name of the scenario. For example: Switch to *Messages* buffer
;; steps - All steps for the scenario.
;; tags  - List of all scenario tags
(defstruct ecukes-scenario name steps tags)


;; A step.
;;
;; name - Name of the step. For example: Given I press "C-h e" 
;; type - The type of the step. Can be regular, py-string or table.
;; arg  - If steps is a table or py-string step. arg is either the
;;        py-string or the table. If a regular step, this is nil.
;; err  - When running the step. If it fails, this contains the error message.
(defstruct ecukes-step name arg type err)


;; If step is a table step, its argument will be an ecukes-table.
;;
;; header - A list with the header
;; rows   - A list of lists, where each inner list is a row in the table.
(defstruct ecukes-table header rows)


;; A step definition.
;;
;; fn   - The step definition function
;; args - The argument from the match groupings
(defstruct ecukes-step-def fn args)


(provide 'ecukes-def)

;;; ecukes-def.el ends here
