;;; ecukes-def.el --- Data structure definitions


;; A feature, which is the top level structure for a feature file.
;;
;;   intro      - The feature introduction if any.
;;   background - The feature background if any.
;;   scenarios  - A list of all scenarios in the feature.
(defstruct ecukes-feature intro background scenarios)


;; Feature introduction. Introduction is optional, but is conventionally included.
;;
;; header      - One line description of feature.
;; description - Conventionally consists of three lines, like this:
;;                 In order ...
;;                 As a ...
;;                 I want ...
;;               It could however consist of fewer or more lines. For example:
;;                 In order ...
;;                 A ... wants ...
;;
;;               The description is a list where each item is a line.
(defstruct ecukes-intro header description)


;; Feature background, which a feature optionally can include.
;;
;; steps - A list of all steps in the background.
(defstruct ecukes-background steps)


;; A feature scenario. Each feature can consist of many scenarios.
;;
;; name  - The name of the scenario. For example: Switch to *Messages* buffer.
;; steps - A list of all steps in the scenario.
;; tags  - A list of all tags in the scenario.
(defstruct ecukes-scenario name steps tags)


;; A step found either in a background or scenario.
;;
;; name - Name of the step. For example: Given I press "C-h e".
;; type - The type of the step. Can be 'regular, 'py-string or 'table.
;; arg  - If steps is a table or py-string step. arg is either the
;;        py-string or the table. If a regular step, this is nil.
;; err  - When running the step. If it fails, this contains the error message.
(defstruct ecukes-step name arg type err)


;; If step is a table step, its argument will be an ecukes-table.
;;
;; header - A list with the table header.
;; rows   - A list of lists, where each inner list is a row in the table.
(defstruct ecukes-table header rows)


;; A step definition.
;;
;; fn   - The step definition function. Can be either a lambda or a symbol.
;; args - The argument from the regular expression match groupings.
(defstruct ecukes-step-def fn args)


(provide 'ecukes-def)

;;; ecukes-def.el ends here
