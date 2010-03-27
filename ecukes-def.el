;;; ecukes-def.el --- Data structure definitions

(defstruct ecukes-feature intro background scenarios)
(defstruct ecukes-intro header description)
(defstruct ecukes-background steps)
(defstruct ecukes-scenario name steps)
(defstruct ecukes-step name arg type)

(provide 'ecukes-def)

;;; ecukes-def.el ends here
