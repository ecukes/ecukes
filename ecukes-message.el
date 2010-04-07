;;; ecukes-message.el --- Advicing for message function

(defvar ecukes-message-history '()
  "List of messages that has been produced during the current scenario.")


;; Print the message as usual, except add the message to the list `ecukes-messages'.
(defadvice message (after message-after (format-string &rest args) activate)
  (add-to-list 'ecukes-message-history (apply 'format format-string args) t))
(ad-activate 'message)


(provide 'ecukes-message)

;;; ecukes-message.el ends here
