;;; ecukes-message.el --- Advising for message function

(defvar ecukes-messages '()
  "List of messages that has been produced during the current scenario.")


;; Print the message as usual, except add the message to the list `ecukes-messages'.
(defadvice message (after message-after (format-string &rest args) activate)
  (add-to-list 'ecukes-messages (apply 'format format-string args) t))

;; Do not print anything.
(defadvice message (around message-around (format-string &rest args) activate))

(ad-activate 'message)

(defun ecukes-message-advice (should-advice)
  "Advise `message' to/not produce any ouput."
  (if should-advice
      (ad-enable-advice 'message 'around 'message-around)
    (ad-disable-advice 'message 'around 'message-around))
  (ad-update 'message))
(ecukes-message-advice nil)


(provide 'ecukes-message)

;;; ecukes-message.el ends here
