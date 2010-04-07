;;; ecukes-message.el --- Advice for message function

(defvar ecukes-message-history '()
  "List of messages that has been produced during the current scenario.")


;; Print the message as usual, except add the message to the list `ecukes-messages'.
(defadvice message (after message-after (format-string &rest args) activate)
  (add-to-list 'ecukes-message-history (apply 'format format-string args) t))
(ad-activate 'message)


(defun ecukes-message-clear ()
  "Clears the message history list."
  (setq ecukes-message-history '()))


(provide 'ecukes-message)

;;; ecukes-message.el ends here
