;;; ecukes-message.el --- Advice for message function

(defvar ecukes-message-history '()
  "List of messages that has been produced during the current scenario.")


;; Print the message as usual, except add the message to the list `ecukes-messages'.
(defadvice message (after message-add-history (format-string &rest args) activate)
  (add-to-list 'ecukes-message-history (apply 'format format-string args) t))
(ad-activate 'message)


(defmacro ecukes-message-no-history (&rest body)
  "Turns off the history advice, evaluates BODY and then turns it back on.
Useful to print a message without storing it to the history."
  `(progn
     (ad-disable-advice 'message 'after 'message-add-history)
     (ad-update 'message)
     ,@body
     (ad-enable-advice 'message 'after 'message-add-history)
     (ad-update 'message)))

(defun ecukes-message-clear ()
  "Clears the message history list."
  (setq ecukes-message-history '()))


(provide 'ecukes-message)

;;; ecukes-message.el ends here
