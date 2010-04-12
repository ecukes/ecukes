;;; ecukes-message.el --- Advice for message function

(defvar ecukes-message-history '()
  "List of messages that has been produced during the current scenario.")


(defun ecukes-message-disable-advice ()
  "Disables the message history advice."
  (ad-disable-advice 'message 'after 'message-add-history)
  (ad-update 'message))

(defun ecukes-message-enable-advice ()
  "Enables the message history advice."
  (ad-enable-advice 'message 'after 'message-add-history)
  (ad-activate 'message)
  (ad-update 'message))

(defmacro ecukes-message-no-history (&rest body)
  "Turns off the history advice, evaluates BODY and then turns it back
on. Useful to print a message without storing it to the history."
  `(progn
     (ecukes-message-disable-advice)
     ,@body
     (ecukes-message-enable-advice)))

(defun ecukes-message-clear ()
  "Clears the message history list."
  (setq ecukes-message-history '()))


;; Print the message as usual, except add the message to the list `ecukes-messages'.
(defadvice message (after message-add-history (format-string &rest args) activate)
  (add-to-list 'ecukes-message-history (apply 'format format-string args) t))
(ecukes-message-enable-advice)


(provide 'ecukes-message)

;;; ecukes-message.el ends here
