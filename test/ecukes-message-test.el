(ert-deftest message-add-to-history ()
  (let ((mess "add to history"))
    (quiet-message
     (message mess))
    (should (member mess ecukes-message-history))))

(ert-deftest message-clear ()
  (quiet-message
   (message "clear me out"))
  (should ecukes-message-history)
  (ecukes-message-clear)  
  (should-not ecukes-message-history))

(ert-deftest message-no-history ()
  (ecukes-message-clear)
  (quiet-message
   (ecukes-message-no-history
    (message "don't save me")))
  (should-not ecukes-message-history))
