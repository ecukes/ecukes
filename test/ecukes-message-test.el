(ert-deftest message-add-to-history ()
  (let ((mess "mess..."))
    (quiet-message
     (ecukes-output-message mess))
    (should (member mess ecukes-message-history))))

(ert-deftest message-clear ()
  (quiet-message
   (ecukes-output-message "some mess..."))
  (should ecukes-message-history)
  (ecukes-message-clear)  
  (should-not ecukes-message-history))
