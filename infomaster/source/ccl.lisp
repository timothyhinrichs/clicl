(in-package :ccl)

(defpascal dmnotify-proc
           (:ptr userdata :signed-integer the-message :ptr notifydata)
  (declare (ignore userdata notifydata))
   ;; 11 on wake from sleep - include that too?
   ;  kDMNotifyDisplayDidWake = 11, /* Mac OS X only */ - not in our headers
  (when (or (eq the-message 11) (eq the-message #$kDMNotifyEvent))
    (get-current-screen-size)
    (get-the-current-window-drag-rect nil)
    (when (eq the-message 11)  ;; << added this
      (dolist (f *sleep-wakeup-functions*)
        (funcall f)))))

(pushnew #'opentransport-cleanup *sleep-wakeup-functions*
         :key #'function-name :test #'eq)
