(in-package :ccl)

(defconstant traps::$kDMNotifyDisplayWillSleep 10)

(defvar *sleep-begin-functions* nil)

(defpascal dmnotify-proc (:ptr userdata :signed-integer the-message :ptr notifydata)
  (declare (ignore userdata notifydata))
  (when (eq the-message #$kDMNotifyDisplayWillSleep)
    (dolist (f *sleep-begin-functions*)
      (funcall f)))
  (when (or (eq the-message 11) (eq the-message #$kDMNotifyEvent))
    (get-current-screen-size)
    (get-the-current-window-drag-rect nil)
    (when (eq the-message 11)  ;; added this
      (dolist (f *sleep-wakeup-functions*)
        (funcall f)))))
