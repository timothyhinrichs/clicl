;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authorizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass authorizer (agent) ())

(defmethod message-handler (msg sender (receiver authorizer))
  (cond ((atom msg) msg)
        ((eq 'update (car msg)) (modify msg sender receiver))
        ((eq 'tell (car msg)) (modify msg sender receiver))
        ((eq 'untell (car msg)) (modify msg sender receiver))
        (t (request msg sender (find-target receiver)))))

(defmethod save (p (th authorizer) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (save p (find-target (name th))))
        ((eq (car p) '<=) (save p (get-rulebase th)))
        ((eq (car p) '=>) (save p (get-rulebase th)))
        (t (save p (find-target (name th))))))

(defmethod drop (p (th authorizer) &optional (f #'samep))
  (declare (ignore f))
  (cond ((atom p) (drop p (find-target (name th))))
        ((eq (car p) '<=) (drop p (get-rulebase th)))
        ((eq (car p) '=>) (drop p (get-rulebase th)))
        (t (drop p (find-target (name th))))))

(defmethod modify (p sender (receiver authorizer))
  (let (updates target security)
    (setq target (get-recipient receiver))
    (setq security (get-security receiver))
    (setq updates (filter-imports (get-updates p) sender receiver security))
    (when updates (request (cons 'update updates) sender target))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-security (agent)
  (symbol-value (find-security (name agent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
