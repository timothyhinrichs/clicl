;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General agents
;;; convert to KIF wherever possible and execute with general mechanisms
;;; intellectual value, useful for checking specialized methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; findinstancep, findinstance, findinstances, findoptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod findoptions (attribute structure (source general))
  (computeoptions attribute structure source))

(defun computeoptions (relation structure source)
  (do ((l (cddr structure) (cdr l)) (nl))
      ((null l)
       (setq nl (cons (list relation '?y) nl))
       (setq structure (list* (car structure) (cadr structure) (nreverse nl))))
      (cond ((eq (caar l) relation)
             (setq nl (nreconc nl (cons (list relation '?y) (cdr l))))
             (setq structure (list* (car structure) (cadr structure) nl))
             (return structure))
            (t (setq nl (cons (car l) nl)))))
  (asks '?y (maksand (fastconverter structure)) nil source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; instancep, classify, instance, instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod classify (x (source agent))
  (let (dum)
    (cond ((characterp x) 'character)
          ((stringp x) 'string)
          ((numberp x) 'number)
          ((setq dum (find-classifier (name source)))
           (or (askx '?class (list dum x '?class) nil source) 'thing))
          (t 'thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doublep, itemx, items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod doublep (predicate object (source general))
  (askp (list predicate object) nil source))

(defmethod itemx (predicate (source general))
  (askx '?x (list predicate '?x) nil source))

(defmethod items (predicate (source general))
  (asks '?x (list predicate '?x) nil source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; triplep, object, objects, result, results, options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod triplep (slot object result (source general))
  (askp (list slot object result) nil source))

(defmethod object (slot value (source general))
  (askx '?x (list slot '?x value) nil source))

(defmethod objects (slot value (source general))
  (asks '?x (list slot '?x value) nil source))

(defmethod result (slot object (source general))
  (askx '?y (list slot object '?y) nil source))

(defmethod results (slot object (source general))
  (asks '?y (list slot object '?y) nil source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
