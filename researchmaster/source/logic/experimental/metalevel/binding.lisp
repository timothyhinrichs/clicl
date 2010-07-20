(in-package :tlh)

;;;;;;;;; Binding lists ;;;;;;;;;;

(defstruct bindinglist 
  (thelist nil)
  (tail nil)
)
(defun new-bindinglist (&optional (bl nil))
  (if bl
    (make-bindinglist :thelist bl :tail (last bl))
    (let ((l (list (cons t t))))
      (make-bindinglist :thelist l :tail (last l))))
)
(defun bindinglist-enqueue (bl item)

  ; if haven't yet initialized tail, do so now
  (cond ((and (not (bindinglist-tail bl)) (bindinglist-thelist bl))
         (setf (bindinglist-tail bl) (last (bindinglist-thelist bl)))
         (setf (cdr (bindinglist-tail bl)) (cons item nil))
         (setf (bindinglist-tail bl) (last (bindinglist-tail bl))))

        ; if thelist is empty, reset everything
        ((null (bindinglist-thelist bl))
         (setf (bindinglist-thelist bl) (cons item nil))
         (setf (bindinglist-tail bl) (last (bindinglist-thelist bl))))

        ; otherwise, just update
        (t
         (setf (cdr (bindinglist-tail bl)) (cons item nil))
         (setf (bindinglist-tail bl) (last (bindinglist-tail bl)))) )
  bl
)

(defstruct metaunifier
  (mgu (new-bindinglist))
  (disjunction nil)
)
(defun empty-metaunifier ()
  (make-metaunifier :mgu (new-bindinglist))
)
(defun simple-bind (var val)
  (make-metaunifier :mgu (bind var val (new-bindinglist)))
)



(defun bind (var val bl)
   "(BIND VAR VAL BL) binds VAR to VAL in the binding list BL."
   (setf (bindinglist-thelist bl) 
         (cons (cons var val) (bindinglist-thelist bl)))
   bl
)
(defun plug (p bl)
   "(PLUG P BL) returns the result of applying the binding list
    BL to the expression P."
    (cond ((eq (type-of bl) 'environment) (common-lisp-user::plugstdexp p bl))  ; in case using Mike's unifier
          ((eq (type-of bl) 'bindinglist) (plugit p bl))
          ((eq (type-of bl) 'cons) (plugit p (make-bindinglist :thelist bl)))
          (t (assert nil nil "Plug only works on environments and bindinglists")) )
)

(defun plugit (p bl)
   "(PLUG P BL) returns the result of applying the binding list
    BL to the expression P when BL is a bindinglist environment."
   (cond ((null bl) p)
         ((qvarp p) (bound-value p bl))
         ((varp p) (bound-value p bl))
         ((atom p) p)
         (t
          (do ((ps p (cdr ps))
               (newp nil))
              ((null ps) (nreverse newp))
            (setq newp (cons (plug (car ps) bl) newp)) )))
)

(defun plug-listd (p bl)
  "(PLUG-LISTD P BL) destructively alters P, which must be a list so that it is the
   result of applying BL to P."
  (assert (listp p) nil "plugd takes a list and a bindinglist.~%")

  (do ((ps p (cdr ps)))
      ((null ps))
    (cond ((qvarp (car ps)) (setf (car ps) (bound-value (car ps) bl)))
          ((varp (car ps)) (setf (car ps) (bound-value (car ps) bl)))
          ((atom (car ps)) nil) ; noop
          (t  ; list, so recurse
           (plug-listd (car ps) bl))))
)

(defun composed (sigma tau)
  "(COMPOSED SIGMA TAU) returns a new unifier where the substitution TAU has been applied 
   to the range of SIGMA.  Both SIGMA and TAU are bindinglists. Destructively alters 
   SIGMA."

  ; destructively bind range of sigma according to tau
  (mapcar #'(lambda (binding) 
              (if (listp (cdr binding))
                (plug-listd (cdr binding) tau)
                (setf (cdr binding) (bound-value (cdr binding) tau))))
          (bindinglist-thelist sigma))

  ; destructively append new bindings to sigma
  (dolist (b (bindinglist-thelist tau))
    (when (not (find (car b) (bindinglist-thelist sigma) :key #'car))
      (bindinglist-enqueue sigma b)))

  sigma
)
(defun compose (sigma tau)
  (cond ((eq (type-of sigma) 'bindinglist)
         (if (eq (type-of tau) 'bindinglist)
           (composeit sigma tau)
           (composeit sigma (make-bindinglist :thelist tau))))
        ((eq (type-of tau) 'bindinglist)
         (composeit (make-bindinglist :thelist sigma) tau))
        (t (assert nil nil "Compose doesn't know what to do.")))
)
(defun composeit (sigma tau)
  "(COMPOSE SIGMA TAU) returns a new unifier where the substitution TAU has been applied 
   to the range of SIGMA.  Both SIGMA and TAU are bindinglists.  "

  (cond ((not tau) nil)
        (t 
         (make-bindinglist 
          :thelist 
          (remove 'nil
                  (append (mapcar #'(lambda (binding) (cons (car binding) (plug (cdr binding) tau))) (bindinglist-thelist sigma))
                          (mapcar #'(lambda (newbinding) 
                                      (if (find (car newbinding) (bindinglist-thelist sigma) :key #'car) nil newbinding))
                                  (bindinglist-thelist tau))) ))))
)
#| Old version
(defun compose (bl tau)
   "(COMPOSE BL TAU) composes the two binding lists in the order
    given and returns the result."
   (cond ((or (not bl) (not tau)) nil)
         (t
   (setq bl (bindinglist-thelist bl)) 
   (let ((new nil) (new2 nil))
      (do ((bs bl (cdr bs)))
          ((null bs))
        ;(format t "~A~%" bs)
        (setq new (cons (cons (caar bs) (plug (cdar bs) tau)) new)))
      (setq tau (bindinglist-thelist tau))
      (do ((ts tau (cdr ts)))
          ((null ts))
        (when (not (member (caar ts) new :key #'car ))
              (setq new (cons (car ts) new))) )
      (do ((ns new (cdr ns)))
          ((null ns) new2)
        (when (not (eq (caar ns) (cdar ns)))
          (setq new2 (cons (car ns) new2)))) 
      (make-bindinglist :thelist new2)) ))
)
|#

(defun bound-value (p bl)
   "(BOUND-VALUE P BL) returns the first value assigned p in the
    binding list BL."
    (let ((val (assoc p (bindinglist-thelist bl) :test #'equal)))
       (if val (cdr val) p))  ; return val unless nil, in which case return p.  
)


;;;;;;; A variant ;;;;;;;;

#|
(defun bind (var val)
  "(BIND VAR VAL) returns a binding list for VAR to VAL."
  (list (cons var val))
)
(defun plug (exp al)
  "(PLUG EXP AL) applies the association list to EXP."
  (cond ((varp exp) (if (assoc exp al) (cdr (assoc exp al)) exp))
        ((listp exp) (mapcar #'(lambda (subexp) (plug subexp al)) exp))
        (t exp))
)
(defun compose (sigma tau)
  "(COMPOSE SIGMA TAU) returns a new unifier where the substitution TAU has been applied 
   to the range of SIGMA."
  (remove 'nil
          (append (mapcar #'(lambda (binding) (cons (car binding) (plug (cdr binding) tau))) sigma)
                  (mapcar #'(lambda (newbinding) 
                              (if (find (car newbinding) sigma :key #'car) nil newbinding))
                          tau)) )
)
|#