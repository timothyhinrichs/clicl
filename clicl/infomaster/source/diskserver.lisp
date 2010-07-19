;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diskserver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that diskservers store lists only, not general dotted pairs.
;;;
;;; Currently read only.
;;; Writing individual facts is slow anyway.
;;; Use diskify to transfer from other agents.
;;;
;;; Eventual write strategy: Put positive differentialagent on the front.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass diskserver (dataserver)
  ((memarray :accessor memarray :initarg :memarray :initform nil)
   (atomfile :accessor atomfile :initarg :atomfile :initform "diskserver.atm")
   (listfile :accessor listfile :initarg :listfile :initform "diskserver.lst")
   (stream   :accessor stream   :initarg :stream   :initform nil)))

(defmethod initialize-instance ((x diskserver) &rest args)
  (apply #'call-next-method x args)
  (unless (probe-file (listfile x)) (filesetbyte 0 (listfile x) 0))
  (save `(indexing ,(name x) fullindexing) *manager*)
  (save `(inference ,(name x) dataserver) *manager*)
  (setf (memarray x) (make-array '(1000000)))
  (diskreadatoms x))



(defmethod findx (x p (th diskserver))
  (with-open-file (fs (listfile th) :direction :input
                      :element-type '(unsigned-byte 32))
    (setf (stream th) fs)
    (datafindx x p th)))

(defmethod finds (x p (th diskserver))
  (with-open-file (fs (listfile th) :direction :input
                      :element-type '(unsigned-byte 32))
    (setf (stream th) fs)
    (datafinds x p th)))



(defmethod setindexees (x y (th diskserver))
  (let (fs)
    (cond ((and (streamp (setq fs (stream th))) (open-stream-p fs))
           (rplacd (chkatom x th) (diskwrite y th fs)))
          (t (rplacd (chkatom x th) (filewrite y th (listfile th)))))
    x))

(defmethod clearindex (x (th diskserver))
  (when (setq x (getproperty x th)) (rplacd x 0))
  x)

(defmethod indexees (x (th diskserver))
  (let (fs)
    (cond ((not (setq x (getproperty x th))) nil)
          ((and (streamp (setq fs (stream th))) (open-stream-p fs))
           (diskread (cdr x) th fs))
          (t (fileread (cdr x) th (listfile th))))))



(defmethod empty ((th diskserver))
  (setf (svref (memarray th) 0) 0)
  (delete-file (listfile th))
  'done)


(defun atom-to-index (x th)
  (car (get x th)))

(defun atom-to-ptr (x th)
  (+ #b1000000000000000000000000 (atom-to-index x th)))   ; 16777216

(defun index-to-atom (index th)
  (svref (memarray th) index))

(defun ptr-to-atom (ptr th)
  (index-to-atom (strip ptr) th))

(defun strip (code)
  (lsh (lsh code 8) -8))

(defun chkatom (x th)
  (let (index)
    (cond ((get x th))
          (t (setq index (1+ (aref (memarray th) 0)))
             (setf (aref (memarray th) 0) index)
             (setf (aref (memarray th) index) x)
             (setf (get x th) (cons index 0))))))


(defun diskreadatoms (th)
  (with-open-file (fs (atomfile th) :direction :input :if-does-not-exist :create)
    (do ((x (read fs nil nil) (read fs nil nil)) (i 1 (1+ i))
         (memory (memarray th)))
        ((null x) (setf (svref memory 0) (1- i)))
        (setf (get x th) (cons i (read fs nil 0)))
        (setf (svref memory i) x))))

(defun diskwriteatoms (th)
  (with-open-file (fs (atomfile th) :direction :output
                      :if-exists :supersede :if-does-not-exist :create)
    (do* ((i 1 (1+ i)) (memory (memarray th)) (n (svref memory 0)))
         ((> i n) 'done)
         (prin1 (svref memory i) fs)
         (princ " " fs)
         (prin1 (or (cdr (get (svref memory i) th)) 0) fs) 
         (terpri fs))))


(defun diskread (ptr th fs)
  (cond ((= ptr 0) nil)
        ((>= ptr 16777216) (ptr-to-atom ptr th))
        (t (file-position fs ptr)
           (do ((left) (nl))
               (nil)
               (setq left (read-byte fs))
               (setq ptr (read-byte fs))
               (setq nl (cons (diskread left th fs) nl))
               (when (= ptr 0) (return (nreverse nl)))
               (unless (or (= left 0) (>= left 16777216))
                 (file-position fs ptr))))))

(defun diskwrite (x th fs)
  (cond ((null x) 0)
        ((atom x) (chkatom x th) (atom-to-ptr x th))
        (t (diskwritelist x th fs))))

(defun diskwritelist (x th fs)
  (let (size vector)
    (setq size (* (conses x) 2))
    (setq vector (make-array (list (1+ size)) :element-type 'integer))
    (setf (svref vector 0) 1)
    (vectorize x th vector)
    (diskwritevector vector fs)))

(defun vectorize (x th vector)
  (cond ((null x) 0)
        ((atom x) (chkatom x th) (atom-to-ptr x th))
        (t (let (head)
             (setq head (svref vector 0))
             (do ((l x (cdr l)) (tail head) (left) (right))
                 ((null l))
                 (setf (svref vector 0) (+ tail 2))
                 (setq left (vectorize (car l) th vector))
                 (setq right (if (null (cdr l)) 0 (svref vector 0)))
                 (setf (svref vector tail) left)
                 (setf (svref vector (1+ tail)) right)
                 (setf (svref vector 0) (+ tail 2))
                 (setq tail right))
             head))))   

(defun diskwritevector (vector fs)
  (let (here)
    (setq here (file-length fs))
    (file-position fs here)
    (do ((i 1 (1+ i)) (n (length vector)) (item))
        ((= i n) here)
        (setq item (svref vector i))
        (unless (or (= item 0) (>= item 16777216)) (setq item (+ item here -1)))
        (write-byte item fs))))

(defun diskwriteold (x th fs)
  (cond ((null x) 0)
        ((atom x) (chkatom x th) (atom-to-ptr x th))
        (t (let (head)
             (setq head (file-length fs))
             (do ((l x (cdr l)) (tail head) (left) (right))
                 ((null l))
                 (file-position fs tail)
                 (write-byte 0 fs)
                 (write-byte 0 fs)
                 (setq left (diskwrite (car l) th fs))
                 (setq right (if (null (cdr l)) 0 (file-position fs)))
                 (file-position fs tail)
                 (write-byte left fs)
                 (write-byte right fs)
                 (setq tail right))
             head))))


(defun fileread (pos th listfile)
  (with-open-file (fs listfile :direction :input
                      :element-type '(unsigned-byte 32))
    (diskread pos th fs)))

(defun filewrite (x th listfile)
  (with-open-file (fs listfile :direction :output
                      :element-type '(unsigned-byte 32)
                      :if-exists :append :if-does-not-exist :create)
    (diskwrite x th fs)))

(defun filelength (listfile)
  (with-open-file (fs listfile :direction :input
                      :element-type '(unsigned-byte 32))
      (file-length fs)))

(defun filegetbyte (pos listfile)
  (with-open-file (fs listfile :direction :input
                      :element-type '(unsigned-byte 32))
    (file-position fs pos)
    (read-byte fs)))

(defun filesetbyte (pos listfile ptr)
  (with-open-file (fs listfile :direction :output
                      :element-type '(unsigned-byte 32)
                      :if-exists :append :if-does-not-exist :create)
    (file-position fs pos)
    (write-byte ptr fs)))

(defun filegetbytes (beg end listfile)
  (with-open-file (fs listfile :direction :input
                      :element-type '(unsigned-byte 32))
      (file-position fs beg)
      (do ((i beg (1+ i)) (nl))
          ((> i end) (nreverse nl))
          (setq nl (cons (read-byte fs) nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Disk Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diskify (source target)
  (with-open-file (fs (listfile target) :direction :output
                      :element-type '(unsigned-byte 32) :if-exists :append)
    (setf (stream target) fs)
    (dolist (word (words source))
      (setindexees word (indexees word source) target)))
  (diskwriteatoms target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
