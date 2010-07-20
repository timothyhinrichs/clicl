;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; web interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (command (eql 'spreadsheet)) postlines)
  (cond ((null postlines) (process-spreadsheet-start s))
        (t (http-problem s "Not yet implemented."))))

(defun process-spreadsheet-start (s)
  (output-prolog s 200)
  (format s "<HTML>") (crlf s)
  (format s "<HEAD><TITLE>")
  (format s "Master's Program Spreadsheet")
  (format s "</TITLE></HEAD>")
  (format s "<BODY BGCOLOR=\"WHITE\">") (crlf s)
  (format s "<BR/>")
  (format s "<CENTER><H1>Master's Program Spreadsheet</H1></CENTER>")
  (format s "<FORM ACTION=\"Spreadsheet?\">")
  (format s "<CENTER><TABLE>")
  (format s "<TR><TH>Autumn</TH><TH>Winter</TH><TH>Spring</TH></TR>")
  (format s "<TR><TD ALIGN=CENTER VALIGN=TOP>")
  (output-menu s "Autumn" (computepredicate 'autumn 'data) (computepredicate 'q1 'data))
  (format s "</TD><TD ALIGN=CENTER VALIGN=TOP>")
  (output-menu s "Winter" (computepredicate 'winter 'data) (computepredicate 'q2 'data))
  (format s "</TD><TD ALIGN=CENTER VALIGN=TOP>")
  (output-menu s "Spring" (computepredicate 'spring 'data) (computepredicate 'q3 'data))
  (format s "</TD><TR></TABLE></CENTER>")
  (format s "</FORM>")
  (output-footer s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *spreadsheet* nil)
(defparameter *q1* nil)
(defparameter *q2* nil)
(defparameter *q3* nil)
(defparameter *q4* nil)
(defparameter *q5* nil)
(defparameter *q6* nil)
(defparameter *q7* nil)
(defparameter *q8* nil)
(defparameter *q9* nil)
(defparameter *complete* nil)
(defparameter *consistent* nil)

(defun startspreadsheet ()
  (let (header h1 h2 h3 reset)
    (setq header (make-instance 'static-text-dialog-item
                   :dialog-item-text "Master's Program Spreadsheet"
                   :view-font '("Times" 36 :bold)
                   :view-position (make-point 79 20)))
    (setq h1 (make-instance 'static-text-dialog-item
               :dialog-item-text "Autumn"
               :view-font '("Times" 14 :bold)
               :view-position (make-point 200 100)))
    (setq h2 (make-instance 'static-text-dialog-item
               :dialog-item-text "Winter"
               :view-font '("Times" 14 :bold)
               :view-position (make-point 300 100)))
    (setq h3 (make-instance 'static-text-dialog-item
               :dialog-item-text "Spring"
               :view-font '("Times" 14 :bold)
               :view-position (make-point 400 100)))
    (setq *q1* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'autumn 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 170 120)))
    (setq *q2* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'autumn 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 170 140)))
    (setq *q3* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'autumn 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 170 160)))
    (setq *q4* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'winter 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 270 120)))
    (setq *q5* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'winter 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 270 140)))
    (setq *q6* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'winter 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 270 160)))
    (setq *q7* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'spring 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 370 120)))
    (setq *q8* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'spring 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 370 140)))
    (setq *q9* (make-instance 'pop-up-menu
                 :menu-items (computemenuitems 'spring 'data)
                 :view-size (make-point 100 20)
                 :view-position (make-point 370 160)))
    (setq *complete* (make-instance 'check-box-dialog-item
                       :dialog-item-text "When checked, program is complete."
                       :dialog-item-action 'updatespread
                       :view-position (make-point 180 190)))
    (setq *consistent* (make-instance 'check-box-dialog-item
                         :dialog-item-text "When checked, program is consistent."
                         :check-box-checked-p t
                         :dialog-item-action 'updatespread
                         :view-position (make-point 180 210)))
    (setq reset (make-instance 'button-dialog-item
                  :dialog-item-text "Reset"
                  :dialog-item-action 'resetspreadsheet
                  :view-position (make-point 300 400)))
    (setq *spreadsheet* (make-instance 'window
                          :view-size (make-point 640 480)
                          :view-position (make-point 320 192)
                          :window-title "Master's Program Spreadsheet"))
    (add-subviews *spreadsheet* header h1 h2 h3
                  *q1* *q2* *q3* *q4* *q5* *q6* *q7* *q8* *q9*
                  *complete* *consistent* reset)))

(defun resetspreadsheet (item)
  (declare (ignore item))
  (dolist (view (coerce (view-subviews *spreadsheet*) 'list))
             (when (typep view 'menu)
               (setf (slot-value view 'ccl::default-item) 1)
               (dolist (item (menu-items view)) (set-menu-item-check-mark item nil))
               (menu-disable view)
               (menu-enable view)))
  (updatespreadsheet))

(defun updatespread (item)
  (declare (ignore item))
  (updatespreadsheet))

(defun updatespreadsheet ()
  (updaterelation 'q1 (intern (menu-item-title (selected-item *q1*))))
  (updaterelation 'q2 (intern (menu-item-title (selected-item *q2*))))
  (updaterelation 'q3 (intern (menu-item-title (selected-item *q3*))))
  (updaterelation 'q4 (intern (menu-item-title (selected-item *q4*))))
  (updaterelation 'q5 (intern (menu-item-title (selected-item *q5*))))
  (updaterelation 'q6 (intern (menu-item-title (selected-item *q6*))))
  (updaterelation 'q7 (intern (menu-item-title (selected-item *q7*))))
  (updaterelation 'q8 (intern (menu-item-title (selected-item *q8*))))
  (updaterelation 'q9 (intern (menu-item-title (selected-item *q9*))))
  (updatecompleteness)
  (updateconsistency)
  (reordermenu *q1* 'q1 (intern (menu-item-title (selected-item *q1*))))
  (reordermenu *q2* 'q2 (intern (menu-item-title (selected-item *q2*))))
  (reordermenu *q3* 'q3 (intern (menu-item-title (selected-item *q3*))))
  (reordermenu *q4* 'q4 (intern (menu-item-title (selected-item *q4*))))
  (reordermenu *q5* 'q5 (intern (menu-item-title (selected-item *q5*))))
  (reordermenu *q6* 'q6 (intern (menu-item-title (selected-item *q6*))))
  (reordermenu *q7* 'q7 (intern (menu-item-title (selected-item *q7*))))
  (reordermenu *q8* 'q8 (intern (menu-item-title (selected-item *q8*))))
  (reordermenu *q9* 'q9 (intern (menu-item-title (selected-item *q9*)))))

(defun updaterelation (r x)
  (kill r 'data)
  (unless (or (eq x '||) (eq x '----)) (insert (list r x) 'data)))

(defun updatecompleteness ()
  (do ((l (coerce (view-subviews *spreadsheet*) 'list) (cdr l)))
      ((null l) (check-box-check *complete*))
      (when (and (typep (car l) 'menu) 
                 (or (equalp (menu-item-title (selected-item (car l))) "")
                     (equalp (menu-item-title (selected-item (car l))) "----")))
        (check-box-uncheck *complete*)
        (return nil))))

(defun updateconsistency ()
  (if (fullfindp '(illegal ?x ?y) 'data) (check-box-uncheck *consistent*)
      (check-box-check *consistent*)))

(defun reordermenu (menu rel obj)
  (kill rel 'data)
  (do ((l (menu-items menu) (cdr l)) (pl) (ul) (nl) (ol))
      ((null l)
       (apply #'remove-menu-items menu (menu-items menu))
       (apply #'add-menu-items menu
              (nconc (list (caddr ol)) (nreverse pl)
                     (list (car ol)) (nreverse ul) 
                     (list (cadr ol)) (nreverse nl)))
       (do ((l (menu-items menu) (cdr l)) (i 1 (1+ i)))
           ((null l))
           (when (equalp (menu-item-title (car l)) (symbol-name obj))
             (setf (slot-value menu 'ccl::default-item) i)
             (return t))))
      (cond ((equalp (menu-item-title (car l)) "") (setq ol (cons (car l) ol)))
            ((equalp (menu-item-title (car l)) "----") (setq ol (cons (car l) ol)))
            ((requiredp (list rel (intern (menu-item-title (car l)))) 'data)
             (setq pl (cons (car l) pl)))
            ((forbiddenp (list rel (intern (menu-item-title (car l)))) 'data)
             (setq nl (cons (car l) nl)))
            (t (setq ul (cons (car l) ul)))))
  (unless (or (eq obj '||) (eq obj '----)) (insert (list rel obj) 'data)))

(defun computemenuitems (p th)
  (let (blank upper lower)
    (setq blank (make-menu-item ""))
    (setq upper (make-menu-item "----"))
    (setq lower (make-menu-item "----"))
    (do ((l (computepredicate p th) (cdr l)) (nl))
        ((null l) (cons blank (cons upper (nreverse (cons lower nl)))))
        (setq nl (cons (make-menu-item (symbol-name (car l))) nl)))))

(defun make-menu-item (s)
  (make-instance 'menu-item
    :menu-item-title s
    :menu-item-action 'updatespreadsheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun computepredicate (p th)
  (fullfinds '?x (list p '?x) th))

(defun computeattribute (p th)
  (fullfinds (list p '?x '?y) (list p '?x '?y) th))

(defun requiredp (p th)
  (let (olds news answer)
    (setq olds (computeattribute 'illegal th))
    (insert `(not ,p) th)
    (setq news (computeattribute 'illegal th))
    (setq answer (not (subsetp news olds :test #'equalp)))
    (drop `(not ,p) th)
    answer))

(defun forbiddenp (p th)
  (let (olds news answer)
    (setq olds (computeattribute 'illegal th))
    (insert p th)
    (setq news (computeattribute 'illegal th))
    (setq answer (not (subsetp news olds :test #'equalp)))
    (drop p th)
    answer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory background
  (course cs101)
  (course cs102)
  (course cs103)
  (course cs110)
  (course cs120)
  (course cs130)
  (course cs210)
  (course cs220)
  (course cs230)
  (course cs240)
  (course cs250)
  (course cs260)
  (course cs310)
  (course cs320)
  (course cs330)
  (course cs340)
  (course cs350)
  (course cs360)

  (autumn cs101)
  (autumn cs102)
  (autumn cs103)
  (autumn cs110)
  (autumn cs120)
  (autumn cs130)

  (winter cs101)
  (winter cs102)
  (winter cs103)
  (winter cs110)
  (winter cs120)
  (winter cs130)
  (winter cs210)
  (winter cs220)
  (winter cs230)
  (winter cs240)
  (winter cs250)
  (winter cs260)

  (spring cs101)
  (spring cs102)
  (spring cs103)
  (spring cs110)
  (spring cs120)
  (spring cs130)
  (spring cs310)
  (spring cs320)
  (spring cs330)
  (spring cs340)
  (spring cs350)
  (spring cs360)

  (coreq cs101 cs102)
  (coreq cs101 cs103)
  (coreq cs101 cs201)
  (coreq cs201 cs301))

(deftheory rules
  (<= (q ?c 1) (q1 ?c))
  (<= (q ?c 2) (q2 ?c))
  (<= (q ?c 3) (q3 ?c))
  (<= (q ?c 4) (q4 ?c))
  (<= (q ?c 5) (q5 ?c))
  (<= (q ?c 6) (q6 ?c))
  (<= (q ?c 7) (q7 ?c))
  (<= (q ?c 8) (q8 ?c))
  (<= (q ?c 9) (q9 ?c))

  (<= (not (autumn ?x ?y)) (unprovable (autumn ?x ?y)))
  (<= (not (winter ?x ?y)) (unprovable (winter ?x ?y)))
  (<= (not (spring ?x ?y)) (unprovable (spring ?x ?y)))
  (<= (not (coreq ?x ?y)) (unprovable (coreq ?x ?y))))

(deftheory legality
  (<= (illegal ?c repeated)
      (q ?c ?m)
      (q ?c ?n)
      (distinct ?m ?n)))

(includes 'data 'background)
(includes 'data 'rules)
(includes 'data 'legality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory background
  (autumn cs101)
  (autumn cs102)
  (autumn cs103)
  (autumn cs110)
  (autumn cs120)
  (autumn cs130)
  (autumn cs140)
  (autumn cs150)
  (autumn cs160)
  (autumn cs170)
  (autumn cs180)
  (autumn cs190)

  (winter cs101)
  (winter cs102)
  (winter cs103)
  (winter cs110)
  (winter cs120)
  (winter cs130)
  (winter cs210)
  (winter cs220)
  (winter cs230)
  (winter cs240)
  (winter cs250)
  (winter cs260)

  (spring cs101)
  (spring cs102)
  (spring cs103)
  (spring cs110)
  (spring cs120)
  (spring cs130)
  (spring cs310)
  (spring cs320)
  (spring cs330)
  (spring cs340)
  (spring cs350)
  (spring cs360)

  (coreq cs101 cs102)
  (coreq cs101 cs103)
  (coreq cs101 cs201)
  (coreq cs201 cs301))

(deftheory rules
  (<= (not (autumn ?x ?y)) (unprovable (autumn ?x ?y)))
  (<= (not (winter ?x ?y)) (unprovable (winter ?x ?y)))
  (<= (not (spring ?x ?y)) (unprovable (spring ?x ?y)))
  (<= (not (coreq ?x ?y)) (unprovable (coreq ?x ?y))))

(deftheory legality
  (<= (illegal (q1 ?c))
      (illegal (q ?c 1)))

  (<= (illegal (q2 ?c))
      (illegal (q ?c 2)))

  (<= (illegal (q3 ?c))
      (illegal (q ?c 3)))

  (<= (illegal (q4 ?c))
      (illegal (q ?c 4)))

  (<= (illegal (q5 ?c))
      (illegal (q ?c 5)))

  (<= (illegal (q6 ?c))
      (illegal (q ?c 6)))

  (<= (illegal (q7 ?c))
      (illegal (q ?c 7)))

  (<= (illegal (q8 ?c))
      (illegal (q ?c 8)))

  (<= (illegal (q9 ?c))
      (illegal (q ?c 9)))

  (<= (illegal (q ?c ?m))
      (q ?c ?n)
      (distinct ?m ?n)))

(includes 'data 'background)
(includes 'data 'rules)
(includes 'data 'legality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; old
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun computevalues (r p th)
  (let (values pos neg)
    (setq values (computepredicate p th))
    (setq pos (computepredicate r th))
    (setq values (set-difference values pos))
    (setq neg (finds '?x `(and (oneof ?x . ,values) (not ,(list r '?x))) th))
    (setq values (set-difference values neg))
    (values pos values neg)))

(defun computevalues2 (r p th)
  (let (old values pos neg)
    (setq old (computepredicate r th))
    (dolist (x old) (drop (list r x) th))
    (setq values (computepredicate p th))
    (setq pos (computepredicate r th))
    (setq values (set-difference values pos))
    (setq neg (finds '?x `(and (oneof ?x . ,values) (not ,(list r '?x))) th))
    (setq values (set-difference values neg))
    (dolist (x old) (insert (list r x) th))
    (values pos values neg)))

(defun computevalues3 (r p th)
  (let (values pos neg)
    (setq values (computepredicate p th))
    (dolist (x values)
      (drop (list r x) th)
      (cond ((posp (list r x) th) (setq neg (cons x neg)))
            ((negp (list r x) th) (setq pos (cons x pos))))
      (insert (list r x) th))
    (setq values (set-difference (set-difference values pos) neg))
    (values pos values neg)))

(defun computevalues4 (r p th)
  (let (values pos neg unk)
    (setq values (computepredicate p th))
    (dolist (x values)
      (cond ((goodp (list r x) th) (setq pos (cons x pos)))
            ((badp (list r x) th) (setq neg (cons x neg)))
            (t (setq unk (cons x unk)))))
    (values pos unk neg)))

(defun computevalues5 (r p th)
  (let (values oldpos pos neg unk)
    (setq values (computepredicate p th))
    (setq oldpos (computepredicate r th))
    (dolist (x oldpos) (drop (list r x) th))
    (dolist (x values)
      (cond ((requiredp (list r x) th) (setq pos (cons x pos)))
            ((forbiddenp (list r x) th) (setq neg (cons x neg)))
            (t (setq unk (cons x unk)))))
    (dolist (x oldpos) (insert (list r x) th))
    (values (nreverse pos) (nreverse unk) (nreverse neg))))

(defun oldspreadsheet ()
    (setq editor (make-instance 'editable-text-dialog-item))
    (setq checker (make-instance 'check-box-dialog-item))
    (setq sequencer (make-instance 'sequence-dialog-item
                      :dialog-item-text "Foo"
                      :table-sequence (computepredicate 'q1 'data)
                      :selection-type :disjoint
                      :cell-size (make-point 48 12))
    (cell-select sequencer 0 1)))

(defun goodp (p th)
  (let (flag olds news answer)
    (setq flag (knownp p th))
    (when flag (drop p th))
    (setq olds (computeattribute 'illegal th))
    (insert `(not ,p) th)
    (setq news (computeattribute 'illegal th))
    (setq answer (not (subsetp news olds :test #'equalp)))
    (drop `(not ,p) th)
    (when flag (insert p th))
    answer))

(defun badp (p th)
  (let (flag olds news answer)
    (setq flag (knownp p th))
    (when flag (drop p th))
    (setq olds (computeattribute 'illegal th))
    (insert p th)
    (setq news (computeattribute 'illegal th))
    (setq answer (not (subsetp news olds :test #'equalp)))
    (drop p th)
    (when flag (insert p th))
    answer))

(defun posp (p th)
  (prog2  (insert `(pos ,p) th)
          (not (null (cdr (finds '?p `(pos ?p) th))))
          (drop `(pos ,p) th)))

(defun negp (p th)
  (prog2 (insert `(neg ,p) th)
         (not (null (finds '?p `(neg ?p) th)))
         (drop `(neg ,p) th)))

(deftheory data
  (course cs101)
  (course cs102)
  (course cs103)
  (course cs110)
  (course cs120)
  (course cs130)
  (course cs210)
  (course cs220)
  (course cs230)
  (course cs240)
  (course cs250)
  (course cs260)
  (course cs310)
  (course cs320)
  (course cs330)
  (course cs340)
  (course cs350)
  (course cs360)

  (autumn cs101)
  (autumn cs102)
  (autumn cs103)
  (autumn cs110)
  (autumn cs120)
  (autumn cs130)

  (winter cs101)
  (winter cs102)
  (winter cs103)
  (winter cs110)
  (winter cs120)
  (winter cs130)
  (winter cs210)
  (winter cs220)
  (winter cs230)
  (winter cs240)
  (winter cs250)
  (winter cs260)

  (spring cs101)
  (spring cs102)
  (spring cs103)
  (spring cs110)
  (spring cs120)
  (spring cs130)
  (spring cs310)
  (spring cs320)
  (spring cs330)
  (spring cs340)
  (spring cs350)
  (spring cs360)

  (coreq cs101 cs102)
  (coreq cs101 cs103)
  (coreq cs101 cs201)
  (coreq cs201 cs301))

(deftheory rules
  (<= (not (autumn ?x ?y)) (unprovable (autumn ?x ?y)))
  (<= (not (winter ?x ?y)) (unprovable (winter ?x ?y)))
  (<= (not (spring ?x ?y)) (unprovable (spring ?x ?y)))
  (<= (not (coreq ?x ?y)) (unprovable (coreq ?x ?y))))

(deftheory legality
  (<= (illegal q1 toomany)
      (q1 ?c1)
      (q1 ?c2)
      (q1 ?c3)
      (q1 ?c4)
      (distinct ?c1 ?c2)
      (distinct ?c1 ?c3)
      (distinct ?c1 ?c4)
      (distinct ?c2 ?c3)
      (distinct ?c2 ?c4)
      (distinct ?c3 ?c4))

  (<= (illegal q2 toomany)
      (q2 ?c1)
      (q2 ?c2)
      (q2 ?c3)
      (q2 ?c4)
      (distinct ?c1 ?c2)
      (distinct ?c1 ?c3)
      (distinct ?c1 ?c4)
      (distinct ?c2 ?c3)
      (distinct ?c2 ?c4)
      (distinct ?c3 ?c4))

  (<= (illegal q3 toomany)
      (q3 ?c1)
      (q3 ?c2)
      (q3 ?c3)
      (q3 ?c4)
      (distinct ?c1 ?c2)
      (distinct ?c1 ?c3)
      (distinct ?c1 ?c4)
      (distinct ?c2 ?c3)
      (distinct ?c2 ?c4)
      (distinct ?c3 ?c4))

  (<= (illegal ?c repeated)
      (q1 ?c)
      (q2 ?c))

  (<= (illegal ?c repeated)
      (q1 ?c)
      (q3 ?c))

  (<= (illegal ?c repeated)
      (q2 ?c)
      (q3 ?c)))

(deftheory consistency
  (<= (not (q1 ?c))
      (q1 ?c1)
      (q1 ?c2)
      (q1 ?c3)
      (distinct ?c1 ?c2)
      (distinct ?c2 ?c3)
      (distinct ?c1 ?c3)
      (distinct ?c ?c1)
      (distinct ?c ?c2)
      (distinct ?c ?c3))

  (<= (not (q2 ?c))
      (q2 ?c1)
      (q2 ?c2)
      (q2 ?c3)
      (distinct ?c1 ?c2)
      (distinct ?c2 ?c3)
      (distinct ?c1 ?c3)
      (distinct ?c ?c1)
      (distinct ?c ?c2)
      (distinct ?c ?c3))

  (<= (not (q3 ?c))
      (q3 ?c1)
      (q3 ?c2)
      (q3 ?c3)
      (distinct ?c1 ?c2)
      (distinct ?c2 ?c3)
      (distinct ?c1 ?c3)
      (distinct ?c ?c1)
      (distinct ?c ?c2)
      (distinct ?c ?c3))

  (<= (not (q1 ?c))
      (q2 ?c))

  (<= (not (q1 ?c))
      (q3 ?c))

  (<= (not (q2 ?c))
      (q1 ?c))

  (<= (not (q2 ?c))
      (q3 ?c))

  (<= (not (q3 ?c))
      (q1 ?c))

  (<= (not (q1 ?c))
      (q2 ?c)))

(defthoeory old
  (offered cs210 winter)
  (offered cs220 winter)
  (offered cs230 winter)
  (offered cs240 winter)
  (offered cs250 winter)
  (offered cs260 winter)
  (offered cs310 spring)
  (offered cs320 spring)
  (offered cs330 spring)
  (offered cs340 spring)
  (offered cs350 spring)
  (offered cs360 spring))

(includes 'data 'rules)
(includes 'data 'legality)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
