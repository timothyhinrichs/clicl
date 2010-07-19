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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; catalog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *catalog* nil)

(defparameter *courses*
  '("Course" "Alternative" "Corequisite"
    cs001 "" ""
    cs002 "" ""
    cs003 "" ""
    cs110 "CS120" "CS210, CS310"
    cs120 "CS110" ""
    cs130 "" ""
    cs140 "" ""
    cs150 "" ""
    cs160 "" ""
    cs210 "" "CS110, CS310"
    cs220 "" ""
    cs230 "" ""
    cs240 "" ""
    cs250 "" ""
    cs260 "" ""
    cs310 "" "CS110, CS210"
    cs320 "" ""
    cs330 "" ""
    cs340 "" ""
    cs350 "" ""
    cs360 "" ""))

(defun startcatalog ()
  (let (header table)
    (setq header (make-instance 'static-text-dialog-item
                   :dialog-item-text "Master's Program Catalog"
                   :view-font '("Times" 36 :bold)
                   :view-position (make-point 113 20)))
    (setq table (make-instance 'sequence-dialog-item
                  :table-sequence *courses*
                  :sequence-order :horizontal
                  :sequence-wrap-length 3
                  :view-position (make-point 160 120)))
    (set-cell-size table (make-point 100 11))
    (set-cell-font table (make-point 0 0) '("Monaco" 9 :srcor :bold))
    (set-cell-font table (make-point 1 0) '("Monaco" 9 :srcor :bold))
    (set-cell-font table (make-point 2 0) '("Monaco" 9 :srcor :bold))
    (setq *catalog* (make-instance 'window
                      :view-size (make-point 640 480)
                      :view-position (make-point 340 212)
                      :window-title "Master's Program Catalog"))
    (add-subviews *catalog* header table)))

(defun showcatalog (item)
  (declare (ignore item))
  (when (typep *catalog* 'window) (window-close *catalog*))
  (startcatalog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; spreadsheet
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
(defparameter *show* nil)
(defparameter *submit* nil)
(defparameter *reset* nil)

(defun startspreadsheet ()
  (let (header h1 h2 h3 rules)
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
    (setq *reset* (make-instance 'button-dialog-item
                    :dialog-item-text "Reset"
                    :dialog-item-action 'resetspreadsheet
                    :view-position (make-point 190 250)))
    (setq *show* (make-instance 'button-dialog-item
                     :dialog-item-text "Catalog"
                     :dialog-item-action 'showcatalog
                     :view-position (make-point 285 250)))
    (setq *submit* (make-instance 'button-dialog-item
                     :dialog-item-text "Submit"
                     :view-position (make-point 385 250)))
    (setq rules (make-instance 'static-text-dialog-item
               :dialog-item-text "Academic Rules:
1. A student may not take a course twice.
2. If a course has an alternative, a student may take either course but not both.
3. If a course has a corequisite, a student must take both courses or neither.

System Rules:
1. If a selector is colored red, the selection is inconsistent with some other selection.
2. If a selector is colored blue, it means the selection is incomplete.
3. A program may be submitted only when it is complete and consistent."
               :view-position (make-point 60 300)))
    (setq *spreadsheet* (make-instance 'window
                          :view-size (make-point 640 480)
                          :view-position (make-point 320 192)
                          :window-title "Master's Program Spreadsheet"))
    (add-subviews *spreadsheet* header h1 h2 h3
                  *q1* *q2* *q3* *q4* *q5* *q6* *q7* *q8* *q9*
                  *complete* *consistent* *reset* *show* *submit* rules)
    (updatespreadsheet)))

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
  (showstatus *q1* 'q1)
  (showstatus *q2* 'q2)
  (showstatus *q3* 'q3)
  (showstatus *q4* 'q4)
  (showstatus *q5* 'q5)
  (showstatus *q6* 'q6)
  (showstatus *q7* 'q7)
  (showstatus *q8* 'q8)
  (showstatus *q9* 'q9)
  (updatecompleteness)
  (updateconsistency)
  (updatesubmission)
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
  (if (fullfindp '(and (illegal ?p) ?p) 'data) (check-box-uncheck *consistent*)
      (check-box-check *consistent*)))

(defun updatesubmission ()
  (if (and (check-box-checked-p *complete*) (check-box-checked-p *consistent*))
      (dialog-item-enable *submit*)
      (dialog-item-disable *submit*)))

(defun reordermenu (menu rel obj)
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
      (set-menu-item-check-mark (car l) nil)
      (cond ((equalp (menu-item-title (car l)) "") (setq ol (cons (car l) ol)))
            ((equalp (menu-item-title (car l)) "----") (setq ol (cons (car l) ol)))
            ((requiredp (list rel (intern (menu-item-title (car l)))) 'data)
             (setq pl (cons (car l) pl)))
            ((forbiddenp (list rel (intern (menu-item-title (car l)))) 'data)
             (setq nl (cons (car l) nl)))
            (t (setq ul (cons (car l) ul))))))

(defun showstatus (menu rel)
  (let (item)
    (setq item (intern (menu-item-title (selected-item menu))))
    (cond ((or (eq item '||) (eq item '----))
           (set-part-color menu :menu-body *light-blue-color*))
          ((fullfindp `(illegal ,(list rel item)) 'data)
           (set-part-color menu :menu-body *pink-color*))
          (t (set-part-color menu :menu-body *white-color*)))))

(defun hidestatus (menu)
  (set-part-color menu :menu-body *white-color*))

(defparameter *interactive* nil)

(defmethod view-mouse-enter-event-handler ((view pop-up-menu))
  (let (rel item supports)
    (setq rel (getrelation view))
    (setq item (intern (menu-item-title (selected-item view))))
    (cond ((not *interactive*))
          ((or (eq item '||) (eq item '----)))
          ((setq supports (fullsupports `(illegal ,(list rel item)) 'data))
           (set-part-color view :menu-body *red-color*)
           (showpartner 'q1 supports *q1*)
           (showpartner 'q2 supports *q2*)
           (showpartner 'q3 supports *q3*)
           (showpartner 'q4 supports *q4*)
           (showpartner 'q5 supports *q5*)
           (showpartner 'q6 supports *q6*)
           (showpartner 'q7 supports *q7*)
           (showpartner 'q8 supports *q8*)
           (showpartner 'q9 supports *q9*)))))

(defmethod view-mouse-leave-event-handler ((view pop-up-menu))
  (cond ((not *interactive*))
        (t (showstatus *q1* 'q1)
           (showstatus *q2* 'q2)
           (showstatus *q3* 'q3)
           (showstatus *q4* 'q4)
           (showstatus *q5* 'q5)
           (showstatus *q6* 'q6)
           (showstatus *q7* 'q7)
           (showstatus *q8* 'q8)
           (showstatus *q9* 'q9))))

(defun getrelation (view)
  (cond ((eq view *q1*) 'q1)
        ((eq view *q2*) 'q2)
        ((eq view *q3*) 'q3)
        ((eq view *q4*) 'q4)
        ((eq view *q5*) 'q5)
        ((eq view *q6*) 'q6)
        ((eq view *q7*) 'q7)
        ((eq view *q8*) 'q8)
        ((eq view *q9*) 'q9)))

(defun showpartner (rel supports view)
  (when (amongp rel supports #'eq)
    (set-part-color view :menu-body *red-color*)))

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
  (fullfindp `(illegal (not ,p)) th))

(defun forbiddenp (p th)
  (fullfindp `(illegal ,p) th))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftheory background
  (autumn cs001)
  (autumn cs002)
  (autumn cs003)
  (autumn cs110)
  (autumn cs120)
  (autumn cs130)
  (autumn cs140)
  (autumn cs150)
  (autumn cs160)

  (winter cs001)
  (winter cs002)
  (winter cs003)
  (winter cs210)
  (winter cs220)
  (winter cs230)
  (winter cs240)
  (winter cs250)
  (winter cs260)

  (spring cs001)
  (spring cs002)
  (spring cs003)
  (spring cs310)
  (spring cs320)
  (spring cs330)
  (spring cs340)
  (spring cs350)
  (spring cs360)

  (coreq cs110 cs210)
  (coreq cs110 cs310)
  (coreq cs210 cs110)
  (coreq cs210 cs310)
  (coreq cs310 cs110)
  (coreq cs310 cs210)

  (alternative cs210 cs310)
  (alternative cs220 cs320)
  (alternative cs230 cs330)
  (alternative cs240 cs340)
  (alternative cs250 cs350)
  (alternative cs260 cs360))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
