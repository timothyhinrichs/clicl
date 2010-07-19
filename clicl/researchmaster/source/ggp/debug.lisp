
(defun gameroles (theory)
  (gdlfinds '?p '(role ?p) theory))

(defun legaljoints (actions theory)
    (every #'(lambda (x y) (legalp x y theory)) (gameroles theory) actions))


(defun generate-moves (theory movestyle)
  (let (roles (actions nil))
    (setq roles (gameroles theory))
    (do ((n roles (cdr n)))
        ((null n) actions)
      (cond ((equal movestyle 'legal)
             (setq actions (cons (legalx (car n) theory) actions)))
            ((equal movestyle 'random)
             (setq actions (cons (legalrx (car n) theory) actions)))
            (t
             (setq actions (cons (legalx (car n) theory) actions)))))
    (nreverse actions)))


;;;;;;;;;;;; general debugging ;;;;;;;;;;;;;;

(defvar *generic-debug-stylesheet* "../style/generic-debug.xsl")
(defstruct debugstruct history states xsl game)

(defmethod process (s (file (eql 'debug)) postlines)
  (cond ((null postlines) (output-display-debug s))
        ((getf-post "rules" postlines) (process-debug-axioms s postlines))
        (t (http-problem s "Bad request."))))

(defun output-display-debug (s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Debug GDL</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style/main.css\">~%")
  ;(format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style/jack.css\">~%")
  ;(format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-debug s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-debug (s)
  (format s "<form ACTION=\"debug?\" METHOD=\"POST\" NAME=\"form1\">
             <div id=\"upload\">
             <INPUT TYPE=\"SUBMIT\" VALUE=\"Upload these axioms\"/>
             <INPUT TYPE=\"HIDDEN\" NAME=\"matchid\" VALUE=\"~A\"/>" 
          (tosymbol (stringappend "debug." (string (gentemp)))))
  (format s "<br/><TEXTAREA NAME=\"rules\" ROWS=\"40\" COLS=\"100\" WRAP=\"soft\"></TEXTAREA></div></form>"))

(defun process-debug-axioms (s postlines)
  (let (rules matchid matchobj)
    (setq matchid (read-symbol (getf-post "matchid" postlines)))
    (setq rules (read-sentences (getf-post "rules" postlines)))

    ; make a new matchobj if necessary
    (cond ((not (boundp matchid))
           (setq matchobj (apply #'makematch matchid rules 0 0 nil)))
          (t
           (setq matchobj (symbol-value matchid)) 
           (empty (theory matchobj))))

      ; reset the game
      (setf (theory matchobj) (define-theory (make-instance 'theory) "" rules))
      (resetgame matchobj)
      (setf (debugstruct-xsl (slot matchobj)) "../style/generic-debug.xsl")
      (output-debug-match-xml s matchobj)))

(defun resetgame (matchobj)
  (setf (data matchobj) (initials (theory matchobj)))
  (setf (herstory matchobj) (list (strip (data matchobj))))
  (setf (history matchobj) nil)
  (setf (slot matchobj) (make-debugstruct :states (make-hash-table) :history nil :xsl nil))
  (setf (gethash 0 (debugstruct-states (slot matchobj))) (data matchobj)))


;;;;;;;;; stopped here



(defmethod process (s (file (eql 'debugreset)) postlines)
  (let* ((matchobj (symbol-value (read-user-string (getf-post "matchid" postlines))))
         (xsl (debugstruct-xsl (slot matchobj))))
    ; reset the game
    (resetgame matchobj)
    (setf (debugstruct-xsl (slot matchobj)) xsl)
    (output-debug-match-xml s matchobj)))

(defmethod process (s (file (eql 'debugwalk)) postlines)
  (let (movestyle state moves illegal matchid matchobj)
    (setq matchid (read-user-string (getf-post "matchid" postlines)))
    (setq matchobj (symbol-value matchid))
    (setq movestyle (read-user-string (getf-post "Kind" postlines)))
    
    ; compute the current state and store it in the history if new state
    (cond ((find movestyle '(goals freeze terminal))
           (setq state (contents (data matchobj))))
          ((eq movestyle 'manual)
           (setq moves (read-user-string (getf-post "Moves" postlines)))
           (cond ((legaljoints moves matchobj)
                  (setf (actions matchobj) (mapcar #'(lambda (x y) `(does ,x ,y)) (gameroles matchobj) moves))
                  (setq state (simulate matchobj))
                  (setf (gethash (1+ (length (debugstruct-history (slot matchobj)))) (debugstruct-states (slot matchobj))) state)
                  (define-theory (data matchobj) "" state))
                 (t
                  (setq illegal t)
                  (setq state (contents (data matchobj))))))
;          ((data matchobj)
;           (setf (gethash (1+ (length (debugstruct-history (slot matchobj)))) (debugstruct-states matchobj)) state))
          (t  ; make a move for the player
           (setq moves (generate-moves matchobj movestyle))
           (setf (actions matchobj) (mapcar #'(lambda (x y) `(does ,x ,y)) (gameroles matchobj) moves))
           (setq state (simulate matchobj))
           (define-theory (data matchobj) "" state)
           (setf (gethash (1+ (length (debugstruct-history (slot matchobj)))) (debugstruct-states (slot matchobj))) state)))

;           (setq matchobj (debug-init-game postlines))
;           (setq state (contents (data matchobj)))))

    ; add the current moves to the history
    (when (and moves (not illegal))
      (setf (debugstruct-history (slot matchobj)) (nreverse (cons moves (nreverse (debugstruct-history (slot matchobj)))))))

    ; show user results: embed a particular trace in the state output
    (cond ((eq movestyle 'goals) (output-debug-match-xml s matchobj #'trace-goals))
          ((eq movestyle 'terminal) (output-debug-match-xml s matchobj #'trace-terminal))
          (illegal (output-debug-match-xml s matchobj #'(lambda (x y) (trace-legals x y moves))))
          (t (output-debug-match-xml s matchobj)))))

;(output-theory-xml s (data matchobj) state moves (debugstruct-history (slot matchobj)) (debugstruct-xsl (slot matchobj)) (name matchobj)))))

(defmethod process (s (file (eql 'debugbackup)) postlines)
  (let (state step matchobj moves)
    (setq step (read-user-string (getf-post "Step" postlines)))
    (setq matchobj (symbol-value (read-user-string (getf-post "matchid" postlines))))
    (setq step (1- step))
    (setq state (gethash step (debugstruct-states (slot matchobj))))
    (define-theory (data matchobj) "" state)
    (setq moves (car (last (debugstruct-history (slot matchobj)))))
    (setf (debugstruct-history (slot matchobj)) (subseq (debugstruct-history (slot matchobj)) 0 step))
    (output-debug-match-xml s matchobj #'(lambda (x y) (trace-legals x y moves)))))

(defmethod process (s (file (eql 'uploadaxioms)) postlines)
  (setq postlines (read-user-string (getf-post "matchid" postlines)))
  (output-prolog s 200)
  (output-header s) ; "Upload Axioms")
  (format s "
<form ACTION=\"addaxioms?\" METHOD=\"POST\" NAME=\"form1\">
<div id=\"upload\">
<INPUT TYPE=\"SUBMIT\" VALUE=\"Upload these axioms\"/>
<INPUT TYPE=\"HIDDEN\" NAME=\"matchid\" VALUE=\"~A\"/>
<SELECT NAME=\"stylesheet\">" (read-from-string (concatenate 'string "debug." (string (gentemp)))))
  (dolist (twolist (viewfinds '(?x ?y) '(game.debug-stylesheet ?x ?y) *repository*))
    (format s "<OPTION~A>~A</OPTION>" (if (equal (debugstruct-xsl (slot (symbol-value postlines))) (cadr twolist))
                                        " SELECTED" "") (car twolist)))
  (format s "</SELECT>
<br/>
<TEXTAREA NAME=\"rules\" ROWS=\"40\" COLS=\"100\" WRAP=\"soft\">~A</TEXTAREA>
</div></form>" (pretty-print-s (contents (theory (symbol-value postlines)))))
 (output-footer s))


(defmethod process (s (file (eql 'addaxioms)) postlines)
  (let ((rules nil) (responses) (matchid (read-user-string (getf-post "matchid" postlines))))
    (setq rules (getf-post "Rules" postlines))
    (setq rules (substitute #\Newline #\Linefeed rules))
    (setq rules (get-list-from-post "Rules" (list (cons "Rules" rules))))

    #|(setq rules (with-input-from-string (f rules)
                       (do ((a (read f nil) (read f nil)) (nl))
	                 ((null a) (nreverse nl))
                         (setq nl (cons a nl))))) |#
    ; check whether the axioms are well-formed
    (setq responses (game-responses rules))
    (cond ((> (+ (local-response-count responses)
                 (global-response-count responses))
              0)
           (output-gdl-responses nil responses nil s))
          (t
           (let ((xsl (read-user-string (getf-post "Stylesheet" postlines))) (matchobj))
             ; make a new matchobj if necessary
             (cond ((not (boundp matchid))
                    (setq matchobj (apply #'makematch matchid rules 0 0 nil)))
                   (t
                    (setq matchobj (symbol-value matchid))
                   
                    ; clean up the last theory as long as it is not the real axioms
                    (unless (eq (theory matchobj) (debugstruct-game (slot matchobj)))
                      (empty (theory matchobj)))))
             ; reset the game
             (setf (theory matchobj) (define-theory (make-instance 'theory) "" rules))
             (resetgame matchobj)
             (setf (debugstruct-xsl (slot matchobj)) (viewfindx '?x `(game.debug-stylesheet ,xsl ?x) *repository*))
             (output-debug-match-xml s matchobj))))))

(defmethod process (s (file (eql 'addrawaxioms)) postlines)
  (let ((rules nil) (matchid (read-user-string (getf-post "matchid" postlines))))
    (setq rules (getf-post "Rules" postlines))
    (setq rules (substitute #\Newline #\Linefeed rules))
    (setq rules (get-list-from-post "Rules" (list (cons "Rules" rules))))

    #|(setq rules (with-input-from-string (f rules)
                       (do ((a (read f nil) (read f nil)) (nl))
	                 ((null a) (nreverse nl))
                         (setq nl (cons a nl))))) |#
    ; check whether the axioms are well-formed
    (let ((xsl (read-user-string (getf-post "Stylesheet" postlines))) (matchobj))
      ; make a new matchobj if necessary
      (cond ((not (boundp matchid))
             (setq matchobj (apply #'makematch matchid rules 0 0 nil)))
            (t
             (setq matchobj (symbol-value matchid))
             
             ; clean up the last theory as long as it is not the real axioms
             (unless (eq (theory matchobj) (debugstruct-game (slot matchobj)))
               (empty (theory matchobj)))))
      ; reset the game
      (setf (theory matchobj) (define-theory (make-instance 'theory) "" rules))
      (resetgame matchobj)
      (setf (debugstruct-xsl (slot matchobj)) (viewfindx '?x `(game.debug-stylesheet ,xsl ?x) *repository*))
      (output-debug-match-xml s matchobj))))

(defmethod process (s (file (eql 'standardgame)) postlines)
  (let ((matchobj (symbol-value (read-user-string (getf-post "matchid" postlines)))))

    ; clean up the last theory as long as it is not the real axioms
    (unless (eq (theory matchobj) (symbol-value (debugstruct-game (slot matchobj))))
      (empty (theory matchobj)))
    (setf (theory matchobj) (symbol-value (debugstruct-game (slot matchobj))))
    (process s 'debugreset postlines)))

(defmethod process (s (file (eql 'explain)) postlines)
  (let (fact matchobj oldstate moves)
    (setq matchobj (symbol-value (read-user-string (getf-post "matchid" postlines))))
    (setq fact (read-user-string (getf-post "Fact" postlines)))
    (setq oldstate (gethash (1- (length (debugstruct-history (slot matchobj)))) (debugstruct-states (slot matchobj))))
    (setq moves (car (last (debugstruct-history (slot matchobj)))))
    (output-debug-match-xml s matchobj #'(lambda (x y) (trace-afact x y fact oldstate moves (null (debugstruct-history (slot matchobj))))))))

;;;;;;;;;; Outputting matches to in XML ;;;;;;;;;;;;;;

(defun trace-legals (s matchobj moves)
  (let ((*trace-device* s) (traceexpressions '(?)) (*tracexml* t))
    (mapc #'(lambda (x y)
              (format s "<tracewrap>")
              (legalp x y matchobj)
              (format s "</tracewrap>")
              (crlf s))
          (gameroles matchobj) moves)))


(defun trace-goals (s matchobj)
  (let ((*trace-device* s) (traceexpressions '(?)) (*tracexml* t))
    (mapc #'(lambda (p)
              (format s "<tracewrap>")
              (gdlfindx '?x `(goal ,p ?x) matchobj)
              (format s "</tracewrap>")
              (crlf s))
          (gameroles matchobj))))

(defun trace-terminal (s matchobj)
  (let ((*trace-device* s) (traceexpressions '(?)) (*tracexml* t))
    (format s "<tracewrap>")
    (gdlfindp 'terminal matchobj)
    (format s "</tracewrap>")
    (crlf s)))

(defun trace-afact (s matchobj fact oldstate moves &optional (init nil))
  (let ((th (define-theory (make-instance 'theory) "" (append oldstate (mapcar #'(lambda (x y) `(does ,x ,y)) (gameroles matchobj) moves)))))
    (includes th (theory matchobj))
    (let ((*trace-device* s) (traceexpressions '(?)) (*tracexml* t))
      (format s "<tracewrap>")
      (if init
        (gdlfindp `(init ,fact) th)
        (gdlfindp `(next ,fact) th))
    (format s "</tracewrap>")
    (crlf s))
    (empty th) (unincludes th (theory matchobj))))


(defun output-debug-match-xml (s matchobj &optional (extrafunc #'success))
  (output-xml-prolog s (debugstruct-xsl (slot matchobj)))
  ;(format s "<!DOCTYPE match SYSTEM \"http://games.stanford.edu/gamemaster/xml/viewmatch.dtd\">")(crlf s)
  (format s "<match>") (crlf s)
  (format s "<match-id>~A</match-id>" (name matchobj)) (crlf s)
  
  (funcall extrafunc s matchobj)
  
  (dolist (ro (gameroles matchobj)) 
    (format s "<role>~A</role>" (prettify ro)) (crlf s))
  (dolist (mo (car (history matchobj))) 
    (format s "<action>~A</action>" (caddr mo)) (crlf s))
  (format s "<state>") (crlf s)
  (dolist (fact (contents (data matchobj)))
    (format s "~T<fact>") (crlf s)
    (cond ((listp (cadr fact))
           (format s "~T~T<prop-f>~A</prop-f>" (caadr fact)) (crlf s)
           (dolist (arg (cdadr fact))
             (format s "~T~T<arg>~A</arg>" arg) (crlf s)))
          (t
           (format s "~T~T<prop>~A</prop>" (cadr fact)) (crlf s)))
    (format s "~T</fact>") (crlf s))
  (format s "</state>") (crlf s)
  (format s "<history>") (crlf s)
  (do ((as (history matchobj) (cdr as))
       (i (length (history matchobj)) (1- i)))
      ((null as))
    (format s "~T<step>") (crlf s)
    (format s "~T~T<step-number>~A</step-number>" i) (crlf s)
    (dolist (action (car as)) 
      (format s "~T~T<move>~A</move>" action) (crlf s))
    (format s "~T</step>") (crlf s))
  (format s "</history>") (crlf s)
  (format s "</match>"))

