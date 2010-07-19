(defmethod process (s (file (eql 'checkgdl)) postlines)
  (declare (ignore postlines))
  (output-header-css s '("playability.css") "Playability")

  (format s "
<body>
<br/>
<form ACTION=\"gdl?\" METHOD=\"POST\" NAME=\"form1\">
<INPUT TYPE=\"submit\" VALUE=\"Check this GDL\"/>
<br/>
<TEXTAREA NAME=\"axioms\" ROWS=\"20\" COLS=\"100\" WRAP=\"soft\"></TEXTAREA>
</form>")
  (crlf s)
  (output-footer s))

(defmethod process (s (file (eql 'gdl)) postlines)
  (output-gdl-responses (get-list-from-post "axioms" postlines)
                        (get-lisp-from-post "responses" postlines)
                        (get-list-from-post "filter" postlines)
                        s))

(defmethod process (s (file (eql 'validategame)) postlines)
  (let (game tmp)
    (setq game (read-user-string (cdar postlines)))
    (cond ((null game) (http-problem s "No rule set specified."))
          ((and (boundp game) (listp (contents (symbol-value game))))
           (output-gdl-responses (contents (symbol-value game)) nil nil s))
          ((viewfindp `(game.creator ,game gamut) *repository*)
           ; grab the description of the game, of which the last token is the filename
           (setq tmp (viewfindx '?x `(game.description ,game ?x) *repository*))
           ; extract the last token and make it a lisp friendly pathname
           (setq tmp (reverse tmp))
           (setq tmp (concatenate 'string "/gullible" (nreverse (subseq tmp 0 (search " " tmp)))))
           ; store the axioms as a theory for the next time this game is used
           (set game (define-theory (make-instance 'theory) "" (read-unix-file-contents (macify tmp))))
           ; show rules
           (output-gdl-responses (contents (symbol-value game)) nil nil s))

          (t (http-problem s "Unknown game")))))

(defmethod process (s (file (eql 'strictvalidategame)) postlines)
  (let ((*ordering* t))
    (process s 'validategame postlines)))


#|(defmethod process (s (file (eql 'addaxioms)) postlines)
  (let (rules responses)
    (setq rules (getf-post "Rules" postlines))
    (setq rules (substitute #\Newline #\Linefeed rules))
    (setq rules (with-input-from-string (f rules)
                       (do ((a (read f nil) (read f nil)) (nl))
	                 ((null a) (nreverse nl))
                         (setq nl (cons a nl)))))
    (setq responses (game-responses rules))
    (cond ((> (+ (local-response-count responses)
                 (global-response-count responses))
              0)
           (output-gdl-responses nil responses nil s))
          (t
           (setq *chesstheory* (define-theory (make-instance 'theory) "" rules))
           (process s 'resetchess postlines)))))
|#

(defun get-lisp-from-post (key postlines)
  (let ((res (getf-post key postlines)))
    (if res 
      (read-from-string res)
      nil)))

(defun output-gdl-responses (rules responses filter s)
  "(OUTPUT-GDL-RESPONSES RULES RESPONSES FILTER S) outputs the page displaying
   the responses to checking RULES and the page that filters those responses."
  (output-header-css s '("playability.css") "GDL Results")
  ;(format s "<HTML>") (crlf s)
  ;(format s "<HEAD><TITLE>GDL Reponses</TITLE>")
  (format s "<style>
        #wrapper {
                font-size: small;
                margin: 20px 20px 20px 20px;
        }
        div.subtitle {
                margin-top: 20px;
                font-size: xx-large;
                clear: left;
        }
        div.localresp {
                font-family: courier new, fixed;
                position: relative;
		border: 1px solid black;
                background: white;
        }
        div.globalresp {
                font-family: courier new, fixed;
                position: relative;
		border: 1px solid black;
                background: white;
        }
        div.sentwresp {
                position: relative;
        }
        div.resperror {
                margin-left: 5px;
                position: relative;
                color: red;
                clear: left;
        }
        span.true { color: blue; }
        span.next { color: blue;} 
        span.does { color: blue; }
        span.init { color: blue; }
        span.goal { color: blue;}
        span.terminal { color: blue; }
        span.legal { color: blue; }
        span.role { color: blue; }

        div.sentencewrap {
                position: relative;
        }
 	div.sentence {
		position: relative;
		clear: left;
	}
	div.atom {
		position: relative;
		margin-left: 4px;
	}
	div.head {
		position: relative;
		float: left;
		margin-left: 4px;
	}
	div.body {
		position: relative;
		float: left;
		margin-left: 4px;
	}
        </style>")

  (format s "<form id=\"form\" method=\"post\" action=\"gdl?\">")
  (crlf s)
  
  ; add a field for filtering and a button for submitting
  (format s "<input type=\"input\" name=\"filter\" value=\"")
  (dolist (v filter) (format s "~A " v))
  (format s "\">")
  (format s "<input type=\"submit\" value=\"Show only those rules with these constants\">")
  
  ; if this is the first time the page has been seen
  (when rules  
    (setq responses (game-responses rules)))
  
  ; display filtered responses
  (responses2html (filter-responses responses filter) s)
  
  ; store all the responses in the form
  (format s "<input type=\"hidden\" name=\"responses\" value=\"")
  (lispresponses2html responses s)
  (format s "\">")
  
  ; populate the filter fields with the current value
  (format s "<input type=\"input\" name=\"filter\" value=\"")
  (dolist (v filter) (format s "~A " v))
  (format s "\">")
  (format s "<input type=\"submit\" value=\"Show only those rules with these constants\">")
  
  (format s "</form>")
  (output-footer s))


(defun resp2h (resp s)
  "(RESP2H RESP S) outputs the response RESP into html that can be embedded inside
   quotes."
  (format s "#S(RESPONSE :TYPE ~A :MSG &quot;~A&quot; :VOCAB ~A)" 
          (response-type resp) 
          (response-msg resp)
          (response-vocab resp)))

(defun rule2h (rule s)
  "(RULE2H RULE S) outputs the rule RULE as HTML, bare HTML."
  (cond ((atom rule) (format s "~A" rule))
        ((find (car rule) '(and or not forall exists))
         (format s "(~A" (car rule)) 
         (mapc #'(lambda (x) (format s " ") (rule2h x s)) (cdr rule))
         (format s ")"))
        ((find (car rule) '(<=> => <=))
         (cond ((eq (car rule) '<=>) (format s "(&lt;=&gt;"))
               ((eq (car rule) '<=) (format s "(&lt;="))
               ((eq (car rule) '=>) (format s "(=&gt;")))
         (mapc #'(lambda (x) (format s " ") (rule2h x s)) (cdr rule))
         (format s ")"))
        (t 
         (format s "(")
         (rule2h (car rule) s)
         (mapc #'(lambda (x) (format s " ") (rule2h x s)) (cdr rule))
         (format s ")"))))

(defun lispresponses2html (responses s)
  "(LISPRESPONSES2HTML RESPONSES S) outputs the Lisp responses object
   RESPONSES into HTML so that it can be assigned the value of a hidden
   form element, i.e. so that the Lisp object can be printed inside quotes."
  (format s "#S(RESPONSES :LOCAL (")
  (mapc #'(lambda (x) (format s "(") (rule2h (car x) s) (format s " ") 
           (mapc #'(lambda (y) (resp2h y s) (format s " ")) (cdr x))
           (format s ") ")) 
        (responses-local responses))
  (format s ") :GLOBAL (")
  (mapc #'(lambda (x) (resp2h x s) (format s " ")) (responses-global responses))
  (format s "))")
  (crlf s))

(defun filter-responses (responses filter)
  "(FILTER-RESPONSES RESPONSES FILTER) removes all the responses that do not include 
   one of the constants in filter."
  (cond ((not filter) responses)
        (t
         (make-responses :local (remove-if-not #'(lambda (x) (mentions-some-constantp (car x) filter))
                                               (responses-local responses))
                         :global (responses-global responses)))))

(defun mentions-some-constantp (p constants)
  "(MENTIONS-SOME-CONSTANTP P CONSTANTS) returns T iff P includes at least one of
   the constants in the list CONSTANTS."
  (cond ((atom p) (find p constants))
        ((find (car p) '(<=> <= => and or not))
         (some #'(lambda (x) (mentions-some-constantp x constants)) (cdr p)))
        ((find (car p) '(forall exists))
         (mentions-some-constantp (caddr p) constants))
        (t
         (some #'(lambda (x) (mentions-some-constantp x constants)) p))))

(defun responses2html (responses s)
  "(RESPONSES2HTML TH RESPONSES S) outputs the responses for theory TH to stream S
   in html."
  (chunk-format s "<div id=\"wrapper\">")
  (format s "<div class=\"subtitle\"> Summary: <br>~A problems with individual axioms and <br>~A problems with the axioms as a whole.</div>" (local-response-count responses) (global-response-count responses))


  ; first output the rules and the responses for each rule
  (when (some #'(lambda (x) (cdr x)) (responses-local responses))
    (chunk-format s "<div class=\"subtitle\">Problems with Individual Axioms</div>") (crlf s))
  (chunk-format s "<div class=\"localresp\">")
  (mapc #'(lambda (x) (ruleresponse2html x s)) (responses-local responses))
  (chunk-format s "<div class=\"sentence\" style=\"visibility: hidden;\">.</div>")
  (chunk-format s "</div>")

  (crlf s) (crlf s)
 
  ; then output the global responses
  (when (responses-global responses)
    (chunk-format s "<div class=\"subtitle\">Problems with the Axioms as a Whole</div>") (crlf s)
    (chunk-format s "<div class=\"globalresp\">")
    (mapc #'(lambda (x) (response2html x s)) (responses-global responses))
    (chunk-format s "</div>"))

  (chunk-format s "</div>"))


(defun ruleresponse2html (response s)
  "(RULERESPONSE2HTML RULE RESPONSE S) prints out the rule and all the reponses to that rule
   to stream S in html."
  ;(format s "<div class=\"sentwresp\">")
  (mapc #'(lambda (x) (response2html x s)) (cdr response))
  ;(format s "<div class=\"sentencewrap\">")
  (datalog2html (car response) s)
  ;(format s "</div></div>") 
  (crlf s) (crlf s))

(defun response2html (response s)
  "(RESPONSE2HTML RESPONSE S) outputs response to s in html."
  (cond ((eq (response-type response) 'error) 
         (if (response-vocab response)
           (chunk-format s "<div class=\"resperror\">; ~A ~A</div>" (response-msg response) (response-vocab response))
           (chunk-format s "<div class=\"resperror\">; ~A</div>" (response-msg response))))
        (t ))
  (crlf s))

(defun datalog2html (rule s)
  "(DATALOG2HTML RULE S) translates the datalog rule RULE into HTML to S.  It treats
   only the connectives <= or not as special."
  (let ((*real-ops* '(<= or not)))
    (fol2html rule s 0)))

(defun kif2html (fol s)
  "(KIF2HTML (FOL S) translates the kif rule FOL into HTML to S."
  (let ((*real-ops* '(forall exists <= => <=> and or not)))
    (fol2html fol s 0)))

(defun fol2html (fol s depth)
  "(FOL2HTML FOL S DEPTH) outputs FOL prettily in HTML to S, starting in the context
   of DEPTH sentences deep, i.e. DEPTH is the number of parentheses that need to be
   added at the end of the call."
  (declare (notinline fol2html))
  (cond ((atom fol)
         (chunk-format s "<div class=\"sentence\"><div class=\"head\">")
         (atomic2html fol s)
         (maptimes #'(lambda () (chunk-format s ")")) depth)
         (chunk-format s "</div></div>")
         (crlf s))

        ((and (find (car fol) '(<= => and or <=>)) (find (car fol) *real-ops*))

         ; wrapper
         (chunk-format s "<div class=\"sentence\">")

         ; make the operator the head of the sentence
         (chunk-format s "<div class=\"head\">(")
         (operator2html (car fol) s)
         (chunk-format s "</div>")

         ; include each of the operands in the body
         (chunk-format s "<div class=\"body\">")
         (fols2html (cdr fol) s (1+ depth))
         (chunk-format s "</div>")

         ; end the sentence
         (chunk-format s "</div>") (crlf s))


        ((and (find (car fol) '(forall exists)) (find (car fol) *real-ops*))
         ; wrapper
         (chunk-format s "<div class=\"sentence\">")

         ; make the operator the head of the sentence
         (chunk-format s "<div class=\"head\">(")
         (operator2html (car fol) s)
         (chunk-format s " ")
         (vars2html (cadr fol) s)
         (chunk-format s "</div>")

         ; make the rest of the rule into the body.
         (chunk-format s "<div class=\"body\">")
         (fol2html (caddr fol) s (1+ depth))
         (chunk-format s "</div>")

         ; end the sentence
         (chunk-format s "</div>") (crlf s))

        (t (chunk-format s "<div class=\"sentence\"><div class=\"head\">")
           (atomic2html fol s)
           (maptimes #'(lambda () (chunk-format s ")")) depth)
           (chunk-format s "</div></div>"))))

(defun fols2html (ps s depth)
  "(FOLS2HTML PS S DEPTH) call fol2html for all but the last element in PS with
   depth 0, and then call fol2html on the last element with depth DEPTH.  This puts
   the appropriate number of parentheses after the last element."
  (cond ((not (cdr ps)) (fol2html (car ps) s depth))
        (t
         (do ((p ps (cdr p)))
             ((not (cdr p)) (fol2html (car p) s depth))
           (fol2html (car p) s 0)))))

(defun vars2html (vs s)
  "(VARS2HTML VS S) outputs a list of variables to HTML."
  (cond ((atom vs) (atomic2html vs s))
        (t
         (chunk-format s "(")
         (atomic2html (car vs) s)
         (mapc #'(lambda (x) (chunk-format s " ") (atomic2html x s)) (cdr vs))
         (chunk-format s ")"))))

(defun operator2html (op s)
  "(OPERATOR2HTML OP S) outputs an operator to HTML."
  (let ((hop (cdr (assoc op *html-connectives*))))
    (when (not hop) (setq hop op))
    (chunk-format s "<span class=\"operator\">~(~A~)</span>" hop)))

;(defun crlf (s)
;  (format s "~%"))
      

(defun atomic2html (a s)
  "(ATOMIC2HTML A S) outputs atoms and terms to html."
  (cond ((atom a) 
         (let ((span (lookup-span a)))
           (if span
             (chunk-format s "<span class=\"~A\">~(~A~)</span>" span a)
             (chunk-format s "~(~A~)" a))
           ))
        (t
         (chunk-format s "(")
         (atomic2html (car a) s)
         (mapc #'(lambda (x) (chunk-format s " ") (atomic2html x s)) (cdr a))
         (chunk-format s ")")
         )))

(defun lookup-span (a)
  "(LOOKUP-SPAN A) returns the span name for the vocabulary element A.  Handles
   variables as well."
  (cond ((varp a) 'var)
        ((listp a) nil)
        ((find a '(true next does init goal terminal legal role)) a)
        (t nil)))



