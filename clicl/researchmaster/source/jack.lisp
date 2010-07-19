
;;;;;;;;;;;;;;;;;;;;; Classical interface ;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'classical)) postlines)
  (cond ((getf-post "axioms_text" postlines)
         (process-classical-query s postlines))
        ((null postlines)
         (process-classical s))
        (t (http-problem s "Bad request."))))

(defun process-classical (s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Classical Reasoning</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  ;(format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<center><h2>Classical Reasoning</h2></center>~%")
  (output-form-header s 'classical?)
  (output-premconc s)
  (output-classical-options s)
  (output-classical-actions s)
  (output-form-footer s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-classical-query (s postlines)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Classical Reasoning Result</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  (format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-classical-query s postlines)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))


(defun output-classical-query (s postlines)
  (let (axioms query conversion equality anc start depth inc things all th result options expr)
    (setq axioms (read-sentences (getf-post "axioms_text" postlines)))
    (setq query (maksand (read-sentences (getf-post "query_text" postlines))))
    (setq conversion (read-symbol (getf-post "conversion" postlines)))
    (setq equality (read-bool (getf-post "equality" postlines)))
    (setq anc (read-bool (getf-post "anc" postlines)))
    (setq start (read-symbol (getf-post "start" postlines)))
    (setq inc (read-symbol (getf-post "inc" postlines)))
    (setq depth (read-symbol (getf-post "depth" postlines)))
    (setq things (read-sentences (getf-post "thing" postlines)))
    (setq all (read-bool (getf-post "allsolns" postlines)))
    (setq th (make-instance 'theory))
    (setq expr (read-sentences (getf-post "trace" postlines)))  ;list '?))

    ; manipulate theory
    (case conversion
      (cnf (define-theory th "" (brfs (maksand axioms))))
      (contrapositives (define-theory th "" (contrapositives (maksand axioms))))
      (otherwise (define-theory th "" axioms)))
    (when equality (definemore th (equality-axioms axioms)))

    ; set default values
    (unless (cdr things) (setq things (first things)))
    (when (not start) (setq start *start*))
    (when (not inc) (setq inc *increment*))
    (when (not depth) (setq depth *depth*))

    ; compute result
    (let ((*ancestry* anc) (*start* start) (*increment* inc) (*depth* depth) (*tracetype* 'html) (*trace-device* s) (traceexpressions expr))
      (format s "<div class=\"trace\">~%")
      (cond ((and all things) (setq result (fullproves things query th)))
            (things (setq result (fullprovex things query th)))
            (t (setq result (fullprovep query th))))
      (format s "~&</div>"))

    ; output result
    (setq options `(("Conversion method" . ,conversion)
                    ("Add equality" . ,equality)
                    ("Ancestry pruning" . ,anc)
                    ("Start depth" . ,start)
                    ("Depth increment" . ,inc)
                    ("Max depth" . ,depth)
                    ("Variables" . ,things)
                    ("All solutions" . ,all)
                    ("Trace expressions" . ,expr)))

    (output-jack-result axioms query options result s)))

(defun output-classical-options (s)
  (format s "
	<div id=\"functionality\">
		
		<table border=\"0\">
		<tr><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">
						
				<tr><td class=\"moreoptions\">
					<b>Sentence Manipulation</b><br><br>
					<input type=\"radio\" name=\"conversion\" value=\"none\">No conversion<br>
					<input type=\"radio\" name=\"conversion\" value=\"cnf\" checked>CNF<br>
					<input type=\"radio\" name=\"conversion\" value=\"contrapositives\">CNF with Contrapositives<br>
					<br>
					<input type=\"checkbox\" name=\"equality\">Add Equality/Substitution axioms<br>
					
				</td></tr>
		</table>
		</td><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">		
				<tr><td class=\"moreoptions\">
					<b>Search Control</b><br><br>
					<input type=\"checkbox\" name=\"anc\" checked>Ancestry Pruning<br>
					<input type=\"input\" name=\"start\" size=\"5\" value=\"1\">Start Depth<br>
					<input type=\"input\" name=\"inc\" size=\"5\" value=\"1\">Increment<br>
					<input type=\"input\" name=\"stop\" size=\"5\" value=\"1000\">Depth Limit<br>
					<br>
					<input type=\"input\" name=\"thing\" size=\"10\" value=\"\">Variables of interest (separated with spaces)<br>
					<input type=\"checkbox\" name=\"allsolns\">Find all solutions<br>
				</td></tr>
                </table>
		</td><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">		
				<tr><td class=\"moreoptions\">
					<b>Output Control</b><br><br>
					<input type=\"input\" name=\"trace\" size=\"20\" value=\"\">Expressions to trace<br>
				</td></tr>
		</table>
		</td></tr></table>
	</div>"))

(defun output-classical-actions (s)
  (format s "
	<div id=\"submitblock\">
		<input type=\"submit\" name=\"command\" value=\"Check Entailment\">
<!--		<input type=\"submit\" name=\"command\" value=\"Show Proof\"> -->
	</div>"))



;;;;;;;;;;;;;;;;;;;;; Logic Programming interface ;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'lp)) postlines)
  (cond ((getf-post "axioms_text" postlines)
         (process-lp-query s postlines))
        ((null postlines)
         (process-lp s))
        (t (http-problem s "Bad request."))))

(defun process-lp (s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Minimal Model Reasoning</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  ;(format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<center><h2>Logic Programming</h2></center>~%")
  (output-form-header s 'lp?)
  (output-premconc s)
  (output-lp-options s)
  (output-lp-actions s)
  (output-form-footer s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-lp-query (s postlines)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Minimal Model Reasoning Result</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  (format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-lp-query s postlines)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))


(defun output-lp-query (s postlines)
  (let (axioms query equality anc start depth inc things all th result options expr)
    (setq axioms (read-sentences (getf-post "axioms_text" postlines)))
    (setq query (maksand (read-sentences (getf-post "query_text" postlines))))
    (setq equality (read-bool (getf-post "equality" postlines)))
    (setq anc (read-bool (getf-post "anc" postlines)))
    (setq start (read-symbol (getf-post "start" postlines)))
    (setq inc (read-symbol (getf-post "inc" postlines)))
    (setq depth (read-symbol (getf-post "depth" postlines)))
    (setq things (read-sentences (getf-post "thing" postlines)))
    (setq all (read-bool (getf-post "allsolns" postlines)))
    (setq th (make-instance 'theory))
    (setq expr (read-sentences (getf-post "trace" postlines)))  ;list '?))

    ; manipulate theory
    (define-theory th "" axioms)
    (when equality (definemore th (equality-axioms axioms)))

    ; set default values
    (unless (cdr things) (setq things (first things)))
    (when (not start) (setq start *start*))
    (when (not inc) (setq inc *increment*))
    (when (not depth) (setq depth *depth*))

    ; compute result
    (let ((*ancestry* anc) (*start* start) (*increment* inc) (*depth* depth) (*tracetype* 'html) (*trace-device* s) (traceexpressions expr))
      (format s "<div class=\"trace\">~%")
      (cond ((and all things) (setq result (viewfinds things query th)))
            (things (setq result (viewfindx things query th)))
            (t (setq result (viewfindp query th))))
      (format s "~&</div>"))

    ; output result
    (setq options `(("Add equality" . ,equality)
                    ("Ancestry pruning" . ,anc)
                    ("Start depth" . ,start)
                    ("Depth increment" . ,inc)
                    ("Max depth" . ,depth)
                    ("Variables" . ,things)
                    ("All solutions" . ,all)
                    ("Trace expressions" . ,expr)))

    (output-jack-result axioms query options result s)))

(defun output-lp-options (s)
  (format s "
	<div id=\"functionality\">
		
		<table border=\"0\">
		<tr><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">
						
				<tr><td class=\"moreoptions\">
					<b>Sentence Manipulation</b><br><br>
					<input type=\"checkbox\" name=\"equality\">Add Equality/Substitution axioms<br>
					
				</td></tr>
		</table>
		</td><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">		
				<tr><td class=\"moreoptions\">
					<b>Search Control</b><br><br>
					<input type=\"checkbox\" name=\"anc\" checked>Ancestry Pruning<br>
					<input type=\"input\" name=\"start\" size=\"5\" value=\"1\">Start Depth<br>
					<input type=\"input\" name=\"inc\" size=\"5\" value=\"1\">Increment<br>
					<input type=\"input\" name=\"stop\" size=\"5\" value=\"1000\">Depth Limit<br>
					<br>
					<input type=\"input\" name=\"thing\" size=\"10\" value=\"\">Variables of interest (separated with spaces)<br>
					<input type=\"checkbox\" name=\"allsolns\">Find all solutions<br>
				</td></tr>
                </table>
		</td><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">		
				<tr><td class=\"moreoptions\">
					<b>Output Control</b><br><br>
					<input type=\"input\" name=\"trace\" size=\"20\" value=\"\">Expressions to trace<br>
				</td></tr>
		</table>
		</td></tr></table>
	</div>"))

(defun output-lp-actions (s)
  (output-classical-actions s))

;;;;;;;;;;;;;;;;;;;;; Reformulation interface ;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'reformulate)) postlines)
  (cond ((getf-post "axioms_text" postlines)
         (process-reformulate-query s postlines))
        ((null postlines)
         (process-reformulate s))
        (t (http-problem s "Bad request."))))

(defun process-reformulate (s &optional (prem nil) (concl nil))
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Classical Reformulation</title>~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/main.css\">~%")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/jack.css\">~%")
  ;(format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (format s "<center><h2>Classical Reformulation</h2></center>~%")
  (output-form-header s 'reformulate?)
  (output-premconc s prem concl)
  (output-reformulate-options s)
  (output-reformulate-actions s)
  (output-form-footer s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-reformulate-query (s postlines)
  (output-reformulate-query s postlines))

(defun output-reformulate-query (s postlines)
  (let (axioms query conversion limit th q q2 univ)
    (setq axioms (read-sentences (getf-post "axioms_text" postlines)))
    (setq query (maksand (read-sentences (getf-post "query_text" postlines))))
    (setq conversion (read-symbol (getf-post "conversion" postlines)))
    (setq limit (read-symbol (getf-post "limit" postlines)))
    (when (not limit) (setq limit 1000000))

    ; manipulate theory
    (case conversion
      (res 
       (if (eq query 'true) (setq query nil) (setq query (convert-basic (list (maknot query)) nil 'cnf)))
       (if (eq axioms 'true) (setq axioms nil))
       (setq th (resolution-closure-snark (nconc axioms query) limit))
       (setq q nil))
      (extensional 
       (cond ((or (eq query 'true) (eq axioms 'true)) (setq q query) (setq th axioms))
             (t
              (let ((*limit* limit))
                (multiple-value-setq (q th univ) (what2how query axioms))
                (multiple-value-setq (q q2) (query2datalog q))
                (setq th (nconc q2 (iffth2datalog th univ)))))))
      (otherwise 
       (if axioms (setq th (convert-basic axioms query conversion)) (setq th axioms))
       (setq q query)))

    ; output result by simply changing the axioms and the query
    (process-reformulate s (contents th) q)))
 
(defun convert-basic (axioms query conversion)
  (case conversion
    (equality (nconc (equality-axioms (cons query axioms)) axioms))
    (skolemize (mapcar #'skolemize axioms))
    (herbrandize (mapcar #'herbrandize axioms))
    (cnf (mapcan #'clauses axioms))
    (brf (mapcan #'brfs axioms))
    (contrapositives (mapcan #'contrapositives axioms))
    ; the following two only do something if the arguments given are all in rule form
    (frf (mapcar #'frf axioms))
    (reverse (mapcar #'reverse-rule axioms))
    (t axioms)))


(defun output-reformulate-options (s)
  (format s "
	<div id=\"functionality\">
		
		<table border=\"0\">
		<tr><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">
						
				<tr><td class=\"moreoptions\">
					<b>Syntactic</b><br><br>
					<input type=\"radio\" name=\"conversion\" value=\"equality\">Add Equality/Substitution axioms<br>
					<input type=\"radio\" name=\"conversion\" value=\"skolemize\">Skolemize<br>
					<input type=\"radio\" name=\"conversion\" value=\"herbrandize\">Herbrandize<br>
					<input type=\"radio\" name=\"conversion\" value=\"cnf\" checked>Conjunctive Normal Form<br>
					<input type=\"radio\" name=\"conversion\" value=\"frf\">Forward Rule Form<br>
					<input type=\"radio\" name=\"conversion\" value=\"brf\">Backward Rule Form<br>
					<input type=\"radio\" name=\"conversion\" value=\"contrapositives\">Backward Rule Form w/ Contrapositives<br>
					<input type=\"radio\" name=\"conversion\" value=\"reverse\">Reverse rules<br>
					
				</td></tr>
		</table>
		</td><td valign=\"top\">
		<table class=\"entailment\" border=\"0\" cellspacing=\"4\" cellpadding=\"2\">		
				<tr><td class=\"moreoptions\">
					<b>Semantic</b><br><br>
					<input type=\"input\" name=\"limit\" size=\"5\" value=\"1000\">Resource Limit<br>
					<input type=\"radio\" name=\"conversion\" value=\"res\">Resolution Closure<br>
					<input type=\"radio\" name=\"conversion\" value=\"extensional\">What (FHL) to How (LP)<br>
				</td></tr>
                </table>
		</td></tr></table>
	</div>"))

(defun output-reformulate-actions (s)
  (format s "
	<div id=\"submitblock\">
		<input type=\"submit\" name=\"command\" value=\"Reformulate\">
<!--		<input type=\"submit\" name=\"command\" value=\"Show Proof\"> -->
	</div>"))


;;;;;;;;;;;;;;;;;;;;; Common routines ;;;;;;;;;;;;;;;;;;;;;
(defun output-jack-result (axioms query options result s)
  (format s "<p><center>")
  (format s "<table class=\"languagespec\" cellspacing=\"0\" cellpadding=\"1\">~%")
  (format s "<tr><td>Premises</td><td>")
  (dolist (a axioms) (kif2html a s))
  (format s "</td></tr>")
  (format s "<tr><td>Conclusion</td><td>")
  (kif2html query s)
  (format s "</td></tr>~%")
  (dolist (o options)
    (format s "<tr><td>~A</td><td>~A</td></tr>~%" (first o) (cdr o)))
  (format s "</table><br>~%")
  (format s "<table class=\"languagespec\" cellspacing=\"0\" cellpadding=\"1\"><tr><td>Result</td><td>~A</td></tr></table>~%" result)
  (format s "</center>"))

  

(defun read-bool (s)
  (setq s (read-symbol s))
  (if (find s '(on checked t)) t nil))

(defun read-symbol (s)
  (ignore-errors (tosymbol s)))

(defun output-form-header (s action)
  (format s "<form method=\"post\" action=\"~A\">~%" action))
(defun output-form-footer (s)
  (format s "</form>~%"))

(defun output-premconc (s &optional (prem nil) (conc nil))
  (unless (and (listp prem) (listp (car prem))) (setq prem (list prem)))
  (unless (and (listp conc) (listp (car conc))) (setq conc (list conc)))

  (format s "
	<table align=\"center\" border=\"0\"><tr>
		<td valign=\"top\">Premises
		</td><td>
			<textarea name=\"axioms_text\" id=\"axioms_text\" cols=\"80\" rows=\"15\">")
  (dolist (p prem)
    (format s "~A~%" p))
  (format s "</textarea><br>
		</td>
		</tr><tr>
		<td valign=\"top\">Conclusion
		</td><td>
			<textarea name=\"query_text\" id=\"query_text\" cols=\"80\" rows=\"5\">")
  (unless (eq conc 'true)
    (dolist (c conc)
      (format s "~A~%" c)))
  (format s "</textarea></td></tr></table>"))

	
