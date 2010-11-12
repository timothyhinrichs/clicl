(defun reload-man ()
  (empty *manager*)
  (definemore *manager* *sentences*)
  (loaddata man *manager*))


(defparameter *displayhier* nil)
(defparameter *tagfile* nil)
(defparameter *tagtheory* nil)
(defparameter *authfile* nil)
(defparameter *auththeory* nil)

;;;;;;;;;;;;;;;;;;;;;;;;; Data Manipulation ;;;;;;;;;;;;;;;;;;;;;;;;;

; add authors to repository
(defun update-authors ()
  "(UPDATE-AUTHORS) runs through all the paper.authors, breaks them apart, and adds them
   to the repository if they are not already there."
  (dolist (p (viewfinds '?y '(paper.author ?x ?y) *repository*))
    (unless (viewfindp `(person.instance ,p) *repository*)
      (let (s1 s2 s3 s4 sp)
        (setq sp (tostring p))
        (multiple-value-setq (s1 s2 s3 s4) (stringmatches "(.*)\\.(.*)\\.(.*)" sp))
        (cond (s1 (save `(person.instance ,p) *repository*)
                  (save `(person.firstname ,p ,(format nil "~:(~A~)" s2)) *repository*)
                  (save `(person.middlename ,p ,(format nil "~:(~A~)" s3)) *repository*)
                  (save `(person.lastname ,p ,(format nil "~:(~A~)" s4)) *repository*))
              (t
               (multiple-value-setq (s1 s2 s3) (stringmatches "(.*)\\.(.*)" sp))
               (cond (s1 (save `(person.instance ,p) *repository*)
                         (save `(person.firstname ,p ,(format nil "~:(~A~)" s2)) *repository*)
                         (save `(person.lastname ,p ,(format nil "~:(~A~)" s3)) *repository*))
                     (t (format t "~&Huh: ~A~%" p)))))))))

; move all the papers immediately under OLDTOPIC to NEWTOPIC 
(defun swaptopics (oldtopic newtopic)
  (dolist (v (viewfinds `(paper.topic ?x ,oldtopic) `(paper.topic ?x ,oldtopic) *repository*)) 
    (print v)
    (drop v *repository*) 
    (save (list (first v) (second v) newtopic) *repository*)))

(defun swapsuperclass (oldsuper newsuper)
  (dolist (v (viewfinds `(topic.superclass ,oldsuper ?x) `(topic.superclass ,oldsuper ?x) *repository*)) 
    (print v)
    (drop v *repository*) 
    (save (list 'topic.superclass oldsuper newsuper) *repository*)))

(defun movetopic (topic oldsuper newsuper)
  (dolist (v (viewfinds `(topic.superclass ,topic ,oldsuper) `(topic.superclass ,topic ,oldsuper) *repository*)) 
    (print v)
    (drop v *repository*)
    (save (list (first v) (second v) newsuper) *repository*)))

(defun movesubtopics (topic newtopic)
  (dolist (v (viewfinds `(topic.superclass ?x ,topic) `(topic.superclass ?x ,topic) *repository*)) 
    (movetopic (second v) topic newtopic)))

(defun movesubs (topic newtopic)
  (swaptopics topic newtopic)
  (movesubtopics topic newtopic))


(defun connected (x)
  (or (eq x 'topic0)
      (some #'connected (viewfinds '?x `(topic.superclass ,x ?x) *repository*))))


;;;;;;;;;;;;;;;;;;;;;;;;; Hierarchy ;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fancy version
(defmethod process (s (file (eql 'displayFancyReferenceHierarchy)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post "Start" postlines)) (setq dum (read-user-string dum)))
           (process-displayfancyreferencehierarchy s dum))
          (t (http-problem s "Bad request.")))))

(defun process-displayfancyreferencehierarchy (s start)
  (let (hier)
    ; displayhier is a recursive datastructure: (name "prettyname" (tags) displayhier1 displayhier2 ...)
    (setq hier (find-hier-start *displayhier* start))
    ; turns hier into (name "prettyname" (tags) (papers) displayhier1 ...)
    (setq hier (populate-display-hierarchy hier)) 
    (format-html s) (crlf s)
    (format-head s)
    (format s "<title>References -- ~A</title>~%" (second hier))
    (format s "<link type=\"text/css\" href=\"/style/references.css\">~%")
    (format s "~A" (showhidescript))
    (finish-head s) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<center><span style=\"font-size: 190%\">~A</span> <span style=\"font-size:140%\">[<a href=\"displaybibtex?topic=~A\">bib</a>]</span><br>"  (second hier) (first hier))
    (when (cdr (third hier)) (output-taglist s (cdr (third hier)))) 
    (format s "</center><br>~%")
    (dolist (p (sortem (fourth hier)
		       (find-sorter 'paper) 
		       'ascending))
      (output-paper-html p s))
    (dolist (h (nthcdr 4 hier))
      (output-fancyreferencehierarchy s h 0 'block))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-taglist (s tags)
  (cond ((atom tags) (output-tag tags s))
	((listp tags)
	 (output-tag (car tags) s)
	 (dolist (v (cdr tags))
	   (format s ", ")
	   (output-tag v s)))))

(defun output-tag (tag s)
  (format s "<span class=\"tag\">~A</span>" (get-topic-printname tag)))

(defun output-fancyreferencehierarchy (s hier level defaultshow)
  (let ((sorter (find-sorter 'paper)) 
        (haspapers (or (fourth hier) (nthcdr 4 hier))))
    (output-flippable-header (first hier) 'none haspapers s)
    (format s "<span style=\"font-size: ~A%; display: ~(~A~)\"><a href=\"displayfancyreferencehierarchy?start=~A\">~A</a> [<a href=\"displaybibtex?topic=~A\">bib</a>] " (max 0 (- 150 (* level 20))) defaultshow (first hier) (second hier) (first hier))
    (when (cdr (third hier)) 
      (format s "<span style=\"font-size:70%\"> (") 
      (output-taglist s (cdr (third hier))) 
      (format s ")</span>"))
    (format s "</span>~%")
    (output-flippable-start (first hier) 'none s)
    (format s "<div style=\"margin-left: ~Apx\">~%" 30)
    (dolist (p (sortem (fourth hier) sorter 'ascending))
      (output-paper-html p s))
    (dolist (h (nthcdr 4 hier))
      (output-fancyreferencehierarchy s h (1+ level) 'block))
    (format s "</div>")
    (output-flippable-end (first hier) 'none s)
    (output-flippable-footer (first hier) 'none s)))
 
(defun init-displayhierarchy (hier &optional (prefix "dspobj."))
  "(INIT-DISPLAYHIERARCHY HIER) basically just adds symbolic names for each hierarchy element."
  (declare (notinline init-displayhierarchy))
  (cond ((atom hier) hier)
	(t (setf (first hier) (tosymbol (stringappend prefix (second hier))))
	   (mapc #'(lambda (x) (init-displayhierarchy x (stringappend (first hier) ".")))
		 (nthcdr 3 hier))
	   hier)))

(defun find-hier-start (hier key)
  (cond ((eq (first hier) key) hier)
	(t (dolist (v (nthcdr 3 hier) nil)
	     (setq hier (find-hier-start v key))
	     (if hier (return hier))))))

(defun populate-display-hierarchy (hier)
  (let (query papers children newchild subpapers allpapers)
    ; all papers that could belong to this node in the hierarchy
    (setq query (make-tags-query (cdr (third hier))))
    (if query
	(setq papers (viewfinds (first (vars query)) query *tagtheory*)) 
	(setq papers nil))
    (cond ((not (nthcdr 3 hier)) 
	   (values (list (first hier) (second hier) (third hier) papers)
		   papers))
	  (t (setq allpapers nil)
	     (dolist (child (nthcdr 3 hier))
	       (multiple-value-setq (newchild subpapers) (populate-display-hierarchy child))
	       (push newchild children)
	       (setq allpapers (union subpapers allpapers)))
	     (setq papers (set-difference papers allpapers))
	     (values (list* (first hier) (second hier) (third hier) papers (nreverse children))
		     (nconc allpapers papers))))))

;;; standard version

(defmethod process (s (file (eql 'displayReferenceHierarchy)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post "Start" postlines)) (setq dum (read-user-string dum)))
           (process-displayreferencehierarchy s dum))
          (t (http-problem s "Bad request.")))))

(defun process-displayreferencehierarchy (s start)
  (let (hier)
    (setq hier (build-hierarchy start))
    (format-html s) (crlf s)
    (format-head s)
    (format s "<title>References -- ~A</title>~%" (second hier))
    (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/style/references.css\">~%")
    (format s "~A" (showhidescript))
    (finish-head s) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (format s "<center><span style=\"font-size: 190%\">~A</span> <span style=\"font-size:140%\">[<a href=\"displaybibtex?topic=~A\">bib</a>]</span></center><br>" (second hier) (first hier)) 
    (dolist (p (sortem (prorequest `(ask-all ?x (paper.topic ?x ,(first hier)))) (find-sorter 'paper) 'ascending))
      (output-paper-html p s))
    (dolist (h (cddr hier))
      (output-referencehierarchy s h 0 'block))
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun build-hierarchy (class)
  (list* class (get-topic-printname class) (build-hierarchy-aux class)))

(defun build-hierarchy-aux (class)
  (let (subs)
    (setq subs (viewfinds '?x `(topic.superclass ?x ,class) *repository*))
    (setq subs (mapcar #'(lambda (x) (list x (get-topic-printname x))) subs))
    (setq subs (sort subs #'string< :key #'second))
    (cond ((null subs) nil)
          (t (mapcar #'(lambda (x) (list* (car x) (cadr x) (build-hierarchy-aux (car x)))) subs)))))

(defun get-topic-printname (topic)
  (let (r)
    (setq r (viewfindx '?x `(shortname ,topic ?x) *repository*))
    (cond (r r)
          (t (setq r (viewfindx '?x `(prettyname ,topic ?x) *repository*))
             (cond (r r)
                   (t (prettify topic)))))))

(defun find-papers-by-topic (topic)
  (remove-duplicates (mapcan #'(lambda (x) (viewfinds '?x `(paper.topic ?x ,x) *repository*)) 
                             (mapcar #'car (flatten-hierarchy (build-hierarchy topic) nil)))))

(defun flatten-hierarchy (h sofar)
  (cond ((not h) sofar)
        ((atom (car h)) (flatten-hierarchy (cddr h) (cons (list (first h) (second h)) sofar)))
        (t (flatten-hierarchy (cdr h) (flatten-hierarchy (car h) sofar)))))

(defun output-referencehierarchy (s hier level defaultshow)
  (let ((sorter (find-sorter 'paper)) 
        (haspapers (or (viewfindp `(paper.topic ?x ,(first hier)) *repository*) (cddr hier))))
    (output-flippable-header (first hier) 'none haspapers s)
    (format s "<span style=\"font-size: ~A%; display: ~(~A~)\"><a href=\"displayreferencehierarchy?start=~A\">~A</a> [<a href=\"displaybibtex?topic=~A\">bib</a>]</span>~%" (max 0 (- 150 (* level 20))) defaultshow (first hier) (second hier) (first hier))
    (output-flippable-start (first hier) 'none s)
    (format s "<div style=\"margin-left: ~Apx\">~%" 30)
    (dolist (p (sortem (prorequest `(ask-all ?x (paper.topic ?x ,(first hier)))) sorter 'ascending))
      (output-paper-html p s))
    (dolist (h (cddr hier))
      (output-referencehierarchy s h (1+ level) 'block))
    (format s "</div>")
    (output-flippable-end (first hier) 'none s)
    (output-flippable-footer (first hier) 'none s)))

(defun output-flippable-header (handle default showflipper s) 
  "default is either block or none, as symbols"
    (setq handle (format nil "~(~A~)" handle))
    (format s "    <table border=\"0\" width=\"100%\">~%")
    (format s "    <tr>~%")
    (format s "        <td width=\"6\" valign=\"center\">~%")
    (cond (showflipper
           (format s "            <a href=\"javascript:showDetails('~A')\">~%" handle)
           (format s "               <img src=\"/docserver/infoserver/examples/researchmaster/images/arrowrt.gif\" width=\"21\" height=\"19\" id=\"~AShow\" style=\"display:~A;\" border=\"0\"></a>~%" handle (if (eq default 'block) "none" "block"))
           (format s "            <a href=\"javascript:hideDetails('~A')\">~%" handle)
           (format s "               <img src=\"/docserver/infoserver/examples/researchmaster/images/arrowdn.gif\" width=\"21\" height=\"19\" id=\"~AHide\" style=\"display:~A\" border=\"0\"></a>~%" handle (if (eq default 'block) "block" "none")))
          (t
           (format s "               <img src=\"/docserver/infoserver/examples/researchmaster/images/transparent.gif\" width=\"21\" height=\"19\" border=\"0\">~%")))
    (format s "        </td>~%")
    (format s "        <td align=\"left\">~%"))

(defun output-flippable-start (handle default s)
    (setq handle (format nil "~(~A~)" handle))
    (format s "        </td>~%")
    (format s "    </tr><tr>~%")
    (format s "        <td colspan=\"2\">~%")
    (format s "            <div id=\"~A\" style=\"display:~A\">~%" handle default))

(defun output-flippable-end (handle default s)
    (declare (ignore handle default))
    (format s "~&            </div>"))

(defun output-flippable-footer (handle default s)
    (declare (ignore handle default))
    (format s "        </td></tr></table>"))

;;;;;;;;;;;;;;;;;;;;;;;; Authorization Policies ;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'updateAuthPolicy)) postlines)
  (cond ((null postlines) (output-updateauthpolicy s))
	(t (process-updateauthpolicy postlines)
	   (process-displayreferences nil s))))

(defun output-updateauthpolicy (s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Update Authorization Policy</title>")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/references.css\">~%")
  (format s "<script type='text/javascript' src='/docserver/infoserver/examples/researchmaster/javascript/util.js'></script>")
  (format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-authpolicy (read-any-file *authfile*) s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-authpolicy (stringpolicy s)
  (format s "<div class=\"bodytext\">~%")
  (format s "<h2>Authorization Policy</h2>~%")
  (format s "<form name=\"myform\" method=\"post\" action=\"updateAuthPolicy?\">")
  (format s "<p><input type=\"submit\" value=\"Update\">")
  (format s "<p>The language below is a KIF-variant of nonrecursive Datalog with stratified negation.
             A user u is denied access to document d if the sentences below entail <tt>(deny u d)</tt>.
             Similarly for <tt>(allow u d)</tt>.")
  (format s "<p><textarea name=\"authpolicy\" id=\"authpolicy\" rows=\"20\" cols=\"80\">")
  (princ stringpolicy s)
;  (dolist (p (contents th))
;    (format s "~A~%" p))
  (format s "</textarea>~%")
  (format s "<p><input type=\"submit\" value=\"Update\">")
  (format s "</form>~%")
  (format s "</div>~%"))

(defun process-updateauthpolicy (postlines)
  (setq postlines (getf-post "authpolicy" postlines))
  (write-file *authfile* postlines)
  (define-prologtheory *auththeory* "" (mapcar #'morph-tag-queries (read-sentences postlines))))

(defun morph-tag-queries (p)
  (cond ((atomicp p)
	 (cond ((atom p) p)
	       ((eq (car p) 'paper.topic) (cons 'paper.topic2 (cdr p)))
	       (t p)))
	(t (cons (car p) (mapcar #'morph-tag-queries (cdr p))))))

;;;;;;;;;;;;;;;;;;;;;;;; Tags and their Relationships ;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod process (s (file (eql 'updateTagTheory)) postlines)
  (cond ((null postlines) (output-updatetagtheory s))
	(t (process-updatetagtheory postlines)
	   (process-displayreferences nil s))))

(defun output-updatetagtheory (s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>Update Tag Theory</title>")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/references.css\">~%")
  (format s "<script type='text/javascript' src='/docserver/infoserver/examples/researchmaster/javascript/util.js'></script>")
  (format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-tagtheory (read-any-file *tagfile*) s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-tagtheory (stringpolicy s)
  (format s "<div class=\"bodytext\">~%")
  (format s "<h2>Tag Ontology</h2>~%")
  (format s "<form name=\"myform\" method=\"post\" action=\"updateTagTheory?\">")
  (format s "<p><input type=\"submit\" value=\"Update\">")
  (format s "<p>The language used below is first-order logic, written in KIF, which uses prefix operators.")
  (format s "<p>The resolution closure of the sentences must be finite.")
  (format s " (In the future this property will be enforced by using monadic logic, ")
  (format s "e.g. car(x) => vehicle(x) instead of paper.topic(x,car) => paper.topic(x,vehicle).)~%")
  (format s "<p><textarea name=\"tagrelationships\" id=\"tagrelationships\" rows=\"20\" cols=\"80\">")
  (princ stringpolicy s)
;  (dolist (p (contents th))
;    (format s "~A~%" p))
  (format s "</textarea>~%")
  (format s "<p><input type=\"submit\" value=\"Update\">")
  (format s "</form>~%")
  (format s "</div>~%"))

(defun process-updatetagtheory (postlines)
  (setq postlines (getf-post "tagrelationships" postlines))
  (write-file *tagfile* postlines)
  (define-prologtheory *tagtheory* "" (construct-datalog-theory (read-sentences postlines))))

(defun construct-datalog-theory (th)
  (setq th (fhlc (contents th) 'complete 'valid :dca nil :una nil
		 :options `((transformation . allpreds) 
			    (fullyfactored . nil)
			    (predicates . nil))))

  ; throw out all the negatives
  (setq th (remove-if #'(lambda (x) (or (negative-literalp (impl-sent (head x)))
					(some #'negative-literalp (body x))))
		      th))
  ; change all heads
  (setq th (mapcar #'(lambda (x) 
		       (let ((newhead (makpapertopic (list (second (impl-sent (head x))) 
							   (third (impl-sent (head x)))))))
			(cond ((eq (car x) '<=) (list* '<= newhead (cddr x)))
			      (t newhead))))
		   th))
  ; ensure safety (just for variables in the head)
  (mapc #'(lambda (x) (let ((v (first (args (head x)))) n)
			(unless (member v (vars (body x)))
			  (setq n (cons `(paper.instance ,v) (body x)))
			  (cond ((eq (car x) '<=) (setf (cddr x) n))
				(t (list* '<= x n))))))
	th)
  ; grab all of the explicitly defined paper.topics as well
  (cons `(<= ,(makpapertopic (head-args 2))
	     ,(cons 'paper.topic (head-args 2)))
	th))

(defun makpapertopic (args)
  (cons (tosymbol '(paper.topic 2)) args))
 
;;;;;;;;;;;;;;;;;;;;;;;;; Reference List ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'displayReferences)) postlines)
  (process-displayreferences postlines s))

(defun process-displayreferences (postlines s)
  (format-html s) (crlf s)
  (format-head s)
  (format s "<title>References</title>")
  (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"/docserver/infoserver/examples/researchmaster/style/references.css\">~%")
  (format s "<script type='text/javascript' src='/docserver/infoserver/examples/researchmaster/javascript/util.js'></script>")
  (format s "~A" (showhidescript))
  (finish-head s) (crlf s)
  (format-body s *bgcolor*) (crlf s)
  (output-header s)
  (output-tagged-references postlines s)
  (output-footer s)
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun output-tagged-references (postlines s)
  (output-tags-references (construct-current-tag-sequence postlines) postlines s))

(defun construct-current-tag-sequence (postlines)
  (let (tags operation optarget)
    ; grab the current tag sequence
    (setq tags (read-sentences (getf-post "tagsequence" postlines)))
    (when (and (atom tags) tags) (setq tags (list tags)))
    (setq operation (read-user-string (getf-post "operation" postlines)))
    (setq optarget (read-user-string (getf-post "operationtarget" postlines)))
    ; create a new tag sequence
    (case operation
      (deleteafter (do ((a tags (cdr a))) ((null a) tags) 
		     (when (eq (car a) optarget) (setf (cdr a) nil) (return tags))))
      (delete (delete optarget tags))
      (add (nconc tags (list optarget)))
      (otherwise tags))))

(defun make-tags-query (tags)
  (let (q)
    (setq q (mapcar #'(lambda (x) (makpapertopic (list (car (head-args 1)) x))) tags))
    (when q (maksand q))))

(defun output-tags-references (tags postlines s)
  (let (low high atatime papers numpapers query user)
    (format s "<form action=\"displayReferences?\" method=\"post\" id=\"navform\">")
    (format s "<table border=\"0\" width=\"100%\">~%")
    (if tags
	(format s "<tr><td valign=\"top\">Displaying all documents with tags: <br>~%")
	(format s "<tr><td valign=\"top\">Displaying all documents<br>~%"))
    ; output current tag state
    (output-tags-state tags s)
    (format s "~&</td><td valign=\"top\" align=\"right\">")
    ; find user ID
    (setq user (read-user-string (getf-post "user" postlines)))
    ; output remaining tags
    (format s "Require tag: <br>~%")
    (output-tag-selector (set-difference (instances 'topic *repository*) tags) s)
    (format s "~&</td><td valign=\"top\" align=\"right\">")
    ; output choice of user ID
    (format s "Login ID: <br>~%")
    (output-user-selector (instances 'person *repository*) user s)
    (format s "<br>~%")
    (format s "<br></td></tr>~%")
    ; construct results
    (setq query (make-tags-query tags))
    (if query
	(setq papers (viewfinds (car (head-args 1)) (make-secure-query query user) *repository*))
	(setq papers (viewfinds '?x (make-secure-query '(paper.instance ?x) user) *repository*)))
    (setq papers (sortem papers (find-sorter 'paper) 'ascending))    
    (setq numpapers (length papers))
    (multiple-value-setq (low high atatime) (construct-current-indices postlines numpapers))   
    ;(format t "Low: ~A, High: ~A, At a Time: ~A~%" low high atatime)
    ; output results navigation
    (format s "<tr><td colspan=\"3\">~%")
    (format s "<center>")
    (format s "<hr width=\"80%\" />")
    (output-tag-results-navigation low high atatime numpapers s)
    (format s "<hr width=\"80%\" />")
    (format s "</center>~%")
    ; output references
    (do ((p papers (cdr p))
	 (i 1 (1+ i)))
	((> i high))
      (when (>= i low)
	(output-paper-html (car p) s)))
    ; output current tags in order, hidden from user
    (format s "<input type=\"hidden\" name=\"tagsequence\" value=\"~A\">" 
	    (apply #'stringappend (mapcar #'(lambda (x) (format nil "~A~%" x)) tags)))
    (format s "<center>")
    (format s "<hr width=\"80%\" />")
    (output-tag-results-navigation low high atatime numpapers s)
    (format s "</center>")
    (format s "</td></tr></table></form>")))

(defun make-secure-query (p sender)
  (cond ((atomicp p) (makand p `(not (deny ,sender ,(first (vars p))))))
	((find (car p) '(forall exists)) 
	 (list* (first p) (second p) (make-secure-query (third p) sender)))
	(t (cons (first p) (mapcar #'(lambda (x) (make-secure-query x sender)) 
				   (cdr p))))))

(defun construct-current-indices (postlines numpapers)
  ; 3 cases: clicked next/previous, clicked update, did something else
  ;  next, previous commands include unique indices and atatime
  ;  update sets low/high/atatime using the boxes, or defaults if values unreasonable.
  ;  doing something else resets indices
  (let (atatime low high)
    (case (read-user-string (getf-post "operation" postlines))
      (fixed (setq low (read-user-string (getf-post "fixedlow" postlines)))
	     (setq atatime (read-user-string (getf-post "fixedatatime" postlines)))
	     (setq high (min (1- (+ low atatime)) numpapers)))
      (display (setq low (read-user-string (getf-post "low" postlines)))
	      (when (not (numberp low)) (setq low 1))
	      (setq atatime (read-user-string (getf-post "atatime" postlines)))
	      (when (not (numberp atatime))
		(setq atatime (- low numpapers)))
	      (setq high (min numpapers (1- (+ low atatime)))))
      (otherwise (setq low (min 1 numpapers) high (min 20 numpapers) atatime 20)))
    (values low high atatime)))


(defun output-tag-results-navigation (low high atatime numpapers s)
  (when (> low 1)
    (format s "<a href=\"javascript:setValue('operation','fixed');setValue('fixedlow',~A);setValue('fixedatatime', ~A);submit('navform')\">" 
	    (max 1 (- low atatime)) atatime)
    (format s "Previous</a>&nbsp;&nbsp;&nbsp;"))
  (format s "Displaying ")
  (format s "<input type=\"text\" name=\"low\" value=\"~A\" size=\"3\" " low)
  (format s "onclick=\"setValue('operation','display'); return submitOnEnter(event,'navform')\">")
  (format s " through ~A of ~A in groups of " high numpapers)
  (format s "<input type=\"text\" name=\"atatime\" size=\"3\" value=\"~A\" " atatime)
  (format s "onclick=\"setValue('operation','display'); return submitOnEnter(event,'navform')\">")
  (format s "<input type=\"hidden\" name=\"operation\" id=\"operation\" value=\"display\">")
  (format s "<input type=\"hidden\" name=\"operationtarget\" id=\"operationtarget\" value=\"\">")
  (format s "<input type=\"hidden\" name=\"fixedlow\" id=\"fixedlow\" value=\"\">")
  (format s "<input type=\"hidden\" name=\"fixedatatime\" id=\"fixedatatime\" value=\"\">")
  (when (< high numpapers)
    (format s "&nbsp;&nbsp;&nbsp;")
    (format s "<a href=\"javascript:setValue('operation','fixed');setValue('fixedlow',~A);setValue('fixedatatime',~A);submit('navform')\">" 
	    (min numpapers (+ low atatime)) atatime)
    (format s "Next</a>")))

(defun output-tags-state (tags s)
  (when tags
    (do ((a tags (cdr a))) ((null a))
      (if (cdr a) 
	  (output-tag-deleteafter (car a) s)
	  (output-tag (car a) s))
      (output-tag-delete (car a) s)
      (when (cdr a) (format s " > ")))))

(defun output-tag-deleteafter (tag s)
  (format s "<a href=\"javascript:setValue('operation','deleteafter');setValue('operationtarget','~A');submit('navform')\">" tag)
  (output-tag tag s)
  (format s "</a>")) 

(defun output-tag-delete (tag s)
  (format s "<a href=\"javascript:setValue('operation','delete');setValue('operationtarget','~A');submit('navform')\"><img src=\"/docserver/infomaster/images/delete.gif\" border=\"0\"></a>" tag))

(defun output-tag-selector (tags s)
  (setq tags (sortem tags (find-sorter 'topic) 'ascending))
  (format s "<select name=\"newtag\" id=\"newtag\" ")
  (format s "onchange=\"setValue('operation','add'); ")
  (format s "if(copyValue('operationtarget','newtag')) {submit('navform');}\">~%")
  (dolist (a (cons 'unknown tags))
    (format s "<option value=\"~A\">~A</option>~%" a (get-topic-printname a)))
  (format s "</select>~%"))

(defun output-user-selector (users user s)
  (setq users (mapcar #'(lambda (x) (cons x (prettyname x))) users))
  (setq users (sort users #'string< :key #'cdr))
  (format s "<select name=\"user\" id=\"user\" ")
  (format s "onchange=\"submit('navform');\">")
  (dolist (a (cons '(unknown . "") users))
    (if (eq (car a) user)
	(format s "<option value=\"~A\" selected>~A</option>~%" (car a) (cdr a))
	(format s "<option value=\"~A\">~A</option>~%" (car a) (cdr a))))
  (format s "</select>~%"))


(defun process-displayreferences-old (s)
  (let (papers sorter)
    (setq sorter (find-sorter 'paper))
    (setq papers (sortem (instances 'paper *gui*) sorter 'ascending))
    (format-html s) (crlf s)
    (format-head s)
    (format s "<title>References</title>")
    (format s "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/references.css\">")
    (format s "~A" (showhidescript))
    (finish-head s) (crlf s)
    (format-body s *bgcolor*) (crlf s)
    (output-header s)
    (output-references s papers)
    (output-footer s)
    (finish-body s) (crlf s)
    (finish-html s) (crlf s)))

(defun output-references (s papers)
  (dolist (p papers)
    (output-paper-html p s)))

;;;;;;;;;;;;;;;;;;;;;;;;; Bibtex and LaTeX ;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod process (s (file (eql 'displayBibtexList)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post-all "paper" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaybibtexlist s dum))
          ((and (setq dum (getf-post-all "tag" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaybibtexlist s (remove-duplicates (find-papers-for-tags dum))))
          (t (process-displaybibtexlist s (instances 'paper *gui*))))))

(defmethod process (s (file (eql 'displayBibtex)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post-all "paper" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaybibtex s dum))
          ((and (setq dum (getf-post-all "tag" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaybibtex s (remove-duplicates (find-papers-for-tags dum))))
          (t (process-displaybibtex s (instances 'paper *gui*))))))

(defmethod process (s (file (eql 'displayLatex)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post-all "paper" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaylatex s dum))
          ((and (setq dum (getf-post-all "tag" postlines)) (setq dum (mapcar #'read-user-string dum)))
           (process-displaylatex s (remove-duplicates (find-papers-for-tags dum))))
          (t (process-displaylatex s (instances 'paper *gui*))))))

(defmethod process (s (file (eql 'displayAuthorLatex)) postlines)
  (let (dum)
    (cond ((and (setq dum (getf-post-all "object" postlines)) (setq dum (mapcar #'read-user-string dum)))
	   (process-displayauthorlatex s dum))
          (t (process-displaylatex s (instances 'paper *gui*))))))

(defun find-papers-for-tags (tags)
  (let (q)
    (setq q (make-tags-query tags))
    (if q (viewfinds (first (vars q)) q *repository*) nil)))   

(defun find-papers-for-topic (topic)
  (let (hier papers)
    (setq hier (find-hier-start *displayhier* topic))
    (multiple-value-setq (hier papers) (populate-display-hierarchy hier))
    papers))


(defun getf-post-all (key postlines)
  (let (result)
    (dolist (p postlines (nreverse result))
      (when (string-equal key (car p))
        (push (cdr p) result)))))

(defun process-displaybibtexlist (s papers)
  (format-html s) (crlf s)
  (format-body s "white") (crlf s)
  (format s "<pre>~%")
  (setq papers (sortem papers (find-sorter 'paper) 'ascending))
  (when (doublep 'paper.instance (car papers) *repository*)
    (format s "~(~A~)" (car papers)))
  (dolist (p (cdr papers))
    (when (doublep 'paper.instance p *repository*)
      (format s ",~(~A~)" p)))
  (format s "~%</pre>")
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-displaybibtex (s papers)
  (format-html s) (crlf s)
  (format-body s "white") (crlf s)
  (format s "<pre>~%")
  (dolist (p (sortem papers (find-sorter 'paper) 'ascending))
    (when (doublep 'paper.instance p *repository*)
      (output-paper-bibtex p s)
      (format s "~%")))
  (format s "</pre>")
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun process-displayauthorlatex (s authors)
  (let (ref unref)
    (setq ref (remove-duplicates (mapcan #'find-auth-refereed authors)))
    (setq unref (mapcan #'(lambda (auth) (viewfinds '?x `(paper.author ?x ,auth) *repository*)) authors))
    (setq unref (remove-duplicates (set-difference unref ref)))
    (format s "<p><h3>Refereed</h3>~%~%")
    (process-displaylatex s ref)
    (format s "<p><h3>Unrefereed</h3>~%~%")
    (process-displaylatex s unref)))

(defun process-displaylatex (s papers)
  (format-html s) (crlf s)
  (format-body s "white") (crlf s)
  (format s "<pre>~%")
  (format s "\\begin{enumerate}[itemsep=0pt]~%")
  (dolist (p (sortem papers (find-sorter 'paper) 'descending))
    (when (doublep 'paper.instance p *repository*)
      (format s "\\item ")
      (output-paper-latex p s)
      (format s "~%")))
  (format s "\\end{itemize}~%")
  (format s "</pre>")
  (finish-body s) (crlf s)
  (finish-html s) (crlf s))

(defun translate-database-schema (th transth vocab)
  (let (tmp newth a)
    (setq tmp (make-instance 'theory))
    (includes tmp th)
    (includes tmp transth)
    (setq newth nil)
    (dolist (v (remove-if-not #'isrelation vocab) (nreverse newth))
      (cond ((= (parameter-arity v) 0)
             (setq a (parameter-symbol v)))
            (t
             (dotimes (i (parameter-arity v))
               (push (tosymbol (format nil "?~A" i)) a))
             (setq a (cons (parameter-symbol v) (nreverse a)))
             (setq newth (nconc (viewfinds a a newth) newth)))))))

;;;;;;;;;;;;;;;;;;;;;;;;; Template Access (mainly to Papers) ;;;;;;;;;;;;;;;;;;;;;;;;;

; http://localhost:5000/references/filltemplate?file=infoserver/examples/researchmaster/pages/publications.tmpl
(defmethod process (s (file (eql 'filltemplate)) postlines)
  (let (name)
    (setq name (filify (getf-post "file" postlines)))
    (cond ((not (probe-file name)) (http-problem s "No such template"))
	  (t (princ (cl-emb:execute-emb (pathname name)) s)))))

(defun find-my-refereed () (find-auth-refereed 'timothylhinrichs))
(defun find-my-unrefereed () (find-auth-unrefereed 'timothylhinrichs))

(defun find-auth-refereed (auth)
  (sortem (prorequest `(ask-all ?x (and (paper.author ?x ,auth) (paper.publication ?x ?pub))))
	  (find-sorter 'paper) 'descending))

(defun find-auth-unrefereed (auth)
  (sortem (set-difference (prorequest `(ask-all ?x (paper.author ?x ,auth)))
			  (find-auth-refereed auth))
	  (find-sorter 'paper) 'descending))


;;;;;;;;;;;;;;;;;;;;;;;;; Papers in HTML ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-paper-html (handle s &key (minimal nil) 
			  (imageurl "/docserver/infoserver/examples/researchmaster/") (shortenauthor t) )
  "(OUTPUT-PAPER-HTML HANDLE S MINIMAL) prints an HTML depiction of the paper with id HANDLE to stream S.
   If MINIMAL is true, doesn't output additional functionality."
  (let (handles author title year bibtex link description booktitle publisher publication volume number startpage endpage rating related tags (*namer* 'prettyname) award experiments)
    (setq handles (format nil "~(~A~)" handle))
    (setq author (prorequest `(ask-all ?x (paper.author ,handle ?x))))
    (setq title (prorequest `(ask-one ?x (paper.title ,handle ?x))))
    (if (stringp title) (setq title (apply #'stringappend (split-string title '(#\{ #\})))) (setq title ""))
    (setq year (prorequest `(ask-one ?x (paper.year ,handle ?x))))
    (setq bibtex (prorequest `(ask-one ?x (paper.bibtex ,handle ?x))))
    (setq link (prorequest `(ask-one ?x (paper.url ,handle ?x))))
    (unless link (setq link (urlify (format nil "javascript:window.location='search?q=~A'" title))))
    (setq description (prorequest `(ask-one ?x (paper.description ,handle ?x))))
    (setq booktitle (prorequest `(ask-one ?x (paper.booktitle ,handle ?x))))
    (setq publisher (prorequest `(ask-one ?x (paper.publisher ,handle ?x))))
    (setq publication (prorequest `(ask-one ?x (paper.publication ,handle ?x))))
    (setq volume (prorequest `(ask-one ?x (paper.volume ,handle ?x))))
    (setq number (prorequest `(ask-all ?x (paper.number ,handle ?x))))
    (setq startpage (prorequest `(ask-one ?x (paper.startpage ,handle ?x))))
    (setq endpage (prorequest `(ask-one ?x (paper.endpage ,handle ?x))))
    (setq rating (prorequest `(ask-one ?x (paper.rank ,handle ?x))))
    (setq related (prorequest `(ask-all ?x (paper.related ,handle ?x))))
    (setq award (prorequest `(ask-all ?x (paper.award ,handle ?x))))
    (setq experiments (prorequest `(ask-all ?x (paper.experiment ,handle ?x))))
    (setq tags (prorequest `(ask-all ?x (paper.topic ,handle ?x))))

    (format s "<a name=\"~Ap\"></a><span class=\"paper\">~%" handles)
    (format s "    <table border=\"0\" width=\"100%\">~%")
    (format s "    <tr>~%")
    (format s "        <td width=\"6\" valign=\"top\">~%")
    (format s "            <a href=\"javascript:showDetails('~A')\">~%" handles)
    (format s "               <img src=\"~Aimages/arrowrt.gif\" width=\"21\" height=\"19\" id=\"~AShow\" style=\"display:block;\" border=\"0\"></a>~%" imageurl handles)
    (format s "            <a href=\"javascript:hideDetails('~A')\">~%" handles)
    (format s "               <img src=\"~Aimages/arrowdn.gif\" width=\"21\" height=\"19\" id=\"~AHide\" style=\"display:none\" border=\"0\"></a>~%" imageurl handles)
    (format s "        </td>~%")
    (format s "        <td align=\"left\" class=\"paperentry\">~%")
    (cond ((and (> (length author) 4) shortenauthor)
           (format s "            ")
           (output-paper-html-author (car author) s)
           (format s ", et al.~%"))
          ((null (cdr author))
           (format s "            ")
           (output-paper-html-author (car author) s)
           (format s ".~%"))
          (t
           (do ((as author (cdr as)))
               ((null (cdr as)))
             (format s "            ")
             (output-paper-html-author (car as) s)
             (when (cddr as) (format s ", ")))
           (format s "            and ")
           (output-paper-html-author (car (last author)) s)
           (format s ".~%")))
    (format s "            <a href=\"~A\"><span class=\"paperTitle\">~A</span></a>.~%" link title)
    (output-paper-html-source handle bibtex booktitle publication volume number startpage endpage publisher year s)
    ;(when year (format s "            <span class=\"date\">~A</span>.~%" year))
    ;(format s "            <span class=\"link\">~A</span>~%" link)
    (when award
      (dolist (a award)
	(format s "        <b>~A</b>" a)))
    (unless minimal
      (when rating (output-paper-html-rank rating s))
      (format s "        [<a href=\"displaybibtex?paper=~A\">bibtex</a>,~%" handles)
      (format s "        <a href=\"fastinspectpage?object=~A\">db</a>]~%" handles)
      (when tags (format s "(") (output-taglist s tags) (format s ")")))
    (format s "        </td>~%")
    (format s "    </tr><tr>~%")
    (format s "        <td colspan=\"2\">~%")
    (format s "            <p id=\"~A\" style=\"display:none\"><span class=\"description\">~%" handles)
    (when description (format s "~A" description))
    (when (or experiments related) (format s "<br><br>"))
    (output-paper-html-experiments handle experiments s)
    (output-paper-html-related handle related s)
    (format s "~&            </span><br><br></p>~%")
    (format s "        </td>~%")
    (format s "    </tr>~%")
    (format s "    </table></span>~%")))

(defun output-paper-html-author (author s)
  (output-paper-author author s))

(defun output-paper-author (author s &key (initials nil) (lastfirst nil))
  (let (f m l)
    (setq f (prorequest `(ask-one ?x (person.firstname ,author ?x))))
    (setq m (prorequest `(ask-one ?x (person.middlename ,author ?x))))
    (setq l (prorequest `(ask-one ?x (person.lastname ,author ?x))))
    ; abbreviate
    (when m (setq m (format nil "~A." (subseq m 0 1))))  ; always shorten middle
    (when initials 
      (when f (setq f (format nil "~A." (subseq f 0 1))))) ; abbreviate first name.
    ; output in proper order
    (cond (lastfirst
	   (when l (format s "~A, " l))
	   (when f (format s "~A" f))
	   (when m (format s " ~A" m)))
	  (t
	   (when f (format s "~A" f))
	   (when m (format s " ~A" m))
	   (when l (format s " ~A, " l))))))

(defun output-paper-html-experiments (handle experiments s)
  (declare (ignore handle))
  (when experiments
    (format s "~&Experiments:") 
    (dolist (e experiments)
      (format s "<a href=\"~A\">~A</a>" e e))))

(defun output-paper-html-related (handle related s)
  (declare (ignore handle))
  (when related
    (format s "~&Related: ")
    (dolist (r related)
      (format s "[<a href=\"#~(~A~)p\">~(~A~)</a>] " r r))
    (when related (format s "~%"))))

(defun output-paper-html-rank (num s)
  (format s "            <span class=\"rank\">[")
  (dotimes (i num) (format s "*"))
  (format s "]</span>~%" ))

(defun output-paper-html-source (handle bibtex booktitle publication volume number startpage endpage publisher year s)
  (declare (ignore handle))
  (setq bibtex (compute-pub-prefix bibtex))
  (cond (booktitle (setq publication (stringappend bibtex booktitle)))
        (publication (let ((short (prorequest `(ask-one ?x (shortname ,publication ?x)))))
		       (setq publication (stringappend bibtex (iconify publication)))
		       (when short (setq publication (stringappend publication " (" short ")"))))))
  (format s "            <span class=\"source\">~%")
  (when publication (format s "                <i>~A</i>~A~A" publication (if (or volume number) " " "") (if volume volume "")))
  (when publication (output-pub-number number s))  
  (when publication (format s "~A~%" (if (or startpage endpage publisher year) "," ".")))
  (when (or startpage endpage) (format s "                pp. ~A-~A~A~%" (if startpage startpage '?) (if endpage endpage '?) (if (or publisher year) "," ".")))
  (when publisher (format s "                ~A~A~%" (iconify publisher) (if year "," ".")))
  (when year (format s "                <span class=\"date\">~A</span>.~%" year))
  (format s "            </span>~%")
  (format s ""))

(defun output-pub-number (nums s)
  (cond ((null nums) "")
        ((null (cdr nums)) (format s "(~A)" (car nums)))
        ((null (cddr nums)) (format s "(~A-~A)" (car nums) (second nums)))
        (t (format s "(~A" (car nums))
           (dolist (v (cdr nums))
             (format s ",~A" v))
           (format s ")"))))

(defun compute-pub-prefix (bibtex)
  (case bibtex
    (inproceedings "In Proceedings of the ")
    (inbook "In ")
    (otherwise "")))
    
;;;;;;;;;;;;;;;;;;;;;;;;; Papers in HTML ;;;;;;;;;;;;;;;;;;;;;;;;;


(defun output-paper-latex (handle s &key (minimal nil) (shortenauthor t) )
  "(OUTPUT-PAPER-HTML HANDLE S MINIMAL) prints an HTML depiction of the paper with id HANDLE to stream S.
   If MINIMAL is true, doesn't output additional functionality."
  (let (author title year bibtex booktitle publisher publication volume number startpage endpage  (*namer* 'prettyname) award)
    (setq author (prorequest `(ask-all ?x (paper.author ,handle ?x))))
    (setq title (prorequest `(ask-one ?x (paper.title ,handle ?x))))
    (if (stringp title) (setq title (apply #'stringappend (split-string title '(#\{ #\})))) (setq title ""))
    (setq year (prorequest `(ask-one ?x (paper.year ,handle ?x))))
    (setq bibtex (prorequest `(ask-one ?x (paper.bibtex ,handle ?x))))
    (setq booktitle (prorequest `(ask-one ?x (paper.booktitle ,handle ?x))))
    (setq publisher (prorequest `(ask-one ?x (paper.publisher ,handle ?x))))
    (setq publication (prorequest `(ask-one ?x (paper.publication ,handle ?x))))
    (setq volume (prorequest `(ask-one ?x (paper.volume ,handle ?x))))
    (setq number (prorequest `(ask-all ?x (paper.number ,handle ?x))))
    (setq startpage (prorequest `(ask-one ?x (paper.startpage ,handle ?x))))
    (setq endpage (prorequest `(ask-one ?x (paper.endpage ,handle ?x))))
    (setq award (prorequest `(ask-all ?x (paper.award ,handle ?x))))

    (cond ((and (> (length author) 4) shortenauthor)
           (output-paper-latex-author (car author) s)
           (format s ", et al."))
          ((null (cdr author))
           (output-paper-latex-author (car author) s))
          (t
           (do ((as author (cdr as)))
               ((null (cdr as)))
             (output-paper-latex-author (car as) s)
             (when (cddr as) (format s ", ")))
           (format s " and ")
           (output-paper-latex-author (car (last author)) s)))
    (format s ": {\\em ~A}, " title)
    (output-paper-latex-source handle bibtex booktitle publication volume number startpage endpage publisher year s)
    (unless minimal
      (when award
	(dolist (a award)
	  (format s " ~A. " a))))))

(defun output-paper-latex-author (author s)
  (output-paper-author author s :initials t :lastfirst t))

(defun output-paper-latex-source (handle bibtex booktitle publication volume number startpage endpage publisher year s)
  (declare (ignore handle))
  (setq bibtex (compute-pub-prefix bibtex))
  (cond (booktitle (setq publication (stringappend bibtex booktitle)))
        (publication (let ((short (prorequest `(ask-one ?x (shortname ,publication ?x)))))
		       (setq publication (stringappend bibtex (iconify publication)))
		       (when short (setq publication (stringappend publication " (" short ")"))))))
  (when publication (format s "~A~A~A" publication (if (or volume number) " " "") (if volume volume "")))
  (when publication (output-pub-number number s))  
  (when publication (format s "~A " (if (or startpage endpage publisher year) "," ".")))
  (when (or startpage endpage) (format s "pp. ~A-~A~A " (if startpage startpage '?) (if endpage endpage '?) (if (or publisher year) "," ".")))
  (when publisher (format s " ~A~A~%" (iconify publisher) (if year "," ".")))
  (when year (format s "~A.~%" year)))



;;;;;;;;;;;;;;;;;;;;;;;;; Paper in Bibtex ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-pub-prefix-bibtex (bibtex)
  (case bibtex
    (inproceedings "Proceedings of the ")
    (inbook "In ")
    (otherwise "")))

(defun output-paper-bibtex (handle s)
  (let (bibtex)
    (setq bibtex (result 'paper.bibtex handle *repository*))
    (format s "@~(~A~){~(~A~),~%" (if bibtex bibtex 'misc) handle)
    (output-paper-bibtex-body handle bibtex s)
    (format s "~&}~%")))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'article)) s)
  (let (vol num beg end)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'journal (result 'paper.publication handle *repository*) s 
			       :pre (compute-pub-prefix-bibtex bibtex))
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s)

    ; optional fields
    (setq vol (result 'paper.volume handle *repository*))
    (setq num (results 'paper.number handle *repository*))
    (setq beg (result 'paper.startpage handle *repository*))
    (setq end (result 'paper.endpage handle *repository*))

    (when (or vol num beg end) (output-paper-bibtex-comma s))
    (when vol
      (output-paper-bibtex-value 'volume vol s)
      (when (or num beg end) (output-paper-bibtex-comma s)))
    (when num
      (output-paper-bibtex-number num s)
      (when (or beg end) (output-paper-bibtex-comma s)))
    (when (or beg end)
      (output-paper-bibtex-pages beg end s))))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'book)) s)
  (let (vol)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'publisher (result 'paper.publisher handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s)

    ; optional fields
    (setq vol (result 'paper.volume handle *repository*))

    (when vol (output-paper-bibtex-comma s))
    (when vol
      (output-paper-bibtex-value 'volume vol s))))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'inbook)) s)
  (let (vol beg end)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'publisher (result 'paper.publisher handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s)

    ; optional fields (actually, pages are not optional, but often missing)
    (setq vol (result 'paper.volume handle *repository*))
    (setq beg (result 'paper.startpage handle *repository*))
    (setq end (result 'paper.endpage handle *repository*))

    (when (or vol beg end) (output-paper-bibtex-comma s))
    (when vol
      (output-paper-bibtex-value 'volume vol s)
      (when (or beg end) (output-paper-bibtex-comma s)))
    (when (or beg end)
      (output-paper-bibtex-pages beg end s))))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'incollection)) s)
  (let (pub beg end booktitle)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s)
    (output-paper-bibtex-comma s)
    (setq booktitle (result 'paper.booktitle handle *repository*))
    (when (not booktitle) (setq booktitle (result 'paper.publication handle *repository*)))
    (output-paper-bibtex-value 'booktitle booktitle s)

    ; optional fields (actually, pages are not optional, but often missing)
    (setq pub (result 'paper.publisher handle *repository*))
    (setq beg (result 'paper.startpage handle *repository*))
    (setq end (result 'paper.endpage handle *repository*))

    (when (or pub beg end) (output-paper-bibtex-comma s))
    (when pub
      (output-paper-bibtex-value 'publisher pub s)
      (when (or beg end) (output-paper-bibtex-comma s)))
    (when (or beg end)
      (output-paper-bibtex-pages beg end s))))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'inproceedings)) s)
  (let (pub beg end)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'booktitle (result 'paper.publication handle *repository*) s
			       :pre (compute-pub-prefix-bibtex bibtex))
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s)

    ; optional fields
    (setq pub (result 'paper.publisher handle *repository*))
    (setq beg (result 'paper.startpage handle *repository*))
    (setq end (result 'paper.endpage handle *repository*))

    (when (or pub beg end) (output-paper-bibtex-comma s))
    (when pub
      (output-paper-bibtex-value 'publisher pub s)
      (when (or beg end) (output-paper-bibtex-comma s)))
    (when (or beg end)
      (output-paper-bibtex-pages beg end s))))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'misc)) s)
  (let (ath tit how yea)
    ; optional fields
    (setq ath (results 'paper.author handle *repository*))
    (setq tit (result 'paper.title handle *repository*))
    (setq how (result 'paper.url handle *repository*))
    (setq yea (result 'paper.year handle *repository*))

    (when ath
      (output-paper-bibtex-authors ath s)
      (when (or tit how yea) (output-paper-bibtex-comma s)))
    (when tit
      (output-paper-bibtex-value 'title tit s)
      (when (or how yea) (output-paper-bibtex-comma s)))
    (when how
      (output-paper-bibtex-value 'howpublished how s)
      (when yea (output-paper-bibtex-comma s)))
    (when yea
      (output-paper-bibtex-value 'year yea s))))


(defmethod output-paper-bibtex-body (handle (bibtex (eql 'phdthesis)) s)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'school (result 'paper.publisher handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s))

(defmethod output-paper-bibtex-body (handle (bibtex (eql 'techreport)) s)
    ; required fields
    (output-paper-bibtex-authors (results 'paper.author handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'title (result 'paper.title handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'institution (result 'paper.publisher handle *repository*) s)
    (output-paper-bibtex-comma s)
    (output-paper-bibtex-value 'year (result 'paper.year handle *repository*) s))

(defmethod output-paper-bibtex-body (handle bibtex s)
  (let (aut tit psh pub boo vol num beg end url yea)

    ; output everything bibtex knows about
    (setq aut (results 'paper.author handle *repository*))
    (setq tit (result 'paper.title handle *repository*))
    (setq psh (result 'paper.publisher handle *repository*))
    (setq pub (result 'paper.publication handle *repository*))
    (setq boo (result 'paper.booktitle handle *repository*))
    (setq vol (result 'paper.volume handle *repository*))
    (setq num (results 'paper.number handle *repository*))
    (setq beg (result 'paper.startpage handle *repository*))
    (setq end (result 'paper.endpage handle *repository*))
    (setq url (result 'paper.url handle *repository*))
    (setq yea (result 'paper.year handle *repository*))

    (when aut
      (output-paper-bibtex-authors aut s)
      (when (or tit psh pub boo vol num beg end url yea) (output-paper-bibtex-comma s)))
    (when tit
      (output-paper-bibtex-value 'title tit  s)
      (when (or psh pub boo vol num beg end url yea) (output-paper-bibtex-comma s)))
    (when psh
      (output-paper-bibtex-value 'publisher psh  s)
      (when (or pub boo vol num beg end url yea) (output-paper-bibtex-comma s)))
    (when pub
      (output-paper-bibtex-value 'publication pub :pre (compute-pub-prefix-bibtex bibtex)  s)
      (when (or boo vol num beg end url yea) (output-paper-bibtex-comma s)))
    (when boo
      (output-paper-bibtex-value 'booktitle boo  s)
      (when (or vol num beg end url yea) (output-paper-bibtex-comma s)))
    (when vol
      (output-paper-bibtex-value 'volume vol s)
      (when (or num beg end url yea) (output-paper-bibtex-comma s)))
    (when num
      (output-paper-bibtex-number num s)
      (when (or beg end url yea) (output-paper-bibtex-comma s)))
    (when (or beg end)
      (output-paper-bibtex-pages beg end s)
      (when (or url yea) (output-paper-bibtex-comma s)))
    (when url
      (output-paper-bibtex-value 'url url s)
      (when yea (output-paper-bibtex-comma s)))
    (when yea
      (output-paper-bibtex-value 'year yea  s))))

(defun output-paper-bibtex-authors (authors s) 
  (cond ((null authors))
        (t (format s "    author = \"")
           (output-paper-bibtex-author (car authors) s)
           (dolist (v (cdr authors))
             (format s " and ")
             (output-paper-bibtex-author v s))
           (format s "\""))))
             
(defun personname (p)
  (let (f m l)
    (setq f (result 'person.firstname p *repository*))
    (setq m (result 'person.middlename p *repository*))
    (setq l (result 'person.lastname p *repository*))
    (format nil "~A~A~A~A~A" (if f f "") (if f " " "") (if m m "") (if m " " "") (if l l ""))))

(defun output-paper-bibtex-author (author s)
  (let (f m l)
    (setq f (result 'person.firstname author *repository*))
    (setq m (result 'person.middlename author *repository*))
    (setq l (result 'person.lastname author *repository*))
    (when f (format s "~A " f))
    (when m (format s "~A. " (subseq m 0 1)))
    (when l (format s "~A" l))))

(defun output-paper-bibtex-value (key value s &key (pre "") (post ""))
  (when value
    (format s "    ~(~A~) = \"~A~A~A\"" key pre (prettyname value) post)))

(defun output-paper-bibtex-comma (s)
  (format s ",~%"))

(defun output-paper-bibtex-number (nums s) 
  (when nums
    (format s "    number = \"")
    (cond ((null (cdr nums)) (format s "~A" (car nums)))
          ((null (cddr nums)) (format s "~A-~A" (car nums) (second nums)))
          (t (format s "(~A" (car nums))
             (dolist (v (cdr nums))
               (format s ",~A" v))
             (format s ")")))
    (format s "\"")))

(defun output-paper-bibtex-pages (beg end s)
  (when (or beg end)
    (unless beg (setq beg '?))
    (unless end (setq end '?))
    (format s "    pages = \"~A--~A\"" beg end)))



#|
(defun output-paper-bibtex (handle s)
  (let (handles author bibtex title year link publisher publication volume number startpage endpage (*namer* 'prettyname))
    (setq handles (format nil "~(~A~)" handle))
    (setq author (prorequest `(ask-all ?x (paper.author ,handle ?x))))
    (setq bibtex (prorequest `(ask-one ?x (paper.bibtex ,handle ?x))))
    (setq title (prorequest `(ask-one ?x (paper.title ,handle ?x))))
    (setq year (prorequest `(ask-one ?x (paper.year ,handle ?x))))
    (setq link (prorequest `(ask-one ?x (paper.url ,handle ?x))))
    (setq publisher (prorequest `(ask-one ?x (paper.publisher ,handle ?x))))
    (setq publication (prorequest `(ask-one ?x (paper.publication ,handle ?x))))
    (setq volume (prorequest `(ask-one ?x (paper.volume ,handle ?x))))
    (setq number (prorequest `(ask-one ?x (paper.number ,handle ?x))))
    (setq startpage (prorequest `(ask-one ?x (paper.startpage ,handle ?x))))
    (setq endpage (prorequest `(ask-one ?x (paper.endpage ,handle ?x))))

    (format s "@~(~A~){~A,~%" bibtex handles)
    (when author
      (format s "    author=\"")
      (output-paper-bibtex-author (car author) s)
      (do ((as (cdr author) (cdr as)))
          ((null (cdr as)))
        (format s " and ")
        (output-paper-bibtex-author (car as) s))
      (format s "\",~%"))
    (when title (format s "    title=\"~A\",~%" title))
    (when link (format s "    url=\"~A\",~%" link))
    (when publisher (format s "    publisher=\"~A\",~%" (iconify publisher)))
    (when publication (format s "    publication=\"~A\",~%" (iconify publication)))
    (when volume (format s "    volume=\"~A\",~%" volume))
    (when number (format s "    number=\"~A\",~%" number))
    (when (and startpage endpage) (format s "    pages=\"~A--~A\",~%" startpage endpage))
    (when year (format s "    year=\"~A\"~%" year))
    (format s "}~%")))
|#




(defun showhidescript ()
"<SCRIPT LANGUAGE=\"JavaScript\">
function showDetails(divID) {
   var strShow = divID+'Show';
   var strHide = divID+'Hide';

   if (document.getElementById) {  /* Netscape */
       document.getElementById(divID).style.display = \"block\";
       document.getElementById(strShow).style.display = \"none\";
       document.getElementById(strHide).style.display = \"block\";
   } else if (document.all) {  /* IE */
       document.all[divID].style.display = \"block\";
       document.all[strShow].style.display = \"none\";
       document.all[strHide].style.display = \"block\"; }}

function hideDetails(divID) {
   var strShow = divID+'Show';
   var strHide = divID+'Hide';

   if (document.getElementById) { /* Netscape */
       document.getElementById(divID).style.display = \"none\";
       document.getElementById(strShow).style.display = \"block\";
       document.getElementById(strHide).style.display = \"none\";
   } else if (document.all) {  /* IE */
       document.all[divID].style.display = \"none\";
       document.all[strShow].style.display = \"block\";
       document.all[strHide].style.display = \"none\"; }}
</SCRIPT>")


;;;;;;;;;;;;;;;;;;;;;;; Old Data Manipulation ;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun cleanpapers (data)
  (let (cleaned)
    (setq cleaned nil)
    (dolist (p data (nreverse cleaned))
      (cond ((eq (car p) 'paper.author) (setq cleaned (nconc (cleanauthor p) cleaned)))
            ((eq (car p) 'paper.publisher) (setq cleaned (nconc (cleanpublisher p) cleaned)))
            ((eq (car p) 'paper.publication) (setq cleaned (nconc (cleanpublication p) cleaned)))
            ((eq (car p) 'paper.volume) (setq cleaned (nconc (cleanvolume p) cleaned)))
            ((eq (car p) 'paper.topic) (setq cleaned (nconc (cleantopic p) cleaned)))
            ((eq (car p) 'paper.rank) (setq cleaned (nconc (cleanrank p) cleaned)))
            (t (setq cleaned (cons p cleaned)))))))

(defun cleantopic (p)
  (if (= (length (third p)) 0)
    nil
    (let (newname)
      (setq newname (text2key (third p)))
      (list `(topic.instance ,newname)
            `(prettyname ,newname ,(third p))
            `(paper.topic ,(second p) ,newname)))))

(defun cleanpublisher (p)
  (if (= (length (third p)) 0)
    nil
    (let (newname)
      (setq newname (text2key (third p)))
      (list `(publisher.instance ,newname)
            `(prettyname ,newname ,(third p))
            `(paper.publisher ,(second p) ,newname)))))

(defun cleanpublication (p)
  (if (= (length (third p)) 0)
    nil
    (let (newname)
      (setq newname (text2key (third p)))
      (list `(publication.instance ,newname)
            `(prettyname ,newname ,(third p))
            `(paper.publication ,(second p) ,newname)))))

(defun text2key (s)
  (tosymbol (delete-if #'(lambda (x) (member x '(#\. #\Tab #\Space #\Newline #\, #\' #\:))) s)))

(defun cleanvolume (p)
  (let (parsed vol num)
    (setq parsed (split (third p) nil '(#\( #\) #\Space #\:)))
    (setq vol (tosymbol (first parsed)))
    (setq num (member-if #'(lambda (x) (> (length (trimwhite x)) 0)) (cdr parsed)))
    (cond (num
           (list `(paper.volume ,(second p) ,vol)
                 `(paper.number ,(second p) ,(tosymbol (first num)))))
          (t
           (list `(paper.volume ,(second p) ,vol))))))

(defun cleanauthor (p)
  (let (id authors result as)
    (setq id (second p))
    (setq authors (split-authors (third p)))
    (setq result nil)
    (dolist (a authors result)
      (setq as (buildauthor a))
      (dolist (b (uniquify (mapcar #'second as)))
        (setq result (cons `(paper.author ,id ,b) result)))
      (setq result (nconc result as)))))

(defun split-authors (s)
  (delete-if #'(lambda (x) (= (length x) 0)) (mapcar #'trimwhite (split-on s '("and" ",")))))

(defun buildauthor (s)
  "(BUILDAUTHOR S) returns kif that represents the author passed in as a string."
  ; split based on whitespace
  (let (slist)
    (setq slist (split s))
    (setq s (text2key s))
    
    ; the first is the first name, the last is the last name; the rest is the middle name.
    (list `(person.instance ,s) 
          `(person.firstname ,s ,(first slist))
          `(person.lastname ,s ,(first (last slist)))
          `(person.middlename ,s ,(apply #'concatenate 'string (cdr (butlast slist)))))))

  
(defun cleanrank (p)
  (list `(paper.rank ,(second p) ,(- (length (trimwhite (third p))) 2))))

(defun trimwhite (s) (string-trim '(#\Space #\Tab #\Newline) s))
|#

(defun split-on (string items)
  (setq items (mapcar #'(lambda (x) (cons x (length x))) items))
  (nreverse
   (let ((list nil) (start 0) (end t))
     (do () 
         ((not end) (push (subseq string start nil) list))
       (setq end nil)
       (setq end (mapcar #'(lambda (x) (cons (search (car x) string :start2 start) (cdr x))) items))
       ; find minimum position
       (setq end (first (sort end 
                              #'(lambda (x y) (cond ((not x) (if y nil t))
                                                    ((not y) t)
                                                    (t (< x y))))
                              :key #'car)))
       (when (car end)
         (push (subseq string start (car end)) list)
         (setq start (+ (cdr end) (car end))))
       (setq end (car end))))))

(defun split-string (string &optional (ws '(#\Space #\Tab)) max)
  "Split `string' along whitespace as defined by the sequence `ws'.
Whitespace which causes a split is elided from the result.  The whole
string will be split, unless `max' is provided, in which case the
string will be split into `max' tokens at most, the last one
containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))

;;; this and that may have different behaviour on strings with
;;; repeated whitespace -- e.g. "foo  bar"

(defun split-quoted (str &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws',
but ignoring whitespace in quoted strings.  Whitespace which causes a
split is elided from the result.  The whole string will be split,
unless `max' is a non-negative integer, in which case the string will
be split into `max' tokens at most, the last one containing the whole
rest of the given `string', if any."
  (do ((i 0 (1+ i))
       (words '())
       (split-allowed-p t)
       (word '()))
      ((>= i (length str))
       (reverse (cons (coerce (reverse word) 'string) words)))
    (if (eql (elt str i) #\")
        (setf split-allowed-p (not split-allowed-p)))
    (if (eql (elt str i) #\\)
        (setf i (1+ i)))                ;advance past escape chars
    (if (and split-allowed-p
             (or (not max) (< (length words) (1- max)))
             (member (elt str i) ws))
        (progn
          (setf words (cons (coerce (reverse word) 'string) words))
          (setf word '()))
      (setf word (cons (elt str i) word)))))

