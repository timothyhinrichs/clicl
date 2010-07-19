
(defun owj-groupbydefined (pc)
  "(OWJ-GROUPBYDEFINED PC) takes a list of (pred . constraint-list) and for each
   predicate, groups the associated constraint list by the set of cells occurring in
   each constraint.  Replaces constraint-list with a list where each element is of the form
   (cellpreds . constraint-list)."
  (mapcar #'(lambda (p)
	      (cons (car p)
		    (group-by (cdr p) 
			#'(lambda (y) (sort (cellpreds (cdr y)) #'string< :key #'string)) 
			:test #'equal))) pc))

(defun owj-addindices (pc)
  "(OWJ-ADDINDICES PC) walks over each constraint in list of (pred . constraint-list) and
   for each predicate turns constraint-list into a list of (i . constraint), where i is 
   the index of constraint in the original pred constraint-list."
  (let (pres res i)
    (setq res nil)
    (dolist (p pc)
      (setq pres nil)
      (setq i 0)
      (dolist (c (cdr p))
	(push (cons i c) pres)
	(setq i (1+ i)))
      (push (cons (car p) (nreverse pres)) res))
    (nreverse res)))


(defun output-websheet-javascript-initbinding (s)
  (format s "function initBinding(val) {~%")
  (format s "   var q = read(\"")
  (print-datalog-atom (car (head-args 1)) s)
  (format s "\");~%")
  (format s "   return acons(q,val,nil); }~%~%"))

(defun output-websheet-javascript-init (struct negpc pospc comp s)
  (let (name first hasneg haspos negs poss negsupports possupports negsupportx possupportx type 
	     mycomp component un)
    (format s "function init () {~%")
    (dolist (o (webform-options struct))
      (format s "   ~(~A~) = ~(~A~);~%" (car o) (cdr o)))
    (format s "   var mycellarray = new Array(")
    (setq first t)
    (dolist (r (webform-widgets struct))
      (when (widget-name r)
	(setq name (websheet-cellname (widget-name r)))
	(unless first (format s ","))
	(setq first nil)
	(setq hasneg (assoc (widget-name r) negpc))
	(setq haspos (assoc (widget-name r) pospc))
	(setq negs (format nil "negs_~A" name) poss (format nil "poss_~A" name))
	(setq possupports (format nil "possupports_~A" name) negsupports (format nil "negsupports_~A" name))
	(setq possupportx (format nil "possupportx_~A" name) negsupportx (format nil "negsupportx_~A" name))
	(setq type (format nil "type_~A" name) un "undefined")
	(setq mycomp (car (find (widget-name r) comp :test #'(lambda (x y) (member x y)))))
	(if mycomp 
	    (setq component (format nil "component_~A" (websheet-cellname mycomp)))
	    (setq component (format nil "empty()")))
	(format s "~%      new cell('~A',~A,~A,~A,~A,~A,~A,~A,~A,'~A')"
		name type 
		(if hasneg negs un) (if haspos poss un) 
		(if hasneg negsupports un) (if haspos possupports un)
		(if hasneg negsupportx un) (if haspos possupportx un)
		component
		(if (widget-init r) (value-shortname (widget-init r)) ""))))
    (format s ");~%")
    (format s "   initspread(mycellarray); }~%~%")))

(defun output-websheet-javascript-types (struct s)
  ; write out one type: var type_p = new Array(val1,val2,...,valn)
  (let (vals)
    (dolist (r (webform-widgets struct))
      (setq vals (widget-type r))
      (format s "var type_~A = new Array(" (websheet-cellname (widget-name r)))
      (when vals
	(format s "'~A'" (value-shortname (car vals)))
	(dolist (c (cdr vals))
	  (format s ", '~A'" (value-shortname c))))
      (format s ");~%~%"))))


; Compute all sets of cells that existentially entail a positive/negative value for a cell
(defun output-websheet-javascript-negsupports (pc s) 
  (output-websheet-javascript-signsupports pc "neg" s))
(defun output-websheet-javascript-possupports (pc s) 
  (output-websheet-javascript-signsupports pc "pos" s))
(defun output-websheet-javascript-signsupports (pc sign s)
  (dolist (p pc)
    (format s "function ~Asupports_~A (bl,justification) {~%" sign (websheet-cellname (car p)))
    (format s "   if (bl == undefined) { bl = nil; }~%")
    (format s "   var ans = {};~%")
    (format s "   var query, v, n, tmp;~%")
    (format s "   rulebase = theory;~%")
    (format s "   var q = read(\"")
    (print-datalog-atom (car (head-args 1)) s)
    (format s "\");~%")
    ;(format s "   var bl = acons(q,cellvalue('~A'),nil);~%" (websheet-cellname (car p)))
    (dolist (grp (cdr p))
      (output-websheet-javascript-evaluationguard (car grp) s)
      (format s "    {~%")
      (dolist (q (cdr grp))
	(format s "     query = queries_~A_~A[~A];~%"
		sign (websheet-cellname (car p)) (car q))
	(format s "     tmp = findsupports(q,query,bl);~%")
	(format s "     if (tmp != false) {~%")
	(format s "        if (justification != undefined) { justification.push(query); }~%")
	(format s "        for (var i = 0; i < tmp[0].length; i++) {~%")
	(format s "           v = ans[tmp[0][i]];~%")
	(format s "           if (v == undefined) { v = empty(); }~%")
	(format s "           n = extractcells(tmp[1][i]);~%")
	(format s "           if (n.length == 0) { n = new Array('~A'); }~%" 
		(websheet-cellname (car p)))
	(format s "           ans[tmp[0][i]] = auniond(v, n); }}~%"))
      (format s "     }~%"))
    (format s "   return ans; }~%~%")))

; Compute a single set of cells that existentially entail a positive/negative value for a cell
(defun output-websheet-javascript-negsupportx (pc s) 
  (output-websheet-javascript-signsupportx pc "neg" s))
(defun output-websheet-javascript-possupportx (pc s) 
  (output-websheet-javascript-signsupportx pc "pos" s))
(defun output-websheet-javascript-signsupportx (pc sign s)
  (dolist (p pc)
    (format s "function ~Asupportx_~A (bl) {~%" sign (websheet-cellname (car p)))
    (format s "   if (bl == undefined) { bl = nil; }~%")
    (format s "   rulebase = theory;~%")
    (format s "   var query, tmp;~%")
    (format s "   var q = read(\"")
    (print-datalog-atom (car (head-args 1)) s)
    (format s "\");~%")
    (format s "   var bl = acons(q,cellvalue('~A'),nil);~%" (websheet-cellname (car p)))
    (dolist (grp (cdr p))
      (output-websheet-javascript-evaluationguard (car grp) s)
      (format s "    {~%")
      (dolist (q (cdr grp))
	(format s "     tmp = findsupportx(q,queries_~A_~A[~A],bl);~%" 
		sign (websheet-cellname (car p)) (car q))
	(format s "     if (tmp != false) {~%")
	(format s "       if (tmp[1].length >= 0) { return new Array('~A'); }~%" 
		(websheet-cellname (car p)))
	(format s "       return auniquify(extractcells(tmp[1])); }~%"))
      (format s "    }~%"))
    (format s "   return empty();}~%~%")))

; Compute all the positive/negative values entailed for a cell
(defun output-websheet-javascript-negs (pc s) 
  (output-websheet-javascript-signs pc "neg" s))
(defun output-websheet-javascript-poss (pc s) 
  (output-websheet-javascript-signs pc "pos" s))
(defun output-websheet-javascript-signs (pc sign s)
  (dolist (p pc)
    (format s "function ~As_~A (bl) {~%" sign (websheet-cellname (car p)))
    (format s "   if (bl == undefined) { bl = nil; }~%")
    (format s "   var ans = empty();~%")
    (format s "   rulebase = theory;~%")
    (format s "   q = read(\"")
    (print-datalog-atom (car (head-args 1)) s)
    (format s "\");~%")
    (dolist (grp (cdr p))
      (output-websheet-javascript-evaluationguard (car grp) s)
      (format s "    {~%")
      (dolist (q (cdr grp))
	(format s "     ans = auniond(ans, finds(q, queries_~A_~A[~A],bl));~%"
	      sign (websheet-cellname (car p)) (car q)))
      (format s "    }~%"))
    (format s "   return ans; }~%~%")))

(defun cellpreds (p)
  (cond ((atom p) nil)
	((member (car p) '(and or not => <= <=> forall exists))
	 (mapunion #'cellpreds (cdr p)))
	((eq (car p) 'cell) (list (second p)))
	(t nil)))

(defun output-websheet-javascript-components (comps s)
  (dolist (c comps)
    (format s "var component_~A = " (websheet-cellname (car c)))
    (output-javascript-newarray c s #'(lambda (x) (format nil "'~A'" (websheet-cellname x))))
    (format s ";~%")))

(defun output-javascript-newarray (list s &optional (f #'identity))
  (cond ((null list))
	(t (format s "new Array(~A" (funcall f (car list)))
	   (dolist (l (cdr list))
	     (format s ", ~A" (funcall f l)))
	   (format s ")"))))

(defun output-websheet-javascript-queries (pc struct sign s)
  (let ((h (make-hash-table)))
    (mapc #'(lambda (x) (setf (gethash (widget-name x) h)
			      (cons (list (widget-name x) '?x) 
				    (list (widget-typename x) '?x)))) 
	  (webform-widgets struct))
    (dolist (p pc)
      (output-websheet-javascript-queryarray 
       (cdr p) (car p) (format nil "queries_~(~A~)_~A" sign (websheet-cellname (car p))) h s)
      (format s "~%"))))

(defun output-websheet-javascript-queryarray (queries pred name typehash s)
  "(OUTPUT-WEBSHEET-JAVASCRIPT-QUERYARRAY QUERIES NAME) takes a list of datalog queries
   and a name for an array and outputs javascript that creates an array of the given
   name whose values are the parsed queries."
   (setq queries (mapcar #'(lambda (x) 
			     (let ((lit (maknot (list pred (car (head-args 1))))))
			       (delete lit (order-datalog-conjuncts 
					    (include-in-list lit x)
					    typehash 'univ))))
			 (mapcar #'(lambda (x) (if (and (listp x) (eq (car x) 'and))
						   (cdr x) (list x))) queries)))
   (format s "var ~A = new Array(" name)
   (format s "read(\"")
   (kif2javascript (maksand (car queries)) s)
   (dolist (q (cdr queries))
     (format s "\"), read(\"")
     (kif2javascript (maksand q) s))
   (format s "\"));~%"))

(defun include-in-list (lit list)
  (if (and (listp list) (listp (car list)))
      (cons lit list)
      (list lit list)))

(defun output-websheet-javascript-evaluationguard (preds s)
  (cond ((null preds)
	 (format s "   if (true)~%"))
	(t
	 (format s "   if (cellhasvalue('~A')" (websheet-cellname (car preds)))
	 (dolist (r (cdr preds))
	   (format s " && cellhasvalue('~A')" (websheet-cellname r)))
	   ;  (format s " && !inconflict(new Array('~A'" (websheet-cellname (car preds)))
	   ;  (dolist (r (cdr preds))
	   ;    (format s ", '~A'" (websheet-cellname r)))
	   ;  (format s "))")
	 (format s ")~%"))))


#|
  (let ((h (make-hash-table)) news)
    (mapc #'(lambda (x) (setf (gethash (car x) h) (cdr x)))
	  '((1 . "one") (2 . "two") (3 . "three") (4 . "four") (5 . "five")
	    (6 . "six") (7 . "seven") (8 . "eight") (9 . "nine")
	    (- . "dash") (_ . "underscore") (\#. . "dot")))))
|#

(defun output-websheet-javascript-theory (struct s)
  (format s "var theory = readdata(\"")
  (output-websheet-javascript-datalog struct s)
  (format s "\");~%~%"))

(defun output-websheet-javascript-datalog (struct s)
    ; theory contents
  (dolist (e (websheet-theory-symbols-to-shortnames (webform-definitions struct) struct))
    (kifrule2javascript e s)
    (format s " "))
    ; cell types
  (dolist (p (webform-widgets struct))
    (dolist (y (widget-type p))
      (kifrule2javascript (list (widget-typename p) (value-shortname y)) s)
      (format s " ")))
    ; universe
  (dolist (o (webform-univ struct))
    (kifrule2javascript `(univ ,(value-shortname o)) s)
    (format s " "))
    ; equality
  (kifrule2javascript `(<= (,*prefixeq* ?x ?x) (univ ?x)) s))

(defun adjust-and-split-complete-queries (th preds)
  "(SPLIT-COMPLETE-QUERIES TH) takes a theory all of whose sentences are of the form
   (<=> (impl '(lit @x) @x) (exists @y (phi @x @y))) and returns two association lists.
   One for all of the positive heads and one for the negative heads.  Each entry
   is of the form (p . <list of conjunctive queries>)."
  (let ((pospc nil) (negpc nil))
    (setq th (contents th))
    (dolist (p th (values pospc negpc))
      (if (negative-literalp (impl-sent (second p)))
	  (push (cons (relation (impl-sent (second p)))
		      (mapcar #'(lambda (x) (adjust-datalog-query x preds))
			      (to-orless-list (drop-exists (third p)))))
		negpc)
	  (push (cons (relation (impl-sent (second p)))
		      (mapcar #'(lambda (x) (adjust-datalog-query x preds))
			      (to-orless-list (drop-exists (third p)))))
		pospc)))))

(defun adjust-datalog-query (q preds) 
  (nsubst *prefixeq* '= (demote-to-cell q preds)))

(defun demote-to-cell (p preds)
  (cond ((atom p) p)
	((member (car p) '(and or not => <= <=> forall exists))
	 (cons (car p) (mapcar #'(lambda (x) (demote-to-cell x preds)) (cdr p))))
	((member (car p) preds) (cons 'cell p))
	(t p)))

