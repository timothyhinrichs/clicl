;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tracing.lisp
;;    upgrades Epilog tracing to be either text, html, or xml
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tracedepth* -1)
(defvar *tracetype* nil
  "*TRACETYPE* controls whether the trace output is text (nil) or html ('html) or xml ('xml)")

(defun tracemessage (depth type p)
  (when (or (= *tracedepth* -1) (> *tracedepth* depth))
    (cond ((not *tracetype*) (tracemessage-text depth type p))
          ((eq *tracetype* 'html) (tracemessage-html depth type p))
          (t (tracemessage-xml depth type p)))))

(defun tracemessage-text (depth type p)
  (fresh-line *trace-device*)
  (tracespaces depth) (princ type *trace-device*) (princ p *trace-device*))

(defun tracemessage-html (depth type p)
  (let ((handle (gentemp)))
    (case type
      (|Call: | (trace-start-html depth handle) (trace-item-html depth type p) (trace-substart-html depth handle))
      (|Exit: | (trace-substop-html depth) (trace-item-html depth type p) (trace-stop-html depth))
      (|Redo: | (trace-start-html depth handle) (trace-item-html depth type p) (trace-substart-html depth handle))
      (|Fail: | (trace-substop-html depth) (trace-item-html depth type p) (trace-stop-html depth))
      (|Stop: | (trace-substop-html depth) (trace-item-html depth type p) (trace-stop-html depth))
      (otherwise (format *trace-device* "Unknown: ~A ~A~%" type p)))))
  
(defun tracemessage-xml (depth type p)
  (case type
    (|Call: | (trace-start-xml depth) (trace-item-xml depth type p 'open) (trace-substart-xml depth))
    (|Exit: | (trace-substop-xml depth) (trace-item-xml depth type p 'close) (trace-stop-xml depth))
    (|Redo: | (trace-start-xml depth) (trace-item-xml depth type p 'open) (trace-substart-xml depth))
    (|Fail: | (trace-substop-xml depth) (trace-item-xml depth type p 'close) (trace-stop-xml depth))
    (otherwise (format *trace-device* "Unknown: ~A ~A~%" type p))))


(defun trace-start-html (d handle) 
    (spaces d) 
    (format *trace-device* "<div class=\"sentence\"><div class=\"head\">")
    (format *trace-device* "	<a href=\"javascript:showDetails('~A')\">" handle)
    (format *trace-device* "<img src=\"/images/arrowrt.gif\" width=\"21\" height=\"19\" id=\"~AShow\" style=\"display:block;\" border=\"0\"></a>" handle)
    (format *trace-device* "<a href=\"javascript:hideDetails('~A')\">" handle)
    (format *trace-device* "<img src=\"/images/arrowdn.gif\" width=\"21\" height=\"19\" id=\"~AHide\" style=\"display:none\" border=\"0\"></a>" handle)
    (format *trace-device* "</div>~%")
    (format *trace-device* "<div class=\"body\">~%"))

(defun trace-substart-html (d handle) (spaces d) (format *trace-device* "<div class=\"subtrace\" id=\"~A\">" handle) (crlf *trace-device*))
(defun trace-stop-html (d) (spaces d) (format *trace-device* "</div></div>") (crlf *trace-device*))
(defun trace-substop-html (d) (spaces d) (format *trace-device* "</div>") (crlf *trace-device*))
(defun trace-item-html (d type p) (spaces d) (format *trace-device* "<div class=\"item\">~A~A</div>" type p) (crlf *trace-device*))

(defun trace-start-xml (d) (spaces d) (format *trace-device* "<trace>") (crlf *trace-device*))
(defun trace-substart-xml (d) (spaces d) (format *trace-device* "<subtrace id=\"~(~A~)\">" (gentemp)) (crlf *trace-device*))
(defun trace-stop-xml (d) (spaces d) (format *trace-device* "</trace>") (crlf *trace-device*))
(defun trace-substop-xml (d) (spaces d) (format *trace-device* "</subtrace>") (crlf *trace-device*))
(defun trace-item-xml (d type p oc) (spaces d) (format *trace-device* "<~(~A~)>~A~A</~(~A~)>" oc type p oc) (crlf *trace-device*))


(defun output-tracing-css ()
  (format *trace-device* "
<style>
  div.trace {
     border: 1px solid black;
  }
  div.subtrace {
     border: 1px solid red;
     margin-left: 20px;
  }
</style>"))

(defun output-tracing-xml ()
  (format *trace-device* "
<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"http://logic/~thinrich/style/trace-view.xsl\"?>
"))

(defun spaces (depth) (dotimes (i depth) (format *trace-device* "   ")))


#|
(with-open-file (f "gullible:users:thinrich:desktop:xtr.xml" :direction :output :if-does-not-exist :create :if-exists :supersede)
    (setq *trace-device* f)
    (print f (xml))
    (viewfindp 'p 'test))
|#


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; proof ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *showproof* nil
  "*SHOW-PROOF* controls whether the proof is saved.")
(defvar *proof-trace* nil
  "*proof-trace* records the order of proof-nodes if a proof is found.")

(defstruct proof-node "a proof trace node for ME--stored in *proof-trace-info*"
  (literal nil)     ; literal operated on
  (instance nil)    ; unifier applied to literal
  (depth 0)         ; depth of node in tree
  (rule nil)        ; rule used if any
  (inference nil)   ; inference rule
)

(defun viewfindx (*thing* p *theory*)
  (let (alist *answer*)
    (setq *unifications* 0)
    (setq *inferences* 0)
    (setq *termination* nil)
    (setq *proof-trace* nil)
    (setq alist (environment))
    (when (viewone p (list p) alist 0 nil) 
      (setq *proof-trace* (nreverse *proof-trace*)) 
      *answer*)))

(defun viewoneexit (pl al depth cont)
  (let (ans)
    (viewexit (car pl) al depth)
    (cond ((cdr pl) (setq ans (viewone (cadr pl) (cdr pl) al depth cont)))
          (cont (setq ans (viewoneexit (caar cont) (cadar cont) (caddar cont) (cdr cont))))
          (t (setq *answer* (plugstdexp *thing* alist) ans t)))
    (cond (ans (proof-trace-push pl al depth) t)
          (t (viewredo (car pl) al depth)))))


(defun proof-trace-push (pl al depth)
  "(PROOF-TRACE-PUSH PL AL DEPTH) pushes instantiated (car pl) on the proof-trace stack at depth d."
  (when *showproof* 
    (setq *proof-trace* (cons (make-proof-node :literal (car pl) :instance (plugstdexp (car pl) al) :depth depth)
                              *proof-trace*))))


;;;;;;;;;;; proof manipulation ;;;;;;;;;;;

(defun proof2text (proof)
  "(PROOF2TEXT PROOF) prints out PROOF to the terminal."
  (proof-trace-print-tree proof))

(defun proof-trace-build-tree (pt &optional (depth 0))
  "(PROOF-TRACE-BUILD-TREE PT) build the proof tree from PT, e.g. 
    p(1), q(2), r(2), s(3), t(1)  is  ((p ((q) (r (s)))) (t)). "
  (do ((ps pt (cdr ps))
       (tree nil)
       (pdepth)
       (lastrecursedepth nil))
      ((null ps) tree)
    (setq pdepth (proof-node-depth (car ps)))
    (when (and lastrecursedepth (< pdepth lastrecursedepth)) (setq lastrecursedepth nil))
 
    (cond ((and lastrecursedepth (>= pdepth lastrecursedepth)))  ; go to next element in list
          ((= pdepth depth) 
           (setq lastrecursedepth (1+ depth))
           (setq tree (cons (cons (car ps) (proof-trace-build-tree (cdr ps) (1+ depth))) tree)))
          ((< pdepth depth)
           (return tree))
          (t    ;(> pdepth depth)
           (setq lastrecursedepth pdepth)
           (setq tree (cons (proof-trace-build-tree ps pdepth) tree))))))

(defun proof-trace-print-tree (tree)
  "(PROOF-TRACE-PRINT-TREE TREE) pretty-prints each of the proof nodes in the tree TREE,
   at their proper depths."
  (cond ((null tree))
        (t (proof-trace-print-node (car tree))
           (mapc #'proof-trace-print-tree (cdr tree))))
  t)

(defun proof-trace-print-node (node)
  "(PROOF-TRACE-PRINT-NODE NODE) prints out a single proof node, at the proper depth."
  (format t "~&")
  (proofspaces (proof-node-depth node) t)
  (format t "~A -- ~A    :  ~A(~A)" 
          (proof-node-literal node)
          (proof-node-instance node)
          (proof-node-inference node)
          (proof-node-rule node)))


(defun proofspaces (n stringbuffer)
  (do ((i 1 (1+ i)))
      ((> i n))
      (format stringbuffer " | ")))



(deftheory test
  (<= p (not q) r (not s))
  (<= q t u)
  (<= t v)
  v
  (<= r a b)
  a
  b
  t
  (<= q v u)
)
(deftheory truetest
  u
  s
)
(includes 'truetest 'test)


|#