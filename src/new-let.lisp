;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: New-Let -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :new-let)

;;; This code implements a new LET macro with expanded syntax and semantics,
;;; a generalization of LET, LET*, and MULTIPLE-VALUE-BIND.  Some examples:
;;;
;;;   (let ((a (foo))
;;;         ((b (bar a))))
;;;     ...)
;;;
;;; This example illustrates that clause nesting depth is used to indicate
;;; ordering of evaluation and binding.  B is bound after A, and its initial
;;; value expression refers to A.
;;;
;;;   (let ((a b c (zot))
;;;         ((d (quux a c))
;;;          ((e f (mumble b d))
;;;           (g (mung a))))
;;;         ((h (frobozz c))
;;;          ((i (xyzzy h))))
;;;         (*print-level* 3))
;;;     ...)
;;;
;;; A, B, and C are bound to the first three values of (ZOT), and in parallel,
;;; *PRINT-LEVEL* is bound to 3; then D and H are bound; then E, F, G, and I
;;; are bound.
;;;
;;; As this example illustrates, all bindings at a given nesting level are
;;; done in parallel, with all bindings at a deeper level following.
;;; 
;;; Since I like to use multiple values, I find this syntax for binding them
;;; very handy, and I think many will agree.  (Those familiar with Dylan
;;; will think that I have borrowed the idea from it, but I wrote the first
;;; version of this macro in 1980.)  The value of using nesting to indicate
;;; sequencing will perhaps be less clear.  The additional flexibility
;;; provided, compared to LET*, is admittedly rarely of importance in terms
;;; of expressing an idea in fewer keystrokes.  Personally, though, I like
;;; being able to indicate clearly the data flow dependences among the
;;; various variables I may be binding in one LET; and I have written LET
;;; expressions of complexity comparable to the second example above.  (I
;;; should emphasize that the breaking up of the clauses into groups, as in
;;; that second example, to emphasize their data dependence relationships
;;; is strictly for clarity; in fact, the initial value expression for G,
;;; for instance, is within the scope of H.)
;;;
;;; This code also implements an extension to COND.  It is simply this: that
;;; if the predicate expression of a COND clause is a LET form, the scope of
;;; all variables bound by the LET is extended to include the consequent
;;; expressions of the clause.  (However, it does not include subsequent
;;; clauses.)  This simplifies the writing of somewhat Prolog-like code that
;;; simultaneously tests that an object has a certain structure and binds
;;; variables to parts of that structure in order to do something else.
;;; (In order to be recognized as such, the predicate expression must be
;;; written as a LET form, not a macro invocation that expands to a LET form.
;;; I think this is a feature, but am open to being persuaded otherwise.)
;;;
;;; To use these macros, you must shadow the standard definitions in your
;;; package.  This can be done by including the following option clause in
;;; your DEFPACKAGE form:
;;;
;;;    (:shadowing-import-from :new-let #:let #:cond)
;;;
;;; If for some reason you don't want to shadow these, you can access this
;;; version of LET as NLET, and this version of COND as BCOND (the "B" is
;;; for "binding"), by using the following DEFPACKAGE option instead:
;;;
;;;    (:import-from :new-let #:nlet #:bcond)
;;;
;;; Enjoy!
;;; Scott L. Burson   2/18/2005


(defmacro let (clauses &body body)
  "A generalization of CL:LET that better supports nested bindings and multiple
values.  Syntax: (let (<clause>*) <body>).  The <clause> syntax is more general
than for CL:LET:
  <clause>  ::=   <symbol>                 ; binds to NIL
                | ( <symbol> )             ; likewise
		| <clause1>
  <clause1> ::=   ( <symbol>+ <form> )     ; binding
                | ( <clause1>+ )           ; nesting
When a clause begins with more than one variable name, they are to be bound to
successive values of the form.  The nesting of clauses indicates sequencing of
bindings; more deeply nested clauses may reference bindings of shallower clauses.
All bindings at a given depth are done in parallel.  This allows arbitrary
combinations of parallel and sequential binding.  Standard declarations at the
head of BODY are handled correctly, though nonstandard ones may not be.  If two
variables of the same name are bound at different levels, any declaration
applies to the inner one."
  (expand-new-let clauses body 'nlet))

;;; Alternative name for the above.  I could have this one expand into that
;;; one, or conversely, but I'd want to duplicate the doc string anyway, and
;;; that's most of the code.
(defmacro nlet (clauses &body body)
  "A generalization of CL:LET that better supports nested bindings and multiple
values.  Syntax: (let (<clause>*) <body>).  The <clause> syntax is more general
than for CL:LET:
  <clause>  ::=   <symbol>                 ; binds to NIL
                | ( <symbol> )             ; likewise
		| <clause1>
  <clause1> ::=   ( <symbol>+ <form> )     ; binding
                | ( <clause1>+ )           ; nesting
When a clause begins with more than one variable name, they are to be bound to
successive values of the form.  The nesting of clauses indicates sequencing of
bindings; more deeply nested clauses may reference bindings of shallower clauses.
All bindings at a given depth are done in parallel.  This allows arbitrary
combinations of parallel and sequential binding.  Standard declarations at the
head of BODY are handled correctly, though nonstandard ones may not be.  If two
variables of the same name are bound at different levels, any declaration
applies to the inner one."
  (expand-new-let clauses body 'nlet))

(defmacro mvlet (clauses &body body)
  (expand-new-let clauses body 'mvlet))

(defmacro mvlet* (clauses &body body)
  (expand-new-let clauses body 'mvlet*))

(defun expand-new-let (clauses body mode)
  (labels ((expand (clauses next-level pending-clauses final-clauses body decls)
	     (cl:cond ((null clauses)
		       (let* ((pending-clauses (reverse pending-clauses))
			      (all-clauses (append pending-clauses final-clauses))
			      (vars (mapcar #'car all-clauses)))
			 (if (null next-level)
			     (values (make-let (if (eq mode 'mvlet*) 'cl:let* 'cl:let)
					       all-clauses (append (bound-decls decls vars) (free-decls decls))
					       body)
				     (prune-decls decls vars))
			   (multiple-value-bind (body decls)
			       (expand next-level nil nil nil body decls)
			     (values (make-let 'cl:let all-clauses (bound-decls decls vars) body)
				     (prune-decls decls vars))))))
		      ((and (listp (car clauses)) (listp (caar clauses)))
		       (if (eq mode 'nlet)
			   (expand (cdr clauses) (append next-level (car clauses))
				   pending-clauses final-clauses body decls)
			 (error "Invalid ~A binding clause: ~S" mode (car clauses))))
		      ((and (listp (car clauses)) (cddar clauses))
		       (cl:let ((mvb-vars (butlast (car clauses))))
			 (if (and (null (cdr clauses)) (null next-level)
				  (null pending-clauses) (null final-clauses))
			     (values `((multiple-value-bind ,mvb-vars
					   ,(car (last (car clauses)))
					 ,@(bound-decls decls mvb-vars)
					 ,@(free-decls decls)
					 . ,body))
				     (prune-decls decls mvb-vars))
			   (if (eq mode 'mvlet*)
			       (cl:let ((pc-vars (mapcar #'car pending-clauses)))
				 (multiple-value-bind (body decls)
				     (expand nil (cdr clauses) nil nil body decls)
				   (values (make-let 'cl:let* (reverse pending-clauses)
						     (bound-decls decls pc-vars)
						     `((multiple-value-bind ,mvb-vars
							   ,(car (last (car clauses)))
							 ,@(bound-decls decls mvb-vars)
							 . ,body)))
					   (prune-decls decls pc-vars))))
			     (let* ((pending-clauses (reverse pending-clauses))
				    (pc-vars (mapcar #'car pending-clauses))
				    (pc-gensyms (mapcar (lambda (v) (declare (ignore v)) (gensym "V"))
							pc-vars))
				    (mvb-gensyms (mapcar (lambda (v) (declare (ignore v)) (gensym "V"))
							 mvb-vars)))
			       (multiple-value-bind (body decls)
				   (expand (cdr clauses) next-level nil
					   (append final-clauses (mapcar #'list pc-vars pc-gensyms)
						   (mapcar #'list mvb-vars mvb-gensyms))
					   body decls)
				 (values (make-let 'cl:let
						   (mapcar (lambda (gensym clause) (cons gensym (cdr clause)))
							   pc-gensyms pending-clauses)
						   nil
						   `((multiple-value-bind ,mvb-gensyms
							 ,(car (last (car clauses)))
						       . ,body)))
					 decls)))))))
		      (t
		       (cl:let ((clause (car clauses)))
			 (expand (cdr clauses) next-level (cons (if (listp clause) clause (list clause))
								pending-clauses)
				 final-clauses body decls)))))
	   (make-let (op clauses decls body)
	     ;; I doubt these cleanups make any difference to the compiler, but they do make
	     ;; the expansions look nicer to human readers.
	     (if (or clauses decls)
		 (if (and clauses (or (eq op 'cl:let*) (= (length clauses) 1))
			  (= (length body) 1)
			  (cl:let ((form (car body)))
			    (and (listp form)
				 (or (eq (car form) 'cl:let*)
				     (and (eq (car form) 'cl:let)
					  (= (length (cadr form)) 1))))))
		     `((cl:let* ,(append clauses (cadar body))
			 ,@decls
			 . ,(cddar body)))
		   `((,op ,clauses ,@decls . ,body)))
	       body)))
    (multiple-value-bind (decls body)
	(analyze-decls clauses body)
      `(progn . ,(expand clauses nil nil nil body decls)))))

(defun bound-decls (decls vars)
  (let* ((bd-alist (car decls))
	 (prs (remove-if-not #'(lambda (pr) (member (car pr) vars))
			     bd-alist)))
    (and prs `((declare . ,(mapcar #'(lambda (pr)
				       (if (listp (cdr pr))
					   `(,@(cdr pr) ,(car pr))
					 `(,(cdr pr) ,(car pr))))
				   prs))))))

(defun free-decls (decls)
  (mapcar (lambda (d) `(declare ,d)) (cdr decls)))

(defun prune-decls (decls vars)
  (cl:let ((bd-alist (car decls)))
    (cons (remove-if #'(lambda (pr) (member (car pr) vars))
		     bd-alist)
	  (cdr decls))))

(defun analyze-decls (clauses body)
  "Returns two values. The first value is a cons of: (a) for the bound declarations
at the head of `body', an alist from variable name to a list of declarations
affecting that variable; (b) a list of the remaining (free) declarations.  The
second value is `body' with the declarations stripped off."
  (labels ((process-declares (body bd-alist free vars)
	     (if (or (null body) (not (consp (car body)))
		     (not (eq (caar body) 'declare)))
		 (values bd-alist free body)
	       (multiple-value-bind (bd-alist free)
		   (process-decls (cdar body) bd-alist free vars)
		 (process-declares (cdr body) bd-alist free vars))))
	   (process-decls (decls bd-alist free vars)
	     (if (null decls)
		 (values bd-alist free)
	       (multiple-value-bind (bd-alist free)
		   (process-decl (car decls) bd-alist free vars)
		 (process-decls (cdr decls) bd-alist free vars))))
	   (process-decl (decl bd-alist free vars)
	     (cl:cond
	       ((not (consp decl))	; defensive programming
		(values bd-alist (cons decl free)))
	       ((eq (car decl) 'optimize)
		(values bd-alist (cons decl free)))
	       ((member (car decl) '(ignore ignorable))
		;; These are always bound.
		(values (append (mapcar #'(lambda (x) (cons x (car decl)))
					(cdr decl))
				bd-alist)
			free))
	       ((eq (car decl) 'type)
		(process-vars (cddr decl) (list 'type (cadr decl)) bd-alist free vars))
	       ((member (car decl) '(special dynamic-extent))
		;; These can be either bound or free.
		(process-vars (cdr decl) (list (car decl)) bd-alist free vars))
	       ((type-specifier-name? (car decl))
		(process-vars (cdr decl) (list 'type (car decl)) bd-alist free vars))
	       ((and (every #'symbolp (cdr decl))
		     (some (lambda (x) (member x vars)) (cdr decl)))
		;; 'type-specifier-name?' can get false negatives on some implementations,
		;; plus there could be implementation-specific bound declarations.  So
		;; heuristically, if it looks like it could be a bound declaration, we
		;; assume it can be.
		(process-vars (cdr decl) (list (car decl)) bd-alist free vars))
	       (t
		(values bd-alist (cons decl free)))))
	   (process-vars (decl-vars decl-specs bd-alist free vars)
	     (if (null decl-vars)
		 (values bd-alist free)
	       (multiple-value-bind (bd-alist free)
		   (process-vars (cdr decl-vars) decl-specs bd-alist free vars)
		 (if (member (car decl-vars) vars)
		     (values (cons (cons (car decl-vars) decl-specs)
				   bd-alist)
			     free)
		   (values bd-alist
			   (cons (append decl-specs (list (car decl-vars)))
				 free)))))))
    (multiple-value-bind (bd-alist free body)
	(process-declares body nil nil (new-let-bound-vars clauses))
      (values (cons bd-alist free) body))))

(defun new-let-bound-vars (clauses)
  (and clauses
       (append (cl:let ((clause (car clauses)))
		 (cl:cond ((symbolp clause) (cons clause nil))
			  ((symbolp (car clause)) (butlast clause))
			  (t (new-let-bound-vars clause))))
	       (new-let-bound-vars (cdr clauses)))))

(defun type-specifier-name? (x)
  ;; Alas, this appears to be the only even semi-portable way to discover whether a symbol
  ;; has been declared as a type (which can happen by being predefined, or by `defstruct',
  ;; `defclass', `deftype', or `define-condition').
  ;; (One source recommended `(subtypep x t)', but in SBCL that always returns `(values t t)'.)
  #+(or sbcl ccl ecl clasp allegro lispworks)
  (handler-case (progn (typep nil x) t)
    (error () nil))

  ;; The only implementation on which I've observed it not to work is ABCL.  The spec does say
  ;; that the consequences of calling `typep' on a non-type-specifier are undefined.  Sigh.
  ;; This has no way to find types defined by `deftype'.
  #-(or sbcl ccl ecl clasp allegro lispworks)
  (or (member x '(array atom bignum bit bit-vector character compiled-function
		  complex cons double-float extended-char fixnum float function
		  hash-table integer keyword list long-float nil null number
		  package pathname random-state ratio rational real readtable
		  sequence short-float simple-array simple-bit-vector
		  simple-string simple-vector single-float standard-char stream
		  string base-char symbol t vector))
      (find-class x nil)))


(defmacro cond (&rest clauses)
  "A generalization of CL:COND that makes it convenient to compute a value in
the predicate expression of a clause and then use that value in the consequent.
If the predicate expression is a LET form, then the scope of the variables bound
by the LET is extended to include the consequent expressions.  For example:

  (cond ((let ((x (foo)))
           (bar x))
         (baz x)))

Here the X in (BAZ X) is the one bound to the result of (FOO)."
  (cl:let ((block-nm (gensym)))
    `(block ,block-nm
       . ,(mapcar #'(lambda (c) (bcond-clause c block-nm)) clauses))))

;;; Cause our version of COND to be indented correctly in Zmacs.  (The variable is
;;; in the Genera 8.2 Zmacs; I'm guessing it's in the LMITI system too -- if it
;;; ever matters :-)
#+LispM (push 'cond zwei:*not-lone-function-superiors*)

(defmacro bcond (&rest clauses)
  "A generalization of CL:COND that makes it convenient to compute a value in
the predicate expression of a clause and then use that value in the consequent.
If the predicate expression is a LET form, then the scope of the variables bound
by the LET is extended to include the consequent expressions.  For example:

  (cond ((let ((x (foo)))
           (bar x))
         (baz x)))

Here the X in (BAZ X) is the one bound to the result of (FOO)."
  (cl:let ((block-nm (gensym)))
    `(block ,block-nm
       . ,(mapcar #'(lambda (c) (bcond-clause c block-nm)) clauses))))

(defun bcond-clause (clause block-nm)
  (cl:cond ((not (listp clause))
	      (error "COND clause is not a list: ~S" clause))
	   ((and (listp (car clause))
		 ;; Allow NLET and CL:LET in case the user hasn't chosen
		 ;; to shadow LET.
		 (member (caar clause) '(let nlet cl:let)))
	    (cl:let ((decls nil)
		     (body (cddar clause)))
	      (do () ((not (and (listp (car body)) (eq (caar body) 'declare))))
		(push (pop body) decls))
	      (setq decls (nreverse decls))
	      (bcond-build-clause (caar clause) (cadar clause) decls
				  `(progn . ,body) (cdr clause) block-nm)))
	   (t
	    (bcond-build-clause nil nil nil (car clause) (cdr clause) block-nm))))

(defun bcond-build-clause (let-sym let-clauses decls pred consequents block-nm)
  (cl:let ((body (if consequents
		     (if (eq pred 't)
			 `(return-from ,block-nm (progn . ,consequents))
		       `(if ,pred (return-from ,block-nm (progn . ,consequents))))
		   (cl:let ((temp-var (gensym)))
		     `(cl:let ((,temp-var ,pred))
			(if ,temp-var (return-from ,block-nm ,temp-var)))))))
    (if let-clauses
	`(,let-sym ,let-clauses ,@decls ,body)
      body)))



