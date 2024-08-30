;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GMap -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package gmap)

;;;
;;; GMAP, version 4.0, by Scott L. Burson
;;;
;;; This file is in the public domain.
;;;
;;; CHANGES:
;;;
;;; Version 4.0, 2024-05-21:
;;;
;;; () New syntax adds `:arg' and `:result' keywords, so that the arg and result
;;; type names no longer have a reason to be in the keyword package.
;;;
;;; Version 3.3, 2007-07-04:
;;;
;;; () Revamped arg types `:index' and `:index-inc' (incompatibly).
;;; () Added doc strings, including on the predefined arg- and result-types,
;;;    and arranged for them to be placed on the symbol-plist, where there's
;;;    an outside chance someone might find them.
;;; () Revamped result types `:vector' and `:string' (incompatibly).
;;; () Removed deprecated arg- and result-types `:array'.
;;; () Changed most optional arguments to arg- and result-types to keyword
;;;    arguments.
;;; () Substantially expanded the multiple-value capability.  Now it is possible
;;;    for an arg-spec to generate any number of arguments to the function being
;;;    mapped, and for a single result-spec to consume more than one value
;;;    returned by that function.
;;; () Using the new multiple-value capabilities, added `:alist' and `:plist'
;;;    as both arg- and result-types.
;;;
;;; Thanks to Joerg Hoehle for some useful suggestions.
;;;
;;; DOCUMENTATION:
;;;
;;; See the GMap section of `README.md', and the doc strings below.
;;;

;;; The top-level macro.
(defmacro gmap (res-spec fn &rest arg-specs)
  "A generalized mapping macro.  Applies `fn' to the successive values generated
by the `arg-specs', analagously to `mapcar'; that is, on each iteration, each
arg-spec yields one value, and `fn' is called with these values as arguments.
The values returned by `fn' are accumulated into a result according to
`res-spec'.  The `res-spec' is either a list whose car is a predefined result
type, or a list whose car is `nil' and whose cdr has the same form as the value
of a result type expander (see `def-gmap-res-type'); or, the `res-spec' can be
a symbol, which is shorthand for a list of that symbol; or, it can be a list
whose car is `:values' and whose cdr is a list of result-specs.  Similarly,
each of the `arg-specs' is either a list whose car is a predefined arg type,
or a list whose car is `nil' and whose cdr has the same form as the value of
an arg type expander (see `def-gmap-arg-type')."
  (unless arg-specs
    (error "At least one argument spec is required."))
  (gmap>expand fn
	       (gmap>res-spec-lookup res-spec)
	       (mapcar #'gmap>arg-spec-lookup arg-specs)))

;;; This does the real work.
;;; (Sorry for the weird Multics-influenced naming convention, but these are
;;; internal symbols anyway.)
(defun gmap>expand (fn res-specs arg-specs)
  (let ((param-list
	  (mapcar #'gmap>param arg-specs))
	(result-list (gmap>res>init-clauses res-specs))
	(let-specs (gmap>let-specs arg-specs res-specs)))
    (let ((one-value-p (null (cdr result-list)))
	  (multi-vars (mapcar #'gmap>param>multi-vars arg-specs))
	  (fnval-vars (mapcan #'(lambda (res-spec)
				  (and res-spec
				       (let ((resfn (second res-spec)))
					 (if (and (consp resfn)
						  (eq (first resfn) ':consume))
					     (let ((vars nil))
					       (dotimes (i (second resfn))
						 (push (gensym "VAR-") vars))
					       (nreverse vars))
					   (list (gensym "VAR-"))))))
			      res-specs)))
      `(let ,let-specs
	 (do (,@param-list
	      ,@result-list)
	     ((or ,@(apply #'append (mapcar #'gmap>param>exit-test	; exit test
					    param-list arg-specs)))
	      ,(gmap>res>cleanup res-specs result-list one-value-p))
	   (let ,(reduce #'append
			 (mapcar #'gmap>param>multi-let-specs
				 param-list arg-specs multi-vars))
	     ,(if (null fnval-vars)
		  ;; Null result spec -- just call the function for effect.
		  (apply #'gmap>funcall fn
			 (reduce #'append
				 (mapcar #'gmap>param>arg
					 param-list arg-specs multi-vars)))
		`(let ((,@fnval-vars
			,(apply #'gmap>funcall fn
				(reduce #'append
					(mapcar #'gmap>param>arg
						param-list arg-specs multi-vars)))))
		   . ,(let ((setqs nil))
			(do ((res-specs res-specs (cdr res-specs))
			     (result-list result-list (cdr result-list)))
			    ((null res-specs))
			  (let ((next-exp fnvs
				  (gmap>res>next (car res-specs) (caar result-list)
						 fnval-vars)))
			    (setq fnval-vars fnvs)
			    (push `(setq ,(caar result-list) ,next-exp) setqs)))
			(nreverse setqs))))))))))


;;; extract the let-specs.
(defun gmap>let-specs (arg-specs res-specs)
  (nconc (mapcan #'fifth arg-specs) (mapcan #'fifth res-specs)))

;;; generate the do-variable spec for each argument.
(defun gmap>param (arg-spec)
  (let ((param-name (gensym "VAR-"))
	(init (first arg-spec))
	(nextfn (fourth arg-spec)))
    `(,param-name
      ,init
      ,@(if nextfn
	    `(,(gmap>funcall nextfn param-name))
	    nil))))

;;; get the argument to the function being mapped from the do-variable.
(defun gmap>param>arg (param arg-spec multi-vars)
  (let ((param-name (first param))
	(argfn (third arg-spec)))
    (or multi-vars
	`(,(gmap>funcall argfn param-name)))))

;;; get the exit test for the variable.
(defun gmap>param>exit-test (param arg-spec)
  (let ((param-name (first param))
	(exitp (second arg-spec)))
    (if exitp
	`(,(gmap>funcall exitp param-name))
	nil)))

(defun gmap>param>multi-vars (arg-spec)
  (let ((argfn (third arg-spec)))
    (and (consp argfn) (eq (first argfn) ':values)
	 ;; (gmap :list (lambda (i) (gensym))
	 ;;       (:index 0 (cadr argfn)))
	 (let ((vars nil))
	   (dotimes (i (second argfn))
	     (push (gensym "VAR-") vars))
	   (nreverse vars)))))

(defun gmap>param>multi-let-specs (param arg-spec multi-vars)
  (let ((argfn (third arg-spec)))
    (and multi-vars
	 `((,@multi-vars ,(gmap>funcall (third argfn) (first param)))))))

;;; get the initial value of the result.
(defun gmap>res>init-clauses (res-specs)
  (mapcan #'(lambda (res-spec)
	      (and res-spec (cons (list (gensym "VAR-") (first res-spec))
				  nil)))
	  res-specs))

;;; compute the next value of the result from the current one and the
;;; current value of the function.
(defun gmap>res>next (res-spec result fnvals)
  (let ((resfn (second res-spec))
	(filterp (fourth res-spec))
	((n-fnvals resfn
	   (if (and (consp resfn) (eq (first resfn) ':consume))
	       (values (second resfn) (third resfn))
	     (values 1 resfn)))
	 ((my-fnvals (subseq fnvals 0 n-fnvals)))))
    (values (if filterp
		`(if ,(apply #'gmap>funcall filterp my-fnvals)
		     ,(apply #'gmap>funcall resfn result my-fnvals)
		   ,result)
	      (apply #'gmap>funcall resfn result my-fnvals))
	    (subseq fnvals n-fnvals))))

;;; call the cleanup function on exit.
(defun gmap>res>cleanup (res-specs result-list one-value-p)
  (if one-value-p
      (gmap>funcall (third (car res-specs)) (caar result-list))
    `(values . ,(mapcar #'(lambda (res-spec result-pair)
			    (gmap>funcall (third res-spec) (car result-pair)))
			res-specs result-list))))

;;; For some reason, some compilers don't convert, e.g., (funcall #'car foo)
;;; to (car foo); thus we lose some efficiency for functions that would normally
;;; open-code, like car.  Hence this function to perform the optimization.  Also
;;; cleans up the expansion a bit.
(defun gmap>funcall (function &rest args)
  (let ((args (copy-list args)))
    (cond ((or (null function) (eq function ':id))
	   `(values . ,args))
	  ((and (listp function)
		(eq (car function) 'function))
	   `(,(cadr function) . ,args))
	  ((and (listp function)
		(eq (car function) 'lambda))
	   `(,function . ,args))
	  (t `(funcall ,function . ,args)))))


(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar *gmap-arg-type-list* nil
    "A list of all GMAP arg types that have been defined.")
  (defvar *gmap-result-type-list* nil
    "A list of all GMAP result types that have been defined."))

(define-condition keyword-name-deprecated (style-warning)
    ((kind :initarg :kind :reader kind
       :documentation "One of { :arg, :res }.")
     (name :initarg :name :reader name))
  (:report
    (lambda (condition stream)
      (format stream
	      "def-gmap-~(~A~)-type ~S: the use of keyword names with gmap:def-gmap-~(~A~)-type~@
	       is deprecated.  Use a name in the package that defines the type you~@
	       wish to iterate over.  For backward compatibility, the keyword name~@
	       will also be defined, but references to it are also deprecated; instead,~@
	       use `(:~A <type> ...)'."
	      (kind condition) (name condition) (kind condition)
	      (if (eq (kind condition) ':res) "result" "arg")))))

(defmacro def-gmap-arg-type (name args &body body)
  "Defines a GMap arg-type.  Syntax is identical to `defun'.  The body should
return a list of 1 to 5 elements: (0, \"init\") the initial value of the
state variable; (1, \"exitp\"), if non-nil, a function of one argument which
is called on the state variable, a true result causing the iteration to
exit; (2, \"argfn\"), if non-nil, a function of one argument which is called
on the state variable to get the value to be used on this iteration; (3,
\"nextfn\"), if non-nil, a function of one argument which is called on the
state variable to get the new value of same; and (4, \"let-specs\") a list of
clauses for an `nlet' that will be wrapped around the entire expansion.

It is also possible for an arg-type to generate multiple arguments.  If
element 2, \"argfn\", is of the form `(:values N FN)', FN should be a function
returning N values, which will be passed as separate arguments to the function
being mapped.

This is the backward-compatibility version of this macro; when `name' is not
in the keyword package, it also tries to identically define the keyword symbol
of the same name, though it checks for collisions first.  If a collision does
occur, you can resolve it by changing at least one of the `def-gmap-arg-type'
forms to `def-arg-type', which does not attempt to define the keyword version
of the name.  Of course, you will also need to update any references in the
old syntax `(:name ...)' to the new syntax `(:arg name ...)'."
  (let ((fn-name (or (get name 'arg-type-expander)
		     (gensym "ARG-TYPE-EXPANDER-")))
	(kwd-name (and (not (eq (symbol-package name) (find-package "KEYWORD")))
		       (intern (symbol-name name) (find-package "KEYWORD")))))
    (unless kwd-name
      (warn 'keyword-name-deprecated :kind ':arg :name name))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name 'arg-type-expander) ',fn-name)
	 ;; For backward compatibility, we also define the keyword version of the name.
	 ,@(and kwd-name
		`((let ((prev (get ',kwd-name 'arg-type-synonym-of)))
		    (when (and prev (not (eq prev ',name)))
		      (cerror "Proceed, overwriting the previous definition"
			      "GMAP arg type name collision: ~S was a synonym for ~S,~@
			       but is now being defined as ~S~@
			       See gmap:def-gmap-arg-type for more info."
			      ',kwd-name prev ',name))
		    (setf (get ',kwd-name 'arg-type-expander) ',fn-name)
		    (setf (get ',kwd-name 'arg-type-synonym-of) ',name))))
	 ,@(and doc-string
		`((setf (get ',name 'arg-type-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-arg-type-list*)))))

;;; The 4.0 version of this macro doesn't have "gmap" in the name -- which was redundant
;;; anyway.  It requires `name' not to be in the keyword package, and does not attempt
;;; to define the corresponding keyword symbol.  Thus, the name will be usable only with
;;; the new `(:arg name ...)' syntax, not the old `(:name ...)' syntax.  But there will
;;; also be no danger of a collision.
(defmacro def-arg-type (name args &body body)
  "Defines a GMap arg-type.  Syntax is identical to `defun'.  The body should
return a list of 1 to 5 elements: (0, \"init\") the initial value of the
state variable; (1, \"exitp\"), if non-nil, a function of one argument which
is called on the state variable, a true result causing the iteration to
exit; (2, \"argfn\"), if non-nil, a function of one argument which is called
on the state variable to get the value to be used on this iteration; (3,
\"nextfn\"), if non-nil, a function of one argument which is called on the
state variable to get the new value of same; and (4, \"let-specs\") a list of
clauses for an `nlet' that will be wrapped around the entire expansion.

It is also possible for an arg-type to generate multiple arguments.  If
element 2, \"argfn\", is of the form `(:values N FN)', FN should be a function
returning N values, which will be passed as separate arguments to the function
being mapped."
  (let ((fn-name (or (get name 'arg-type-expander)
		     (gensym "ARG-TYPE-EXPANDER-"))))
    (when (eq (symbol-package name) (find-package "KEYWORD"))
      (error "def-arg-type ~S: the use of keyword names with gmap:def-arg-type is~@
	     not permitted.  Use a name in the package that defines the type you~@
	     wish to iterate over.  If you need to define the keyword name for~@
	     backward compatibility, use `def-gmap-arg-type'."
	    name))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name 'arg-type-expander) ',fn-name)
	 ,@(and doc-string
		`((setf (get ',name 'arg-type-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-arg-type-list*)))))


(defmacro def-gmap-res-type (name args &body body)
  "Defines a GMap result-type.  Syntax is identical to `defun'.  The body should
return a list of 2 to 5 elements: (0, \"init\") the initial value of the state
variable; (1, \"resfn\") a function of two arguments which is called on the
state variable and the current value of the function being mapped, returning
the new value of the state variable; (2, \"cleanup\"), if non-nil, a function
of one argument which is called on the final value of the state variable to
get the value of the `gmap' form; (3, \"filterp\"), if non-nil, a predicate
of one argument which is called on the current value of the function being
mapped, a false value causing \"resfn\" not to be called on this iteration (and
the state variable to be unchanged); and (4, \"let-specs\") a list of
clauses for an `nlet' that will be wrapped around the entire expansion.

It is also possible for a result-type to consume more than one value of the
function being mapped.  If element 1, \"resfn\", is of the form `(:consume N
FN)', FN should be a function of N + 1 arguments, and will receive N values
from the function being mapped.

This is the backward-compatibility version of this macro; when `name' is not
in the keyword package, it also tries to identically define the keyword symbol
of the same name, though it checks for collisions first.  If a collision does
occur, you can resolve it by changing at least one of the `def-gmap-res-type'
forms to `def-res-type', which does not attempt to define the keyword version
of the name.  Of course, you will also need to update any references in the
old syntax `(:name ...)' to the new syntax `(:result name ...)'."
  (let ((fn-name (or (get name 'res-type-expander)
		     (gensym "RES-TYPE-EXPANDER-")))
	(kwd-name (and (not (eq (symbol-package name) (find-package "KEYWORD")))
		       (intern (symbol-name name) (find-package "KEYWORD")))))
    (unless kwd-name
      (warn 'keyword-name-deprecated :kind ':res :name name))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name 'res-type-expander) ',fn-name)
	 ;; For backward compatibility, we also define the keyword version of the name.
	 ,@(and kwd-name
		`((let ((prev (get ',kwd-name 'res-type-synonym-of)))
		    (when (and prev (not (eq prev ',name)))
		      (cerror "Proceed, overwriting the previous definition"
			      "GMAP result type name collision: ~S was a synonym for ~S,~@
			       but is now being defined as ~S~@
			       See gmap:def-gmap-res-type for more info."
			      ',kwd-name prev ',name))
		    (setf (get ',kwd-name 'res-type-expander) ',fn-name)
		    (setf (get ',kwd-name 'res-type-synonym-of) ',name))))
	 ,@(and doc-string
		`((setf (get ',name 'res-type-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-result-type-list*)))))

;;; The 4.0 version of this macro doesn't have "gmap" in the name -- which was redundant
;;; anyway.  It requires `name' not to be in the keyword package, and does not attempt
;;; to define the corresponding keyword symbol.  Thus, the name will be usable only with
;;; the new `(:result name ...)' syntax, not the old `(:name ...)' syntax.  But there will
;;; also be no danger of a collision.
(defmacro def-result-type (name args &body body)
  "Defines a GMap result-type.  Syntax is identical to `defun'.  The body should
return a list of 2 to 5 elements: (0, \"init\") the initial value of the state
variable; (1, \"resfn\") a function of two arguments which is called on the
state variable and the current value of the function being mapped, returning
the new value of the state variable; (2, \"cleanup\"), if non-nil, a function
of one argument which is called on the final value of the state variable to
get the value of the `gmap' form; (3, \"filterp\"), if non-nil, a predicate
of one argument which is called on the current value of the function being
mapped, a false value causing \"resfn\" not to be called on this iteration (and
the state variable to be unchanged); and (4, \"let-specs\") a list of
clauses for an `nlet' that will be wrapped around the entire expansion.

It is also possible for a result-type to consume more than one value of the
function being mapped.  If element 1, \"resfn\", is of the form `(:consume N
FN)', FN should be a function of N + 1 arguments, and will receive N values
from the function being mapped."
  (let ((fn-name (gensym "RES-TYPE-EXPANDER-")))
    (when (eq (symbol-package name) (find-package "KEYWORD"))
      (error "def-res-type ~S: the use of keyword names with gmap:def-res-type is~@
	     not permitted.  Use a name in the package that defines the type you~@
	     wish to iterate over.  If you need to define the keyword name for~@
	     backward compatibility, use `def-gmap-res-type'."
	    name))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name 'res-type-expander) ',fn-name)
	 ,@(and doc-string
		`((setf (get ',name 'res-type-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-result-type-list*)))))


;;; look up an arg type.
(defun gmap>arg-spec-lookup (raw-arg-spec)
  (let ((raw-arg-spec (if (eq (car raw-arg-spec) ':arg) (cdr raw-arg-spec)
			raw-arg-spec)))
    (let ((type (car raw-arg-spec)))
      (if (null type)
	  (cdr raw-arg-spec)
	(let ((generator (or (get type 'arg-type-expander)
			     ;; Backward compatibility for the old property
			     (get type ':gmap-arg-spec-expander))))
	  (if generator
	      (apply generator (cdr raw-arg-spec))
	    (error "Argument spec, ~S, to gmap is of unknown type~@
		  (Do you have the package right?)"
		   raw-arg-spec)))))))

;;; look up a result type.
(defun gmap>res-spec-lookup (raw-res-spec)
  (let ((raw-res-spec (if (and (listp raw-res-spec) (eq (car raw-res-spec) ':result))
			  (cdr raw-res-spec)
			(progn
			  (when (eq raw-res-spec ':result)
			    (error "Keyword ':result' must be the car of a sublist"))
			  raw-res-spec))))
    (if (and (listp raw-res-spec)
	     (member (car raw-res-spec) '(values :values)))
	(mapcar #'gmap>res-spec-lookup-1 (cdr raw-res-spec))
      (list (gmap>res-spec-lookup-1 raw-res-spec)))))
(defun gmap>res-spec-lookup-1 (raw-res-spec)
  (let ((type (if (listp raw-res-spec) (car raw-res-spec)
		raw-res-spec)))
    (if (null type)
	(cdr raw-res-spec)
      (let ((generator (or (get type 'res-type-expander)
			   ;; Backward compatibility for the old property
			   (get type ':gmap-res-spec-expander))))
	(if generator
	    (apply generator (and (listp raw-res-spec) (cdr raw-res-spec)))
	  (error "Result spec, ~S, to gmap is of unknown type~@
		  (Do you have the package right?)"
		 raw-res-spec))))))


;;; ******** Predefined argument types ********
;;; See above for documentation.

(def-gmap-arg-type constant (value)
  "Yields an unbounded sequence of `value'."
  `(,value))

(def-gmap-arg-type list (list)
  "Yields the successive elements of `list'."
  `(,list
    #'endp #'car #'cdr))

(def-gmap-arg-type improper-list (list)
  "Yields the successive elements of `list', which may be improper; any non-consp
tail terminates the iteration."
  `(,list
    #'(lambda (x) (not (consp x)))
    #'car #'cdr))

(def-gmap-arg-type alist (alist)
  "Yields, as two values, the successive pairs of `alist'."
  `(,alist
    #'endp
    (:values 2 #'(lambda (alist) (values (caar alist) (cdar alist))))
    #'cdr))

(def-gmap-arg-type plist (plist)
  "Yields, as two values, the successive pairs of elements of `plist'; that is,
there is one iteration for each two elements."
  `(,plist
    #'endp
    (:values 2 #'(lambda (plist) (values (car plist) (cadr plist))))
    #'cddr))

(def-gmap-arg-type tails (list)
  "Yields the successive tails (cdrs) of `list', starting with `list' itself, which
may be improper."
  `(,list
    #'(lambda (x) (not (consp x)))
    nil #'cdr))

;;; If `incr' is +1 or -1, then swapping `start' and `stop' and negating `incr'
;;; generates the same sequence in reverse order.  This isn't true, though, in
;;; general.  Should it be?
(def-gmap-arg-type index (&optional (start 0) stop &key (incr 1) (fixnums? t))
  "Yields integers in the interval [`start', `stop') if `incr' (which defaults
to 1) is positive; or in the interval [`stop', `start') if `incr' is negative.
Specifically, in the upward case, the values begin with `start' and increase by
`incr' until >= `stop'; in the downward case, the values begin with
`start' - `incr' and decrease by `incr' until < `stop'.  All values are
assumed to be fixnums unless `fixnums?' is a literal `nil'.  `stop' can be
omitted or a literal `nil' to indicate an unbounded sequence.  `start' can be
omitted to start at 0."
  (let ((incr-temp (gensym "INCR-"))
	(stop-temp (gensym "STOP-")))
    ;; Aargh, have to handle the constant vs. variable cases of `incr' separately
    ;; to avoid unreachable-code warnings from Python.  (Suggested heuristic:
    ;; if code becomes unreachable because a variable is bound to a compile-time
    ;; constant, and the name of the variable is an uninterned symbol, suppress
    ;; the warning on the grounds that a macro generated the code in question.)
    `(,(if (numberp incr)			; init
	   (if (minusp incr)
	       (if fixnums? `(+ (the fixnum ,start) ,incr)
		 `(+ ,start ,incr))
	     start)
	 `(if (minusp ,incr-temp)
	      ,(if fixnums? `(+ (the fixnum ,start) (the fixnum ,incr-temp))
		   `(+ ,start ,incr-temp))
	   ,start))
      ,(and stop				; exitp
	    (if (numberp incr)
		(if fixnums?
		    `#'(lambda (val)
			 (,(if (minusp incr) '< '>=)
			   (the fixnum val) (the fixnum ,stop-temp)))
		  `#'(lambda (val)
		       (,(if (minusp incr) '< '>=) val ,stop-temp)))
	      `#'(lambda (val)
		   ,@(and fixnums? `((declare (type fixnum val ,incr-temp ,stop-temp))))
		   (if (minusp ,incr-temp)
		       (< val ,stop-temp)
		     (>= val ,stop-temp)))))
      nil					; no argfn
      #'(lambda (val)				; nextfn
	  ,(if fixnums?
	       `(the fixnum (+ (the fixnum val)
			       (the fixnum ,(if (numberp incr) incr incr-temp))))
	     `(+ val ,(if (numberp incr) incr incr-temp))))
      (,@(and (not (numberp incr))		; let-specs
	      `((,incr-temp ,incr)))
       ,@(and stop
	      `((,stop-temp ,stop)))))))

(def-gmap-arg-type index-inc (start stop &key (incr 1) (fixnums? t))
  "Yields integers in the interval [`start', `stop'].  Specifically, in the
upward case (`incr' > 0), the values begin with `start' and increase by
`incr' until > `stop'; in the downward case, the values begin with `start'
and decrease by `incr' until < `stop'.  All values are assumed to be fixnums
unless `fixnums?' is a literal `nil'.  `stop' can be a literal `nil' to
indicate an unbounded sequence."
  (let ((incr-temp (gensym "INCR-"))
	(stop-temp (gensym "STOP-")))
    ;; Aargh, have to handle the constant vs. variable cases of `incr' separately
    ;; to avoid unreachable-code warnings from Python.  (Suggested heuristic:
    ;; if code becomes unreachable because a variable is bound to a compile-time
    ;; constant, and the name of the variable is an uninterned symbol, suppress
    ;; the warning on the grounds that a macro generated the code in question.)
    `(,start
      ,(and stop				; exitp
	    (if (numberp incr)
		(if fixnums?
		    `#'(lambda (val)
			 (,(if (minusp incr) '< '>)
			   (the fixnum val) (the fixnum ,stop-temp)))
		  `#'(lambda (val)
		       (,(if (minusp incr) '< '>) val ,stop-temp)))
	      `#'(lambda (val)
		   ,@(and fixnums? `((declare (type fixnum val ,incr-temp ,stop-temp))))
		   (if (minusp ,incr-temp)
		       (< val ,stop-temp)
		     (> val ,stop-temp)))))
      nil					; no argfn
      #'(lambda (val)				; nextfn
	  ,(if fixnums?
	       `(the fixnum (+ (the fixnum val)
			       (the fixnum ,(if (numberp incr) incr incr-temp))))
	     `(+ val ,(if (numberp incr) incr incr-temp))))
      (,@(and (not (numberp incr))		; let-specs
	      `((,incr-temp ,incr)))
       ,@(and stop
	      `((,stop-temp ,stop)))))))

(def-gmap-arg-type exp (initial-value base)
  "Yields an unbounded exponential sequence starting with `initial-value'
and multiplying by `base' after each iteration."
  (let ((base-temp (gensym "BASE-")))
    `(,initial-value
      nil
      nil
      #'(lambda (x) (* x ,base-temp))
      ((,base-temp ,base)))))

(def-gmap-arg-type vector (vec &key start stop incr)
  "Yields elements of vector `vec'.  `start' and `stop' may be supplied to select
a subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc.  For performance, you may prefer `:simple-vector'."
  (let ((vec-temp (gensym "VEC-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= i ,stop-temp))
       #'(lambda (i) (aref ,vec-temp i))
       #'(lambda (x) (+ x ,(or incr-temp 1)))
       ((,vec-temp ,vec)
	,@(and incr `((,incr-temp ,incr)))
	((,stop-temp ,(or stop `(length ,vec-temp))))))))

(def-gmap-arg-type simple-vector (vec &key start stop incr)
  "Yields elements of vector `vec', which is assumed to be simple, and whose size
is assumed to be a fixnum.  `start' and `stop' may be supplied to select a
subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc."
  (let ((vec-temp (gensym "VEC-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= (the fixnum i) ,stop-temp))
       #'(lambda (i) (svref ,vec-temp (the fixnum i)))
       #'(lambda (i) (the fixnum (+ (the fixnum i) ,(or incr-temp 1))))
       ((,vec-temp ,vec)
	,@(and incr `((,incr-temp (the fixnum ,incr))))
	((,stop-temp (the fixnum ,(or stop `(length ,vec-temp)))))))))

(def-gmap-arg-type string (str &key start stop incr)
  "Yields elements of string `str'.  `start' and `stop' may be supplied to select
a subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc.  For performance, you may prefer `:simple-string'."
  (let ((str-temp (gensym "STR-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= i ,stop-temp))
       #'(lambda (i) (char ,str-temp i))
       #'(lambda (i) (+ i ,(or incr-temp 1)))
       ((,str-temp (string ,str))
	,@(and incr `((,incr-temp ,incr)))
	((,stop-temp ,(or stop `(length ,str-temp))))))))

(def-gmap-arg-type simple-string (str &key start stop incr)
  "Yields elements of string `str', which is assumed to be simple, and whose size
is assumed to be a fixnum.  `start' and `stop' may be supplied to select a
subsequence of `str'; `incr' may be supplied (it must be positive) to select
every second element etc."
  (let ((str-temp (gensym "STR-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= (the fixnum i) ,stop-temp))
       #'(lambda (i) (schar ,str-temp (the fixnum i)))
       #'(lambda (i) (+ (the fixnum i) ,(or incr-temp 1)))
       ((,str-temp ,str)
	,@(and incr `((,incr-temp (the fixnum ,incr))))
	((,stop-temp (the fixnum ,(or stop `(length ,str-temp)))))))))


;;; ******** Predefined result types ********

(def-gmap-res-type list (&key filterp)
  "Returns a list of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (x y) (cons y x)) #'nreverse ,filterp))

(def-gmap-res-type alist (&key filterp)
  "Consumes two values from the mapped function; returns an alist of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (res x y) (cons (cons x y) res))) #'nreverse ,filterp))

(def-gmap-res-type plist (&key filterp)
  "Consumes two values from the mapped function; returns a plist of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (res x y) (cons y (cons x res)))) #'nreverse ,filterp))

(def-gmap-res-type append (&key filterp)
  "Returns the result of `append'ing the values, optionally filtered by
`filterp'."
  `(nil
    #'(lambda (old new) (revappend new old))
    #'nreverse
    ,filterp))

(def-gmap-res-type nconc (&key filterp)
  "Returns the result of `nconc'ing the values, optionally filtered by
`filterp'."
  (let ((result-var (gensym "RESULT-")))	; have to use our own, sigh.
    `(nil					; init
      #'(lambda (tail-loc new)			; nextfn
	  (if tail-loc (rplacd tail-loc new)
	    (setq ,result-var new))
	  (if new (last new) tail-loc))
      #'(lambda (ignore)
	  (declare (ignore ignore))
	  ,result-var)
      ,filterp
      ((,result-var nil)))))

(def-gmap-res-type and ()
  "If one of the values is false, terminates the iteration and returns false;
otherwise, returns the last value.  Does not work as an operand of `:values'."
  '(t #'(lambda (ignore new)
	  (declare (ignore ignore))
	  (if new new (return nil)))))

(def-gmap-res-type or ()
  "If one of the values is true, terminates the iteration and returns it;
otherwise, returns false.  Does not work as an operand of `:values'."
  '(nil #'(lambda (ignore new)
	    (declare (ignore ignore))
	    (if new (return new) nil))))

(def-gmap-res-type sum (&key filterp)
  "Returns the sum of the values, optionally filtered by `filterp'."
  `(0 #'+ nil ,filterp))

(def-gmap-res-type count-if ()
  "Returns the number of true values."
  '(0 #'(lambda (n new)
	  (if new (1+ n) n))))

(def-gmap-res-type max (&key filterp key)
  "Optionally filters the values by `filterp', then returns the maximum, or if `key'
is supplied, the value with the maximum key (if that's not unique, returns the first
one); or `nil' if no values were supplied (or survived filtering).  If `key' is
`:second-value', the second value of the mapped function is used; for example,

   (gmap (:result max :key :second-value) nil (:arg fset::bag-pairs b))

returns the (first) member of bag `b' with the maximum multiplicity."
  (if key
      (let ((key-val-var (gensym "KEY-VAL-")))
	`(nil
	   ,(if (eq key ':second-value)
		`(:consume 2 #'(lambda (old new key)
				 (if (or (null ,key-val-var) (> key ,key-val-var))
				     (progn
				       (setq ,key-val-var key)
				       new)
				   old)))
	      `#'(lambda (old new)
		   (let ((new-key-val ,(gmap>funcall key 'new)))
		     (if (or (null ,key-val-var) (> new-key-val ,key-val-var))
			 (progn
			   (setq ,key-val-var new-key-val)
			   new)
		       old))))
	   nil ,filterp ((,key-val-var nil))))
    `(nil
      #'(lambda (old new) (if (null old) new (max old new)))
      nil
      ,filterp)))

(def-gmap-res-type min (&key filterp key)
  "Optionally filters the values by `filterp', then returns the minimum, or if `key'
is supplied, the value with the minimum key (if that's not unique, returns the first
one); or `nil' if no values were supplied (or survived filtering).  If `key' is
`:second-value', the second value of the mapped function is used; for example,

   (gmap (:result min :key :second-value) nil (:arg fset::bag-pairs b))

returns the (first) member of bag `b' with the minimum multiplicity."
  (if key
      (let ((key-val-var (gensym "KEY-VAL-")))
	`(nil
	   ,(if (eq key ':second-value)
		`(:consume 2 #'(lambda (old new key)
				 (if (or (null ,key-val-var) (< key ,key-val-var))
				     (progn
				       (setq ,key-val-var key)
				       new)
				   old)))
	      `#'(lambda (old new) (let ((new-key-val ,(gmap>funcall key 'new)))
				     (if (or (null ,key-val-var) (< new-key-val ,key-val-var))
					 (progn
					   (setq ,key-val-var new-key-val)
					   new)
				       old))))
	   nil
	   ,filterp
	   ((,key-val-var nil))))
    `(nil
      #'(lambda (old new) (if (null old) new (min old new)))
      nil
      ,filterp)))

(def-gmap-res-type vector (&key use-vector length fill-pointer adjustable filterp)
  "Constructs a vector containing the results.  If `use-vector' is supplied,
the argument will be filled with the results and returned; if `fill-pointer'
is true and `adjustable' is true, it must have a fill pointer and be adjustable,
and values will be appended to it with `vector-push-extend'; if `fill-pointer'
is true and `adjustable' is false, it must have a fill pointer, and values will
be appended to it with `vector-push'; otherwise, the vector is assumed to be
simple and must be large enough to hold the results.  (Recall that `vector-push'
has no effect if the vector is full.)

If `use-vector' is not supplied, a vector will be constructed and returned;
if `length' is supplied, returns a simple vector of the specified length (which
must be sufficient to hold the results); otherwise, returns a simple vector of
the correct length (but to do this, it must cons a temporary list).

In any case, if `filterp' is supplied, it is a predicate of one argument,
the value of the function being mapped, that says whether to include it in
the result."
  (cond ((and use-vector fill-pointer)
	 `(,use-vector
	   #'(lambda (vec next-elt)
	       (,(if adjustable
		     'vector-push-extend
		   'vector-push)
		next-elt vec)
	       vec)
	   nil
	   ,filterp))
	((and use-vector length)
	 (error "Makes no sense to supply both `:use-vector' and `:length'"))
	(use-vector
	 (let ((index-temp (gensym "INDEX-")))
	   `(,use-vector
	     #'(lambda (vec next-elt)
		 (setf (svref vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(length
	 (let ((index-temp (gensym "INDEX-")))
	   `((make-array ,length)
	     #'(lambda (vec next-elt)
		 (setf (svref vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(t
	 (let ((len-temp (gensym "LEN-")))
	   `(nil
	     #'(lambda (list next-elt)
		 (incf (the fixnum ,len-temp))
		 (cons next-elt list))
	     #'(lambda (list)
		 (let ((vec (make-array (the fixnum ,len-temp))))
		   (dolist (x list)
		     (setf (svref vec (decf (the fixnum ,len-temp))) x))
		   vec))
	     ,filterp
	     ((,len-temp 0)))))))

(def-gmap-res-type string (&key use-string length fill-pointer adjustable filterp)
  "Constructs a string containing the results.  If `use-string' is supplied,
the argument will be filled with the results and returned; if `fill-pointer'
is true and `adjustable' is true, it must have a fill pointer and be adjustable,
and values will be appended to it with `vector-push-extend'; if `fill-pointer'
is true and `adjustable' is false, it must have a fill pointer, and values will
be appended to it with `vector-push'; otherwise, the vector is assumed to be
simple and must be large enough to hold the results.  (Recall that `vector-push'
has no effect if the vector is full.)

If `use-string' is not supplied, a string will be constructed and returned;
if `length' is supplied, returns a simple string of the specified length (which
must be sufficient to hold the results); otherwise, returns a simple string of
the correct length (but to do this, it must cons a temporary list).

In any case, if `filterp' is supplied, it is a predicate of one argument,
the value of the function being mapped, that says whether to include it in
the result."
  (cond ((and use-string fill-pointer)
	 `(,use-string
	   #'(lambda (str next-elt)
	       (,(if adjustable
		     'vector-push-extend
		   'vector-push)
		next-elt str)
	       str)
	   nil
	   ,filterp))
	(use-string
	 (let ((index-temp (gensym "INDEX-")))
	   `(,use-string
	     #'(lambda (vec next-elt)
		 (setf (schar vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(length
	 (let ((index-temp (gensym "INDEX-")))
	   `((make-string ,length)
	     #'(lambda (vec next-elt)
		 (setf (schar vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(t
	 (let ((len-temp (gensym "LEN-")))
	   `(nil				; init
	     #'(lambda (list next-elt)		; nextfn
		 (incf (the fixnum ,len-temp))
		 (cons next-elt list))
	     #'(lambda (list)			; cleanup
		 (let ((str (make-string (the fixnum ,len-temp))))
		   (dolist (x list)
		     (setf (schar str (decf (the fixnum ,len-temp))) x))
		   str))
	     ,filterp
	     ((,len-temp 0)))))))


; End of gmap.lisp
