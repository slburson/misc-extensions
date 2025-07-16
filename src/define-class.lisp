;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: misc-extensions.define-class -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY
;;; NO WARRANTY.

(in-package :misc-extensions.define-class)

(defmacro define-class (class-name superclasses slot-specs &body class-options)
  "A macro to make class definitions less verbose.  Upwardly compatible with
`cl:defclass', but adds additional syntax:

  - The first element of a slot specifier, which is normally a slot name,
    can instead be a list (slot-name initform); an `:initform' option will
    be generated
  - `:may-init' generates `:initarg :slot-name'
  - `:must-init' generates `:initarg :slot-name', and also an `:initform'
    that signals an error if the argument is not provided to `make-instance'
  - `:readable' and `:accessible' generate `:reader' resp. `:accessor' slot
    options; the name given will be the slot name, unless a _class_ option
    `:conc-name' is supplied, in which case the conc-name will be prepended
  - Additionally, `:constant' is an abbreviation for `:must-init :readable'
    if the spec contains no initform, or `:may-init :readable' if there is
    an initform (in either syntax)
  - And `:variable' is an abbreviation for `:may-init :accessible'
  - A doc string can appear anywhere in the slot options; a `:documentation'
    slot option will be generated
  - Or, you can use `:doc' as an abbreviation for `:documentation'
  - Class option `:predicate' can be supplied to define a predicate on the class

Also, a doc string for the class can be placed just before the slot-specs.

Example:

  (define-class convertible (car)
    \"A convertible is a car with a top that can be either down or up.\"
    ((top-color :constant
       \"The color of the top.\")
     ((top-state ':up) :variable
       \"The current state of the top, `:down' or `:up'.\")))"
  (when (stringp slot-specs)
    (let ((class-doc slot-specs))
      (setq slot-specs (pop class-options))
      (setq class-options (cons (list ':documentation class-doc) class-options))))
  (let* ((conc-name (let ((pr (assoc ':conc-name class-options)))
		      (and pr (string (cadr pr)))))
	 (extension-data nil)
	 (expanded-slot-specs
	   (mapcar (lambda (slot-spec)
		      (if (symbolp slot-spec) slot-spec
			(let ((slot-name
				(if (consp (car slot-spec)) (caar slot-spec)
				  (car slot-spec)))
			      (initform? (consp (car slot-spec)))
			      (constant? nil))
			  (labels ((walk (spec result)
				     (cond ((null spec)
					    (if constant?
						(progn
						  (setq constant? nil)
						  (walk `(,(if initform? ':may-init ':must-init) :readable)
							result))
					      result))
					   ((member (car spec) '(:doc :documentation))
					    (walk (cddr spec)
						  (list* ':documentation (remove-indentation (cadr spec)) result)))
					   ((member (car spec) '(:reader :writer :accessor :allocation :initarg
								 :initform :type))
					    (when (eq (car spec) ':initform)
					      (setq initform? t))
					    (walk (cddr spec) (list* (car spec) (cadr spec) result)))
					   ((stringp (car spec))
					    (walk (cdr spec) (list* ':documentation (remove-indentation (car spec))
								    result)))
					   ((eq (car spec) ':must-init)
					    (walk (cdr spec)
						  (let ((initarg (intern (string slot-name) (symbol-package ':doc))))
						    (list* ':initarg initarg
							   ':initform `(error ,(format nil "No value supplied for ~S"
										       initarg))
							   result))))
					   ((eq (car spec) ':may-init)
					    (walk (cdr spec)
						  (list* ':initarg (intern (string slot-name) (symbol-package ':doc))
							 result)))
					   ((member (car spec) '(:readable :accessible))
					    (let ((nm (if conc-name
							  (intern (concatenate 'string conc-name (string slot-name)))
							slot-name)))
					      (walk (cdr spec)
						    (list* (if (eq (car spec) ':readable) ':reader ':accessor)
							   nm result))))
					   ;; A couple of second-level abbreviations.
					   ((eq (car spec) ':constant)
					    (setq constant? t)
					    (walk (cdr spec) result))
					   ((eq (car spec) ':variable)
					    (walk (list* ':may-init ':accessible (cdr spec)) result))
					   (t
					    (let ((ext-fn? (and (symbolp (car spec))
								(get (car spec) 'define-class-extensions))))
					      (if ext-fn?
						  (let ((prev-pr (assoc (car spec) extension-data)))
						    (if prev-pr
							(push slot-name (cdr prev-pr))
						      (push (cons (car spec) (list slot-name))
							    extension-data))
						    (walk (cdr spec) result))
						(error "Unrecognized slot option ~S" (car spec))))))))
			    (cons slot-name (walk (cdr slot-spec)
						  (and initform? `(:initform ,(cadar slot-spec)))))))))
	     slot-specs)))
    `(progn
       (defclass ,class-name ,superclasses
	   ,expanded-slot-specs
	 . ,(remove-if (lambda (x) (member (car x) '(:conc-name :predicate))) class-options))
       ,@(let ((pr (assoc ':predicate class-options)))
	   (and pr `((defun ,(cadr pr) (x) (typep x ',class-name)))))
       . ,(reduce #'append
		  (mapcar (lambda (pr)
			    (mapcar (lambda (ext-fn)
				      (funcall ext-fn class-name (reverse (cdr pr))))
				    (get (car pr) 'define-class-extensions)))
			  extension-data)))))

(defun add-define-class-extension (option ext-fn)
  "Adds a keyword slot option to `define-class'.  `option' should be a
keyword, and `ext-fn' the name of a function.  If the option is used for
any slots in a `define-class' form, the function will be called \(at load
time\) with two arguments, the class name and the list of slots on which
the option appeared."
  (pushnew ext-fn (get option 'define-class-extensions)))

;;; For `remove-indentation' to work correctly in the presence of tabs, it needs
;;; to know how wide they are.  If you use tabs at all -- many don't -- and set
;;; them to a different width, you'll want to change this.  Be aware that it takes
;;; effect at compile time.
(defparameter *tab-width* 8)

(defun remove-indentation (str)
  "De-indents the second and subsequent lines of `str' by an amount equal
to the minimum indentation of those lines."
  (let ((lines nil))
    (do ((pos 0))
	(nil)
      (let* ((nl (position #\Newline str :start pos))
	     (line (subseq str pos nl)))
	(push line lines)
	(if nl (setq pos (1+ nl))
	  (return))))
    (setq lines (nreverse lines))
    (labels ((line-indentation (line)
	       (do ((i 0 (1+ i))
		    (indent 0))
		   ((= i (length line)) nil)
		 (case (char line i)
		   (#\Space (incf indent))
		   (#\Tab (setq indent (* *tab-width* (ceiling (1+ indent) *tab-width*))))
		   (t (return (values indent i))))))
	     (reindent (line text-start new-indent)
	       (concatenate 'string (make-string new-indent :initial-element #\Space)
			    (subseq line text-start))))
      (let ((min-indent nil)
	    (new-lines (list (car lines))))
	(dolist (line (cdr lines))
	  (unless (zerop (length line))
	    (let ((indent (line-indentation line)))
	      (when indent
		(setq min-indent (if min-indent (min min-indent indent) indent))))))
	(dolist (line (cdr lines))
	  (new-let:nlet ((indent text-start (line-indentation line)))
	    (push (if (null indent) ""
		    (reindent line text-start (- indent min-indent)))
		  new-lines)))
	(reduce (lambda (x y) (concatenate 'string x (string #\Newline) y))
		(nreverse new-lines))))))

#||

Here's the Emacs indentation definition I use:

(put 'define-class 'common-lisp-indent-function
     '(6 (&whole 4 &rest 1) &rest (&whole 2 &rest (&whole 1 &rest 2))))

Also, if, using the above macro, you include doc strings without prefixing them
with `:documentation' or `:doc' -- as the macro allows you to do -- you may notice
that Emacs renders them in the face for ordinary strings (`font-lock-string-face'),
rather than in the face used for doc strings (`font-lock-doc-face').
This Emacs-Lisp customization will cause them to be rendered in the doc string face:

;;; Current wrt GNU Emacs 30.0.50.
(defun lisp-string-after-doc-keyword-p (listbeg startpos)
  "Return non-nil if `:documentation' symbol ends at STARTPOS inside a list.
`:doc' can also be used.

LISTBEG is the position of the start of the innermost list
containing STARTPOS."
  (and listbeg                          ; We are inside a Lisp form.
       (or (save-excursion
             (goto-char startpos)
             (ignore-errors
               (progn (backward-sexp 1)
                      (looking-at ":documentation\\_>\\|:doc\\_>"))))
	   ;; Detects a class doc string in `define-class'.
	   (save-excursion
	     (goto-char listbeg)
	     (forward-char)
	     (looking-at "define-class"))
	   ;; Detects doc strings in `define-class' slot options.
	   (save-excursion
	     (goto-char listbeg)
	     (ignore-errors
	       (backward-up-list)
	       (backward-sexp)
	       (if (looking-at "\"")
		   (backward-sexp 3)
		 (backward-sexp 2))
	       (looking-at "define-class"))))))

||#
