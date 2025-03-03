;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: misc-extensions.define-class -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY
;;; NO WARRANTY.

(in-package :misc-extensions.define-class)

(defmacro define-class (name superclasses slot-specs &body class-options)
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
  - A doc string can appear anywhere in the slot options; a `:documentation'
    slot option will be generated
  - Or, you can use `:doc' as an abbreviation for `:documentation'

Also, a doc string for the class can be placed just before the slot-specs.

Example:

  (define-class convertible (car)
    \"A convertible is a car with a top that can be either down or up.\"
    ((top-color :must-init :readable
       \"The color of the top.\")
     ((top-state ':up) :accessible
       \"The current state of the top, `:down' or `:up'.\")))"
  (when (stringp slot-specs)
    (let ((class-doc slot-specs))
      (setq slot-specs (pop class-options))
      (setq class-options (cons (list ':documentation class-doc) class-options))))
  `(defclass ,name ,superclasses
       ,(mapcar (lambda (slot-spec)
		  (if (symbolp slot-spec) slot-spec
		    (labels ((walk (spec slot-name conc-name result)
			       (cond ((null spec) result)
				     ((member (car spec) '(:reader :writer :accessor :allocation :initarg
							   :initform :type :documentation))
				      (walk (cddr spec) slot-name conc-name
					    (list* (car spec) (cadr spec) result)))
				     ((stringp (car spec))
				      (walk (cdr spec) slot-name conc-name
					    (list* ':documentation (car spec) result)))
				     ((eq (car spec) ':must-init)
				      (walk (cdr spec) slot-name conc-name
					    (let ((initarg (intern (string slot-name) (symbol-package ':initarg))))
					      (list* ':initarg initarg
						     ':initform `(error ,(format nil "No value supplied for ~S"
										 initarg))
						     result))))
				     ((eq (car spec) ':may-init)
				      (walk (cdr spec) slot-name conc-name
					    (list* ':initarg (intern (string slot-name) (symbol-package ':initarg))
						   result)))
				     ((member (car spec) '(:readable :accessible))
				      (let ((nm (if conc-name
						    (intern (concatenate 'string conc-name (string slot-name)))
						  slot-name)))
					(walk (cdr spec) slot-name conc-name
					      (list* (if (eq (car spec) ':readable) ':reader ':accessor)
						     nm result))))
				     ((eq (car spec) ':doc)
				      (walk (cddr spec) slot-name conc-name
					    (list* ':documentation (cadr spec) result)))
				     (t (error "Unrecognized slot option ~S" (car spec))))))
		      (let ((slot-name (if (consp (car slot-spec)) (caar slot-spec)
					 (car slot-spec)))
			    (conc-name (let ((pr (assoc ':conc-name class-options)))
					 (and pr (string (cadr pr))))))
			(cons slot-name (walk (cdr slot-spec) slot-name conc-name
					      (and (consp (car slot-spec)) `(:initform ,(cadar slot-spec)))))))))
	        slot-specs)
     . ,(remove-if (lambda (x) (eq (car x) ':conc-name)) class-options)))


#||

If, using the above macro, you include doc strings without prefixing them with
`:documentation' or `:doc' -- as the macro allows you to do -- you may notice
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
	   ;; Detects a class doc string in `define-class/model'.
	   (save-excursion
	     (goto-char listbeg)
	     (forward-char)
	     (or (looking-at "define-class") (looking-at "define-model")))
	   ;; Detects doc strings in `define-class/model' slot options.
	   (save-excursion
	     (goto-char listbeg)
	     (ignore-errors
	       (backward-up-list)
	       (backward-sexp)
	       (if (looking-at "\"")
		   (backward-sexp 3)
		 (backward-sexp 2))
	       (or (looking-at "define-class") (looking-at "define-model")))))))

||#
