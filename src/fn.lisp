;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: New-Let -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :new-let)

(defmacro fn (lambda-list &body body)
  "A variant of `lambda' with two features: (a) the name is shorter, and (b)
it automatically generates an `ignore' declaration for any parameter
whose name begins with an underscore.  If the parameter's name is just `_',
renames it to a gensym, so as to avoid duplicate-parameter errors (this
applies to any symbol named `_', regardless of package).
Note that
  #'(fn ...)
doesn't work, since CL doesn't have Zetalisp-style lambda macros; you have
to say just
  (fn ...)
This is the modern style anyway, I think."
  (let ((to-ignore nil))
    (labels ((rewrite (ll)
	       (and ll (let ((ll-elt (car ll))
			     ((new-ll-elt
				(cond ((member ll-elt '(&optional &rest &key &aux
							&allow-other-keys))
				       ll-elt)
				      ((listp ll-elt)
				       (if (listp (car ll-elt))
					   ;; &key ((:foo _foo) default)
					   (cons (list (caar ll-elt) (rewrite-param (cadar ll-elt)))
						 (cdr ll-elt))
					 (rewrite-param (car ll-elt))))
				      (t (rewrite-param ll-elt))))))
			 (cons new-ll-elt (rewrite (cdr ll))))))
	     (rewrite-param (param)
	       (if (string= (symbol-name param) "_")
		   (let ((g (gensym "_")))
		     (push g to-ignore)
		     g)
		 (progn
		   (when (eql #\_ (char (symbol-name param) 0))
		    (push param to-ignore))
		   param))))
      (let ((new-lambda-list (rewrite lambda-list)))
	`(lambda ,new-lambda-list
	   ,@(and to-ignore `((declare (ignore . ,to-ignore))))
	   . ,body)))))

