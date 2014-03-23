;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: New-Let -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :new-let)

(defmacro fn (lambda-list &body body)
  "A variant of the `lambda' with two features: (a) the name is shorter
and (b) it automatically generates an `ignore' declaration for any parameter
whose name begins with an underscore.
Note that
  #'(fn ...)
doesn't work, since CL doesn't have Zetalisp-style lambda macros; you have
to say just
  (fn ...)
This is the modern style anyway, I think."
  (let ((params (reduce #'append
			(mapcar (lambda (ll-elt)
				  (cond ((member ll-elt '(&optional &rest &key &aux
							  &allow-other-keys))
					 nil)
					((listp ll-elt)
					 (list (if (listp (car ll-elt))
						   ;; &key ((:foo _foo) default)
						   (cadar ll-elt)
						 (car ll-elt))))
					(t (list ll-elt))))
				lambda-list))))
    `(lambda ,lambda-list
       (declare (ignore . ,(remove-if-not (lambda (sym)
					    (eql #\_ (char (symbol-name sym) 0)))
					  params)))
       . ,body)))

