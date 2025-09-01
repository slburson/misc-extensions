;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

;;; GNU Emacs code that improves the indentation of `nlet' forms.

(defun common-lisp-indent-nlet (path state indent-point sexp-column normal-indent)
  ;; If we're in the body, or (we're in the clauses and) the car of the containing list
  ;; is a list, or the car of the next containing list is _not_ a list, indent normally.
  (if (or (/= 1 (car path))
	  (save-excursion
	    (goto-char indent-point)
	    (backward-up-list)
	    (forward-char)
	    (or (looking-at "(")
		(backward-char)
		(backward-up-list)
		(forward-char)
		(not (looking-at "(")))))
      (lisp-indent-259 '((&whole 4 &rest (&whole 1 1 1)) &body)
		       path state indent-point sexp-column normal-indent)
    ;; If this is the last subform, indent by 2, else 1.
    (if (save-excursion
	  (goto-char indent-point)
	  (ignore-errors
	    (forward-sexp))
	  (looking-at ")"))
	(+ sexp-column 2)
      (+ sexp-column 1))))

(put 'nlet 'common-lisp-indent-function 'common-lisp-indent-nlet)
;; Needed to find the `nlet' from inside a deeply nested clause
(setq lisp-indent-maximum-backtracking 10)
