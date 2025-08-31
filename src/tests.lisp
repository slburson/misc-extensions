;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GMap -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package gmap)

;;; Itty bitty test suite -- also serves as a demo.
(defun test-new-syntax ()
  (unless (and (equalp (gmap (:result list :filterp #'oddp) #'+
			     (:arg index 0 6) (:arg constant 17))
		       '(17 19 21))
	       (equalp (gmap (:result alist) #'(lambda (x y) (values x (* x y)))
			     (:arg plist '(2 14 3 26 -4 19)))
		       '((2 . 28) (3 . 78) (-4 . -76)))
	       ;; `nil' as the function to be mapped is shorthand for the identity function.
	       (equalp (gmap (:result vector) nil (:arg index 11 1 :incr -3))
		       #(8 5 2))
	       (equalp (gmap (:result vector :use-vector (make-array 1 :fill-pointer 0 :adjustable t)
				      :fill-pointer t :adjustable t)
			     #'+
			     (:arg simple-vector #(3 44 217))
			     (:arg index-inc 17 19))
		       #(20 62 236))
	       (equalp (gmap (:result vector :use-vector (make-array 3))
			     #'+
			     (:arg vector #(33 44 55))
			     (:arg index-inc 22 13 :incr -4))
		       #(55 62 69))
	       (equalp (gmap (:result string) #'code-char (:arg index 32 119 :incr 12))
		       " ,8DP\\ht")
	       (equalp (gmap (:result string :use-string (make-array 1 :element-type 'character
								       :fill-pointer 0 :adjustable t)
				      :fill-pointer t :adjustable t)
			     #'code-char
			     (:arg index-inc 33 105 :incr 12))
		       "!-9EQ]i")
	       (equalp (gmap (:result string :use-string (make-string 4)) #'code-char
			     (:arg index 70 74))
		       "FGHI")
	       (equalp (gmap (:result array '(2 3) :element-type 'single-float)
			     (lambda (rm-idx) (let ((i j (floor rm-idx 3)))
						(float (+ (* i i) (* j j)))))
			     (:arg index 0 6))
		       #2A((0.0 1.0 4.0) (1.0 2.0 5.0)))
	       (equalp (gmap (:result array '(2 3))
			     (lambda (x) (* x 1.5))
			     (:arg array #2A((0.0 1.0 4.0) (1.0 2.0 5.0))))
		       #2A((0.0 1.5 6.0) (1.5 3.0 7.5)))
	       (equalp (gmap (:result list) nil
			     (:arg array #2A((0.0 1.0 4.0) (1.0 2.0 5.0)) :start 1 :stop 5))
		       '(1.0 4.0 1.0 2.0))
	       (equalp (gmap (:result append) nil
			     (:arg list '((seoie 2dlkes) (zlcxildk oiden xinld)
					  (kthsio soi3kd zilk) (oiwnxlk lkdw))))
		       '(seoie 2dlkes zlcxildk oiden xinld kthsio soi3kd zilk oiwnxlk lkdw))
	       ;; If the result-spec is `nil', `gmap' functions as a generalization of `mapc'.
	       ;; While I don't encourage this usage mode, it is sometimes more elegant than
	       ;; the available alternatives such as `do'.
	       (equalp (let ((vals nil))
			 (gmap nil #'(lambda (x y) (push (list x y) vals))
			       (:arg index 0)
			       (:arg list '(foo bar baz)))
			 (nreverse vals))
		       '((0 foo) (1 bar) (2 baz)))
	       ;; Fun with multiple values!
	       ;; The multiple-value identity function can be written as `nil' or as `#'values'.
	       (equalp (multiple-value-list (gmap (:result :values list vector) nil
						  (:arg alist '((x . 1) (y . 2) (z . 3)))))
		       '((x y z) #(1 2 3)))
	       (equalp (gmap (:result alist) nil (:arg plist '(a 17 b 28 q 47)))
		       '((a . 17) (b . 28) (q . 47)))
	       (equalp (gmap (:result plist) (lambda (x y) (values (string x) (float y)))
			     (:arg alist '((X . 2) (Y . 13) (Z . 44))))
		       '("X" 2.0 "Y" 13.0 "Z" 44.0))
	       ;; Shows how multiple-value-consuming result-specs interact with `:values'.
	       (equalp (multiple-value-list
			(gmap (:result :values alist vector)
			      (lambda (x y) (values x y (1+ y)))
			      (:arg plist '(x 1 y 2 z 3))))
		       '(((x . 1) (y . 2) (z . 3)) #(2 3 4))))
    (error "Some test case failed")))

(defun test-old-syntax ()
  (unless (and (equalp (gmap (:list :filterp #'oddp) #'+ (:index 0 6) (:constant 17))
		       '(17 19 21))
	       (equalp (gmap :alist #'(lambda (x y) (values x (* x y)))
			     (:plist '(2 14 3 26 -4 19)))
		       '((2 . 28) (3 . 78) (-4 . -76)))
	       (equalp (gmap :vector nil (:index 11 1 :incr -3))
		       #(8 5 2))
	       (equalp (gmap (:vector :use-vector (make-array 1 :fill-pointer 0
								:adjustable t)
				      :fill-pointer t :adjustable t)
			     #'+
			     (:simple-vector #(3 44 217))
			     (:index-inc 17 19))
		       #(20 62 236))
	       (equalp (gmap (:vector :use-vector (make-array 3))
			     #'+
			     (:vector #(33 44 55))
			     (:index-inc 22 13 :incr -4))
		       #(55 62 69))
	       (equalp (gmap :string #'code-char (:index 32 119 :incr 12))
		       " ,8DP\\ht")
	       (equalp (gmap (:string :use-string (make-array 1 :element-type 'character
								:fill-pointer 0 :adjustable t)
				      :fill-pointer t :adjustable t)
			     #'code-char (:index-inc 33 105 :incr 12))
		       "!-9EQ]i")
	       (equalp (gmap (:string :use-string (make-string 4)) #'code-char
			     (:index 70 74))
		       "FGHI")
	       (equalp (gmap :append nil (:list '((seoie 2dlkes) (zlcxildk oiden xinld)
						  (kthsio soi3kd zilk) (oiwnxlk lkdw))))
		       '(seoie 2dlkes zlcxildk oiden xinld kthsio soi3kd zilk oiwnxlk lkdw))
	       ;; Fun with multiple values!
	       (equalp (multiple-value-list (gmap (:values :list :vector) nil
						  (:alist '((x . 1) (y . 2) (z . 3)))))
		       '((x y z) #(1 2 3)))
	       (equalp (gmap :alist nil (:plist '(a 17 b 28 q 47)))
		       '((a . 17) (b . 28) (q . 47)))
	       (equalp (gmap :plist (lambda (x y) (values (string x) (float y)))
			     (:alist '((X . 2) (Y . 13) (Z . 44))))
		       '("X" 2.0 "Y" 13.0 "Z" 44.0))
	       ;; Shows how multiple-value-consuming result-specs interact with `:values'.
	       (equalp (multiple-value-list
			(gmap (:values :alist :vector)
			      (lambda (x y) (values x y (1+ y)))
			      (:plist '(x 1 y 2 z 3))))
		       '(((x . 1) (y . 2) (z . 3)) #(2 3 4))))
    (error "Some test case failed")))


(defun test-nlet ()
  (macrolet ((test (form)
	       `(unless ,form
		  (error "Test failed: ~S" ',form))))
    ;; Tests that the `incf' follows the binding of `a'.
    (cl:let ((a 0)
	     (b 1)
	     (c 2))
      (nlet ((a (+ b c))
	     (b c (values (+ a 3) (+ (incf c) (arbfun 2)))))
	(test (= (+ a b c) 11))))
    ;; Tests that the `c' in the `b c' clause doesn't shadow the outer one in the `d' clause.
    (cl:let ((a 0)
	     (b 1)
	     (c 2))
      (nlet ((a (+ b c))
	     (b c (values (+ a 3) (+ c (arbfun 2))))
	     (d (+ c 2)))
	(test (= (+ a b c d) 14))))
    ;; Test that bound declarations wind up in the right places.
    (locally
	(declare (special foo bar baz))
      (setq foo 10)
      (setq bar 11)
      (setq baz 12))
    (nlet ((foo 20)
	   ((bar baz (values 21 22))
	    ((a b (values 8 42))))
	   (c 31))
      (declare (special foo bar baz))
      (test (= (+ (value-of-locally-special-foo)
		  (value-of-locally-special-bar)
		  (value-of-locally-special-baz)
		  a b c)
	       144)))
    (nlet ((foo 20)
	   ((bar baz (values 21 22))
	    ((a b (values 8 42))))
	   ((c 31)))  ; <-- only difference from previous
      (declare (special foo bar baz))
      (test (= (+ (value-of-locally-special-foo)
		  (value-of-locally-special-bar)
		  (value-of-locally-special-baz)
		  a b c)
	       144)))
    ;; Test for an observed bug causing miscompilation on consecutive multi clauses.
    (nlet ((a b (values 7 42))
	   (c d (values 13 8)))
      (test (= (+ a b c d) 70))))
  t)

;;; Block SBCL's constant propagation.
(defun arbfun (x) x)

(defun value-of-locally-special-foo ()
  (declare (special foo))
  foo)

(defun value-of-locally-special-bar ()
  (declare (special bar))
  bar)

(defun value-of-locally-special-baz ()
  (declare (special baz))
  baz)


(defun test-mvlet ()
  (macrolet ((test (form)
	       `(unless ,form
		  (error "Test failed: ~S" ',form))))
    (cl:let ((a 1)
	     (b 2)
	     (c 3))
      ;; Test that the outer `a', `b', and `c' are not shadowed within the clauses.
      (mvlet ((a (+ a 3))
	      (c (+ a b c)))
	(test (= (+ a b c) 12)))
      (mvlet ((a b (values (+ a 3) (+ b 3)))
	      (c (+ a b c)))
	(test (= (+ a b c) 15)))
      (mvlet ((a (+ a 3))
	      (b c (values (+ a b) (+ a c))))
	(test (= (+ a b c) 11))))
    (cl:let ((foo nil))
      ;; Test that init-forms are evaluated in the correct order.
      (mvlet ((a (progn (push 'a foo) 3))
	      (b c (progn (push 'bc foo) (values 4 7)))
	      (d (progn (push 'd foo) 11)))
	(test (= (+ a b c d) 25))
	(test (equal foo '(d bc a)))))
    ;; Test that bound declarations wind up in the right places.
    (locally
	(declare (special foo bar baz))
      (setq foo 10)
      (setq bar 11)
      (setq baz 12))
    (mvlet ((foo 20)
	    (bar baz (values 21 22))
	    (a b (values 8 42))
	    (c 31))
      (declare (special foo bar baz))
      (test (= (+ (value-of-locally-special-foo)
		  (value-of-locally-special-bar)
		  (value-of-locally-special-baz)
		  a b c)
	       144))))
  t)

(defun test-mvlet* ()
  (macrolet ((test (form)
	       `(unless ,form
		  (error "Test failed: ~S" ',form))))
    (cl:let ((a 1)
	     (b 2)
	     (c 3))
      ;; Test that the outer `a', `b', and `c' are shadowed correctly within the clauses.
      (mvlet* ((a (+ a 3))
	       (c (+ a b c)))
	(test (= (+ a b c) 15)))
      (mvlet* ((a b (values (+ a 3) (+ b 3)))
	       (c (+ a b c)))
	(test (= (+ a b c) 21)))
      (mvlet* ((a (+ a 3))
	       (b c (values (+ a b) (+ a c))))
	(test (= (+ a b c) 17)))
      (mvlet* ((a (+ a 3))
	       (d 8)
	       (b c (values (+ a b) (+ a c))))
	(test (= (+ a b c d) 25))))
    (cl:let ((foo nil))
      ;; Test that init-forms are evaluated in the correct order.
      (mvlet ((a (progn (push 'a foo) 3))
	      (b c (progn (push 'bc foo) (values 4 7)))
	      (d (progn (push 'd foo) 11)))
	(test (= (+ a b c d) 25))
	(test (equal foo '(d bc a)))))
    ;; Test that bound declarations wind up in the right places.
    (locally
	(declare (special foo bar baz))
      (setq foo 10)
      (setq bar 11)
      (setq baz 12))
    (mvlet* ((foo 20)
	     (bar baz (values 21 22))
	     (a b (values 8 42))
	     (c 31))
      (declare (special foo bar baz))
      (test (= (+ (value-of-locally-special-foo)
		  (value-of-locally-special-bar)
		  (value-of-locally-special-baz)
		  a b c)
	       144)))
    (mvlet* ((foo 20)
	     (a b (values 8 42))
	     (c 31)
	     (bar baz (values 21 22)))
      (declare (special foo bar baz))
      (test (= (+ (value-of-locally-special-foo)
		  (value-of-locally-special-bar)
		  (value-of-locally-special-baz)
		  a b c)
	       144))))
  t)
