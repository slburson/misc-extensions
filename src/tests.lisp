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

