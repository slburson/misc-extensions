;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-User -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :cl-user)

(defpackage :new-let
  (:use :cl)
  (:shadow cl:let cl:cond)
  (:export #:let #:cond #:nlet #:bcond #:fn))

(defpackage :gmap
  (:use :cl)
  (:export #:gmap #:def-gmap-arg-type #:def-arg-type #:def-gmap-res-type #:def-result-type
	   ;; Predefined argument and result type names that aren't inherited from `cl:'.
	   #:constant #:index #:index-inc #:alist #:plist #:sum #:product)
  (:shadowing-import-from :new-let #:let #:cond))
;;; Instead of `(:use :gmap)', I recommend that clients do `(:import-from :gmap #:gmap)',
;;; adding to the import list any of the predefined argument and result type names that
;;; they wish to use without a `gmap:' prefix.

(defpackage :rev-fun-bind
  (:use :cl)
  (:export #:rlabels #:rflet #:rmacrolet))

(defpackage :lexical-contexts
  (:use cl)
  (:export #:defcontext #:with-context #:with-contexts #:deflex #:import-context
	   #:deflex-reinit #:isetq))

