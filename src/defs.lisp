;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-User -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :cl-user)

(defpackage :misc-extensions.new-let
  (:nicknames :new-let)
  (:use :cl)
  (:shadow cl:let cl:cond)
  (:export #:let #:cond #:nlet #:bcond #:fn))

(defpackage :misc-extensions.gmap
  (:nicknames :gmap)
  (:use :cl)
  (:export #:gmap #:def-gmap-arg-type #:def-arg-type #:def-gmap-res-type #:def-result-type
	   ;; Predefined argument and result type names that aren't inherited from `cl:'.
	   #:constant #:index #:index-inc #:alist #:plist #:sum #:product)
  (:shadowing-import-from :new-let #:let #:cond))
;;; Instead of `(:use :gmap)', I recommend that clients do `(:import-from :gmap #:gmap)',
;;; adding to the import list any of the predefined argument and result type names that
;;; they wish to use without a `gmap:' prefix.

(defpackage :misc-extensions.rev-fun-bind
  (:nicknames :rev-fun-bind)
  (:use :cl)
  (:export #:rlabels #:rflet #:rmacrolet))

(defpackage :misc-extensions.lexical-contexts
  (:nicknames :lexical-contexts)
  (:use :cl)
  (:export #:defcontext #:with-context #:with-contexts #:import-context
	   #:deflex #:deflex-reinit #:isetq))

(defpackage :misc-extensions.define-class
  ;; No nickname for this one -- risk of collision seems higher
  (:use :cl)
  (:export #:define-class #:add-define-class-extension))

