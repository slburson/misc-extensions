;;; -*- Lisp -*-

(defsystem Misc-Extensions
  :description "The GMap iteration macro, plus a few other useful macros."
  :author "Scott L. Burson <Scott@sympoiesis.com>"
  :version "4.0.7"
  :homepage "https://github.com/slburson/misc-extensions"
  :source-control "https://github.com/slburson/misc-extensions"
  :license "Public domain"
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "defs")
			     (:file "new-let")
			     (:file "gmap")
			     (:file "fn")
			     (:file "tests")
			     (:file "rev-fun-bind")
			     (:file "define-class")
			     (:file "contexts")
			     (:file "context-tests")))))

(defsystem Misc-Extensions/Test
  :description "Test system for misc-extensions"
  :depends-on (:misc-extensions)
  :components ((:module "src"
	        :components
		((:file "tests")
		 (:file "context-tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :misc-extensions))))
  (load-system :misc-extensions/test)
  (funcall (intern "TEST-NEW-SYNTAX" :gmap))
  (funcall (intern "TEST-OLD-SYNTAX" :gmap))
  (funcall (intern "TEST-CONTEXTS" :lexical-contexts)))

