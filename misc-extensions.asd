;;; -*- Lisp -*-

(defsystem Misc-Extensions
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "defs")
			     (:file "new-let")
			     (:file "gmap")
			     (:file "fn")
			     (:file "tests")
			     (:file "rev-fun-bind")
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

