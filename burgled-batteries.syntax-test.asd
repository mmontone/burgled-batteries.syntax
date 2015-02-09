(asdf:defsystem #:burgled-batteries.syntax-test
  :serial t
  :description "Tests of embedded Python syntax for burgled-batteries"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:burgled-batteries.syntax
	       #:lift)
  :components ((:module "t"
			:components
			((:file "package")
			 (:file "tests"))
			:serial t))
  :serial t
  :perform (asdf:test-op (o c)
			 (uiop:symbol-call :python.syntax-test :run-tests)))
