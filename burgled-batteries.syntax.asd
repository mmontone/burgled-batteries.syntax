(asdf:defsystem #:burgled-batteries.syntax
  :serial t
  :description "Embedded Python syntax for burgled-batteries"
  :author "Mariano Montone"
  :license "MIT"
  :homepage "https://github.com/mmontone/burgled-batteries.syntax"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))		
  :depends-on (#:burgled-batteries 
	       #:esrap
	       #:named-readtables)
  :components ((:file "package")
	       (:file "syntax"))
  :in-order-to ((asdf:test-op 
		 (asdf:test-op :burgled-batteries.syntax-test))))
