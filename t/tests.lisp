(in-package :python.syntax-test)

(lift:deftestsuite burgled-batteries.syntax ()
  ()
  (:setup (burgled-batteries:startup-python))
  (:teardown (burgled-batteries:shutdown-python)))

(defun run-tests ()
  (lift::summarize-test-result 
   (lift:run-tests :suite 'burgled-batteries.syntax)
   t
   :describe))

(lift:addtest (burgled-batteries.syntax)
  literal-string
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "''")
                    '(:literal-string ""))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "'asdf'")
                    '(:literal-string "asdf"))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "'foo bar'")
                    '(:literal-string "foo bar"))
  (lift:ensure-same (python.syntax::parse-python "'foo bar'")
                    '(:literal-string "foo bar"))
  (lift:ensure-same (python.syntax::parse-python "''")
                    '(:literal-string ""))
  (lift:ensure-same (python.syntax::parse-python "'asdf'")
                    '(:literal-string "asdf"))
  (lift:ensure-error (esrap:parse 'python.syntax::literal-string "'"))
  (lift:ensure-error (esrap:parse 'python.syntax::literal-string "'''"))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "'\\''")
                    '(:literal-string "\\'"))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "\"\"")
                    '(:literal-string ""))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "\"asdf\"")
                    '(:literal-string "asdf"))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "\"foo bar\"")
                    '(:literal-string "foo bar"))
  (lift:ensure-same (esrap:parse 'python.syntax::literal-string "\"\\\"asdf\\\"\"")
                    '(:literal-string "\\\"asdf\\\"")))

(lift:addtest (burgled-batteries.syntax)
  literal-integer
  (lift:ensure-same (python.syntax::parse-python "234")
                    '(:literal-integer 234))
  (lift:ensure-same (python.syntax::parse-python "-234")
                    '(:literal-integer -234))
  (lift:ensure-same (python.syntax::parse-python "a234")
                    '(:python-reference "a234"))
  (lift:ensure-error (python.syntax::parse-python "234asdf")))

(lift:addtest (burgled-batteries.syntax)
  transform-syntax
  (lift:ensure-same (python.syntax::parse-python "^'asdf'")
                    '(:TRANSFORM (:LITERAL-STRING "asdf")))
  (lift:ensure-same (python.syntax::parse-python "^$asdf")
                    '(:TRANSFORM (:LISP-REFERENCE "$asdf"))))

(lift:addtest (burgled-batteries.syntax)
  references
  (lift:ensure-same (python.syntax::parse-python "a")
                    '(:PYTHON-REFERENCE "a"))
  (lift:ensure-same (python.syntax::parse-python "$a")
                    '(:lisp-reference "$a")))

(lift:addtest (burgled-batteries.syntax)
  function-application
  (lift:ensure-same
   (python.syntax::parse-python "str(a)")
   '(:FUNCTION-CALL "str" ((:PYTHON-REFERENCE "a"))))
  (lift:ensure-same
   (python.syntax::parse-python "print(str(a))")
   '(:FUNCTION-CALL "print" ((:FUNCTION-CALL "str" ((:PYTHON-REFERENCE "a"))))))
  (lift:ensure-same
   (esrap:parse 'python.syntax::args-list "(x,y)")
   '((:PYTHON-REFERENCE "x") (:PYTHON-REFERENCE "y")))
  (lift:ensure-same
   (esrap:parse 'python.syntax::kwarg "x=2")
   '(:NAMED-ARG "x" (:LITERAL-INTEGER 2)))
  (lift:ensure-same (esrap:parse 'python.syntax::args-list "(x=2)")
                    '((:NAMED-ARG "x" (:LITERAL-INTEGER 2))))
  (lift:ensure-same (esrap:parse 'python.syntax::args-list "(x, y=2)")
                    '((:PYTHON-REFERENCE "x") (:NAMED-ARG "y" (:LITERAL-INTEGER 2))))
  (lift:ensure-same (esrap:parse 'python.syntax::function-call "f(x)")
                    '(:FUNCTION-CALL "f" ((:PYTHON-REFERENCE "x"))))
  (lift:ensure-same (esrap:parse 'python.syntax::function-call "f(x,y=22)")
                    '(:FUNCTION-CALL "f"
                      ((:PYTHON-REFERENCE "x") (:NAMED-ARG "y" (:LITERAL-INTEGER 22)))))
  (lift:ensure-same (python.syntax::parse-python "f(x, $y)")
                    '(:FUNCTION-CALL "f" ((:PYTHON-REFERENCE "x") (:LISP-REFERENCE "$y")))))

(lift:addtest (burgled-batteries.syntax)
  accessing
  (lift:ensure-same (python.syntax::parse-python "a.x")
                    '(:PROPERTY-ACCESS (:PYTHON-REFERENCE "a") "x"))
  (lift:ensure-same (python.syntax::parse-python "a.x.y(44)")
                    '(:METHOD-CALL (:PROPERTY-ACCESS (:PYTHON-REFERENCE "a") "x") "y"
                      ((:LITERAL-INTEGER 44))))
  (lift:ensure-same (python.syntax::parse-python "a.x(x)")
                    '(:METHOD-CALL (:PYTHON-REFERENCE "a") "x" ((:PYTHON-REFERENCE "x"))))
  (lift:ensure-same (python.syntax::parse-python "x.y(44).aa")
                    '(:PROPERTY-ACCESS
                      (:METHOD-CALL (:PYTHON-REFERENCE "x") "y" ((:LITERAL-INTEGER 44))) "aa"))
  (lift:ensure-same (python.syntax::parse-python "x[22]")
                    '(:INDEX-ACCESS (:PYTHON-REFERENCE "x") (:LITERAL-INTEGER 22)))
  (lift:ensure-same (python.syntax::parse-python "f(x).x[44]")
                    '(:INDEX-ACCESS
                      (:PROPERTY-ACCESS (:FUNCTION-CALL "f" ((:PYTHON-REFERENCE "x"))) "x")
                      (:LITERAL-INTEGER 44)))
  (lift:ensure-same (python.syntax::parse-python "x.y(44)[22]")
                    '(:INDEX-ACCESS
                      (:METHOD-CALL (:PYTHON-REFERENCE "x") "y" ((:LITERAL-INTEGER 44)))
                      (:LITERAL-INTEGER 22))))

(lift:addtest (burgled-batteries.syntax)
  assignment
  (lift:ensure-same (python.syntax::parse-python "x = 22")
		    '(:ASSIGNMENT (:PYTHON-REFERENCE "x") (:LITERAL-INTEGER 22)))
  (lift:ensure-same (python.syntax::parse-python "x['lala'] = f(y)")
		    '(:ASSIGNMENT (:INDEX-ACCESS (:PYTHON-REFERENCE "x") (:LITERAL-STRING "lala"))
		      (:FUNCTION-CALL "f" ((:PYTHON-REFERENCE "y")))))
  (lift:ensure-same (python.syntax::parse-python "x = f(x)")
		    '(:ASSIGNMENT (:PYTHON-REFERENCE "x")
		      (:FUNCTION-CALL "f" ((:PYTHON-REFERENCE "x"))))))

;; (python.syntax::parse-python "x.y.a(22)")
;; (python.syntax::parse-python "x.y(33).z('asdf')")
;; (python.syntax::parse-python "$x.y(33, name='asdf')")
;; (python.syntax::parse-python "f(33, name='asdf')")
;; (python.syntax::parse-python "f(x,name='asdf')")
;; (python.syntax::parse-python "f(f, x=x,y=33)")
;; (python.syntax::parse-python "x.f(f, x=x, y=33)")

(lift:addtest (burgled-batteries.syntax)
  literal-list
  (lift:ensure-same (python.syntax::parse 'python.syntax::literal-list "[1,2]")
		    '(:LITERAL-LIST ((:LITERAL-INTEGER 1) (:LITERAL-INTEGER 2)))))

(lift:addtest (burgled-batteries.syntax)
  literal-dictionary
  (lift:ensure-same (python.syntax::parse 'python.syntax::literal-dictionary "{'sdf':2}")
		    '(:LITERAL-DICTIONARY (((:LITERAL-STRING "sdf") :LITERAL-INTEGER 2))))
  (lift:ensure-same (python.syntax::parse 'python.syntax::literal-dictionary "{'sdf':f(x), \"a\": 33}")
		    '(:LITERAL-DICTIONARY
		      (((:LITERAL-STRING "sdf") :FUNCTION-CALL "f" ((:PYTHON-REFERENCE "x")))
		       ((:LITERAL-STRING "a") :LITERAL-INTEGER 33)))))

(lift:addtest (burgled-batteries.syntax)
  lisp-expression
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(hello)")
		    '(:LISP-EXPRESSION "(hello)"))
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(foobar foo bar)")
		    '(:LISP-EXPRESSION "(foobar foo bar)"))
  (lift:ensure-error (python.syntax::parse 'python.syntax::lisp-expression "$((foobar foo bar)"))
  (lift:ensure-error (python.syntax::parse 'python.syntax::lisp-expression "$(foobar foo bar))"))
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(foo-bar $foo bar)")
		    '(:LISP-EXPRESSION "(foo-bar $foo bar)"))
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(\"asfd asdf\")")
		    '(:LISP-EXPRESSION "(\"asfd asdf\")"))
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(concatenate    'string foo \"bar  foo\")")
		    '(:LISP-EXPRESSION "(concatenate 'string foo \"bar  foo\")"))
  (lift:ensure-same (python.syntax::parse 'python.syntax::lisp-expression "$(234 'asdf 234-asdf #\\A)")
		    '(:LISP-EXPRESSION "(234 'asdf 234-asdf #\\A)")))
