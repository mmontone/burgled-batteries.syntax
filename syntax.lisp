(in-package :python.syntax)

;; Utility functions

(defun ref (name)
  (burgled-batteries::run name))

(defun ref* (name)
  (burgled-batteries::run* name))

(defun call (object method &rest args)
  (burgled-batteries::with-cpython-pointer (ptr (burgled-batteries::object.get-attr-string* object method))
    (burgled-batteries::object.call-object ptr args)))

(defun call* (object method &rest args)
  (burgled-batteries::with-cpython-pointer (ptr (burgled-batteries::object.get-attr-string* object method))
    (burgled-batteries::object.call-object* ptr args)))

(defun send (method object &rest args)
  (apply #'call object (cons method args)))

(defun send* (method object &rest args)
  (apply #'call* object (cons method args)))

(defun is-none (x)
  (cffi:pointer-eq x burgled-batteries::+none+))

(defun is-true (x)
  (cffi:pointer-eq x burgled-batteries::+true+))

(defun is-false (x)
  (cffi:pointer-eq x burgled-batteries::+false+))

(defun py-slot-value (object slot)
  (burgled-batteries::object.get-attr-string object slot))

(defrule eol #\NewLine)

(defrule eof (! (characterp character)))

(defrule blank #\ )

(defrule tab #\Tab)

(defrule spacing (* (or blank tab))
  (:text t))

(defrule spacing* (* (or eol blank tab))
  (:text t))

(defrule letter
    (character-ranges (#\a #\z)
                      (#\A #\Z)
                      (#\0 #\9)))
(defrule word (and
               (+ (not (or blank tab eol eof)))
               (& (or blank tab eol eof)))
  (:function (lambda (match)
               (text (first match)))))

(defrule word-separator (or blank tab eol eof
                            #\, #\; #\. #\:
                            #\( #\)
                            #\[ #\]
                            #\{ #\}))

(defrule lisp-name-separator
    (or blank tab eol eof
        #\( #\) #\. #\[ #\]
        #\= #\^ #\,
        #\{ #\}))

(defrule word* (and
                (+ (not word-separator))
                (& word-separator))
  (:function (lambda (match)
               (text (first match)))))

(defrule literal-string (or (and #\'
                                 (* (or (and #\\ #\')
					(not #\')))
                                 #\')
                            (and #\"
                                 (* (or (and #\\ #\" )
					(not #\")))
                                 #\"))
  (:function (lambda (match)
               (list :literal-string (text (second match))))))

(defrule literal-none "None"
  (:function (lambda (match)
               (list :literal-none))))

(defrule literal-boolean (or "True" "False")
  (:function (lambda (match)
               (list :literal-boolean (if (equalp match "True")
                                          t
                                          nil)))))

(defrule digit (character-ranges (#\0 #\9)))

(defrule integer (and (? (or #\- #\+)) (+ digit))
  (:function (lambda (match)
	       (parse-integer (text (first match) (second match))))))

(defrule literal-long (and integer (or #\l #\L))
  (:function (lambda (match)
	       (list :literal-long (first match)))))

(defrule literal-integer integer
  (:function (lambda (match)
	       (list :literal-integer match))))

(defrule literal-float (or exponent-float point-float)
  (:function (lambda (match)
	       (list :literal-float match))))

(defrule point-float (and (? (or #\- #\+)) (* digit) #\. (+ digit))
  (:function (lambda (match)
	       (read-from-string (apply #'text match)))))

(defrule exponent-float (and (or point-float integer)
			     (or #\e #\E) 
			     integer)
  (:function (lambda (match)
	       (* (first match)
		  (expt 10
			(third match))))))

(defrule literal-list (and #\[ spacing*
                           (? (and expression (* (and spacing* #\, spacing* expression))))
                           spacing* #\])
  (:function (lambda (match)
               (let ((list (when (third match)
                             (cons (first (third match))
                                   (mapcar (lambda (rest)
                                             (nth 3 rest))
                                           (second (third match)))))))
                 (list :literal-list list)))))

(defrule literal-dictionary-key-and-value (and expression spacing* #\: spacing* expression)
  (:function (lambda (match)
               (cons (first match)
                     (nth 4 match)))))

(defrule literal-dictionary (and #\{ spacing*
                                 (? (and literal-dictionary-key-and-value
                                         (* (and spacing* #\, spacing*
                                                 literal-dictionary-key-and-value))))
                                 spacing* #\})
  (:function (lambda (match)
               (list :literal-dictionary
                     (when (third match)
                       (cons (first (third match))
                             (mapcar (lambda (rest)
                                       (nth 3 rest))
                                     (second (third match)))))))))

(defrule python-name-separator (or blank tab eol eof
                                   #\, #\; #\. #\: #\$
                                   #\( #\) #\= #\^
                                   #\[ #\]
                                   #\{ #\}))

(defrule python-name
    (and
     (+ (not python-name-separator))
     (& python-name-separator))
  (:function (lambda (match)
               (text (first match)))))

(defrule lisp-name
    (and
     (+ (not lisp-name-separator))
     (& lisp-name-separator))
  (:function (lambda (match)
               (text (first match)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *transform-character* #\^))

(defrule transform (and #.*transform-character* expression)
  (:function (lambda (match)
               (list :transform (second match)))))

(defrule expression (or literal-none
                        literal-boolean
                        literal-string
			literal-float
			literal-long
                        literal-integer
			literal-list
                        literal-dictionary
			assignment
                        method-call
                        property-access
                        index-access
                        function-call
                        reference
                        lisp-expression
                        transform))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *lisp-reference-prefix* #\$))

(defrule python-reference python-name
  (:function (lambda (match)
               (list :python-reference match))))

(defrule lisp-reference (and #.*lisp-reference-prefix* lisp-name)
  (:function (lambda (match)
               (list :lisp-reference (format nil "$~A" (second match))))))

(defrule reference (or lisp-reference python-reference))

(defrule assignment (and expression spacing* #\= spacing* expression)
  (:function (lambda (match)
               (list :assignment
                     (first match)
                     (nth 4 match)))))

(defrule index-access (and expression
                           #\[
                           expression
                           #\])
  (:function (lambda (match)
               (list :index-access
                     (first match)
                     (third match)))))

(defrule property-access (and
                          expression
                          #\.
                          property-name)
  (:function (lambda (match)
               (list :property-access
                     (first match)
                     (third match)))))

(defrule property-name python-name)

(defrule method-call (and
                      expression
                      #\.
                      method-name
                      args-list)
  (:function (lambda (match)
               (list :method-call
                     (first match)
                     (third match)
                     (nth 3 match)))))

(defrule method-name python-name)

(defrule arg expression)
(defrule kwarg (and python-name #\= expression)
  (:function (lambda (match)
               (list :named-arg
                     (first match)
                     (third match)))))

(defrule args (and (! kwarg) arg (* (and spacing* #\, spacing*  (! kwarg) arg)))
  (:function (lambda (match)
               (list*
                (second match)
                (loop for arg in (third match)
                   collect (nth 4 arg))))))

(defrule kwargs (and kwarg (* (and spacing* #\, spacing* kwarg)))
  (:function (lambda (match)
               (list*
                (first match)
                (loop for arg in (second match)
                   collect (nth 3 arg))))))

(defrule args-and-kwargs (and args (? (and spacing* #\, spacing* kwargs)))
  (:function (lambda (match)
               (append
                (first match)
                (nth 3 (second match))))))

(defrule args-list (and #\( spacing*
                        (? (or args-and-kwargs kwargs))
                        spacing* #\))
  (:function (lambda (match)
               (third match))))

(defrule function-name python-name)

(defrule function-call (and function-name
                            args-list)
  (:function (lambda (match)
               (list* :function-call
                      match))))

;; lisp expression

(defun not-doublequote (char)
  (not (eql #\" char)))

;;; Utility rules.

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))


(defrule string-char (or (not-doublequote character) (and #\\ #\")))

(defrule lisp-string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
		(declare (ignore q1 q2))
		(format nil "\"~A\"" (text string))))

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.
(defrule sexp (and (? whitespace) (or lisp-list lisp-atom))
  (:destructure (w s &bounds start end)
                (declare (ignore w))
                s))

(defrule lisp-list (and #\( sexp (* sexp) (? whitespace) #\))
  (:destructure (p1 car cdr w p2)
                (declare (ignore p1 p2 w))
                (princ-to-string (cons car cdr))))

(defrule lisp-atom (or lisp-string lisp-atom-thing))

(defrule lisp-atom-thing (+ (not (or #\space #\tab #\newline #\( #\) )))
  (:lambda (list)
    (text list)))

(defrule lisp-expression (and #\$ (& #\( )
			      sexp)
  (:function (lambda (match)
	       (list :lisp-expression
		     (third match)))))

(defun compile-expression (expression)
  (compile-expression% (first expression) expression))

(defgeneric compile-expression% (expression-type expression))

(defmethod compile-expression% ((type (eql :literal-boolean)) expression)
  (if (second expression)
      'burgled-batteries::+true+
      'burgled-batteries::+false+))

(defmethod compile-expression% ((type (eql :literal-none)) expression)
  'burgled-batteries::+none+)

(defmethod compile-expression% ((type (eql :literal-string)) expression)
  `(burgled-batteries::string.from-string* ,(second expression)))

(defmethod compile-expression% ((type (eql :literal-integer)) expression)
  `(burgled-batteries::number.int* ,(second expression)))

(defmethod compile-expression% ((type (eql :literal-long)) expression)
  `(burgled-batteries::number.long* ,(second expression)))

(defmethod compile-expression% ((type (eql :literal-float)) expression)
  `(burgled-batteries::number.float* ,(coerce (second expression) 'double-float)))

(defmethod compile-expression% ((type (eql :literal-list)) expression)
  (alexandria:with-unique-names (list)
    `(let ((,list (burgled-batteries::list.new* ,(length (second expression)))))
       ,@(loop for item in (second expression)
            collect `(burgled-batteries::list.append ,list ,(compile-expression item)))
       ,list)))

(defmethod compile-expression% ((type (eql :literal-dictionary)) expression)
  (alexandria:with-unique-names (dict)
    `(let ((,dict (burgled-batteries::dict.new*)))
       ,@(loop for (key . value) in (second expression)
            collect `(burgled-batteries::dict.set-item ,dict
                                            ,(compile-expression key)
                                            ,(compile-expression value)))
       ,dict)))

(defmethod compile-expression% ((type (eql :python-reference)) expression)
  `(ref* ,(second expression)))

(defmethod compile-expression% ((type (eql :lisp-reference)) expression)
  (read-from-string (second expression)))

(defun split-args (args)
  (let ((unnamed-args nil)
        (named-args nil))
    (loop for arg in args
       do
         (if (equalp (first arg) :named-arg)
             (push arg named-args)
             (push arg unnamed-args)))
    (values (nreverse unnamed-args) (nreverse named-args))))

(defmethod compile-expression% ((type (eql :function-call)) expression)
  (destructuring-bind (function-name args) (cdr expression)
    (multiple-value-bind (unnamed-args named-args) (split-args args)
      (if named-args
          (alexandria:with-unique-names (kwargs pyfun)
            `(let ((,kwargs (make-hash-table)))
               ,@(loop for arg in named-args
                    collect
                      `(setf (gethash ,(second arg) ,kwargs)
                             ,(compile-expression (third arg))))
               (burgled-batteries::with-cpython-pointer 
		   (,pyfun (burgled-batteries::%get-function ,function-name))
                 (burgled-batteries::object.call* 
		  ,pyfun 
		  (list ,@(mapcar #'compile-expression unnamed-args))
		  ,kwargs))))
          `(burgled-batteries::pyapply* 
	    ,function-name
	    ,@(mapcar #'compile-expression unnamed-args))))))

(defmethod compile-expression% ((type (eql :method-call)) expression)
  (destructuring-bind (object method-name args) (cdr expression)
    (multiple-value-bind (unnamed-args named-args) (split-args args)
      (if named-args
          (alexandria:with-unique-names (kwargs pyfun)
            `(let ((,kwargs (make-hash-table)))
               ,@(loop for named-arg in named-args
                    collect
                      `(setf (gethash ,(second named-arg) ,kwargs)
                             ,(compile-expression (third named-arg))))
               (burgled-batteries::with-cpython-pointer 
		   (,pyfun
		    (burgled-batteries::object.get-attr-string*
		     ,(compile-expression object)
		     ,method-name))
                 (burgled-batteries::object.call* 
		  ,pyfun 
		  (list ,@(mapcar #'compile-expression unnamed-args))
		  ,kwargs))))
          `(call*
            ,(compile-expression object)
            ,method-name
            ,@(mapcar #'compile-expression args))))))

(defmethod compile-expression% ((type (eql :property-access)) expression)
  (destructuring-bind (object property) (cdr expression)
    `(burgled-batteries::object.get-attr*
      ,(compile-expression object)
      ,property)))

(defmethod compile-expression% ((type (eql :index-access)) expression)
  (destructuring-bind (object index) (cdr expression)
    `(burgled-batteries::object.get-item* ,(compile-expression object)
                               ,(compile-expression index))))

(defmethod compile-expression% ((type (eql :assignment)) expression)
  (destructuring-bind (lexpr rexpr) (cdr expression)
    (ecase (first lexpr)
      (:index-access
       (destructuring-bind (object index) (cdr lexpr)
         `(burgled-batteries::object.set-item ,(compile-expression object)
                                   ,(compile-expression index)
                                   ,(compile-expression rexpr))))
      (:property-access
       (destructuring-bind (object property) (cdr lexpr)
         `(burgled-batteries::object.set-attr ,(compile-expression object)
                                   ,property
                                   ,(compile-expression rexpr))))
      (:lisp-reference
       `(setf ,(compile-expression lexpr)
              ,(compile-expression rexpr)))
      (:python-reference
       (error "Not implemented")))))

(defmethod compile-expression% ((type (eql :transform)) expression)
  (alexandria:with-unique-names (transformed)
    `(let ((,transformed
            (cffi:convert-from-foreign ,(compile-expression (second expression))
                                       'cpython::object!)))
       ,transformed)))

(defmethod compile-expression% ((type (eql :lisp-expression)) expression)
  (read-from-string (second expression)))

(defun parse-python (string)
  (esrap:parse 'expression string))

(defun compile-python (string)
  (compile-expression
   (parse-python string)))

(defun run-python (input)
  (let ((code (if (stringp input)
                  (compile-python input)
                  input)))
    (eval code)))

(named-readtables:defreadtable :python
  (:merge :standard)
  (:macro-char
   #\[
   #'(lambda (s c)
       (declare (ignore c))
       (compile-python
        (with-output-to-string (str)
          (loop with brackets = 0
             for c = (read-char s nil 'eof)
             until (or (eq c 'eof)
                       (and (char= c #\])
                            (zerop brackets)))
             do (progn
                  (when (equalp c #\[)
                    (incf brackets))
                  (when (equalp c #\])
                    (decf brackets))
                  (write-char c str))))))))

(defun enable-python-syntax ()
  (named-readtables:in-readtable :python))
