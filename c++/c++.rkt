#lang racket/base

(require syntax/parse
         "debug.rkt"
         (only-in "utils.rkt" connect syntax-map)
         (for-template "c++-literals.rkt"))

(provide assignment-operator
         inside-curlies inside-brackets
         binary-operator
         operator
         input-operator
         dotted-identifier)

(define-literal-set operators #:for-template
                    (+= -= - / << >>))

(define-syntax-class assignment-operator
                     #:literal-sets (operators)
                     [pattern (~or += -=)])

(define-syntax-class input-operator
                     #:literal-sets (operators)
                     [pattern (~or << >>)])

(define-syntax-class inside-curlies
                     [pattern x #:when (eq? #\{ (syntax-property #'x 'paren-shape))])

(define-syntax-class inside-brackets
                     [pattern x #:when (eq? #\[ (syntax-property #'x 'paren-shape))])

(define-syntax-class operator
                     #:literal-sets (operators)
                     [pattern (~or - /)])

(define-syntax-class binary-operator
                     #:literal-sets (operators)
                     [pattern (~or - /)])

(define-syntax-class dotted-identifier
                     [pattern x:id #:when (regexp-match #rx"^\\...*" (symbol->string (syntax-e #'x)))])

#;
(define-syntax-class assignment-operator
                     #:literals (+=)
                     [pattern +=])

(define raw-identifier syntax-e)

(provide function-argument)
(define-syntax-class function-argument
  #:literals (const reference pointer)
  [pattern ((~optional (~and const has-const)) type:identifier
                                               (~optional (~and reference has-reference))
                                               (~optional (~and pointer has-pointer))
                                               variable:identifier)
           #:with final (string-append 
                          (if (attribute has-const) "const " "")
                          (symbol->string (raw-identifier #'type))
                          " "
                          (if (attribute has-reference) "& " "")
                          (if (attribute has-pointer) "* " "")
                          (symbol->string (raw-identifier #'variable)))])

(provide type)
(define-splicing-syntax-class type
  #:literals (pointer const signed unsigned)
  [pattern (~seq (~optional (~and const has-const)) type:identifier pointer)
           #:with final (format "~a~a*"
                                (if (attribute has-const) "const " "")
                                (raw-identifier #'type))]
  [pattern (~seq (~optional (~and unsigned has-unsigned))
                 type:identifier)
           #:with final (string-append
                          (if (attribute has-unsigned) "unsigned " "")
                          (symbol->string (raw-identifier #'type)))])

(define-syntax-class single-expression
  [pattern (x:expression) #:with final (attribute x.final)])

(define-splicing-syntax-class (debug-here d)
  [pattern (~seq) #:when (begin
                           (debug "Debug parse I got here ~a\n" d)
                           #t)])


(define-splicing-syntax-class infix-expression
  [pattern (~seq (~var xx (debug-here "infix1"))
                 stuff ...)
           #:attr final
           (connect (syntax-map (lambda (x)
                                  (debug "Parse infix '~a'\n" x)
                                  (with-syntax ([check x])
                                    (syntax-parse #'((check))
                                      [(what:operator) (format "~a" (raw-identifier #'what))]
                                      [(any:expression) (attribute any.final)])))
                                stuff ...)
                    " ")])

(define-splicing-syntax-class (debug-parse stuff)
  [pattern x #:when (begin
                      (debug "~a\n" stuff)
                      #t)])

(provide expression)
(define-splicing-syntax-class expression
  #:literals (sizeof cast pointer)
  [pattern (~and (~seq (~var xx (debug-parse "expression 1")))
                 (~seq (sizeof arg:expression)))
           #:attr final (format "sizeof(~a)" (attribute arg.final))]
  [pattern (~seq pointer more:expression)
           #:attr final (format "*~a" (syntax-e #'more.final))]
  [pattern (~seq (cast stuff ...) more:expression)
           #:attr final
           (format "(~a) ~a" (connect (syntax-map
                                     (lambda (x)
                                       (symbol->string (raw-identifier x)))
                                     stuff ...)
                                   " ")
                   (attribute more.final)
                   )]
  [pattern ((name:id (~and is:inside-curlies (index:expression))))
           #:attr final
           (format "~a[~a]"
                   (raw-identifier #'name)
                   (attribute index.final))]
  [pattern (~and (~seq structure:inside-brackets) (~seq infix:infix-expression))
           #:attr final (attribute infix.final)]
  [pattern ((expression:expression flow1:dotted-identifier arg:single-expression ...))
           #:attr final
           (format "~a~a(~a)"
                   (attribute expression.final)
                   (syntax-e #'flow1.x)
                   (connect (attribute arg) ", "))]
  [pattern (name:id) #:attr final (format "~a" (raw-identifier #'name))]
  [pattern (constant:str) #:attr final (format "\"~a\"" (raw-identifier #'constant))]
  [pattern (constant:number) #:attr final (format "~a" (raw-identifier #'constant))]
  [pattern (constant:char) #:attr final
       (if (eq? #\0 (raw-identifier #'constant))
         "'\\0'"
         (format "'~a'" (raw-identifier #'constant)))]
  [pattern (~var final (debug-here "just before function call")) #:when #f]
  [pattern ((name:expression arg:expression ...))
           #:attr final
           (format "~a(~a)" (attribute name.final)
                   (connect (attribute arg.final) ", "))
           #;
           (begin
             (debug "do a function from ~a - ~a\n" expression #'name)
             (format "~a(~a)"
                     (canonical-c++-expression #'(name))
                     (connect (syntax-map canonical-c++-expression (arg) ...) ", ")))]
  [pattern (~var final (debug-here "just after function call")) #:when #f]
  [pattern x:infix-expression #:attr final (attribute x.final)
           ;;else (canonical-c++-infix expression)]
           ])

(provide class-declaration)
(define-syntax-class class-declaration
  #:literals (class)
  [pattern (class name:identifier super-class:identifier body ...)
           #:attr final
           (format "class ~a: public ~a {\n~a\n};\n"
                   (raw-identifier #'name)
                   (raw-identifier #'super-class)
                   ""
                   #;
                   (connect (syntax-map (lambda (what) (canonical-c++-class #'name what))
                                        body ...)))])

