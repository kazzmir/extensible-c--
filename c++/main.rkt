#lang racket/base

(require (for-syntax (except-in racket/base syntax)
                     racket/pretty syntax/stx
                     racket/list racket/match
                     syntax/parse
                     "utils.rkt"
                     "transformer.rkt"
                     (prefix-in c++- "c++.rkt")
                     )
         racket/list
         ;; (prefix-in c++- "c++.rkt")
         (prefix-in c++- "c++-literals.rkt")
         racket/match)

(begin-for-syntax
  #;
  (define (expand-c++ code)
    (define (recur code)
      (expand-c++ code))
    #;
    (printf "Expanding c++ ~a\n" code)
    (define expanded (local-expand code 'expression #f))
    (syntax-case expanded ()
      [(sub ...) #'(sub::recur ...)

       #;
       (with-syntax ([(sub* ...) (map recur (syntax->list #'(sub ...)))])
                   #'(sub* ...))]
      [_ code]))
  )

(provide c++)
#;
(define c++ #f)

(define-for-syntax (expand-c++ form)
  (syntax-parse form
    [(something:id rest ...)
     (define transformer (syntax-local-value #'something (lambda () #f)))
     (if (c++-transformer? transformer)
       (let ()
         (define macro (c++-transformer-transformer transformer))
         (printf "Expanding ~a with ~a\n" #'something transformer)
         (expand-c++ (macro (datum->syntax form #'(something rest ...) form form))))
       (syntax/location+properties
         form form
         (something rest::expand-c++ ...)))]
    [(x ...) (syntax/location+properties
               form form (x::expand-c++ ...))]
    [x form]
    ))

(define indent-space "    ")
(define (indent what)
  #;
  (string-append indent-space
                 (regexp-replace* #px"Q" 
                                 (regexp-replace* #px"\n" what
                                                  (format "~aQ" indent-space))
                                 "Z"))
  (string-append indent-space
                 (regexp-replace* #px"\t" 
                                 (regexp-replace* #px"\n" what
                                                  (format "\t~a" indent-space))
                                 "\n")))

#|
(define (canonical-c++-infix code)
  #f
  )

(define (canonical-c++-expression expression)
  (match expression
    [(list (list name args ...))
     (format "~a(~a)" name (connect (map (lambda (x)
                                           (format "~a" x))
                                         args) ", "))]
    [(list (and (? symbol?) name)) (format "~a" name)]
    [else (canonical-c++-infix expression)]))

(define (canonical-c++-body what)
  (match what
    [(list 'variable type name '= expression ...)
     (format "~a ~a = ~a;" type name (canonical-c++-expression expression))]
    [(list 'for (list initializer condition increment) body ...)
     (format "for (~a; ~a ~a){\n~a\n}"
             (canonical-c++-expression initializer)
             (canonical-c++-expression condition)
             (canonical-c++-expression increment)
             (indent (connect (map canonical-c++-body body))))]
    [(list 'class name super-class body ...)
     (format "class ~a: public ~a {\n~a\n}\n" name super-class (connect (map (lambda (what)
                                                                               (canonical-c++-class name what))
                                                                             body)))]
    [(list name '= expression ...)
     (format "~a = ~a;" name (canonical-c++-expression expression))]
    [(list name { list index-expression ... } '= expression ...)
     (format "~a[~a] = ~a;" name index-expression (canonical-c++-expression expression))]
    [else (format "~a;" (canonical-c++-expression (list what)))]))

(define (canonical-c++-class-body name form)
  (match form
    [(list 'constructor (list args ...) (list members ...)
           body ...)
     (format "~a(){\n~a\n}" name (indent (connect (map canonical-c++-body body))))]
    [else (canonical-c++-top form)]))

(define (canonical-c++-class name body)
  (match body
    [(list 'public stuff ...)
     (format "public:\n~a" (indent (connect (map (lambda (what)
                                                   (canonical-c++-class-body name what))
                                                 stuff))))]))
|#

#;
(define (canonical-c++-top form)
  (match form
    [(list 'function type (list name args ...) body ...)
     (format "~a ~a(){\n~a\n}" type name (indent (connect (map canonical-c++-body body))))]
    [(list 'class name super-class body ...)
     (format "class ~a: public ~a {\n~a\n}\n" name super-class (connect (map (lambda (what)
                                                                               (canonical-c++-class name what))
                                                                               body)))]
    [else (error 'c++-top "what is ~a" form)]))

(begin-for-syntax
  (define indent-space "    ")
  (define-recursive
  (define (raw-identifier name) (syntax-e name))
  (define (canonical-c++-infix expression)
    (syntax-parse expression
      [(stuff ...)
       (define output (syntax-map (lambda (x)
                                    (syntax-parse x
                                      [what:c++-operator (format "~a" (raw-identifier #'what))]
                                      [any (canonical-c++-expression #'(any))]))
                                  stuff ...))
       (connect output " ")]))

  #;
  (define (canonical-c++-expression expression)
    (struct binary-operator (precedence association) #:transparent)
    (define (parse input table precedence left current)
      (syntax-parse input
        [(constant:number rest ...) (parse #'(rest ...) table precedence left #'constant)]
        [(operator:c++-binary-operator rest ...)
         (match (hash-ref table #'operator #f)
           [(binary-operator new-precedence association)
            (define check (case association
                            [(right) >=]
                            [(left) >]))
            (if (check new-precedence precedence)
              (let ()
                (define-values (parsed unparsed)
                               (parse #'(rest ...) table new-precedence
                                      (lambda (what)
                                        (if (not current)
                                          (with-syntax ([what what])
                                            #'(operator what))
                                          (with-syntax ([current current]
                                                        [what what])
                                            #'(current operator what)))
                                          #f)))
                (parse unparsed table precedence left parsed))
              (values (left current) input))]
           [else (raise-syntax-error 'parse "unknown")])]
        [(something:identifier rest ...) (parse #'(rest ...) table precedence left #'something)]
        [() (values (left current) '())]
        [else (raise-syntax-error 'parse "what is ~a" input)]))
    (define table (hash #'c++-/ (binary-operator 100 'left)))
    (define-values (parsed unparsed)
                   (parse expression table 0 (lambda (x) x) #f))
    parsed)

  (define (canonical-c++-expression expression)
    (syntax-parse expression #:literals (c++-sizeof)
      [((c++-sizeof arg))
       (format "sizeof(~a)" (canonical-c++-expression #'(arg)))]
      [((name:id index:c++-inside-curlies))
       (format "~a[~a]"
               (raw-identifier #'name)
               (canonical-c++-expression #'index))]
      [(infix:c++-inside-brackets) (canonical-c++-infix #'infix)]
      [((expression flow1:c++-dotted-identifier arg ...))
       (format "~a~a(~a)"
               (canonical-c++-expression #'(expression))
               (canonical-c++-expression #'(flow1))
               (connect (syntax-map canonical-c++-expression (arg) ...) ", "))]
      [((name arg ...))
       (format "~a(~a)"
               (canonical-c++-expression #'(name))
               (connect (syntax-map canonical-c++-expression (arg) ...) ", "))]
      [(name:id) (format "~a" (raw-identifier #'name))]
      [(constant:str) (format "\"~a\"" (raw-identifier #'constant))]
      [(constant:number) (format "~a" (raw-identifier #'constant))]
      [else (canonical-c++-infix expression)]))
  (define (canonical-c++-class-body name form)
    (syntax-parse form #:literals (c++-constructor)
      [(c++-constructor (args ...) (members ...)
                        body ...)
       (format "~a(){\n~a\n}" (raw-identifier name)
               (indent (connect (syntax-map (lambda (x)
                                              (canonical-c++-body x #f))
                                            body ...))))]
    [else (canonical-c++-top form)]))

  (define (canonical-c++-class name body)
    (syntax-parse body #:literals (c++-public)
      [(c++-public stuff ...)
       (format "public:\n~a" (indent (connect (syntax-map (lambda (what)
                                                            (canonical-c++-class-body name what))
                                                 stuff ...)
                                              "\n\n")))]))

  (define (canonical-c++-declaration declaration)
    (syntax-parse declaration #:literals (c++-variable c++-=)
      [(c++-variable type:identifier name:identifier)
       (format "~a ~a" (raw-identifier #'type) (raw-identifier #'name))]
      [(c++-variable type:identifier name:identifier c++-= expression ...)
       (format "~a ~a = ~a" (raw-identifier #'type) (raw-identifier #'name)
               (canonical-c++-expression #'(expression ...)))]
      ))

  (define (canonical-c++-body body last?)
    (syntax-parse body #:literals (c++-variable c++-= c++-class)
      [(c++-variable type:identifier name:identifier c++-= expression ...)
       (format "~a ~a = ~a;" (raw-identifier #'type) (raw-identifier #'name)
               (canonical-c++-expression #'(expression ...)))]
      [(variable:identifier assignment:c++-assignment-operator expression ...)
       (format "~a ~a ~a;" (raw-identifier #'variable)
               (raw-identifier #'assignment)
               (canonical-c++-expression #'(expression ...)))]
      [(array index:c++-inside-curlies c++-= expression ...)
       (format "~a[~a] = ~a;"
               (canonical-c++-expression #'(array))
               (canonical-c++-expression #'index)
               (canonical-c++-expression #'(expression ...)))]
      [(c++-for (initializer condition increment) body ...)
       (format "for (~a; ~a; ~a){\n~a\n}"
               (canonical-c++-declaration #'initializer)
               (canonical-c++-expression #'condition)
               (canonical-c++-expression #'increment)
               (indent (connect (syntax-map (lambda (x)
                                              (canonical-c++-body x #f))
                                            body ...))))]
      [(c++-class name:id super-class:id body ...)
       (format "class ~a: public ~a {\n~a\n};\n"
               (raw-identifier #'name)
               (raw-identifier #'super-class)
               (connect (syntax-map (lambda (what) (canonical-c++-class #'name what))
                                    body ...)))]
      [else (begin
              (define code (canonical-c++-expression
                             (with-syntax ([body body])
                               #'(body))))
              (if last?
                (format "return ~a;" code)
                (format "~a;" code)))]
      ))

  (define (canonical-c++-top form)
    (define (last? stuff what)
      (eq? (last stuff) what))
    #;
    (printf "top ~a\n" (syntax->datum form))
    (syntax-parse form #:literals (c++-include c++-function c++-class)
      [(c++-include file:str ...)
       (connect (syntax-map (lambda (x)
                              (format "#include ~a" (syntax-e x)))
                            file ...))]
      [(c++-function type:id (name:id arg ...) body ...)
       (format "~a ~a(){\n~a\n}" (raw-identifier #'type) (raw-identifier #'name)
               (indent (connect (syntax-map (lambda (x)
                                              (canonical-c++-body x (last? (syntax->list #'(body ...)) x)))
                                            body
                                            ...))))]
      [else "?"]
      ))

  (define (indent what)
    (string-append indent-space
                   (regexp-replace* "\t"
                                    (regexp-replace* #px"\n" what
                                                     (format "\t~a" indent-space))
                                    "\n")))

  
  (define (connect lines [separator "\n"])
    (apply string-append (add-between lines separator)))
  ))

(define (connect lines [separator "\n"])
  (apply string-append (add-between lines separator)))

#;
(define (canonical-c++ forms)
  (connect (map canonical-c++-top forms)))

(define-syntax (canonical-c++ stx)
  #;
  (printf "canonical ~a\n" (syntax->datum stx))
  (syntax-parse stx
    [(_ form ...)
     (datum->syntax stx (connect (syntax-map canonical-c++-top form ...)) stx)]))

(define-syntax (c++ stx)
  (syntax-parse stx
    [(_ form ...)
     #'(printf "~a\n" (canonical-c++ form::expand-c++ ...))]))

#;
(define-syntax (c++ stx)
  (syntax-case stx ()
    [(_ form ...) #'(compile-c++ form::expand-c++ ...)
     #;
     (with-syntax ([(form* ...) (map expand-c++ (syntax->list #'(forms ...)))])
       #'(compile-c++ form* ...))]))

(begin-for-syntax

  #;
(define compile-top-level #f)

#;
(define indent-space "    ")
#;
(define (indent what)
  #;
  (string-append indent-space
                 (regexp-replace* #px"Q" 
                                 (regexp-replace* #px"\n" what
                                                  (format "~aQ" indent-space))
                                 "Z"))
  (string-append indent-space
                 (regexp-replace* #px"\t" 
                                 (regexp-replace* #px"\n" what
                                                  (format "\t~a" indent-space))
                                 "\n")))

;; connect lines together with newlines between them
#;
(define (connect lines [separator "\n"])
  (apply string-append (add-between lines separator)))

(define (compile-expression what)
  (match what
    [(and (? symbol?) x) (format "~a" x)]
    [else (format "unknown ~a" what)]))

#;
(define (compile-class-statement form)
  (match form
    [(list 'constructor x ...) "constructor"]
    [(list 'function x ...) (compile-top-level form)]))

#;
(define (compile-class-body body)
  (match body
    [(list 'public forms ...)
     (format "public:\n~a\n"
             (indent (connect (map compile-class-statement forms))))]))

(define (operator? name)
  (memq name '(+=)))

#|
(with-syntax ([(foo* ...) (map bar (syntax->list #'(foo ...)))])
  #'(foo* ...))

#'(foo::bar ...)
|#

(define-recursive
  (define (compile-class line)
    (syntax-parse line
      #:literals (c++-class)
      [(c++-class name super-class body ...)
       #'(class name { body::compile-class-top ... })]
      ))

  (define (compile-class-top line)
    (syntax-parse line
      #:literals (c++-public)
      [(c++-public body ...)
       #'(public: body::compile-class-statement ...)]))

  (define (compile-class-statement line)
    (syntax-parse line
      #:literals (c++-constructor c++-function)
      [(c++-constructor body ...)
       #''constructor]
      [(c++-function blah ...)
       (compile-top-level line)]
      ))

  (define (compile-statement line)
    (syntax-parse line
      #:literals (c++-class)
      [(c++-class blah ...)
       (compile-class line)]
      [else line]
      #;
      [else (raise-syntax-error 'compile-statement "failed" line)]))

  (define (compile-top-level code)
    #;
    (printf "Compiling top level ~a\n" code)
    (syntax-parse code
      #:literals (c++-function)
      [(c++-function type:id (name:id parameters ...) body ...)

       #'(c++-function type name (){ body::compile-statement ... })

       #;
       (with-syntax ([(body ...) (compile-body (syntax->list #'(body ...)))])
         #'(type name(){ body ... }))]

      [else (raise-syntax-error 'top-level "fail")]
      ))
  )

(define (compile-body code)
  (for/list ([statement code])
    (compile-statement statement))
  #;
  (connect (for/list ([statement code])
             (compile-statement statement))))


#;
  (match code
    [(list 'function type (list name parameters ...) body ...)
     (format "~a ~a(){\n~a\n}\n" type name (indent (compile-body body)))])

  #;
(set! compile-top-level compile-top-level*)

)

#|
(define (convert-c++ code)
  (syntax-parse code
    #:literals (c++-function)
  (match code
    [(list '
  code)
     |#

(define-syntax (compile-c++ code)
  (syntax-parse code
    [(_ stuff ...)
     #'(printf "~a\n" (connect (map convert-c++ '(stuff::compile-top-level ...))))]))

(define-syntax (stare stx)
  (syntax-case stx (c++)
    [(_ (c++ forms ...)) (error 'stare "ok")]
    [(_ form rest ...)
     #'(begin form (stare rest ...))]))

(define-syntax (c++-module-begin module-body)
  #;
  (printf "module begin! ~a\n" module-body)
  (syntax-case module-body (c++)
    [(_ stuff ... (c++ c++-forms ...))

     #;
     #'(#%module-begin (stare stuff ...))

     (let ()
       (define expanded (local-expand #'(#%plain-module-begin stuff ... (c++ c++-forms ...)) 'module-begin '()))
       expanded)

     #;
     (let ()
       #;
       (printf "stuff is ~a\n" #'(stuff ...))
       (define expanded (local-expand #'(#%plain-module-begin stuff ... (c++ c++-forms ...)) 'module-begin '()))
       (pretty-print (syntax->datum expanded))

       #;
       (syntax-parse expanded
         [(_ ... (c++ forms ...))
          ...])

       expanded)
     #;
     (let ()
       (define context (syntax-local-make-definition-context))
       (for ([something (syntax->list #'(stuff ...))])
         (define out (local-expand something 'top-level '()))
         (syntax-case out (define-syntaxes)
           [(define-syntaxes (name) expression)
            (let ()
              (printf "Add ~a ~a\n" #'name #'expression)
              (syntax-local-bind-syntaxes (list #'name) #'expression context))]
           [_ (void)]))
       (internal-definition-context-seal context)
       (define expanded (expand-c++ #'(c++-forms ...) context))
       #;
       (define expanded (local-expand #'(c++-forms ...) 'module-begin #f))
       (pretty-print (syntax->datum expanded)))])
  #;
  #'(#%plain-module-begin 1))

(define-syntax (c++-top stx)
  (syntax-case stx ()
    [(_ . x)
     (if (syntax-local-value #'x (lambda () #f))
       (begin
         (printf "top for ~a\n" #'x)
         #'(#%top . x))
       #''x)]))

(define-syntax (c++-app stx)
  (syntax-case stx ()
    [(_) #'(list)]
    [(_ x ...) #'(x ...)]
    ))

(define-syntax (c++-define-syntax stx)
  (syntax-parse stx
    [(_ name:id function)
     #'(define-syntax name (c++-transformer function))]
    [(_ (name:id args ...) body ...)
     #'(define-syntax name (c++-transformer (lambda (args ...) body ...)))]))

;; copy and pasted from racket/private/misc.rkt
(define-syntax c++-define-syntax-rule
  (lambda (stx)
    (let-values ([(err) (lambda (what . xs)
                          (apply raise-syntax-error
                                 'define-syntax-rule what stx xs))])
      (syntax-case stx ()
        [(dr (name . pattern) template)
         (identifier? #'name)
         (syntax/loc stx
                     (c++-define-syntax name
                       (lambda (user-stx)
                         (syntax-case* user-stx () free-identifier=?
                                        [(_ . pattern) (syntax/loc user-stx template)]
                                        [_ (let*-values
                                             ([(sexpr) (syntax->datum user-stx)]
                                              [(msg)
                                               (if (pair? sexpr)
                                                 (format "use does not match pattern: ~.s"
                                                         (cons (car sexpr) 'pattern))
                                                 (if (symbol? sexpr)
                                                   (format "use does not match pattern: ~.s"
                                                           (cons sexpr 'pattern))
                                                   (error 'internal-error
                                                          "something bad happened")))])
                                             (raise-syntax-error #f msg user-stx))]))))]
        [(_ (name . ptrn) tmpl)         (err "expected an identifier" #'name)]
        [(_ (name . ptrn))              (err "missing template")]
        [(_ (name . ptrn) tmpl etc . _) (err "too many forms" #'etc)]
        [(_ head . _)                   (err "invalid pattern" #'head)]))))

(provide (rename-out [c++-module-begin #%module-begin]
                     [c++-function function]
                     [c++-class class]
                     [c++-variable variable]
                     [c++-= =]
                     [c++--= -=] [c++-+= +=]
                     [c++-- -] [c++-/ /]
                     [c++-public public]
                     [c++-constructor constructor]
                     [c++-sizeof sizeof]
                     [c++-include include]
                     #;
                     [c++-top #%top]
                     [c++-define-syntax define-syntax]
                     [c++-define-syntax-rule define-syntax-rule]
                     [c++-app #%app])
         ;;define-syntax
         ;; define-syntax-rule
         #%datum
         require for-syntax
         c++
         #%top
         (for-meta 1 #%app #%top #%datum
                   syntax printf stx-car stx-cdr syntax->list))
