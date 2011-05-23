#lang racket/base

(require (for-syntax (except-in racket/base syntax)
                     racket/pretty syntax/stx
                     racket/list racket/match
                     syntax/parse
                     "utils.rkt"
                     "transformer.rkt"
                     "debug.rkt"
                     (prefix-in c++- "c++.rkt")
                     )
         racket/list
         ;; (prefix-in c++- "c++.rkt")
         (prefix-in c++- "c++-literals.rkt")
         racket/match)

(provide c++)

;; the expander. doesn't deal with hygiene, it just calls a function whose
;; type is 'syntax -> syntax'
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
    (syntax-parse expression #:literals (c++-sizeof c++-cast)
      [((c++-sizeof arg))
       (format "sizeof(~a)" (canonical-c++-expression #'(arg)))]
      [((c++-cast stuff ...))
        (format "(~a)" (connect (syntax-map
                                  (lambda (x)
                                    (symbol->string (raw-identifier x)))
                                  stuff ...)
                                " "))]
      [((name:id index:c++-inside-curlies))
       (format "~a[~a]"
               (raw-identifier #'name)
               (canonical-c++-expression #'index))]
      [(infix:c++-inside-brackets) (canonical-c++-infix #'infix)]
      [((expression:c++-expression flow1:c++-dotted-identifier arg:c++-expression ...))
       (format "~a~a(~a)"
               (attribute expression.final)
               (canonical-c++-expression #'(flow1))
               (connect (attribute arg.final) ", "))]
      [(name:id) (format "~a" (raw-identifier #'name))]
      [(constant:str) (format "\"~a\"" (raw-identifier #'constant))]
      [(constant:number) (format "~a" (raw-identifier #'constant))]
      [(constant:char)
       (if (eq? #\0 (raw-identifier #'constant))
         "'\\0'"
         (format "'~a'" (raw-identifier #'constant)))]
      [((name arg ...))
       (debug "do a function from ~a - ~a\n" expression #'name)
       (format "~a(~a)"
               (canonical-c++-expression #'(name))
               (connect (syntax-map canonical-c++-expression (arg) ...) ", "))]
      [else (canonical-c++-infix expression)]))
  (define (canonical-c++-class-body name form)
    (define-syntax-class declaration
                         #:literals (c++-local c++-=)
      [pattern (type:identifier name:identifier)
               #:with final (format "~a ~a"
                                    (raw-identifier #'type)
                                    (raw-identifier #'name))
               #:with init #f]
      [pattern (type:identifier name:identifier c++-= expression ...)
               #:with final (format "~a ~a"
                                    (raw-identifier #'type)
                                    (raw-identifier #'name))
               #:with init (format "~a(~a)"
                                   (raw-identifier #'name)
                                   (canonical-c++-expression #'(expression ...)))]
      )
    (syntax-parse form #:literals (c++-constructor)
      [(c++-constructor (args ...) (member:declaration ...)
                        body ...)
       (define (initializer inits)
         (let ([use (filter (lambda (init) (syntax-e init)) inits)])
           (if (null? use) ""
             (string-append ":\n"
                            (connect (map (lambda (what) (syntax-e what)) use))))))
       (format "~a()~a{\n~a\n}\n~a"
               (raw-identifier name)
               (initializer (syntax->list #'(member.init ...)))
               (indent (canonical-c++-block #'(body ...) #f))
               (connect (syntax-map (lambda (x)
                                      (format "~a;" (syntax-e x)))
                                    member.final ...)))]
    [else (canonical-c++-top form)]))

  (define (canonical-c++-class name body)
    (syntax-parse body #:literals (c++-public)
      [(c++-public stuff ...)
       (format "public:\n~a" (indent (connect (syntax-map (lambda (what)
                                                            (canonical-c++-class-body name what))
                                                 stuff ...)
                                              "\n\n")))]
      [(stuff ...)
       (indent (connect (syntax-map (lambda (what)
                                      (canonical-c++-class-body name what))
                                    stuff ...)))]
      ))

  (define (canonical-c++-declaration declaration)
    (syntax-parse declaration #:literals (c++-variable c++-=)
      [(c++-variable type:identifier name:identifier)
       (format "~a ~a" (raw-identifier #'type) (raw-identifier #'name))]
      [(c++-variable type:identifier name:identifier c++-= expression ...)
       (format "~a ~a = ~a" (raw-identifier #'type) (raw-identifier #'name)
               (canonical-c++-expression #'(expression ...)))]
      
      ))

  (define (canonical-c++-block body last?)
    ;; (printf "do block ~a\n" body)
    (define (is-last? list element)
      (eq? (last list) element))
    (connect (map (lambda (x)
                    ;; (printf "body ~a\n" x)
                    (canonical-c++-body x
                                        (and last? (is-last? (syntax->list body) x))))
                  (syntax->list body))))

  (define (canonical-c++-body body last?)
    (debug "do body ~a\n" body)
    (syntax-parse body #:literals (c++-variable c++-= c++-class c++-try
                                   c++-for c++-else c++-return
                                   c++-catch c++-if c++-while)
      ;; int x = 2
      [(c++-variable type:c++-type name:identifier c++-= expression:c++-expression)
       (debug "here\n")
       (format "~a ~a = ~a;"
               (raw-identifier #'type.final) (raw-identifier #'name)
               (attribute expression.final)
               #;
               (canonical-c++-expression #'(expression ...)))]
      ;; Foo f(5)
      [(c++-variable type:c++-type (name:identifier expression:c++-expression))
       (debug "here\n")
       (format "~a ~a(~a);" (raw-identifier #'type.final)
               (raw-identifier #'name)
               (attribute expression.final))]
      ;; int x
      [(c++-variable type:c++-type name:identifier (~optional array:c++-inside-brackets))
       (debug "here\n")
       (format "~a ~a~a;"
               (raw-identifier #'type.final)
               (raw-identifier #'name)
               (if (attribute array)
                 (format "[~a]" (canonical-c++-expression #'array))
                 "")
               )]
      [(c++-variable ~! rest ...) (raise-syntax-error 'variable "invalid declaration of a variable" body)]
      [(variable:identifier assignment:c++-assignment-operator expression ...)
       (debug "here\n")
       (format "~a ~a ~a;" (raw-identifier #'variable)
               (raw-identifier #'assignment)
               (canonical-c++-expression #'(expression ...)))]
      [(c++-while (condition:c++-expression) body ...)
       (debug "here\n")
       (format "while (~a){\n~a\n}"
               (attribute condition.final)
               (indent (canonical-c++-block #'(body ...) last?)))]
      [(c++-do body:c++-inside-curlies c++-while (condition ...))
       (format "do{\n~a\n} while (~a);"
               (indent (canonical-c++-block #'body #f))
               (canonical-c++-expression #'(condition ...)))]
      [(c++-if (condition ...) body:c++-inside-curlies (~optional (~seq c++-else else-part)))
       (debug "here\n")
       (format "if (~a){\n~a\n}~a"
               (canonical-c++-expression #'(condition ...))
               (indent (canonical-c++-block #'body last?))
               (if (attribute else-part)
                 (format " else ~a"
                         (syntax-parse #'else-part #:literals (c++-if)
                           [(c++-if rest ...) (canonical-c++-body #'else-part last?)]
                           [body:c++-inside-curlies (canonical-c++-block #'body last?)]
                           [else (canonical-c++-body #'else-part last?)]))
                 ""))]
      [(c++-if (condition ...) body ... (~optional (~seq c++-else else-part)))
       (debug "here\n")
       (format "if (~a){\n~a\n}~a"
               (canonical-c++-expression #'(condition ...))
               (indent (canonical-c++-block #'(body ...) last?))
               (if (attribute else-part)
                 (syntax-parse #'else-part #:literals (c++-if)
                   [(c++-if rest ...) (canonical-c++-body #'else last?)]
                   [body:c++-inside-curlies (canonical-c++-block #'body last?)]
                   [else (canonical-c++-body #'else-part last?)])
                 ""))]
      [(c++-try try-body (c++-catch exception:c++-function-argument catch-body ...))
       (debug "here\n")
       (format "try{\n~a\n} catch (~a){\n~a\n}"
               (indent (canonical-c++-block #'try-body last?))
               (syntax-e #'exception.final)
               (indent (canonical-c++-block #'(catch-body ...) last?)))]
      #;
      [(c++-variable blah ...)
       (raise-syntax-error 'variable "declaring a variable must have the form 'variable <type> <name> = <expression ...>'" body)]
      
      [(array index:c++-inside-brackets c++-= expression ...)
       (debug "here\n")
       (format "~a[~a] = ~a;"
               (canonical-c++-expression #'(array))
               (canonical-c++-expression #'index)
               (canonical-c++-expression #'(expression ...)))]
      [(c++-for (initializer condition increment) body ...)
       (debug "here\n")
       (format "for (~a; ~a; ~a){\n~a\n}"
               (canonical-c++-declaration #'initializer)
               (canonical-c++-expression #'condition)
               (canonical-c++-expression #'increment)
               (indent (canonical-c++-block #'(body ...) last?)))]
      [(c++-for rest ...)
       (raise-syntax-error 'for "invalid for syntax" body)]
      [(taker input:c++-input-operator expression ...)
       (debug "here\n")
       (format "~a ~a ~a;" (canonical-c++-expression #'(taker))
               (raw-identifier #'input)
               (canonical-c++-expression #'(expression ...)))]
      [(c++-class name:identifier super-class:identifier body ...)
       (debug "here\n")
       (format "class ~a: public ~a {\n~a\n};\n"
               (raw-identifier #'name)
               (raw-identifier #'super-class)
               (connect (syntax-map (lambda (what) (canonical-c++-class #'name what))
                                    body ...)))]
      [(c++-return expression ...)
       (format "return ~a" (canonical-c++-expression #'(expression ...)))]
      [() ""]
      [else (begin
              (define code (canonical-c++-expression
                             (with-syntax ([body body])
                               (debug "do expression body ~a\n" #'body)
                               #'(body))))
              (if last?
                (format "return ~a;" code)
                (format "~a;" code)))]
      ))

  (define (canonical-c++-top form)
    (debug "top ~a\n" (syntax->datum form))
    (define-syntax-class symbol-or-string
      [pattern what:str #:with raw (format "\"~a\"" (syntax-e #'what))]
      [pattern what:identifier #:with raw (syntax-e #'what)])
    (define-syntax-class declaration
                         #:literals (c++-template c++-class
                                     c++-static c++-enum
                                     c++-struct)
      [pattern (c++-static type:identifier variable:identifier c++-= expression ...)
               #:with final (format "static ~a ~a = ~a"
                                    (raw-identifier #'type)
                                    (raw-identifier #'variable)
                                    (canonical-c++-expression #'(expression ...)))]
      [pattern (c++-struct name:identifier body ...)
               #:with final (format "struct ~a{\n~a\n}"
                                    (raw-identifier #'name)
                                    (indent (connect (syntax-map
                                                       (lambda (form)
                                                         (canonical-c++-class-body #'name form))
                                                       body ...))))]
      [pattern (c++-enum name:identifier stuff ...)
               #:with final (format "enum ~a{\n~a\n}"
                                    (raw-identifier #'name)
                                    (indent
                                      (connect (syntax-map (lambda (x)
                                                           (symbol->string (raw-identifier x)))
                                                         stuff ...) ",\n")))]
      [pattern (c++-template (c++-class template-type:identifier)
                             type:identifier variable:identifier)
               #:with final (format "template <class ~a> ~a ~a"
                                    (raw-identifier #'template-type)
                                    (raw-identifier #'type)
                                    (raw-identifier #'variable))])

    (define-syntax-class c++-attribute
                         #:literals (c++-static)
      [pattern c++-static])

    (define-splicing-syntax-class boring-const
                                  #:literals (c++-const)
      [pattern c++-const])
    (define-splicing-syntax-class boring-reference
                                  #:literals (c++-reference)
      [pattern c++-reference])

    #;
    (define-syntax-class function-argument
                         #:literals (c++-const c++-reference)
      [pattern ((~optional (~and c++-const const)) type:identifier
                (~optional (~and c++-reference reference)) variable:identifier)
               #:with final (format "~a~a ~a~a"
                                    (if (attribute const) "const " "")
                                    (raw-identifier #'type)
                                    (if (attribute reference) "& " "")
                                    (raw-identifier #'variable))])
    (syntax-parse form #:literals (c++-include c++-function c++-class
                                   c++-using c++-namespace)
      [(c++-include file:symbol-or-string ...)
       (string-append (connect (syntax-map (lambda (x)
                              (format "#include ~a" (syntax-e x)))
                            file.raw ...))
                      ;; add an extra newline after all #include's
                      "\n")]
      [(c++-namespace name:identifier stuff ...)
       (format "namespace ~a{\n~a\n}"
               (raw-identifier #'name)
               (indent (connect (syntax-map canonical-c++-top stuff ...))))]
      [(c++-using c++-namespace what:identifier)
       (format "using namespace ~a;" (raw-identifier #'what))]
      [(c++-function modifier:c++-attribute ...
                     type:identifier (name:id arg:c++-function-argument ...) body ...)
       (define (extra modifiers)
         (if (null? modifiers)
           ""
           (string-append
             (connect (map (lambda (x) (format "~a" (raw-identifier x)))
                           (filter (lambda (x) x) modifiers))
                      " ")
             " ")))
       (format "~a~a ~a(~a){\n~a\n}\n"
               (extra (syntax->list #'(modifier ...)))
               (raw-identifier #'type) (raw-identifier #'name)
               (connect (syntax-map (lambda (x) (format "~a" (syntax-e x))) arg.final ...) ", ")
               (indent (canonical-c++-block #'(body ...) (not (eq? 'void (raw-identifier #'type))))))]
      [(c++-function rest ...)
       (raise-syntax-error 'function "invalid syntax" form)]
      [declaration:declaration (format "~a;" (syntax-e #'declaration.final))]
      [else (raise-syntax-error 'top "unknown form" form)]
      ))

  (define (indent what)
    (string-append indent-space
                   (regexp-replace* "\t"
                                    (regexp-replace* #px"\n" what
                                                     (format "\t~a" indent-space))
                                    "\n")))

  
  #;
  (define (connect lines [separator "\n"])
    (apply string-append (add-between lines separator)))
  ))

#;
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
       (pretty-print (syntax->datum expanded)))]
    [(_ stuff ...)
     #'(#%plain-module-begin stuff ...)]
    )
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
;; syntax-case** was changed to syntax-case
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
                         (syntax-case user-stx ()
                           [(_ . pattern) (syntax/loc user-stx template)]
                           [_ (let*-values ([(sexpr) (syntax->datum user-stx)]
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
                     [c++-<< <<] [c++->> >>]
                     [c++-public public]
                     [c++-constructor constructor]
                     [c++-const const]
                     [c++-unsigned unsigned] [c++-signed signed]
                     [c++-sizeof sizeof]
                     [c++-include include]
                     [c++-template template]
                     [c++-using using] [c++-namespace namespace]
                     [c++-static static]
                     [c++-reference &]
                     [c++-struct struct]
                     [c++-local local]
                     [c++-try try] [c++-catch catch]
                     [c++-pointer *]
                     [c++-if if] [c++-while while] [c++-do do]
                     [c++-return return]
                     [c++-else else]
                     [c++-cast cast]
                     [c++-for for]
                     [c++-enum enum]
                     #;
                     [c++-top #%top]
                     [c++-define-syntax define-syntax]
                     [c++-define-syntax-rule define-syntax-rule]
                     [c++-app #%app])
         ;;define-syntax
         ;; define-syntax-rule
         #%datum
         require for-syntax
         provide define
         syntax prefix-in
         c++
         #%top
         (for-meta 1 #%app #%top #%datum
                   syntax printf stx-car stx-cdr syntax->list))
