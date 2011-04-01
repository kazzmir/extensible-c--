#lang racket/base

(require (for-syntax (except-in racket/base syntax)
                     racket/pretty syntax/stx
                     racket/list racket/match
                     syntax/parse
                     "utils.rkt"
                     "transformer.rkt"
                     )
         racket/list
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
         (expand-c++ (macro #'(something rest ...))))
       #'(something rest::expand-c++ ...))]
    [(x ...) #'(x::expand-c++ ...)]
    [x #'x]
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

(define (canonical-c++-expression expression)
  (match expression
    [(list (list name args ...))
     (format "~a(~a)" name (connect (map (lambda (x)
                                           (format "~a" x))
                                         args) ", "))]))

(define (canonical-c++-body what)
  (match what
    [(list 'variable type name '= expression ...)
     (format "~a ~a = ~a;" type name (canonical-c++-expression expression))]
    [else ""]))

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

(define (canonical-c++-top form)
  (match form
    [(list 'function type (list name args ...) body ...)
     (format "~a ~a(){\n~a\n}" type name (indent (canonical-c++ body)))]
    [(list 'class name super-class body ...)
     (format "class ~a: public ~a {\n~a\n}\n" name super-class (connect (map (lambda (what)
                                                                               (canonical-c++-class name what))
                                                                               body)))]
    [else ""]))

(define (connect lines [separator "\n"])
  (apply string-append (add-between lines separator)))

(define (canonical-c++ forms)
  (connect (map canonical-c++-top forms)))

(define-syntax (c++ stx)
  (syntax-parse stx
    [(_ form ...)
     #'(printf "~a\n" (canonical-c++ '(form::expand-c++ ...)))]))

#;
(define-syntax (c++ stx)
  (syntax-case stx ()
    [(_ form ...) #'(compile-c++ form::expand-c++ ...)
     #;
     (with-syntax ([(form* ...) (map expand-c++ (syntax->list #'(forms ...)))])
       #'(compile-c++ form* ...))]))

(define-syntax-rule (define-literals name ...)
                    (begin
                      (define name #f) ...))

;; literal syntax anchors
(define-literals c++-function c++-class c++-public
                 c++-constructor)

(begin-for-syntax

  #;
(define compile-top-level #f)

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

;; connect lines together with newlines between them
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

(provide (rename-out [c++-module-begin #%module-begin]
                     [c++-function function]
                     [c++-class class]
                     [c++-public public]
                     [c++-constructor constructor]
                     #;
                     [c++-top #%top]
                     [c++-define-syntax define-syntax]
                     [c++-app #%app])
         ;;define-syntax
         define-syntax-rule #%datum
         require for-syntax
         c++
         #%top
         (for-meta 1 #%app #%top #%datum
                   syntax printf stx-car stx-cdr syntax->list))
