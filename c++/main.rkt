#lang racket/base

(require (for-syntax (except-in racket/base syntax)
                     racket/pretty syntax/stx
                     racket/list racket/match
                     syntax/parse
                     "utils.rkt"
                     )
         racket/match)

(begin-for-syntax
  (define (expand-c++ code)
    (define (recur code)
      (expand-c++ code))
    #;
    (printf "Expanding c++ ~a\n" code)
    (define expanded (local-expand code 'expression #f))
    (syntax-case expanded ()
      [(sub ...) (with-syntax ([(sub* ...) (map recur (syntax->list #'(sub ...)))])
                   #'(sub* ...))]
      [_ code]))
  )

(define-syntax (c++ stx)
  (syntax-case stx ()
    [(_ forms ...)
     (with-syntax ([(form* ...) (map expand-c++ (syntax->list #'(forms ...)))])
       #'(compile-c++ form* ...))]))

(define-syntax-rule (define-literals name ...)
                    (begin
                      (define name #f) ...))

;; literal syntax anchors
(define-literals c++-function c++-class)

(begin-for-syntax

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

(define (compile-class-statement form)
  (match form
    [(list 'constructor x ...) "constructor"]
    [(list 'function x ...) (compile-top-level form)]))

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

(define (compile-statement line)
  (syntax-parse line
    [(c++-class name super-class body ...)
     #'(class name)]
    [else (raise-syntax-error 'compile-statement "failed" line)])
  #;
  (match line
    [(list 'class name super-class body ...)
     (format "class ~a: public ~a {
~a
};\n" name super-class (connect (map compile-class-body body)))]
    [(and (? symbol?) name) (format "return ~a;" name)]
    [(list (and (? symbol?) name) (and (? operator?) operator) args ...)
     (format "~a ~a ~a;" name operator (map compile-expression args))]
    [(list (and (? symbol?) name) arguments ...)
     (format "~a(~a);" name (connect (map compile-expression arguments) ", "))]
    [else "something"]
    [else (error 'compile-statement "unknown statement ~a" line)]
    ))

(define (compile-body code)
  (for/list ([statement code])
    (compile-statement statement))
  #;
  (connect (for/list ([statement code])
             (compile-statement statement))))

(define (compile-top-level* code)
  #;
  (printf "Compiling top level ~a\n" code)
  (syntax-parse code
    #:literals (c++-function)
    [(c++-function type:id (name:id parameters ...) body ...)

     #'(type name (){ body::compile-statement ... })

     #;
     (with-syntax ([(body ...) (compile-body (syntax->list #'(body ...)))])
       #'(type name(){ body ... }))]

    [else (raise-syntax-error 'top-level "fail")]
    ))

#;
  (match code
    [(list 'function type (list name parameters ...) body ...)
     (format "~a ~a(){\n~a\n}\n" type name (indent (compile-body body)))])

(set! compile-top-level compile-top-level*)

)

(define-syntax (compile-c++ code)
  (syntax-parse code
    [(_ stuff ...)
     (with-syntax ([(compiled ...) (syntax-map compile-top-level stuff ...)])
       #'(printf "~a\n" #''(compiled ...)))]))

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
       #;
       (printf "stuff is ~a\n" #'(stuff ...))
       (define expanded (local-expand #'(#%plain-module-begin stuff ... (c++ c++-forms ...)) 'module-begin '()))
       (pretty-print (syntax->datum expanded))
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

(provide (rename-out [c++-module-begin #%module-begin]
                     [c++-function function]
                     [c++-class class]
                     #;
                     [c++-top #%top]
                     [c++-app #%app])
         define-syntax define-syntax-rule #%datum
         require for-syntax
         c++
         #%top
         (for-meta 1 #%app #%top #%datum
                   syntax printf stx-car stx-cdr syntax->list))
