#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/pretty
                     racket/match
                     syntax/stx
                     )
         syntax/stx
         )

(define-syntax-rule (syntax-map function template ...)
                    (map function (syntax->list #'(template ...))))

(provide syntax-map)

(provide (rename-out [new-syntax syntax]))

(define-for-syntax syntax-list? stx-list?)

(define-for-syntax (list->syntax objects lexical)
  (datum->syntax lexical objects lexical))

(define-syntax (new-syntax stx)
  (struct ellipses-node (data) #:transparent)
  ;; converts #'(a b ... c) into (a (ellipses-node b) c)
  ;; basically just puts ellipses in front
  (define (convert-to-ast data)
    (if (stx-pair? data)
      (let loop ([objects data]
                 [out '()])
        (if (stx-null? objects)
          (reverse out)
          (let ([current (if (stx-pair? objects)
                           (stx-car objects)
                           objects)]
                [next (if (stx-pair? objects)
                        (stx-cdr objects)
                        #'())])
            (if (and (identifier? current)
                     (free-identifier=? #'(... ...) current))
              (loop next (cons (ellipses-node (convert-to-ast (car out)))
                                            (cdr out)))
              (loop next (cons (convert-to-ast current) out))))))
      data))

  (define (traverse ast store [depth 0])
    (define (implicit-map? what)
      (regexp-match (pregexp ".*::.*")
                    (match (syntax-e what)
                      [(and (? symbol?) x) (symbol->string x)]
                      [else ""])))
    
    (match ast
      [(struct ellipses-node (node))
       (traverse node store (add1 depth))]
      [(list x ...)
       (for ([child x])
         (traverse child store depth))]
      [(and (? implicit-map?) x)
       (define name (syntax-e x))
       (hash-set! store name (max depth (hash-ref store name (lambda () 0))))
       (printf "Found implicit ~a\n" x)]
      [else (void)]))

  (define environment (make-hash))
  (traverse (convert-to-ast stx) environment)
  #;
  (printf "~a\n" environment)

  #;
  (define (traverse data [environment (make-hash)])
    (for ([object (syntax->list data)])))

  #;
  (pretty-print (convert-to-ast stx))

  (define (get-match regexp data)
    (cadr (regexp-match (pregexp regexp) data)))
  (define (extract-name what)
    (string->symbol (get-match "(.*)::.*" (symbol->string what))))
  (define (extract-function what)
    (string->symbol (get-match ".*::(.*)" (symbol->string what))))

  ;; need to collect all identifiers that look like id::id
  
  (syntax-case stx ()
    [(_ stuff ...)
     (with-syntax ([(new-id ...) 
                    (for/list ([(key value) (in-hash environment)])
                      (datum->syntax stx
                                     (for/fold ([result key])
                                               ([depth (in-range value)])
                                       (list result '...))))]
                   #;
                   [(function ...)
                    (for/list ([(key value) (in-hash environment)])
                      (datum->syntax stx
                                     (extract-function key)
                                     stx))]
                   [(id-stuff ...)
                    (for/list ([(key value) (in-hash environment)])
                      (define name (extract-name key))
                      (define function (datum->syntax stx (extract-function key) stx))
                      (define use
                        (for/fold ([start (with-syntax ([function function])
                                            #'(lambda (obj) (function obj)))])
                                  ([depth (in-range value)])
                                  (with-syntax ([start start])
                                    #'(lambda (stx)
                                        (map start (syntax->list stx))))))
                      (with-syntax ([name
                                      (datum->syntax stx
                                                     (for/fold ([result name])
                                                               ([depth (in-range value)])
                                                               (list result '...))
                                                     stx)]
                                    [use use])
                        #'(use #'name)))

                      #;
                      (for/fold ([result (datum->syntax stx name stx)])
                                ([depth (in-range value)])
                         (with-syntax ([next (datum->syntax stx (list result '...) stx)])
                           #'(syntax->list #'next)))

                      #;
                      (datum->syntax stx
                                     (with-syntax ([name
                                                     (datum->syntax stx
                                                                    (let ([name (extract-name key)])
                                                                      (for/fold ([result name])
                                                                                ([depth (in-range value)])
                                                                                (list result '...)))
                                                                    stx)])
                                       #'(stx->list #'name)))]
                   )

       (pretty-print (syntax->datum (syntax 
                                       (with-syntax ([new-id id-stuff] ...)
                                         (syntax stuff ...)))))

     #'(with-syntax ([new-id id-stuff] ...)
         (syntax stuff ...)))])
      

  #;
  (define-values (new out) (traverse stx))
  #;
  #'(with-syntax ([new-id (map function id-stuff)] ...)
      out)
  )

(define-syntax (new-syntax2 stx)
  (define (implicit-map? what)
    (regexp-match (pregexp ".*::.*") (symbol->string (syntax-e what))))
  (define (get-match regexp data)
    (cadr (regexp-match (pregexp regexp) data)))
  (define (extract-name what)
    (string->symbol (get-match "(.*)::.*" (symbol->string (syntax-e what)))))
  (define (extract-function what)
    (string->symbol (get-match ".*::(.*)" (symbol->string (syntax-e what)))))

  (define-splicing-syntax-class implicit-function
    [pattern x:id #:when (implicit-map? #'x)
                  #:with id (datum->syntax #'x (extract-name #'x) #'x)
                  #:with function (datum->syntax #'x (extract-function #'x) #'x)])

  (define-syntax-class syntax-dsl 
                       #:literals ([literal-ellipses ...])
    [pattern (x:implicit-function literal-ellipses rest:syntax-dsl ...)
             #:with result #'(#,@(map x.function (syntax->list #'(x.id (... ...)))) rest ...)]
    [pattern (a:syntax-dsl ...) #:with result #'(a ...)]
    [pattern x #:with result #'x])

  (syntax-parse stx
    [(_ stuff:syntax-dsl ...)
     #;
     (printf "Result is ~a\n" (syntax->datum #'(quasisyntax stuff.result ...)))
     #'(quasisyntax stuff.result ...)]))
