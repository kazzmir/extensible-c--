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

(provide syntax-map define-recursive)

(provide (rename-out [new-syntax syntax]))

(define-for-syntax syntax-list? stx-list?)

(define-for-syntax (list->syntax objects lexical)
  (datum->syntax lexical objects lexical))

;; find all occurences of id1::id2 and wrap the expression inside a
;; with-syntax that maps the function `id2' over the pattern variable `id1'
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

  ;; pull out identifiers that look like id::id and compute their ellipses depth
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
       #;
       (printf "Found implicit ~a\n" x)]
      [else (void)]))

  ;; need to collect all identifiers that look like id::id
  (define environment (make-hash))
  (traverse (convert-to-ast stx) environment)

  (define (get-match regexp data)
    (cadr (regexp-match (pregexp regexp) data)))
  (define (extract-name what)
    (string->symbol (get-match "(.*)::.*" (symbol->string what))))
  (define (extract-function what)
    (string->symbol (get-match ".*::(.*)" (symbol->string what))))

  (define (extract-ids environment)
    (for/list ([(key value) (in-hash environment)])
      ;; our new variable should use the same lexical scope as the pattern variable
      ;; that is being considered. as a hack we can use the lexical scope of
      ;; the current syntax object.
      (datum->syntax stx
                     (for/fold ([result key])
                               ([depth (in-range value)])
                               (list result '...))
                     stx)))

  (define (extract-values environment)
    (for/list ([(key value) (in-hash environment)])
      (define name (extract-name key))
      (define function (datum->syntax stx (extract-function key) stx))
      (define use
        (for/fold ([start function])
                  ([depth (in-range value)])
                  ;; every time through the loop is another layer of ellipses nesting
                  ;; so we are guaranteed to have a list of syntax objects.
                  ;; all we have to do is apply the previous function to each object
                  ;; in the syntax list
                  (with-syntax ([start start])
                    #'(lambda (stx)
                        (map start (syntax->list stx))))))
      ;; now we have to apply the function we built to the original pattern
      ;; variable, making sure to apply the correct amount of ellipses
      ;; to the pattern variable
      (with-syntax ([name (datum->syntax stx
                                         (for/fold ([result name])
                                                   ([depth (in-range value)])
                                                   (list result '...))
                                         stx)]
                    [use use])
        #'(use #'name))))

  (syntax-case stx ()
    [(_ stuff ...)
     (with-syntax ([(new-id ...) (extract-ids environment)]
                   [(id-stuff ...) (extract-values environment)])

       #;
       (pretty-print (syntax->datum (syntax 
                                       (with-syntax ([new-id id-stuff] ...)
                                         (syntax stuff ...)))))

       ;; bind foo::bar things to (map bar (syntax->list #'(foo ...))) or
       ;; whatever the original `foo' is bound to
     #'(with-syntax ([new-id id-stuff] ...)
         (syntax stuff ...)))])
  )

;; converts `foo::bar ...' into #,@(map bar foo)
;; but it doesn't handle ellipses depths other than 1
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

(define-syntax-rule (define-recursive (define (name args ...) body ...) ...)
                    (define-values (name ...)
                                   (letrec ([name (lambda (args ...) body ...)]
                                            ...)
                                     (values name ...))))
