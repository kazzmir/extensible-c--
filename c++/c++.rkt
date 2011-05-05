#lang racket/base

(require syntax/parse
         (for-template "c++-literals.rkt"))

(provide assignment-operator
         inside-curlies)

(define-literal-set operators #:for-template
                    (+= -=))

(define-syntax-class assignment-operator
                     #:literal-sets (operators)
                     [pattern (~or += -=)])

(define-syntax-class inside-curlies
                     [pattern x #:when (begin
                                         #;
                                         (printf "curly? ~a ~a\n" #'x (syntax-property #'x 'paren-shape))
                                         (eq? #\{ (syntax-property #'x 'paren-shape)))])

#;
(define-syntax-class assignment-operator
                     #:literals (+=)
                     [pattern +=])
