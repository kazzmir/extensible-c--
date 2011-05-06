#lang racket/base

(require syntax/parse
         (for-template "c++-literals.rkt"))

(provide assignment-operator
         inside-curlies inside-brackets
         binary-operator
         operator)

(define-literal-set operators #:for-template
                    (+= -= - /))

(define-syntax-class assignment-operator
                     #:literal-sets (operators)
                     [pattern (~or += -=)])

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

#;
(define-syntax-class assignment-operator
                     #:literals (+=)
                     [pattern +=])
