#lang racket/base

(require syntax/parse
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
  #:literals (const reference)
  [pattern ((~optional (~and const has-const)) type:identifier
                                               (~optional (~and reference has-reference)) variable:identifier)
           #:with final (format "~a~a ~a~a"
                                (if (attribute has-const) "const " "")
                                (raw-identifier #'type)
                                (if (attribute has-reference) "& " "")
                                (raw-identifier #'variable))])

(provide type)
(define-splicing-syntax-class type
  #:literals (pointer const)
  [pattern (~seq (~optional (~and const has-const)) type:identifier pointer)
           #:with final (format "~a~a*"
                                (if (attribute has-const) "const " "")
                                (raw-identifier #'type))]
  [pattern (~seq type:identifier)
           #:with final (format "~a" (raw-identifier #'type))])
