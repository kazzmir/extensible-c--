#lang racket/base

(require syntax/parse (for-template "c++-literals.rkt"))

(provide assignment-operator)

(define-syntax-class assignment-operator
                     #:literals (+=)
                     [pattern +=])
