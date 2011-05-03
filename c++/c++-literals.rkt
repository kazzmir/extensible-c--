#lang racket/base

(define-syntax-rule (define-literals name ...)
                    (begin
                      (define name #f) ...))

(define-syntax-rule (define+provide-literals name ...)
                    (begin
                      (provide name ...)
                      (define-literals name ...)))

;; literal syntax anchors
(define+provide-literals function class public
                         variable
                         constructor
                         -= +=)
