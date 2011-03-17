(module reader syntax/module-reader c++

#|
#:read c++-read
#:read-syntax c++-read-syntax
#:whole-body-readers? #t

(define (c++-read port)
  (syntax->datum (c++-read-syntax #f port)))

(define (c++-read-syntax source-name port)
  (printf "Read from ~a\n" port)
  (define syntax (read-syntax source-name port))
  (printf "Read ~a\n" (syntax->datum syntax))
  syntax)
|#

)
