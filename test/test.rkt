#lang c++

(require (for-syntax racket/base syntax/parse))

#;
(define-syntax (function stx) #'1)

#;
(define-syntax (local stx)
  #;
  (printf "local ~a\n" stx)
  #;
  (printf "~a ~a ~a ~a\n" (stx-car stx) (stx-car (stx-cdr stx)) (stx-car (stx-cdr (stx-cdr stx)))
          (stx-car (stx-cdr (stx-cdr (stx-cdr stx))))
          )
  #;
  (printf "~a\n" (syntax->list stx))
  #;
  (printf "prepare for syntax case\n")
  #;
  (syntax-parse stx
    [(x y ...) (printf "syntax-parse worked\n")])
  #;
  (syntax-case stx (local)
    [(x y ...) (printf "got an x\n")]
    [else (printf "could not match??\n")])
  (syntax-case stx ()
    [(_ stuff ...)
     #'(local-variable stuff ...)]
    [else (raise-syntax-error 'local "wtf")]))

#;
(define-syntax-rule (local stuff ...)
                    (local-variable stuff ...))

#|
function void loadingScreenSimpleX1(LoadingContext & context, const Level::LevelInfo & level){
  class Logic extends Util::Logic {
  }
}
|#

(c++

(function void (loadingScreenSimpleX1 [LoadingContext & context]
                                      [const Level::LevelInfo & level])
          ;; #:attributes [static]

  (class Logic Util::Logic
    (public
      (constructor ([local]
                    [local &]
                    [local LoadingContext & context]
                    [local int & angle]
                    [local int speed])
                   ()
                   )
      (function double (ticks [double system]) system)
      (function bool (done) (context.done))
      (function void (run) (angle += speed))))
  (class Draw Util::Draw
    (public
      (constructor ([local int & angle]
                    [local const int speed])
                   ([local Graphics::Bitmap work 40 40]
                    [local Graphics::Bitmap original 40 40])
        (original.BlitFromScreen 0 0)
        (variable int color1 = (Graphics::makeColor 0 0 0))
        (variable int color2 = (Graphics::makeColor 0x00 0x99 0xff))
        (variable int color3 = (Graphics::makeColor 0xff 0x22 0x33))
        (variable int color4 = (Graphics::makeColor 0x44 0x77 0x33))
        (color{0} = color1)
        (color{1} = color2)
        (color{2} = color3)
        (color{3} = color4)
        (Graphics::Bitmap::transBlender 0 0 0 64))
      (function void (draw)
        (variable int max = (sizeof colors) / (sizeof int))
        (variable double middleX = (work.getWidth) / 2)
        (variable double middleY = (work.getHeight) / 2)
        (original.Blit work)
        (for ([variable int i = 0]
              [i < max]
              [i += 1])
          (variable double x = (cos [(Util::radians angle) + 360 / max * i]) * 15)
          (variable double y = (sin [(Util::radians angle) + 360 / max * i]) * 15)
          ((work.translucent).circleFill [middleX + x] [middleY + y] 2 colors{i}))))))
)
