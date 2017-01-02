;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex290) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define UNDEFINED "undefined function")

(define-struct add (left right))
(define-struct mul (left right))

(define-struct fun (name arg))
; A function application (FA) is (make-fun String BSLF)

(define-struct fd (name param body))
; A BSL-fun-def (FD) is (make-fd Symbol Symbol BSLF)

(define f (make-fd 'f 'x (make-add 3 'x)))
(define g (make-fd 'g 'y (make-fun 'f
                                   (make-mul 2 'y))))
(define h (make-fd 'h 'v (make-add (make-fun 'f 'v)
                                   (make-fun 'g 'v))))

; A BSL-fun-def* (FD*) is [List-of FD]
(define da-fgh (list f g h))

; lookup-def: FD* Symbol -> FD
; retrieve the definition of f in function definition list da
; or signal "undefined function" if da does not contain one
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 't) UNDEFINED)

(define (lookup-def da f)
    (cond
      [(empty? da) (error UNDEFINED)]
      [else (if (symbol=? f (fd-name (first da)))
                (first da)
                (lookup-def (rest da) f))]))