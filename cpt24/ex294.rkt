;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex294) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; errors
(define UNDEFINEDV "undefined variable: ")
(define UNDEFINEDF "undefined function: ")

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

(define-struct con (name value))
; A constant definition (CD) is (make-con Symbol BSLF)

(define c0 (make-con 'foo (make-add 1 (make-mul 2 3))))
(define c1 (make-con 'hello (make-mul (make-fun 'f 2) 3)))

; A BSL-da-all (DA) is one of:
; - empty
; - (cons CD DA)
; - (cons FD DA)

(define da0 (list c1 c0 f g h))

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSLF BSLF)
; - (make-mul BSLF BSLF)
; - (make-fun BSLF BSLF)


; lookup-con-def: Symbol BSL-da-all -> BSLF
; Retrieve the definition of the constant f from da

(check-expect (lookup-con-def 'foo da0)
              (make-add 1 (make-mul 2 3)))
(check-error (lookup-con-def 'f da0)
             (string-append UNDEFINEDV "'f"))


(define (lookup-con-def f da)
  (cond
    [(empty? da) (error UNDEFINEDV f)]
    [(con? (first da))
     (if (symbol=? f (con-name (first da)))
         (con-value (first da))
         (lookup-con-def f (rest da)))]
    [(fd? (first da))
     (lookup-con-def f (rest da))]))


; lookup-fun-def: Symbol BSL-da-all -> BSLF
; Retrieve the definition for function f from da

(check-expect (lookup-fun-def 'f da0) f)
(check-error (lookup-fun-def 'k da0)
             (string-append UNDEFINEDF "'k"))

(define (lookup-fun-def f da)
  (cond
    [(empty? da) (error UNDEFINEDF f)]
    [(con? (first da))
     (lookup-fun-def f (rest da))]
    [(fd? (first da))
     (if (symbol=? f (fd-name (first da)))
         (first da)
         (lookup-fun-def f (rest da)))]))