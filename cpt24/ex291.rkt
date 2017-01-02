;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex291) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; errors
(define UNDEFINED "undefined function")
(define VARIABLES "Can not evaluate an expression with variables.")

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

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSLF BSLF)
; - (make-mul BSLF BSLF)
; - (make-fun BSLF BSLF)

(define bslf0 (make-fun 'k
                        (make-add 1 1)))

(define bslf1 (make-mul 5
                        (make-fun 'k
                                  (make-add 1 1))))

(define bslf2 (make-mul (make-fun 'i 5)
                        (make-fun 'k
                                  (make-add 1 1))))


; subst: BSLV Symbol Number -> BSLV
; Replace symbol sy for number n in BSL-expression bsl

(check-expect (subst (make-add 'x 3) 'x 5)
              (make-add 5 3))
(check-expect (subst (make-add 'x 3) 'y 5)
              (make-add 'x 3))
(check-expect (subst (make-add (make-mul 'x 5) 3) 'x 5)
              (make-add (make-mul 5 5) 3))

(define (subst bsl sy n)
  (cond
    [(number? bsl) bsl]
    [(symbol? bsl) (if (symbol=? sy bsl) n bsl)]
    [(add? bsl) (make-add (subst (add-left bsl) sy n) (subst (add-right bsl) sy n))]
    [(mul? bsl) (make-mul (subst (mul-left bsl) sy n) (subst (mul-right bsl) sy n))]))


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

; eval-definition1: BSLF Symbol Symbol BSLF
; Evaluate a BSL expression exp with a function application
; of the function f with the argument x. The function f is defined as
; the expression body

(check-error (eval-definition1 (make-add 1 'v) 'f 'x (make-add 'x 1))
             "Can not evaluate an expression with variables.")
(check-error (eval-definition1 (make-add 1 (make-fun 'g 1))
                               'f 'x (make-add 'x 1))
             "Can not evaluate an expression with variables.")
(check-expect (eval-definition1 (make-add 1 1) 'f 'x (make-add 'x 1))
              2)
(check-expect (eval-definition1 (make-add 1 (make-fun 'f 1))
                                'f 'x (make-add 'x 1))
              3)
(check-expect (eval-definition1 (make-mul 1 (make-fun 'f 1))
                                'f 'x (make-add 'x 1))
              2)


(define (eval-definition1 exp f x body)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (error "Can not evaluate an expression with variables.")]
    [(add? exp) (+ (eval-definition1 (add-left exp) f x body)
                   (eval-definition1 (add-right exp) f x body))]
    [(mul? exp) (* (eval-definition1 (mul-left exp) f x body)
                   (eval-definition1 (mul-right exp) f x body))]
    [(fun? exp) (if (symbol=? f (fun-name exp))
                    (eval-definition1
                     (subst body x (eval-definition1 (fun-arg exp) f x body))
                     f
                     x
                     body)
                    (error "Can not evaluate an expression with variables."))]))

; eval-definition*: BSLF FD* -> Number
; Evaluate an expression e with functions defined in da

(check-error (eval-definition* (make-add 1 'v) da-fgh)
             VARIABLES)
(check-error (eval-definition* (make-fun 'v 1) da-fgh)
             UNDEFINED)
(check-expect (eval-definition* (make-add 1 1) da-fgh) 2)
(check-expect (eval-definition* (make-add 1 (make-fun 'f 2)) da-fgh) 6)
(check-expect (eval-definition* (make-add 1
                                          (make-mul 2
                                                    (make-fun 'f 2))) da-fgh) 11)

(define (eval-definition* e da)
  (cond
    [(number? e) e]
    [(symbol? e) (error VARIABLES)]
    [(add? e) (+ (eval-definition* (add-left e) da)
                 (eval-definition* (add-right e) da))]
    [(mul? e) (* (eval-definition* (mul-left e) da)
                 (eval-definition* (mul-right e) da))]
    [(fun? e) (local (
                      (define result (lookup-def da (fun-name e)))
                      ) 
                (eval-definition1 e
                                  (fd-name result)
                                  (fd-param result)
                                  (fd-body result)))]))