;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex289) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct add (left right))
(define-struct mul (left right))

(define-struct fun (name arg))
; A function application (FA) is (make-fun String BSLF)

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