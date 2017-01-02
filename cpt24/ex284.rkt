;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex284) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct add (left right))
(define-struct mul (left right))
; A BSL expression (BSL) is one of:
; - Number
; - (make-add BSL BSL)
; - (make-mul BSL BSL)
; This version only supports two arguments

; A BSL-var-expr (BSLV) is one of:
; – Number
; – Symbol
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; An AL (association list) is [List-of Association].
; An Association is (cons Symbol (cons Number empty)).

(define al0 '((x 5)
              (y 3)
              (z 2)))

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

; eval-expression: BSL -> Number
; Evaluate a BSL expression

(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-add (make-mul 2 2) 1)) 5)

(define (eval-expression exp)
  (cond
    [(number? exp) exp]
    [(add? exp) (+ (eval-expression (add-left exp))
                   (eval-expression (add-right exp)))]
    [(mul? exp) (* (eval-expression (mul-left exp))
                   (eval-expression (mul-right exp)))]))

; numeric?: BSLV -> Boolean
; Are there only numeric values in BSLV?

(check-expect (numeric? (make-add 4 5)) true)
(check-expect (numeric? (make-add 'a 5)) false)
(check-expect (numeric? (make-add (make-mul 5 5) 5)) true)
(check-expect (numeric? (make-add (make-mul 'e 5) 5)) false)

(define (numeric? bsl)
  (cond
    [(number? bsl) true]
    [(symbol? bsl) false]
    [(add? bsl) (and (numeric? (add-left bsl))
                     (numeric? (add-right bsl)))]
    [(mul? bsl) (and (numeric? (mul-left bsl))
                     (numeric? (mul-right bsl)))]))

; eval-variable: BSLV -> Number
; Evaluate BSLV if it doesn't contain variables

(check-expect (eval-variable (make-add 1 1)) 2)
(check-expect (eval-variable (make-add (make-mul 2 3) 1)) 7)
(check-error (eval-variable (make-add 's 1)) "Can not evaluate an expression with variables.")

(define (eval-variable bslv)
  (if (numeric? bslv)
      (eval-expression bslv)
      (error "Can not evaluate an expression with variables.")))

; eval-variable*: BSLV AL -> Number
; Evaluate an expression e with variables assigned in association list da

(check-expect (eval-variable* 1 empty) 1)
(check-error (eval-variable* 's empty) "Undefined variables.")
(check-expect (eval-variable* (make-add 1 1) al0) 2)
(check-expect (eval-variable* (make-add (make-mul 2 3) 1) al0) 7)
(check-expect (eval-variable* (make-add 'x 1) al0) 6)
(check-expect (eval-variable* (make-add 'x 'y) al0) 8)
(check-error (eval-variable* (make-add 's 2) al0) "Undefined variables.")

(define (eval-variable* e da)
  (cond
    [(empty? da) (if (numeric? e)
                     (eval-expression e)
                     (error "Undefined variables."))]
    [else (local (
                  (define result
                    (foldr (lambda (head tail)
                             (subst tail (first head) (second head)))
                     e
                     da))
                  )
            (if (numeric? result)
                (eval-expression result)
                (error "Undefined variables.")))]))