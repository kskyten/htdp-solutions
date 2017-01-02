;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex283) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct add (left right))
(define-struct mul (left right))
; A BSL-var-expr (BSLV) is one of:
; – Number
; – Symbol
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)


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