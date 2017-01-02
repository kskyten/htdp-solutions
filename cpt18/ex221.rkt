;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex221) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct toy (desc aprice sprice))
; A Toy is (make-toy String Number Number)
; The desciption, acquisition price and sales price
; of the toy

(define t0 (make-toy "Teddy bear" 0.5 10))
(define lt (list (make-toy "Teddy bear" 0.5 10)
                 (make-toy "Barbie" 0.5 50)
                 (make-toy "Action man" 2 10)))

; profit: R [Toy] -> [Toy]
; Sort a list of toys into an
; ascending (R = <) or descending (R = >)
; list by the profit margin

(check-expect (profit > lt)
              (list (make-toy "Barbie" 0.5 50)
                    (make-toy "Teddy bear" 0.5 10)
                    (make-toy "Action man" 2 10)))

(define (profit R lot)
  (local (
          ; cmp: Toy Toy -> Boolean
          ; Compare toy1 to toy2
          (define (cmp toy1 toy2)
          (R (- (toy-sprice toy1) (toy-aprice toy1))
             (- (toy-sprice toy2) (toy-aprice toy2))))
          )
    (sort lot cmp)))