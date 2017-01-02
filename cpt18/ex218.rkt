;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex218) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct ir (name price))
; An IR is
;   (make-ir String Number)

(define i0 (list (make-ir "Foo" 12)
                 (make-ir "Bar" 0.4)
                 (make-ir "Baz" 1)
                 (make-ir "Ice cream" 0.24)
                 (make-ir "Jelly" 6)
                 (make-ir "Foo" 12)
                 (make-ir "Bar" 0.4)
                 (make-ir "Baz" 1)
                 (make-ir "Ice cream" 0.24)
                 (make-ir "Jelly" 6)
                 (make-ir "Foo" 12)
                 (make-ir "Bar" 0.4)
                 (make-ir "Baz" 1)
                 (make-ir "Ice cream" 0.24)
                 (make-ir "Jelly" 6)))

; Inventory -> Inventory
; to create an Inventory from an-inv for all
; those items that cost less than $1
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) empty]
    [else
     (local ((define le1 (extract1 (rest an-inv))))
       (cond
         [(<= (ir-price (first an-inv)) 1.0)
          (cons (first an-inv) le1)]
         [else le1]))]))