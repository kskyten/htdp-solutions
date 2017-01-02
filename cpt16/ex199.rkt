;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex199) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; addn*: N LoN -> LoN
; add n to each number on l

(check-expect (addn* 1 '()) empty)
(check-expect (addn* 1 '(1 2 3)) '(2 3 4))

(define (addn* n l)
  (cond
    [(empty? l) '()]
    [else
     (cons (+ (first l) n)
           (addn* n (rest l)))]))

; Lon -> Lon
; add 1 to each number on l

(check-expect (add1* '()) empty)
(check-expect (add1* '(1 2 3)) '(2 3 4))

(define (add1* l)
  (addn* 1 l))

; Lon -> Lon
; add 5 to each number on l

(check-expect (plus5 '()) empty)
(check-expect (plus5 '(1 2 3)) '(6 7 8))

(define (plus5 l)
  (addn* 5 l))