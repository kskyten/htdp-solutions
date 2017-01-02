;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex137) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; multiply: N Number -> Number
; Compute (* n x) without using *

(check-expect (multiply 2 5) (+ 5 5))
(check-expect (multiply 3 5) (+ 5 5 5))

(define (multiply n k)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ k (multiply (sub1 n) k))]
    [else (error "Expected a N, given: " n)]))