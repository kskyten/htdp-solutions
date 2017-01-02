;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex209) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define (f x) x)
(define (g x) 2)

; function=at-values: (N -> N) (N -> N) -> Boolean
; Determine whether two functions have the same
; values at 1.2, 3 and -5.755

(check-expect (function=at-values f f)
              true)
(check-expect (function=at-values f g)
              false)

(define (function=at-values f g)
  (and (= (f 1.2) (g 1.2))
       (= (f 3) (g 3))
       (= (f -5.755) (g -5.755))))