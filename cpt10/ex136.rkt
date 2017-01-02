;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex136) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; N is one of:
; - 0
; - (add1 N)

; add-to-pi: N -> Number
; compute (+ n pi) without using +

(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (add-to-pi n)
  (cond
   [(zero? n) pi]
   [(positive? n) (+ 1 (add-to-pi (sub1 n)))]
   ))

; add: N -> Number
; compute (+ n k) without using +

(check-within (add 3 pi) (+ 3 pi) 0.001)

(define (add n k)
  (cond
   [(zero? n) k]
   [(positive? n) (+ 1 (add-to-pi (sub1 n)))]
   ))