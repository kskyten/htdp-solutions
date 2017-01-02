;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex56) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define MED-TAX 0.05)
(define HIGH-TAX 0.08)

; A Price falls into one of the three intervals:
; - [0,1000)
; - [1000,10000)
; - [10000,oo)

; sales-tax: Price -> Number
; compute tax for price p

(check-expect (sales-tax 50) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 1234) (* 0.05 1234))
(check-expect (sales-tax 10000) (* 0.08 10000))

(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p 1000)) 0]
    [(and (>= p 1000) (< p 10000)) (* MED-TAX p)]
    [(>= p 10000) (* HIGH-TAX p)]))