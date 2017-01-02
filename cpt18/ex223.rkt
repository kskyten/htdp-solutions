;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex223) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; n-1: Number -> [Number]
; Create list from 0 to (n - 1)

(check-expect (n-1 1) '(0))
(check-expect (n-1 3) '(0 1 2))

(define (n-1 n)
  (build-list n (lambda (x) x)))

; to-n: Number -> [Number]
; Create a list of numbers from 1 to n

(check-expect (to-n 2) '(1 2))

(define (to-n n)
  (build-list n add1))

; recip: Number -> [Number]
; Create a list [1, 1/10, 1/100, ...] of length n

(check-expect (recip 1) (list 1))
(check-expect (recip 2) (list 1 1/10))
(check-expect (recip 3) (list 1 1/10 1/100))

(define (recip n)
  (build-list n (lambda (n) (expt 10 (- n)))))

; evens: Number -> [Number]
; Create the first n even numbers

(check-expect (evens 2) '(0 2))
(check-expect (evens 3) '(0 2 4))

(define (evens n)
  (build-list n (lambda (n) (* 2 n))))

; eye: Number -> [Number]
; Create a n x n identity matrix

(check-expect (eye 3)
              (list
               (list 1 0 0)
               (list 0 1 0)
               (list 0 0 1)))

(define (eye n)
  (local (; make-row: Number -> [Number]
          (define (make-row i)
            (build-list n (lambda (j)
                            (if (= j i) 1 0))))
          )
    (build-list n make-row)))