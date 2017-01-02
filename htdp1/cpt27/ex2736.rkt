;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2736) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
(define TOLERANCE 0.001)

; weird-fun: Number -> Number
; Test function for integration
(define (weird-fun x)
  (cond
    [(and (<= 0 x)
          (>= 5 x)) 5]
    [else (sqr x)]))


; trapezoid: (Number -> Number) Number Number -> Number
; Compute the area of a trapezoid with a base b-a and a height
; determined by f

(check-expect (trapezoid (lambda (x) x) 0 2) 2)

(define (trapezoid f a b)
  (* (- b a)
     (/ (+ (f a) (f b)) 2)))

; integrate-dc: (Number -> Number) Number Number -> Number
; Compute the riemann sum of the function f between a and b
; using trapezoids

(check-within (integrate-dc (lambda (x) 1) 0 2) 2 (* 2 TOLERANCE))

(define (integrate-dc f a b)
  (cond
    [(<= (- b a) TOLERANCE) (trapezoid f a b)]
    [else 
     (local ((define mid (/ (+ a b) 2)))
       (+ (integrate-dc f a mid)
          (integrate-dc f mid b)))]))

; integrate-adaptive: (Number -> Number) Number Number -> Number
; Compute the integral of the function f between a and b in a smart way

(check-expect (integrate-adaptive weird-fun 0 5) 25)
(check-within (integrate-adaptive weird-fun 0 10)
              (+ 25 (/ (- 1000 125) 3)) 1)

(define (integrate-adaptive f a b)
  (local ((define mid (/ (+ a b) 2)))
  (cond
    ; recurse only if the function is not flat
    [(>= (* TOLERANCE (- b a))
         (abs (- (trapezoid f a mid)
                 (trapezoid f mid b))))
     (trapezoid f a b)]
    ; base case for recursion
    [(<= (- b a) TOLERANCE) (trapezoid f a b)]
    [else (+ (integrate-adaptive f a mid)
             (integrate-adaptive f mid b))])))