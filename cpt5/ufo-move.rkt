;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo-move) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct velocity (dx dy))
; A Velocity is a structure: (make-vel Number Number)
; interp. (make-vel a b) means that the object moves a steps
; along the horizontal and b steps along the vertical per tick
 
(define-struct ufo (loc vel))
; A UFO is a structure: (make-ufo Posn Velocity)
; interp. (make-ufo p v) is at location p
; moving at velocity v

(define v1 (make-velocity 8 -3))
(define v2 (make-velocity -5 -3))
(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))
(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; UFO -> UFO
; move the ufo u, i.e., compute its new location in one clock
; tick from now and leave the velocity as is
 
(check-expect (ufo-move u1) u3)
(check-expect (ufo-move u2) (make-ufo (make-posn 17 77) v2))
 
(define (ufo-move u)
  (make-ufo
   (posn+ (ufo-loc u) (ufo-vel u))
   (ufo-vel u)))

; posn+: Posn Velocity -> Posn
; "add" postition and velocity

(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))

(define (posn+ p v)
  (make-posn
   (+ (posn-x p) (velocity-dx v))
   (+ (posn-y p) (velocity-dy v))))