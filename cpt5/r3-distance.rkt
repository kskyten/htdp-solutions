;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname r3-distance) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct r3 (x y z))
; R3 is (make-r3 Number Number Number)
 
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

; distance: R3 -> Number
; compute the distance between a point p and the origin

(check-expect (distance (make-r3 0 0 10)) 10)
(check-expect (distance (make-r3 3 4 0)) 5)
(check-expect (distance (make-r3 0 5 12)) 13)

(define (distance p)
  (sqrt
   (+ (sqr (r3-x p))
      (sqr (r3-y p))
      (sqr (r3-z p)))))