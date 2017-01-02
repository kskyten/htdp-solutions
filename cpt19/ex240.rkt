;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex240) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Shape is a function:
;   [Posn -> Boolean]
; interpretation:
;   if s is a shape and p a Posn,
;   (s p) is true if the Posn is inside the shape
;   and true if the Posn is outside

; Shape Posn -> Boolean
; Is point p inside shape s
(define (inside? s p)
  (s p))

; Number Number Number -> Shape
; create a data representation for a circle of radius r
;   located at (center-x, center-y)
(define (make-circle center-x center-y r)
  ; Posn -> Boolean
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

; Number Number Number Number -> Shape
; create a data representation of a rectangle
(define (make-rectangle upperleft-x upperleft-y width height)
  ; Posn -> Boolean
  (lambda (p)
    (and (between upperleft-x (posn-x p) width)
         (between (- upperleft-y height) (posn-y p) height))))

(define circle1 (make-circle 3 4 5))
(define rectangle1 (make-rectangle 0 3 10 3))

(define sl (list (make-circle 1 2 3)
                 (make-circle 5 5 2)
                 (make-circle 6 7 8)
                 (make-rectangle 0 3 10 3)
                 (make-rectangle 6 6 10 3)))

; distance-between: Number Number Posn -> Number
; Calculate the distance between (x, y) an p

(check-expect (distance-between 0 0 (make-posn 0 1))
              1)
(check-expect (distance-between 0 0 (make-posn 0 3))
              3)
(check-expect (distance-between 0 0 (make-posn 0 0))
              0)

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))


; between: Number Number Number -> Boolean
; Is x between left and (left + width)

(check-expect (between 1 3 5)
              true)
(check-expect (between 1 7 5)
              false)

(define (between left x width)
  (< x (- width left)))

; Shape Shape -> Shape
(define (make-combination s1 s2)
  ; Posn -> Boolean
  (lambda (p)
    (or (inside? s1 p) (inside? s2 p))))