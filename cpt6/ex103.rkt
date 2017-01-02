;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex103) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; A UFO is Posn.
; interp. (make-posn x y) is the UFO's current location

(define-struct tank (loc vel))
; A Tank is (make-tank Number Number).
; interp. (make-tank x dx) means the tank is at (x ,HEIGHT)
;   and that it moves dx pixels per clock tick

; A Missile is Posn.
; interp. (make-posn x y) is the missile's current location 

(define-struct aim (ufo tank))
(define-struct fired (ufo tank missile))
; A SIGS (short for “space invader game state”) is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)

; sigs?: Any -> Boolean
; Is this an element of SIGS?

(check-expect (sigs? (make-aim (make-posn  50 50)
                               (make-tank 50 5)))
              true)

(check-expect (sigs? (make-fired (make-posn  50 50)
                                 (make-tank 50 5)
                                 (make-posn 30 30)))
              true)

(check-expect (sigs? "foo") false)

(define (sigs? a)
  (cond
    [(aim? a) true]
    [(fired? a) true]
    [else false]
    ))

; A Coordinate is one of:
; – a negative number
;    interp. a point on the Y axis, distance from top
; – a positive number
;    interp. a point on the X axis, distance from left
; – a Posn
;    interp. a point in a scene, usual interpretation

; coordinate?: Any -> Boolean
; Is a an element of coordinate?

(check-expect (coordinate? 3) true)
(check-expect (coordinate? -5) true)
(check-expect (coordinate? (make-posn 8 -3)) true)
(check-expect (coordinate? "bar") false)

(define (coordinate? a)
  (cond
    [(number? a) true]
    [(posn? a) true]
    [else false]
    ))

; A VAnimal is either
; – a VCat
; – a VCham

; vanimal?: Any -> Boolean
; Is a an element of VAnimal?
(define (vanimal? a)
  (cond
    [(or (vcat?)
         (vcham?)) true]
    [else false]
    ))