;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex39) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; World constants
(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 100)
(define tree
  (underlay/xy (circle 10 'solid 'green)
               9 15
               (rectangle 2 20 'solid 'brown)))
(define CANVAS (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
(define BACKGROUND (overlay/xy tree -50 -50 CANVAS))

; Car constants
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-RADIUS 1 "solid" "white"))
(define BOTH-WHEELs (beside WHEEL SPACE WHEEL))

(define BODY
  (rectangle WHEEL-DISTANCE
             (/ WHEEL-DISTANCE 2) "solid" "red"))

(define CAR (overlay/xy BOTH-WHEELs 0 (- (* 2 WHEEL-RADIUS)) BODY))

(define Y-CAR (- HEIGHT-OF-WORLD (image-height CAR)))
(define CAR-WIDTH (image-width CAR))
(define HALF-CAR-WIDTH (/ CAR-WIDTH 2))
(define OFFSET (/ WIDTH-OF-WORLD 2))

; AnimationState is a Number
; the number of clock ticks since the animation started

; tock: AnimationState -> AnimationState
; the clock has ticked; change the animation state
(define (tock as)
  (+ 1 as))

; render: AnimationState -> Image
; render the image of the world with a given animation state

(check-expect (render 0) (place-image CAR (+ (car-posn 0) OFFSET) Y-CAR BACKGROUND))
(check-expect (render 50) (place-image CAR (+ (car-posn 50) OFFSET) Y-CAR BACKGROUND))

(define (render as)
  (place-image CAR (+ (car-posn as) OFFSET) Y-CAR BACKGROUND))

; car-posn: AnimationState -> Number
; calculate car x-coordinate for a given animation state
(define (car-posn as)
  (* (/ WIDTH-OF-WORLD 2) (sin (/ as 10))))

; end?: AnimationState -> Boolean
; Stop the animation after 500 ticks

(check-expect (end? 10) false)
(check-expect (end? 501) true)

(define (end? as)
  (>= as 500))

; main : AnimationState -> AnimationState
; launch the program from some initial state
(define (main as)
   (big-bang as
             [on-tick tock]
             [to-draw render]
             [stop-when end?]))

