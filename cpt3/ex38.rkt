;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex38) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
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

; WorldState is a Number
; the x-coordinate of the right edge of the car

; render: WorldState -> Image
; place the car into a scene, according to the given world state

(check-expect (render 50) (place-image CAR (- 50 HALF-CAR-WIDTH) Y-CAR BACKGROUND))
(check-expect (render 100) (place-image CAR (- 100 HALF-CAR-WIDTH) Y-CAR BACKGROUND))

(define (render ws)
  (place-image CAR (- ws HALF-CAR-WIDTH) Y-CAR BACKGROUND))
 
; tock: WorldState -> WorldState
; the clock ticked; move the car by three pixels

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(define (tock ws)
  (+ ws 3))

; end?: WorldState -> Boolean
; Stop program when car dissapears

(check-expect (end? (/ WIDTH-OF-WORLD 2)) false)
(check-expect (end? (+ WIDTH-OF-WORLD 1)) true)

(define (end? cw)
  (>= cw WIDTH-OF-WORLD))

; main : WorldState -> WorldState
; launch the program from some initial state
(define (main ws)
   (big-bang ws
             [on-tick tock]
             [to-draw render]
             [stop-when end?]))

