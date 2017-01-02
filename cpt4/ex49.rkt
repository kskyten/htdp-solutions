;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex49) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define W 200)
(define H 200)
(define MT (empty-scene W H))
(define RADIUS 10)
(define SIM-DURATION)
(define SPEED 3)

; TrafficLight shows one of three colors:
; - "red"
; - "yellow"
; - "green"

; next-light: TrafficLight -> TrafficLight
; given state s, determine the next state

(check-expect (next-light "red") "green")
(check-expect (next-light "green") "yellow")
(check-expect (next-light "yellow") "red")

(define (next-light s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

;render: TrafficLight -> Image
; render the traffic light with color c

(check-expect (render "red")
              (place-image
               (circle RADIUS "solid" "red")
               (/ W 2)
               (/ H 2)
               MT))

(define (render c)
  (place-image
   (circle RADIUS "solid" c)
   (/ W 2)
   (/ H 2)
   MT))

; main: WorlState -> WorldState
; initialize world
(define (main ws)
  (big-bang ws
            [on-tick next-light SPEED SIM-DURATION]
            [to-draw render]))