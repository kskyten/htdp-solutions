;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex53) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; physical constants
(define H 300)
(define W  100)
(define X-ROCKET (/ W 2))
(define YDELTA 3)
 
; graphical constants
(define BACKG  (empty-scene W H))
(define HROCKET 30)
(define HALF-ROCKET (/ HROCKET 2))
(define ROCKET (rectangle 5 HROCKET "solid" "red"))

; A LRCD (short for: launching rocket count down) is one of:
; – "resting"
; – a number in [-3,-1]
; – a non-negative number
; interp. a rocket resting on the ground, in count-down mode,
;   or the number of pixels from the top of canvas to the bottom of the rocket

; render: LRCD -> Image
; render the state of the rocket

(check-expect (render "resting") (place-image ROCKET X-ROCKET (- H HALF-ROCKET) BACKG))

(check-expect
 (render -2)
 (place-image (text "-2" 20 "red") 10 (* 3/4 W)
              (place-image ROCKET X-ROCKET (- H HALF-ROCKET) BACKG)))

(check-expect (render 50) (place-image ROCKET X-ROCKET (- 50 HALF-ROCKET) BACKG))

(check-expect
 (render H)
 (place-image ROCKET X-ROCKET (- H HALF-ROCKET) BACKG))

(define (render lr)
  (cond
    [(string? lr) (place-rocket H)]
    [(<= -3 lr -1)
     (place-image (text "-2" 20 "red") 10 (* 3/4 W)
                  (place-rocket H))]
    [(>= lr 0) (place-rocket lr)]))

; place-rocket: Number -> Image
; place rocket to scenery at height h
(define (place-rocket h)
  (place-image ROCKET X-ROCKET (- h HALF-ROCKET) BACKG))

; launch: LRCD KeyEvent -> LRCD
; Start the countdown when spacebar is pressed

;(check-expect (launch "resting" "a") "resting")
;(check-expect (launch "resting" " ") -3)
;(check-expect (launch -2 " ") -2)
;(check-expect (launch 50 " ") 50)
;
(define (launch lr ke) lr)
  

; fly: LRCD -> LRCD
; raise the rocket by YDELTA if it is moving already
(define (fly lr) lr)

; main: LRCD -> LRCD
; initialize world with state s
(define (main s)
  (big-bang s
            [on-tick fly]
            [on-key launch]
            [to-draw render]))
