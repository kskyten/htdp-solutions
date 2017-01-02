;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex54) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
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
     (place-image (text (number->string lr) 20 "red") 10 (* 3/4 W)
                  (place-rocket H))]
    [(>= lr 0) (place-rocket lr)]))

; place-rocket: Number -> Image
; place rocket to scenery at height h
(define (place-rocket h)
  (place-image ROCKET X-ROCKET (- h HALF-ROCKET) BACKG))

; launch: LRCD KeyEvent -> LRCD
; Start the countdown when spacebar is pressed

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 "a") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch lr ke)
  (cond
    [(string? lr) (if (string=? " " ke) -3 lr)]
    [(<= -3 lr -1) lr]
    [(>= lr 0) lr]))
  
; fly: LRCD -> LRCD
; raise the rocket by YDELTA if it is moving already

(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) H)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly lr)
  (cond
    [(string? lr) lr]
    [(<= -3 lr -1) (if (= lr -1) H (+ lr 1))]
    [(>= lr 0) (- lr YDELTA)]))

; end?: LRCD -> Boolean
; Stop animation when rocket is out of sight

(check-expect (end? "resting") false)
(check-expect (end? -2) false)
(check-expect (end? 0) true)

(define (end? lr)
  (and 
   (number? lr)
   (= lr 0)))

; main: LRCD -> LRCD
; initialize world with state s
(define (main s)
  (big-bang s
            [on-tick fly]
            [on-key launch]
            [to-draw render]
            [stop-when end?]))
