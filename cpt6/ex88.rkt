;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex88) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define WIDTH 300)
(define HEIGHT 150)
(define TANK-HEIGHT 5)
(define Y0 (- HEIGHT TANK-HEIGHT))
(define Y-LAUNCH (- HEIGHT (+ Y0 TANK-HEIGHT)))
(define R 20)

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define BG MT)
(define UFO (overlay (rectangle 30 3 "solid" "green")
                     (circle 5 "solid" "green")))
(define TANK (rectangle 20 TANK-HEIGHT "solid" "blue"))
(define MISSILE (circle 3 "solid" "red"))

; example images
(define AIM (place-image
             UFO 50 20
             (place-image TANK 58 Y0 BG)))
(define FIRED1 (place-image
                UFO 50 20
                (place-image TANK 58 Y0
                             (place-image MISSILE 58 (- HEIGHT TANK-HEIGHT) BG))))
(define FIRED2 (place-image
                UFO 50 80 
                (place-image TANK 70 Y0
                             (place-image MISSILE 58 (- HEIGHT TANK-HEIGHT 62) BG))))

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

; si-render: SIGS -> Image
; Add TANK, UFO and possibly MISSILE to BG

(check-expect (si-render (make-aim
                          (make-posn 50 20)
                          (make-tank 58 -3)))
              AIM)
(check-expect (si-render (make-fired
                          (make-posn 50 20)
                          (make-tank 58 -3)
                          (make-posn 58 (- HEIGHT TANK-HEIGHT))))
              FIRED1)
(check-expect (si-render (make-fired
                          (make-posn 50 80)
                          (make-tank 70 -3)
                          (make-posn 58 (- HEIGHT TANK-HEIGHT 62))))
              FIRED2)

(define (si-render s)
  (cond
    [(aim? s) (render-ufo (aim-ufo s)
                          (render-tank (aim-tank s) BG))]
    [(fired? s)
     (render-ufo (fired-ufo s)
                 (render-tank (fired-tank s)
                              (render-missile (fired-missile s) BG)))]))

; render-ufo: UFO Image -> Image
; render the ufo u on scene s

(check-expect (render-ufo (make-posn 50 20) BG)
              (place-image UFO 50 20 BG))

(define (render-ufo u s)
  (place-image UFO (posn-x u) (posn-y u) s))

; render-tank: Tank Image -> Image
; render the tank t on scene s

(check-expect (render-tank (make-tank 58 -3) BG)
              (place-image TANK 58 Y0 BG))

(define (render-tank t s)
  (place-image TANK (tank-loc t) Y0 s))

; render-missile: Missile Image -> Image
; render the missile m on scene s

(check-expect (render-missile (make-posn 58 50) BG)
              (place-image MISSILE 58 50 BG))

(define (render-missile m s)
  (place-image MISSILE (posn-x m) (posn-y m) s))

; si-game-over?: SIGS -> Boolean
; Has the ufo landed or the missile hit the ufo

(check-expect (si-game-over? (make-aim
                              (make-posn 50 20)
                              (make-tank 58 -3)))
              false)
(check-expect (si-game-over? (make-fired
                              (make-posn 50 20)
                              (make-tank 58 -3)
                              (make-posn 58 70)))
              false)
(check-expect (si-game-over? (make-aim
                              (make-posn 50 HEIGHT)
                              (make-tank 58 -3)))
              true)
(check-expect (si-game-over? (make-fired
                              (make-posn 50 HEIGHT)
                              (make-tank 70 -3)
                              (make-posn 70 50)))
              true)
(check-expect (si-game-over? (make-fired
                              (make-posn 50 80)
                              (make-tank 70 -3)
                              (make-posn 58 (- HEIGHT TANK-HEIGHT 62))))
              true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (close-to? (aim-ufo s)
                (make-posn (posn-x (aim-ufo s)) HEIGHT))]
    [(fired? s) (or
                 (close-to? (fired-ufo s)
                            (make-posn (posn-x (fired-ufo s)) HEIGHT))
                 (close-to? (fired-ufo s) (fired-missile s)))]))

; close-to?: Posn Posn -> Boolean
; Is pos1 within R distance of pos2?

(check-expect (close-to? (make-posn 60 20)
                         (make-posn 80 50))
              false)
(check-expect (close-to? (make-posn 50 80)
                         (make-posn 58 (- HEIGHT TANK-HEIGHT 62)))
              true)

(define (close-to? pos1 pos2)
  (< (sqrt (+ 
            (sqr (- (posn-x pos1) (posn-x pos2)))
            (sqr (- (posn-y pos1) (posn-y pos2)))))
     R))

; si-render-final: SIGS -> Image
; Render the game over screen

(check-expect (si-render-final (make-fired
                                (make-posn 50 80)
                                (make-tank 70 -3)
                                (make-posn 58 (- HEIGHT TANK-HEIGHT 62))))
              (overlay/align "middle" "middle"
                             (text "GAME OVER" 20 "red")
                             FIRED2))


(define (si-render-final s)
  (overlay/align "middle" "middle"
                 (text "GAME OVER" 20 "red")
                 (si-render s)))