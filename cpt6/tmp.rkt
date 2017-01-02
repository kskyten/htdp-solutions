;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tmp) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define WIDTH 300)
(define HEIGHT 300)
(define TANK-HEIGHT 5)
(define Y0 5)
(define Y-LAUNCH (+ Y0 TANK-HEIGHT)) 

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define UFO (overlay (rectangle 30 3 "solid" "green")
                     (circle 5 "solid" "green")))
(define TANK (rectangle 20 TANK-HEIGHT "solid" "blue"))
(define MISSILE (circle 3 "solid" "red"))

; A UFO is Posn.
; interp. (make-posn x y) is the UFO's current location
 
(define-struct tank (loc vel))
; A Tank is (make-tank Number Number).
; interp. (make-tank x dx) means the tank is at (x ,HEIGHT)
;   and that it moves dx pixels per clock tick
 
; A Missile is Posn.
; interp. (make-posn x y) is the missile's current location 

; A SIGS (short for “space invader game state”) is one of:
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)