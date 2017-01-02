;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex83) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define WIDTH 300)
(define HEIGHT 300)

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define UFO (overlay (rectangle 30 3 "solid" "green")
                     (circle 5 "solid" "green")))
(define TANK (rectangle 20 5 "solid" "blue"))
(define MISSILE (circle 3 "solid" "red"))