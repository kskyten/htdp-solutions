;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex45) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define MAX 100)
(define DELTAH -0.1)
(define H-INCR1 1/5)
(define H-INCR2 1/3)

(define WIDTH MAX)
(define HEIGHT 20)
(define BAR-HEIGHT HEIGHT)

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define BAR-COLOR "red")

; H is a Number in [0,MAX]
; interp. The level of happiness

; render: H -> Image
; Render the happiness level h

(check-expect (render 100)
              (overlay (rectangle 100 BAR-HEIGHT "solid" BAR-COLOR)
                       MT))
(check-expect (render 50)
              (overlay/align "left" "middle" 
               (rectangle 50 BAR-HEIGHT "solid" BAR-COLOR)
                       MT))

(define (render h)
  (overlay/align "left" "middle"
   (rectangle h BAR-HEIGHT "solid" BAR-COLOR)
                       MT))

; increase-happiness: H KeyEvent -> H
; Increase the happiness level:
; - by H-INCR1 if down arrow is pressed
; - by H-INCR2 if up arrow is pressed

(check-expect (increase-happiness 50 "down") (+ 50 H-INCR1))
(check-expect (increase-happiness 50 "up") (+ 50 H-INCR2))
(check-expect (increase-happiness 50 "r") 50)

(define (increase-happiness h ke)
  (cond
    [(key=? ke "down") (+ h H-INCR1)]
    [(key=? ke "up") (+ h H-INCR2)]
    [else h]
    ))

; decrease-happiness: H -> H
; Decrase happiness level by DELTAH per tick

(check-expect (decrease-happiness 30) (+ 30 DELTAH))

(define (decrease-happiness h)
  (+ h DELTAH))

; main: H -> H
; Initialize happiness and start program
(define (main h)
  (big-bang h
            [on-tick decrease-happiness]
            [on-key increase-happiness]
            [to-draw render]
            [stop-when zero?]))