;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo-interval) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)
 
; constants:
(define WIDTH 200)
(define HEIGHT 300)
(define CLOSE (/ HEIGHT 3))

; A WorldState falls into one of three intervals:
; – between 0 and CLOSE
; – between CLOSE and HEIGHT
; – below HEIGHT
 
; visual constants:
(define MT (empty-scene WIDTH HEIGHT))
(define UFO
  (overlay (circle 10 "solid" "green")
           (rectangle 40 2 "solid" "green")))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
            [on-tick nxt]
            [to-draw render/status]))
 
; WorldState -> WorldState
; compute next location of UFO
 
(check-expect (nxt 11) 14)
 
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; place UFO at given height into the center of MT
 
(check-expect
  (render 11) (place-image UFO (/ WIDTH 2) 11 MT))
 
(define (render y)
  (place-image UFO (/ WIDTH 2) y MT))

; WorldState -> Image
; add a status line to the scene create by render  
 
(check-expect (render/status 10)
              (place-image (text "descending" 11 "green")
                           30 10
                           (render 10)))
 
(define (render/status y)
  (place-image
   (cond
     [(<= 0 y CLOSE)
      (text "descending" 11 "green")]
    [(and (< CLOSE y) (<= y HEIGHT))
     (text "closing in" 11 "orange")]
    [(> y HEIGHT)
     (text "landed" 11 "red")])
    30 10
    (render y)))