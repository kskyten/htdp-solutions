;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex142) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")))))
; physical constants
(define HEIGHT 80)
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))
 
; A ShotWorld is List-of-numbers.
; interp.: the collection of shots fired and moving straight up
 
; ShotWorld -> ShotWorld
; move each shot up by one pixel

(check-expect (tock empty) empty)
(check-expect (tock (cons 50 (cons 20 (cons 10 empty))))
              (cons 49 (cons 19 (cons 9 empty))))

(define (tock w)
  (cond
    [(empty? w) empty]
    [else (cons (sub1 (first w)) (tock (rest w)))]))
 
; ShotWorld KeyEvent -> ShotWorld
; add a shot to the world if the space bar was hit

(check-expect (keyh empty " ") (cons HEIGHT empty))
(check-expect (keyh empty "a") empty)
(check-expect (keyh (cons 10 empty) " ")
              (cons HEIGHT (cons 10 empty)))

(define (keyh w ke)
  (cond
    [(key=? ke " ") (cons HEIGHT w)]
    [else w]))
 
; ShotWorld -> Image
; add each shot y on w at (MID,y) to the background image

(check-expect (to-image empty) BACKGROUND)
(check-expect (to-image (cons 10 empty))
                        (place-image SHOT
                                     XSHOTS
                                     10
                                     BACKGROUND))

(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w) (to-image (rest w)))]))
 
; ShotWorld -> ShotWorld
(define (main w0)
  (big-bang w0
            (on-tick tock)
            (on-key keyh)
            (to-draw to-image)))