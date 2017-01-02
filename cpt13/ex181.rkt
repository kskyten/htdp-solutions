;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex181) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Constants
(define WORM-DIAMETER 5)
(define LHEIGHT 100)
(define LWIDTH 100)
(define HEIGHT (* LHEIGHT WORM-DIAMETER))
(define WIDTH (* LWIDTH WORM-DIAMETER))

; Graphical constants
(define WORM (circle (/ WORM-DIAMETER 2) "solid" "red"))
(define MT (empty-scene WIDTH HEIGHT))

(define-struct worm (x y dir))
; Worm is (make-worm Number Number Direction)
; interp. the logical position of the worm in
; units of worm diameter and the direction of movement

; example worms
(define w1 (make-worm 1 1 "down"))
(define wd (make-worm 2 3 "down"))
(define wu (make-worm 2 3 "up"))
(define wl (make-worm 2 3 "left"))
(define wr (make-worm 2 3 "right"))

; moving worms
(define wd2 (make-worm 2 4 "down"))
(define wu2 (make-worm 2 2 "up"))
(define wl2 (make-worm 1 3 "left"))
(define wr2 (make-worm 3 3 "right"))

; Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

; render: Worm -> Image
; Render the worm w

(check-expect (render w1)
              (place-image WORM
                           (* WORM-DIAMETER 1)
                           (* WORM-DIAMETER 1)
                           MT))

(define (render w)
  (place-image WORM
               (* WORM-DIAMETER (worm-x w))
               (* WORM-DIAMETER (worm-y w))
               MT))

; move: Worm KeyEvent -> Worm
; Change the direction the worm is going
; to with the keypad

(check-expect (move wd "up") wu)
(check-expect (move wu "down") wd)
(check-expect (move wd "left") wl)
(check-expect (move wd "right") wr)
(check-expect (move wd "down") wd)
(check-expect (move wd "r") wd)

(define (move w ke)
  (cond
    [(or (key=? ke "up")
         (key=? ke "down")
         (key=? ke "left")
         (key=? ke "right"))
     (make-worm (worm-x w) (worm-y w) ke)]
    [else w]))

; crawl: Worm -> Worm
; Move worm by WORM-DIAMETER for each tick

(check-expect (crawl wd) wd2)
(check-expect (crawl wu) wu2)
(check-expect (crawl wl) wl2)
(check-expect (crawl wr) wr2)

(define (crawl w)
  (cond 
    [(string=? "up" (worm-dir w)) (make-worm
                                   (worm-x w)
                                   (- (worm-y w) 1)
                                   (worm-dir w))]
    [(string=? "down" (worm-dir w)) (make-worm
                                   (worm-x w)
                                   (+ (worm-y w) 1)
                                   (worm-dir w))]
    [(string=? "left" (worm-dir w)) (make-worm
                                   (- (worm-x w) 1)
                                   (worm-y w)
                                   (worm-dir w))]
    [(string=? "right" (worm-dir w)) (make-worm
                                   (+ (worm-x w) 1)
                                   (worm-y w)
                                   (worm-dir w))]
    ))

; main: Worm -> Worm
; initialize world and start game
(define (main w) ; functions with no args?
  (big-bang (make-worm (/ LWIDTH 2) (/ LHEIGHT 2) "up")
   [on-tick crawl 1/3]
   [on-key move]
   [to-draw render]))