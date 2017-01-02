;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex182) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Constants
(define WORM-DIAMETER 5)
(define LHEIGHT 20)
(define LWIDTH 20)
(define HEIGHT (* LHEIGHT WORM-DIAMETER))
(define WIDTH (* LWIDTH WORM-DIAMETER))

; Text constants
(define GO-TEXT "You hit the wall! Game over.")
(define FONTSIZE 12)
(define TEXT-COLOR "red")
(define TEXT-X (/ WIDTH 2))
(define TEXT-Y (- HEIGHT 20))

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

; end?: Worm -> Boolean
; Has worm w hit a wall?

(check-expect (end? (make-worm 0 5 "left")) true)
(check-expect (end? (make-worm 3 0 "up")) true)
(check-expect (end? (make-worm 0 LHEIGHT "down")) true)
(check-expect (end? (make-worm 3 5 "left")) false)

(define (end? w)
  (or (>= 1 (worm-x w))
      (>= 1 (worm-y w))
      (<= (- LWIDTH 1) (worm-x w))
      (<= (- LHEIGHT 1) (worm-y w))))

; final: Worm -> Image
; Render game over screen

(check-expect (final (make-worm 1 5 "left"))
              (place-image (text "You hit the wall! Game over." 12 "red")
                           TEXT-X
                           TEXT-Y
                           (render (make-worm 1 5 "left"))))

(define (final w)
  (place-image (text GO-TEXT FONTSIZE TEXT-COLOR)
               TEXT-X
               TEXT-Y
               (render w)))

; main: Worm -> Worm
; initialize world and start game
(define (main w) ; functions with no args?
  (big-bang (make-worm (/ LWIDTH 2) (/ LHEIGHT 2) "up")
            [on-tick crawl]
            [on-key move]
            [to-draw render]
            [stop-when end? final]))