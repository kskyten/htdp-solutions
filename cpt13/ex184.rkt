;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex184) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Constants
(define WORM-DIAMETER 5)
(define LHEIGHT 100)
(define LWIDTH 100)
(define HEIGHT (* LHEIGHT WORM-DIAMETER))
(define WIDTH (* LWIDTH WORM-DIAMETER))

; Text constants
(define WTEXT "You hit the wall! Game over.")
(define HTEXT "You ran into yourself! Game over.")
(define FONTSIZE 12)
(define TEXT-COLOR "red")
(define TEXT-X (/ WIDTH 2))
(define TEXT-Y (- HEIGHT 20))

; Graphical constants
(define WORM (circle (/ WORM-DIAMETER 2) "solid" "red"))
(define MT (empty-scene WIDTH HEIGHT))

; Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"

(define-struct worms (x y dir))
; Wormsegment WS is
; (make-worms Number Number Direction)
; interp. the logical position of the worm segment in
; units of worm diameter and the direction of movement

; example worms segments
(define w1 (make-worms 1 1 "down"))
(define wd (make-worms 2 3 "down"))
(define wu (make-worms 2 3 "up"))
(define wl (make-worms 2 3 "left"))
(define wr (make-worms 2 3 "right"))

; moving worm segments
(define wd2 (make-worms 2 4 "down"))
(define wu2 (make-worms 2 2 "up"))
(define wl2 (make-worms 1 3 "left"))
(define wr2 (make-worms 3 3 "right"))

; Worm is one of:
; - (cons WS empty)
; - (cons WS Worm)
; Worm is a non-empty list of connected worm segments

; example worm
(define bob (list (make-worms 10 10 "up")
                  (make-worms 10 11 "up")
                  (make-worms 9 11 "right")))
(define bob2 (list (make-worms 10 9 "up")
                   (make-worms 10 10 "up")
                   (make-worms 10 11 "up")))

; renders: WS -> Image
; Render the worm segment w

(check-expect (renders w1)
              (place-image WORM
                           (* WORM-DIAMETER 1)
                           (* WORM-DIAMETER 1)
                           MT))

(define (renders w)
  (place-image WORM
               (* WORM-DIAMETER (worms-x w))
               (* WORM-DIAMETER (worms-y w))
               MT))

; moves: WS KeyEvent -> WS
; Change the direction the worm segment is going
; to with the keypad

(check-expect (moves wd "up") wu)
(check-expect (moves wu "down") wd)
(check-expect (moves wd "left") wl)
(check-expect (moves wd "right") wr)
(check-expect (moves wd "down") wd)
(check-expect (moves wd "r") wd)

(define (moves w ke)
  (cond
    [(or (key=? ke "up")
         (key=? ke "down")
         (key=? ke "left")
         (key=? ke "right"))
     (make-worms (worms-x w) (worms-y w) ke)]
    [else w]))

; crawls: WS -> WS
; Move worm segment by WORM-DIAMETER for each tick

(check-expect (crawls wd) wd2)
(check-expect (crawls wu) wu2)
(check-expect (crawls wl) wl2)
(check-expect (crawls wr) wr2)

(define (crawls w)
  (cond 
    [(string=? "up" (worms-dir w)) (make-worms
                                    (worms-x w)
                                    (- (worms-y w) 1)
                                    (worms-dir w))]
    [(string=? "down" (worms-dir w)) (make-worms
                                      (worms-x w)
                                      (+ (worms-y w) 1)
                                      (worms-dir w))]
    [(string=? "left" (worms-dir w)) (make-worms
                                      (- (worms-x w) 1)
                                      (worms-y w)
                                      (worms-dir w))]
    [(string=? "right" (worms-dir w)) (make-worms
                                       (+ (worms-x w) 1)
                                       (worms-y w)
                                       (worms-dir w))]
    ))

; render: Worm -> Image
; Render a segmented worm w

(check-expect (render bob)
              (place-image WORM (lc 10) (lc 10)
                           (place-image WORM (lc 10) (lc 11)
                                        (place-image WORM (lc 9) (lc 11) MT))))

(define (render w)
  (cond
    [(empty? (rest w))
     (renders-bg (first w) MT)]
    [else (renders-bg (first w)
                      (render (rest w)))]))

; renders-bg: WS Image -> Image
; Render the wormsegment w on background bg

(check-expect (renders-bg w1 MT)
              (place-image WORM (lc 1) (lc 1) MT))

(define (renders-bg w bg)
  (place-image WORM
               (lc (worms-x w))
               (lc (worms-y w))
               bg))

; lc: Number -> Number
; Convert coordinates to logical coordinates measured in
; worm diameter
(check-expect (lc 20) (* WORM-DIAMETER 20))

(define (lc n)
  (* WORM-DIAMETER n))

; crawl: Worm -> Worm
; Move the worm to the direction it's heading

(check-expect (crawl bob) bob2)

(define (crawl w)
  (add-first (remove-last w)))

; add-first: Worm -> Worm
; Add a worm segment to the worm in the direction of movement

(check-expect (add-first (list (make-worms 10 10 "up")))
              (list (make-worms 10 9 "up")
                    (make-worms 10 10 "up")))

(define (add-first w)
  (cons (crawls (first w)) w))

; remove-last: Worm -> Worm
; Remove the last worm segment

(check-expect (remove-last (list (make-worms 10 10 "up")
                                 (make-worms 10 11 "up")))
              (list (make-worms 10 10 "up")))

(define (remove-last w)
  (cond
    [(empty? (rest w)) empty]
    [else (cons (first w)
                (remove-last (rest w)))]))

; move: Worm KeyEvent -> Worm
; Change the direction of the worm

(check-expect (move bob "right")
              (list (make-worms 10 10 "right")
                    (make-worms 10 11 "up")
                    (make-worms 9 11 "right")))

(define (move w ke)
  (cons (moves (first w) ke) (rest w)))

; walls?: WS -> Boolean
; Will the worm segment w hit a wall on the next move?

(check-expect (walls? (make-worms 0 5 "left")) true)
(check-expect (walls? (make-worms 3 0 "up")) true)
(check-expect (walls? (make-worms 0 LHEIGHT "down")) true)
(check-expect (walls? (make-worms 3 5 "left")) false)

(define (walls? w)
  (or (>= 1 (worms-x w))
      (>= 1 (worms-y w))
      (<= (- LWIDTH 1) (worms-x w))
      (<= (- LHEIGHT 1) (worms-y w))))

; wall-hitting worm
(define whworm (list (make-worms 3 0 "up")))

; wall?: Worm -> Boolean
; Has the worm hit the wall?

(check-expect (wall? whworm) true)
(check-expect (wall? bob) false)

(define (wall? w)
  (walls? (first w)))

; hitself?: Worm -> Boolean
; Has the worm ran into itself?

; self-hitting worm
(define shworm (list (make-worms 10 10 "up")
                     (make-worms 10 11 "up")
                     (make-worms 11 11 "left")
                     (make-worms 12 11 "left")
                     (make-worms 12 10 "down")
                     (make-worms 11 10 "right")
                     (make-worms 10 10 "right")))

(check-expect (hitself? bob) false)
(check-expect (hitself? shworm) true)

(define (hitself? w)
  (member? (worms-pos (first w))
           (map worms-pos (rest w))))

; worms-pos: Worm -> Posn
; Get the logical position of the worm

(check-expect (worms-pos w1)
              (make-posn 1 1))

(define (worms-pos w)
  (make-posn (worms-x w) (worms-y w)))

; end?: Worm -> Boolean
; Has the worm hit the wall or itself?

(check-expect (end? bob) false)
(check-expect (end? shworm) true)
(check-expect (end? whworm) true)

(define (end? w)
  (or
   (wall? w)
   (hitself? w)))

; game-over: Worm -> Image
; Render game over screen

(check-expect (game-over whworm)
              (place-image (text "You hit the wall! Game over." 12 "red")
                           TEXT-X
                           TEXT-Y
                           (render whworm)))
(check-expect (game-over shworm)
              (place-image (text "You ran into yourself! Game over." 12 "red")
                           TEXT-X
                           TEXT-Y
                           (render shworm)))

(define (game-over w)
  (place-image
   (text (cond
           [(wall? w) WTEXT]
           [(hitself? w) HTEXT])
         FONTSIZE
         TEXT-COLOR)
   TEXT-X
   TEXT-Y
   (render w)))

; main: Worm -> Worm
; initialize world and start game
(define (main w) ; functions with no args?
  (big-bang w
            [on-tick crawl]
            [on-key move]
            [to-draw render]
            [stop-when end? game-over]))