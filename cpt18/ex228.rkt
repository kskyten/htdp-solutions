;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex228) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Constants
(define SPEED 1/10)
(define WORM-DIAMETER 5)
(define LHEIGHT 75)
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
(define FOOD (circle (/ WORM-DIAMETER 2) "solid" "green"))
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
(define fatbob (list (make-worms 10 10 "up")
                     (make-worms 10 11 "up")
                     (make-worms 9 11 "right")
                     (make-worms 9 11 "right")))

; wall-hitting worm
(define whworm (list (make-worms 3 0 "up")))

; self-hitting worm
(define shworm (list (make-worms 10 10 "up")
                     (make-worms 10 11 "up")
                     (make-worms 11 11 "left")
                     (make-worms 12 11 "left")
                     (make-worms 12 10 "down")
                     (make-worms 11 10 "right")
                     (make-worms 10 10 "right")))

(define-struct sg (worm food))
; SnakeGame SG is (make-sg Worm Posn)
; interp. The state of the worm and
; the location of food in logical coordinates


; example SG
(define bg (make-sg bob (make-posn 5 5)))

; render-sg: SG -> Image
; Render the game state

(check-expect (render-sg (make-sg bob (make-posn 5 5)))
              (place-image FOOD (lc 5) (lc 5) (place-image WORM (lc 10) (lc 10)
                                                           (place-image WORM (lc 10) (lc 11)
                                                                        (place-image WORM (lc 9) (lc 11) MT)))))

(define (render-sg s)
  (local (; render: Worm -> Image
          ; Render a segmented worm w
          (define (render worm)
            (foldr (lambda (segment bg)
                     (place-image WORM
                                  (lc (worms-x segment))
                                  (lc (worms-y segment))
                                  bg))
                   MT
                   worm))
          )
    (place-image FOOD
                 (lc (posn-x (sg-food s)))
                 (lc (posn-y (sg-food s)))
                 (render (sg-worm s)))))

; move-sg: SG KeyEvent -> SG
; Change the direction of the worm

(check-expect (move-sg bg "right")
              (make-sg (list (make-worms 10 10 "right")
                             (make-worms 10 11 "up")
                             (make-worms 9 11 "right"))
                       (make-posn 5 5)))

(define (move-sg s ke)
  (local (; moves: WS KeyEvent -> WS
          ; Change the direction the worm segment is going
          ; to with the keypad
          (define (moves w ke)
            (cond
              [(or (key=? ke "up")
                   (key=? ke "down")
                   (key=? ke "left")
                   (key=? ke "right"))
               (make-worms (worms-x w) (worms-y w) ke)]
              [else w]))
          
          ; move: Worm KeyEvent -> Worm
          ; Change the direction of the worm
          (define (move w ke)
            (cons (moves (first w) ke) (rest w)))
          )
    (make-sg (move (sg-worm s) ke)
             (sg-food s))))

; crawl-sg: SG -> SG
; Move the worm, manage the eating process and create more food

(check-expect (crawl-sg (make-sg bob (make-posn 1 1)))
              (make-sg bob2 (make-posn 1 1)))

(define (crawl-sg s)
  (local (; crawls: WS -> WS
          ; Move worm segment by WORM-DIAMETER for each tick
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
          
          ; crawl: Worm -> Worm
          ; Move the worm to the direction it's heading
          (define (crawl w)
            (add-first (remove-last w)))
          
          ; add-first: Worm -> Worm
          ; Add a worm segment to the worm in the direction of movement
          (define (add-first w)
            (cons (crawls (first w)) w))
          
          ; remove-last: Worm -> Worm
          ; Remove the last worm segment
          (define (remove-last w)
            (cond
              [(empty? (rest w)) empty]
              [else (cons (first w)
                          (remove-last (rest w)))]))
          
          ; crawl-worm: SG -> SG
          ; Move the worm
          (define (crawl-worm s)
            (make-sg (crawl (sg-worm s))
                     (sg-food s)))
          
          )
    (cond 
      [(eat? s) (eat s)]
      [else (crawl-worm s)])))

; lc: Number -> Number
; Convert coordinates to logical coordinates measured in
; worm diameter
(check-expect (lc 20) (* WORM-DIAMETER 20))

(define (lc n)
  (* WORM-DIAMETER n))

; wall-sg?: SG -> Boolean
; Has the worm hit the wall?

(check-expect (wall-sg? (make-sg
                         whworm
                         (make-posn 19 19)))
              true)

(define (wall-sg? s)
  (local (; walls?: WS -> Boolean
          ; Will the worm segment w hit a wall on the next move?
          (define (walls? w)
            (or (>= 1 (worms-x w))
                (>= 1 (worms-y w))
                (<= (- LWIDTH 1) (worms-x w))
                (<= (- LHEIGHT 1) (worms-y w))))
          
          ; wall?: Worm -> Boolean
          ; Has the worm hit the wall?
          (define (wall? w)
            (walls? (first w)))
          
          )
    (wall? (sg-worm s))))

; hitself-sg?: SG -> Boolean
; Has the worm ran into itself?

(check-expect (end-sg? (make-sg
                        shworm
                        (make-posn 19 19)))
              true)

(define (hitself-sg? s)
  (local (; hitself?: Worm -> Boolean
          ; Has the worm ran into itself?
          (define (hitself? w)
            (member? (worms-pos (first w))
                     (map worms-pos (rest w))))
          )
    (hitself? (sg-worm s))))

; worms-pos: Worm -> Posn
; Get the logical position of the worm

(check-expect (worms-pos w1)
              (make-posn 1 1))

(define (worms-pos w)
  (make-posn (worms-x w) (worms-y w)))

; end-sg?: SG -> Boolean
; Game over?

(check-expect (end-sg? (make-sg
                        bob
                        (make-posn 19 19)))
              false)
(check-expect (end-sg? (make-sg
                        shworm
                        (make-posn 19 19)))
              true)
(check-expect (end-sg? (make-sg
                        whworm
                        (make-posn 19 19)))
              true)

(define (end-sg? s)
  (or
   (wall-sg? s)
   (hitself-sg? s)))

; game-over-sg: SG -> Image
; Render game over screen

(check-expect (game-over-sg (make-sg whworm (make-posn 19 19)))
              (place-image (text "You hit the wall! Game over." 12 "red")
                           TEXT-X
                           TEXT-Y
                           (render-sg (make-sg whworm (make-posn 19 19)))))
(check-expect (game-over-sg (make-sg shworm (make-posn 19 19)))
              (place-image (text "You ran into yourself! Game over." 12 "red")
                           TEXT-X
                           TEXT-Y
                           (render-sg (make-sg shworm (make-posn 19 19)))))

(define (game-over-sg s)
  (local (
          ; gotext: String -> Image
          ; Render the game over text
          (define (gotext str)
            (text str FONTSIZE TEXT-COLOR))
          )
    (place-image
     (cond
       [(wall-sg? s) (gotext WTEXT)]
       [(hitself-sg? s) (gotext HTEXT)])
     TEXT-X
     TEXT-Y
     (render-sg s))))

; eat?: SG -> Boolean
; Is the head of the worm on food

(check-expect (eat? (make-sg bob (make-posn 1 1)))
              false)
(check-expect (eat? (make-sg bob (make-posn 10 10)))
              true)

(define (eat? s)
  (equal? (wormhead-pos (sg-worm s))
          (sg-food s)))

; wormhead-pos: Worm -> Posn
; Get the logical position of the worms head

(check-expect (wormhead-pos bob)
              (make-posn 10 10))

(define (wormhead-pos w)
  (worms-pos (first w)))

; eat-proper: SG Posn -> SG
; Grow the worm and make more food (deterministic version)

(check-expect (eat-proper (make-sg bob (make-posn 10 10))
                          (make-posn 5 5))
              (make-sg fatbob (make-posn 5 5)))

(define (eat-proper s pos)
  (local (; grow-worm: Worm -> Worm
          ; Add a worm segment to the end of the worm
          (define (grow-worm w)
            (cond
              [(empty? (rest w)) (list (first w) (first w))]
              [else (cons (first w)
                          (grow-worm (rest w)))]))
          )
    (make-sg (grow-worm (sg-worm s)) pos)))

; eat: SG -> SG
; Grow the worm and make more food
(define (eat s)
  (eat-proper s (food-create (sg-food s))))

; Posn -> Posn
; Create food randomly within the box
(define (food-create p)
  (local (; Posn Posn -> Posn
          ; generative recursion
          ; If there's already food in the position create it somewhere else
          (define (food-check-create p candidate)
            (if (equal? p candidate) (food-create p) candidate))
          )
    (food-check-create p (make-posn (random LWIDTH) (random LHEIGHT)))))

; sg-length: SG -> N
; Get the length of the worm

(check-expect (sg-length bg) 3)

(define (sg-length s)
  (length (sg-worm s)))

; main: SG -> SG
; Initialize world and start game
(define (main s)
  (sg-length (big-bang s
                       [on-tick crawl-sg SPEED]
                       [on-key move-sg]
                       [to-draw render-sg]
                       [stop-when end-sg? game-over-sg])))