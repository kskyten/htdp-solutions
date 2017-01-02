;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex145) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define NBALLS 50)
(define X-RANGE 80)
(define Y-RANGE 180)
(define sq (square 10 "outline" "black"))
(define PAINTBALL (circle 5 "solid" "red"))

; col: N Image -> Image
; Produce a column with n copies of im

(check-expect (col 1 sq) sq)
(check-expect (col 2 sq) (above sq sq))
(check-expect (col 4 sq) (above sq 
                                (above sq 
                                       (above sq sq))))

(define (col n im)
  (cond
    [(zero? (sub1 n)) im]
    [(positive? n) (above im (col (sub1 n) im))]
    [else (error "Expected a positive number, given: " n)]
    ))

; row: N Image -> Image
; Produce a row with n copies of im

(check-expect (row 1 sq) sq)
(check-expect (row 2 sq) (beside sq sq))
(check-expect (row 3 sq) (beside sq (beside sq sq)))

(define (row n im)
  (cond
    [(zero? (sub1 n)) im]
    [(positive? n) (beside im (row (sub1 n) im))]
    [else (error "Expected a positive number, given: " n)]
    ))

(define SEATS (col 18 (row 8 sq)))

; Shotworld is one of:
; - empty
; - (cons Posn Shotworld)
; interp. a list of thrown balls

; drop-balloon-proper: ShotWorld RandomPosn -> Shotworld
; Throw one paintball for each tick

(check-expect (drop-balloon-proper empty (make-posn 10 10))
              (cons (make-posn 10 10) empty))
(check-expect (drop-balloon-proper (cons (make-posn 10 10)
                                         (cons (make-posn 30 46) empty))
                                   (make-posn 67 89))
              (cons (make-posn 67 89)
                    (cons (make-posn 10 10)
                          (cons (make-posn 30 46) empty))))

(define (drop-balloon-proper s rnd-pos) 
  (cond
    [(empty? s) (cons rnd-pos s)]
    [else (cons rnd-pos s)]))

; drop-balloon: Shotworld -> Shotworld
; Throw one paintball for ech tick
(define (drop-balloon s)
  (drop-balloon-proper s (rand-posn X-RANGE Y-RANGE)))

; rand-posn: N N -> Posn
; Get a random posn

(check-within (posn-x (rand-posn X-RANGE Y-RANGE)) (/ X-RANGE 2) (/ X-RANGE 2))
(check-within (posn-y (rand-posn X-RANGE Y-RANGE)) (/ Y-RANGE 2) (/ Y-RANGE 2))

(define (rand-posn x y)
  (make-posn (random x)
             (random y)))

; to-image: Shotworld -> Image
; Render the student riot given s

(check-expect (to-image empty) SEATS)
(check-expect (to-image (cons (make-posn 10 10) empty))
              (place-image PAINTBALL 10 10 SEATS))

(check-expect (to-image (cons (make-posn 35 70)
                              (cons (make-posn 10 10) empty)))
              (place-image PAINTBALL 35 70
                           (place-image PAINTBALL 10 10 SEATS)))

(define (to-image s)
  (cond
    [(empty? s) SEATS]
    [else (place-image PAINTBALL
                       (posn-x (first s))
                       (posn-y (first s))
                       (to-image (rest s)))]))

; long-enough: Shotworld -> Boolean
; Stop the simulation when the balls run out

(check-expect (long-enough empty) false)
(check-expect (long-enough (cons (make-posn 10 10) empty)) false)

(define (long-enough s)
  (>= (length s) NBALLS))

; main: Number -> ShotWorld
; display the student riot at rate ticks per second
(define (main rate)
  (big-bang empty
            (on-tick drop-balloon rate)
            (stop-when long-enough)
            (to-draw to-image)))