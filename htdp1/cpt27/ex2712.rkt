;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2712) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
(require htdp/draw)

(define CENTER (make-posn 200 200))
(define RADIUS 200)

(define THRESHOLD 5)
(define COLOR 'green)

;; sierpinski : posn posn posn  ->  true
;; to draw a Sierpinski triangle down at a, b, and c,
;; assuming it is large enough
(define (sierpinski a b c)
  (cond
    [(too-small? a b c) true]
    [else 
     (local ((define a-b (mid-point a b))
             (define b-c (mid-point b c))
             (define c-a (mid-point a c)))
       (and
        (draw-triangle a b c)	    
        (sierpinski a a-b c-a)
        (sierpinski b a-b b-c)
        (sierpinski c c-a b-c)))]))

;; mid-point : posn posn  ->  posn
;; to compute the mid-point between a-posn and b-posn
(define (mid-point a-posn b-posn)
  (make-posn
   (mid (posn-x a-posn) (posn-x b-posn))
   (mid (posn-y a-posn) (posn-y b-posn))))

;; mid : number number  ->  number
;; to compute the average of x and y
(define (mid x y)
  (/ (+ x y) 2))

;; draw-triangle: posn posn posn -> true
;; Draw a triangle
(define (draw-triangle a b c)
  (and (draw-solid-line a b COLOR)
       (draw-solid-line b c COLOR)
       (draw-solid-line c a COLOR)))

;; too-small?: posn posn posn -> Boolean
;; Is the triangle too small to draw?

(check-expect (too-small? A B C) false)
(check-expect (too-small? (make-posn 1 1)
                          (make-posn 3 0)
                          (make-posn 3 3)) true)

(define (too-small? a b c)
  (< (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
              (sqr (-  (posn-y a) (posn-y b)))))
     THRESHOLD))

;; circle-pt: Number -> Posn
;; Compute a posn on the circle with CENTER and RADIUS
;; for a given angle.

(check-within (posn-y (circle-pt 360/360)) 200 1)
(check-within (posn-x (circle-pt 360/360)) 400 1)

(define (circle-pt factor)
  (local (
          (define angle (* 2 pi factor))
          )
  (make-posn (+ (posn-x CENTER) (* RADIUS (cos angle)))
             (- (posn-y CENTER) (* RADIUS (sin angle))))))

(define A (circle-pt 90/360))
(define B (circle-pt 210/360))
(define C (circle-pt 330/360))

(start 400 400)
(sierpinski A B C)