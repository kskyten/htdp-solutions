;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2715) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
(define p1 (make-posn 50 50))
(define p2 (make-posn 100 300))
(define p3 (make-posn 250 100))

(define THRESHOLD 2)
(define COLOR 'red)

; bezier: Posn Posn Posn -> true
; Draw a bezier-curve between p1 and p3 with
; p2 as the controlling point
(define (bezier p1 p2 p3)
  (local (
          (define q (mid-point p1 p2))
          (define r (mid-point p2 p3))
          (define s (mid-point q r))
          )
    (cond
      [(too-small? p1 p3)
       (and (draw-solid-line p1 s COLOR)
            (draw-solid-line s p3 COLOR))]
      [else
       (and (bezier p1 q s)
            (bezier s r p3))])))

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

;; too-small?: posn posn posn -> Boolean
;; Is the hypotenuse of the triangle too small to draw?

(check-expect (too-small? p1 p3) false)
(check-expect (too-small? (make-posn 0 0)
                          (make-posn 1 1)) true)

(define (too-small? p1 p3)
  (< (sqrt (+ (sqr (- (posn-x p1) (posn-x p3)))
              (sqr (-  (posn-y p1) (posn-y p3)))))
     THRESHOLD))

(start 300 200)
(bezier p1 p2 p3)