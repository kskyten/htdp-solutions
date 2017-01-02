;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex86) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define R 2)

; Location is one of:
; – Posn
; – Number
; interp. Posn are positions on the Cartesian plane,
; Numbers are positions on either the x or the y axis.

; in-reach?: Location -> Boolean
; Is the given location l within distance R of the origin?

(check-expect (in-reach? 1) true)
(check-expect (in-reach? -5) false)
(check-expect (in-reach? (make-posn 0 1)) true)
(check-expect (in-reach? (make-posn -1 0)) true)
(check-expect (in-reach? (make-posn 1 5)) false)

(define (in-reach? l)
  (cond
    [(posn? l) (< (distance-to-0 l) R)]
    [(number? l) (< (abs l) R)]
    ))

; distance-to-0: Posn -> Number
; Compute the euclidean distance to the origin from pos

(check-expect (distance-to-0 (make-posn 0 2)) 2)
(check-expect (distance-to-0 (make-posn -5 0)) 5)

(define (distance-to-0 pos)
  (sqrt
   (+ (sqr (posn-x pos))
      (sqr (posn-y pos)))))