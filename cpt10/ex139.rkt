;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex139) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
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

; riot: N -> Image
; Produce an image of the student riot with n balls

(check-expect (riot 0) SEATS)
(check-expect (riot 1) (place-image PAINTBALL 50 50 SEATS))
(check-expect (riot 3) (place-image PAINTBALL 50 50
                                    (place-image PAINTBALL 30 70
                                                 (place-image PAINTBALL 10 100 SEATS))))

(define (riot n)
  (cond
    [(zero? n) SEATS]
    [(positive? n) (place-image PAINTBALL
                                (random 80)
                                (random 180)
                                (riot (sub1 n)))]))