;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex203) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define l1
  (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define l2
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; Compare all the number in l with
; comparison operator R
(define (cmp R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (cond
       [(R (first l) (cmp R (rest l)))
        (first l)]
       [else
        (cmp R (rest l))])]))

; inf-1: Nelon -> Number
; Determine the smallest number in the list l
(define (inf-1 l)
  (cmp < l))

; sup-1: Nelon -> Number
; Determine the largest number in the list l
(define (sup-1 l)
  (cmp > l))

; Nelon -> Number
; to determine the smallest or largest number in l
; according to the operator m {min, max}
(define (ult m l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (m (first l) (ult m (rest l)))]))

; inf-2: Nelon -> Number
; Determine the smallest number in the list l
(define (inf-2 l)
  (ult min l))

; sup-2: Nelon -> Number
; Determine the largest number in the list l
(define (sup-2 l)
  (ult max l))