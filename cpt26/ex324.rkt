;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex324) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; merge: [Number] [Number] -> [Number]
; Merge two lists of numbers in ascending order
; ASSUMPTION: the two lists are sorted in ascending order

(check-expect (merge empty empty) empty)
(check-expect (merge '(1 2 3) empty) '(1 2 3))
(check-expect (merge empty '(5 6 7)) '(5 6 7))
(check-expect (merge '(2 3 3 5) '(2 5 7 7)) '(2 2 3 3 5 5 7 7))

(define (merge lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) empty]
    [(and (cons? lon1) (empty? lon2)) lon1]
    [(and (empty? lon1) (cons? lon2)) lon2]
    [else (if (<= (first lon1) (first lon2))
              (cons (first lon1) (merge (rest lon1) lon2))
              (cons (first lon2) (merge lon1 (rest lon2))))]))