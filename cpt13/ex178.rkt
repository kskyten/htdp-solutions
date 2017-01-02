;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex178) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; A Word is either
; – empty or
; – (cons 1String Word)

(define w0 '("h" "i"))
(define w1 '("h" "e" "l" "l" "o"))

; List-of-words is either
; - empty
; - (cons Word List-of-words)

(define lw0 '(("h" "i")
              ("i" "h")))

; arrangements: Word -> List-of-words
; Get all permutations of the letters in w

(check-expect (arrangements empty) (list empty))
(check-expect (arrangements w0) lw0)

(define (arrangements w)
  (cond
    [(empty? w) (list empty)]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))