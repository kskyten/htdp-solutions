;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex172) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct gp (name score))
; A GamePlayer is a structure:
; – (make-gp String Number)
; interp. (make-gp p s) represents player p who scored
; a maximum of s points 

; example data
(define GP1 (make-gp "John" 1000))
(define GP2 (make-gp "James" 756))
(define GP3 (make-gp "Jim" 123))

; sort-dsc: LoG -> LoG
; Sort a list of players in a descending order by score

(check-expect (sort-dsc empty) empty)
(check-expect (sort-dsc (list GP3 GP2 GP1))
              (list GP1 GP2 GP3))

(define (sort-dsc alog)
  (foldr insert-dsc empty alog))

; insert-dsc: GamePlayer LoG -> LoG
; insert gp into a list of game players

(check-expect (insert-dsc GP1 empty) 
              (list GP1))

(check-expect (insert-dsc GP1 (list GP2 GP3))
              (list GP1 GP2 GP3))

(define (insert-dsc gp alog)
  (cond
    [(empty? alog) (cons gp empty)]
    [(cons? alog) (if (score>? gp (first alog))
                      (cons gp alog)
                      (cons (first alog) (insert-dsc gp (rest alog))))]))

; score>?: GamePlayer GamePlayer -> Boolean
; Does gp1 have a higher score than gp2?

(check-expect (score>? GP1 GP2) true)
(check-expect (score>? GP2 GP1) false)

(define (score>? gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))
