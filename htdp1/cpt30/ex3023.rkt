;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex3023) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Node is a number
; A SimpleGraph is a vector of Nodes
; where the index i contains the neighbor of the node i

(define SimpleG
  (vector 
   1
   2
   4
   4
   1
   5))

;; route-exists2? : node node graph  ->  boolean
;; to determine whether there is a route from orig to dest in sg

(check-expect (route-exists2? 1 1 SimpleG) true)
(check-expect (route-exists2? 0 1 SimpleG) true)
(check-expect (route-exists2? 1 5 SimpleG) false)

(define (route-exists2? orig dest sg)
  (local ((define (re-accu? orig accu-seen)
            (cond
              [(= orig dest) true]
              [(member? orig accu-seen) false]
              [else
               (re-accu? (vector-ref sg orig) (cons orig accu-seen))]))) 
    (re-accu? orig empty)))