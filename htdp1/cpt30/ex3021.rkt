;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex3021) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Node is a symbol
; A Pair is a list of two nodes
; A SimpleGraph is a list of pairs

(define SimpleG 
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

;; route-exists2? : node node simple-graph  ->  boolean
;; to determine whether there is a route from orig to dest in sg

(check-expect (route-exists2? 'A 'A SimpleG) true)
(check-expect (route-exists2? 'A 'F SimpleG) false)

(define (route-exists2? orig dest sg)
  (local ((define (re-accu? orig dest sg accu-seen)
            (cond
              [(symbol=? orig dest) true]
              [(member? orig accu-seen) false]
              [else
               (re-accu? (neighbor orig sg) dest sg (cons orig accu-seen))]))) 
    (re-accu? orig dest sg empty)))

;; neighbor : node simple-graph  ->  node
;; to determine the node that is connected to a-node in sg

(check-error (neighbor 'A empty) "neighbor: impossible")
(check-expect (neighbor 'A SimpleG) 'B)
(check-expect (neighbor 'F SimpleG) 'F)

(define (neighbor a-node sg)
  (cond
    [(empty? sg) (error "neighbor: impossible")]
    [else (cond
	    [(symbol=? (first (first sg)) a-node)
	     (second (first sg))]
	    [else (neighbor a-node (rest sg))])]))