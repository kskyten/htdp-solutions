;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2812) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Node is a Symbol
; A Neigbourlist is (cons Node [List-of Node])
; A Graph is [List-of Neighbourlist]]

(define Graph 
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

; neighbours: Node Graph -> [Node]
; Get a list of the neighbours of node n in graoh g
; ASSUMPTION: Nodes are unique

(check-error (neighbours 'Y Graph) "Node doesn't exist.")
(check-expect (neighbours 'G Graph) empty)
(check-expect (neighbours 'A Graph) '(B E))
(check-expect (neighbours 'B Graph) '(E F))

(define (neighbours n g)
  (local ((define results
           (filter (lambda (neighbourlist)
                     (symbol=? n (first neighbourlist))) g)))
    (cond
      [(empty? results) (error "Node doesn't exist.")]
      [else (second (first results))])))