;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2732) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
(define TOLERANCE 0.001)

;; poly : number  ->  number
(define (poly x)
 (* (- x 2) (- x 4)))

;; find-root : (number  ->  number) number number  ->  number
;; to determine a number R such that f has a 
;; root between R and (+ R TOLERANCE) 
;; 
;; ASSUMPTION: f is continuous and monotonic

(check-within (find-root (lambda (x) (- x 5)) 4 6)
              5 (* 2 TOLERANCE))

(define (find-root f left right)
  (cond
    [(<= (- right left) TOLERANCE) left]
    [else 
      (local ((define mid (/ (+ left right) 2)))
	(cond
	  [(<= (f mid) 0 (f right)) 
           (find-root f mid right)]
	  [else 
           (find-root f left mid)]))]))
