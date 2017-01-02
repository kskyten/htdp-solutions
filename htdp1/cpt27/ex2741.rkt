;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2741) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
(define TOLERANCE 0.0001)

;; newton : (number  ->  number) number  ->  number
;; to find a number r such that (< (abs (f r)) TOLERANCE)
(define (newton f r0)
  (cond
    [(<= (abs (f r0)) TOLERANCE) r0]
    [else (newton f (find-root-tangent f r0))]))

;; find-root-tangent : (number  ->  number) number  ->  number
;; to find the root of the tagent of f at r0
(define (find-root-tangent f r0)
  (local ((define fprime (d/dx f)))
    (- r0
       (/ (f r0)
          (fprime r0)))))

; d/dx: (number -> number) -> (number -> number)
; Determine the derivative of the function f
(define (d/dx f)
  (local ((define EPSILON 0.001))
    (lambda (x) (/ (- (f (+ x EPSILON))
                      (f x))
                   EPSILON))))

;; f : number  ->  number
(define (f x)
  (- (* x x) x 1.8))