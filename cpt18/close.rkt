;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname close) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; [List-of Posn] -> Boolean
; Is any point in lop within 5 pixels of pt
(define (close? lop pt)
  (local (
          (define CLOSENESS 5)
          
          ; Posn -> Boolean
          ; is one shot close to pt?
          (define (is-one-close? p)
            (close-to p pt CLOSENESS))
          
          ; Posn Posn Number -> Boolean
          ; Is pt1 close to pt2
          (define (close-to pt1 pt2 eps)
            (<  (sqrt (+ (sqr (- (posn-x pt1) (posn-x pt2)))
                         (sqr (- (posn-y pt1) (posn-y pt2)))))
                eps))
          )    
    (ormap is-one-close? lop)))