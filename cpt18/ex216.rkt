;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex216) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

(define MT (empty-scene 50 50))

; Polygon -> Image
; add the Polygon p into an image in MT
(define (render-polygon p)
  (local 
    (; A NELoP is one of:
     ; – (cons Posn empty)
     ; – (cons Posn NELoP)
     
     ; NELoP -> Image
     ; connect the Posns in p
     (define (connect-dots p)
       (cond
         [(empty? (rest p)) MT]
         [else
          (render-line
           (connect-dots (rest p)) (first p) (second p))]))
     
     ; NELoP -> Posn
     ; extract the last Posn from p
     (define (last p)
       (cond
         [(empty? (rest (rest (rest p)))) (third p)]
         [else (last (rest p))])))
    (render-line (connect-dots p) (first p) (last p))))

; Image Posn Posn -> Image
; draw a red line from Posn p to Posn q into im
(define (render-line im p q)
  (add-line
   im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))

