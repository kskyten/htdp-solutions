;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex220) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; convert-euro: [List-of-Number] Number -> [List-of-Number]
; Convert a list of usd prices to eur prices with
; conversion rate C

(check-expect (convert-euro empty 1.22 ) empty)
(check-expect (convert-euro '(1 2 3) 1.22 )
              `(,(* 1.22 1)
                ,(* 1.22 2)
                ,(* 1.22 3)))

(define (convert-euro lon C)
  (map (lambda (x) (* C x))
       lon))

; translate: [List-of-Posn] -> [[Number Number]]
; Translate a list of Posns to number pairs

(check-expect (translate (list (make-posn 1 1)
                               (make-posn 2 3)
                               (make-posn 4 5)
                               (make-posn 6 7)))
              '((1 1)
                (2 3)
                (4 5)
                (6 7)))

(define (translate lop)
  (local
    (; Posn -> [Number Number]
     (define (translate-one p)
       (list (posn-x p) (posn-y p))))
    (map translate-one lop)))