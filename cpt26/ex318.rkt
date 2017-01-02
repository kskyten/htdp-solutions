;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex318) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; cross: [List-of Symbol] [List-of Number] -> [List-of [list Symbol Number]]
; Produce all ordered pairs of symbols and numbers

(check-expect (cross '(a b c) empty) empty)
(check-expect (cross empty '(1 2 3)) empty)
(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a) '(1 2)) '((a 1)
                                    (a 2)))
(check-expect (cross '(a b) '(1)) '((a 1)
                                    (b 1)))

(define (cross los lon)
  (local (; ordered-pairs: Symbol [List-of Number] -> [list Symbol Number]
          ; Create all ardered pairs from a symbol and a list of numbers
          (define (ordered-pairs sy lon)
            (foldr (lambda (number pairs)
                     (cons (list sy number) pairs))
                   empty
                   lon))
    )
    (foldr (lambda (symbol result)
             (append (ordered-pairs symbol lon) result))
           empty
           los)))