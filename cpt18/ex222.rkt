;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex222) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct toy (desc aprice sprice))
; A Toy is (make-toy String Number Number)
; The desciption, acquisition price and sales price
; of the toy

(define t0 (make-toy "Teddy bear" 0.5 5))
(define lt (list (make-toy "Teddy bear" 0.5 5)
                 (make-toy "Barbie" 0.5 50)
                 (make-toy "Action man" 2 10)))

; eliminate-exp: Number [Toy] -> [Toy]
; Filter all toys that are more expensive than ua

(check-expect (eliminate-exp 6 lt)
              (list (make-toy "Teddy bear" 0.5 5)))

(define (eliminate-exp ua lot)
  (local (
          ; exp?: Toy -> Boolean
          (define (exp? t)
            (< (toy-sprice t) ua))
          )
    (filter exp? lot)))

; recall: String [Toy] ->  [Toy]
; Get the toys that aren't named ty

(check-expect (recall "Barbie" lt)
              (list (make-toy "Teddy bear" 0.5 5)
                    (make-toy "Action man" 2 10)))

(define (recall ty lot)
  (local (; not-eq?: Toy -> Boolean
          (define (not-eq? t)
            (not (string=? ty (toy-desc t))))
          )
          (filter not-eq? lot)))

; selection: [Toy] [Toy] -> [Toy]
; Select all the toys from lst2 that are in lst1

(check-expect (selection empty lt) empty)
(check-expect (selection lt empty) empty)
(check-expect (selection lt lt) lt)
(check-expect (selection (list t0) lt) (list t0))

(define (selection lst1 lst2)
  (local (; in-lst1?: Toy -> Boolean
          (define (in-lst1? t)
            (member? t lst1))
          )
    (filter in-lst1? lst2)))