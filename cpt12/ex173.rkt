;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex173) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) false]
    [else (or 
           (= (first alon) n)
           (search n (rest alon)))]))

; search-sorted: Number LoN -> Boolean
; determine if n occurs in a list of ascending numbers

(check-expect (search-sorted 2 empty)
              false)
(check-expect (search-sorted 2 (list 1 2 3 4))
              true)
(check-expect (search-sorted 2 (list 1 3 4))
              false)

(define (search-sorted n alon)
  (cond
    [(empty? alon) false]
    [else (if (< n (first alon))
              false
              (or (= n (first alon))
                  (search-sorted n (rest alon))))]
    ))