;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2612) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; make-singles: [Number] -> [[Number]]
; Construct a list of one number lists

(check-expect (make-singles '()) '())
(check-expect (make-singles '(1 2 3))
              '((1)
                (2)
                (3)))

(define (make-singles alon)
  (map list alon))

; merge-all-neighbours: [[Number]] -> [[Number]]
; Merges pairs of neighbouring lists.

(check-expect (merge-all-neighbours (list (list 2) (list 5) (list 9) (list 3)))
              (list (list 2 5) (list 3 9)))
(check-expect (merge-all-neighbours (list (list 2 5) (list 3 9)))
              (list (list 2 3 5 9)))
(check-expect (merge-all-neighbours (list (list 2) (list 5) (list 3)))
              (list (list 2 5) (list 3)))
(check-expect (merge-all-neighbours (list (list 2 5) (list 3)))
              (list (list 2 3 5)))

(define (merge-all-neighbours llon)
  (cond
    [(empty? llon) empty]
    [(empty? (rest llon)) llon]
    [else (cons (merge (first llon)
                       (second llon))
                (merge-all-neighbours (cddr llon)))]))

; merge: [Number] [Number] -> [Number]
; Merge two lists of numbers into an ascending list
; ASSUMPTION: The two lists are sorted in ascending order

(check-expect (merge '() '()) empty)
(check-expect (merge '(1) '(2 3))
              '(1 2 3))
(check-expect (merge '(1 5 6) '(2 3))
              '(1 2 3 5 6))

(define (merge lon1 lon2)
  (cond
    [(and (empty? lon1) (empty? lon2)) empty]
    [(and (empty? lon1) (cons? lon2)) lon2]
    [(and (cons? lon1) (empty? lon2)) lon1]
    [(and (cons? lon1) (cons? lon2))
     (if (< (first lon1) (first lon2))
         (cons (first lon1) (merge (rest lon1) lon2))
         (cons (first lon2) (merge lon1 (rest lon2))))]))

; merge-sort: [Number] -> [Number]
; Sort a list of numbers in ascending order

(check-expect (merge-sort '()) '())
(check-expect (merge-sort '(3 45 2 1))
              '(1 2 3 45))

(define (merge-sort lon)
  (local (; main: [[Number]] -> [Number]
          (define (main llon)
            (cond
              [(= 1 (length llon)) (first llon)]
              [else (main (merge-all-neighbours llon))]))
          )
    (cond
      [(empty? lon) empty]
      [else (main (make-singles lon))])))