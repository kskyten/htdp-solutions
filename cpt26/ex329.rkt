;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex329) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; DNAPrefix?: [Symbol] [Symbol] -> Boolean
; Is the pattern a prefix of the search string?

(check-expect (DNAPrefix? '() '()) true)
(check-expect (DNAPrefix? '(a) '()) false)
(check-expect (DNAPrefix? '() '(a g c)) true)
(check-expect (DNAPrefix? '(a) '(a g c t)) true)
(check-expect (DNAPrefix? '(a g c) '(a g c t)) true)
(check-expect (DNAPrefix? '(a g c) '(t a g c t)) false)

(define (DNAPrefix? pattern search-str)
  (cond
    [(empty? pattern) true]
    [(empty? search-str) false]
    [else
     (if (not (symbol=? (first pattern) (first search-str)))
         false
         (DNAPrefix? (rest pattern) (rest search-str)))]))


; DNADelta: [Symbol] [Symbol] -> Symbol
; Return the first symbol after a matching pattern in search string.
; If the lists are identical signal an error.

(check-error (DNADelta '() '()) "DNA's are identical")
(check-expect (DNADelta '(a) '()) false)
(check-expect (DNADelta '() '(a g c)) 'a)
(check-expect (DNADelta '(a) '(a g c)) 'g)
(check-expect (DNADelta '(a g c) '(a g c t)) 't)
(check-expect (DNADelta '(a g c) '(t a g c t)) false)
(check-error (DNADelta '(a g c) '(a g c)) "DNA's are identical")

(define (DNADelta pattern search-str)
   (cond
    [(and (empty? pattern) (empty? search-str))
     (error "DNA's are identical")]
    [(and (empty? pattern) (cons? search-str)) (first search-str)]
    [(empty? search-str) false]
    [else
      (if (not (symbol=? (first pattern) (first search-str)))
         false
         (DNADelta (rest pattern) (rest search-str)))]))