;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex217) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; A Word is either
; – empty or
; – (cons 1String Word)

(define w0 '("h" "i"))
(define w1 '("h" "e" "l" "l" "o"))

; List-of-words is either
; - empty
; - (cons Word List-of-words)

(define lw0 '(("h" "i")
              ("i" "h")))

; arrangements: Word -> List-of-words
; Get all permutations of the letters in w

(check-expect (arrangements empty) (list empty))
(check-expect (arrangements w0) lw0)

(define (arrangements w)
  (local 
    (; insert-everywhere/in-all-words: 1String NELoW -> NELoW
     ; Insert chr to all positions in every word in alow
     (define (insert-everywhere/in-all-words chr alow)
       (cond
         [(empty? (rest alow)) (insert-everywhere/in-word chr (first alow))]
         [else (append (insert-everywhere/in-word chr (first alow)) ; LoW
                       (insert-everywhere/in-all-words chr (rest alow))) ; LoW 
               ]))
     
     ; insert-everywhere/in-word: 1String Word -> List-of-words
     ; Insert char to all positions in word     
     (define (insert-everywhere/in-word char word)
       (cond 
         [(empty? word) `((,char))]
         [else (cons (cons char word)
                     (append-to-all (first word)
                                    (insert-everywhere/in-word char (rest word))))]))
     
     ; append-to-all: 1String NELoW -> NELoW
     ; Append char to all words in alow
     (define (append-to-all char alow)
       (map (lambda (x) (cons char x)) alow)))
    
    (foldr insert-everywhere/in-all-words (list empty) w)))

; arrange-main: String -> LoS
; Produce a list of all permutations of str

(check-expect (arrange-main "hi")
              '("hi" "ih"))

(define (arrange-main str)
  (map implode
       (arrangements (explode str))))