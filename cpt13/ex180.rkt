;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex180) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
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
  (cond
    [(empty? w) (list empty)]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; insert-everywhere/in-all-words: 1String NELoW -> NELoW
; Insert chr to all positions in every word in alow

(check-expect (insert-everywhere/in-all-words "d" (list empty))
              '(("d")))
(check-expect (insert-everywhere/in-all-words "d" '(("e")))
              '(("d" "e")
                ("e" "d")))
(check-expect (insert-everywhere/in-all-words "d" '(("e" "r")
                                                    ("r" "e")))
              '(("d" "e" "r")
                ("e" "d" "r")
                ("e" "r" "d")
                ("d" "r" "e")
                ("r" "d" "e")
                ("r" "e" "d")))

(define (insert-everywhere/in-all-words chr alow)
  (cond
    [(empty? (rest alow)) (insert-everywhere/in-word chr (first alow))]
    [else (append (insert-everywhere/in-word chr (first alow)) ; LoW
                  (insert-everywhere/in-all-words chr (rest alow))) ; LoW 
          ]))

; insert-everywhere/in-word: 1String Word -> List-of-words
; Insert char to all positions in word

(check-expect (insert-everywhere/in-word "d" empty)
              '(("d")))
(check-expect (insert-everywhere/in-word "d" '("e"))
              '(("d" "e")
                ("e" "d")))
(check-expect (insert-everywhere/in-word "d" '("e" "r"))
              '(("d" "e" "r")
                ("e" "d" "r")
                ("e" "r" "d")))

(define (insert-everywhere/in-word char word)
  (cond 
    [(empty? word) `((,char))]
    [else (cons (cons char word)
                (append-to-all (first word)
                               (insert-everywhere/in-word char (rest word))))]))

; append-to-all: 1String NELoW -> NELoW
; Append char to all words in alow

(check-expect (append-to-all "d" '(("e")))
              '(("d" "e")))
(check-expect (append-to-all "e" '(("r" "d")
                                   ("d" "r")))
              '(("e" "r" "d")
                ("e" "d" "r")))

(define (append-to-all char alow)
  (cond 
    [(empty? (rest alow)) (list (cons char (first alow)))]
    [else (cons (cons char (first alow))
                (append-to-all char (rest alow)))]))


; arrange-main: String -> LoS
; Produce a list of all permutations of str

(check-expect (arrange-main "hi")
              '("hi" "ih"))

(define (arrange-main str)
  (map implode
       (arrangements (explode str))))