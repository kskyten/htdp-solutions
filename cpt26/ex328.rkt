;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; [List-of String] -> [List-of String]
; to pick a “random” non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))
 
; arrangements: [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of the given list of names

(check-expect (arrangements empty) (list empty))
(check-expect (arrangements '("foo" "bar"))
                            '(("foo" "bar")
                              ("bar" "foo")))

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


; [List-of X] -> X
; returns a random item from the list
; Assumption: the list is not empty
(define (random-pick l)
  (list-ref l (random (length l))))
 
; [List-of String] [List-of [List-of String]]
; ->
; [List-of [List-of String]]
; produces the list of those lists in ll that do not agree
; with names at any place

(check-expect (non-same '("Jane" "Sue") empty) empty)
(check-expect (non-same '("Jane" "Sue") '(("Jane" "Sue")
                                          ("Sue" "Jane")))
              '(("Sue" "Jane")))

(define (non-same names ll)
  (filter (lambda (permutation) (different-recipients? names permutation)) ll))

; different-recipients?: [List-of String] [List-of String] -> Boolean
; Are the lists of names different in each place?
; ASSUMPTION: The list are the of the same length

(check-expect (different-recipients? '("Jane" "Sue") '("Jane" "Sue")) false)
(check-expect (different-recipients? '("Jane" "Sue") '("Sue" "Jane")) true)

(define (different-recipients? names candidate)
  (cond
    [(empty? names) true]
    [else (if (string=? (first names) (first candidate))
              false
              (different-recipients? (rest names) (rest candidate)))]))