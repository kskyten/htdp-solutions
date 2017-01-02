;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex157) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")))))
; List-of-strings (LoS) is one of:
; - empty
; - (cons String LoS)

; List-of-list-of-strings (LLS) is one of:
; - empty
; - (cons LoS LLoS)

; example data
(define l0 (list "This" "is" "a" "line"))
(define l1 empty)
(define l2 (list "the" "other" "line"))

(define lls (list l0 l2))
(define str "This is a line\nthe other line")

(define lls2 (list l0 l1 l2))
(define str2 "This is a line\n\nthe other line")

; collapse: LLS -> String
; Convert a list of lines into a string

(check-expect (collapse lls) str)
(check-expect (collapse lls2) str2)

(define (collapse l)
  (cond
    [(empty? (rest l)) (line->string (first l))]
    [else (string-append (line->string (first l)) "\n"
                         (collapse (rest l)))]))

; line->string: LoS -> String
; Convert a line into a string.
; If the line is empty the string is ""

(check-expect (line->string empty) "")
(check-expect (line->string l0) "This is a line")

(define (line->string s)
  (cond
    [(= 0 (length s)) ""]
    [(empty? (rest s)) (first s)]
    [else (string-append (first s) " " (line->string (rest s)))]))