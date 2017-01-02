;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex158) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; List-of-strings (LoS) is one of:
; - empty
; - (cons String LoS)

; List-of-list-of-strings (LLS) is one of:
; - empty
; - (cons LoS LLoS)

; example data
(define l0 (list "This" "is" "a" "line"))
(define rl0 (list "This" "is" "line"))
(define l1 empty)
(define l2 (list "the" "other" "line"))
(define rl2 (list "other" "line"))

(define lls (list l0 l1 l2))
(define lls2 (list l0 l1 l2))
(define rlls (list rl0 l1 rl2))

(define str "This is a line\n\nthe other line")
(define str2 "This is a line\n\nthe other line")

; remove-articles: LLS -> LLS
; Remove the words "a", "an", "the" from a text t

(check-expect (remove-articles lls) rlls)

(define (remove-articles t)
  (cond
    [(empty? t) empty]
    [(cons? t) (cons
                (line-processor (first t)) ; LoS (line)
                (remove-articles (rest t)))])) ; LLS (remaining lines)

; line-processor: LoS -> LoS
; remove articles from a line l

(check-expect (line-processor l1) l1)
(check-expect (line-processor l0) rl0)
(check-expect (line-processor l2) rl2)

(define (line-processor l)
  (cond
    [(empty? l) empty]
    [else (if (article? (first l))
              (line-processor (rest l))
              (cons (first l) (line-processor (rest l))))]))

; article?: String -> Boolean
; Is the given word an article?

(check-expect (article? "a") true)
(check-expect (article? "the") true)
(check-expect (article? "foo") false)

(define (article? s)
  (or (string=? "a" s)
      (string=? "an" s)
      (string=? "the" s)))

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

; rem: LLS -> String
; Remove the articles from a text t and produce a string

(check-expect (rem lls) "This is line\n\nother line")

(define (rem s)
  (collapse (remove-articles s)))

; main: LLS -> String
; Remove the articles from file f and save it as "no-articles-" f
(define (main f)
  (write-file (string-append "no-articles-" f) (rem (read-words/line f))))