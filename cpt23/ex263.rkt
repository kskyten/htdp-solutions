;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex263) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; A Dir1 (short for directory) is one of:
; – empty
; – (cons File1 Dir1)
; – (cons Dir1 Dir1)

; A File1 is a Symbol

(define dirtree '( (part1 part2 part3)
                   read!
                   ((hang draw) (read!)) ))

; how-many: Dir1 -> N
; Determines how many files are in a directory

(check-expect (how-many empty) 0)
(check-expect (how-many '(foo)) 1)
(check-expect (how-many dirtree) 7)

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(symbol? (first dir)) (+ 1 (how-many (rest dir)))]
    [else (+ (how-many (first dir))
             (how-many (rest dir)))]))

