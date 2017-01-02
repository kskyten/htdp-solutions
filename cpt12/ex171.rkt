;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex171) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct email (from date message))
; A Email Message is a structure:
; – (make-email String Number String)
; interp. (make-email f d m) represents text m sent by
; f, d seconds after the beginning of time

; List-of-email (LoE) is one of:
; - empty
; - (cons Email LoE)

; example data
(define E1 (make-email "John" 20000 "Hello"))
(define E2 (make-email "Sara" 15000 "Foo"))
(define E3 (make-email "Mike" 12003 "What's up?"))


; sort-emails: (Email Email -> Boolean) LoE -> LoE
; Sort a list of emails by date

(check-expect (sort-emails date-dsc empty) empty)
(check-expect (sort-emails date-dsc (list E1 E3 E2))
              (list E1 E2 E3))

(define (sort-emails sortby loe)
  (cond
    [(empty? loe) empty]
    [(cons? loe) (insert sortby (first loe) ; Email
                         (sort-emails sortby (rest loe)))] ; sorted list of emails
    ))

; insert: (Email Email -> Boolen) Email LoE -> LoE
; insert the email e in the correct place in a sorted list of emails

(check-expect (insert date-dsc E1 empty)
              (cons E1 empty))

(check-expect (insert date-dsc E2 (list E1 E3))
              (list E1 E2 E3))

(define (insert sortby e loe)
  (cond
    [(empty? loe) (cons e empty)]
    [(cons? loe) (if (sortby e (first loe))
                     (cons e loe)
                     (cons (first loe) (insert sortby e (rest loe))))]
    ))

; date-dsc: Email Email -> Boolean
; Determine if e1 is older than e2

(check-expect (date-dsc E1 E2) true)
(check-expect (date-dsc E2 E1) false)

(define (date-dsc e1 e2)
  (> (email-date e1)
     (email-date e2)))

; alpha: Email Email -> Boolean
; Determine if the sender of e1 is before e2 alphabetically

(check-expect (date-dsc E1 E2) true)
(check-expect (date-dsc E2 E1) false)

(define (alpha e1 e2)
  (string<? (email-from e1) 
            (email-from e2)))