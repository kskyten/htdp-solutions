;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex155) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct phone (area switch four))
; A Phone is a structure:
; (make-phone Three Three Four)
; A Three is between 100 and 999.
; A Four is between 1000 and 9999.

; List-of-phones (LoP) is one of:
; - empty
; - (cons Phone LoP)
; interp. a list of phone numbers

; replace: LoP -> LoP
; Replace all occurences of area code 713 with 281

(check-expect (replace empty) empty)
(check-expect (replace (list (make-phone 713 645 8769)
                             (make-phone 653 999 1232)))
              (list (make-phone 281 645 8769)
                    (make-phone 653 999 1232)))

(define (replace alop)
  (cond
    [(empty? alop) empty]
    [else (cons
           (replace-area (first alop))
           (replace (rest alop)))]))

; replace-area: Phone -> Phone
; Replace an area code of 713 with 281

(check-expect (replace-area (make-phone 713 123 4567))
              (make-phone 281 123 4567))

(define (replace-area p)
  (if (= 713 (phone-area p))
      (make-phone 281 (phone-switch p) (phone-four p))
      p))