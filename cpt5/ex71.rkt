;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex71) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct 3word (a b c))
; 3word is (make-3word Char Char Char)
; where each character is a lowercase letter in [a,z]
; for example (make-3word "a" "i" "m")
; interp. a three letter word

(define wrd1 (make-3word "d" "f" "f"))
(define wrd2 (make-3word "d" "o" "f"))

; different: 3word 3word -> 3word
; diff two words

(check-expect (different wrd1 wrd2) (make-3word "d" "*" "f"))
(check-expect (different wrd1 wrd1) (make-3word "d" "f" "f"))

(define (different wrd1 wrd2)
  (make-3word
   (compare-letters (3word-a wrd1) (3word-a wrd2))
   (compare-letters (3word-b wrd1) (3word-b wrd2))
   (compare-letters (3word-c wrd1) (3word-c wrd2))
   ))

; compare-letters: Letter Letter -> Letter
; compare two letters and return the second letter
; if they match and "*" if they don't

(check-expect (compare-letters "a" "a") "a")
(check-expect (compare-letters "a" "b") "*")

(define (compare-letters l1 l2)
  (if (string=? l1 l2) l2 "*"))