;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex68) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct person (fstname lstname male?))
; Person is (make-person String String Boolean)
; interp. (make-person "Mike" "Stone" true) represents
; a male by the name of Mike Stone

(define-struct dog (owner name age happiness))
; Dog is (make-dog Person String PositiveInteger H)
; interp. (make-dog p "Spot" 5 70) represents a 5 year old
; dog name Spot who is owned my person p

(define c "yellow") ; a Color
(define h 50 ) ; an element of H
(define p (make-person "Mike" "Stone" true)) ; a Person
(define d (make-dog p "Spot" 5 70)) ; a Dog
(define w false) ; a Weapon
