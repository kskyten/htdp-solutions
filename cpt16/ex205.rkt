;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex205) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; A [Bucket String] is
;   (make-bucket N [List-of String])
; interp. the n in (make-bucket n l) is the length of l
;   i.e., (= (length l) n) is always true

; The bucket contains the number of strings in the list
; and the list itself

(make-bucket 0 empty)
(make-bucket 2 '("foo" "bar"))

; A [Bucket IR] is
;   (make-bucket N [List-of IR])
; interp. the n in (make-bucket n l) is the length of l
;   i.e., (= (length l) n) is always true 

; The bucket contains the number of inventory records
; in the list and the list itself

(make-bucket 0 empty)
(make-bucket 1 `(,(make-ir "foobar" 1)))

; A [Bucket Posn] is
;   (make-bucket N [List-of Posn])
; interp. the n in (make-bucket n l) is the length of l
;   i.e., (= (length l) n) is always true

; The bucket contains the number of Posns
; in the list and the list itself

(make-bucket 0 empty)
(make-bucket 1 (list (make-posn 1 1)))