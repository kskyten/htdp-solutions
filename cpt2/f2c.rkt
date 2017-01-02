;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname f2c) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define (convert in out)
  (write-file out
    (number->string
      (f2c
        (string->number
          (read-file in))))))
 
(define (f2c f)
  (* 5/9 (- f 32)))
 
(convert "/tmp/sample.dat" "/tmp/out.dat")