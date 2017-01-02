;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex47) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define (ctest y)
  (- 200 (cond
         [(> y 200) 0]
         [else y])))

(ctest 100)
(ctest 210)

(define (create-rocket-scene h)
  (place-image ROCKET X
               (cond
                 [(<= h ROCKET-CENTER-TO-BOTTOM) h]
                 [(> h ROCKET-CENTER-TO-BOTTOM) ROCKET-CENTER-TO-BOTTOM])
               MTSCN))