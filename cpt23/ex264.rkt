;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex264) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct dir (name content))
; A Dir.v2 is a structure:
;   (make-dir Symbol LOFD)

; A LOFD (short for list of files and directories) is one of:
; – empty
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)

; A File.v2 is a Symbol.

(define dt0 (make-dir 'root empty))
(define dt1 (make-dir 'root '(foo)))
(define dt (make-dir 'TS
                           (list (make-dir 'Text '(part1 part2 part3))
                                 'read!
                                 (make-dir 'Libs (list (make-dir 'Code '(hang draw))
                                                       (make-dir 'Docs 'read!))))))
