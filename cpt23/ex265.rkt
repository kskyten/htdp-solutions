;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex265) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
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
(define dt2 (make-dir 'TS (list 'read!
                                (make-dir 'Text '(part1 part2 part3))
                                (make-dir 'Libs (list (make-dir 'Docs '(read!))
                                                      (make-dir 'Code '(hang draw)))))))

; how-many: Dir.v2 -> N
; Determines how many files are in a directory

(check-expect (how-many dt0) 0)
(check-expect (how-many dt1) 1)
(check-expect (how-many dt2) 7)

(define (how-many dir)
  (local (; dirf: Dir.v2 -> N
          (define (dirf dir)
            (lofdf (dir-content dir)))
          
          ; lofdf: LOFD -> N
          (define (lofdf alofd)
            (cond
              [(empty? alofd) 0]
              [(symbol? (first alofd)) (+ 1 (lofdf (rest alofd)))]
              [else (+ (dirf (first alofd))
                       (lofdf (rest alofd)))]))
          )
    (dirf dir)))