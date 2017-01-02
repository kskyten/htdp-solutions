;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex268) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require htdp/dir)
(define d0 (create-dir "."))

;(define-struct file (name size content))
; A File.v3 is a structure:
;   (make-file Symbol N String)

;(define-struct dir (name dirs files))
; A Dir.v3 is a structure:
;   (make-dir Symbol Dir* File*)

; A Dir* is one of:
; – empty
; – (cons Dir.v3 Dir*)

; A File* is one of:
; – empty
; – (cons File.v3 File*)

(define dt0 (make-dir 'TS (list (make-dir 'Text empty (list (make-file 'part1 1 "")
                                                            (make-file 'part2 2 "")
                                                            (make-file 'part3 3 "")))
                                (make-dir 'Libs (list (make-dir 'Code empty (list (make-file 'hang 4 "")
                                                                                  (make-file 'draw 5 "")))
                                                      (make-dir 'Docs empty (list (make-file 'read! 6"" )))) empty))
                      (list (make-file 'read! 7 ""))))

; how-many: Dir.v3 -> N
; Determine how many files are in a directory

(check-expect (how-many (make-dir 'Foo empty empty)) 0)
(check-expect (how-many (make-dir 'Foo empty (list (make-file 'bar 3 "")))) 1)

(check-expect (how-many (make-dir 'Foo
                                  (list (make-dir 'Bar
                                                  empty
                                                  (list (make-file 'baz 4 ""))))
                                  empty)) 1)

(check-expect (how-many (make-dir 'Foo
                                  (list (make-dir 'Bar
                                                  empty
                                                  (list (make-file 'baz 4 ""))))
                                  (list (make-file 'bar 3 "")))) 2)
(check-expect (how-many dt0) 7)

(define (how-many dir)
  (local (;dirfun: Dir.v3 -> Number
          (define (dirfun dir)
            (+ (dir* (dir-dirs dir))
               (file* (dir-files dir))))
          
          ;dir*: Dir* -> Number
          (define (dir* lod)
            (cond
              [(empty? lod) 0]
              [else (+ (dirfun (first lod))
                       (dir* (rest lod)))]))
          
          ;file*: File* -> Number
          (define (file* lof)
            (length lof))
          )
    (dirfun dir)))