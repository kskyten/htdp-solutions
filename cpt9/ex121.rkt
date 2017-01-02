;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex121) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; A List-of-names is one of:
; – empty
; – (cons String List-of-names)
; interp. a List-of-names represents a list of invitees by last name

; contains-flatt?: List-of-names -> Boolean
; to determine whether "Flatt" occurs on a-list-of-names

(check-expect (contains-flatt? empty) false)
(check-expect (contains-flatt? (cons "Findler" empty)) false)
(check-expect (contains-flatt? (cons "Flatt" empty)) true)
(check-expect
 (contains-flatt? (cons "Mur" (cons "Fish"  (cons "Find" empty))))
 false)
(check-expect
 (contains-flatt? (cons "A" (cons "Flatt" (cons "C" empty))))
 true)

(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) false]
    [(cons? a-list-of-names)
     (or (string=? (first a-list-of-names) "Flatt")
         (contains-flatt? (rest a-list-of-names)))]))

(define names (cons "Fagan"
                    (cons "Findler"
                          (cons "Fisler"
                                (cons "Flanagan"
                                      (cons "Flatt"
                                            (cons "Felleisen"
                                                  (cons "Friedman" empty))))))))
(contains-flatt? names)